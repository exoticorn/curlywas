use crate::ast;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0, none_of},
    combinator::{self, cut, map, map_res, not, opt, peek, recognize, value},
    error::VerboseError,
    multi::{fold_many0, many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated},
    Finish,
};

type IResult<'a, O> = nom::IResult<&'a str, O, VerboseError<&'a str>>;

pub fn parse(s: &str) -> Result<ast::Script, VerboseError<&str>> {
    let (_, script) = combinator::all_consuming(terminated(script, multispace0))(s).finish()?;
    Ok(script)
}

fn script(s: &str) -> IResult<ast::Script> {
    let (s, items) = many0(top_level_item)(s)?;
    let mut imports = vec![];
    let mut global_vars = vec![];
    let mut functions = vec![];
    for item in items {
        match item {
            ast::TopLevelItem::Import(i) => imports.push(i),
            ast::TopLevelItem::GlobalVar(v) => global_vars.push(v),
            ast::TopLevelItem::Function(f) => functions.push(f),
        }
    }
    Ok((
        s,
        ast::Script {
            imports,
            global_vars,
            functions,
        },
    ))
}

fn top_level_item(s: &str) -> IResult<ast::TopLevelItem> {
    alt((
        map(import, ast::TopLevelItem::Import),
        map(function, ast::TopLevelItem::Function),
        map(global_var, ast::TopLevelItem::GlobalVar),
    ))(s)
}

fn import(s: &str) -> IResult<ast::Import> {
    let (s, position) = ws(position)(s)?;
    let (s, _) = tag("import")(s)?;
    let (s, import) = ws(delimited(
        char('"'),
        recognize(many1(none_of("\""))),
        char('"'),
    ))(s)?;
    let (s, type_) = alt((
        map_res(
            preceded(
                ws(tag("memory")),
                delimited(ws(char('(')), ws(digit1), ws(char(')'))),
            ),
            |num| num.parse().map(ast::ImportType::Memory),
        ),
        map(
            preceded(
                ws(tag("global")),
                pair(identifier, preceded(ws(char(':')), type_)),
            ),
            |(name, type_)| ast::ImportType::Variable { name, type_ },
        ),
    ))(s)?;
    let (s, _) = ws(char(';'))(s)?;

    Ok((
        s,
        ast::Import {
            position,
            import,
            type_,
        },
    ))
}

fn global_var(s: &str) -> IResult<ast::GlobalVar> {
    let (s, _) = ws(tag("global"))(s)?;
    let (s, position) = ws(position)(s)?;
    let (s, name) = identifier(s)?;
    let (s, type_) = preceded(ws(char(':')), type_)(s)?;
    let (s, _) = ws(char(';'))(s)?;

    Ok((
        s,
        ast::GlobalVar {
            position,
            name: name,
            type_,
        },
    ))
}

fn function(s: &str) -> IResult<ast::Function> {
    let (s, export) = map(ws(opt(tag("export"))), |e| e.is_some())(s)?;
    let (s, _) = ws(tag("fn"))(s)?;
    cut(move |s| {
        let (s, position) = ws(position)(s)?;
        let (s, name) = identifier(s)?;
        let (s, params) = delimited(
            ws(char('(')),
            separated_list0(
                ws(char(',')),
                pair(map(identifier, |i| i), preceded(ws(tag(":")), type_)),
            ),
            ws(char(')')),
        )(s)?;
        let (s, type_) = opt(preceded(ws(tag("->")), type_))(s)?;
        let (s, body) = block(s)?;

        Ok((
            s,
            ast::Function {
                position,
                export,
                name: name,
                params,
                type_,
                body,
            },
        ))
    })(s)
}

fn block(s: &str) -> IResult<ast::Block> {
    let (s, (statements, final_expression)) = delimited(
        ws(char('{')),
        pair(many0(statement), opt(expression)),
        ws(char('}')),
    )(s)?;
    Ok((
        s,
        ast::Block {
            statements,
            final_expression: final_expression.map(|e| e.into()),
        },
    ))
}

fn statement(s: &str) -> IResult<ast::Statement> {
    alt((
        map(local_var, ast::Statement::LocalVariable),
        map(terminated(expression, ws(char(';'))), |e| {
            ast::Statement::Expression(e.into())
        }),
        map(
            terminated(block_expression, not(peek(ws(char('}'))))),
            |e| ast::Statement::Expression(e.into()),
        ),
        map(
            terminated(
                pair(
                    mem_location,
                    ws(pair(position, preceded(char('='), expression))),
                ),
                ws(char(';')),
            ),
            |(mem_location, (position, value))| ast::Statement::Poke {
                position,
                mem_location,
                value: value.into(),
            },
        ),
    ))(s)
}

fn local_var(s: &str) -> IResult<ast::LocalVariable> {
    let (s, _) = ws(tag("let"))(s)?;
    let (s, position) = ws(position)(s)?;
    let (s, name) = identifier(s)?;
    let (s, type_) = opt(preceded(ws(char(':')), type_))(s)?;
    let (s, value) = opt(preceded(ws(char('=')), expression))(s)?;
    let (s, _) = ws(char(';'))(s)?;

    Ok((
        s,
        ast::LocalVariable {
            position,
            name: name,
            type_,
            value: value.map(|v| v.into()),
        },
    ))
}

fn mem_location(s: &str) -> IResult<ast::MemoryLocation> {
    let (s, position) = ws(position)(s)?;
    let (s, left) = expression(s)?;
    let (s, size) = map(ws(alt((char('?'), char('!')))), |op| match op {
        '?' => ast::MemSize::Byte,
        '!' => ast::MemSize::Word,
        _ => unreachable!(),
    })(s)?;
    let (s, right) = expression(s)?;

    Ok((
        s,
        ast::MemoryLocation {
            position,
            size,
            left: left.into(),
            right: right.into(),
        },
    ))
}

fn expression(s: &str) -> IResult<ast::Expr> {
    expression_cmp(s)
}

fn expression_atom(s: &str) -> IResult<ast::Expr> {
    alt((
        branch_if,
        block_expression,
        map(
            separated_pair(pair(ws(position), identifier), ws(tag(":=")), expression),
            |((position, name), value)| ast::Expr::LocalTee {
                position,
                name: name,
                value: Box::new(value.into()),
            },
        ),
        map(integer, |v| ast::Expr::I32Const(v)),
        map(ws(pair(position, identifier)), |(position, name)| {
            ast::Expr::Variable {
                position,
                name: name,
            }
        }),
        delimited(ws(char('(')), cut(expression), ws(char(')'))),
    ))(s)
}

fn branch_if(s: &str) -> IResult<ast::Expr> {
    let (s, position) = ws(position)(s)?;
    let (s, _) = tag("branch_if")(s)?;
    cut(move |s| {
        let (s, condition) = expression(s)?;
        let (s, _) = ws(char(':'))(s)?;
        let (s, label) = identifier(s)?;

        Ok((
            s,
            ast::Expr::BranchIf {
                position,
                condition: Box::new(condition.into()),
                label: label,
            },
        ))
    })(s)
}

fn expression_product(s: &str) -> IResult<ast::Expr> {
    let (s, mut init) = map(expression_atom, Some)(s)?;
    fold_many0(
        pair(
            ws(pair(position, alt((char('*'), char('/'), char('%'))))),
            expression_atom,
        ),
        move || init.take().unwrap(),
        |left, ((position, op), right)| {
            let op = match op {
                '*' => ast::BinOp::Mul,
                '/' => ast::BinOp::Div,
                '%' => ast::BinOp::Rem,
                _ => unreachable!(),
            };
            ast::Expr::BinOp {
                position,
                op,
                left: Box::new(left.into()),
                right: Box::new(right.into()),
            }
        },
    )(s)
}

fn expression_sum(s: &str) -> IResult<ast::Expr> {
    let (s, mut init) = map(expression_product, Some)(s)?;
    fold_many0(
        pair(
            ws(pair(position, alt((char('+'), char('-'))))),
            expression_product,
        ),
        move || init.take().unwrap(),
        |left, ((position, op), right)| {
            let op = if op == '+' {
                ast::BinOp::Add
            } else {
                ast::BinOp::Sub
            };
            ast::Expr::BinOp {
                position,
                op,
                left: Box::new(left.into()),
                right: Box::new(right.into()),
            }
        },
    )(s)
}

fn expression_bit(s: &str) -> IResult<ast::Expr> {
    let (s, mut init) = map(expression_sum, Some)(s)?;
    fold_many0(
        pair(
            ws(pair(position, alt((char('&'), char('|'), char('^'))))),
            expression_sum,
        ),
        move || init.take().unwrap(),
        |left, ((position, op), right)| {
            let op = match op {
                '&' => ast::BinOp::And,
                '|' => ast::BinOp::Or,
                '^' => ast::BinOp::Xor,
                _ => unreachable!(),
            };
            ast::Expr::BinOp {
                position,
                op,
                left: Box::new(left.into()),
                right: Box::new(right.into()),
            }
        },
    )(s)
}

fn expression_cmp(s: &str) -> IResult<ast::Expr> {
    let (s, mut init) = map(expression_bit, Some)(s)?;
    fold_many0(
        pair(
            ws(pair(
                position,
                alt((
                    tag("=="),
                    tag("!="),
                    tag("<="),
                    tag("<"),
                    tag(">="),
                    tag(">"),
                )),
            )),
            expression_bit,
        ),
        move || init.take().unwrap(),
        |left, ((position, op), right)| {
            let op = match op {
                "==" => ast::BinOp::Eq,
                "!=" => ast::BinOp::Ne,
                "<=" => ast::BinOp::Le,
                "<" => ast::BinOp::Lt,
                ">=" => ast::BinOp::Ge,
                ">" => ast::BinOp::Gt,
                _ => unreachable!(),
            };
            ast::Expr::BinOp {
                position,
                op,
                left: Box::new(left.into()),
                right: Box::new(right.into()),
            }
        },
    )(s)
}

fn block_expression(s: &str) -> IResult<ast::Expr> {
    loop_(s)
}

fn loop_(s: &str) -> IResult<ast::Expr> {
    let (s, position) = ws(position)(s)?;
    let (s, _) = tag("loop")(s)?;
    cut(move |s| {
        let (s, label) = identifier(s)?;
        let (s, block) = block(s)?;

        Ok((
            s,
            ast::Expr::Loop {
                position,
                label: label,
                block: Box::new(block.into()),
            },
        ))
    })(s)
}

fn integer(s: &str) -> IResult<i32> {
    ws(map_res(
        recognize(pair(opt(char('-')), digit1)),
        |n: &str| n.parse::<i32>(),
    ))(s)
}

fn type_(s: &str) -> IResult<ast::Type> {
    ws(alt((
        value(ast::Type::I32, tag("i32")),
        value(ast::Type::I64, tag("i64")),
        value(ast::Type::F32, tag("f32")),
        value(ast::Type::F64, tag("f64")),
    )))(s)
}

fn identifier(s: &str) -> IResult<&str> {
    ws(recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    )))(s)
}

fn position(s: &str) -> IResult<ast::Position> {
    Ok((s, ast::Position(s.len())))
}

fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> IResult<O>
where
    F: FnMut(&'a str) -> IResult<'a, O>,
{
    preceded(multispace0, inner)
}

#[cfg(test)]
mod test {
    use nom::combinator::all_consuming;

    #[test]
    fn identifier() {
        all_consuming(super::identifier)("_froobaz123").unwrap();
    }

    #[test]
    fn type_() {
        all_consuming(super::type_)("i32").unwrap();
        all_consuming(super::type_)("i64").unwrap();
        all_consuming(super::type_)("f32").unwrap();
        all_consuming(super::type_)("f64").unwrap();
    }

    #[test]
    fn integer() {
        all_consuming(super::integer)("123").unwrap();
        all_consuming(super::integer)("-123").unwrap();
    }

    #[test]
    fn local_var() {
        all_consuming(super::local_var)("let foo: i32;").unwrap();
        all_consuming(super::local_var)("let bar = 42;").unwrap();
    }

    #[test]
    fn function() {
        all_consuming(super::function)("export fn foo(a: i32, b: f32) -> i32 { let x = 42; x }")
            .unwrap();
    }

    #[test]
    fn loop_() {
        all_consuming(super::loop_)("loop foo { 42 }").unwrap();
        all_consuming(super::loop_)("loop foo { i?64 = (i % 320 + time / 10) ^ (i / 320); }")
            .unwrap();
    }

    #[test]
    fn block() {
        all_consuming(super::block)("{loop frame {}}").unwrap();
    }

    #[test]
    fn expression() {
        all_consuming(super::expression)("foo + 2 * (bar ^ 23)").unwrap();
        all_consuming(super::expression)("i := i + 1").unwrap();
        all_consuming(super::expression)("(i := i + 1)").unwrap();
    }

    #[test]
    fn poke() {
        all_consuming(super::statement)("i?64 = (i % 320 + time / 10) ^ (i / 320);").unwrap();
    }

    #[test]
    fn branch_if() {
        all_consuming(super::branch_if)("branch_if (i := i + 1) < 10: foo").unwrap();
    }
}
