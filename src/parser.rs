use crate::ast;
use crate::Span;
use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::{prelude::*, stream::Stream};
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Token {
    Import,
    Export,
    Fn,
    Let,
    Memory,
    Global,
    Mut,
    Loop,
    Block,
    Branch,
    BranchIf,
    Lazy,
    Inline,
    As,
    Select,
    If,
    Else,
    Return,
    Data,
    Ident(String),
    Str(String),
    Int(i32),
    Int64(i64),
    Float(String),
    Float64(String),
    Op(String),
    Ctrl(char),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Import => write!(f, "import"),
            Token::Export => write!(f, "export"),
            Token::Fn => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::Memory => write!(f, "memory"),
            Token::Global => write!(f, "global"),
            Token::Mut => write!(f, "mut"),
            Token::Loop => write!(f, "loop"),
            Token::Block => write!(f, "block"),
            Token::Branch => write!(f, "branch"),
            Token::BranchIf => write!(f, "branch_if"),
            Token::Lazy => write!(f, "lazy"),
            Token::Inline => write!(f, "inline"),
            Token::As => write!(f, "as"),
            Token::Select => write!(f, "select"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Return => write!(f, "return"),
            Token::Data => write!(f, "data"),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Str(s) => write!(f, "{:?}", s),
            Token::Int(v) => write!(f, "{}", v),
            Token::Int64(v) => write!(f, "{}", v),
            Token::Float(v) => write!(f, "{}", v),
            Token::Float64(v) => write!(f, "{}", v),
            Token::Op(s) => write!(f, "{}", s),
            Token::Ctrl(c) => write!(f, "{}", c),
        }
    }
}

pub fn parse(source: &str) -> Result<ast::Script, ()> {
    let tokens = match lexer().parse(source) {
        Ok(tokens) => tokens,
        Err(errors) => {
            report_errors(
                errors
                    .into_iter()
                    .map(|e| e.map(|c| c.to_string()))
                    .collect(),
                source,
            );
            return Err(());
        }
    };

    let source_len = source.chars().count();
    let script = match script_parser().parse(Stream::from_iter(
        source_len..source_len + 1,
        tokens.into_iter(),
    )) {
        Ok(script) => script,
        Err(errors) => {
            report_errors(
                errors
                    .into_iter()
                    .map(|e| e.map(|t| t.to_string()))
                    .collect(),
                source,
            );
            return Err(());
        }
    };
    Ok(script)
}

fn report_errors(errors: Vec<Simple<String>>, source: &str) {
    for error in errors {
        let report = Report::build(ReportKind::Error, (), error.span().start());

        let report = match error.reason() {
            chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
                .with_message(format!(
                    "Unclosed delimiter {}",
                    delimiter.fg(Color::Yellow)
                ))
                .with_label(
                    Label::new(span.clone())
                        .with_message(format!(
                            "Unclosed delimiter {}",
                            delimiter.fg(Color::Yellow)
                        ))
                        .with_color(Color::Yellow),
                )
                .with_label(
                    Label::new(error.span())
                        .with_message(format!(
                            "Must be closed before this {}",
                            error
                                .found()
                                .unwrap_or(&"end of file".to_string())
                                .fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                ),
            chumsky::error::SimpleReason::Unexpected => report
                .with_message(format!(
                    "{}, expected one of {}",
                    if error.found().is_some() {
                        "Unexpected token in input"
                    } else {
                        "Unexpted end of input"
                    },
                    if error.expected().len() == 0 {
                        "end of input".to_string()
                    } else {
                        error
                            .expected()
                            .map(|x| x.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    }
                ))
                .with_label(
                    Label::new(error.span())
                        .with_message(format!(
                            "Unexpected token {}",
                            error
                                .found()
                                .unwrap_or(&"end of file".to_string())
                                .fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                ),
            chumsky::error::SimpleReason::Custom(msg) => report.with_message(msg).with_label(
                Label::new(error.span())
                    .with_message(format!("{}", msg.fg(Color::Red)))
                    .with_color(Color::Red),
            ),
        };

        report.finish().eprint(Source::from(source)).unwrap();
    }
}

fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let float64 = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)))
        .then_ignore(seq::<_, _, Simple<char>>("f64".chars()))
        .collect::<String>()
        .map(Token::Float64);

    let float = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)))
        .collect::<String>()
        .map(Token::Float);

    let integer = seq::<_, _, Simple<char>>("0x".chars())
        .ignore_then(text::int(16))
        .try_map(|n, span| {
            u64::from_str_radix(&n, 16).map_err(|err| Simple::custom(span, err.to_string()))
        })
        .or(text::int(10).try_map(|n: String, span: Span| {
            u64::from_str_radix(&n, 10).map_err(|err| Simple::custom(span, err.to_string()))
        }))
        .boxed();

    let int64 = integer
        .clone()
        .then_ignore(seq::<_, _, Simple<char>>("i64".chars()))
        .map(|n| Token::Int64(n as i64));

    let int = integer.try_map(|n, span| {
        u32::try_from(n)
            .map(|n| Token::Int(n as i32))
            .map_err(|err| Simple::custom(span, err.to_string()))
    });

    let str_ = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Str);

    let op = one_of("+-*/%&^|<=>#".chars())
        .repeated()
        .at_least(1)
        .or(just(':').chain(just('=')))
        .collect::<String>()
        .map(Token::Op);

    let ctrl = one_of("(){};,:?!".chars()).map(Token::Ctrl);

    fn ident() -> impl Parser<char, String, Error = Simple<char>> + Copy + Clone {
        filter(|c: &char| c.is_ascii_alphabetic() || *c == '_')
            .map(Some)
            .chain::<char, Vec<_>, _>(
                filter(|c: &char| c.is_ascii_alphanumeric() || *c == '_' || *c == '.').repeated(),
            )
            .collect()
    }

    let ident = ident().map(|ident: String| match ident.as_str() {
        "import" => Token::Import,
        "export" => Token::Export,
        "fn" => Token::Fn,
        "let" => Token::Let,
        "memory" => Token::Memory,
        "global" => Token::Global,
        "mut" => Token::Mut,
        "loop" => Token::Loop,
        "block" => Token::Block,
        "branch" => Token::Branch,
        "branch_if" => Token::BranchIf,
        "lazy" => Token::Lazy,
        "inline" => Token::Inline,
        "as" => Token::As,
        "select" => Token::Select,
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,
        "data" => Token::Data,
        _ => Token::Ident(ident),
    });

    let single_line =
        seq::<_, _, Simple<char>>("//".chars()).then_ignore(take_until(text::newline()));

    let multi_line =
        seq::<_, _, Simple<char>>("/*".chars()).then_ignore(take_until(seq("*/".chars())));

    let comment = single_line.or(multi_line);

    let token = float
        .or(float64)
        .or(int64)
        .or(int)
        .or(str_)
        .or(op)
        .or(ctrl)
        .or(ident)
        .recover_with(skip_then_retry_until([]));

    token
        .map_with_span(|tok, span| (tok, span))
        .padded()
        .padded_by(comment.padded().repeated())
        .repeated()
}

fn map_token<O>(
    f: impl Fn(&Token) -> Option<O> + 'static + Clone,
) -> impl Parser<Token, O, Error = Simple<Token>> + Clone {
    filter_map(move |span, tok: Token| {
        if let Some(output) = f(&tok) {
            Ok(output)
        } else {
            Err(Simple::expected_input_found(span, Vec::new(), Some(tok)))
        }
    })
}

fn script_parser() -> impl Parser<Token, ast::Script, Error = Simple<Token>> + Clone {
    let identifier = filter_map(|span, tok| match tok {
        Token::Ident(id) => Ok(id),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    })
    .labelled("identifier");

    let integer = map_token(|tok| match tok {
        Token::Int(v) => Some(*v),
        _ => None,
    });

    let string = map_token(|tok| match tok {
        Token::Str(s) => Some(s.clone()),
        _ => None,
    });

    let mut expression_out = None;
    let block = recursive(|block| {
        let mut block_expression = None;
        let expression = recursive(|expression| {
            let val = map_token(|tok| match tok {
                Token::Int(v) => Some(ast::Expr::I32Const(*v)),
                Token::Int64(v) => Some(ast::Expr::I64Const(*v)),
                Token::Float(v) => Some(ast::Expr::F32Const(v.parse().unwrap())),
                Token::Float64(v) => Some(ast::Expr::F64Const(v.parse().unwrap())),
                _ => None,
            })
            .labelled("value");

            let variable = filter_map(|span, tok| match tok {
                Token::Ident(id) => Ok(ast::Expr::Variable(id)),
                _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
            })
            .labelled("variable");

            let local_tee = identifier
                .then(just(Token::Op(":=".to_string())).ignore_then(expression.clone()))
                .map(|(name, expr)| ast::Expr::LocalTee {
                    name,
                    value: Box::new(expr),
                })
                .boxed();

            let loop_expr = just(Token::Loop)
                .ignore_then(identifier)
                .then(block.clone())
                .map(|(label, block)| ast::Expr::Loop {
                    label,
                    block: Box::new(block),
                });

            let label_block_expr = just(Token::Block)
                .ignore_then(identifier)
                .then(block.clone())
                .map(|(label, block)| ast::Expr::LabelBlock {
                    label,
                    block: Box::new(block),
                });

            let if_expr = just(Token::If)
                .ignore_then(expression.clone())
                .then(block.clone())
                .then(just(Token::Else).ignore_then(block.clone()).or_not())
                .map(|((condition, if_true), if_false)| ast::Expr::If {
                    condition: Box::new(condition),
                    if_true: Box::new(if_true),
                    if_false: if_false.map(Box::new),
                });

            let block_expr = loop_expr.or(label_block_expr).or(if_expr).boxed();

            block_expression = Some(block_expr.clone());

            let branch = just(Token::Branch)
                .ignore_then(identifier)
                .map(|label| ast::Expr::Branch(label));

            let branch_if = just(Token::BranchIf)
                .ignore_then(expression.clone())
                .then_ignore(just(Token::Ctrl(':')))
                .then(identifier)
                .map(|(condition, label)| ast::Expr::BranchIf {
                    condition: Box::new(condition),
                    label,
                })
                .boxed();

            let let_ = just(Token::Let)
                .ignore_then(
                    (just(Token::Lazy)
                        .to(ast::LetType::Lazy)
                        .or(just(Token::Inline).to(ast::LetType::Inline)))
                    .or_not(),
                )
                .then(identifier.clone())
                .then(just(Token::Ctrl(':')).ignore_then(type_parser()).or_not())
                .then(
                    just(Token::Op("=".to_string()))
                        .ignore_then(expression.clone())
                        .or_not(),
                )
                .map(|(((let_type, name), type_), value)| ast::Expr::Let {
                    name,
                    type_,
                    value: value.map(Box::new),
                    let_type: let_type.unwrap_or(ast::LetType::Normal),
                })
                .boxed();

            let tee = identifier
                .clone()
                .then_ignore(just(Token::Op(":=".to_string())))
                .then(expression.clone())
                .map(|(name, value)| ast::Expr::LocalTee {
                    name,
                    value: Box::new(value),
                })
                .boxed();

            let assign = identifier
                .clone()
                .then_ignore(just(Token::Op("=".to_string())))
                .then(expression.clone())
                .map(|(name, value)| ast::Expr::Assign {
                    name,
                    value: Box::new(value),
                })
                .boxed();

            let select = just(Token::Select)
                .ignore_then(
                    expression
                        .clone()
                        .then_ignore(just(Token::Ctrl(',')))
                        .then(expression.clone())
                        .then_ignore(just(Token::Ctrl(',')))
                        .then(expression.clone())
                        .delimited_by(Token::Ctrl('('), Token::Ctrl(')')),
                )
                .map(|((condition, if_true), if_false)| ast::Expr::Select {
                    condition: Box::new(condition),
                    if_true: Box::new(if_true),
                    if_false: Box::new(if_false),
                })
                .boxed();

            let function_call = identifier
                .clone()
                .then(
                    expression
                        .clone()
                        .separated_by(just(Token::Ctrl(',')))
                        .delimited_by(Token::Ctrl('('), Token::Ctrl(')')),
                )
                .map(|(name, params)| ast::Expr::FuncCall { name, params })
                .boxed();

            let return_ = just(Token::Return)
                .ignore_then(expression.clone().or_not())
                .map(|value| ast::Expr::Return {
                    value: value.map(Box::new),
                });

            let atom = val
                .or(tee)
                .or(function_call)
                .or(assign)
                .or(local_tee)
                .or(variable)
                .or(block_expr)
                .or(branch)
                .or(branch_if)
                .or(let_)
                .or(select)
                .or(return_)
                .map_with_span(|expr, span| expr.with_span(span))
                .or(expression
                    .clone()
                    .delimited_by(Token::Ctrl('('), Token::Ctrl(')')))
                .or(block)
                .recover_with(nested_delimiters(
                    Token::Ctrl('('),
                    Token::Ctrl(')'),
                    [(Token::Ctrl('{'), Token::Ctrl('}'))],
                    |span| ast::Expr::Error.with_span(span),
                ))
                .boxed();

            let unary_op = just(Token::Op("-".to_string()))
                .to(ast::UnaryOp::Negate)
                .or(just(Token::Ctrl('!')).to(ast::UnaryOp::Not))
                .map_with_span(|op, span| (op, span))
                .repeated()
                .then(atom)
                .map(|(ops, value)| {
                    ops.into_iter().rev().fold(value, |acc, (op, span)| {
                        let span = span.start..acc.span.end;
                        ast::Expr::UnaryOp {
                            op,
                            value: Box::new(acc),
                        }
                        .with_span(span)
                    })
                })
                .boxed();

            let op_cast = unary_op
                .clone()
                .then(
                    just(Token::As)
                        .ignore_then(type_parser())
                        .map_with_span(|type_, span| (type_, span))
                        .repeated(),
                )
                .foldl(|value, (type_, span)| {
                    ast::Expr::Cast {
                        value: Box::new(value),
                        type_,
                    }
                    .with_span(span)
                })
                .boxed();

            let mem_size = just(Token::Ctrl('?'))
                .to(ast::MemSize::Byte)
                .or(just(Token::Ctrl('!')).to(ast::MemSize::Word));

            let mem_op = mem_size.then(op_cast.clone());

            fn make_memory_op(
                left: ast::Expression,
                peek_ops: Vec<(ast::MemSize, ast::Expression)>,
                poke_op: Option<((ast::MemSize, ast::Expression), ast::Expression)>,
            ) -> ast::Expression {
                let left = peek_ops.into_iter().fold(left, |left, (size, right)| {
                    let span = left.span.start..right.span.end;
                    ast::Expr::Peek(ast::MemoryLocation {
                        span: span.clone(),
                        left: Box::new(left),
                        size,
                        right: Box::new(right),
                    })
                    .with_span(span)
                });
                if let Some(((size, right), value)) = poke_op {
                    let span = left.span.start..value.span.end;
                    ast::Expr::Poke {
                        mem_location: ast::MemoryLocation {
                            span: span.clone(),
                            left: Box::new(left),
                            size,
                            right: Box::new(right),
                        },
                        value: Box::new(value),
                    }
                    .with_span(span)
                } else {
                    left
                }
            }

            let short_memory_op = mem_op
                .clone()
                .then(
                    just(Token::Op("=".to_string()))
                        .ignore_then(expression.clone())
                        .or_not(),
                )
                .map(|((size, left), value)| {
                    let right = ast::Expr::I32Const(0).with_span(left.span.clone());
                    if let Some(value) = value {
                        make_memory_op(left, vec![], Some(((size, right), value)))
                    } else {
                        make_memory_op(left, vec![(size, right)], None)
                    }
                })
                .clone();

            let memory_op = op_cast
                .clone()
                .or(short_memory_op.clone())
                .then(mem_op.clone().repeated().at_least(1))
                .then(
                    just(Token::Op("=".to_string()))
                        .ignore_then(expression.clone())
                        .or_not(),
                )
                .map(|((left, mut peek_ops), poke_op)| {
                    if let Some(value) = poke_op {
                        let poke_op = Some((peek_ops.pop().unwrap(), value));
                        make_memory_op(left, peek_ops, poke_op)
                    } else {
                        make_memory_op(left, peek_ops, None)
                    }
                })
                .boxed()
                .or(op_cast.clone())
                .or(short_memory_op.clone());

            let op_product = memory_op
                .clone()
                .then(
                    just(Token::Op("*".to_string()))
                        .to(ast::BinOp::Mul)
                        .or(just(Token::Op("/".to_string())).to(ast::BinOp::Div))
                        .or(just(Token::Op("#/".to_string())).to(ast::BinOp::DivU))
                        .or(just(Token::Op("%".to_string())).to(ast::BinOp::Rem))
                        .or(just(Token::Op("#%".to_string())).to(ast::BinOp::RemU))
                        .then(memory_op.clone())
                        .repeated(),
                )
                .foldl(|left, (op, right)| {
                    let span = left.span.start..right.span.end;
                    ast::Expr::BinOp {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    }
                    .with_span(span)
                })
                .boxed();

            let op_sum = op_product
                .clone()
                .then(
                    just(Token::Op("+".to_string()))
                        .to(ast::BinOp::Add)
                        .or(just(Token::Op("-".to_string())).to(ast::BinOp::Sub))
                        .then(op_product.clone())
                        .repeated(),
                )
                .foldl(|left, (op, right)| {
                    let span = left.span.start..right.span.end;
                    ast::Expr::BinOp {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    }
                    .with_span(span)
                })
                .boxed();

            let op_shift = op_sum
                .clone()
                .then(
                    just(Token::Op("<<".to_string()))
                        .to(ast::BinOp::Shl)
                        .or(just(Token::Op("#>>".to_string())).to(ast::BinOp::ShrU))
                        .or(just(Token::Op(">>".to_string())).to(ast::BinOp::ShrS))
                        .then(op_sum.clone())
                        .repeated(),
                )
                .foldl(|left, (op, right)| {
                    let span = left.span.start..right.span.end;
                    ast::Expr::BinOp {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    }
                    .with_span(span)
                })
                .boxed();

            let op_cmp = op_shift
                .clone()
                .then(
                    just(Token::Op("==".to_string()))
                        .to(ast::BinOp::Eq)
                        .or(just(Token::Op("!=".to_string())).to(ast::BinOp::Ne))
                        .or(just(Token::Op("<".to_string())).to(ast::BinOp::Lt))
                        .or(just(Token::Op("#<".to_string())).to(ast::BinOp::LtU))
                        .or(just(Token::Op("<=".to_string())).to(ast::BinOp::Le))
                        .or(just(Token::Op("#<=".to_string())).to(ast::BinOp::LeU))
                        .or(just(Token::Op(">".to_string())).to(ast::BinOp::Gt))
                        .or(just(Token::Op("#>".to_string())).to(ast::BinOp::GtU))
                        .or(just(Token::Op(">=".to_string())).to(ast::BinOp::Ge))
                        .or(just(Token::Op("#>=".to_string())).to(ast::BinOp::GeU))
                        .then(op_shift.clone())
                        .repeated(),
                )
                .foldl(|left, (op, right)| {
                    let span = left.span.start..right.span.end;
                    ast::Expr::BinOp {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    }
                    .with_span(span)
                })
                .boxed();

            let op_bit = op_cmp
                .clone()
                .then(
                    just(Token::Op("&".to_string()))
                        .to(ast::BinOp::And)
                        .or(just(Token::Op("|".to_string())).to(ast::BinOp::Or))
                        .or(just(Token::Op("^".to_string())).to(ast::BinOp::Xor))
                        .then(op_cmp.clone())
                        .repeated(),
                )
                .foldl(|left, (op, right)| {
                    let span = left.span.start..right.span.end;
                    ast::Expr::BinOp {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    }
                    .with_span(span)
                })
                .boxed();

            let op_first = op_bit
                .clone()
                .then(
                    just(Token::Op("<|".to_string()))
                        .ignore_then(op_bit)
                        .repeated(),
                )
                .foldl(|left, right| {
                    let span = left.span.start..right.span.end;
                    ast::Expr::First {
                        value: Box::new(left),
                        drop: Box::new(right),
                    }
                    .with_span(span)
                })
                .boxed();

            op_first
        });

        expression_out = Some(expression.clone());

        let block_expression = block_expression.unwrap();

        expression
            .clone()
            .then(just(Token::Ctrl(';')).to(false))
            .or(block_expression.map_with_span(|expr, span| (expr.with_span(span), true)))
            .repeated()
            .then(expression.clone().or_not())
            .map_with_span(|(mut statements, mut final_expression), span| {
                if final_expression.is_none()
                    && statements
                        .last()
                        .map(|(_, block_expr)| *block_expr)
                        .unwrap_or(false)
                {
                    final_expression = statements.pop().map(|(expr, _)| expr);
                }
                ast::Expr::Block {
                    statements: statements.into_iter().map(|(expr, _)| expr).collect(),
                    final_expression: final_expression.map(|e| Box::new(e)),
                }
                .with_span(span)
            })
            .delimited_by(Token::Ctrl('{'), Token::Ctrl('}'))
            .boxed()
    });

    let expression = expression_out.unwrap();

    let top_level_item = {
        let import_memory = just(Token::Memory)
            .ignore_then(
                integer
                    .clone()
                    .delimited_by(Token::Ctrl('('), Token::Ctrl(')')),
            )
            .map(|min_size| ast::ImportType::Memory(min_size as u32))
            .boxed();

        let import_global = just(Token::Global)
            .ignore_then(just(Token::Mut).or_not())
            .then(identifier.clone())
            .then_ignore(just(Token::Ctrl(':')))
            .then(type_parser())
            .map(|((mut_opt, name), type_)| ast::ImportType::Variable {
                mutable: mut_opt.is_some(),
                name,
                type_,
            })
            .boxed();

        let import_function = just(Token::Fn)
            .ignore_then(identifier.clone())
            .then(
                type_parser()
                    .separated_by(just(Token::Ctrl(',')))
                    .delimited_by(Token::Ctrl('('), Token::Ctrl(')')),
            )
            .then(
                just(Token::Op("->".to_string()))
                    .ignore_then(type_parser())
                    .or_not(),
            )
            .map(|((name, params), result)| ast::ImportType::Function {
                name,
                params,
                result,
            })
            .boxed();

        let import = just(Token::Import)
            .ignore_then(string.clone())
            .then(import_memory.or(import_global).or(import_function))
            .then_ignore(just(Token::Ctrl(';')))
            .map_with_span(|(import, type_), span| {
                ast::TopLevelItem::Import(ast::Import {
                    span,
                    import,
                    type_,
                })
            })
            .boxed();

        let parameter = identifier
            .clone()
            .then_ignore(just(Token::Ctrl(':')))
            .then(type_parser())
            .boxed();

        let function = just(Token::Export)
            .or_not()
            .then_ignore(just(Token::Fn))
            .then(identifier.clone())
            .then(
                parameter
                    .separated_by(just(Token::Ctrl(',')))
                    .delimited_by(Token::Ctrl('('), Token::Ctrl(')')),
            )
            .then(
                just(Token::Op("->".to_string()))
                    .ignore_then(type_parser())
                    .or_not(),
            )
            .then(block.clone())
            .map_with_span(|((((export, name), params), type_), body), span| {
                ast::TopLevelItem::Function(ast::Function {
                    span,
                    params,
                    export: export.is_some(),
                    name,
                    type_,
                    body,
                })
            })
            .boxed();

        let global = just(Token::Global)
            .ignore_then(just(Token::Mut).or_not())
            .then(identifier.clone())
            .then(just(Token::Ctrl(':')).ignore_then(type_parser()).or_not())
            .then(just(Token::Op("=".to_string())).ignore_then(expression.clone()))
            .then_ignore(just(Token::Ctrl(';')))
            .map_with_span(|(((mutable, name), type_), value), span| {
                ast::TopLevelItem::GlobalVar(ast::GlobalVar {
                    name,
                    type_,
                    value,
                    mutable: mutable.is_some(),
                    span,
                })
            })
            .boxed();

        let data_i8 = just(Token::Ident("i8".to_string()))
            .to(ast::DataType::I8)
            .or(just(Token::Ident("i16".to_string())).to(ast::DataType::I16))
            .or(just(Token::Ident("i32".to_string())).to(ast::DataType::I32))
            .or(just(Token::Ident("i64".to_string())).to(ast::DataType::I64))
            .or(just(Token::Ident("f32".to_string())).to(ast::DataType::F32))
            .or(just(Token::Ident("f64".to_string())).to(ast::DataType::F64))
            .then(
                expression
                    .clone()
                    .separated_by(just(Token::Ctrl(',')))
                    .delimited_by(Token::Ctrl('('), Token::Ctrl(')')),
            )
            .map(|(type_, values)| ast::DataValues::Array { type_, values });

        let data_string = string.map(|s| ast::DataValues::String(s));

        let data = just(Token::Data)
            .ignore_then(expression.clone())
            .then(
                data_i8
                    .or(data_string)
                    .repeated()
                    .delimited_by(Token::Ctrl('{'), Token::Ctrl('}')),
            )
            .map(|(offset, data)| {
                ast::TopLevelItem::Data(ast::Data {
                    offset: Box::new(offset),
                    data,
                })
            })
            .boxed();

        import.or(function).or(global).or(data).boxed()
    };

    top_level_item.repeated().then_ignore(end()).map(|items| {
        let mut script = ast::Script {
            imports: Vec::new(),
            global_vars: Vec::new(),
            functions: Vec::new(),
            data: Vec::new(),
        };
        for item in items {
            match item {
                ast::TopLevelItem::Import(i) => script.imports.push(i),
                ast::TopLevelItem::GlobalVar(v) => script.global_vars.push(v),
                ast::TopLevelItem::Function(f) => script.functions.push(f),
                ast::TopLevelItem::Data(d) => script.data.push(d),
            }
        }
        script
    })
}

fn type_parser() -> impl Parser<Token, ast::Type, Error = Simple<Token>> + Clone {
    filter_map(|span, tok| match tok {
        Token::Ident(id) if id == "i32" => Ok(ast::Type::I32),
        Token::Ident(id) if id == "i64" => Ok(ast::Type::I64),
        Token::Ident(id) if id == "f32" => Ok(ast::Type::F32),
        Token::Ident(id) if id == "f64" => Ok(ast::Type::F64),
        _ => Err(Simple::expected_input_found(
            span,
            vec![
                Token::Ident("i32".into()),
                Token::Ident("i64".into()),
                Token::Ident("f32".into()),
                Token::Ident("f64".into()),
            ],
            Some(tok),
        )),
    })
}
