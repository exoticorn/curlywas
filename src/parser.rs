use crate::ast;
use anyhow::Result;
use ariadne::{Color, Fmt, Label, Report, ReportKind};
use chumsky::prelude::*;
use std::{
    fmt,
    fs::File,
    io::Read,
    ops::Range,
    path::{Path, PathBuf},
};

pub type Span = (usize, Range<usize>);

pub struct SourceFile {
    source: ariadne::Source,
    path: PathBuf,
}

pub struct Sources(Vec<SourceFile>);

impl Sources {
    pub fn new() -> Sources {
        Sources(Vec::new())
    }

    pub fn add(&mut self, path: &Path) -> Result<(usize, bool)> {
        let canonical = path.canonicalize()?;
        for (index, source) in self.0.iter().enumerate() {
            if source.path.canonicalize()? == canonical {
                return Ok((index, false));
            }
        }
        let mut source = String::new();
        File::open(path)?.read_to_string(&mut source)?;
        self.0.push(SourceFile {
            source: ariadne::Source::from(source),
            path: path.to_path_buf(),
        });
        Ok((self.0.len() - 1, true))
    }
}

impl std::ops::Index<usize> for Sources {
    type Output = SourceFile;
    fn index(&self, idx: usize) -> &SourceFile {
        &self.0[idx]
    }
}

impl ariadne::Cache<usize> for &Sources {
    fn fetch(&mut self, id: &usize) -> Result<&ariadne::Source, Box<dyn std::fmt::Debug + '_>> {
        Ok(&self.0[*id].source)
    }

    fn display<'a>(&self, id: &'a usize) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new(self.0[*id].path.clone().display().to_string()))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Token {
    Fn,
    Let,
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
    Ident(String),
    Str(String),
    Int(i32),
    Int64(i64),
    IntFloat(i32),
    Float(String),
    Float64(String),
    Op(String),
    Ctrl(char),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Fn => write!(f, "fn"),
            Token::Let => write!(f, "let"),
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
            Token::Ident(s) => write!(f, "{}", s),
            Token::Str(s) => write!(f, "{:?}", s),
            Token::Int(v) => write!(f, "{}", v),
            Token::Int64(v) => write!(f, "{}", v),
            Token::IntFloat(v) => write!(f, "{}_f", v),
            Token::Float(v) => write!(f, "{}", v),
            Token::Float64(v) => write!(f, "{}", v),
            Token::Op(s) => write!(f, "{}", s),
            Token::Ctrl(c) => write!(f, "{}", c),
        }
    }
}

type SourceStream<It> = chumsky::Stream<'static, char, Span, It>;
type TokenStream<It> = chumsky::Stream<'static, Token, Span, It>;

pub fn parse(sources: &Sources, source_id: usize) -> Result<ast::Script, ()> {
    let source = &sources[source_id].source;
    let source_stream = SourceStream::from_iter(
        (source_id, source.len()..source.len() + 1),
        Box::new(
            // this is a bit of an ugly hack
            // once the next version of ariadne is released
            // use the source string chars as input
            // and only create the ariadne::Source lazily when printing errors
            source
                .lines()
                .flat_map(|l| {
                    let mut chars: Vec<char> = l.chars().collect();
                    while chars.len() + 1 < l.len() {
                        chars.push(' ');
                    }
                    if chars.len() < l.len() {
                        chars.push('\n');
                    }
                    chars.into_iter()
                })
                .enumerate()
                .map(|(index, char)| (char, (source_id, index..(index + 1)))),
        ),
    );

    let tokens = match lexer().parse(source_stream) {
        Ok(tokens) => tokens,
        Err(errors) => {
            report_errors(
                errors
                    .into_iter()
                    .map(|e| e.map(|c| c.to_string()))
                    .collect(),
                sources,
            );
            return Err(());
        }
    };

    let script = match script_parser().parse(TokenStream::from_iter(
        (source_id, source.len()..source.len() + 1),
        tokens.into_iter(),
    )) {
        Ok(script) => script,
        Err(errors) => {
            report_errors(
                errors
                    .into_iter()
                    .map(|e| e.map(|t| t.to_string()))
                    .collect(),
                sources,
            );
            return Err(());
        }
    };
    Ok(script)
}

fn report_errors(errors: Vec<Simple<String, Span>>, sources: &Sources) {
    for error in errors {
        let report = Report::build(ReportKind::Error, error.span().0, error.span().1.start());

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
                            .map(|x| x.as_deref().unwrap_or("EOF"))
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

        report.finish().eprint(sources).unwrap();
    }
}

type LexerError = Simple<char, Span>;
fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = LexerError> {
    let float64 = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)))
        .then_ignore(just("f64"))
        .collect::<String>()
        .map(Token::Float64);

    let float = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)))
        .collect::<String>()
        .map(Token::Float);

    let integer = just::<_, _, LexerError>("0x")
        .ignore_then(text::int(16))
        .try_map(|n, span| {
            u64::from_str_radix(&n, 16).map_err(|err| LexerError::custom(span, err.to_string()))
        })
        .or(text::int(10).try_map(|n: String, span: Span| {
            n.parse::<u64>()
                .map_err(|err| LexerError::custom(span, err.to_string()))
        }))
        .boxed();

    let int64 = integer
        .clone()
        .then_ignore(just("i64"))
        .map(|n| Token::Int64(n as i64));

    let int_float = integer
        .clone()
        .then_ignore(just("_f"))
        .map(|n| Token::IntFloat(n as i32));

    let int = integer.try_map(|n, span| {
        u32::try_from(n)
            .map(|n| Token::Int(n as i32))
            .map_err(|err| LexerError::custom(span, err.to_string()))
    });

    let str_ = just('\\')
        .then(any())
        .map(|t| vec![t.0, t.1])
        .or(none_of("\"").map(|c| vec![c]))
        .repeated()
        .flatten()
        .delimited_by(just('"'), just('"'))
        .collect::<String>()
        .map(|s| Token::Str(parse_string_escapes(s)));

    let char_ = just('\\')
        .then(any())
        .map(|t| vec![t.0, t.1])
        .or(none_of("\'").map(|c| vec![c]))
        .repeated()
        .flatten()
        .delimited_by(just('\''), just('\''))
        .collect::<String>()
        .map(|s| {
            let s = parse_string_escapes(s);
            let mut value = 0;
            for (i, c) in s.chars().enumerate() {
                // TODO: generate error on overflow
                if i < 4 {
                    value |= (c as u32) << (i * 8);
                }
            }
            Token::Int(value as i32)
        });

    let op = choice((
        just("#/"),
        just("#%"),
        just("<<"),
        just(">>"),
        just("#>>"),
        just(">="),
        just("<="),
        just("=="),
        just("!="),
        just("#>="),
        just("#<="),
        just("#<"),
        just("#>"),
        just("->"),
        just(":="),
        just("<|"),
    ))
    .map(|s| s.to_string())
    .or(one_of("+-*/%&^|<=>").map(|s: char| s.to_string()))
    .map(Token::Op)
    .boxed();

    let ctrl = one_of("(){};,:?!$").map(Token::Ctrl);

    fn ident() -> impl Parser<char, String, Error = LexerError> + Copy + Clone {
        filter(|c: &char| c.is_ascii_alphabetic() || *c == '_')
            .map(Some)
            .chain::<char, Vec<_>, _>(
                filter(|c: &char| c.is_ascii_alphanumeric() || *c == '_' || *c == '.').repeated(),
            )
            .collect()
    }

    let ident = ident().map(|ident: String| match ident.as_str() {
        "fn" => Token::Fn,
        "let" => Token::Let,
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
        _ => Token::Ident(ident),
    });

    let single_line = just("//").then_ignore(take_until(text::newline()));

    let multi_line = just("/*").then_ignore(take_until(just("*/")));

    let comment = single_line.or(multi_line);

    let token = choice((
        float, float64, int64, int_float, int, str_, char_, op, ctrl, ident,
    ))
    .recover_with(skip_then_retry_until([]));

    token
        .map_with_span(|tok, span| (tok, span))
        .padded()
        .padded_by(comment.padded().repeated())
        .repeated()
        .boxed()
}

fn map_token<O>(
    f: impl Fn(&Token, &Span) -> Option<O> + 'static + Clone,
) -> impl Parser<Token, O, Error = ScriptError> + Clone {
    filter_map(move |span, tok: Token| {
        if let Some(output) = f(&tok, &span) {
            Ok(output)
        } else {
            Err(ScriptError::expected_input_found(
                span,
                Vec::new(),
                Some(tok),
            ))
        }
    })
}

fn parse_string_escapes(s: String) -> String {
    let mut result = String::new();
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c != '\\' {
            result.push(c);
        } else if let Some(c) = chars.next() {
            match c {
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    let mut number = c.to_string();
                    if let Some('0'..='9' | 'a'..='f' | 'A'..='F') = chars.peek() {
                        number.push(chars.next().unwrap());
                    }
                    result.push(u8::from_str_radix(&number, 16).unwrap() as char);
                }
                'n' => result.push('\n'),
                'r' => result.push('\r'),
                't' => result.push('\t'),
                other => result.push(other),
            }
        } else {
            result.push('\\');
        }
    }
    result
}

type ScriptError = Simple<Token, Span>;
fn script_parser() -> impl Parser<Token, ast::Script, Error = ScriptError> + Clone {
    let identifier = filter_map(|span, tok| match tok {
        Token::Ident(id) => Ok(id),
        _ => Err(ScriptError::expected_input_found(
            span,
            Vec::new(),
            Some(tok),
        )),
    })
    .labelled("identifier");

    let integer = map_token(|tok, _| match tok {
        Token::Int(v) => Some(*v),
        _ => None,
    });

    let string = map_token(|tok, _| match tok {
        Token::Str(s) => Some(s.clone()),
        _ => None,
    });

    let product_op = just(Token::Op("*".to_string()))
        .to(ast::BinOp::Mul)
        .or(just(Token::Op("/".to_string())).to(ast::BinOp::Div))
        .or(just(Token::Op("#/".to_string())).to(ast::BinOp::DivU))
        .or(just(Token::Op("%".to_string())).to(ast::BinOp::Rem))
        .or(just(Token::Op("#%".to_string())).to(ast::BinOp::RemU))
        .boxed();
    let sum_op = just(Token::Op("+".to_string()))
        .to(ast::BinOp::Add)
        .or(just(Token::Op("-".to_string())).to(ast::BinOp::Sub))
        .boxed();
    let shift_op = just(Token::Op("<<".to_string()))
        .to(ast::BinOp::Shl)
        .or(just(Token::Op("#>>".to_string())).to(ast::BinOp::ShrU))
        .or(just(Token::Op(">>".to_string())).to(ast::BinOp::ShrS))
        .boxed();
    let bit_op = just(Token::Op("&".to_string()))
        .to(ast::BinOp::And)
        .or(just(Token::Op("|".to_string())).to(ast::BinOp::Or))
        .or(just(Token::Op("^".to_string())).to(ast::BinOp::Xor))
        .boxed();

    let mut expression_out = None;
    let block = recursive(|block| {
        let mut block_expression = None;
        let expression = recursive(|expression| {
            let val = map_token(|tok, span| match tok {
                Token::Int(v) => Some(ast::Expr::I32Const(*v)),
                Token::Int64(v) => Some(ast::Expr::I64Const(*v)),
                Token::IntFloat(v) => Some(ast::Expr::Cast {
                    value: Box::new(ast::Expr::I32Const(*v).with_span(span.clone())),
                    type_: ast::Type::F32,
                }),
                Token::Float(v) => Some(ast::Expr::F32Const(v.parse().unwrap())),
                Token::Float64(v) => Some(ast::Expr::F64Const(v.parse().unwrap())),
                _ => None,
            })
            .labelled("value");

            let variable = filter_map(|span, tok| match tok {
                Token::Ident(id) => Ok(ast::Expr::Variable {
                    name: id,
                    local_id: None,
                }),
                _ => Err(ScriptError::expected_input_found(
                    span,
                    Vec::new(),
                    Some(tok),
                )),
            })
            .labelled("variable");

            let local_tee = identifier
                .then(just(Token::Op(":=".to_string())).ignore_then(expression.clone()))
                .map(|(name, expr)| ast::Expr::LocalTee {
                    name,
                    value: Box::new(expr),
                    local_id: None,
                })
                .boxed();

            let local_tee_op = identifier
                .then(
                    product_op
                        .clone()
                        .or(sum_op.clone())
                        .or(shift_op.clone())
                        .or(bit_op.clone()),
                )
                .then_ignore(just(Token::Op(":=".to_string())))
                .then(expression.clone())
                .map_with_span(|((name, op), expr), span| ast::Expr::LocalTee {
                    name: name.clone(),
                    value: Box::new(
                        ast::Expr::BinOp {
                            left: Box::new(
                                ast::Expr::Variable {
                                    name,
                                    local_id: None,
                                }
                                .with_span(span.clone()),
                            ),
                            right: Box::new(expr),
                            op,
                        }
                        .with_span(span),
                    ),
                    local_id: None,
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

            let if_expr = recursive::<_, ast::Expr, _, _, _>(|if_expr| {
                just(Token::If)
                    .ignore_then(expression.clone())
                    .then(block.clone())
                    .then(
                        just(Token::Else)
                            .ignore_then(
                                block
                                    .clone()
                                    .or(if_expr.map_with_span(|expr, span| expr.with_span(span))),
                            )
                            .or_not(),
                    )
                    .map(|((condition, if_true), if_false)| ast::Expr::If {
                        condition: Box::new(condition),
                        if_true: Box::new(if_true),
                        if_false: if_false.map(Box::new),
                    })
            });

            let block_expr = loop_expr.or(label_block_expr).or(if_expr).boxed();

            block_expression = Some(block_expr.clone());

            let branch = just(Token::Branch)
                .ignore_then(identifier)
                .map(ast::Expr::Branch);

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
                .then(identifier)
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
                    local_id: None,
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
                        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
                )
                .map(|((condition, if_true), if_false)| ast::Expr::Select {
                    condition: Box::new(condition),
                    if_true: Box::new(if_true),
                    if_false: Box::new(if_false),
                })
                .boxed();

            let function_call = identifier
                .then(
                    expression
                        .clone()
                        .separated_by(just(Token::Ctrl(',')))
                        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
                )
                .map(|(name, params)| ast::Expr::FuncCall { name, params })
                .boxed();

            let return_ = just(Token::Return)
                .ignore_then(expression.clone().or_not())
                .map(|value| ast::Expr::Return {
                    value: value.map(Box::new),
                });

            let atom = choice((
                val,
                function_call,
                local_tee,
                local_tee_op,
                variable,
                block_expr,
                branch,
                branch_if,
                let_,
                select,
                return_,
            ))
            .map_with_span(|expr, span| expr.with_span(span))
            .or(expression
                .clone()
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
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
                        let span = (span.0, span.1.start..acc.span.1.end);
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
                .or(just(Token::Ctrl('!')).to(ast::MemSize::Word))
                .or(just(Token::Ctrl('$')).to(ast::MemSize::Float));

            let mem_op = mem_size.then(op_cast.clone());

            fn make_memory_op(
                left: ast::Expression,
                peek_ops: Vec<(ast::MemSize, ast::Expression)>,
                poke_op: Option<((ast::MemSize, ast::Expression), ast::Expression)>,
            ) -> ast::Expression {
                let left = peek_ops.into_iter().fold(left, |left, (size, right)| {
                    let span = (left.span.0, left.span.1.start..right.span.1.end);
                    ast::Expr::Peek(ast::MemoryLocation {
                        span: span.clone(),
                        left: Box::new(left),
                        size,
                        right: Box::new(right),
                    })
                    .with_span(span)
                });
                if let Some(((size, right), value)) = poke_op {
                    let span = (left.span.0, left.span.1.start..value.span.1.end);
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

            let memory_op = op_cast
                .clone()
                .then(
                    mem_op
                        .clone()
                        .repeated()
                        .at_least(1)
                        .then(
                            just(Token::Op("=".to_string()))
                                .ignore_then(expression.clone())
                                .or_not(),
                        )
                        .or_not(),
                )
                .map(|(left, ops)| {
                    if let Some((mut peek_ops, poke_op)) = ops {
                        if let Some(value) = poke_op {
                            let poke_op = Some((peek_ops.pop().unwrap(), value));
                            make_memory_op(left, peek_ops, poke_op)
                        } else {
                            make_memory_op(left, peek_ops, None)
                        }
                    } else {
                        left
                    }
                })
                .boxed();

            let op_product = memory_op
                .clone()
                .then(product_op.clone().then(memory_op.clone()).repeated())
                .foldl(|left, (op, right)| {
                    let span = (left.span.0, left.span.1.start..right.span.1.end);
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
                .then(sum_op.clone().then(op_product.clone()).repeated())
                .foldl(|left, (op, right)| {
                    let span = (left.span.0, left.span.1.start..right.span.1.end);
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
                .then(shift_op.clone().then(op_sum.clone()).repeated())
                .foldl(|left, (op, right)| {
                    let span = (left.span.0, left.span.1.start..right.span.1.end);
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
                    let span = (left.span.0, left.span.1.start..right.span.1.end);
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
                .then(bit_op.clone().then(op_cmp.clone()).repeated())
                .foldl(|left, (op, right)| {
                    let span = (left.span.0, left.span.1.start..right.span.1.end);
                    ast::Expr::BinOp {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    }
                    .with_span(span)
                })
                .boxed();

            op_bit
                .clone()
                .then(
                    just(Token::Op("<|".to_string()))
                        .ignore_then(op_bit)
                        .repeated(),
                )
                .foldl(|left, right| {
                    let span = (left.span.0, left.span.1.start..right.span.1.end);
                    ast::Expr::First {
                        value: Box::new(left),
                        drop: Box::new(right),
                    }
                    .with_span(span)
                })
                .boxed()
        });

        expression_out = Some(expression.clone());

        let block_expression = block_expression.unwrap();

        let assign = identifier
            .then_ignore(just(Token::Op("=".to_string())))
            .then(expression.clone())
            .map(|(name, value)| ast::Expr::Assign {
                name,
                value: Box::new(value),
                local_id: None,
            })
            .map_with_span(|expr, span| expr.with_span(span))
            .boxed();

        let assign_op = identifier
            .then(
                product_op
                    .clone()
                    .or(sum_op.clone())
                    .or(shift_op.clone())
                    .or(bit_op.clone()),
            )
            .then_ignore(just(Token::Op("=".to_string())))
            .then(expression.clone())
            .map_with_span(|((name, op), value), span| {
                ast::Expr::Assign {
                    name: name.clone(),
                    value: Box::new(
                        ast::Expr::BinOp {
                            left: Box::new(
                                ast::Expr::Variable {
                                    name,
                                    local_id: None,
                                }
                                .with_span(span.clone()),
                            ),
                            right: Box::new(value),
                            op,
                        }
                        .with_span(span.clone()),
                    ),
                    local_id: None,
                }
                .with_span(span)
            })
            .boxed();

        block_expression
            .clone()
            .then(just(Token::Ctrl(';')).or_not())
            .map_with_span(|(expr, semi), span| (expr.with_span(span), semi.is_none()))
            .or(assign
                .or(assign_op)
                .or(expression.clone())
                .then(just(Token::Ctrl(';')).to(false)))
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
                    final_expression: final_expression.map(Box::new),
                }
                .with_span(span)
            })
            .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
            .boxed()
    });

    let expression = expression_out.unwrap();

    let top_level_item = {
        let import_memory = just(Token::Ident("memory".to_string()))
            .ignore_then(
                integer
                    .clone()
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
            )
            .map(|min_size| ast::ImportType::Memory(min_size as u32))
            .boxed();

        let import_global = just(Token::Global)
            .ignore_then(just(Token::Mut).or_not())
            .then(identifier)
            .then_ignore(just(Token::Ctrl(':')))
            .then(type_parser())
            .map(|((mut_opt, name), type_)| ast::ImportType::Variable {
                mutable: mut_opt.is_some(),
                name,
                type_,
            })
            .boxed();

        let import_function = just(Token::Fn)
            .ignore_then(identifier)
            .then(
                type_parser()
                    .separated_by(just(Token::Ctrl(',')))
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
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

        let import = just(Token::Ident("import".to_string()))
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
            .then_ignore(just(Token::Ctrl(':')))
            .then(type_parser())
            .boxed();

        let function = just(Token::Ident("export".to_string()))
            .or_not()
            .then(just(Token::Ident("start".to_string())).or_not())
            .then_ignore(just(Token::Fn))
            .then(identifier)
            .then(
                parameter
                    .separated_by(just(Token::Ctrl(',')))
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
            )
            .then(
                just(Token::Op("->".to_string()))
                    .ignore_then(type_parser())
                    .or_not(),
            )
            .then(block.clone())
            .map_with_span(|(((((export, start), name), params), type_), body), span| {
                ast::TopLevelItem::Function(ast::Function {
                    span,
                    params,
                    export: export.is_some(),
                    start: start.is_some(),
                    name,
                    type_,
                    body,
                    locals: ast::Locals::default(),
                })
            })
            .boxed();

        let global = just(Token::Global)
            .ignore_then(just(Token::Mut).or_not())
            .then(identifier)
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

        let global_const = just(Token::Ident("const".to_string()))
            .ignore_then(identifier)
            .then(just(Token::Ctrl(':')).ignore_then(type_parser()).or_not())
            .then(just(Token::Op("=".to_string())).ignore_then(expression.clone()))
            .then_ignore(just(Token::Ctrl(';')))
            .map_with_span(|((name, type_), value), span| {
                ast::TopLevelItem::Const(ast::GlobalConst {
                    name,
                    type_,
                    value,
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
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
            )
            .map(|(type_, values)| ast::DataValues::Array { type_, values });

        let data_string = string.clone().map(ast::DataValues::String);

        let data_file = just(Token::Ident("file".to_string()))
            .ignore_then(
                string
                    .clone()
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
            )
            .map(|s| ast::DataValues::File {
                path: s.into(),
                data: vec![],
            });

        let data = just(Token::Ident("data".to_string()))
            .ignore_then(expression.clone())
            .then(
                data_i8
                    .or(data_string)
                    .or(data_file)
                    .repeated()
                    .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}'))),
            )
            .map(|(offset, data)| {
                ast::TopLevelItem::Data(ast::Data {
                    offset: Box::new(offset),
                    data,
                })
            })
            .boxed();

        let include =
            just(Token::Ident("include".to_string())).ignore_then(string.clone().map_with_span(
                |path, span| ast::TopLevelItem::Include(ast::Include { span, path }),
            ));

        import
            .or(function)
            .or(global)
            .or(data)
            .or(include)
            .or(global_const)
            .boxed()
    };

    top_level_item.repeated().then_ignore(end()).map(|items| {
        let mut script = ast::Script::default();
        for item in items {
            match item {
                ast::TopLevelItem::Import(i) => script.imports.push(i),
                ast::TopLevelItem::GlobalVar(v) => script.global_vars.push(v),
                ast::TopLevelItem::Function(f) => script.functions.push(f),
                ast::TopLevelItem::Data(d) => script.data.push(d),
                ast::TopLevelItem::Include(i) => script.includes.push(i),
                ast::TopLevelItem::Const(c) => script.consts.push(c),
            }
        }
        script
    })
}

fn type_parser() -> impl Parser<Token, ast::Type, Error = ScriptError> + Clone {
    filter_map(|span, tok| match tok {
        Token::Ident(id) if id == "i32" => Ok(ast::Type::I32),
        Token::Ident(id) if id == "i64" => Ok(ast::Type::I64),
        Token::Ident(id) if id == "f32" => Ok(ast::Type::F32),
        Token::Ident(id) if id == "f64" => Ok(ast::Type::F64),
        _ => Err(ScriptError::expected_input_found(
            span,
            vec![
                Some(Token::Ident("i32".into())),
                Some(Token::Ident("i64".into())),
                Some(Token::Ident("f32".into())),
                Some(Token::Ident("f64".into())),
            ],
            Some(tok),
        )),
    })
}
