//! WutLang parser

use std::fmt::format;

use chumsky::{error, extra, prelude::*};

type ParseErr<'a> = extra::Err<error::Rich<'a, char>>;

#[derive(Debug)]
pub struct SourceFile<'src> {
    declarations: Vec<GlobalDeclaration<'src>>,
}

impl<'src> SourceFile<'src> {
    pub fn parser() -> impl Parser<'src, &'src str, Self, ParseErr<'src>> {
        GlobalDeclaration::parser()
            .padded()
            .repeated()
            .collect()
            .map(|decls| Self {
                declarations: decls,
            })
            .padded()
    }

    pub fn dump(&self) -> String {
        self.declarations
            .iter()
            .map(|decl| decl.dump())
            .collect::<Vec<_>>()
            .join("\n\n")
    }
}

#[derive(Debug)]
pub enum GlobalDeclaration<'src> {
    Function(FunctionDeclaration<'src>),
}

impl<'src> GlobalDeclaration<'src> {
    pub fn parser() -> impl Parser<'src, &'src str, Self, ParseErr<'src>> {
        FunctionDeclaration::parser().map(Self::Function)
    }

    pub fn dump(&self) -> String {
        match self {
            GlobalDeclaration::Function(fndecl) => fndecl.dump(),
        }
    }
}

#[derive(Debug)]
pub struct FunctionDeclaration<'src> {
    pub ident: Ident<'src>,
    pub args: Vec<FnArg<'src>>,
    pub ret: Type<'src>,
    pub body: Vec<Statement<'src>>,
}

impl<'src> FunctionDeclaration<'src> {
    pub fn parser() -> impl Parser<'src, &'src str, Self, ParseErr<'src>> {
        let func_keyword = just("function").padded();
        let name = Ident::parser();
        let args = FnArg::parser()
            .padded()
            .separated_by(just(','))
            .collect()
            .delimited_by(just('('), just("):"));

        let ret = Type::parser();
        let body = Statement::parser()
            .repeated()
            .collect()
            .delimited_by(just('{'), just('}'));

        func_keyword
            .ignore_then(name)
            .then(args.padded())
            .then(ret.padded())
            .then(body)
            .map(|(((ident, args), ret), body)| Self {
                ident,
                args,
                ret,
                body,
            })
    }

    fn dump(&self) -> String {
        let body = if self.body.is_empty() {
            "".to_string()
        } else {
            self.body
                .iter()
                .map(|stmt| stmt.dump(Some(0)))
                .collect::<Vec<_>>()
                .join("\n")
                + "\n"
        };

        format!(
            "function {}({}): {} {{\n{}}}",
            self.ident.dump(),
            self.args
                .iter()
                .map(|arg| arg.dump())
                .collect::<Vec<_>>()
                .join(", "),
            self.ret.dump(),
            body
        )
    }
}

#[derive(Debug)]
pub struct FnArg<'src> {
    pub ty: Type<'src>,
    pub ident: Ident<'src>,
}

impl<'src> FnArg<'src> {
    pub fn parser() -> impl Parser<'src, &'src str, Self, ParseErr<'src>> {
        let ty = Type::parser().padded();
        let ident = Ident::parser().padded();

        ty.then(ident).map(|(ty, ident)| Self { ty, ident })
    }

    pub fn dump(&self) -> String {
        format!("{} {}", self.ty.dump(), self.ident.dump())
    }
}

#[derive(Debug)]
pub struct Type<'src>(pub &'src str);

impl<'src> Type<'src> {
    pub fn parser() -> impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone {
        text::ascii::ident().map(|id| Self(id))
    }

    pub fn dump(&self) -> String {
        self.0.to_string()
    }
}

#[derive(Debug)]
pub struct Ident<'src>(pub &'src str);

impl<'src> Ident<'src> {
    pub fn parser() -> impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone {
        text::ascii::ident().map(|id| Self(id))
    }

    pub fn dump(&self) -> String {
        self.0.to_string()
    }
}

#[derive(Debug)]
pub enum Statement<'src> {
    VarDecl {
        ty: Type<'src>,
        ident: Ident<'src>,
        init: Expr<'src>,
    },
    VarAssign {
        ident: Ident<'src>,
        val: Expr<'src>,
    },
    Return {
        val: Option<Expr<'src>>,
    },
    Expr {
        expr: Expr<'src>,
    },
    While {
        cond: Expr<'src>,
        body: Vec<Statement<'src>>,
    },
    DoWhile {
        cond: Expr<'src>,
        body: Vec<Statement<'src>>,
    },
    For {
        init: Option<(Type<'src>, Ident<'src>, Expr<'src>)>,
        cond: Option<Expr<'src>>,
        incr: Option<Box<Statement<'src>>>,
        body: Vec<Statement<'src>>,
    },
    Branch {
        cond: Expr<'src>,
        yes: Vec<Statement<'src>>,
        no: Vec<Statement<'src>>,
    },
    Block(Vec<Statement<'src>>),
}

impl<'src> Statement<'src> {
    const INDENT: &'static str = "    ";

    pub fn parser() -> impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone {
        recursive(|statement| {
            choice((
                Self::parse_block(statement.clone()).map(Self::Block),
                Self::parse_return(),
                Self::parse_do_while(statement.clone()),
                Self::parse_while(statement.clone()),
                Self::parse_for(statement.clone()),
                Self::parse_branch(statement),
                Self::parse_vardecl(),
                Self::parse_varassign(),
                Self::parse_expr(),
            ))
            .padded()
            .labelled("Statement")
        })
    }

    fn parse_vardecl() -> impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone {
        Type::parser()
            .padded()
            .then(Ident::parser().padded())
            .then_ignore(just('=').padded())
            .then(Expr::parser().padded())
            .then_ignore(just(';'))
            .map(|((ty, ident), init)| Self::VarDecl { ty, ident, init })
    }

    fn parse_varassign() -> impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone {
        Ident::parser()
            .padded()
            .then_ignore(just('=').padded())
            .then(Expr::parser().padded())
            .then_ignore(just(';'))
            .map(|(ident, val)| Self::VarAssign { ident, val })
    }
    fn parse_return() -> impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone {
        just("return")
            .padded()
            .ignore_then(Expr::parser().padded().or_not())
            .then_ignore(just(';'))
            .map(|val| Self::Return { val })
    }
    fn parse_expr() -> impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone {
        Expr::parser()
            .padded()
            .then_ignore(just(';'))
            .map(|expr| Self::Expr { expr })
    }

    fn parse_block(
        statement: impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone,
    ) -> impl Parser<'src, &'src str, Vec<Self>, ParseErr<'src>> + Clone {
        statement
            .repeated()
            .collect()
            .delimited_by(just('{').padded(), just('}').padded())
            .labelled("Block")
    }

    fn parse_while(
        statement: impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone,
    ) -> impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone {
        just("while")
            .padded()
            .ignore_then(Expr::parser().delimited_by(just('('), just(')')))
            .then(Self::parse_block(statement))
            .map(|(cond, body)| Self::While { cond, body })
            .padded()
    }

    fn parse_do_while(
        statement: impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone,
    ) -> impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone {
        just("do")
            .padded()
            .ignore_then(Self::parse_block(statement))
            .then_ignore(just("while"))
            .then(Expr::parser().delimited_by(just('('), just(')')))
            .padded()
            .map(|(body, cond)| Self::DoWhile { cond, body })
    }

    fn parse_for(
        statement: impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone,
    ) -> impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone {
        let init = Type::parser()
            .padded()
            .then(Ident::parser().padded())
            .then_ignore(just('=').padded())
            .then(Expr::parser().padded())
            .map(|((ty, ident), val)| (ty, ident, val));

        just("for")
            .padded()
            .ignore_then(just('(').padded())
            // Initializer
            .ignore_then(init.padded().or_not())
            .then_ignore(just(';').padded())
            // Condition
            .then(Expr::parser().or_not())
            .then_ignore(just(';').padded())
            // Increment
            .then(statement.clone().padded().or_not())
            .then_ignore(just(')').padded())
            // Body
            .then(Self::parse_block(statement))
            .padded()
            .map(|(((init, cond), incr), body)| Self::For {
                init,
                cond,
                incr: incr.map(Box::new),
                body,
            })
    }

    fn parse_branch(
        statement: impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone,
    ) -> impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone {
        let el = just("else")
            .padded()
            .ignore_then(Self::parse_block(statement.clone()).padded());

        just("if")
            .padded()
            .ignore_then(Expr::parser().padded().delimited_by(just('('), just(')')))
            .padded()
            .then(Self::parse_block(statement).padded())
            .then(el.or_not())
            .map(|((cond, yes), no)| Self::Branch {
                cond,
                yes,
                no: no.unwrap_or_default(),
            })
    }

    pub fn dump(&self, indent: Option<usize>) -> String {
        let mut indent_str = String::new();

        match indent {
            Some(indent) => {
                for _ in 0..(indent + 1) {
                    indent_str += Self::INDENT;
                }
            }
            None => (),
        }

        match self {
            Statement::VarDecl { ty, ident, init } => {
                format!(
                    "{}{} {} = {};",
                    indent_str,
                    ty.dump(),
                    ident.dump(),
                    init.dump()
                )
            }
            Statement::VarAssign { ident, val } => {
                format!("{}{} = {};", indent_str, ident.dump(), val.dump())
            }
            Statement::Return { val } => match val {
                Some(v) => format!("{}return {};", indent_str, v.dump()),
                None => format!("{}return;", indent_str),
            },
            Statement::Expr { expr } => format!("{}{};", indent_str, expr.dump()),
            Statement::Block(statements) => format!(
                "{}{}",
                indent_str,
                Self::dump_block(statements, &indent_str, indent)
            ),
            Statement::While { cond, body } => format!(
                "{}while({}) {}",
                indent_str,
                cond.dump(),
                Self::dump_block(body, &indent_str, indent)
            ),
            Statement::DoWhile { cond, body } => format!(
                "{}do {} while({})",
                indent_str,
                Self::dump_block(body, &indent_str, indent),
                cond.dump()
            ),
            Statement::For {
                init,
                cond,
                incr,
                body,
            } => {
                let init = match init {
                    Some((ty, ident, val)) => {
                        format!("{} {} = {}", ty.dump(), ident.dump(), val.dump())
                    }
                    None => "".to_string(),
                };

                let cond = match cond {
                    Some(expr) => expr.dump(),
                    None => "".to_string(),
                };

                let incr = match incr {
                    Some(expr) => expr.dump(None),
                    None => "".to_string(),
                };
                format!(
                    "{}for({}; {}; {}) {}",
                    indent_str,
                    init,
                    cond,
                    incr,
                    Self::dump_block(body, &indent_str, indent)
                )
            }
            Statement::Branch { cond, yes, no } => {
                if no.is_empty() {
                    format!(
                        "{}if({}) {}",
                        indent_str,
                        cond.dump(),
                        Self::dump_block(yes, &indent_str, indent)
                    )
                } else {
                    format!(
                        "{}if({}) {} else {}",
                        indent_str,
                        cond.dump(),
                        Self::dump_block(yes, &indent_str, indent),
                        Self::dump_block(no, &indent_str, indent)
                    )
                }
            }
        }
    }

    fn dump_block<'a>(block: &[Statement<'a>], indent_str: &str, indent: Option<usize>) -> String {
        format!(
            "{{\n{}\n{}}}",
            block
                .iter()
                .map(|stmt| stmt.dump(indent.map(|i| i + 1)))
                .collect::<Vec<_>>()
                .join("\n"),
            indent_str
        )
    }
}

#[derive(Debug)]
pub enum Expr<'src> {
    VarRead(Ident<'src>),
    Literal(Literal),
    BinOp(Box<Expr<'src>>, BinOperator, Box<Expr<'src>>),
    UnOp(UnOperator, Box<Expr<'src>>),
    FnCall(Ident<'src>, Vec<Expr<'src>>),
}

impl<'src> Expr<'src> {
    fn binop_parser(
        atom: impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone + 'src,
        filter: impl Fn(&BinOperator) -> bool + 'src,
    ) -> Boxed<'src, 'src, &'src str, Self, ParseErr<'src>> {
        atom.clone()
            .foldl(
                BinOperator::parser()
                    .filter(filter)
                    .padded()
                    .then(atom)
                    .repeated(),
                |lhs, (binop, rhs)| Self::BinOp(Box::new(lhs), binop, Box::new(rhs)),
            )
            .boxed()
    }

    pub fn parser() -> impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone {
        recursive(|expr| {
            let atom = choice((
                Self::parse_fncall(expr.clone()),
                Self::parse_literal(),
                Self::parse_varread(),
                expr.delimited_by(just('('), just(')')),
            ))
            .padded();

            let unary = UnOperator::parser()
                .padded()
                .repeated()
                .foldr(atom, |op, rhs| Self::UnOp(op, Box::new(rhs)));

            let product = Self::binop_parser(unary, |op| {
                *op == BinOperator::Mul || *op == BinOperator::Div || *op == BinOperator::Mod
            });

            let sum = Self::binop_parser(product, |op| {
                *op == BinOperator::Add || *op == BinOperator::Sub
            });

            let xor = Self::binop_parser(sum, |op| *op == BinOperator::Xor);

            let cmp = Self::binop_parser(xor, |op| {
                matches!(
                    op,
                    BinOperator::Eq
                        | BinOperator::Ne
                        | BinOperator::Lt
                        | BinOperator::Gt
                        | BinOperator::Le
                        | BinOperator::Ge
                )
            });

            let log_and = Self::binop_parser(cmp, |op| *op == BinOperator::LogAnd);

            let log_or = Self::binop_parser(log_and, |op| *op == BinOperator::LogOr);

            log_or
        })
    }

    pub fn parse_varread() -> impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone {
        Ident::parser().padded().map(Self::VarRead)
    }
    pub fn parse_literal() -> impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone {
        Literal::parser().padded().map(Self::Literal)
    }
    pub fn parse_fncall(
        expr: impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone,
    ) -> impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone {
        Ident::parser()
            .then(
                expr.separated_by(just(',').padded())
                    .collect()
                    .delimited_by(just('('), just(')')),
            )
            .map(|(ident, args)| Self::FnCall(ident, args))
    }

    pub fn dump(&self) -> String {
        match self {
            Expr::VarRead(ident) => ident.dump(),
            Expr::Literal(literal) => literal.dump(),
            Expr::BinOp(l, op, r) => format!("({} {} {})", l.dump(), op.dump(), r.dump()),
            Expr::UnOp(op, expr) => format!("{}({})", op.dump(), expr.dump()),
            Expr::FnCall(ident, args) => format!(
                "{}({})",
                ident.dump(),
                args.iter()
                    .map(|arg| arg.dump())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LogOr,
    LogAnd,
    Xor,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

impl BinOperator {
    pub fn parser<'src>() -> impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone {
        choice((
            just('+').to(Self::Add),
            just('-').to(Self::Sub),
            just('*').to(Self::Mul),
            just('/').to(Self::Div),
            just('%').to(Self::Mod),
            just("||").to(Self::LogOr),
            just("&&").to(Self::LogAnd),
            just('^').to(Self::Xor),
            just("==").to(Self::Eq),
            just("!=").to(Self::Ne),
            just('<').to(Self::Lt),
            just('>').to(Self::Gt),
            just("<=").to(Self::Le),
            just(">=").to(Self::Ge),
        ))
        .labelled("binary operator")
    }

    pub fn dump(&self) -> String {
        match self {
            BinOperator::Add => "+",
            BinOperator::Sub => "-",
            BinOperator::Mul => "*",
            BinOperator::Div => "/",
            BinOperator::Mod => "%",
            BinOperator::LogOr => "||",
            BinOperator::LogAnd => "&&",
            BinOperator::Xor => "^",
            BinOperator::Eq => "==",
            BinOperator::Ne => "!=",
            BinOperator::Lt => "<",
            BinOperator::Gt => ">",
            BinOperator::Le => "<=",
            BinOperator::Ge => ">=",
        }
        .to_string()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnOperator {
    Neg,
    Not,
}

impl UnOperator {
    pub fn parser<'src>() -> impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone {
        choice((just('-').to(Self::Neg), just('!').to(Self::Not))).labelled("unary operator")
    }

    pub fn dump(&self) -> String {
        match self {
            UnOperator::Neg => "-",
            UnOperator::Not => "!",
        }
        .to_string()
    }
}

#[derive(Debug)]
pub enum Literal {
    Int(u64),
    Float(f64),
    Bool(bool),
}

impl Literal {
    pub fn parser<'src>() -> impl Parser<'src, &'src str, Self, ParseErr<'src>> + Clone {
        enum LitFmt {
            Dec,
            Hex,
            Bin,
        }

        let litbool = just("true")
            .to(true)
            .or(just("false").to(false))
            .map(Self::Bool);

        let litfloat = digits().then_ignore(just('.')).then(digits()).map(|flt| {
            Self::Float(
                <f64 as core::str::FromStr>::from_str(format!("{}.{}", flt.0, flt.1).as_str())
                    .unwrap(),
            )
        });

        let litint = digits().map(|int| Self::Int(int.parse::<u64>().unwrap()));

        choice((litbool, litfloat, litint)).labelled("literal")
    }

    pub fn dump(&self) -> String {
        match self {
            Literal::Int(x) => x.to_string(),
            Literal::Float(x) => x.to_string(),
            Literal::Bool(x) => x.to_string(),
        }
    }
}

fn digits<'src>() -> impl Parser<'src, &'src str, String, ParseErr<'src>> + Clone {
    one_of("0123456789")
        .repeated()
        .at_least(1)
        .collect::<String>()
}
