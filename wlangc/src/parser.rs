//! WutLang parser

use chumsky::{error, extra, prelude::*};

type ParseErr<'a> = extra::Err<error::Rich<'a, char>>;

#[derive(Debug)]
pub struct SourceFile<'a> {
    declarations: Vec<GlobalDeclaration<'a>>,
}

impl<'a> SourceFile<'a> {
    pub fn parser() -> impl Parser<'a, &'a str, Self, ParseErr<'a>> {
        GlobalDeclaration::parser()
            .padded()
            .repeated()
            .collect()
            .map(|decls| Self {
                declarations: decls,
            })
            .padded()
    }
}

#[derive(Debug)]
pub enum GlobalDeclaration<'a> {
    Function(FunctionDeclaration<'a>),
}

impl<'a> GlobalDeclaration<'a> {
    pub fn parser() -> impl Parser<'a, &'a str, Self, ParseErr<'a>> {
        FunctionDeclaration::parser().map(Self::Function)
    }
}

#[derive(Debug)]
pub struct FunctionDeclaration<'a> {
    pub name: Ident<'a>,
    pub ret: Type<'a>,
}

impl<'a> FunctionDeclaration<'a> {
    pub fn parser() -> impl Parser<'a, &'a str, Self, ParseErr<'a>> {
        let func_keyword = just("function").padded();
        let name = Ident::parser();
        let args = just("():");
        let ret = Type::parser();
        let body = regex(r"[a-zA-Z =0-9;\n\+]+").delimited_by(just("{"), just("}"));

        func_keyword
            .ignore_then(name)
            .then_ignore(args.padded())
            .then(ret.padded())
            .then_ignore(body)
            .map(|(name, ret)| Self { name, ret })
    }
}

#[derive(Debug)]
pub struct Type<'a>(pub &'a str);

impl<'a> Type<'a> {
    pub fn parser() -> impl Parser<'a, &'a str, Self, ParseErr<'a>> {
        text::ascii::ident().map(|id| Self(id))
    }
}

#[derive(Debug)]
pub struct Ident<'a>(pub &'a str);

impl<'a> Ident<'a> {
    pub fn parser() -> impl Parser<'a, &'a str, Self, ParseErr<'a>> {
        text::ascii::ident().map(|id| Self(id))
    }
}
