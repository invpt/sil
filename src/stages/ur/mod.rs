use crate::{
    builtin::Builtin,
    char_reader::CharReader,
    error_stream::ErrorStream,
    primitive::{BinOp, PrimitiveOperation, PrimitiveValue, UnOp},
    span::Span,
    stages::ur::preds::{bpred, vpred},
    strings::Intern,
    unknown::UnknownQualifier,
};

use self::{preds::tpred, scoper::Scoper};

use super::tok::{Token, TokenKind, TokenizationError, Tokens};

mod parser;
mod preds;
mod resolver;
mod scoper;
mod typer;

#[derive(Debug, Clone)]
pub struct UrExpr<'s> {
    pub ty: UrType<'s>,
    pub kind: UrExprKind<'s>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum UrExprKind<'s> {
    Abstraction {
        id: UrAbstractionId,
        input_pat: Option<Box<UrExpr<'s>>>,
        output_ty: Option<Box<UrExpr<'s>>>,
        body_expr: Option<Box<UrExpr<'s>>>,
    },
    Selection {
        cond_expr: Box<UrExpr<'s>>,
        then_expr: Box<UrExpr<'s>>,
        else_expr: Option<Box<UrExpr<'s>>>,
    },
    Iteration {
        iter_expr: Box<UrExpr<'s>>,
        loop_expr: Box<UrExpr<'s>>,
    },
    Scope {
        stmts: Box<[UrStmt<'s>]>,
        expr: Option<Box<UrExpr<'s>>>,
    },
    Dictionary {
        impl_ty: Option<Box<UrExpr<'s>>>,
        stmts: Box<[UrStmt<'s>]>,
    },
    Tuple {
        items: Box<[UrItem<'s, UrExpr<'s>>]>,
    },
    Union {
        items: Box<[UrItem<'s, UrExpr<'s>>]>,
    },
    Lookup {
        symbol: Symbol<'s>,
    },
    Unknown {
        qual: UnknownQualifier,
        symbol: Symbol<'s>,
        type_: Option<Box<UrExpr<'s>>>,
    },
    PrimitiveOperation {
        operation: PrimitiveOperation<Box<UrExpr<'s>>>,
    },
    PrimitiveValue {
        value: PrimitiveValue<'s>,
    },
    Label {
        value: Intern<'s>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UrAbstractionId(usize);

#[derive(Debug, Clone)]
pub enum UrStmt<'s> {
    Def(UrDef<'s>),
    Expr(UrExpr<'s>),
}

impl<'s> UrStmt<'s> {
    fn span(&self) -> Span {
        match self {
            UrStmt::Def(def) => def.span,
            UrStmt::Expr(expr) => expr.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UrType<'s> {
    Abstraction(
        Option<UrAbstractionId>,
        Option<Box<UrType<'s>>>,
        Box<UrType<'s>>,
    ),
    Dictionary(Box<[UrDefTypeEntry<'s>]>),
    Derivative(UrAbstractionId, Box<UrType<'s>>),
    Tuple(Box<[UrItem<'s, UrType<'s>>]>),
    Union(Box<[UrType<'s>]>),
    Label(Intern<'s>),
    S32,
    F32,
    Str,
    Bool,

    Any(&'s [UrType<'s>]),
    All,
}

impl<'s> UrType<'s> {
    pub const NUMBER: UrType<'static> = UrType::Any(&[UrType::S32, UrType::F32]);
    pub const INTEGER: UrType<'static> = UrType::Any(&[UrType::S32]);
    pub const FLOAT: UrType<'static> = UrType::Any(&[UrType::F32]);

    pub fn compatible_with(&self, other: &UrType<'s>) -> bool {
        self == other
    }

    pub fn is_subtype(&self, other: &UrType<'s>) -> bool {
        match other {
            UrType::Abstraction(_, _, _) => todo!(),
            UrType::Dictionary(_) => todo!(),
            UrType::Derivative(_, _) => todo!(),
            UrType::Tuple(items) => todo!(),
            UrType::Union(tys) => {
                if let UrType::Union(self_tys) = self {
                    todo!()
                } else {
                    tys.iter().any(|ty| self.is_subtype(ty))
                }
            }
            UrType::Label(lab) => self == &UrType::Label(*lab),
            UrType::S32 => self == &UrType::S32,
            UrType::F32 => self == &UrType::F32,
            UrType::Str => self == &UrType::Str,
            UrType::Bool => self == &UrType::Bool,
            UrType::Any(tys) => tys.iter().any(|ty| self.is_subtype(ty)),
            UrType::All => true,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UrDefTypeEntry<'s> {
    pub name: Option<Symbol<'s>>,
    pub ty: UrType<'s>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UrItem<'s, T> {
    pub label: Option<Intern<'s>>,
    pub value: T,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Symbol<'s> {
    pub name: Intern<'s>,
    pub index: usize,
}

impl<'s> Symbol<'s> {
    fn builtin(builtin: Builtin) -> Symbol<'static> {
        Symbol {
            name: builtin.name(),
            index: builtin.index(),
        }
    }

    fn unknown(name: Intern<'s>) -> Symbol<'s> {
        Symbol {
            name,
            index: usize::MAX,
        }
    }

    fn is_unknown(&self) -> bool {
        self.index == usize::MAX
    }
}

#[derive(Debug, Clone)]
pub struct UrDef<'s> {
    pub name: Option<Symbol<'s>>,
    pub expr: UrExpr<'s>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Ur<'s> {
    pub root: Box<[UrDef<'s>]>,
}

impl<'s> Ur<'s> {
    pub fn of(tokens: Tokens<'s, impl CharReader>, errors: ErrorStream) -> Result<'s, Ur<'s>> {
        Ok(typer::r#type(
            resolver::resolve(parser::parse(tokens, errors)?, errors),
            errors,
        ))
    }
}

#[derive(Debug)]
pub enum UrError<'s> {
    Unexpected(Option<Token<'s>>),
    TokenizationError(TokenizationError),
    InvalidTrailingExpr,
    ImplNotAlone,
    UndefinedSymbol,
}

impl<'s> From<TokenizationError> for UrError<'s> {
    fn from(value: TokenizationError) -> Self {
        UrError::TokenizationError(value)
    }
}

type Result<'s, T> = std::result::Result<T, UrError<'s>>;
