//! The predicate functions used by the parser to make it easy to consume input.

use crate::stages::tok::Token;

#[allow(unused)]
pub fn either<'s, T>(
    a: impl Fn(&Token<'s>) -> Option<T>,
    b: impl Fn(&Token<'s>) -> Option<T>,
) -> impl Fn(&Token<'s>) -> Option<T> {
    move |t| a(t).or_else(|| b(t))
}

#[allow(unused)]
pub fn to_bpred<'s, T>(
    pred: impl Fn(&Token<'s>) -> Option<T>,
) -> impl Fn(&Token<'s>) -> Option<()> {
    move |t| match pred(t) {
        Some(_) => Some(()),
        None => None,
    }
}

macro_rules! bpred {
    ($($($pattern:pat_param)|+ $(if $guard:expr)?),* $(,)?) => {
        |t: &Token<'s>| match t.kind {
            $($($pattern)|+ $(if $guard)? => Some(()),)*
            _ => None,
        }
    };
}

macro_rules! tpred {
    ($($($pattern:pat_param)|+ $(if $guard:expr)?),* $(,)?) => {
        |t| match t.kind {
            $($($pattern)|+ $(if $guard)? => Some(t.clone()),)*
            _ => None,
        }
    };
}

macro_rules! vpred {
    ($(@$t:ident)? $($($pattern:pat_param)|+ $(if $guard:expr)? => $val:expr),* $(,)?) => {
        |t| {
            $(let $t = t;)?
            match t.kind {
                $($($pattern)|+ $(if $guard)? => {Some($val)})*
                _ => None,
            }
        }
    };
}

pub(super) use {bpred, tpred, vpred};
