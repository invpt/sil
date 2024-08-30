//! The predicate functions used by the parser to make it easy to consume input.

use crate::stages::tok::Token;

pub fn either<'s, T>(
    a: impl Fn(&Token<'s>) -> Option<T>,
    b: impl Fn(&Token<'s>) -> Option<T>,
) -> impl Fn(&Token<'s>) -> Option<T> {
    move |t| a(t).or_else(|| b(t))
}

macro_rules! bpred {
    ($($($pattern:pat_param)|+ $(if $guard:expr)?),* $(,)?) => {
        |t: &Token<'s>| match t.kind {
            $($($pattern)|+ $(if $guard)? => Some(()),)*
            _ => None,
        }
    };
}

macro_rules! spred {
    ($($($pattern:pat_param)|+ $(if $guard:expr)?),* $(,)?) => {
        |t: &Token<'s>| match t.kind {
            $($($pattern)|+ $(if $guard)? => Some(t.span.start),)*
            _ => None,
        }
    };
}

macro_rules! epred {
    ($($($pattern:pat_param)|+ $(if $guard:expr)?),* $(,)?) => {
        |t: &Token<'s>| match t.kind {
            $($($pattern)|+ $(if $guard)? => Some(t.span.end),)*
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

pub(super) use {bpred, spred, epred, tpred, vpred};
