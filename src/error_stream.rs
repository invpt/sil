use crate::span::Span;

#[derive(Clone, Copy)]
pub struct ErrorStream {}

impl ErrorStream {
    pub fn new() -> ErrorStream {
        ErrorStream {}
    }

    pub fn error<T: std::fmt::Debug>(&self, error: T, span: Option<Span>) {
        panic!("ErrorStream: {error:?} {span:?}")
    }
}
