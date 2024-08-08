use std::io;

use crate::{
    char_reader::{CharReader, CharReaderSaver}, span::Span, strings::{Intern, Strings}
};

#[derive(Debug)]
pub struct TokenizationError {
    #[allow(unused)]
    pub kind: TokenizationErrorKind,
    #[allow(unused)]
    pub span: Option<Span>,
}

#[derive(Debug)]
pub enum TokenizationErrorKind {
    Unexpected,
    UnexpectedEof,
    #[allow(unused)]
    Io(io::Error),
}

impl From<io::Error> for TokenizationError {
    fn from(err: io::Error) -> Self {
        TokenizationError {
            kind: TokenizationErrorKind::Io(err),
            span: None,
        }
    }
}

type Result<T> = std::result::Result<T, TokenizationError>;

pub struct Tokens<'s, R> {
    chars: R,
    pub(crate) strings: &'s Strings,
    peek: Option<Token<'s>>,
}

#[derive(Debug, Clone)]
pub struct Token<'s> {
    pub kind: TokenKind<'s>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TokenKind<'s> {
    /* Keywords */
    Def,
    Use,
    Val,
    Var,
    Set,
    Type,
    Case,
    Else,
    Loop,
    In,

    /* Punctuation */
    Comma,
    Colon,
    ColonColon,
    Semicolon,
    Question,
    Bang,
    Pipe,
    Amp,
    Dollar,
    ThinArrow,
    FatArrow,
    ColonEqual,
    Eq,
    Neq,
    Gt,
    Lt,
    Geq,
    Leq,
    Shl,
    Shr,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Tilde,
    AmpAmp,
    PipePipe,
    Backslash,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    LeftArrow,

    Float(f64),
    Integer(u64),
    Name(Intern<'s>),
    Label(Intern<'s>),
    String(Intern<'s>),
}

impl<'i, 's, R: CharReader> Tokens<'s, R> {
    pub fn of(chars: R, strings: &'s Strings) -> Tokens<'s, R> {
        Tokens {
            chars,
            strings,
            peek: None,
        }
    }

    /// Reads the next token from the input stream.
    pub fn next(&mut self) -> Result<Option<Token<'s>>> {
        if let Some(peek) = self.peek.take() {
            return Ok(Some(peek));
        }

        while let Some((start, ch)) = self.chars.peek()? {
            return match ch {
                _ if ch.is_ascii_whitespace() => {
                    self.chars.next()?;
                    continue;
                }
                '$' => self.advance_single(TokenKind::Dollar),
                '?' => self.advance_single(TokenKind::Question),
                ',' => self.advance_single(TokenKind::Comma),
                ':' => self.advance_double(TokenKind::Colon, |ch| match ch {
                    ':' => Some(TokenKind::ColonColon),
                    '=' => Some(TokenKind::ColonEqual),
                    _ => None,
                }),
                ';' => self.advance_single(TokenKind::Semicolon),
                '!' => self.advance_double(TokenKind::Bang, |ch| match ch {
                    '=' => Some(TokenKind::Neq),
                    _ => None,
                }),
                '|' => self.advance_double(TokenKind::Pipe, |ch| match ch {
                    '|' => Some(TokenKind::PipePipe),
                    _ => None,
                }),
                '&' => self.advance_double(TokenKind::Amp, |ch| match ch {
                    '&' => Some(TokenKind::AmpAmp),
                    _ => None,
                }),
                '-' => self.advance_double(TokenKind::Minus, |ch| match ch {
                    '>' => Some(TokenKind::ThinArrow),
                    _ => None,
                }),
                '=' => self.advance_double(TokenKind::Eq, |ch| match ch {
                    '>' => Some(TokenKind::FatArrow),
                    _ => None,
                }),
                '>' => self.advance_double(TokenKind::Gt, |ch| match ch {
                    '>' => Some(TokenKind::Shr),
                    '=' => Some(TokenKind::Geq),
                    _ => None,
                }),
                '<' => self.advance_double(TokenKind::Lt, |ch| match ch {
                    '<' => Some(TokenKind::Shl),
                    '=' => Some(TokenKind::Leq),
                    '-' => Some(TokenKind::LeftArrow),
                    _ => None,
                }),
                '+' => self.advance_single(TokenKind::Plus),
                '*' => self.advance_single(TokenKind::Star),
                '%' => self.advance_single(TokenKind::Percent),
                '^' => self.advance_single(TokenKind::Caret),
                '~' => self.advance_single(TokenKind::Tilde),
                '\\' => self.advance_single(TokenKind::Backslash),
                '(' => self.advance_single(TokenKind::OpenParen),
                ')' => self.advance_single(TokenKind::CloseParen),
                '[' => self.advance_single(TokenKind::OpenBracket),
                ']' => self.advance_single(TokenKind::CloseBracket),
                '{' => self.advance_single(TokenKind::OpenBrace),
                '}' => self.advance_single(TokenKind::CloseBrace),
                '.' => self.label(),
                '"' => self.string(),
                '/' => {
                    self.chars.next()?;

                    if let Some((_, '/')) = self.chars.peek()? {
                        self.chars.next()?;
                        self.skip_line()?;
                        continue;
                    } else if let Some((_, '*')) = self.chars.peek()? {
                        self.chars.next()?;
                        self.block_comment()?;
                        continue;
                    } else {
                        Ok(Some(Token {
                            kind: TokenKind::Slash,
                            span: Span {
                                start,
                                end: start + ch.len_utf8(),
                            },
                        }))
                    }
                }
                _ if ch.is_alphabetic() || ch == '_' => self.name(),
                _ if ch.is_ascii_digit() => self.number(),
                _ => {
                    self.chars.next()?;
                    Err(TokenizationError {
                        kind: TokenizationErrorKind::Unexpected,
                        span: Some(Span {
                            start,
                            end: start + ch.len_utf8(),
                        }),
                    })
                }
            };
        }

        Ok(None)
    }

    pub fn peek(&mut self) -> Result<Option<&Token<'s>>> {
        if let Some(ref peek) = self.peek {
            Ok(Some(peek))
        } else {
            self.peek = self.next()?;
            Ok(self.peek.as_ref())
        }
    }

    fn skip_line(&mut self) -> Result<()> {
        while let Some((_, ch)) = self.chars.next()? {
            if ch == '\n' {
                break;
            }
        }

        Ok(())
    }

    fn block_comment(&mut self) -> Result<()> {
        let mut i = 1;
        while let Some((_, ch)) = self.chars.next()? {
            if ch == '*' {
                if let Some((_, '/')) = self.chars.next()? {
                    i -= 1;
                }
            } else if ch == '/' {
                if let Some((_, '*')) = self.chars.next()? {
                    i += 1;
                }
            }

            if i == 0 {
                break;
            }
        }

        Ok(())
    }

    fn string(&mut self) -> Result<Option<Token<'s>>> {
        let Some((start, ch)) = self.chars.peek()? else {
            return Err(TokenizationError {
                kind: TokenizationErrorKind::UnexpectedEof,
                span: None,
            })
        };
        if ch != '"' {
            return Err(TokenizationError {
                kind: TokenizationErrorKind::Unexpected,
                span: Some(Span {
                    start,
                    end: start + ch.len_utf8(),
                }),
            });
        }

        self.chars.next()?;

        let mut string = std::string::String::new();
        let mut slash = false;
        while let Some((curr, ch)) = self.chars.peek()? {
            if ch == '"' && !slash {
                break;
            }

            self.chars.next()?;
            if slash {
                match ch {
                    '"' => string += "\"",
                    '\0' => string += "\0",
                    't' => string += "\t",
                    'n' => string += "\n",
                    'r' => string += "\r",
                    '\\' => string += "\\",
                    _ => {
                        return Err(TokenizationError {
                            kind: TokenizationErrorKind::Unexpected,
                            span: Some(Span {
                                start: curr,
                                end: curr + ch.len_utf8(),
                            }),
                        })
                    }
                }
                slash = false
            } else if ch == '\\' {
                slash = true
            } else {
                string.push(ch);
            }
        }

        let Some((inner_end, end_ch)) = self.chars.next()? else {
            return Err(TokenizationError {
                kind: TokenizationErrorKind::UnexpectedEof,
                span: None,
            })
        };

        Ok(Some(Token {
            kind: TokenKind::String(self.strings.intern(string.into_boxed_str())),
            span: Span {
                start,
                end: inner_end + end_ch.len_utf8(),
            },
        }))
    }

    fn label(&mut self) -> Result<Option<Token<'s>>> {
        let Some((start, ch)) = self.chars.next()? else {
            return Err(TokenizationError {
                kind: TokenizationErrorKind::UnexpectedEof,
                span: None,
            })
        };
        if ch != '.' {
            return Err(TokenizationError {
                kind: TokenizationErrorKind::Unexpected,
                span: Some(Span {
                    start,
                    end: start + ch.len_utf8(),
                }),
            });
        }

        let (name, Span { end, .. }) = self.ident()?;

        Ok(Some(Token {
            kind: TokenKind::Label(name),
            span: Span { start, end },
        }))
    }

    fn name(&mut self) -> Result<Option<Token<'s>>> {
        let Some((start, ch)) = self.chars.peek()? else {
            return Err(TokenizationError {
                kind: TokenizationErrorKind::UnexpectedEof,
                span: None,
            })
        };
        if !ch.is_alphabetic() && ch != '_' {
            return Err(TokenizationError {
                kind: TokenizationErrorKind::Unexpected,
                span: Some(Span {
                    start,
                    end: start + ch.len_utf8(),
                }),
            });
        }

        let (name, span) = self.ident()?;

        Ok(Some(Token {
            kind: match &*name {
                "def" => TokenKind::Def,
                "use" => TokenKind::Use,
                "val" => TokenKind::Val,
                "var" => TokenKind::Var,
                "set" => TokenKind::Set,
                "type" => TokenKind::Type,
                "case" => TokenKind::Case,
                "else" => TokenKind::Else,
                "loop" => TokenKind::Loop,
                "in" => TokenKind::In,
                _ => TokenKind::Name(name),
            },
            span,
        }))
    }

    fn ident(&mut self) -> Result<(Intern<'s>, Span)> {
        let Some((start, _ch)) = self.chars.peek()? else {
            return Err(TokenizationError {
                kind: TokenizationErrorKind::UnexpectedEof,
                span: None,
            })
        };

        let mut saver = CharReaderSaver::new(&mut self.chars);

        while let Some((_, ch)) = saver.peek()? {
            if !ch.is_alphanumeric() && ch != '_' {
                break;
            } else {
                saver.next()?;
            }
        }

        let name = saver.finish();
        let end = start + name.len();

        Ok((
            self.strings.intern(name.into_boxed_str()),
            Span { start, end },
        ))
    }

    fn number(&mut self) -> Result<Option<Token<'s>>> {
        let Some((start, ch)) = self.chars.peek()? else { return Ok(None) };
        if !ch.is_ascii_digit() {
            return Ok(None);
        }

        let mut seen_point = false;

        let mut saver = CharReaderSaver::new(&mut self.chars);

        while let Some((_, ch)) = saver.peek()? {
            if !seen_point && ch == '.' {
                seen_point = true;
                saver.next()?;
            } else if !ch.is_ascii_digit() {
                break;
            } else {
                saver.next()?;
            }
        }

        let saved = saver.finish();
        let end = start + saved.len();

        if seen_point {
            let Ok(value) = saved.parse::<f64>() else {
                unreachable!("Compiler bug: Unexpected error from parse::<f64>()")
            };

            Ok(Some(Token {
                kind: TokenKind::Float(value),
                span: Span { start, end },
            }))
        } else {
            let Ok(value) = saved.parse::<u64>() else {
                unreachable!("Compiler bug: Unexpected error from parse::<u64>()")
            };

            Ok(Some(Token {
                kind: TokenKind::Integer(value),
                span: Span { start, end },
            }))
        }
    }

    fn advance_single(&mut self, kind: TokenKind<'s>) -> Result<Option<Token<'s>>> {
        let Some((start, ch)) = self.chars.next()? else { return Ok(None) };
        Ok(Some(Token {
            kind,
            span: Span {
                start,
                end: start + ch.len_utf8(),
            },
        }))
    }

    fn advance_double(
        &mut self,
        primary: TokenKind<'s>,
        secondary: impl FnOnce(char) -> Option<TokenKind<'s>>,
    ) -> Result<Option<Token<'s>>> {
        let Some((start, ch)) = self.chars.next()? else { return Ok(None) };
        if let Some((peek_start, peek)) = self.chars.peek()? {
            if let Some(sec) = secondary(peek) {
                self.chars.next()?;
                return Ok(Some(Token {
                    kind: sec,
                    span: Span {
                        start,
                        end: peek_start + peek.len_utf8(),
                    },
                }));
            }
        }

        Ok(Some(Token {
            kind: primary,
            span: Span {
                start,
                end: start + ch.len_utf8(),
            },
        }))
    }
}
