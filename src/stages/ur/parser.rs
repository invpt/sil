use crate::{char_reader::CharReader, error_stream::ErrorStream, stages::tok::Tokens};

use super::*;

pub fn parse<'s, R: CharReader>(tokens: Tokens<'s, R>, errors: ErrorStream) -> Result<'s, Ur<'s>> {
    Parser { abstraction_id_counter: 0, tokens, errors }.parse()
}

struct Parser<'s, R> {
    abstraction_id_counter: usize,
    tokens: Tokens<'s, R>,
    errors: ErrorStream,
}

impl<'s, R: CharReader> Parser<'s, R> {
    fn parse(mut self) -> Result<'s, Ur<'s>> {
        let mut defs = Vec::new();
        let mut just_reported = false;
        while let Some(tok) = self.tokens.peek()? {
            let TokenKind::Def = tok.kind else {
                if !just_reported {
                    self.errors
                        .error(UrError::Unexpected(Some(tok.clone())), Some(tok.span));
                    just_reported = true;
                }

                continue;
            };

            defs.push(self.def()?);
            just_reported = false;
        }

        Ok(Ur {
            root: defs.into_boxed_slice(),
        })
    }

    fn def(&mut self) -> Result<'s, UrDef<'s>> {
        self.require(bpred!(TokenKind::Def))?;
        if let Some((name, name_span)) =
            self.eat(vpred!(@t TokenKind::Name(name) => (name, t.span)))?
        {
            let expr = self.small(true)?;
            Ok(UrDef {
                span: Span {
                    start: name_span.start,
                    end: expr.span.end,
                },
                name: Some(Symbol::unknown(name)),
                expr,
            })
        } else {
            let expr = self.small(true)?;
            Ok(UrDef {
                span: expr.span,
                name: None,
                expr,
            })
        }
    }

    fn scope(
        &mut self,
        end_pred: impl Fn(&Token<'s>) -> Option<()>,
    ) -> Result<'s, (Box<[UrStmt<'s>]>, Option<UrExpr<'s>>)> {
        if self.has_peek(&end_pred)? {
            return Ok((Box::new([]), None));
        }

        let mut stmts = Vec::new();

        let (final_stmt, discard) = loop {
            let (stmt, is_terminated) = self.stmt()?;
            if self.has_peek(&end_pred)? {
                break (stmt, false);
            }

            if !is_terminated {
                self.require(bpred!(TokenKind::Semicolon))?;

                if self.has_peek(&end_pred)? {
                    break (stmt, true);
                }
            }

            stmts.push(stmt);
        };

        match (final_stmt, discard) {
            (final_stmt @ UrStmt::Def(_), _) | (final_stmt @ UrStmt::Expr(_), true) => {
                stmts.push(final_stmt);

                Ok((stmts.into_boxed_slice(), None))
            }
            (UrStmt::Expr(expr), false) => Ok((stmts.into_boxed_slice(), Some(expr))),
        }
    }

    fn stmt(&mut self) -> Result<'s, (UrStmt<'s>, bool)> {
        if self.has_peek(bpred!(TokenKind::Def))? {
            Ok((UrStmt::Def(self.def()?), true))
        } else {
            Ok((UrStmt::Expr(self.expr()?), false))
        }
    }

    fn expr(&mut self) -> Result<'s, UrExpr<'s>> {
        self.logical()
    }

    fn logical(&mut self) -> Result<'s, UrExpr<'s>> {
        self.logical_or()
    }

    fn logical_or(&mut self) -> Result<'s, UrExpr<'s>> {
        self.bin_op(
            Self::logical_and,
            vpred!(TokenKind::PipePipe => BinOp::LogOr),
        )
    }

    fn logical_and(&mut self) -> Result<'s, UrExpr<'s>> {
        self.bin_op(Self::cmp, vpred!(TokenKind::AmpAmp => BinOp::LogAnd))
    }

    fn cmp(&mut self) -> Result<'s, UrExpr<'s>> {
        self.bin_op(
            Self::unknown,
            vpred!(
                TokenKind::Eq => BinOp::Eq,
                TokenKind::Neq => BinOp::Neq,
                TokenKind::Lt => BinOp::Lt,
                TokenKind::Leq => BinOp::Leq,
                TokenKind::Gt => BinOp::Gt,
                TokenKind::Geq => BinOp::Geq,
                TokenKind::LeftArrow => BinOp::Recv,
            ),
        )
    }

    fn unknown(&mut self) -> Result<'s, UrExpr<'s>> {
        if let Some((qual_span, qual)) = self.eat(vpred!(@t
            TokenKind::Val => (t.span, UnknownQualifier::Val),
            TokenKind::Var => (t.span, UnknownQualifier::Var),
            TokenKind::Set => (t.span, UnknownQualifier::Set),
        ))? {
            let (name, name_span) = self.require(vpred!(@t TokenKind::Name(name) => (name, t.span)))?;
            let symbol = Symbol::unknown(name);

            let type_ = if self.eat(bpred!(TokenKind::Colon))?.is_some() {
                Some(Box::new(self.bitwise()?))
            } else {
                None
            };

            Ok(UrExpr {
                ty: UrType::Any,
                span: Span {
                    start: qual_span.start,
                    end: type_
                        .as_ref()
                        .map(|t| t.span.end)
                        .unwrap_or(name_span.end),
                },
                kind: UrExprKind::Unknown {
                    qual,
                    symbol,
                    type_,
                },
            })
        } else {
            self.small(false)
        }
    }

    fn small(&mut self, term: bool) -> Result<'s, UrExpr<'s>> {
        let expr = if !self.has_peek(bpred!(
            TokenKind::ThinArrow,
            TokenKind::FatArrow,
            TokenKind::OpenBrace,
            TokenKind::Impl,
            TokenKind::DotOpenBrace
        ))? {
            Some(self.bitwise()?)
        } else {
            None
        };

        enum MarkerKind {
            ThinArrow,
            Impl,
        }

        let (marker_kind, marker, output) =
            if let Some(thin_arrow) = self.eat(tpred!(TokenKind::ThinArrow))? {
                (
                    Some(MarkerKind::ThinArrow),
                    Some(thin_arrow),
                    Some(Box::new(self.jux()?)),
                )
            } else if let Some(impl_marker) = self.eat(tpred!(TokenKind::Impl))? {
                (
                    Some(MarkerKind::Impl),
                    Some(impl_marker),
                    Some(Box::new(self.jux()?)),
                )
            } else {
                (None, None, None)
            };

        if let Some(MarkerKind::Impl) | None = marker_kind {
            if let Some(open) = self.eat(tpred!(TokenKind::DotOpenBrace))? {
                let (stmts, trailing_expr) = self.scope(bpred!(TokenKind::CloseBrace))?;
                if let Some(trailing_expr) = trailing_expr {
                    self.errors
                        .error(UrError::InvalidTrailingExpr, Some(trailing_expr.span));
                }
                let close = self.require(tpred!(TokenKind::CloseBrace))?;

                return Ok(UrExpr {
                    ty: UrType::Any,
                    span: Span {
                        start: expr
                            .as_ref()
                            .map(|e| e.span.start)
                            .unwrap_or(open.span.start),
                        end: close.span.end,
                    },
                    kind: UrExprKind::Dictionary {
                        impl_ty: output,
                        stmts,
                    },
                });
            }
        }

        if let Some(MarkerKind::ThinArrow) | None = marker_kind {
            if let Some(arrow) = self.eat(tpred!(TokenKind::FatArrow))? {
                let body = self.small(false)?;
                let end = if term {
                    let semi = self.require(tpred!(TokenKind::Semicolon))?;
                    semi.span.end
                } else {
                    body.span.end
                };

                return Ok(UrExpr {
                    ty: UrType::Any,
                    span: Span {
                        start: expr
                            .as_ref()
                            .map(|e| e.span.start)
                            .unwrap_or(arrow.span.start),
                        end,
                    },
                    kind: UrExprKind::Abstraction {
                        id: self.abstraction_id(),
                        input_pat: expr.map(Box::new),
                        output_ty: output,
                        body_expr: Some(Box::new(body)),
                    },
                });
            }

            if let Some(open) = self.eat(tpred!(TokenKind::OpenBrace))? {
                let (stmts, trailing_expr) = self.scope(bpred!(TokenKind::CloseBrace))?;
                let close = self.require(tpred!(TokenKind::CloseBrace))?;

                return Ok(UrExpr {
                    ty: UrType::Any,
                    span: Span {
                        start: expr
                            .as_ref()
                            .map(|e| e.span.start)
                            .unwrap_or(open.span.start),
                        end: close.span.end,
                    },
                    kind: UrExprKind::Abstraction {
                        id: self.abstraction_id(),
                        input_pat: expr.map(Box::new),
                        output_ty: output,
                        body_expr: Some(Box::new(UrExpr {
                            ty: UrType::Any,
                            span: Span {
                                start: open.span.start,
                                end: close.span.end,
                            },
                            kind: UrExprKind::Scope {
                                stmts,
                                expr: trailing_expr.map(Box::new),
                            },
                        })),
                    },
                });
            }

            if let Some(output) = output {
                if let Some(MarkerKind::Impl) = marker_kind {
                    self.errors
                        .error(UrError::ImplNotAlone, marker.map(|m| m.span));
                    return Ok(*output);
                }
                let end = if term {
                    let semi = self.require(tpred!(TokenKind::Semicolon))?;
                    semi.span.end
                } else {
                    output.span.end
                };

                return Ok(UrExpr {
                    ty: UrType::Any,
                    span: Span {
                        start: expr.as_ref().map(|e| e.span.start).unwrap_or(
                            marker
                                .as_ref()
                                .map(|a| a.span.start)
                                .unwrap_or(output.span.start),
                        ),
                        end,
                    },
                    kind: UrExprKind::Abstraction {
                        id: self.abstraction_id(),
                        input_pat: expr.map(Box::new),
                        output_ty: Some(output),
                        body_expr: None,
                    },
                });
            }
        }

        let expr = expr.unwrap();

        if term {
            let semi = self.require(tpred!(TokenKind::Semicolon))?;
            Ok(UrExpr {
                ty: UrType::Any,
                span: Span {
                    start: expr.span.start,
                    end: semi.span.end,
                },
                kind: expr.kind,
            })
        } else {
            Ok(expr)
        }
    }

    fn bitwise(&mut self) -> Result<'s, UrExpr<'s>> {
        self.bit_or()
    }

    fn bit_or(&mut self) -> Result<'s, UrExpr<'s>> {
        self.bin_op(Self::bit_xor, vpred!(TokenKind::Pipe => BinOp::BitOr))
    }

    fn bit_xor(&mut self) -> Result<'s, UrExpr<'s>> {
        self.bin_op(Self::bit_and, vpred!(TokenKind::Tilde => BinOp::BitXor))
    }

    fn bit_and(&mut self) -> Result<'s, UrExpr<'s>> {
        self.bin_op(Self::shift, vpred!(TokenKind::Amp => BinOp::BitAnd))
    }

    fn shift(&mut self) -> Result<'s, UrExpr<'s>> {
        self.bin_op(
            Self::arith,
            vpred!(
                TokenKind::Shl => BinOp::Shl,
                TokenKind::Shr => BinOp::Shr,
            ),
        )
    }

    fn arith(&mut self) -> Result<'s, UrExpr<'s>> {
        self.bin_op(
            Self::term,
            vpred!(
                TokenKind::Plus => BinOp::Add,
                TokenKind::Minus => BinOp::Sub,
            ),
        )
    }

    fn term(&mut self) -> Result<'s, UrExpr<'s>> {
        self.bin_op(
            Self::prefix,
            vpred!(
                TokenKind::Star => BinOp::Mul,
                TokenKind::Slash => BinOp::Div,
                TokenKind::Percent => BinOp::Rem,
            ),
        )
    }

    fn prefix(&mut self) -> Result<'s, UrExpr<'s>> {
        if let Some((op, op_span)) = self.eat(vpred!(@t
            TokenKind::Bang => (UnOp::Not, t.span),
            TokenKind::Minus => (UnOp::Neg, t.span),
        ))? {
            let inner = self.jux()?;
            Ok(UrExpr {
                ty: UrType::Any,
                span: Span {
                    start: op_span.start,
                    end: inner.span.end,
                },
                kind: UrExprKind::PrimitiveOperation {
                    operation: PrimitiveOperation::Unary(op, Box::new(inner)),
                },
            })
        } else {
            self.jux()
        }
    }

    fn jux(&mut self) -> Result<'s, UrExpr<'s>> {
        let mut expr = self.atom()?;

        while let Some(arg) = self.maybe_atom()? {
            expr = UrExpr {
                ty: UrType::Any,
                span: Span {
                    start: expr.span.start,
                    end: arg.span.end,
                },
                kind: UrExprKind::PrimitiveOperation {
                    operation: PrimitiveOperation::Binary(
                        BinOp::Apply,
                        Box::new(expr),
                        Box::new(arg),
                    ),
                },
            }
        }

        Ok(expr)
    }

    fn atom(&mut self) -> Result<'s, UrExpr<'s>> {
        if let Some(open) = self.eat(tpred!(TokenKind::OpenParen))? {
            let (stmts, expr) = self.scope(bpred!(TokenKind::CloseParen))?;
            let close = self.require(tpred!(TokenKind::CloseParen))?;
            Ok(UrExpr {
                ty: UrType::Any,
                span: Span {
                    start: open.span.start,
                    end: close.span.end,
                },
                kind: if stmts.is_empty() {
                    if let Some(expr) = expr {
                        expr.kind
                    } else {
                        UrExprKind::Tuple { items: Box::new([]) }
                    }
                } else {
                    UrExprKind::Scope { stmts, expr: expr.map(Box::new) }
                }
            })
        } else if let Some((name, span)) = self.eat(vpred!(@t TokenKind::Name(name) => (name, t.span)))? {
            Ok(UrExpr {
                ty: UrType::Any,
                span,
                kind: UrExprKind::Lookup {
                    symbol: Symbol::unknown(name),
                },
            })
        } else if let Some((i, span)) = self.eat(vpred!(@t TokenKind::Integer(i) => (i, t.span)))? {
            Ok(UrExpr {
                ty: UrType::Any,
                span,
                kind: UrExprKind::PrimitiveValue {
                    value: PrimitiveValue::S64(i as i64),
                },
            })
        } else if let Some((f, span)) = self.eat(vpred!(@t TokenKind::Float(f) => (f, t.span)))? {
            Ok(UrExpr {
                ty: UrType::Any,
                span,
                kind: UrExprKind::PrimitiveValue {
                    value: PrimitiveValue::F64(f),
                },
            })
        } else {
            let (s, span) = self.require(vpred!(@t TokenKind::String(s) => (s, t.span)))?;
            Ok(UrExpr {
                ty: UrType::Any,
                span,
                kind: UrExprKind::PrimitiveValue {
                    value: PrimitiveValue::Str(s.text()),
                },
            })
        }
    }

    fn maybe_atom(&mut self) -> Result<'s, Option<UrExpr<'s>>> {
        if let Some(open) = self.eat(tpred!(TokenKind::OpenParen))? {
            let (stmts, expr) = self.scope(bpred!(TokenKind::CloseParen))?;
            let close = self.require(tpred!(TokenKind::CloseParen))?;
            Ok(Some(UrExpr {
                ty: UrType::Any,
                span: Span {
                    start: open.span.start,
                    end: close.span.end,
                },
                kind: if stmts.is_empty() {
                    if let Some(expr) = expr {
                        expr.kind
                    } else {
                        UrExprKind::Tuple { items: Box::new([]) }
                    }
                } else {
                    UrExprKind::Scope { stmts, expr: expr.map(Box::new) }
                }
            }))
        } else if let Some((name, span)) = self.eat(vpred!(@t TokenKind::Name(name) => (name, t.span)))? {
            Ok(Some(UrExpr {
                ty: UrType::Any,
                span,
                kind: UrExprKind::Lookup {
                    symbol: Symbol::unknown(name),
                },
            }))
        } else if let Some((i, span)) = self.eat(vpred!(@t TokenKind::Integer(i) => (i, t.span)))? {
            Ok(Some(UrExpr {
                ty: UrType::Any,
                span,
                kind: UrExprKind::PrimitiveValue {
                    value: PrimitiveValue::S64(i as i64),
                },
            }))
        } else if let Some((f, span)) = self.eat(vpred!(@t TokenKind::Float(f) => (f, t.span)))? {
            Ok(Some(UrExpr {
                ty: UrType::Any,
                span,
                kind: UrExprKind::PrimitiveValue {
                    value: PrimitiveValue::F64(f),
                },
            }))
        } else if let Some((l, span)) = self.eat(vpred!(@t TokenKind::Label(l) => (l, t.span)))? {
            Ok(Some(UrExpr {
                ty: UrType::Any,
                span,
                kind: UrExprKind::Label { value: l },
            }))
        } else if let Some((s, span)) = self.eat(vpred!(@t TokenKind::String(s) => (s, t.span)))? {
            Ok(Some(UrExpr {
                ty: UrType::Any,
                span,
                kind: UrExprKind::PrimitiveValue {
                    value: PrimitiveValue::Str(s.text()),
                },
            }))
        } else {
            Ok(None)
        }
    }

    fn bin_op(
        &mut self,
        next: impl Fn(&mut Self) -> Result<'s, UrExpr<'s>>,
        pred: impl Fn(&Token<'s>) -> Option<BinOp>,
    ) -> Result<'s, UrExpr<'s>> {
        let mut a = next(self)?;

        while let Some(op) = self.eat(&pred)? {
            let b = next(self)?;

            let span = Span {
                start: a.span.start,
                end: a.span.end,
            };

            a = UrExpr {
                ty: UrType::Any,
                span,
                kind: UrExprKind::PrimitiveOperation {
                    operation: PrimitiveOperation::Binary(op, Box::new(a), Box::new(b)),
                },
            }
        }

        Ok(a)
    }

    #[allow(unused)]
    fn peek<T>(&mut self, pred: impl FnOnce(&Token<'s>) -> Option<T>) -> Result<'s, Option<T>> {
        if let Some(token) = self.tokens.peek()? {
            if let Some(t) = pred(token) {
                Ok(Some(t))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    /// Returns `true` if the current token peek satisfies `pred`.
    fn has_peek(&mut self, pred: impl FnOnce(&Token<'s>) -> Option<()>) -> Result<'s, bool> {
        if let Some(token) = self.tokens.peek()? {
            if pred(token).is_some() {
                Ok(true)
            } else {
                Ok(false)
            }
        } else {
            Ok(false)
        }
    }

    /// Requires that the next token exists and satisfies `pred` and errors otherwise.
    ///
    /// Does not consume the token if it does not satisfy `pred`.
    fn require<T>(&mut self, pred: impl FnOnce(&Token<'s>) -> Option<T>) -> Result<'s, T> {
        match self.maybe_require(pred) {
            Ok(Some(t)) => Ok(t),
            Ok(None) => Err(UrError::Unexpected(None)),
            Err(e) => Err(e),
        }
    }

    /// Requires that the next token (if one exists) satisfies `pred` and errors otherwise.
    ///
    /// Does not consume the token if it does not satisfy `pred`.
    fn maybe_require<T>(
        &mut self,
        pred: impl FnOnce(&Token<'s>) -> Option<T>,
    ) -> Result<'s, Option<T>> {
        if let Some(token) = self.tokens.peek()? {
            if let Some(t) = pred(token) {
                self.tokens.next()?;
                Ok(Some(t))
            } else {
                //panic!();
                Err(UrError::Unexpected(Some(token.clone())))
            }
        } else {
            Ok(None)
        }
    }

    /// Checks if the next token (if one exists) satisfies `pred` and returns None otherwise.
    ///
    /// Does not consume the token if it does not satisfy `pred`.
    fn eat<T>(&mut self, pred: impl FnOnce(&Token<'s>) -> Option<T>) -> Result<'s, Option<T>> {
        if let Some(token) = self.tokens.peek()? {
            if let Some(t) = pred(token) {
                self.tokens.next()?;
                Ok(Some(t))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    fn abstraction_id(&mut self) -> UrAbstractionId {
        let id = UrAbstractionId(self.abstraction_id_counter);
        self.abstraction_id_counter += 1;
        id
    }
}