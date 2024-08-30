use std::convert::identity;

use preds::{either, epred};

use crate::{char_reader::CharReader, error_stream::ErrorStream, stages::tok::Tokens};

use super::*;

pub fn parse<'s, R: CharReader>(tokens: Tokens<'s, R>, errors: ErrorStream) -> Result<'s, Ur<'s>> {
    Parser {
        abstraction_id_counter: 0,
        tokens,
        errors,
    }
    .parse()
}

struct Parser<'s, R> {
    abstraction_id_counter: usize,
    tokens: Tokens<'s, R>,
    errors: ErrorStream,
}

impl<'s, R: CharReader> Parser<'s, R> {
    /*
        fn parse2(mut self) -> Result<'s, Ur<'s>> {

        }

        fn dict(&mut self) -> Result<'s, Dictionary<'s>> {
            todo!()
        }
    */
    fn parse(mut self) -> Result<'s, Ur<'s>> {
        let root = self.dict_inner(|_| None)?;

        Ok(Ur { root })
    }

    fn dict_inner(&mut self, end_pred: impl Fn(&Token<'s>) -> Option<()>) -> Result<'s, UrDict<'s>> {
        let mut defs = Vec::new();
        let mut just_reported = false;
        while self.tokens.peek()?.is_some() {
            if self.has_peek(&end_pred)? {
                break;
            }

            let Some(tok) = self.tokens.peek()? else {
                unreachable!()
            };
            if !matches!(tok.kind, TokenKind::Def | TokenKind::Ext) {
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

        Ok(UrDict {
            defs: defs.into_boxed_slice(),
        })
    }

    fn def(&mut self) -> Result<'s, UrDef<'s>> {
        let start = self.require(spred!(TokenKind::Def))?;
        let name = self.require(vpred!(TokenKind::Name(name) => name))?;
        let func = self.func_always()?;
        let end = Self::minmax_span(
            [&func.input_pat, &func.output_ty, &func.body_expr]
                .into_iter()
                .flat_map(identity)
                .map(|it| it.span),
        )
        .end;

        Ok(UrDef {
            name: Some(Symbol::unknown(name)),
            func,
            span: Span { start, end },
        })
    }

    fn func_always(&mut self) -> Result<'s, UrFunc<'s>> {
        let items = self.func_inner()?;
        let [input_pat, output_ty, body_expr] = items;

        Ok(UrFunc {
            id: self.func_id(),
            input_pat: input_pat.map(Box::new),
            output_ty: output_ty.map(Box::new),
            body_expr: body_expr.map(Box::new),
        })
    }

    fn func_level(&mut self) -> Result<'s, UrExpr<'s>> {
        match self.func_inner()? {
            [None, None, None] => todo!("what error to throw"),
            [Some(expr), None, None] => Ok(expr),
            [None, None, Some(
                expr @ UrExpr {
                    kind: UrExprKind::Dict(_),
                    ..
                },
            )] => Ok(expr),
            [input_pat, output_ty, body_expr] => Ok(UrExpr {
                ty: UrType::Any,
                span: Self::minmax_span(
                    [&input_pat, &output_ty, &body_expr]
                        .iter()
                        .copied()
                        .flat_map(identity)
                        .map(|it| it.span),
                ),
                kind: UrExprKind::Func(UrFunc {
                    id: self.func_id(),
                    input_pat: input_pat.map(Box::new),
                    output_ty: output_ty.map(Box::new),
                    body_expr: body_expr.map(Box::new),
                }),
            }),
        }
    }

    fn minmax_span(spans: impl IntoIterator<Item = Span>) -> Span {
        let mut start = usize::MAX;
        let mut end = 0;
        for span in spans {
            start = start.min(span.start);
            end = end.max(span.end);
        }
        Span { start, end }
    }

    fn func_inner(&mut self) -> Result<'s, [Option<UrExpr<'s>>; 3]> {
        let expr = if !self.has_peek(bpred!(
            TokenKind::ThinArrow
                | TokenKind::Colon
                | TokenKind::OpenBrace
                | TokenKind::DotOpenBrace
        ))? {
            Some(self.next_lower()?)
        } else {
            None
        };

        let output_ty = if self.eat(bpred!(TokenKind::ThinArrow))?.is_some() {
            Some(self.next_lower()?)
        } else {
            None
        };

        let body_expr = if self.eat(bpred!(TokenKind::Colon))?.is_some() {
            Some(self.next_lower()?)
        } else if let Some(start) = self.eat(spred!(TokenKind::OpenBrace))? {
            let inner = self.scope(bpred!(TokenKind::CloseBrace))?;
            let end = self.require(epred!(TokenKind::CloseBrace))?;

            Some(UrExpr {
                ty: UrType::Any,
                kind: inner,
                span: Span { start, end },
            })
        } else if let Some(start) = self.eat(spred!(TokenKind::DotOpenBrace))? {
            let inner = self.dict_inner(bpred!(TokenKind::CloseBrace))?;
            let end = self.require(spred!(TokenKind::CloseBrace))?;

            Some(UrExpr {
                ty: UrType::Any,
                kind: UrExprKind::Dict(inner),
                span: Span { start, end },
            })
        } else {
            None
        };

        Ok([expr, output_ty, body_expr])
    }

    fn scope(&mut self, end_pred: impl Fn(&Token<'s>) -> Option<()>) -> Result<'s, UrExprKind<'s>> {
        todo!()
    }

    fn next_lower(&mut self) -> Result<'s, UrExpr<'s>> {
        todo!("Haven't decided the next lower step")
    }

    /*

    fn next_lower(&mut self) -> Result<'s, UrExpr<'s>> {
        todo!("Haven't decided the next lower step")
    }
     */

    /*fn parse(mut self) -> Result<'s, Ur<'s>> {
        let mut defs = Vec::new();
        let mut just_reported = false;
        while let Some(tok) = self.tokens.peek()? {
            if !matches!(tok.kind, TokenKind::Def | TokenKind::Type) {
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
        self.require(bpred!(TokenKind::Def | TokenKind::Type))?;
        if let Some((name, name_span)) =
            self.eat(vpred!(@t TokenKind::Name(name) => (name, t.span)))?
        {
            let expr = self.small(true, true)?;
            Ok(UrDef {
                span: Span {
                    start: name_span.start,
                    end: expr.span.end,
                },
                name: Some(Symbol::unknown(name)),
                expr,
            })
        } else {
            let expr = self.small(true, true)?;
            Ok(UrDef {
                span: expr.span,
                name: None,
                expr,
            })
        }
    }

    fn tuple(
        &mut self,
        start: usize,
        end_pred: impl Fn(&Token<'s>) -> Option<usize>,
    ) -> Result<'s, TupleParseResult<'s>> {
        let (first_stmts, first_expr) = self.scope(&end_pred)?;
        if let Some(end) = self.peek(&end_pred)? {
            return Ok(TupleParseResult::Single(UrExpr {
                ty: UrType::Any,
                span: Span { start, end },
                kind: if first_stmts.is_empty() && first_expr.is_some() {
                    first_expr.map(|e| e.kind).unwrap()
                } else {
                    UrExprKind::Scope {
                        stmts: first_stmts,
                        expr: first_expr.map(Box::new),
                    }
                },
            }));
        }

        let (first_label, first_stmts, first_expr) = {
            if let Some(UrExprKind::Lookup { symbol }) = first_expr.as_ref().map(|e| &e.kind) {
                if first_stmts.is_empty() && self.eat(bpred!(TokenKind::Colon))?.is_some() {
                    let (first_stmts, first_expr) =
                        self.scope(either(&end_pred, spred!(TokenKind::Comma)))?;

                    (Some(symbol.name), first_stmts, first_expr)
                } else {
                    (None, first_stmts, first_expr)
                }
            } else {
                (None, first_stmts, first_expr)
            }
        };

        let first_comma = self.require_peek(tpred!(TokenKind::Comma))?;
        let mut items = Vec::from([UrTupleItem {
            label: first_label,
            value: UrExpr {
                ty: UrType::Any,
                span: Span {
                    start,
                    end: first_comma.span.start,
                },
                kind: if first_stmts.is_empty() && first_expr.is_some() {
                    first_expr.map(|e| e.kind).unwrap()
                } else {
                    UrExprKind::Scope {
                        stmts: first_stmts,
                        expr: first_expr.map(Box::new),
                    }
                },
            },
        }]);

        while let Some(comma) = self.eat(tpred!(TokenKind::Comma))? {
            if self.has_peek(&end_pred)? {
                break;
            }

            let (label, stmts, expr) = {
                let (stmts, expr) = self.scope(either(&end_pred, spred!(TokenKind::Comma)))?;

                if let Some(UrExprKind::Lookup { symbol }) = expr.as_ref().map(|e| &e.kind) {
                    if stmts.is_empty() && self.eat(bpred!(TokenKind::Colon))?.is_some() {
                        let (stmts, expr) =
                            self.scope(either(&end_pred, spred!(TokenKind::Comma)))?;

                        (Some(symbol.name), stmts, expr)
                    } else {
                        (None, stmts, expr)
                    }
                } else {
                    (None, stmts, expr)
                }
            };

            let start = comma.span.end;
            let end = if let Some(end) = self.peek(spred!(TokenKind::Comma))? {
                end
            } else {
                self.require_peek(&end_pred)?
            };

            items.push(UrTupleItem {
                label,
                value: UrExpr {
                    ty: UrType::Any,
                    span: Span { start, end },
                    kind: if stmts.is_empty() && expr.is_some() {
                        expr.map(|e| e.kind).unwrap()
                    } else {
                        UrExprKind::Scope {
                            stmts,
                            expr: expr.map(Box::new),
                        }
                    },
                },
            })
        }

        Ok(TupleParseResult::Multiple(items.into_boxed_slice()))
    }

    fn scope(
        &mut self,
        end_pred: impl Fn(&Token<'s>) -> Option<usize>,
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
                if self.eat(bpred!(TokenKind::Semicolon))?.is_none() {
                    break (stmt, false);
                }

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
        if self.has_peek(bpred!(TokenKind::Def | TokenKind::Type))? {
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
            let (name, name_span) =
                self.require(vpred!(@t TokenKind::Name(name) => (name, t.span)))?;
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
                    end: type_.as_ref().map(|t| t.span.end).unwrap_or(name_span.end),
                },
                kind: UrExprKind::Unknown {
                    qual,
                    symbol,
                    type_,
                },
            })
        } else {
            self.small(false, false)
        }
    }

    fn small(&mut self, term: bool, is_def: bool) -> Result<'s, UrExpr<'s>> {
        let expr = if !self.has_peek(bpred!(
            TokenKind::ThinArrow,
            TokenKind::FatArrow,
            TokenKind::OpenBrace,
        ))? {
            Some(self.bitwise()?)
        } else {
            None
        };

        enum MarkerKind {
            ThinArrow,
        }

        let (marker, output) =
            if let Some(thin_arrow) = self.eat(tpred!(TokenKind::ThinArrow))? {
                (
                    Some(thin_arrow),
                    Some(Box::new(self.jux()?)),
                )
            } else {
                (None, None)
            };

        if let Some(arrow) = self.eat(tpred!(TokenKind::FatArrow))? {
            let body = self.small(false, false)?;
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
            let (stmts, trailing_expr) = self.scope(spred!(TokenKind::CloseBrace))?;
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

        let expr = expr.unwrap();

        if term {
            self.require(bpred!(TokenKind::Semicolon))?;
        }

        if is_def {
            Ok(UrExpr {
                ty: UrType::Any,
                span: expr.span,
                kind: UrExprKind::Abstraction {
                    id: self.abstraction_id(),
                    input_pat: Some(Box::new(expr)),
                    output_ty: None,
                    body_expr: None,
                },
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
        if let Some((span, item)) = self.maybe_variant_item()? {
            let start = span.start;
            let mut end = span.end;

            let mut items = Vec::from([item]);
            while let Some((span, item)) = self.maybe_variant_item()? {
                items.push(item);
                end = span.end;
            }

            Ok(UrExpr {
                ty: UrType::Any,
                span: Span { start, end },
                kind: UrExprKind::Variant {
                    items: items.into_boxed_slice(),
                },
            })
        } else {
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
    }

    fn atom(&mut self) -> Result<'s, UrExpr<'s>> {
        if let Some(open) = self.eat(tpred!(TokenKind::OpenParen))? {
            let result = self.tuple(open.span.start, spred!(TokenKind::CloseParen))?;
            let close = self.require(tpred!(TokenKind::CloseParen))?;
            match result {
                TupleParseResult::Single(expr) => Ok(UrExpr {
                    ty: expr.ty,
                    kind: expr.kind,
                    span: Span {
                        start: open.span.start,
                        end: close.span.end,
                    },
                }),
                TupleParseResult::Multiple(items) => Ok(UrExpr {
                    ty: UrType::Any,
                    kind: UrExprKind::Tuple { items },
                    span: Span {
                        start: open.span.start,
                        end: close.span.end,
                    },
                }),
            }
        } else if let Some((name, span)) =
            self.eat(vpred!(@t TokenKind::Name(name) => (name, t.span)))?
        {
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
            let result = self.tuple(open.span.start, spred!(TokenKind::CloseParen))?;
            let close = self.require(tpred!(TokenKind::CloseParen))?;
            match result {
                TupleParseResult::Single(expr) => Ok(Some(UrExpr {
                    ty: expr.ty,
                    kind: expr.kind,
                    span: Span {
                        start: open.span.start,
                        end: close.span.end,
                    },
                })),
                TupleParseResult::Multiple(items) => Ok(Some(UrExpr {
                    ty: UrType::Any,
                    kind: UrExprKind::Tuple { items },
                    span: Span {
                        start: open.span.start,
                        end: close.span.end,
                    },
                })),
            }
        } else if let Some((name, span)) =
            self.eat(vpred!(@t TokenKind::Name(name) => (name, t.span)))?
        {
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

    fn maybe_variant_item(&mut self) -> Result<'s, Option<(Span, UrVariantItem<'s, UrExpr<'s>>)>> {
        if let Some((span, name)) = self.eat(vpred!(@t TokenKind::Label(name) => (t.span, name)))? {
            if let Some(body) = self.maybe_atom()? {
                Ok(Some((
                    span,
                    UrVariantItem {
                        label: name,
                        value: Some(body),
                    },
                )))
            } else {
                Ok(Some((
                    span,
                    UrVariantItem {
                        label: name,
                        value: None,
                    },
                )))
            }
        } else {
            Ok(None)
        }
    }

    */
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

    fn require_peek<T>(&mut self, pred: impl FnOnce(&Token<'s>) -> Option<T>) -> Result<'s, T> {
        if let Some(token) = self.tokens.peek()? {
            if let Some(t) = pred(token) {
                Ok(t)
            } else {
                Err(UrError::Unexpected(Some(token.clone())))
            }
        } else {
            Err(UrError::Unexpected(None))
        }
    }

    /// Returns `true` if the current token peek satisfies `pred`.
    fn has_peek<T>(&mut self, pred: impl FnOnce(&Token<'s>) -> Option<T>) -> Result<'s, bool> {
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

    fn func_id(&mut self) -> UrFuncId {
        let id = UrFuncId(self.abstraction_id_counter);
        self.abstraction_id_counter += 1;
        id
    }
}

enum TupleParseResult<'s> {
    Multiple(Box<[UrTupleItem<'s, UrExpr<'s>>]>),
    Single(UrExpr<'s>),
}
