use super::*;

pub(super) fn resolve(mut ur: Ur, errors: ErrorStream) -> Ur {
    Resolver {
        scoper: Scoper::new(),
        errors,
    }
    .resolve_ur(&mut ur);
    ur
}

struct Resolver<'s> {
    scoper: Scoper<'s>,
    errors: ErrorStream,
}

impl<'s> Resolver<'s> {
    fn resolve_ur(mut self, ur: &mut Ur<'s>) {
        for def in ur.root.iter_mut() {
            if let Some(symbol) = &mut def.name {
                *symbol = self.scoper.new_symbol(symbol.name);
            }
        }
        for def in ur.root.iter_mut() {
            self.resolve(&mut def.expr);
        }
    }

    fn resolve(&mut self, expr: &mut UrExpr<'s>) {
        match &mut expr.kind {
            UrExprKind::Abstraction {
                id: _,
                input_pat,
                output_ty,
                body_expr,
            } => {
                self.scoper.push();
                if let Some(input_pat) = input_pat.as_mut() {
                    self.resolve(input_pat);
                }
                if let Some(output_ty) = output_ty.as_mut() {
                    self.resolve(output_ty);
                }
                if let Some(body_expr) = body_expr.as_mut() {
                    self.resolve(body_expr)
                }
                self.scoper.pop();
            }
            UrExprKind::Selection {
                cond_expr,
                then_expr,
                else_expr,
            } => {
                self.scoper.push();
                self.resolve(cond_expr);
                self.resolve(then_expr);
                self.scoper.pop();
                if let Some(else_expr) = else_expr.as_mut() {
                    self.resolve(else_expr)
                }
            }
            UrExprKind::Iteration {
                iter_expr,
                loop_expr,
            } => {
                self.scoper.push();
                self.resolve(iter_expr);
                self.resolve(loop_expr);
                self.scoper.pop();
            }
            UrExprKind::Scope { stmts, expr } => {
                self.scoper.push();
                for def in stmts.iter_mut().filter_map(|s| match s {
                    UrStmt::Def(def) => Some(def),
                    UrStmt::Expr(_) => None,
                }) {
                    if let Some(symbol) = &mut def.name {
                        *symbol = self.scoper.new_symbol(symbol.name);
                    }
                }
                for stmt in stmts.iter_mut() {
                    match stmt {
                        UrStmt::Def(def) => {
                            self.resolve(&mut def.expr);
                        }
                        UrStmt::Expr(expr) => self.resolve(expr),
                    }
                }
                if let Some(expr) = expr.as_mut() {
                    self.resolve(expr);
                }
                self.scoper.pop();
            }
            UrExprKind::Dictionary { impl_ty, stmts } => {
                if let Some(impl_ty) = impl_ty.as_mut() {
                    self.resolve(impl_ty);
                }
                self.scoper.push();
                for def in stmts.iter_mut().filter_map(|s| match s {
                    UrStmt::Def(def) => Some(def),
                    UrStmt::Expr(_) => None,
                }) {
                    if let Some(symbol) = &mut def.name {
                        *symbol = self.scoper.new_symbol(symbol.name);
                    }
                }
                for stmt in stmts.iter_mut() {
                    match stmt {
                        UrStmt::Def(def) => {
                            self.resolve(&mut def.expr);
                        }
                        UrStmt::Expr(expr) => self.resolve(expr),
                    }
                }
                self.scoper.pop();
            }
            UrExprKind::Tuple { items } => {
                for item in items.iter_mut() {
                    self.resolve(&mut item.value);
                }
            }
            UrExprKind::Union { items } => {
                for item in items.iter_mut() {
                    self.resolve(&mut item.value);
                }
            }
            UrExprKind::Lookup { symbol } => {
                if symbol.is_unknown() {
                    if let Some(found) = self.scoper.lookup(symbol.name) {
                        *symbol = found;
                    } else {
                        self.errors.error(UrError::UndefinedSymbol, Some(expr.span))
                    }
                }
            }
            UrExprKind::Unknown {
                qual,
                symbol,
                type_,
            } => {
                match qual {
                    UnknownQualifier::Val | UnknownQualifier::Var => {
                        *symbol = self.scoper.new_symbol(symbol.name)
                    }
                    UnknownQualifier::Set => {
                        if symbol.is_unknown() {
                            if let Some(found) = self.scoper.lookup(symbol.name) {
                                *symbol = found;
                            } else {
                                self.errors.error(UrError::UndefinedSymbol, Some(expr.span))
                            }
                        }
                    }
                }
                if let Some(type_) = type_ {
                    self.resolve(type_);
                }
            }
            UrExprKind::PrimitiveOperation { operation } => match operation {
                PrimitiveOperation::Binary(_, a, b) => {
                    self.resolve(a);
                    self.resolve(b);
                }
                PrimitiveOperation::Unary(_, a) => {
                    self.resolve(a);
                }
            },
            UrExprKind::PrimitiveValue { .. } => {}
            UrExprKind::Label { .. } => {}
        }
    }
}
