use rustc_hash::FxHashMap;

use crate::{
    error_stream::ErrorStream,
    primitive::{BinOp, PrimitiveOperation, PrimitiveValue, UnOp},
    stages::ur::{UrDefTypeEntry, UrTupleItem},
    unknown::UnknownQualifier,
};

use super::{Symbol, Ur, UrDef, UrExpr, UrExprKind, UrStmt, UrType, UrVariantItem};

pub fn r#type(mut ur: Ur, errors: ErrorStream) -> Ur {
    Typer {
        errors,
        symbols: FxHashMap::default(),
    }
    .type_ur(&mut ur);
    ur
}

#[derive(Clone, Copy, Debug)]
enum DataflowDirection {
    Provider,
    Receiver,
}

use DataflowDirection::*;

struct Typer<'s> {
    errors: ErrorStream,
    symbols: FxHashMap<Symbol<'s>, UrType<'s>>,
}

impl<'s> Typer<'s> {
    fn type_ur(&mut self, ur: &mut Ur<'s>) {
        for def in ur.root.iter() {
            if let Some(name) = def.name {
                self.symbols.insert(name, UrType::Any);
            }
        }
        for def in ur.root.iter_mut() {
            self.type_expr(&mut def.expr, &UrType::Any, Provider);
        }
    }

    fn type_expr(&mut self, expr: &mut UrExpr<'s>, outer_ty: &UrType<'s>, flow: DataflowDirection) {
        let ty = match &mut expr.kind {
            UrExprKind::Abstraction {
                id,
                input_pat,
                output_ty,
                body_expr,
            } => {
                if let Some(input_pat) = input_pat {
                    self.type_expr(input_pat, &UrType::Any, Receiver);
                }
                if let Some(body_expr) = body_expr {
                    let body_superty = if let Some(output_ty) = output_ty {
                        self.type_type(output_ty);
                        output_ty.ty.clone()
                    } else {
                        UrType::Any
                    };
                    self.type_expr(body_expr, &body_superty, Provider);
                    let input_ty = if let Some(input_pat) = input_pat {
                        Some(Box::new(input_pat.ty.clone()))
                    } else {
                        None
                    };
                    UrType::Abstraction(Some(*id), input_ty, Box::new(body_expr.ty.clone()))
                } else {
                    todo!("errors")
                }
            }
            UrExprKind::Selection {
                cond_expr,
                then_expr,
                else_expr,
            } => todo!(),
            UrExprKind::Iteration {
                iter_expr,
                loop_expr,
            } => todo!(),
            UrExprKind::Scope { stmts, expr } => {
                for stmt in stmts.iter_mut() {
                    match stmt {
                        UrStmt::Def(def) => {
                            self.type_expr(&mut def.expr, &UrType::Any, Provider);
                        }
                        UrStmt::Expr(expr) => {
                            self.type_expr(expr, &UrType::Any, Provider);
                        }
                    }
                }

                if let Some(expr) = expr {
                    self.type_expr(expr, outer_ty, flow);
                    expr.ty.clone()
                } else {
                    UrType::Tuple(Box::new([]))
                }
            }
            UrExprKind::Tuple { items } => {
                let supertys_slice = if let UrType::Tuple(supertys) = outer_ty {
                    &**supertys
                } else {
                    &[]
                };
                let supertys = supertys_slice.iter().chain(std::iter::repeat(&UrTupleItem {
                    label: None,
                    value: UrType::Any,
                }));

                // TODO: match superty based on labels before positionality

                UrType::Tuple(
                    items
                        .iter_mut()
                        .zip(supertys)
                        .map(|(item, superty)| UrTupleItem {
                            label: item.label,
                            value: {
                                self.type_expr(&mut item.value, &superty.value, flow);
                                item.value.ty.clone()
                            },
                        })
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                )
            }
            // TODO: use superty here
            UrExprKind::Variant { items } => UrType::Variant(
                items
                    .iter_mut()
                    .map(|it| UrVariantItem::<'s, UrType<'s>> {
                        label: it.label,
                        value: {
                            if let Some(value) = &mut it.value {
                                self.type_expr(value, &UrType::Any, flow);
                                Some(value.ty.clone())
                            } else {
                                None
                            }
                        },
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            ),
            UrExprKind::Lookup { symbol } => self
                .symbols
                .get(symbol)
                .expect("use before define should not be possible to observe here")
                .clone(),
            UrExprKind::Unknown {
                qual,
                symbol,
                type_,
            } => match qual {
                UnknownQualifier::Val | UnknownQualifier::Var => {
                    if let Some(type_) = type_ {
                        self.type_type(type_);

                        type_.ty.clone()
                    } else {
                        self.symbols.insert(*symbol, outer_ty.clone());

                        outer_ty.clone()
                    }
                }
                UnknownQualifier::Set => self
                    .symbols
                    .get(symbol)
                    .expect("use before define should not be possible to observe here")
                    .clone(),
            },
            UrExprKind::PrimitiveOperation { operation } => match operation {
                PrimitiveOperation::Binary(op, a, b) => match op {
                    BinOp::Apply => {
                        self.type_expr(
                            a,
                            &UrType::Abstraction(
                                None,
                                Some(Box::new(UrType::Any)),
                                Box::new(outer_ty.clone()),
                            ),
                            Provider,
                        );

                        let UrType::Abstraction(_id, input, output) = &a.ty else {
                            todo!("errors")
                        };

                        let Some(input) = input else { todo!("errors") };

                        self.type_expr(b, input, flow);

                        (**output).clone()
                    }
                    BinOp::Recv => {
                        // undecided if this will actually exist.

                        todo!()
                    }
                    BinOp::LogOr | BinOp::LogAnd => {
                        self.type_expr(a, &UrType::Bool, Provider);
                        self.type_expr(b, &UrType::Bool, Provider);

                        UrType::Bool
                    }
                    BinOp::Eq | BinOp::Neq => {
                        self.type_expr(a, &UrType::Any, Provider);
                        self.type_expr(b, &UrType::Any, Provider);
                        if !a.ty.compatible_with(&b.ty) {
                            todo!("errors")
                        }

                        UrType::Bool
                    }
                    BinOp::Lt | BinOp::Leq | BinOp::Gt | BinOp::Geq => {
                        self.type_expr(a, &UrType::NUMBER, Provider);
                        self.type_expr(b, &UrType::NUMBER, Provider);
                        if a.ty != b.ty {
                            todo!("errors")
                        }

                        UrType::Bool
                    }
                    BinOp::BitOr
                    | BinOp::BitXor
                    | BinOp::BitAnd
                    | BinOp::Shl
                    | BinOp::Shr
                    | BinOp::Rem => {
                        self.type_expr(a, &UrType::INTEGER, Provider);
                        self.type_expr(b, &UrType::INTEGER, Provider);
                        if a.ty != b.ty {
                            todo!("errors")
                        }

                        a.ty.clone()
                    }
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                        self.type_expr(a, &UrType::NUMBER, Provider);
                        self.type_expr(b, &UrType::NUMBER, Provider);
                        if a.ty != b.ty {
                            todo!("errors")
                        }

                        a.ty.clone()
                    }
                },
                PrimitiveOperation::Unary(op, a) => match op {
                    UnOp::Not => {
                        self.type_expr(a, &UrType::Bool, Provider);

                        UrType::Bool
                    }
                    UnOp::Neg => {
                        self.type_expr(a, &UrType::NUMBER, Provider);

                        a.ty.clone()
                    }
                },
            },
            UrExprKind::PrimitiveValue { value } => match value {
                PrimitiveValue::S32(_) => UrType::S32,
                PrimitiveValue::S64(_) => todo!(),
                PrimitiveValue::F32(_) => UrType::F32,
                PrimitiveValue::F64(_) => todo!(),
                PrimitiveValue::Str(_) => UrType::Str,
            },
            UrExprKind::Label { value } => UrType::Label(*value),
        };

        let valid = match flow {
            Provider => ty.is_subtype(outer_ty),
            Receiver => outer_ty.is_subtype(&ty),
        };

        if !valid {
            todo!("errors");
        }
    }

    fn type_type(&mut self, expr: &mut UrExpr<'s>) {
        todo!()
    }
}
