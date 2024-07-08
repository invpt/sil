use rustc_hash::FxHashMap;

use crate::{
    error_stream::ErrorStream,
    primitive::{BinOp, PrimitiveOperation, PrimitiveValue, UnOp},
    stages::ur::UrItem, unknown::UnknownQualifier,
};

use super::{Symbol, Ur, UrExpr, UrExprKind, UrType};

pub fn r#type(mut ur: Ur, errors: ErrorStream) -> Ur {
    Typer { errors, symbols: FxHashMap::default() }.type_ur(&mut ur);
    ur
}

struct Typer<'s> {
    errors: ErrorStream,
    symbols: FxHashMap<Symbol<'s>, UrType<'s>>,
}

impl<'s> Typer<'s> {
    fn type_ur(&mut self, ur: &mut Ur<'s>) {}

    fn type_expr(&mut self, expr: &mut UrExpr<'s>, superty: &UrType<'s>) {
        let _ty = match &mut expr.kind {
            UrExprKind::Abstraction {
                id,
                input_pat,
                output_ty,
                body_expr,
            } => {
                if let Some(input_pat) = input_pat {
                    self.type_pat(input_pat, UrType::Any);
                }
                if let Some(body_expr) = body_expr {
                    let body_superty = if let Some(output_ty) = output_ty {
                        self.type_type(output_ty);
                        output_ty.ty.clone()
                    } else {
                        UrType::Any
                    };
                    self.type_expr(body_expr, &body_superty);
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
            UrExprKind::Scope { stmts, expr } => todo!(),
            UrExprKind::Dictionary { impl_ty, stmts } => todo!(),
            UrExprKind::Tuple { items } => {
                let supertys = if let UrType::Tuple(supertys) = superty {
                    &**supertys
                } else {
                    &[]
                }
                .iter()
                .chain(std::iter::repeat(&UrItem {
                    label: None,
                    value: UrType::Any,
                }));

                for (item, superty) in items.iter_mut().zip(supertys) {
                    self.type_expr(&mut item.value, &superty.value);
                }

                todo!()
            }
            UrExprKind::Union { items } => todo!(),
            UrExprKind::Lookup { symbol } => todo!(),
            UrExprKind::Unknown {
                qual,
                symbol,
                type_,
            } => match qual {
                UnknownQualifier::Val | UnknownQualifier::Var => {
                    
                    todo!()
                }
                UnknownQualifier::Set => todo!(),
            },
            UrExprKind::PrimitiveOperation { operation } => match operation {
                PrimitiveOperation::Binary(op, a, b) => match op {
                    BinOp::Apply => {
                        self.type_expr(
                            a,
                            &UrType::Abstraction(
                                None,
                                Some(Box::new(UrType::Any)),
                                Box::new(superty.clone()),
                            ),
                        );

                        let UrType::Abstraction(_id, input, output) = &a.ty else {
                            todo!("errors")
                        };

                        let Some(input) = input else { todo!("errors") };

                        self.type_expr(b, input);

                        (**output).clone()
                    }
                    BinOp::Recv => {
                        // undecided if this will actually exist.

                        todo!()
                    }
                    BinOp::LogOr | BinOp::LogAnd => {
                        self.type_expr(a, &UrType::Bool);
                        self.type_expr(b, &UrType::Bool);

                        UrType::Bool
                    }
                    BinOp::Eq | BinOp::Neq => {
                        self.type_expr(a, &UrType::Any);
                        self.type_expr(b, &UrType::Any);
                        if !a.ty.compatible_with(&b.ty) {
                            todo!("errors")
                        }

                        UrType::Bool
                    }
                    BinOp::Lt | BinOp::Leq | BinOp::Gt | BinOp::Geq => {
                        self.type_expr(a, &UrType::NUMBER);
                        self.type_expr(b, &UrType::NUMBER);
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
                        self.type_expr(a, &UrType::INTEGER);
                        self.type_expr(b, &UrType::INTEGER);
                        if a.ty != b.ty {
                            todo!("errors")
                        }

                        a.ty.clone()
                    }
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                        self.type_expr(a, &UrType::NUMBER);
                        self.type_expr(b, &UrType::NUMBER);
                        if a.ty != b.ty {
                            todo!("errors")
                        }

                        a.ty.clone()
                    }
                },
                PrimitiveOperation::Unary(op, a) => match op {
                    UnOp::Not => {
                        self.type_expr(a, &UrType::Bool);

                        UrType::Bool
                    }
                    UnOp::Neg => {
                        self.type_expr(a, &UrType::NUMBER);

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

        todo!()
    }

    fn type_pat(&mut self, expr: &mut UrExpr<'s>, superty: UrType<'s>) {}

    fn type_type(&mut self, expr: &mut UrExpr<'s>) {
        todo!()
    }
}
