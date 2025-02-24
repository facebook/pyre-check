/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::Expr;
use ruff_python_ast::ExprList;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::binding::binding::Key;
use crate::error::collector::ErrorCollector;
use crate::module::short_identifier::ShortIdentifier;
use crate::types::callable::Param;
use crate::types::callable::Required;
use crate::types::literal::Lit;
use crate::types::special_form::SpecialForm;
use crate::types::tuple::Tuple;
use crate::types::types::Type;
use crate::util::prelude::SliceExt;

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    fn extra_unpack_error(&self, errors: &ErrorCollector, range: TextRange) -> Type {
        self.error(
            errors,
            range,
            "Only one unbounded type is allowed to be unpacked".to_owned(),
        )
    }

    // Check if type args can be used to construct a valid tuple type
    // Returns the successfully constructed tuple along with whether any arguments were unpacked
    // Otherwise, records an error and falls back to Any
    fn check_args_and_construct_tuple(
        &self,
        arguments: &[Expr],
        errors: &ErrorCollector,
    ) -> Result<(Tuple, bool), Type> {
        let mut prefix: Vec<Type> = Vec::new();
        let mut suffix: Vec<Type> = Vec::new();
        let mut middle: Option<Type> = None;
        let mut has_unpack = false;
        for value in arguments.iter() {
            if matches!(value, Expr::EllipsisLiteral(_)) {
                if let [t] = prefix.as_slice()
                    && middle.is_none()
                    && arguments.len() == 2
                {
                    if has_unpack || t.is_unpack() {
                        return Err(self.error(
                            errors,
                            value.range(),
                            "`...` cannot be used with an unpacked TypeVarTuple or tuple"
                                .to_owned(),
                        ));
                    } else {
                        return Ok((Tuple::unbounded(t.clone()), false));
                    }
                } else {
                    return Err(self.error(
                        errors,
                        value.range(),
                        "Invalid position for `...`".to_owned(),
                    ));
                }
            }
            let ty = self.expr_untype(value, errors);
            match ty {
                Type::Unpack(box Type::Tuple(Tuple::Concrete(elts))) => {
                    has_unpack = true;
                    if middle.is_none() {
                        prefix.extend(elts)
                    } else {
                        suffix.extend(elts)
                    }
                }
                Type::Unpack(box ty @ Type::Tuple(Tuple::Unbounded(_))) => {
                    has_unpack = true;
                    if middle.is_none() {
                        middle = Some(ty)
                    } else {
                        return Err(self.extra_unpack_error(errors, value.range()));
                    }
                }
                Type::Unpack(box Type::Tuple(Tuple::Unpacked(box (pre, mid, suff)))) => {
                    has_unpack = true;
                    if middle.is_none() {
                        prefix.extend(pre);
                        middle = Some(mid);
                        suffix.extend(suff)
                    } else {
                        return Err(self.extra_unpack_error(errors, value.range()));
                    }
                }
                Type::Unpack(box ty) if ty.is_kind_type_var_tuple() => {
                    has_unpack = true;
                    if middle.is_none() {
                        middle = Some(ty)
                    } else {
                        return Err(self.extra_unpack_error(errors, value.range()));
                    }
                }
                Type::Unpack(box ty) => {
                    return Err(self.error(
                        errors,
                        value.range(),
                        format!("Expected a tuple or TypeVarTuple, got `{}`", ty),
                    ));
                }
                ty if ty.is_kind_type_var_tuple() => {
                    return Err(self.error(
                        errors,
                        value.range(),
                        "TypeVarTuple must be unpacked".to_owned(),
                    ));
                }
                _ => {
                    if middle.is_none() {
                        prefix.push(ty)
                    } else {
                        suffix.push(ty)
                    }
                }
            }
        }
        if let Some(middle) = middle {
            Ok((Tuple::unpacked(prefix, middle, suffix), has_unpack))
        } else {
            Ok((Tuple::concrete(prefix), has_unpack))
        }
    }

    pub fn apply_special_form(
        &self,
        special_form: SpecialForm,
        arguments: &[Expr],
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        match special_form {
            SpecialForm::Optional if arguments.len() == 1 => Type::type_form(Type::Union(vec![
                self.expr_untype(&arguments[0], errors),
                Type::None,
            ])),
            SpecialForm::Optional => self.error(
                errors,
                range,
                format!(
                    "Optional requires exactly one argument but {} was found",
                    arguments.len()
                ),
            ),
            SpecialForm::Union => Type::type_form(Type::Union(
                arguments.map(|arg| self.expr_untype(arg, errors)),
            )),
            SpecialForm::Tuple => match self.check_args_and_construct_tuple(arguments, errors) {
                Ok((tuple, _)) => Type::type_form(Type::Tuple(tuple)),
                Err(ty) => ty,
            },
            SpecialForm::Literal => {
                let mut literals = Vec::new();
                for x in arguments.iter() {
                    let lit = Lit::from_expr(
                        x,
                        &|enum_name, member_name| {
                            let ty = self.get(&Key::Usage(ShortIdentifier::new(&enum_name)));
                            let cls = match &*ty {
                                Type::ClassDef(c) => c,
                                _ => {
                                    return None;
                                }
                            };
                            self.get_enum_member(cls, member_name)
                        },
                        errors,
                    );
                    literals.push(lit);
                }
                Type::type_form(self.unions(literals))
            }
            SpecialForm::Concatenate => {
                if arguments.len() < 2 {
                    self.error(
                        errors,
                        range,
                        format!(
                            "`Concatenate` must take at least two arguments, got {}",
                            arguments.len()
                        ),
                    )
                } else {
                    let args = arguments[0..arguments.len() - 1]
                        .iter()
                        .map(|x| self.expr_untype(x, errors))
                        .collect();
                    let pspec = self.expr_untype(arguments.last().unwrap(), errors);
                    Type::type_form(Type::Concatenate(args, Box::new(pspec)))
                }
            }
            SpecialForm::Callable if arguments.len() == 2 => {
                let ret = self.expr_untype(&arguments[1], errors);
                match &arguments[0] {
                    Expr::List(ExprList { elts, .. }) => {
                        match self.check_args_and_construct_tuple(elts, errors) {
                            Ok((tuple, true)) => Type::type_form(Type::callable(
                                vec![Param::VarArg(Type::Unpack(Box::new(Type::Tuple(tuple))))],
                                ret,
                            )),
                            Ok((Tuple::Concrete(elts), false)) => Type::type_form(Type::callable(
                                elts.map(|t| Param::PosOnly(t.clone(), Required::Required)),
                                ret,
                            )),
                            Ok(_) => self.error(
                                errors,
                                range,
                                "Unrecognized callable type form".to_owned(),
                            ),
                            Err(t) => t,
                        }
                    }
                    Expr::EllipsisLiteral(_) => Type::type_form(Type::callable_ellipsis(ret)),
                    name @ Expr::Name(_) => {
                        let ty = self.expr_untype(name, errors);
                        Type::type_form(Type::callable_param_spec(ty, ret))
                    }
                    x @ Expr::Subscript(_) => {
                        let ty = self.expr_untype(x, errors);
                        match ty {
                            Type::Concatenate(args, pspec) => {
                                Type::type_form(Type::callable_concatenate(args , *pspec, ret))
                            }
                            _ => self.error(errors, x.range(), format!("Callable types can only have `Concatenate` in this position, got `{}`", ty.deterministic_printing())),
                        }
                    }
                    x => self.todo(errors, "expr_infer, Callable type", x),
                }
            }
            SpecialForm::Callable => self.error(
                errors,
                range,
                format!(
                    "Callable requires exactly two arguments but {} was found",
                    arguments.len()
                ),
            ),
            SpecialForm::TypeGuard if arguments.len() == 1 => Type::type_form(Type::TypeGuard(
                Box::new(self.expr_untype(&arguments[0], errors)),
            )),
            SpecialForm::TypeGuard => self.error(
                errors,
                range,
                format!(
                    "TypeGuard requires exactly one argument but got {}",
                    arguments.len()
                ),
            ),
            SpecialForm::TypeIs if arguments.len() == 1 => Type::type_form(Type::TypeIs(Box::new(
                self.expr_untype(&arguments[0], errors),
            ))),
            SpecialForm::TypeIs => self.error(
                errors,
                range,
                format!(
                    "TypeIs requires exactly one argument but got {}",
                    arguments.len()
                ),
            ),
            SpecialForm::Unpack if arguments.len() == 1 => Type::type_form(Type::Unpack(Box::new(
                self.expr_untype(&arguments[0], errors),
            ))),
            SpecialForm::Unpack => self.error(
                errors,
                range,
                format!(
                    "Unpack requires exactly one argument but got {}",
                    arguments.len()
                ),
            ),
            SpecialForm::Type if arguments.len() == 1 => {
                Type::type_form(Type::type_form(self.expr_untype(&arguments[0], errors)))
            }
            SpecialForm::Type => self.error(
                errors,
                range,
                format!(
                    "Type requires exactly one argument but got {}",
                    arguments.len()
                ),
            ),
            SpecialForm::Annotated if arguments.len() > 1 => self.expr(&arguments[0], None, errors),
            _ => self.todo(
                errors,
                &format!(
                    "Answers::apply_special_form cannot handle `{special_form}[{}]`",
                    arguments
                        .map(|x| self.module_info().display(x).to_string())
                        .join(", ")
                ),
                range,
            ),
        }
    }
}
