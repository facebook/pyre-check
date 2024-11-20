/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt::Display;
use std::fmt::{self};

use dupe::Dupe;
use ruff_python_ast::name::Name;
use ruff_python_ast::Arguments;
use ruff_python_ast::Expr;
use ruff_python_ast::Identifier;

use crate::error::collector::ErrorCollector;
use crate::module::module_info::ModuleInfo;
use crate::types::qname::QName;
use crate::types::types::Type;
use crate::util::arc_id::ArcId;

/// Used to represent TypeVar calls. Each TypeVar is unique, so use the ArcId to separate them.
#[derive(Clone, Dupe, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct TypeVar(ArcId<TypeVarInner>);

impl Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.qname.name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
struct TypeVarInner {
    qname: QName,
}

impl TypeVar {
    pub fn new(name: Identifier, module: ModuleInfo) -> Self {
        Self(ArcId::new(TypeVarInner {
            qname: QName::new(name, module),
        }))
    }

    pub fn qname(&self) -> &QName {
        &self.0.qname
    }

    pub fn to_type(&self) -> Type {
        Type::TypeVar(self.dupe())
    }

    pub fn from_arguments(
        arguments: &Arguments,
        module_info: &ModuleInfo,
        errors: &ErrorCollector,
    ) -> Self {
        let tv = match arguments.args.first() {
            Some(Expr::StringLiteral(x)) => {
                let name = Identifier::new(Name::new(x.value.to_str()), x.range);
                TypeVar::new(name, module_info.dupe())
            }
            _ => {
                let msg = if arguments.args.is_empty() {
                    "Missing `name` argument to TypeVar"
                } else {
                    "Expected first argument of TypeVar to be a string literal"
                };
                errors.add(module_info, arguments.range, msg.to_owned());
                // FIXME: This isn't ideal - we are creating a fake Identifier, which is not good.
                TypeVar::new(
                    Identifier::new(Name::new("unknown"), arguments.range),
                    module_info.dupe(),
                )
            }
        };
        // TODO: store these on the TypeVar
        let constraints = if arguments.args.len() > 1 {
            arguments.args[1..].to_vec()
        } else {
            Vec::new()
        };
        let mut bound = None;
        let mut _default = None;
        let mut _covariant = None;
        let mut _contravariant = None;
        let mut _infer_variance = None;
        for kw in arguments.keywords.iter() {
            match &kw.arg {
                Some(id) if id.id == "bound" => {
                    bound = Some(kw.value.clone());
                }
                Some(id) if id.id == "default" => {
                    _default = Some(kw.value.clone());
                }
                Some(id) if id.id == "covariant" => {
                    _covariant = Some(kw.value.clone());
                }
                Some(id) if id.id == "contravariant" => {
                    _contravariant = Some(kw.value.clone());
                }
                Some(id) if id.id == "infer_variance" => {
                    _infer_variance = Some(kw.value.clone());
                }
                Some(id) => errors.add(
                    module_info,
                    kw.range,
                    format!("Unexpected keyword argument `{}` to TypeVar", id.id),
                ),
                None => errors.add(
                    module_info,
                    kw.range,
                    "Unexpected anonymous keyword to TypeVar".to_string(),
                ),
            }
        }
        if !constraints.is_empty() && bound.is_some() {
            errors.add(
                module_info,
                arguments.range,
                "TypeVar cannot have both constraints and bound".to_string(),
            );
        }
        tv
    }

    pub fn is_ctor(x: &Type) -> bool {
        matches!(
            x, Type::ClassDef(cls)
            if cls.name() == "TypeVar" && matches!(cls.module_info().name().as_str(), "typing" | "typing_extensions"))
    }
}
