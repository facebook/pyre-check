/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Display;

use dupe::Dupe;
use ruff_python_ast::Arguments;
use ruff_python_ast::Expr;
use ruff_python_ast::Identifier;
use ruff_python_ast::Keyword;

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

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum Restriction {
    Constraints(Vec<Type>),
    Bound(Type),
    Unrestricted,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum Variance {
    Covariant,
    Contravariant,
    Invariant,
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
struct TypeVarInner {
    qname: QName,
    restriction: Restriction,
    default: Option<Type>,
    /// The variance if known, or None for infer_variance=True
    variance: Option<Variance>,
}

impl TypeVar {
    pub fn new(
        name: Identifier,
        module: ModuleInfo,
        restriction: Restriction,
        default: Option<Type>,
        variance: Option<Variance>,
    ) -> Self {
        Self(ArcId::new(TypeVarInner {
            qname: QName::new(name, module),
            restriction,
            default,
            variance,
        }))
    }

    pub fn qname(&self) -> &QName {
        &self.0.qname
    }

    pub fn restriction(&self) -> &Restriction {
        &self.0.restriction
    }

    pub fn default(&self) -> Option<&Type> {
        self.0.default.as_ref()
    }

    pub fn variance(&self) -> Option<Variance> {
        self.0.variance
    }

    pub fn to_type(&self) -> Type {
        Type::TypeVar(self.dupe())
    }

    pub fn is_ctor(x: &Type) -> bool {
        matches!(
            x, Type::ClassDef(cls)
            if cls.name() == "TypeVar" && matches!(cls.module_info().name().as_str(), "typing" | "typing_extensions"))
    }
}

#[derive(Default, Debug)]
pub struct TypeVarArgs<'a> {
    pub name: Option<&'a Expr>,
    pub constraints: Vec<&'a Expr>,
    pub bound: Option<&'a Expr>,
    pub default: Option<&'a Expr>,
    pub covariant: Option<&'a Expr>,
    pub contravariant: Option<&'a Expr>,
    pub infer_variance: Option<&'a Expr>,
    pub unknown: Vec<&'a Keyword>,
}

impl<'a> TypeVarArgs<'a> {
    pub fn from_arguments(arguments: &'a Arguments) -> Self {
        let mut res = Self::default();
        match arguments.args.first() {
            Some(x) => res.name = Some(x),
            _ => {}
        }
        res.constraints = arguments.args.iter().skip(1).collect();
        for kw in arguments.keywords.iter() {
            match &kw.arg {
                Some(id) if id.id == "bound" => {
                    res.bound = Some(&kw.value);
                }
                Some(id) if id.id == "default" => {
                    res.default = Some(&kw.value);
                }
                Some(id) if id.id == "covariant" => {
                    res.covariant = Some(&kw.value);
                }
                Some(id) if id.id == "contravariant" => {
                    res.contravariant = Some(&kw.value);
                }
                Some(id) if id.id == "infer_variance" => {
                    res.infer_variance = Some(&kw.value);
                }
                _ => res.unknown.push(kw),
            }
        }
        res
    }
}
