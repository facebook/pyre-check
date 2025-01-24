/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Display;

use ruff_python_ast::name::Name;

use crate::module::module_name::ModuleName;
use crate::types::types::Type;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Callable {
    pub params: Params,
    pub ret: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Params {
    List(Vec<Param>),
    Ellipsis,
    ParamSpec(Type),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Param {
    PosOnly(Type, Required),
    Pos(Name, Type, Required),
    VarArg(Type),
    KwOnly(Name, Type, Required),
    Kwargs(Type),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Required {
    Required,
    Optional,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Kind {
    IsInstance,
    IsSubclass,
    Dataclass,
    Def,
    Anon,
}

impl Callable {
    pub fn fmt_with_type<'a, D: Display + 'a>(
        &'a self,
        f: &mut fmt::Formatter<'_>,
        wrap: &'a impl Fn(&'a Type) -> D,
    ) -> fmt::Result {
        match &self.params {
            Params::List(params) => {
                write!(f, "(")?;
                let mut kwonly = false;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    if !kwonly && matches!(param, Param::KwOnly(..)) {
                        kwonly = true;
                        write!(f, "*, ")?;
                    }
                    param.fmt_with_type(f, wrap)?;
                }
                write!(f, ") -> {}", wrap(&self.ret))
            }
            Params::Ellipsis => write!(f, "(...) -> {}", wrap(&self.ret)),
            Params::ParamSpec(ty) => {
                write!(f, "(ParamSpec({})) -> {}", wrap(ty), wrap(&self.ret))
            }
        }
    }

    pub fn list(params: Vec<Param>, ret: Type) -> Self {
        Self {
            params: Params::List(params),
            ret,
        }
    }

    pub fn ellipsis(ret: Type) -> Self {
        Self {
            params: Params::Ellipsis,
            ret,
        }
    }

    pub fn param_spec(p: Type, ret: Type) -> Self {
        Self {
            params: Params::ParamSpec(p),
            ret,
        }
    }

    pub fn visit<'a>(&'a self, mut f: impl FnMut(&'a Type)) {
        self.params.visit(&mut f);
        f(&self.ret)
    }

    pub fn visit_mut<'a>(&'a mut self, mut f: impl FnMut(&'a mut Type)) {
        self.params.visit_mut(&mut f);
        f(&mut self.ret);
    }
}

impl Params {
    pub fn visit<'a>(&'a self, mut f: impl FnMut(&'a Type)) {
        match &self {
            Params::List(params) => params.iter().for_each(|x| x.visit(&mut f)),
            Params::Ellipsis => {}
            Params::ParamSpec(ty) => f(ty),
        }
    }

    pub fn visit_mut<'a>(&'a mut self, mut f: impl FnMut(&'a mut Type)) {
        match self {
            Params::List(params) => params.iter_mut().for_each(|x| x.visit_mut(&mut f)),
            Params::Ellipsis => {}
            Params::ParamSpec(ty) => f(ty),
        }
    }
}

impl Param {
    pub fn fmt_with_type<'a, D: Display + 'a>(
        &'a self,
        f: &mut fmt::Formatter<'_>,
        wrap: impl Fn(&'a Type) -> D,
    ) -> fmt::Result {
        match self {
            Param::PosOnly(ty, _required) => write!(f, "{}", wrap(ty)),
            Param::Pos(name, ty, _required) => write!(f, "{}: {}", name, wrap(ty)),
            Param::VarArg(ty) => write!(f, "*{}", wrap(ty)),
            Param::KwOnly(name, ty, _required) => write!(f, "{}: {}", name, wrap(ty)),
            Param::Kwargs(ty) => write!(f, "**{}", wrap(ty)),
        }
    }

    pub fn visit<'a>(&'a self, mut f: impl FnMut(&'a Type)) {
        match &self {
            Param::PosOnly(ty, _required) => f(ty),
            Param::Pos(_, ty, _required) => f(ty),
            Param::VarArg(ty) => f(ty),
            Param::KwOnly(_, ty, _required) => f(ty),
            Param::Kwargs(ty) => f(ty),
        }
    }

    pub fn visit_mut<'a>(&'a mut self, mut f: impl FnMut(&'a mut Type)) {
        match self {
            Param::PosOnly(ty, _required) => f(ty),
            Param::Pos(_, ty, _required) => f(ty),
            Param::VarArg(ty) => f(ty),
            Param::KwOnly(_, ty, _required) => f(ty),
            Param::Kwargs(ty) => f(ty),
        }
    }

    pub fn is_required(&self) -> bool {
        match self {
            Param::PosOnly(_, Required::Required)
            | Param::Pos(_, _, Required::Required)
            | Param::KwOnly(_, _, Required::Required) => true,
            _ => false,
        }
    }
}

impl Kind {
    pub fn from_name(module: ModuleName, name: &Name) -> Self {
        match (module.as_str(), name.as_str()) {
            ("builtins", "isinstance") => Self::IsInstance,
            ("builtins", "issubclass") => Self::IsSubclass,
            ("dataclasses", "dataclass") => Self::Dataclass,
            _ => Self::Def,
        }
    }
}
