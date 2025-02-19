/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::borrow::Cow;
use std::fmt;
use std::fmt::Display;

use ruff_python_ast::name::Name;
use ruff_python_ast::Identifier;
use starlark_map::ordered_map::OrderedMap;

use crate::module::module_name::ModuleName;
use crate::types::literal::Lit;
use crate::types::types::Type;
use crate::util::display::commas_iter;
use crate::util::prelude::SliceExt;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Callable {
    pub params: Params,
    pub ret: Type,
}

impl Display for Callable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_type(f, &|t| t)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ParamList(Vec<Param>);

impl ParamList {
    pub fn new(xs: Vec<Param>) -> Self {
        Self(xs)
    }

    /// Create a new ParamList from a list of types, as required position-only parameters.
    pub fn new_types(xs: &[Type]) -> Self {
        Self(xs.map(|t| Param::PosOnly(t.clone(), Required::Required)))
    }

    /// Prepend some required position-only parameters.
    pub fn prepend_types(&self, pre: &[Type]) -> Cow<ParamList> {
        if pre.is_empty() {
            Cow::Borrowed(self)
        } else {
            Cow::Owned(ParamList(
                pre.iter()
                    .map(|t| Param::PosOnly(t.clone(), Required::Required))
                    .chain(self.0.iter().cloned())
                    .collect(),
            ))
        }
    }

    pub fn fmt_with_type<'a, D: Display + 'a>(
        &'a self,
        f: &mut fmt::Formatter<'_>,
        wrap: &'a impl Fn(&'a Type) -> D,
    ) -> fmt::Result {
        let mut kwonly = false;
        for (i, param) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            if !kwonly && matches!(param, Param::KwOnly(..)) {
                kwonly = true;
                write!(f, "*, ")?;
            }
            param.fmt_with_type(f, wrap)?;
        }
        Ok(())
    }

    pub fn visit<'a>(&'a self, mut f: impl FnMut(&'a Type)) {
        self.0.iter().for_each(|x| x.visit(&mut f));
    }

    pub fn visit_mut<'a>(&'a mut self, mut f: impl FnMut(&'a mut Type)) {
        self.0.iter_mut().for_each(|x| x.visit_mut(&mut f));
    }

    pub fn items(&self) -> &[Param] {
        &self.0
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn tail(&self) -> ParamList {
        Self(self.0[1..].to_vec())
    }

    /// Type signature that permits everything, namely `*args, **kwargs`.
    pub fn everything() -> ParamList {
        ParamList(vec![
            Param::VarArg(Type::any_implicit()),
            Param::Kwargs(Type::any_implicit()),
        ])
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Params {
    List(ParamList),
    Ellipsis,
    /// Any arguments to Concatenate, followed by a ParamSpec.
    /// E.g. `Concatenate[int, str, P]` would be `ParamSpec([int, str], P)`,
    /// while `P` alone would be `ParamSpec([], P)`.
    ParamSpec(Box<[Type]>, Type),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Param {
    PosOnly(Type, Required),
    Pos(Name, Type, Required),
    VarArg(Type),
    KwOnly(Name, Type, Required),
    Kwargs(Type),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Required {
    Required,
    Optional,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CallableKind {
    IsInstance,
    IsSubclass,
    Dataclass(Box<BoolKeywords>),
    DataclassField,
    ClassMethod,
    Overload,
    Def,
    Anon,
}

/// A map from keywords to boolean values. Useful for storing sets of keyword arguments for various
/// dataclass functions.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BoolKeywords(OrderedMap<Name, bool>);

impl BoolKeywords {
    pub fn new() -> Self {
        Self(OrderedMap::new())
    }

    pub fn set_keyword(&mut self, name: Option<&Identifier>, ty: Type) {
        if let Some(name) = name.map(|id| &id.id) {
            let value = match ty {
                Type::Literal(Lit::Bool(b)) => b,
                _ => {
                    return;
                }
            };
            self.0.insert(name.clone(), value);
        }
    }

    pub fn is_set(&self, name_and_default: &(Name, bool)) -> bool {
        let (name, default) = name_and_default;
        *(self.0.get(name).unwrap_or(default))
    }
}

/// Namespace for keyword names and defaults.
pub struct DataclassKeywords;

impl DataclassKeywords {
    pub const INIT: (Name, bool) = (Name::new_static("init"), true);
    pub const ORDER: (Name, bool) = (Name::new_static("order"), false);
    pub const FROZEN: (Name, bool) = (Name::new_static("frozen"), false);
    pub const MATCH_ARGS: (Name, bool) = (Name::new_static("match_args"), true);
    pub const KW_ONLY: (Name, bool) = (Name::new_static("kw_only"), false);
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
                params.fmt_with_type(f, wrap)?;
                write!(f, ") -> {}", wrap(&self.ret))
            }
            Params::Ellipsis => write!(f, "(...) -> {}", wrap(&self.ret)),
            Params::ParamSpec(args, pspec) => {
                write!(f, "({}", commas_iter(|| args.iter().map(wrap)))?;
                match pspec {
                    Type::ParamSpecValue(params) => {
                        if !args.is_empty() && !params.is_empty() {
                            write!(f, ", ")?;
                        }
                        params.fmt_with_type(f, wrap)?;
                    }
                    _ => {
                        if !args.is_empty() {
                            write!(f, ", ")?;
                        }
                        write!(f, "ParamSpec({})", wrap(pspec))?;
                    }
                }
                write!(f, ") -> {}", wrap(&self.ret))
            }
        }
    }

    /// Like `list`, but if the last two arguments are
    /// `*args: P.args, **kwargs: P.kwargs` it produces a `concatenate` type.
    pub fn make(params: Vec<Param>, ret: Type) -> Self {
        if params.len() >= 2
            && let Param::VarArg(Type::Args(q1)) = params[params.len() - 2]
            && let Param::Kwargs(Type::Kwargs(q2)) = params[params.len() - 1]
            && q1 == q2
        {
            let len = params.len() - 2;
            let args = params
                .into_iter()
                .take(len)
                .map(|x| match x {
                    Param::PosOnly(ty, _) => ty,
                    Param::Pos(_, ty, _) => ty,
                    // TODO: Probably these are errors?
                    Param::VarArg(ty) => ty,
                    Param::KwOnly(_, ty, _) => ty,
                    Param::Kwargs(ty) => ty,
                })
                .collect();
            Self::concatenate(args, Type::Quantified(q1), ret)
        } else {
            Self::list(ParamList(params), ret)
        }
    }

    pub fn list(params: ParamList, ret: Type) -> Self {
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
            params: Params::ParamSpec(Box::default(), p),
            ret,
        }
    }

    pub fn concatenate(args: Box<[Type]>, param_spec: Type, ret: Type) -> Self {
        Self {
            params: Params::ParamSpec(args, param_spec),
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
            Params::List(params) => params.visit(f),
            Params::Ellipsis => {}
            Params::ParamSpec(args, pspec) => {
                args.iter().for_each(&mut f);
                f(pspec);
            }
        }
    }

    pub fn visit_mut<'a>(&'a mut self, mut f: impl FnMut(&'a mut Type)) {
        match self {
            Params::List(params) => params.visit_mut(f),
            Params::Ellipsis => {}
            Params::ParamSpec(args, pspec) => {
                args.iter_mut().for_each(&mut f);
                f(pspec);
            }
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

    #[expect(dead_code)]
    pub fn is_required(&self) -> bool {
        match self {
            Param::PosOnly(_, Required::Required)
            | Param::Pos(_, _, Required::Required)
            | Param::KwOnly(_, _, Required::Required) => true,
            _ => false,
        }
    }
}

impl CallableKind {
    pub fn from_name(module: ModuleName, name: &Name) -> Self {
        match (module.as_str(), name.as_str()) {
            ("builtins", "isinstance") => Self::IsInstance,
            ("builtins", "issubclass") => Self::IsSubclass,
            ("builtins", "classmethod") => Self::ClassMethod,
            ("dataclasses", "dataclass") => Self::Dataclass(Box::new(BoolKeywords::new())),
            ("dataclasses", "field") => Self::DataclassField,
            ("typing", "overload") => Self::Overload,
            _ => Self::Def,
        }
    }
}
