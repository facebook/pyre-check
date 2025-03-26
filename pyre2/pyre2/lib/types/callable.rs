/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::borrow::Cow;
use std::fmt;
use std::fmt::Display;

use pyrefly_derive::TypeEq;
use pyrefly_derive::Visit;
use pyrefly_derive::VisitMut;
use ruff_python_ast::name::Name;
use ruff_python_ast::Identifier;
use ruff_python_ast::Keyword;
use starlark_map::ordered_map::OrderedMap;

use crate::module::module_name::ModuleName;
use crate::types::literal::Lit;
use crate::types::types::Type;
use crate::util::display::commas_iter;
use crate::util::prelude::SliceExt;
use crate::util::visit::Visit;
use crate::util::visit::VisitMut;

#[derive(
    Debug, Clone, Visit, VisitMut, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash
)]
pub struct Callable {
    pub params: Params,
    pub ret: Type,
}

impl Display for Callable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_type(f, &|t| t)
    }
}

#[derive(
    Debug, Clone, Default, Visit, VisitMut, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash
)]
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
            Param::VarArg(None, Type::any_implicit()),
            Param::Kwargs(Type::any_implicit()),
        ])
    }
}

#[derive(
    Debug, Clone, Visit, VisitMut, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash
)]
pub enum Params {
    List(ParamList),
    Ellipsis,
    /// Any arguments to Concatenate, followed by a ParamSpec.
    /// E.g. `Concatenate[int, str, P]` would be `ParamSpec([int, str], P)`,
    /// while `P` alone would be `ParamSpec([], P)`.
    ParamSpec(Box<[Type]>, Type),
}

#[derive(
    Debug, Clone, Visit, VisitMut, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash
)]
pub enum Param {
    PosOnly(Type, Required),
    Pos(Name, Type, Required),
    VarArg(Option<Name>, Type),
    KwOnly(Name, Type, Required),
    Kwargs(Type),
}

#[derive(
    Debug, Clone, Copy, Visit, VisitMut, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash
)]
pub enum Required {
    Required,
    Optional,
}

#[derive(
    Debug, Clone, Visit, VisitMut, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash
)]
pub struct Function {
    pub signature: Callable,
    pub metadata: FuncMetadata,
}

#[derive(
    Debug, Clone, Visit, VisitMut, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash
)]
pub struct FuncMetadata {
    pub kind: FunctionKind,
    pub flags: FuncFlags,
}

impl FuncMetadata {
    pub fn def(module: ModuleName, cls: Name, func: Name) -> Self {
        Self {
            kind: FunctionKind::Def(Box::new(FuncId {
                module,
                cls: Some(cls),
                func,
            })),
            flags: FuncFlags::default(),
        }
    }
}

#[derive(
    Debug, Clone, Visit, VisitMut, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash, Default
)]
pub struct FuncFlags {
    pub is_overload: bool,
    pub is_staticmethod: bool,
    pub is_classmethod: bool,
    /// A function decorated with `@property`
    pub is_property_getter: bool,
    /// A function decorated with `@foo.setter`, where `foo` is some `@property`-decorated function.
    /// The stored type is `foo` (the getter).
    pub is_property_setter_with_getter: Option<Type>,
    pub has_enum_member_decoration: bool,
    pub is_override: bool,
    pub has_final_decoration: bool,
}

#[derive(
    Debug, Clone, Visit, VisitMut, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash
)]
pub struct FuncId {
    pub module: ModuleName,
    pub cls: Option<Name>,
    pub func: Name,
}

impl FuncId {
    pub fn format(&self, current_module: ModuleName) -> String {
        let module_prefix =
            if self.module == current_module || self.module == ModuleName::builtins() {
                "".to_owned()
            } else {
                format!("{}.", self.module)
            };
        let class_prefix = match &self.cls {
            Some(cls) => {
                format!("{cls}.")
            }
            None => "".to_owned(),
        };
        format!("{module_prefix}{class_prefix}{}", self.func)
    }
}

#[derive(
    Debug, Clone, TypeEq, Visit, VisitMut, PartialEq, Eq, PartialOrd, Ord, Hash
)]
pub enum FunctionKind {
    IsInstance,
    IsSubclass,
    Dataclass(Box<BoolKeywords>),
    DataclassField,
    ClassMethod,
    Overload,
    Override,
    Cast,
    AssertType,
    RevealType,
    Final,
    PropertySetter(Box<FuncId>),
    Def(Box<FuncId>),
}

/// A map from keywords to boolean values. Useful for storing sets of keyword arguments for various
/// dataclass functions.
#[derive(Debug, Clone, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BoolKeywords(OrderedMap<Name, bool>);

impl Visit<Type> for BoolKeywords {
    const RECURSE_CONTAINS: bool = false;
    fn recurse<'a>(&'a self, _: &mut dyn FnMut(&'a Type)) {}
}

impl VisitMut<Type> for BoolKeywords {
    const RECURSE_CONTAINS: bool = false;
    fn recurse_mut(&mut self, _: &mut dyn FnMut(&mut Type)) {}
}

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

    pub fn set(&mut self, name: Name, value: bool) {
        self.0.insert(name, value);
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
    /// We combine default and default_factory into a single "default" keyword indicating whether
    /// the field has a default. The default value isn't stored.
    pub const DEFAULT: (Name, bool) = (Name::new_static("default"), false);
    pub const EQ: (Name, bool) = (Name::new_static("eq"), true);
    pub const UNSAFE_HASH: (Name, bool) = (Name::new_static("unsafe_hash"), false);
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
                    Type::Ellipsis => {
                        if !args.is_empty() {
                            write!(f, ", ")?;
                        }
                        write!(f, "...")?;
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

    pub fn drop_first_param(&self) -> Option<Self> {
        match self {
            Self {
                params: Params::List(params),
                ret,
            } if !params.is_empty() => Some(Self::list(params.tail(), ret.clone())),
            _ => None,
        }
    }

    pub fn is_typeguard(&self) -> bool {
        matches!(
            self,
            Self {
                params: _,
                ret: Type::TypeGuard(_)
            }
        )
    }

    pub fn is_typeis(&self) -> bool {
        matches!(
            self,
            Self {
                params: _,
                ret: Type::TypeIs(_),
            }
        )
    }

    pub fn subst_self_type_mut(&mut self, replacement: &Type) {
        match &mut self.params {
            Params::List(params) => {
                for param in params.0.iter_mut() {
                    param.subst_self_type_mut(replacement);
                }
            }
            Params::Ellipsis => {}
            Params::ParamSpec(ts, t) => {
                for t in ts.iter_mut() {
                    t.subst_self_type_mut(replacement);
                }
                t.subst_self_type_mut(replacement);
            }
        }
        self.ret.subst_self_type_mut(replacement);
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
            Param::VarArg(Some(name), ty) => write!(f, "*{}: {}", name, wrap(ty)),
            Param::VarArg(None, ty) => write!(f, "*{}", wrap(ty)),
            Param::KwOnly(name, ty, _required) => write!(f, "{}: {}", name, wrap(ty)),
            Param::Kwargs(ty) => write!(f, "**{}", wrap(ty)),
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

    fn subst_self_type_mut(&mut self, replacement: &Type) {
        match self {
            Param::PosOnly(ty, _)
            | Param::Pos(_, ty, _)
            | Param::VarArg(_, ty)
            | Param::KwOnly(_, ty, _)
            | Param::Kwargs(ty) => ty.subst_self_type_mut(replacement),
        }
    }
}

impl FunctionKind {
    pub fn from_name(module: ModuleName, cls: Option<&Name>, func: &Name) -> Self {
        match (module.as_str(), cls, func.as_str()) {
            ("builtins", None, "isinstance") => Self::IsInstance,
            ("builtins", None, "issubclass") => Self::IsSubclass,
            ("builtins", None, "classmethod") => Self::ClassMethod,
            ("dataclasses", None, "dataclass") => Self::Dataclass(Box::new(BoolKeywords::new())),
            ("dataclasses", None, "field") => Self::DataclassField,
            ("typing", None, "overload") => Self::Overload,
            ("typing", None, "override") => Self::Override,
            ("typing", None, "cast") => Self::Cast,
            ("typing", None, "assert_type") => Self::AssertType,
            ("typing", None, "reveal_type") => Self::RevealType,
            ("typing", None, "final") => Self::Final,
            _ => Self::Def(Box::new(FuncId {
                module,
                cls: cls.cloned(),
                func: func.clone(),
            })),
        }
    }

    pub fn as_func_id(&self) -> FuncId {
        match self {
            Self::IsInstance => FuncId {
                module: ModuleName::builtins(),
                cls: None,
                func: Name::new_static("isinstance"),
            },
            Self::IsSubclass => FuncId {
                module: ModuleName::builtins(),
                cls: None,
                func: Name::new_static("issubclass"),
            },
            Self::ClassMethod => FuncId {
                module: ModuleName::builtins(),
                cls: None,
                func: Name::new_static("classmethod"),
            },
            Self::Dataclass(_) => FuncId {
                module: ModuleName::dataclasses(),
                cls: None,
                func: Name::new_static("dataclass"),
            },
            Self::DataclassField => FuncId {
                module: ModuleName::dataclasses(),
                cls: None,
                func: Name::new_static("field"),
            },
            Self::Final => FuncId {
                module: ModuleName::typing(),
                cls: None,
                func: Name::new_static("final"),
            },
            Self::Overload => FuncId {
                module: ModuleName::typing(),
                cls: None,
                func: Name::new_static("overload"),
            },
            Self::Override => FuncId {
                module: ModuleName::typing(),
                cls: None,
                func: Name::new_static("override"),
            },
            Self::Cast => FuncId {
                module: ModuleName::typing(),
                cls: None,
                func: Name::new_static("cast"),
            },
            Self::AssertType => FuncId {
                module: ModuleName::typing(),
                cls: None,
                func: Name::new_static("assert_type"),
            },
            Self::RevealType => FuncId {
                module: ModuleName::typing(),
                cls: None,
                func: Name::new_static("reveal_type"),
            },
            Self::PropertySetter(func_id) | Self::Def(func_id) => (**func_id).clone(),
        }
    }
}

pub fn unexpected_keyword(error: &dyn Fn(String), func: &str, keyword: &Keyword) {
    let desc = if let Some(id) = &keyword.arg {
        format!(" `{}`", id)
    } else {
        "".to_owned()
    };
    error(format!("`{func}` got an unexpected keyword argument{desc}"));
}
