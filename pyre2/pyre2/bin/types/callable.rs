use std::fmt;
use std::fmt::Display;

use ruff_python_ast::name::Name;

use crate::types::types::Type;
use crate::util::display::commas_iter;
use crate::util::display::Fmt;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Callable {
    pub args: Args,
    pub ret: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Args {
    List(Vec<Arg>),
    Ellipsis,
    ParamSpec(Type),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Arg {
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

impl Callable {
    pub fn fmt_with_type<'a, D: Display + 'a>(
        &'a self,
        f: &mut fmt::Formatter<'_>,
        wrap: &'a impl Fn(&'a Type) -> D,
    ) -> fmt::Result {
        match &self.args {
            Args::List(args) => {
                write!(
                    f,
                    "Callable[[{}], {}]",
                    commas_iter(|| args.iter().map(|x| x.display_with_type(wrap))),
                    wrap(&self.ret),
                )
            }
            Args::Ellipsis => write!(f, "Callable[..., {}]", wrap(&self.ret)),
            Args::ParamSpec(ty) => {
                write!(f, "Callable[ParamSpec({}), {}]", wrap(ty), wrap(&self.ret))
            }
        }
    }

    pub fn list(args: Vec<Arg>, ret: Type) -> Self {
        Self {
            args: Args::List(args),
            ret,
        }
    }

    pub fn ellipsis(ret: Type) -> Self {
        Self {
            args: Args::Ellipsis,
            ret,
        }
    }

    pub fn param_spec(p: Type, ret: Type) -> Self {
        Self {
            args: Args::ParamSpec(p),
            ret,
        }
    }

    pub fn args_len(&self) -> Option<usize> {
        match &self.args {
            Args::List(args) => Some(args.len()),
            Args::Ellipsis => None,
            Args::ParamSpec(_) => None,
        }
    }

    pub fn visit<'a>(&'a self, mut f: impl FnMut(&'a Type)) {
        self.args.visit(&mut f);
        f(&self.ret)
    }

    pub fn visit_mut<'a>(&'a mut self, mut f: impl FnMut(&'a mut Type)) {
        self.args.visit_mut(&mut f);
        f(&mut self.ret);
    }
}

impl Args {
    pub fn as_list(&self) -> Option<&[Arg]> {
        match self {
            Args::List(args) => Some(args),
            Args::Ellipsis => None,
            Args::ParamSpec(_) => None,
        }
    }

    pub fn visit<'a>(&'a self, mut f: impl FnMut(&'a Type)) {
        match &self {
            Args::List(args) => args.iter().for_each(|x| x.visit(&mut f)),
            Args::Ellipsis => {}
            Args::ParamSpec(ty) => f(ty),
        }
    }

    pub fn visit_mut<'a>(&'a mut self, mut f: impl FnMut(&'a mut Type)) {
        match self {
            Args::List(args) => args.iter_mut().for_each(|x| x.visit_mut(&mut f)),
            Args::Ellipsis => {}
            Args::ParamSpec(ty) => f(ty),
        }
    }
}

impl Arg {
    pub fn fmt_with_type<'a, D: Display + 'a>(
        &'a self,
        f: &mut fmt::Formatter<'_>,
        wrap: impl Fn(&'a Type) -> D,
    ) -> fmt::Result {
        match self {
            Arg::PosOnly(ty, _required) => write!(f, "{}", wrap(ty)),
            Arg::Pos(_name, ty, _required) => write!(f, "{}", wrap(ty)),
            Arg::VarArg(ty) => write!(f, "Var[{}]", wrap(ty)),
            Arg::KwOnly(name, ty, _required) => write!(f, "KwOnly[{}: {}]", name, wrap(ty)),
            Arg::Kwargs(ty) => write!(f, "KwArgs[{}]", wrap(ty)),
        }
    }

    pub fn display_with_type<'a, D: Display + 'a>(
        &'a self,
        wrap: &'a impl Fn(&'a Type) -> D,
    ) -> impl Display + 'a {
        Fmt(move |f| self.fmt_with_type(f, wrap))
    }

    pub fn visit<'a>(&'a self, mut f: impl FnMut(&'a Type)) {
        match &self {
            Arg::PosOnly(ty, _required) => f(ty),
            Arg::Pos(_, ty, _required) => f(ty),
            Arg::VarArg(ty) => f(ty),
            Arg::KwOnly(_, ty, _required) => f(ty),
            Arg::Kwargs(ty) => f(ty),
        }
    }

    pub fn visit_mut<'a>(&'a mut self, mut f: impl FnMut(&'a mut Type)) {
        match self {
            Arg::PosOnly(ty, _required) => f(ty),
            Arg::Pos(_, ty, _required) => f(ty),
            Arg::VarArg(ty) => f(ty),
            Arg::KwOnly(_, ty, _required) => f(ty),
            Arg::Kwargs(ty) => f(ty),
        }
    }

    pub fn is_required(&self) -> bool {
        match self {
            Arg::PosOnly(_, Required::Required)
            | Arg::Pos(_, _, Required::Required)
            | Arg::KwOnly(_, _, Required::Required) => true,
            _ => false,
        }
    }
}
