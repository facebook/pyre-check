/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Display a type. The complexity comes from if we have two classes with the same name,
//! we want to display disambiguating information (e.g. module name or location).

use std::fmt;
use std::fmt::Display;

use ruff_python_ast::name::Name;
use ruff_text_size::TextRange;
use starlark_map::small_map::Entry;
use starlark_map::small_map::SmallMap;
use starlark_map::smallmap;

use crate::module::module_name::ModuleName;
use crate::types::callable::Function;
use crate::types::class::TArgs;
use crate::types::qname::QName;
use crate::types::quantified::Quantified;
use crate::types::types::AnyStyle;
use crate::types::types::BoundMethod;
use crate::types::types::NeverStyle;
use crate::types::types::Type;
use crate::util::display::append;
use crate::util::display::commas_iter;
use crate::util::display::Fmt;

/// Information about the classes we have seen.
/// Set to None to indicate we have seen different values, or Some if they are all the same.
#[derive(Clone, Debug)]
struct ClassInfo {
    /// For each module, record either the one unique range, or None if there are multiple.
    info: SmallMap<ModuleName, Option<TextRange>>,
}

impl ClassInfo {
    fn new(qname: &QName) -> Self {
        Self {
            info: smallmap! {qname.module_name() => Some(qname.range())},
        }
    }

    fn qualified() -> Self {
        Self {
            info: SmallMap::new(),
        }
    }

    fn update(&mut self, qname: &QName) {
        match self.info.entry(qname.module_name()) {
            Entry::Vacant(e) => {
                e.insert(Some(qname.range()));
            }
            Entry::Occupied(mut e) => {
                if e.get() != &Some(qname.range()) {
                    *e.get_mut() = None;
                }
            }
        }
    }

    fn fmt(&self, qname: &QName, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let module_name = qname.module_name();
        match self.info.get(&module_name) {
            Some(None) | None => qname.fmt_with_location(f),
            _ if self.info.len() > 1 => qname.fmt_with_module(f),
            _ => qname.fmt_name(f),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct TypeDisplayContext<'a> {
    classes: SmallMap<&'a Name, ClassInfo>,
    quantifieds: SmallMap<Quantified, &'a Name>,
}

impl<'a> TypeDisplayContext<'a> {
    pub fn new(xs: &[&'a Type]) -> Self {
        let mut res = Self::default();
        for x in xs {
            res.add(x);
        }
        res
    }

    pub fn add(&mut self, t: &'a Type) {
        t.universe(&mut |t| {
            let qname = match t {
                Type::ClassDef(cls) => Some(cls.qname()),
                Type::ClassType(c) => Some(c.qname()),
                Type::TypedDict(c) => Some(c.qname()),
                Type::TypeVar(t) => Some(t.qname()),
                Type::TypeVarTuple(t) => Some(t.qname()),
                Type::ParamSpec(t) => Some(t.qname()),
                _ => None,
            };
            if let Some(qname) = qname {
                match self.classes.entry(qname.id()) {
                    Entry::Vacant(e) => {
                        e.insert(ClassInfo::new(qname));
                    }
                    Entry::Occupied(mut e) => e.get_mut().update(qname),
                }
            }
            let tparams = match t {
                Type::ClassDef(cls) => Some(cls.tparams()),
                Type::Forall(forall) => Some(&forall.tparams),
                _ => None,
            };
            if let Some(tparams) = tparams {
                for tparam in tparams.iter() {
                    self.quantifieds.insert(tparam.quantified, &tparam.name);
                }
            }
        })
    }

    pub fn display(&'a self, t: &'a Type) -> impl Display + 'a {
        Fmt(|f| self.fmt(t, f))
    }

    fn fmt_targs(&self, targs: &TArgs, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !targs.is_empty() {
            write!(
                f,
                "[{}]",
                commas_iter(|| targs.as_slice().iter().map(|t| self.display(t)))
            )
        } else {
            Ok(())
        }
    }

    fn fmt_qname(&self, qname: &QName, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.classes.get(&qname.id()) {
            Some(info) => info.fmt(qname, f),
            None => ClassInfo::qualified().fmt(qname, f), // we should not get here, if we do, be safe
        }
    }

    fn fmt_quantified(&self, quantified: &Quantified, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.quantifieds.get(quantified) {
            Some(name) => write!(f, "{name}"),
            None => write!(f, "{quantified}"),
        }
    }

    fn fmt<'b>(&self, t: &'b Type, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match t {
            // Things that have QName's and need qualifying
            Type::ClassDef(cls) => {
                write!(f, "type[")?;
                self.fmt_qname(cls.qname(), f)?;
                write!(f, "]")
            }
            Type::ClassType(class_type) => {
                self.fmt_qname(class_type.qname(), f)?;
                self.fmt_targs(class_type.targs(), f)
            }
            Type::TypedDict(typed_dict) => {
                write!(f, "TypedDict[")?;
                self.fmt_qname(typed_dict.qname(), f)?;
                self.fmt_targs(typed_dict.targs(), f)?;
                write!(f, "]")
            }
            Type::TypeVar(t) => {
                write!(f, "TypeVar[")?;
                self.fmt_qname(t.qname(), f)?;
                write!(f, "]")
            }
            Type::TypeVarTuple(t) => {
                write!(f, "TypeVarTuple[")?;
                self.fmt_qname(t.qname(), f)?;
                write!(f, "]")
            }
            Type::ParamSpec(t) => {
                write!(f, "ParamSpec[")?;
                self.fmt_qname(t.qname(), f)?;
                write!(f, "]")
            }

            // Other things
            Type::Literal(lit) => write!(f, "Literal[{}]", lit),
            Type::LiteralString => write!(f, "LiteralString"),
            Type::Callable(box c)
            | Type::Function(box Function {
                signature: c,
                metadata: _,
            }) => c.fmt_with_type(f, &|t| self.display(t)),
            Type::Overload(overload) => {
                write!(
                    f,
                    "Overload[{}",
                    self.display(&overload.signatures.first().as_type())
                )?;
                for sig in overload.signatures.iter().skip(1) {
                    write!(f, ", {}", self.display(&sig.as_type()))?;
                }
                write!(f, "]")
            }
            Type::ParamSpecValue(x) => {
                write!(f, "(")?;
                x.fmt_with_type(f, &|t| self.display(t))?;
                write!(f, ")")
            }
            Type::BoundMethod(box BoundMethod { obj, func }) => {
                write!(
                    f,
                    "BoundMethod[{}, {}]",
                    self.display(obj),
                    self.display(&func.as_type())
                )
            }
            Type::Never(NeverStyle::NoReturn) => write!(f, "NoReturn"),
            Type::Never(NeverStyle::Never) => write!(f, "Never"),
            Type::Union(types) if types.is_empty() => write!(f, "Never"),
            Type::Union(types) => {
                // All Literals will be collected into a single Literal at the index of the first Literal.
                let mut literal_idx = None;
                let mut literals = Vec::new();
                let mut display_types = Vec::new();
                for (i, t) in types.iter().enumerate() {
                    match t {
                        Type::Literal(lit) => {
                            if literal_idx.is_none() {
                                literal_idx = Some(i);
                            }
                            literals.push(lit)
                        }
                        Type::Callable(_) | Type::Function(_) => {
                            display_types.push(format!("({})", self.display(t)))
                        }
                        _ => display_types.push(format!("{}", self.display(t))),
                    }
                }
                if let Some(i) = literal_idx {
                    display_types.insert(i, format!("Literal[{}]", commas_iter(|| &literals)));
                }
                write!(f, "{}", display_types.join(" | "))
            }
            Type::Intersect(types) => {
                write!(
                    f,
                    "Intersect[{}]",
                    commas_iter(|| types.iter().map(|t| self.display(t)))
                )
            }
            Type::Tuple(t) => t.fmt_with_type(f, |t| self.display(t)),
            Type::Forall(forall) => {
                write!(
                    f,
                    "Forall[{}, {}]",
                    commas_iter(|| forall.tparams.iter()),
                    self.display(&forall.body.clone().as_type()),
                )
            }
            Type::Type(ty) => write!(f, "type[{}]", self.display(ty)),
            Type::TypeGuard(ty) => write!(f, "TypeGuard[{}]", self.display(ty)),
            Type::TypeIs(ty) => write!(f, "TypeIs[{}]", self.display(ty)),
            Type::Unpack(box ty @ Type::TypedDict(_)) => write!(f, "Unpack[{}]", self.display(ty)),
            Type::Unpack(ty) => write!(f, "*{}", self.display(ty)),
            Type::Concatenate(args, pspec) => write!(
                f,
                "Concatenate[{}]",
                commas_iter(|| append(args.iter(), [pspec]))
            ),
            Type::Module(m) => write!(f, "Module[{m}]"),
            Type::Var(var) => write!(f, "{var}"),
            Type::Quantified(var) => self.fmt_quantified(var, f),
            Type::Args(q) => {
                write!(f, "Args[")?;
                self.fmt_quantified(q, f)?;
                write!(f, "]")
            }
            Type::Kwargs(q) => {
                write!(f, "Kwargs[")?;
                self.fmt_quantified(q, f)?;
                write!(f, "]")
            }
            Type::SpecialForm(x) => write!(f, "{x}"),
            Type::Ellipsis => write!(f, "Ellipsis"),
            Type::Any(style) => match style {
                AnyStyle::Explicit => write!(f, "Any"),
                AnyStyle::Implicit => write!(f, "Unknown"),
                AnyStyle::Error => write!(f, "Error"),
            },
            Type::TypeAlias(ta) => {
                write!(
                    f,
                    "{}[{}, {}]",
                    ta.style,
                    ta.name,
                    self.display(&ta.as_type())
                )
            }
            Type::SuperInstance(cls, obj) => {
                write!(f, "super[")?;
                self.fmt_qname(cls.qname(), f)?;
                write!(f, ", ")?;
                self.fmt_qname(obj.qname(), f)?;
                self.fmt_targs(obj.targs(), f)?;
                write!(f, "]")
            }
            Type::None => write!(f, "None"),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        TypeDisplayContext::new(&[self]).fmt(self, f)
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use std::sync::Arc;

    use dupe::Dupe;
    use ruff_python_ast::Identifier;
    use ruff_text_size::TextSize;
    use starlark_map::ordered_map::OrderedMap;

    use super::*;
    use crate::module::module_info::ModuleInfo;
    use crate::module::module_path::ModulePath;
    use crate::types::callable::Callable;
    use crate::types::callable::Param;
    use crate::types::callable::ParamList;
    use crate::types::callable::Required;
    use crate::types::class::Class;
    use crate::types::class::ClassIndex;
    use crate::types::class::ClassType;
    use crate::types::literal::Lit;
    use crate::types::quantified::Quantified;
    use crate::types::quantified::QuantifiedKind;
    use crate::types::tuple::Tuple;
    use crate::types::type_var::Restriction;
    use crate::types::type_var::TypeVar;
    use crate::types::type_var::Variance;
    use crate::types::typed_dict::TypedDict;
    use crate::types::types::TParamInfo;
    use crate::types::types::TParams;
    use crate::util::uniques::UniqueFactory;

    fn fake_class(name: &str, module: &str, range: u32, tparams: Vec<TParamInfo>) -> Class {
        let mi = ModuleInfo::new(
            ModuleName::from_str(module),
            ModulePath::filesystem(PathBuf::from(module)),
            Arc::new("1234567890".to_owned()),
        );
        Class::new_identity(
            ClassIndex(0),
            Identifier::new(Name::new(name), TextRange::empty(TextSize::new(range))),
            mi,
            TParams::new(tparams).unwrap(),
            SmallMap::new(),
        )
    }

    fn fake_tparam(uniques: &UniqueFactory, name: &str, kind: QuantifiedKind) -> TParamInfo {
        TParamInfo {
            name: Name::new(name),
            quantified: Quantified::new(uniques, kind),
            restriction: Restriction::Unrestricted,
            default: None,
            variance: Some(Variance::Invariant),
        }
    }

    fn fake_tyvar(name: &str, module: &str, range: u32) -> TypeVar {
        let mi = ModuleInfo::new(
            ModuleName::from_str(module),
            ModulePath::filesystem(PathBuf::from(module)),
            Arc::new("1234567890".to_owned()),
        );
        TypeVar::new_identity(
            Identifier::new(Name::new(name), TextRange::empty(TextSize::new(range))),
            mi,
            Restriction::Unrestricted,
            None,
            Some(Variance::Invariant),
        )
    }

    #[test]
    fn test_display() {
        let uniques = UniqueFactory::new();
        let foo1 = fake_class("foo", "mod.ule", 5, Vec::new());
        let foo2 = fake_class("foo", "mod.ule", 8, Vec::new());
        let foo3 = fake_class("foo", "ule", 3, Vec::new());
        let bar = fake_class(
            "bar",
            "mod.ule",
            0,
            vec![fake_tparam(&uniques, "T", QuantifiedKind::TypeVar)],
        );

        fn class_type(class: &Class, targs: TArgs) -> Type {
            Type::ClassType(ClassType::new(class.dupe(), targs))
        }

        assert_eq!(
            Type::Tuple(Tuple::unbounded(class_type(&foo1, TArgs::default()))).to_string(),
            "tuple[foo, ...]"
        );
        assert_eq!(
            Type::Tuple(Tuple::concrete(vec![
                class_type(&foo1, TArgs::default()),
                class_type(&bar, TArgs::new(vec![class_type(&foo1, TArgs::default())]))
            ]))
            .to_string(),
            "tuple[foo, bar[foo]]"
        );
        assert_eq!(
            Type::Tuple(Tuple::concrete(vec![
                class_type(&foo1, TArgs::default()),
                class_type(&bar, TArgs::new(vec![class_type(&foo2, TArgs::default())]))
            ]))
            .to_string(),
            "tuple[mod.ule.foo@1:6, bar[mod.ule.foo@1:9]]"
        );
        assert_eq!(
            Type::Tuple(Tuple::concrete(vec![
                class_type(&foo1, TArgs::default()),
                class_type(&foo3, TArgs::default())
            ]))
            .to_string(),
            "tuple[mod.ule.foo, ule.foo]"
        );
        assert_eq!(
            Type::Tuple(Tuple::concrete(vec![])).to_string(),
            "tuple[()]"
        );

        let t1 = class_type(&foo1, TArgs::default());
        let t2 = class_type(&foo2, TArgs::default());
        let ctx = TypeDisplayContext::new(&[&t1, &t2]);
        assert_eq!(
            format!("{} <: {}", ctx.display(&t1), ctx.display(&t2)),
            "mod.ule.foo@1:6 <: mod.ule.foo@1:9"
        );
    }

    #[test]
    fn test_display_typevar() {
        let t1 = fake_tyvar("foo", "bar", 1);
        let t2 = fake_tyvar("foo", "bar", 2);
        let t3 = fake_tyvar("qux", "bar", 2);

        assert_eq!(
            Type::Union(vec![t1.to_type(), t2.to_type()]).to_string(),
            "TypeVar[bar.foo@1:2] | TypeVar[bar.foo@1:3]"
        );
        assert_eq!(
            Type::Union(vec![t1.to_type(), t3.to_type()]).to_string(),
            "TypeVar[foo] | TypeVar[qux]"
        );
    }

    #[test]
    fn test_display_literal() {
        assert_eq!(Type::Literal(Lit::Bool(true)).to_string(), "Literal[True]");
        assert_eq!(
            Type::Literal(Lit::Bool(false)).to_string(),
            "Literal[False]"
        );
    }

    #[test]
    fn test_display_union() {
        let lit1 = Type::Literal(Lit::Bool(true));
        let lit2 = Type::Literal(Lit::String("test".into()));
        let nonlit1 = Type::None;
        let nonlit2 = Type::LiteralString;

        assert_eq!(
            Type::Union(vec![nonlit1.clone(), nonlit2.clone()]).to_string(),
            "None | LiteralString"
        );
        assert_eq!(
            Type::Union(vec![nonlit1, lit1, nonlit2, lit2]).to_string(),
            "None | Literal[True, 'test'] | LiteralString"
        );
    }

    #[test]
    fn test_display_callable() {
        let param1 = Param::Pos(Name::new("hello"), Type::None, Required::Required);
        let param2 = Param::KwOnly(Name::new("world"), Type::None, Required::Required);
        let callable = Callable::list(ParamList::new(vec![param1, param2]), Type::None);
        assert_eq!(
            Type::Callable(Box::new(callable)).to_string(),
            "(hello: None, *, world: None) -> None"
        );
    }

    #[test]
    fn test_display_generic_typeddict() {
        let cls = fake_class("C", "test", 0, Vec::new());
        let t = Type::None;
        let targs = TArgs::new(vec![t]);
        let td = TypedDict::new(cls, targs, OrderedMap::new());
        assert_eq!(
            Type::TypedDict(Box::new(td)).to_string(),
            "TypedDict[C[None]]"
        );
    }
}
