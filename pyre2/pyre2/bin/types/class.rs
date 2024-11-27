/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cmp::Ordering;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::sync::Arc;

use dupe::Dupe;
use parse_display::Display;
use ruff_python_ast::name::Name;
use ruff_python_ast::Identifier;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use crate::module::module_info::ModuleInfo;
use crate::types::mro::Mro;
use crate::types::qname::QName;
use crate::types::types::Quantified;
use crate::types::types::QuantifiedVec;
use crate::types::types::Type;
use crate::util::arc_id::ArcId;

/// The name of a nominal type, e.g. `str`
#[derive(Debug, Clone, Display, Dupe, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct Class(ArcId<ClassInner>);

#[derive(Clone, Debug, PartialEq, Eq)]
struct ClassInner {
    qname: QName,
    tparams: QuantifiedVec,
    fields: SmallSet<Name>,
}

impl PartialOrd for ClassInner {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ClassInner {
    fn cmp(&self, other: &Self) -> Ordering {
        self.qname.cmp(&other.qname)
    }
}

impl Display for ClassInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "class {}", self.qname.name)?;
        if !self.tparams.0.is_empty() {
            write!(
                f,
                "[{}]",
                self.tparams
                    .0
                    .iter()
                    .map(|_| "_")
                    .collect::<Vec<_>>()
                    .join(", ")
            )?;
        }
        writeln!(f, ": ...")
    }
}

// A note on terminology regarding attribute-related concepts:
// - "field" refers to something defined in a class body, with a raw type as written.
// - "member" refers to a name defined on a class, including inherited members whose
//   types should be expressed in terms of the current classe's type parameters.
// - "attribute" refers to a value actually accessed from an instance or class object,
//   which involves subtituting type arguments for the class type parameters as
//   well as descriptor handling (including method binding).
impl Class {
    pub fn new(
        name: Identifier,
        module_info: ModuleInfo,
        tparams: QuantifiedVec,
        fields: SmallSet<Name>,
    ) -> Self {
        Self(ArcId::new(ClassInner {
            qname: QName::new(name, module_info),
            tparams,
            fields,
        }))
    }

    pub fn is_enum(&self, get_mro: &dyn Fn(&Class) -> Arc<Mro>) -> bool {
        // TODO(yangdanny): we need to check the metaclass in the future
        get_mro(self)
            .ancestors_no_object()
            .iter()
            .any(|ancestor| ancestor.name().as_str() == "Enum")
    }

    pub fn contains(&self, name: &Name) -> bool {
        self.0.fields.contains(name)
    }

    pub fn name(&self) -> &Identifier {
        &self.0.qname.name
    }

    pub fn qname(&self) -> &QName {
        &self.0.qname
    }

    pub fn tparams(&self) -> &QuantifiedVec {
        &self.0.tparams
    }

    pub fn self_type(&self, tparams: &QuantifiedVec) -> Type {
        let tparams_as_targs = TArgs::new(
            tparams
                .as_slice()
                .iter()
                .map(|q| q.clone().to_type())
                .collect(),
        );
        ClassType::create_with_validated_targs(self.clone(), tparams_as_targs).to_type()
    }

    pub fn module_info(&self) -> &ModuleInfo {
        &self.0.qname.module
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct TArgs(Vec<Type>);

impl TArgs {
    pub fn new(targs: Vec<Type>) -> Self {
        Self(targs)
    }

    pub fn as_slice(&self) -> &[Type] {
        &self.0
    }

    pub fn as_mut(&mut self) -> &mut [Type] {
        &mut self.0
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn visit<'a>(&'a self, mut f: impl FnMut(&'a Type)) {
        self.0.iter().for_each(&mut f)
    }

    pub fn visit_mut<'a>(&'a mut self, mut f: impl FnMut(&'a mut Type)) {
        self.0.iter_mut().for_each(&mut f)
    }

    /// Apply a substitution to type arguments.
    ///
    /// This is useful mainly to re-express ancestors (which, in the MRO, are in terms of class
    /// type parameters)
    ///
    /// This is mainly useful to take ancestors coming from the MRO (which are always in terms
    /// of the current class's type parameters) and re-express them in terms of the current
    /// class specialized with type arguments.
    pub fn substitute(&self, substitution: &Substitution) -> Self {
        Self::new(
            self.0
                .iter()
                .map(|ty| substitution.substitute(ty.clone()))
                .collect(),
        )
    }
}

pub struct Substitution(SmallMap<Quantified, Type>);

impl Substitution {
    pub fn substitute(&self, ty: Type) -> Type {
        ty.subst(&self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ClassType(Class, TArgs);

impl Display for ClassType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Type::ClassType(self.clone()))
    }
}

impl ClassType {
    /// Create a ClassType. The caller must have validated that
    /// the targs are correctly aligned with the `tparams`; failure to do so
    /// will lead to panics downstream.
    pub fn create_with_validated_targs(class: Class, targs: TArgs) -> Self {
        Self(class, targs)
    }

    pub fn class_object(&self) -> &Class {
        &self.0
    }

    pub fn targs(&self) -> &TArgs {
        &self.1
    }

    pub fn targs_mut(&mut self) -> &mut TArgs {
        &mut self.1
    }

    /// Rewrite type arguments of some class relative to another.
    ///
    /// This is used to propagate instantiation of base class type parameters when computing
    /// the MRO.
    pub fn substitute(&self, substitution: &Substitution) -> Self {
        Self(self.0.dupe(), self.1.substitute(substitution))
    }

    pub fn substitution(&self, tparams: &QuantifiedVec) -> Substitution {
        let targs = &self.1.as_slice();
        if targs.len() != tparams.len() {
            // Invariant violation: all type arguments should be constructed through
            // `check_and_sanitize_targs_for_class`, which should guarantee zippability.
            unreachable!(
                "Encountered invalid type arguments of length {} in class `{}` (expected {})",
                targs.len(),
                self.name().id,
                tparams.len(),
            );
        }
        Substitution(
            tparams
                .as_slice()
                .iter()
                .cloned()
                .zip(targs.iter().cloned())
                .collect(),
        )
    }

    pub fn name(&self) -> &Identifier {
        self.0.name()
    }

    pub fn qname(&self) -> &QName {
        self.0.qname()
    }

    pub fn to_type(self) -> Type {
        Type::ClassType(self)
    }

    pub fn visit<'a>(&'a self, mut f: impl FnMut(&'a Type)) {
        self.1.visit(&mut f)
    }

    pub fn visit_mut<'a>(&'a mut self, mut f: impl FnMut(&'a mut Type)) {
        self.1.visit_mut(&mut f)
    }
}
