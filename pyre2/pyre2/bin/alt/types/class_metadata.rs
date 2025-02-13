/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;
use std::iter;
use std::sync::Arc;

use ruff_python_ast::name::Name;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;
use vec1::Vec1;

use crate::alt::class::classdef::ClassField;
use crate::alt::class::classdef::ClassFieldInner;
use crate::binding::binding::ClassFieldInitialization;
use crate::error::collector::ErrorCollector;
use crate::types::callable::DataclassKeywords;
use crate::types::class::Class;
use crate::types::class::ClassType;
use crate::types::qname::QName;
use crate::types::stdlib::Stdlib;
use crate::types::types::Type;
use crate::util::display::commas_iter;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClassMetadata {
    mro: Mro,
    metaclass: Metaclass,
    keywords: Keywords,
    typed_dict_metadata: Option<TypedDictMetadata>,
    named_tuple_metadata: Option<NamedTupleMetadata>,
    enum_metadata: Option<EnumMetadata>,
    is_protocol: bool,
    dataclass_metadata: Option<DataclassMetadata>,
}

impl Display for ClassMetadata {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "ClassMetadata({}, {})", self.mro, self.metaclass)
    }
}

impl ClassMetadata {
    pub fn new(
        cls: &Class,
        bases_with_metadata: Vec<(ClassType, Arc<ClassMetadata>)>,
        metaclass: Option<ClassType>,
        keywords: Vec<(Name, Type)>,
        typed_dict_metadata: Option<TypedDictMetadata>,
        named_tuple_metadata: Option<NamedTupleMetadata>,
        enum_metadata: Option<EnumMetadata>,
        is_protocol: bool,
        dataclass_metadata: Option<DataclassMetadata>,
        errors: &ErrorCollector,
    ) -> ClassMetadata {
        ClassMetadata {
            mro: Mro::new(cls, bases_with_metadata, errors),
            metaclass: Metaclass(metaclass),
            keywords: Keywords(keywords),
            typed_dict_metadata,
            named_tuple_metadata,
            enum_metadata,
            is_protocol,
            dataclass_metadata,
        }
    }

    pub fn recursive() -> Self {
        ClassMetadata {
            mro: Mro::Cyclic,
            metaclass: Metaclass::default(),
            keywords: Keywords::default(),
            typed_dict_metadata: None,
            named_tuple_metadata: None,
            enum_metadata: None,
            is_protocol: false,
            dataclass_metadata: None,
        }
    }

    pub fn metaclass(&self) -> Option<&ClassType> {
        self.metaclass.0.as_ref()
    }

    #[allow(dead_code)] // This is used in tests now, and will be needed later in production.
    pub fn keywords(&self) -> &[(Name, Type)] {
        &self.keywords.0
    }

    pub fn is_typed_dict(&self) -> bool {
        self.typed_dict_metadata.is_some()
    }

    pub fn typed_dict_metadata(&self) -> Option<&TypedDictMetadata> {
        self.typed_dict_metadata.as_ref()
    }

    pub fn named_tuple_metadata(&self) -> Option<&NamedTupleMetadata> {
        self.named_tuple_metadata.as_ref()
    }

    pub fn enum_metadata(&self) -> Option<&EnumMetadata> {
        self.enum_metadata.as_ref()
    }

    pub fn is_protocol(&self) -> bool {
        self.is_protocol
    }

    pub fn dataclass_metadata(&self) -> Option<&DataclassMetadata> {
        self.dataclass_metadata.as_ref()
    }

    pub fn ancestors<'a>(&'a self, stdlib: &'a Stdlib) -> impl Iterator<Item = &'a ClassType> {
        self.ancestors_no_object()
            .iter()
            .chain(iter::once(stdlib.object_class_type()))
    }

    /// The MRO doesn't track `object` directly for efficiency, since it always comes last, and
    /// some use cases (for example checking if the type is an enum) do not care about `object`.
    pub fn ancestors_no_object(&self) -> &[ClassType] {
        self.mro.ancestors_no_object()
    }

    pub fn visit_mut<'a>(&'a mut self, mut f: impl FnMut(&'a mut Type)) {
        self.mro.visit_mut(&mut f)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClassSynthesizedField {
    pub inner: ClassField,
    /// Should the synthesized field ovewrite a user-defined field of the same name defined on the same class?
    pub overwrite: bool,
}

impl Display for ClassSynthesizedField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, overwrite={})", self.inner, self.overwrite)
    }
}

impl ClassSynthesizedField {
    pub fn new(ty: Type, overwrite: bool) -> Self {
        Self {
            inner: ClassField(ClassFieldInner::Simple {
                ty,
                annotation: None,
                initialization: ClassFieldInitialization::Class,
                readonly: false,
            }),
            overwrite,
        }
    }
}

/// A class's synthesized fields, such as a dataclass's `__init__` method.
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct ClassSynthesizedFields(SmallMap<Name, ClassSynthesizedField>);

impl ClassSynthesizedFields {
    pub fn new(fields: SmallMap<Name, ClassSynthesizedField>) -> Self {
        Self(fields)
    }

    pub fn get(&self, name: &Name) -> Option<&ClassSynthesizedField> {
        self.0.get(name)
    }

    pub fn visit_type_mut(&mut self, f: &mut dyn FnMut(&mut Type)) {
        for field in self.0.values_mut() {
            field.inner.visit_type_mut(f);
        }
    }
}

impl Display for ClassSynthesizedFields {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "ClassSynthesizedFields {{ {} }}",
            commas_iter(|| self
                .0
                .iter()
                .map(|(name, field)| format!("{name} => {field}")))
        )
    }
}

/// A struct representing a class's metaclass. A value of `None` indicates
/// no explicit metaclass, in which case the default metaclass is `type`.
#[derive(Clone, Debug, PartialEq, Eq, Default)]
struct Metaclass(Option<ClassType>);

impl Display for Metaclass {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.0 {
            Some(metaclass) => write!(f, "{metaclass}"),
            None => write!(f, "type"),
        }
    }
}

/// A struct representing the keywords in a class header, e.g. for
/// `Class A(foo=True): ...` we will have `"foo": Literal[True]`.
///
/// The `metaclass` keyword is not included, since we store the metaclass
/// separately as part of `ClassMetadata`.
#[derive(Clone, Debug, PartialEq, Eq, Default)]
struct Keywords(Vec<(Name, Type)>);

impl Display for Keywords {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "[{}]",
            commas_iter(|| self.0.iter().map(|(n, ty)| format!("{n}: {ty}")))
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypedDictMetadata {
    /// Field name to the value of the `total` keyword in the defining class.
    pub fields: SmallMap<Name, bool>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumMetadata {
    pub cls: ClassType,
    /// Whether this enum inherits from enum.Flag.
    pub is_flag: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NamedTupleMetadata {
    pub elements: Vec<Name>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DataclassMetadata {
    /// The dataclass fields, e.g., `{'x'}` for `@dataclass class C: x: int`.
    pub fields: SmallSet<Name>,
    pub kws: DataclassKeywords,
}

impl DataclassMetadata {
    /// Gets the DataclassMetadata that should be inherited by a class that is a dataclass via
    /// inheritance but is not itself decorated with `@dataclass`.
    pub fn inherit(&self) -> Self {
        Self {
            // Dataclass fields are inherited.
            fields: self.fields.clone(),
            // The remaining metadata are irrelevant, so just set them to some sensible-seeming value.
            kws: self.kws,
        }
    }
}

/// A struct representing a class's ancestors, in method resolution order (MRO)
/// and after dropping cycles and nonlinearizable inheritance.
///
/// Each ancestor is represented as a pair of a class and the type arguments
/// for that class, relative to the body of the current class, so for example
/// in
/// ```
/// class A[T]: pass
/// class B[S](A[list[S]]): pass
/// class C(B[int]): pass
/// ```
/// we would get `[B[int], A[list[int]]]`.
///
/// If a class is present in multiple places of the inheritance tree (and is
/// linearizable using C3 linearization), it is possible it appears with
/// different type arguments. The type arguments computed here will always be
/// those coming from the instance that was selected during lineariation.
#[derive(Clone, Debug, PartialEq, Eq)]
enum Mro {
    Resolved(Vec<ClassType>),
    Cyclic,
}

impl Display for Mro {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Mro::Resolved(xs) => {
                write!(f, "[{}]", commas_iter(|| xs.iter()))
            }
            Mro::Cyclic => write!(f, "Cyclic"),
        }
    }
}

struct ClassName<'a>(&'a QName);

impl Display for ClassName<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        QName::fmt_with_module(self.0, f)
    }
}

impl Mro {
    /// Compute all ancestors the method resolution order (MRO).
    ///
    /// Each ancestor is paired with `targs: TArgs` representing type
    /// arguments in terms of the current class's type parameters. The `self`
    /// class is not included and should be considered implicitly at the front
    /// of the MRO.
    ///
    /// Python uses the C3 linearization algorithm to compute MRO. You can read
    /// about the algorithm and a worked-through example here:
    /// https://en.wikipedia.org/wiki/C3_linearization
    ///
    /// TODO: We currently omit some classes that are in the runtime MRO:
    /// `Generic`, `Protocol`, and `object`.
    pub fn new(
        cls: &Class,
        bases_with_metadata: Vec<(ClassType, Arc<ClassMetadata>)>,
        errors: &ErrorCollector,
    ) -> Self {
        match Linearization::new(cls, bases_with_metadata, errors) {
            Linearization::Cyclic => Self::Cyclic,
            Linearization::Resolved(ancestor_chains) => {
                let ancestors = Linearization::merge(cls, ancestor_chains, errors);
                Self::Resolved(ancestors)
            }
        }
    }

    /// The MRO doesn't track `object` directly for efficiency, since it always comes last, and
    /// some use cases (for example checking if the type is an enum) do not care about `object`.
    pub fn ancestors_no_object(&self) -> &[ClassType] {
        match self {
            Mro::Resolved(ancestors) => ancestors,
            Mro::Cyclic => &[],
        }
    }

    pub fn visit_mut<'a>(&'a mut self, mut f: impl FnMut(&'a mut Type)) {
        match self {
            Mro::Resolved(ref mut ancestors) => {
                ancestors.iter_mut().for_each(|c| c.visit_mut(&mut f))
            }
            Mro::Cyclic => {}
        }
    }
}

/// Represents one linearized "chain" of ancestors in the C3 linearization algorithm, which involves
/// building a series of linearized chains and then merging them. Each chain is one of:
/// - The MRO for a base class of the current class, or
/// - The list of direct base classes, in the order defined
///
/// All chains are represented in reverse order because that allows the merge step to be pop()-based,
/// and we use Vec1 so that we can automatically drop a chain once it's empty as the merge progresses.
struct AncestorChain(Vec1<ClassType>);

impl AncestorChain {
    fn from_base_and_ancestors(base: ClassType, base_ancestors: Vec<ClassType>) -> Self {
        AncestorChain(Vec1::from_vec_push(base_ancestors, base))
    }
}

enum Linearization {
    Resolved(Vec<AncestorChain>),
    Cyclic,
}

impl Linearization {
    pub fn empty() -> Self {
        Linearization::Resolved(vec![])
    }

    /// Implements the linearize stage of the C3 linearization algorithm for method resolution order (MRO).
    ///
    /// This is the `L(...)` function in https://web.archive.org/web/20241001181736/https://en.wikipedia.org/wiki/C3_linearization
    ///
    /// We detect cycles here, and return `Cyclic` if one is detected.
    ///
    /// The output, when successful, is a series of AncestorChains:
    /// - One for each base class's MRO, in the order that base classes are defined
    /// - One consisting of the base classes themselves in the order defined.
    fn new(
        cls: &Class,
        bases_with_metadata: Vec<(ClassType, Arc<ClassMetadata>)>,
        errors: &ErrorCollector,
    ) -> Linearization {
        let bases = match Vec1::try_from_vec(
            bases_with_metadata
                .iter()
                .rev()
                .map(|(base, _)| base.clone())
                .collect(),
        ) {
            Ok(bases) => bases,
            Err(_) => return Linearization::empty(),
        };
        let mut ancestor_chains = Vec::new();
        for (base, mro) in bases_with_metadata.iter() {
            match &**mro {
                ClassMetadata {
                    mro: Mro::Resolved(ancestors),
                    ..
                } => {
                    let ancestors_through_base = ancestors
                        .iter()
                        .map(|ancestor| ancestor.substitute(&base.substitution()))
                        .rev()
                        .collect::<Vec<_>>();
                    ancestor_chains.push(AncestorChain::from_base_and_ancestors(
                        base.clone(),
                        ancestors_through_base,
                    ));
                }
                // None and Cyclic both indicate a cycle, the distinction just
                // depends on how exactly the recursion in resolving keys plays out.
                ClassMetadata {
                    mro: Mro::Cyclic, ..
                } => {
                    errors.add(
                        cls.module_info(),
                        cls.name().range,
                        format!(
                            "Class `{}` inheriting from `{}` creates a cycle.",
                            ClassName(cls.qname()),
                            ClassName(base.qname()),
                        ),
                    );
                    // Signal that we detected a cycle
                    return Linearization::Cyclic;
                }
            }
        }
        ancestor_chains.push(AncestorChain(bases));
        Linearization::Resolved(ancestor_chains)
    }

    /// Implements the `merge` step of the C3 linearization algorithm for method resolution order (MRO).
    ///
    /// We detect linearization failures here; if one occurs we abort with the merge results thus far.
    fn merge(
        cls: &Class,
        mut ancestor_chains: Vec<AncestorChain>,
        errors: &ErrorCollector,
    ) -> Vec<ClassType> {
        // Merge the base class ancestors into a single Vec, in MRO order.
        //
        // The merge rule says we take the first available "head" of a chain (which are represented
        // as revered vecs) that is not in the "tail" of any chain, then strip it from all chains.
        let mut ancestors = Vec::new();
        while !ancestor_chains.is_empty() {
            // Identify a candiate for the next MRO entry: it must be the next ancestor in some chain,
            // and not be in the tail of any chain.
            let mut selected = None;
            for candidate_chain in ancestor_chains.iter() {
                let candidate = candidate_chain.0.last();
                let mut rejected = false;
                for ancestor_chain in ancestor_chains.iter() {
                    if ancestor_chain
                        .0
                        .iter()
                        .rev()
                        .skip(1)
                        .any(|class| class.qname() == candidate.qname())
                    {
                        rejected = true;
                        break;
                    }
                }
                if !rejected {
                    selected = Some(candidate.clone());
                    break;
                }
            }
            if let Some(selected) = selected {
                // Strip the selected class from all chains. Any empty chain is removed.
                let mut chains_to_remove = Vec::new();
                for (idx, ancestors) in ancestor_chains.iter_mut().enumerate() {
                    if ancestors.0.last().class_object().qname() == selected.class_object().qname()
                    {
                        match ancestors.0.pop() {
                            Ok(_) => {}
                            Err(_) => chains_to_remove.push(idx),
                        }
                    }
                }
                for (offset, idx) in chains_to_remove.into_iter().enumerate() {
                    ancestor_chains.remove(idx - offset);
                }
                // Push the selected class onto the result
                ancestors.push(selected);
            } else {
                // The ancestors are not linearizable at this point. Record an error and stop with
                // what we have so far.
                // (The while loop invariant ensures that ancestor_chains is non-empty, so unwrap is safe.)
                let first_candidate = &ancestor_chains.first().unwrap().0.last().class_object();
                errors.add(
                    cls.module_info(),
                    cls.name().range,
                    format!(
                        "Class `{}` has a nonlinearizable inheritance chain detected at `{}`.",
                        ClassName(cls.qname()),
                        ClassName(first_candidate.qname()),
                    ),
                );

                ancestor_chains = Vec::new()
            }
        }
        ancestors
    }
}
