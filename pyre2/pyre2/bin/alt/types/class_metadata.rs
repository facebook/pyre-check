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

use crate::error::collector::ErrorCollector;
use crate::types::class::Class;
use crate::types::class::ClassType;
use crate::types::literal::Lit;
use crate::types::qname::QName;
use crate::types::stdlib::Stdlib;
use crate::types::types::Type;
use crate::util::display::commas_iter;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClassMetadata {
    mro: Mro,
    metaclass: Metaclass,
    keywords: Keywords,
    is_typed_dict: bool,
    is_named_tuple: bool,
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
        is_typed_dict: bool,
        is_named_tuple: bool,
        enum_metadata: Option<EnumMetadata>,
        is_protocol: bool,
        dataclass_metadata: Option<DataclassMetadata>,
        errors: &ErrorCollector,
    ) -> ClassMetadata {
        ClassMetadata {
            mro: Mro::new(cls, bases_with_metadata, errors),
            metaclass: Metaclass(metaclass),
            keywords: Keywords(keywords),
            is_typed_dict,
            is_named_tuple,
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
            is_typed_dict: false,
            is_named_tuple: false,
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

    pub fn get_keyword(&self, name: &Name) -> Option<Type> {
        self.keywords
            .0
            .iter()
            .find(|(n, _)| n.as_str() == name)
            .map(|(_, ty)| ty.clone())
    }

    pub fn is_typed_dict(&self) -> bool {
        self.is_typed_dict
    }

    pub fn is_named_tuple(&self) -> bool {
        self.is_named_tuple
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
pub struct EnumMetadata {
    pub cls: ClassType,
}

impl EnumMetadata {
    pub fn get_member(&self, name: &Name) -> Option<Lit> {
        // TODO(stroxler, yangdanny) Enums can contain attributes that are not
        // members, we eventually need to implement enough checks to know the
        // difference.
        //
        // Instance-only attributes are one case of this and are correctly handled
        // upstream, but there are other cases as well.

        // Names starting but not ending with __ are private
        // Names starting and ending with _ are reserved by the enum
        if name.starts_with("__") && !name.ends_with("__")
            || name.starts_with("_") && name.ends_with("_")
        {
            None
        } else if self.cls.class_object().contains(name) {
            Some(Lit::Enum(Box::new((self.cls.clone(), name.clone()))))
        } else {
            None
        }
    }

    pub fn get_members(&self) -> SmallSet<Lit> {
        self.cls
            .class_object()
            .fields()
            .iter()
            .filter_map(|f| self.get_member(f))
            .collect()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DataclassMetadata {
    pub fields: Vec<Name>,
    pub synthesized_methods: SmallMap<Name, Type>,
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
