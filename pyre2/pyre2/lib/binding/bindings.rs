/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::mem;
use std::sync::Arc;

use dupe::Dupe;
use itertools::Either;
use itertools::Itertools;
use ruff_python_ast::name::Name;
use ruff_python_ast::AnyParameterRef;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprAttribute;
use ruff_python_ast::ExprName;
use ruff_python_ast::ExprYield;
use ruff_python_ast::ExprYieldFrom;
use ruff_python_ast::Identifier;
use ruff_python_ast::Stmt;
use ruff_python_ast::StmtReturn;
use ruff_python_ast::TypeParam;
use ruff_python_ast::TypeParams;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;
use starlark_map::small_map::Entry;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;
use starlark_map::Hashed;
use vec1::Vec1;

use crate::binding::binding::AnnotationTarget;
use crate::binding::binding::Binding;
use crate::binding::binding::BindingAnnotation;
use crate::binding::binding::BindingExport;
use crate::binding::binding::BindingLegacyTypeParam;
use crate::binding::binding::BindingYield;
use crate::binding::binding::BindingYieldFrom;
use crate::binding::binding::Key;
use crate::binding::binding::KeyAnnotation;
use crate::binding::binding::KeyClass;
use crate::binding::binding::KeyExport;
use crate::binding::binding::KeyFunction;
use crate::binding::binding::KeyLegacyTypeParam;
use crate::binding::binding::KeyYield;
use crate::binding::binding::KeyYieldFrom;
use crate::binding::binding::Keyed;
use crate::binding::binding::TypeParameter;
use crate::binding::narrow::NarrowOps;
use crate::binding::scope::Flow;
use crate::binding::scope::FlowInfo;
use crate::binding::scope::FlowStyle;
use crate::binding::scope::InstanceAttribute;
use crate::binding::scope::Loop;
use crate::binding::scope::LoopExit;
use crate::binding::scope::ScopeKind;
use crate::binding::scope::ScopeTrace;
use crate::binding::scope::Scopes;
use crate::binding::table::TableKeyed;
use crate::error::collector::ErrorCollector;
use crate::error::kind::ErrorKind;
use crate::export::exports::Exports;
use crate::export::exports::LookupExport;
use crate::export::special::SpecialEntry;
use crate::export::special::SpecialEnv;
use crate::export::special::SpecialExport;
use crate::graph::index::Idx;
use crate::graph::index::Index;
use crate::graph::index_map::IndexMap;
use crate::metadata::RuntimeMetadata;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::module::short_identifier::ShortIdentifier;
use crate::solver::solver::Solver;
use crate::state::loader::FindError;
use crate::table;
use crate::table_for_each;
use crate::table_try_for_each;
use crate::types::quantified::QuantifiedKind;
use crate::types::types::Type;
use crate::types::types::Var;
use crate::util::display::DisplayWithCtx;
use crate::util::uniques::UniqueFactory;

#[derive(Clone, Dupe, Debug)]
pub struct Bindings(Arc<BindingsInner>);

pub type BindingEntry<K> = (Index<K>, IndexMap<K, <K as Keyed>::Value>);

table! {
    #[derive(Debug, Clone, Default)]
    pub struct BindingTable(pub BindingEntry)
}

#[derive(Clone, Debug)]
struct BindingsInner {
    module_info: ModuleInfo,
    table: BindingTable,
    scope_trace: Option<ScopeTrace>,
}

impl Display for Bindings {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn go<K: Keyed>(
            items: &BindingEntry<K>,
            me: &Bindings,
            f: &mut fmt::Formatter<'_>,
        ) -> fmt::Result {
            for (idx, k) in items.0.items() {
                writeln!(
                    f,
                    "{} = {}",
                    me.module_info().display(k),
                    items.1.get_exists(idx).display_with(me)
                )?;
            }
            Ok(())
        }
        table_try_for_each!(self.0.table, |items| go(items, self, f));
        Ok(())
    }
}

pub struct BindingsBuilder<'a> {
    pub module_info: ModuleInfo,
    pub lookup: &'a dyn LookupExport,
    pub config: &'a RuntimeMetadata,
    pub class_count: u32,
    errors: &'a ErrorCollector,
    solver: &'a Solver,
    uniques: &'a UniqueFactory,
    pub scopes: Scopes,
    pub functions: Vec1<FuncInfo>,
    pub table: BindingTable,
}

/// Things we collect from inside a function
#[derive(Default, Clone, Debug)]
pub struct FuncInfo {
    pub returns: Vec<StmtReturn>,
    pub yields: Vec<Either<ExprYield, ExprYieldFrom>>,
}

impl Bindings {
    #[expect(dead_code)] // Useful API
    fn len(&self) -> usize {
        let mut res = 0;
        table_for_each!(&self.0.table, |x: &BindingEntry<_>| res += x.1.len());
        res
    }

    pub fn display<K: Keyed>(&self, idx: Idx<K>) -> impl Display + '_
    where
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
    {
        self.module_info().display(self.idx_to_key(idx))
    }

    pub fn module_info(&self) -> &ModuleInfo {
        &self.0.module_info
    }

    pub fn available_definitions(&self, position: TextSize) -> SmallSet<Idx<Key>> {
        if let Some(trace) = &self.0.scope_trace {
            trace.available_definitions(&self.0.table, position)
        } else {
            SmallSet::new()
        }
    }

    pub fn definition_at_position(&self, position: TextSize) -> Option<&Key> {
        if let Some(trace) = &self.0.scope_trace {
            trace.definition_at_position(&self.0.table, position)
        } else {
            None
        }
    }

    /// Within the LSP, check if a Usage key exists.
    /// It may not exist within `if False:` or `if sys.version == 0:` style code.
    pub fn is_valid_usage(&self, k: &Identifier) -> bool {
        self.0
            .table
            .get::<Key>()
            .0
            .key_to_idx(&Key::Usage(ShortIdentifier::new(k)))
            .is_some()
    }

    pub fn key_to_idx<K: Keyed>(&self, k: &K) -> Idx<K>
    where
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
    {
        self.key_to_idx_hashed(Hashed::new(k))
    }

    pub fn key_to_idx_hashed<K: Keyed>(&self, k: Hashed<&K>) -> Idx<K>
    where
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
    {
        self.0
            .table
            .get::<K>()
            .0
            .key_to_idx_hashed(k)
            .unwrap_or_else(|| {
                panic!(
                    "key_to_idx - key not found, module `{}`, path `{}`, key {k:?}",
                    self.0.module_info.name(),
                    self.0.module_info.path(),
                )
            })
    }

    pub fn get<K: Keyed>(&self, idx: Idx<K>) -> &K::Value
    where
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
    {
        self.0.table.get::<K>().1.get(idx).unwrap_or_else(|| {
            let key = self.idx_to_key(idx);
            panic!(
                "Internal error: key lacking binding, module={}, path={}, key={}, key-debug={key:?}",
                self.module_info().name(),
                self.module_info().path(),
                self.module_info().display(key),
            )
        })
    }

    pub fn idx_to_key<K: Keyed>(&self, idx: Idx<K>) -> &K
    where
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
    {
        self.0.table.get::<K>().0.idx_to_key(idx)
    }

    pub fn keys<K: Keyed>(&self) -> impl ExactSizeIterator<Item = Idx<K>> + '_
    where
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
    {
        self.0.table.get::<K>().0.items().map(|(k, _)| k)
    }

    pub fn get_lambda_param(&self, name: &Identifier) -> Var {
        let b = self.get(self.key_to_idx(&Key::Definition(ShortIdentifier::new(name))));
        if let Binding::LambdaParameter(var) = b {
            *var
        } else {
            panic!(
                "Internal error: unexpected binding for lambda parameter `{}` @  {:?}: {}, module={}, path={}",
                &name.id,
                name.range,
                b.display_with(self),
                self.module_info().name(),
                self.module_info().path(),
            )
        }
    }

    pub fn get_function_param(&self, name: &Identifier) -> Either<Idx<KeyAnnotation>, Var> {
        let b = self.get(self.key_to_idx(&Key::Definition(ShortIdentifier::new(name))));
        if let Binding::FunctionParameter(p) = b {
            match p {
                Either::Left(idx) => Either::Left(*idx),
                Either::Right((var, _, _)) => Either::Right(*var),
            }
        } else {
            panic!(
                "Internal error: unexpected binding for parameter `{}` @  {:?}: {}, module={}, path={}",
                &name.id,
                name.range,
                b.display_with(self),
                self.module_info().name(),
                self.module_info().path(),
            )
        }
    }

    pub fn new(
        module_scope_range: TextRange,
        x: Vec<Stmt>,
        module_info: ModuleInfo,
        exports: Exports,
        solver: &Solver,
        lookup: &dyn LookupExport,
        config: &RuntimeMetadata,
        errors: &ErrorCollector,
        uniques: &UniqueFactory,
        enable_trace: bool,
    ) -> Self {
        let mut builder = BindingsBuilder {
            module_info: module_info.dupe(),
            lookup,
            config,
            errors,
            solver,
            uniques,
            class_count: 0,
            scopes: Scopes::module(module_scope_range, enable_trace),
            functions: Vec1::new(FuncInfo::default()),
            table: Default::default(),
        };
        builder.init_static_scope(&x, true);
        if module_info.name() != ModuleName::builtins() {
            builder.inject_builtins();
        }
        builder.stmts(x);
        // Create dummy bindings for any invalid yield/yield from expressions.
        let (func_info, _) = builder.functions.split_off_first();
        for x in func_info.yields {
            match x {
                Either::Left(x) => {
                    builder
                        .table
                        .insert(KeyYield(x.range), BindingYield::Invalid(x));
                }
                Either::Right(x) => {
                    builder
                        .table
                        .insert(KeyYieldFrom(x.range), BindingYieldFrom::Invalid(x));
                }
            }
        }
        for x in func_info.returns {
            if let Some(x) = x.value {
                builder
                    .table
                    .insert(Key::Anon(x.range()), Binding::Expr(None, *x));
            }
            errors.add(
                x.range,
                "Invalid `return` outside of a function".to_owned(),
                ErrorKind::BadReturn,
                None,
            );
        }
        let scope_trace = builder.scopes.finish();
        let last_scope = scope_trace.toplevel_scope();
        let exported = exports.exports(lookup);
        for (k, static_info) in last_scope.stat.0.iter_hashed() {
            if static_info.is_nonlocal() || static_info.is_global() {
                // Nonlocal and global don't do anything outside a function
                continue;
            }
            let info = last_scope.flow.info.get_hashed(k);
            let binding = match info {
                Some(FlowInfo { key, .. }) => {
                    if let Some(ann) = static_info.annot {
                        Binding::AnnotatedType(ann, Box::new(Binding::Forward(*key)))
                    } else {
                        Binding::Forward(*key)
                    }
                }
                None => {
                    // We think we have a binding for this, but we didn't encounter a flow element, so have no idea of what.
                    // This might be because we haven't fully implemented all bindings, or because the two disagree. Just guess.
                    errors.add(
                        static_info.loc,
                        format!("Could not find flow binding for `{k}`"),
                        ErrorKind::InternalError,
                        None,
                    );
                    Binding::Type(Type::any_error())
                }
            };
            if exported.contains_key_hashed(k) {
                builder
                    .table
                    .insert(KeyExport(k.into_key().clone()), BindingExport(binding));
            }
        }
        Self(Arc::new(BindingsInner {
            module_info,
            table: builder.table,
            scope_trace: if enable_trace {
                Some(scope_trace)
            } else {
                None
            },
        }))
    }
}

impl BindingTable {
    pub fn insert<K: Keyed>(&mut self, key: K, value: K::Value) -> Idx<K>
    where
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
    {
        let entry = self.get_mut::<K>();
        let idx = entry.0.insert(key);
        self.insert_idx(idx, value)
    }

    pub fn insert_idx<K: Keyed>(&mut self, idx: Idx<K>, value: K::Value) -> Idx<K>
    where
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
    {
        let entry = self.get_mut::<K>();
        let existing = entry.1.insert(idx, value);
        if let Some(existing) = existing {
            panic!(
                "Key {:?} already exists with value {:?}, cannot insert new value {:?}",
                entry.0.idx_to_key(idx),
                existing,
                entry.1.get_exists(idx)
            );
        }
        idx
    }

    fn insert_anywhere(
        &mut self,
        name: Name,
        range: TextRange,
    ) -> (Idx<Key>, &mut SmallSet<Idx<Key>>) {
        let idx = self.types.0.insert(Key::Anywhere(name, range));
        match self
            .types
            .1
            .insert_if_missing(idx, || Binding::Phi(SmallSet::new(), None))
        {
            Binding::Phi(phi, _) => (idx, phi),
            _ => unreachable!(),
        }
    }
}

/// Errors that can occur when we try to look up a name
pub enum LookupError {
    /// We can't find the name at all
    NotFound,
    /// We expected the name to be mutable from the current scope, but it's not
    NotMutable,
    /// We expected the name to be in an enclosing, non-global scope, but it's not
    NonlocalScope,
    /// This variable was assigned before the nonlocal declaration
    AssignedBeforeNonlocal,
    /// We expected the name to be in the global scope, but it's not
    GlobalScope,
    /// This variable was assigned before the global declaration
    AssignedBeforeGlobal,
}

impl LookupError {
    pub fn message(&self, name: &Identifier) -> String {
        match self {
            Self::NotFound => format!("Could not find name `{name}`"),
            Self::NotMutable => format!("`{name}` is not mutable from the current scope"),
            Self::NonlocalScope => {
                format!("Found `{name}`, but it was not in a valid enclosing scope")
            }
            Self::AssignedBeforeNonlocal => {
                format!(
                    "`{name}` was assigned in the current scope before the nonlocal declaration"
                )
            }
            Self::GlobalScope => {
                format!("Found `{name}`, but it was not the global scope")
            }
            Self::AssignedBeforeGlobal => {
                format!("`{name}` was assigned in the current scope before the global declaration")
            }
        }
    }
}

#[derive(PartialEq, Eq)]
pub enum LookupKind {
    Regular,
    /// Look up a name that must be mutable from the current scope, like a `del` or augmented assignment statement
    Mutable,
    /// Look up a name in a `global` statement
    Global,
    /// Look up a name in a `nonlocal` statement
    Nonlocal,
}

impl<'a> BindingsBuilder<'a> {
    pub fn init_static_scope(&mut self, x: &[Stmt], top_level: bool) {
        let current = self.scopes.current_mut();
        current.stat.stmts(
            x,
            &self.module_info,
            top_level,
            self.lookup,
            self.config,
            |x| {
                self.table
                    .annotations
                    .0
                    .insert(KeyAnnotation::Annotation(x))
            },
        );
        // Presize the flow, as its likely to need as much space as static
        current.flow.info.reserve(current.stat.0.capacity());
    }

    pub fn stmts(&mut self, xs: Vec<Stmt>) {
        for x in xs {
            self.stmt(x);
        }
    }

    fn inject_builtins(&mut self) {
        let builtins_module = ModuleName::builtins();
        match self.lookup.get(builtins_module) {
            Ok(builtins_export) => {
                for name in builtins_export.wildcard(self.lookup).iter() {
                    let key = Key::Import(name.clone(), TextRange::default());
                    let idx = self
                        .table
                        .insert(key, Binding::Import(builtins_module, name.clone()));
                    self.bind_key(
                        name,
                        idx,
                        Some(FlowStyle::Import(builtins_module, name.clone())),
                    );
                }
            }
            Err(FindError::NotFound(err)) => {
                self.error(
                    TextRange::default(),
                    FindError::display(err, builtins_module),
                    ErrorKind::InternalError,
                );
            }
            Err(FindError::Ignored) => (),
        }
    }

    pub fn todo(&mut self, msg: &str, x: impl Ranged + Debug) {
        self.errors.todo(msg, x);
    }

    pub fn as_special_export(&self, e: &Expr) -> Option<SpecialExport> {
        SpecialExport::as_special_export(self, e)
    }

    pub fn error(&self, range: TextRange, msg: String, error_kind: ErrorKind) {
        self.errors.add(range, msg, error_kind, None);
    }

    pub fn lookup_name(&mut self, name: &Name, kind: LookupKind) -> Result<Idx<Key>, LookupError> {
        self.lookup_name_hashed(Hashed::new(name), kind)
    }

    pub fn lookup_name_hashed(
        &mut self,
        name: Hashed<&Name>,
        kind: LookupKind,
    ) -> Result<Idx<Key>, LookupError> {
        let mut barrier = false;
        let mut allow_nonlocal_reference = kind == LookupKind::Nonlocal;
        let mut allow_global_reference = kind == LookupKind::Global;
        let mut result = Err(LookupError::NotFound);
        // If there is static info for the name in the current scope and this value is not None
        // set the `annot` field to this value
        let mut static_annot_override = None;
        for (idx, scope) in self.scopes.iter_rev().enumerate() {
            let in_current_scope = idx == 0;
            let valid_nonlocal_reference = allow_nonlocal_reference
                && !in_current_scope
                && !matches!(scope.kind, ScopeKind::Module | ScopeKind::ClassBody(_));
            let valid_global_reference = allow_global_reference
                && !in_current_scope
                && matches!(scope.kind, ScopeKind::Module);
            if let Some(flow) = scope.flow.info.get_hashed(name) {
                match kind {
                    LookupKind::Regular => {
                        if !barrier || valid_nonlocal_reference || valid_global_reference {
                            return Ok(flow.key);
                        }
                    }
                    LookupKind::Mutable => {
                        if !barrier || valid_nonlocal_reference || valid_global_reference {
                            return Ok(flow.key);
                        } else {
                            return Err(LookupError::NotMutable);
                        }
                    }
                    LookupKind::Nonlocal => {
                        if in_current_scope {
                            // If there's a flow type for the name in the current scope
                            // it must have been assigned before
                            return Err(LookupError::AssignedBeforeNonlocal);
                        }
                    }
                    LookupKind::Global => {
                        if in_current_scope {
                            // If there's a flow type for the name in the current scope
                            // it must have been assigned before
                            return Err(LookupError::AssignedBeforeGlobal);
                        }
                    }
                }
            }
            if !matches!(scope.kind, ScopeKind::ClassBody(_))
                && let Some(info) = scope.stat.0.get_hashed(name)
            {
                if !info.is_nonlocal() && !info.is_global() {
                    match kind {
                        LookupKind::Regular => {
                            let key = info.as_key(name.into_key());
                            return Ok(self.table.types.0.insert(key));
                        }
                        LookupKind::Mutable => {
                            if barrier && !valid_nonlocal_reference {
                                return Err(LookupError::NotMutable);
                            }
                        }
                        LookupKind::Nonlocal => {
                            if valid_nonlocal_reference {
                                let key = info.as_key(name.into_key());
                                result = Ok(self.table.types.0.insert(key));
                                // We can't return immediately, because we need to override
                                // the static annotation in the current scope with the one we found
                                static_annot_override = info.annot;
                                break;
                            } else if !in_current_scope {
                                return Err(LookupError::NonlocalScope);
                            }
                        }
                        LookupKind::Global => {
                            if valid_global_reference {
                                let key = info.as_key(name.into_key());
                                result = Ok(self.table.types.0.insert(key));
                                // We can't return immediately, because we need to override
                                // the static annotation in the current scope with the one we found
                                static_annot_override = info.annot;
                                break;
                            } else if !in_current_scope {
                                return Err(LookupError::GlobalScope);
                            }
                        }
                    }
                }
                if !barrier && info.is_nonlocal() {
                    allow_nonlocal_reference = true;
                }
                if !barrier && info.is_global() {
                    allow_global_reference = true;
                }
            }
            barrier = barrier || scope.barrier;
        }
        if let Some(annot) = static_annot_override
            && let Some(current_scope_info) = self.scopes.current_mut().stat.0.get_mut_hashed(name)
        {
            current_scope_info.annot = Some(annot);
        }
        result
    }

    pub fn forward_lookup(&mut self, name: &Identifier) -> Result<Binding, LookupError> {
        self.lookup_name(&name.id, LookupKind::Regular)
            .map(Binding::Forward)
    }

    pub fn lookup_legacy_tparam(
        &mut self,
        name: &Identifier,
    ) -> Either<Idx<KeyLegacyTypeParam>, Result<Idx<Key>, LookupError>> {
        let found = self.lookup_name(&name.id, LookupKind::Regular);
        if let Ok(mut idx) = found {
            loop {
                if let Some(b) = self.table.types.1.get(idx) {
                    match b {
                        Binding::Forward(fwd_idx) => {
                            idx = *fwd_idx;
                            continue;
                        }
                        Binding::TypeVar(..)
                        | Binding::ParamSpec(..)
                        | Binding::TypeVarTuple(..) => {
                            return Either::Left(self.table.insert(
                                KeyLegacyTypeParam(ShortIdentifier::new(name)),
                                BindingLegacyTypeParam(idx),
                            ));
                        }
                        Binding::Import(..) => {
                            // TODO: We need to recursively look through imports to determine
                            // whether it is a legacy type parameter. We can't simply walk through
                            // bindings, because we could recursively reach ourselves, resulting in
                            // a deadlock.
                            return Either::Left(self.table.insert(
                                KeyLegacyTypeParam(ShortIdentifier::new(name)),
                                BindingLegacyTypeParam(idx),
                            ));
                        }
                        _ => {}
                    }
                }
                break;
            }
        }
        Either::Right(found)
    }

    pub fn bind_definition(
        &mut self,
        name: &Identifier,
        binding: Binding,
        style: Option<FlowStyle>,
    ) -> Option<Idx<KeyAnnotation>> {
        let idx = self
            .table
            .insert(Key::Definition(ShortIdentifier::new(name)), binding);
        self.bind_key(&name.id, idx, style)
    }

    pub fn bind_assign(
        &mut self,
        name: &ExprName,
        binding: impl FnOnce(Option<Idx<KeyAnnotation>>) -> Binding,
    ) {
        let key = Key::Definition(ShortIdentifier::expr_name(name));
        let idx = self.table.types.0.insert(key);
        let ann = self.bind_key(&name.id, idx, None);
        self.table.types.1.insert(idx, binding(ann));
    }

    /// In methods, we track assignments to `self` attribute targets so that we can
    /// be aware of class fields defined in methods.
    ///
    /// We currently apply this logic in all methods, although downstream code only uses
    /// attributes defined in constructors; this may change in the future.
    ///
    /// Returns `true` if the attribute was a self attribute.
    pub fn bind_attr_if_self(
        &mut self,
        x: &ExprAttribute,
        binding: Binding,
        annotation: Option<Idx<KeyAnnotation>>,
    ) -> bool {
        for scope in self.scopes.iter_rev_mut() {
            if let ScopeKind::Method(method) = &mut scope.kind
                && let Some(self_name) = &method.self_name
                && matches!(&*x.value, Expr::Name(name) if name.id == self_name.id)
            {
                if !method.instance_attributes.contains_key(&x.attr.id) {
                    method.instance_attributes.insert(
                        x.attr.id.clone(),
                        InstanceAttribute(binding, annotation, x.attr.range()),
                    );
                }
                return true;
            }
        }
        false
    }

    /// Return the annotation that should be used at the moment, if one was provided.
    pub fn bind_key(
        &mut self,
        name: &Name,
        key: Idx<Key>,
        style: Option<FlowStyle>,
    ) -> Option<Idx<KeyAnnotation>> {
        let name = Hashed::new(name);
        self.scopes.update_flow_info_hashed(name, key, style);
        let info = self
            .scopes
            .current()
            .stat
            .0
            .get_hashed(name)
            .unwrap_or_else(|| {
                let module = self.module_info.name();
                panic!("Name `{name}` not found in static scope of module `{module}`")
            });
        if info.count > 1 {
            self.table
                .insert_anywhere(name.into_key().clone(), info.loc)
                .1
                .insert(key);
        }
        info.annot
    }

    pub fn handle_type_param_constraint(&mut self, x: &mut Expr) -> Idx<Key> {
        self.ensure_type(x, &mut None);
        self.table
            .insert(Key::Anon(x.range()), Binding::Expr(None, x.clone()))
    }

    pub fn type_params(&mut self, x: &mut TypeParams) {
        for x in x.type_params.iter_mut() {
            let mut default_info = None;
            let mut bound_info = None;
            let mut constraint_info = None;
            let kind = match x {
                TypeParam::TypeVar(tv) => {
                    if let Some(box bound) = &mut tv.bound {
                        if let Expr::Tuple(tuple) = bound {
                            let mut constraints = Vec::new();
                            for constraint in &mut tuple.elts {
                                let idx = self.handle_type_param_constraint(constraint);
                                constraints.push(idx);
                            }
                            constraint_info = Some((constraints, bound.range()))
                        } else {
                            let idx = self.handle_type_param_constraint(bound);
                            bound_info = Some((idx, bound.range()));
                        }
                    }
                    if let Some(box default) = &mut tv.default {
                        let idx = self.handle_type_param_constraint(default);
                        default_info = Some((idx, default.range()));
                    }
                    QuantifiedKind::TypeVar
                }
                TypeParam::ParamSpec(x) => {
                    if let Some(box default) = &mut x.default {
                        let idx = self.handle_type_param_constraint(default);
                        default_info = Some((idx, default.range()));
                    }
                    QuantifiedKind::ParamSpec
                }
                TypeParam::TypeVarTuple(x) => {
                    if let Some(box default) = &mut x.default {
                        let idx = self.handle_type_param_constraint(default);
                        default_info = Some((idx, default.range()));
                    }
                    QuantifiedKind::TypeVarTuple
                }
            };
            let name = x.name();
            self.scopes
                .current_mut()
                .stat
                .add(name.id.clone(), name.range, None);
            self.bind_definition(
                name,
                Binding::TypeParameter(Box::new(TypeParameter {
                    name: name.id.clone(),
                    unique: self.uniques.fresh(),
                    kind,
                    default: default_info,
                    bound: bound_info,
                    constraints: constraint_info,
                })),
                None,
            );
        }
    }

    pub fn add_loop_exitpoint(&mut self, exit: LoopExit, range: TextRange) {
        let scope = self.scopes.current_mut();
        let flow = scope.flow.clone();
        if let Some(innermost) = scope.loops.last_mut() {
            innermost.0.push((exit, flow));
            scope.flow.no_next = true;
        } else {
            // Python treats break and continue outside of a loop as a syntax error.
            self.error(
                range,
                format!("Cannot `{exit}` outside loop"),
                ErrorKind::ParseError,
            );
        }
    }

    pub fn bind_narrow_ops(&mut self, narrow_ops: &NarrowOps, use_range: TextRange) {
        for (name, (op, op_range)) in narrow_ops.0.iter_hashed() {
            if let Ok(name_key) = self.lookup_name_hashed(name, LookupKind::Regular) {
                let binding_key = self.table.insert(
                    Key::Narrow(name.into_key().clone(), *op_range, use_range),
                    Binding::Narrow(name_key, Box::new(op.clone()), use_range),
                );
                self.scopes.update_flow_info_hashed(name, binding_key, None);
            }
        }
    }

    pub fn bind_lambda_param(&mut self, name: &Identifier) {
        let var = self.solver.fresh_contained(self.uniques);
        let bind_key = self.table.insert(
            Key::Definition(ShortIdentifier::new(name)),
            Binding::LambdaParameter(var),
        );
        self.scopes
            .current_mut()
            .stat
            .add(name.id.clone(), name.range, None);
        self.bind_key(&name.id, bind_key, None);
    }

    pub fn bind_function_param(
        &mut self,
        target: AnnotationTarget,
        x: AnyParameterRef,
        function_idx: Idx<KeyFunction>,
        class_key: Option<Idx<KeyClass>>,
    ) {
        let name = x.name();
        let annot = x.annotation().map(|x| {
            self.table.insert(
                KeyAnnotation::Annotation(ShortIdentifier::new(name)),
                BindingAnnotation::AnnotateExpr(target.clone(), x.clone(), class_key),
            )
        });
        let (annot, def) = match annot {
            Some(annot) => (annot, Either::Left(annot)),
            None => {
                let var = self.solver.fresh_contained(self.uniques);
                let annot = self.table.insert(
                    KeyAnnotation::Annotation(ShortIdentifier::new(name)),
                    BindingAnnotation::Type(target.clone(), var.to_type()),
                );
                (annot, Either::Right((var, function_idx, target)))
            }
        };
        let key = self.table.insert(
            Key::Definition(ShortIdentifier::new(name)),
            Binding::FunctionParameter(def),
        );
        self.scopes
            .current_mut()
            .stat
            .add(name.id.clone(), name.range, Some(annot));
        self.bind_key(&name.id, key, None);
    }

    /// Helper for loops, inserts a phi key for every name in the given flow.
    fn insert_phi_keys(&mut self, mut flow: Flow, range: TextRange) -> Flow {
        for (name, info) in flow.info.iter_mut() {
            info.key = self.table.types.0.insert(Key::Phi(name.clone(), range));
        }
        flow.no_next = false;
        flow
    }

    pub fn setup_loop(&mut self, range: TextRange, narrow_ops: &NarrowOps) {
        let base = mem::take(&mut self.scopes.current_mut().flow);
        // To account for possible assignments to existing names in a loop, we
        // speculatively insert phi keys upfront.
        self.scopes.current_mut().flow = self.insert_phi_keys(base.clone(), range);
        self.bind_narrow_ops(narrow_ops, range);
        self.scopes
            .current_mut()
            .loops
            .push(Loop(vec![(LoopExit::NeverRan, base)]));
    }

    pub fn teardown_loop(&mut self, range: TextRange, narrow_ops: &NarrowOps, orelse: Vec<Stmt>) {
        let done = self.scopes.current_mut().loops.pop().unwrap();
        let (breaks, other_exits): (Vec<Flow>, Vec<Flow>) =
            done.0.into_iter().partition_map(|(exit, flow)| match exit {
                LoopExit::Break => Either::Left(flow),
                LoopExit::NeverRan | LoopExit::Continue => Either::Right(flow),
            });
        // We associate a range to the non-`break` exits from the loop; it doesn't matter much what
        // it is as long as it's different from the loop's range.
        let other_range = TextRange::new(range.start(), range.start());
        if breaks.is_empty() {
            // When there are no `break`s, the loop condition is always false once the body has exited,
            // and any `orelse` always runs.
            self.merge_loop_into_current(other_exits, range);
            self.bind_narrow_ops(&narrow_ops.negate(), other_range);
            self.stmts(orelse);
        } else {
            // Otherwise, we negate the loop condition and run the `orelse` only when we don't `break`.
            self.merge_loop_into_current(other_exits, other_range);
            self.bind_narrow_ops(&narrow_ops.negate(), other_range);
            self.stmts(orelse);
            self.merge_loop_into_current(breaks, range);
        }
    }

    fn merge_flow_style(&mut self, styles: Vec<Option<FlowStyle>>) -> Option<FlowStyle> {
        let mut it = styles.into_iter();
        let mut merged = it.next()?;
        for x in it {
            match (&merged, x) {
                // If they're identical, keep it
                (l, r) if l == &r => {}
                // Uninitialized takes precedence over Unbound
                (Some(FlowStyle::Uninitialized), Some(FlowStyle::Unbound)) => {}
                (Some(FlowStyle::Unbound), Some(FlowStyle::Uninitialized)) => {
                    merged = Some(FlowStyle::Uninitialized);
                }
                // Unbound and bound branches merge into PossiblyUnbound
                // Uninitialized and bound branches merge into PossiblyUninitialized
                (Some(FlowStyle::Unbound), _) => {
                    return Some(FlowStyle::PossiblyUnbound);
                }
                (Some(FlowStyle::Uninitialized), _) => {
                    return Some(FlowStyle::PossiblyUninitialized);
                }
                (_, Some(FlowStyle::PossiblyUnbound | FlowStyle::Unbound)) => {
                    return Some(FlowStyle::PossiblyUnbound);
                }
                (_, Some(FlowStyle::PossiblyUninitialized | FlowStyle::Uninitialized)) => {
                    return Some(FlowStyle::PossiblyUninitialized);
                }
                // Unclear how to merge, default to None
                _ => {
                    merged = None;
                }
            }
        }
        merged
    }

    pub fn merge_flow(&mut self, xs: Vec<Flow>, range: TextRange) -> Flow {
        self.merge_flow_is_loop(xs, range, false)
    }

    pub fn merge_flow_is_loop(
        &mut self,
        mut xs: Vec<Flow>,
        range: TextRange,
        is_loop: bool,
    ) -> Flow {
        if xs.len() == 1 && xs[0].no_next {
            return xs.pop().unwrap();
        }
        let (hidden_branches, mut visible_branches): (Vec<_>, Vec<_>) =
            xs.into_iter().partition(|x| x.no_next);

        // We normally go through the visible branches, but if nothing is visible no one is going to
        // fill in the Phi keys we promised. So just give up and use the hidden branches instead.
        if visible_branches.is_empty() {
            visible_branches = hidden_branches;
        }

        // Collect all the information that we care about from all branches
        let mut names: SmallMap<Name, (Idx<Key>, SmallSet<Idx<Key>>, Vec<Option<FlowStyle>>)> =
            SmallMap::with_capacity(visible_branches.first().map_or(0, |x| x.info.len()));
        let visible_branches_len = visible_branches.len();
        for flow in visible_branches {
            for (name, info) in flow.info.into_iter_hashed() {
                let f = |v: &mut (Idx<Key>, SmallSet<Idx<Key>>, Vec<Option<FlowStyle>>)| {
                    if info.key != v.0 {
                        // Optimization: instead of x = phi(x, ...), we can skip the x.
                        // Avoids a recursive solving step later.
                        v.1.insert(info.key);
                    }
                    v.2.push(info.style);
                };

                match names.entry_hashed(name) {
                    Entry::Occupied(mut e) => f(e.get_mut()),
                    Entry::Vacant(e) => {
                        let key = self.table.types.0.insert(Key::Phi(e.key().clone(), range));
                        f(e.insert((
                            key,
                            SmallSet::new(),
                            Vec::with_capacity(visible_branches_len),
                        )));
                    }
                };
            }
        }

        let mut res = SmallMap::with_capacity(names.len());
        for (name, (key, values, styles)) in names.into_iter_hashed() {
            let style = self.merge_flow_style(styles);
            self.table.insert_idx(
                key,
                if is_loop {
                    Binding::phi_first_default(values)
                } else {
                    Binding::phi(values)
                },
            );
            res.insert_hashed(name, FlowInfo { key, style });
        }
        Flow {
            info: res,
            no_next: false,
        }
    }

    fn merge_loop_into_current(&mut self, mut branches: Vec<Flow>, range: TextRange) {
        branches.push(mem::take(&mut self.scopes.current_mut().flow));
        self.scopes.current_mut().flow = self.merge_flow_is_loop(branches, range, true);
    }
}

impl SpecialEnv for BindingsBuilder<'_> {
    fn current_module(&self) -> ModuleName {
        self.module_info.name()
    }

    fn lookup_special(&self, name: &Name) -> Option<SpecialEntry> {
        self.scopes.get_special_entry(name)
    }
}

/// Handle intercepting names inside either function parameter/return
/// annotations or base class lists of classes, in order to check whether they
/// point at type variable declarations and need to be converted to type
/// parameters.
pub struct LegacyTParamBuilder {
    /// All of the names used. Each one may or may not point at a type variable
    /// and therefore bind a legacy type parameter.
    legacy_tparams: SmallMap<Name, Either<(Identifier, Idx<KeyLegacyTypeParam>), Option<Idx<Key>>>>,
    /// Are there scoped type parameters? Used to control downstream errors.
    has_scoped_tparams: bool,
}

impl LegacyTParamBuilder {
    pub fn new(has_scoped_tparams: bool) -> Self {
        Self {
            legacy_tparams: SmallMap::new(),
            has_scoped_tparams,
        }
    }

    /// Perform a forward lookup of a name used in either base classes of a class
    /// or parameter/return annotations of a function. We do this to create bindings
    /// that allow us to later determine whether this name points at a type variable
    /// declaration, in which case we intercept it to treat it as a type parameter in
    /// the current scope.
    pub fn forward_lookup(
        &mut self,
        builder: &mut BindingsBuilder,
        name: &Identifier,
    ) -> Option<Binding> {
        let result = self
            .legacy_tparams
            .entry(name.id.clone())
            .or_insert_with(|| {
                builder
                    .lookup_legacy_tparam(name)
                    .map_left(|idx| (name.clone(), idx))
                    .map_right(|right| right.ok())
            });
        match result {
            Either::Left((_, idx)) => {
                let range_if_scoped_params_exist = if self.has_scoped_tparams {
                    Some(name.range())
                } else {
                    None
                };
                Some(Binding::CheckLegacyTypeParam(
                    *idx,
                    range_if_scoped_params_exist,
                ))
            }
            Either::Right(idx) => idx.map(Binding::Forward),
        }
    }

    /// Add `Definition` bindings to a class or function body scope for all the names
    /// referenced in the function parameter/return annotations or the class bases.
    ///
    /// We do this so that AnswersSolver has the opportunity to determine whether any
    /// of those names point at legacy (pre-PEP-695) type variable declarations, in which
    /// case the name should be treated as a Quantified type parameter inside this scope.
    pub fn add_name_definitions(&self, builder: &mut BindingsBuilder) {
        for entry in self.legacy_tparams.values() {
            if let Either::Left((name, idx)) = entry {
                builder
                    .scopes
                    .current_mut()
                    .stat
                    .add(name.id.clone(), name.range, None);
                builder.bind_definition(
                    name,
                    // Note: we use None as the range here because the range is
                    // used to error if legacy tparams are mixed with scope
                    // tparams, and we only want to do that once (which we do in
                    // the binding created by `forward_lookup`).
                    Binding::CheckLegacyTypeParam(*idx, None),
                    None,
                );
            }
        }
    }

    /// Get the keys that correspond to the result of checking whether a name
    /// corresponds to a legacy type param. This is used when actually computing
    /// the final type parameters for classes and functions, which have to take
    /// all the names that *do* map to type variable declarations and combine
    /// them (potentially) with scoped type parameters.
    pub fn lookup_keys(&self) -> Vec<Idx<KeyLegacyTypeParam>> {
        self.legacy_tparams
            .values()
            .filter_map(|x| x.as_ref().left().as_ref().map(|(_, idx)| *idx))
            .collect()
    }
}
