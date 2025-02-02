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
use ruff_python_ast::Expr;
use ruff_python_ast::ExprAttribute;
use ruff_python_ast::ExprYield;
use ruff_python_ast::ExprYieldFrom;
use ruff_python_ast::Identifier;
use ruff_python_ast::Stmt;
use ruff_python_ast::StmtReturn;
use ruff_python_ast::TypeParam;
use ruff_python_ast::TypeParams;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;
use vec1::Vec1;

use crate::binding::binding::Binding;
use crate::binding::binding::BindingExpect;
use crate::binding::binding::BindingLegacyTypeParam;
use crate::binding::binding::FunctionBinding;
use crate::binding::binding::Key;
use crate::binding::binding::KeyAnnotation;
use crate::binding::binding::KeyClassField;
use crate::binding::binding::KeyClassMetadata;
use crate::binding::binding::KeyExpect;
use crate::binding::binding::KeyExport;
use crate::binding::binding::KeyLegacyTypeParam;
use crate::binding::binding::Keyed;
use crate::binding::binding::SizeExpectation;
use crate::binding::binding::UnpackedPosition;
use crate::binding::narrow::NarrowOps;
use crate::binding::scope::Flow;
use crate::binding::scope::FlowInfo;
use crate::binding::scope::FlowStyle;
use crate::binding::scope::InstanceAttribute;
use crate::binding::scope::Loop;
use crate::binding::scope::LoopExit;
use crate::binding::scope::ScopeKind;
use crate::binding::scope::Scopes;
use crate::binding::table::TableKeyed;
use crate::config::Config;
use crate::error::collector::ErrorCollector;
use crate::export::exports::Exports;
use crate::export::exports::LookupExport;
use crate::export::special::SpecialExport;
use crate::graph::index::Idx;
use crate::graph::index::Index;
use crate::graph::index_map::IndexMap;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::module::short_identifier::ShortIdentifier;
use crate::table;
use crate::table_for_each;
use crate::table_try_for_each;
use crate::types::quantified::Quantified;
use crate::types::types::AnyStyle;
use crate::util::display::DisplayWith;
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
    pub config: &'a Config,
    errors: &'a ErrorCollector,
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

    pub fn key_to_idx<K: Keyed>(&self, k: &K) -> Idx<K>
    where
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
    {
        self.0.table.get::<K>().0.key_to_idx(k).unwrap_or_else(|| {
            panic!(
                "key_to_idx - key not found, module {}, key {k:?}",
                self.0.module_info.name()
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

    pub fn new(
        x: Vec<Stmt>,
        module_info: ModuleInfo,
        exports: Exports,
        lookup: &dyn LookupExport,
        config: &Config,
        errors: &ErrorCollector,
        uniques: &UniqueFactory,
    ) -> Self {
        let mut builder = BindingsBuilder {
            module_info: module_info.dupe(),
            lookup,
            config,
            errors,
            uniques,
            scopes: Scopes::module(),
            functions: Vec1::new(FuncInfo::default()),
            table: Default::default(),
        };
        builder
            .scopes
            .current_mut()
            .stat
            .stmts(&x, &module_info, true, lookup, config);
        if module_info.name() != ModuleName::builtins() {
            builder.inject_builtins();
        }
        builder.stmts(x);
        let last_scope = builder.scopes.finish();
        for (k, static_info) in last_scope.stat.0 {
            let info = last_scope.flow.info.get(&k);
            let val = match info {
                Some(FlowInfo {
                    key,
                    style: Some(FlowStyle::Annotated { ann, .. }),
                }) => Binding::AnnotatedType(*ann, Box::new(Binding::Forward(*key))),
                Some(FlowInfo { key, .. }) => Binding::Forward(*key),
                None => {
                    // We think we have a binding for this, but we didn't encounter a flow element, so have no idea of what.
                    // This might be because we haven't fully implemented all bindings, or because the two disagree. Just guess.
                    errors.add(
                        &module_info,
                        static_info.loc,
                        format!("Could not find flow binding for `{k}`"),
                    );
                    Binding::AnyType(AnyStyle::Error)
                }
            };
            if exports.contains(&k, lookup) {
                builder.table.insert(KeyExport(k), val);
            }
        }
        Self(Arc::new(BindingsInner {
            module_info,
            table: builder.table,
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
            .insert_if_missing(idx, || Binding::Phi(SmallSet::new()))
        {
            Binding::Phi(phi) => (idx, phi),
            _ => unreachable!(),
        }
    }
}

impl<'a> BindingsBuilder<'a> {
    pub fn stmts(&mut self, xs: Vec<Stmt>) {
        let mut iter = xs.into_iter();
        'outer: while let Some(x) = iter.next() {
            if let Stmt::FunctionDef(x) = x {
                let mut defs = Vec1::new(self.function_def(x));
                for x in iter.by_ref() {
                    if let Stmt::FunctionDef(x) = x {
                        if defs.first().def.name.id == x.name.id {
                            defs.push(self.function_def(x))
                        } else {
                            self.bind_function_defs(defs);
                            defs = Vec1::new(self.function_def(x));
                        }
                    } else {
                        self.bind_function_defs(defs);
                        self.stmt_not_function(x);
                        continue 'outer;
                    }
                }
                self.bind_function_defs(defs);
            } else {
                self.stmt_not_function(x)
            }
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
                    self.bind_key(name, idx, Some(FlowStyle::Import(builtins_module)));
                }
            }
            Err(err) => {
                self.error(TextRange::default(), Arc::unwrap_or_clone(err));
            }
        }
    }

    pub fn todo(&mut self, msg: &str, x: impl Ranged + Debug) {
        self.errors.todo(&self.module_info, msg, x);
    }

    pub fn as_special_export(&self, e: &Expr) -> Option<SpecialExport> {
        self.scopes.as_special_export(e, self.module_info.name())
    }

    pub fn error(&self, range: TextRange, msg: String) {
        self.errors.add(&self.module_info, range, msg);
    }

    fn lookup_name(&mut self, name: &Name) -> Option<Idx<Key>> {
        let mut barrier = false;
        for scope in self.scopes.iter_rev() {
            if !barrier && let Some(flow) = scope.flow.info.get(name) {
                return Some(flow.key);
            } else if !matches!(scope.kind, ScopeKind::ClassBody(_))
                && let Some(info) = scope.stat.0.get(name)
            {
                let key = if info.count == 1 {
                    if info.uses_key_import {
                        Key::Import(name.clone(), info.loc)
                    } else {
                        // We are constructing an identifier, but it must have been one that we saw earlier
                        assert_ne!(info.loc, TextRange::default());
                        Key::Definition(ShortIdentifier::new(&Identifier {
                            id: name.clone(),
                            range: info.loc,
                        }))
                    }
                } else {
                    Key::Anywhere(name.clone(), info.loc)
                };
                return Some(self.table.types.0.insert(key));
            }
            barrier = barrier || scope.barrier;
        }
        None
    }

    pub fn forward_lookup(&mut self, name: &Identifier) -> Option<Binding> {
        self.lookup_name(&name.id).map(Binding::Forward)
    }

    pub fn bind_definition(
        &mut self,
        name: &Identifier,
        binding: Binding,
        style: Option<FlowStyle>,
    ) -> Idx<Key> {
        let idx = self
            .table
            .insert(Key::Definition(ShortIdentifier::new(name)), binding);
        self.bind_key(&name.id, idx, style);
        idx
    }

    fn bind_function_defs(&mut self, defs: Vec1<FunctionBinding>) {
        let name = defs.last().def.name.clone();
        self.bind_definition(&name, Binding::Function(defs), None);
    }

    fn bind_unpacking(
        &mut self,
        elts: &[Expr],
        make_binding: &dyn Fn(Option<Idx<KeyAnnotation>>) -> Binding,
        range: TextRange,
    ) {
        // An unpacking has zero or one splats (starred expressions).
        let mut splat = false;
        for (i, e) in elts.iter().enumerate() {
            match e {
                Expr::Starred(e) => {
                    splat = true;
                    // Counts how many elements are after the splat.
                    let j = elts.len() - i - 1;
                    let make_nested_binding = |_: Option<Idx<KeyAnnotation>>| {
                        Binding::UnpackedValue(
                            Box::new(make_binding(None)),
                            range,
                            UnpackedPosition::Slice(i, j),
                        )
                    };
                    self.bind_target(&e.value, &make_nested_binding, None);
                }
                _ => {
                    let idx = if splat {
                        // If we've encountered a splat, we no longer know how many values have been consumed
                        // from the front, but we know how many are left at the back.
                        UnpackedPosition::ReverseIndex(elts.len() - i)
                    } else {
                        UnpackedPosition::Index(i)
                    };
                    let make_nested_binding = |_: Option<Idx<KeyAnnotation>>| {
                        Binding::UnpackedValue(Box::new(make_binding(None)), range, idx)
                    };
                    self.bind_target(e, &make_nested_binding, None);
                }
            }
        }
        let expect = if splat {
            SizeExpectation::Ge(elts.len() - 1)
        } else {
            SizeExpectation::Eq(elts.len())
        };
        self.table.insert(
            KeyExpect(range),
            BindingExpect::UnpackedLength(Box::new(make_binding(None)), range, expect),
        );
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

    /// Bind the LHS of a target in a syntactic form (e.g. assignments, variables
    /// bound in a `for`` loop header, variables defined by a `with` statement header).
    ///
    /// The `target` is the LHS. It is an `Expr`, but in fact only a handful of forms
    /// are legal because targets can only be names, attributes, subscripts, or unpackings. An
    /// example target illustrating all of the cases is `(x.y, d["k"], [z, *w, q])`
    ///
    /// The `make_binding` function is a callback to the caller, who is responsible for constructing
    /// a binding that provides the value of the RHS. To handle cases where the type of the LHS
    /// is restricted, it takes an optional `KeyAnnotation` which should be the annotation for the
    /// target when one is available.
    ///
    /// The `value` argument is only provided when handling top-level assignment targets;
    /// it enables contextual typing. At the moment it is only used in the attribute case (because
    /// the other cases instead rely on `make_binding` to handle contextual typing, which works
    /// when the form is not an unpacking but results in false negatives when it is).
    ///
    /// TODO(stroxler): The way this is wired up does not work well in
    /// the general case of an unpacking. The attempt to pass around a `make_binding`
    /// callable for both inference and checking does not compose properly with `bind_unpacking`,
    /// because for an unpack target there is no annotation for the entire RHS.
    /// As a result, for all cases except attributes we wind up ignoring type errors
    /// when the target is an unpacking pattern.
    pub fn bind_target(
        &mut self,
        target: &Expr,
        make_binding: &dyn Fn(Option<Idx<KeyAnnotation>>) -> Binding,
        value: Option<&Expr>,
    ) {
        match target {
            Expr::Name(name) => {
                let key = Key::Definition(ShortIdentifier::expr_name(name));
                let idx = self.table.types.0.insert(key);
                let ann = self.bind_key(&name.id, idx, None);
                self.table.types.1.insert(idx, make_binding(ann));
            }
            Expr::Attribute(x) => {
                // `make_binding` will give us a binding for inferring the value type, which we
                // *might* use to compute the attribute type if there are no explicit annotations.
                // Note that this binding uses non-contextual typing.
                let value_binding = make_binding(None);
                // Create a check binding to verify that the assignment is valid.
                if let Some(value) = value {
                    // If this is not an unpacked assignment, we can use contextual typing on the
                    // expression itself.
                    self.table.insert(
                        KeyExpect(x.range),
                        BindingExpect::CheckAssignExprToAttribute(Box::new((
                            x.clone(),
                            value.clone(),
                        ))),
                    );
                } else {
                    // Handle an unpacked assignment, where we don't have easy access to the expression.
                    // Note that contextual typing will not be used in this case.
                    self.table.insert(
                        KeyExpect(x.range),
                        BindingExpect::CheckAssignTypeToAttribute(Box::new((
                            x.clone(),
                            value_binding.clone(),
                        ))),
                    );
                }
                // If this is a self-assignment, record it because we may use it to infer
                // the existance of an instance-only attribute.
                self.bind_attr_if_self(x, value_binding, None);
            }
            Expr::Subscript(x) => {
                let binding = make_binding(None);
                self.table.insert(
                    Key::Anon(x.range),
                    Binding::SubscriptValue(Box::new(binding), x.clone()),
                );
            }
            Expr::Tuple(tup) => {
                self.bind_unpacking(&tup.elts, make_binding, tup.range);
            }
            Expr::List(lst) => {
                self.bind_unpacking(&lst.elts, make_binding, lst.range);
            }
            _ => self.todo("unrecognized assignment target", target),
        }
    }

    /// Return the annotation that should be used at the moment, if one was provided.
    pub fn bind_key(
        &mut self,
        name: &Name,
        key: Idx<Key>,
        style: Option<FlowStyle>,
    ) -> Option<Idx<KeyAnnotation>> {
        let annotation = self.scopes.update_flow_info(name, key, style);
        let info = self.scopes.current().stat.0.get(name).unwrap_or_else(|| {
            let module = self.module_info.name();
            panic!("Name `{name}` not found in static scope of module `{module}`")
        });
        if info.count > 1 || matches!(self.scopes.current().kind, ScopeKind::ClassBody(_)) {
            self.table
                .insert_anywhere(name.clone(), info.loc)
                .1
                .insert(key);
        }
        annotation
    }

    pub fn type_params(&mut self, x: &mut TypeParams) -> Vec<Quantified> {
        let mut qs = Vec::new();
        for x in x.type_params.iter_mut() {
            let q = match x {
                TypeParam::TypeVar(x) => {
                    if let Some(bound) = &mut x.bound {
                        self.ensure_type(bound, &mut None);
                    }
                    Quantified::type_var(self.uniques)
                }
                TypeParam::ParamSpec(_) => Quantified::param_spec(self.uniques),
                TypeParam::TypeVarTuple(_) => Quantified::type_var_tuple(self.uniques),
            };
            qs.push(q);
            let name = x.name();
            self.scopes
                .current_mut()
                .stat
                .add(name.id.clone(), name.range);
            self.bind_definition(name, Binding::TypeParameter(q), None);
        }
        qs
    }

    pub fn add_loop_exitpoint(&mut self, exit: LoopExit, range: TextRange) {
        let scope = self.scopes.current_mut();
        let flow = scope.flow.clone();
        if let Some(innermost) = scope.loops.last_mut() {
            innermost.0.push((exit, flow));
            scope.flow.no_next = true;
        } else {
            self.error(range, format!("Cannot `{exit}` outside loop"));
        }
    }

    pub fn bind_narrow_ops(&mut self, narrow_ops: &NarrowOps, use_range: TextRange) {
        for (name, (op, op_range)) in narrow_ops.0.iter() {
            if let Some(name_key) = self.lookup_name(name) {
                let binding_key = self.table.insert(
                    Key::Narrow(name.clone(), *op_range, use_range),
                    Binding::Narrow(name_key, op.clone()),
                );
                self.scopes.update_flow_info(name, binding_key, None);
            }
        }
    }

    /// Helper for loops, inserts a phi key for every name in the given flow.
    fn insert_phi_keys(&mut self, x: Flow, range: TextRange) -> Flow {
        let items = x
            .info
            .iter_hashed()
            .map(|x| (x.0.cloned(), x.1.ann()))
            .collect::<SmallSet<_>>();
        let mut res = SmallMap::with_capacity(items.len());
        for (name, ann) in items.into_iter() {
            let key = self
                .table
                .types
                .0
                .insert(Key::Phi(name.key().clone(), range));
            res.insert_hashed(name, FlowInfo::new_with_ann(key, ann));
        }
        Flow {
            info: res,
            no_next: false,
        }
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

    fn merge_flow_style(
        &mut self,
        styles: SmallSet<Option<&FlowStyle>>,
        name: &Name,
        is_loop: bool,
    ) -> Option<FlowStyle> {
        if styles.len() == 1 {
            return styles.first().unwrap().cloned();
        }

        // The only distinct styles we can meaningfully merge are annotations
        let unordered_anns: SmallSet<Option<Idx<KeyAnnotation>>> =
            styles.iter().map(|x| x.as_ref()?.ann()).collect();
        let mut anns = unordered_anns
            .into_iter()
            .flatten()
            .map(|k| (k, self.table.annotations.0.idx_to_key(k).range()))
            .collect::<Vec<_>>();
        anns.sort_by_key(|(_, range)| (range.start(), range.end()));
        // If there are multiple annotations, this picks the first one.
        let mut ann = None;
        for other_ann in anns.into_iter() {
            match &ann {
                None => {
                    ann = Some(other_ann);
                }
                Some(ann) => {
                    // A loop might capture the same annotation multiple times at many exit points.
                    // But we only want to consider it when we join up `if` statements.
                    if !is_loop {
                        self.table.insert(
                            KeyExpect(other_ann.1),
                            BindingExpect::Eq(other_ann.0, ann.0, name.clone()),
                        );
                    }
                }
            }
        }
        ann.map(|x| FlowStyle::Annotated {
            ann: x.0,
            is_initialized: true,
        })
    }

    pub fn merge_flow(&mut self, mut xs: Vec<Flow>, range: TextRange, is_loop: bool) -> Flow {
        if xs.len() == 1 && xs[0].no_next {
            return xs.pop().unwrap();
        }
        let (hidden_branches, mut visible_branches): (Vec<_>, Vec<_>) =
            xs.into_iter().partition(|x| x.no_next);

        // We normally go through the visible branches, but if nothing is visible no one is going to
        // fill in the Phi keys we promised. So just given up and use the hidden branches instead.
        if visible_branches.is_empty() {
            visible_branches = hidden_branches;
        }

        let names = visible_branches
            .iter()
            .flat_map(|x| x.info.iter_hashed().map(|x| x.0.cloned()))
            .collect::<SmallSet<_>>();
        let mut res = SmallMap::with_capacity(names.len());
        for name in names.into_iter() {
            let (values, styles): (SmallSet<Idx<Key>>, SmallSet<Option<&FlowStyle>>) =
                visible_branches
                    .iter()
                    .flat_map(|x| x.info.get(name.key()).map(|x| (x.key, x.style.as_ref())))
                    .unzip();
            let style = self.merge_flow_style(styles, name.key(), is_loop);
            let key = self
                .table
                .insert(Key::Phi(name.key().clone(), range), Binding::phi(values));
            res.insert_hashed(name, FlowInfo { key, style });
        }
        Flow {
            info: res,
            no_next: false,
        }
    }

    fn merge_loop_into_current(&mut self, mut branches: Vec<Flow>, range: TextRange) {
        branches.push(mem::take(&mut self.scopes.current_mut().flow));
        self.scopes.current_mut().flow = self.merge_flow(branches, range, true);
    }
}

/// Handle intercepting names inside either function parameter/return
/// annotations or base class lists of classes, in order to check whether they
/// point at type variable declarations and need to be converted to type
/// parameters.
pub struct LegacyTParamBuilder {
    /// All of the names used. Each one may or may not point at a type variable
    /// and therefore bind a legacy type parameter.
    legacy_tparams: SmallMap<Name, Option<(Identifier, Idx<Key>)>>,
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
        self.legacy_tparams
            .entry(name.id.clone())
            .or_insert_with(|| builder.lookup_name(&name.id).map(|x| (name.clone(), x)))
            .as_ref()
            .map(|(id, _)| {
                let range_if_scoped_params_exist = if self.has_scoped_tparams {
                    Some(name.range())
                } else {
                    None
                };
                Binding::CheckLegacyTypeParam(
                    builder
                        .table
                        .legacy_tparams
                        .0
                        .insert(KeyLegacyTypeParam(ShortIdentifier::new(id))),
                    range_if_scoped_params_exist,
                )
            })
    }

    /// Add `Definition` bindings to a class or function body scope for all the names
    /// referenced in the function parameter/return annotations or the class bases.
    ///
    /// We do this so that AnswersSolver has the opportunity to determine whether any
    /// of those names point at legacy (pre-PEP-695) type variable declarations, in which
    /// case the name should be treated as a Quantified type parameter inside this scope.
    pub fn add_name_definitions(&self, builder: &mut BindingsBuilder) {
        for entry in self.legacy_tparams.values() {
            if let Some((identifier, key)) = entry {
                builder.table.insert(
                    KeyLegacyTypeParam(ShortIdentifier::new(identifier)),
                    BindingLegacyTypeParam(*key),
                );
                builder
                    .scopes
                    .current_mut()
                    .stat
                    .add(identifier.id.clone(), identifier.range);
                let key = builder
                    .table
                    .legacy_tparams
                    .0
                    .insert(KeyLegacyTypeParam(ShortIdentifier::new(identifier)));
                builder.bind_definition(
                    identifier,
                    // Note: we use None as the range here because the range is
                    // used to error if legacy tparams are mixed with scope
                    // tparams, and we only want to do that once (which we do in
                    // the binding created by `forward_lookup`).
                    Binding::CheckLegacyTypeParam(key, None),
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
    pub fn lookup_keys(&self, builder: &mut BindingsBuilder) -> Vec<Idx<KeyLegacyTypeParam>> {
        self.legacy_tparams
            .values()
            .flatten()
            .map(|(id, _)| {
                builder
                    .table
                    .legacy_tparams
                    .0
                    .insert(KeyLegacyTypeParam(ShortIdentifier::new(id)))
            })
            .collect()
    }
}
