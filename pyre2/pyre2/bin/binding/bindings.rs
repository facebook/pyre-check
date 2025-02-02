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
use std::sync::LazyLock;

use dupe::Dupe;
use itertools::Either;
use itertools::Itertools;
use regex::Regex;
use ruff_python_ast::name::Name;
use ruff_python_ast::BoolOp;
use ruff_python_ast::Comprehension;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprAttribute;
use ruff_python_ast::ExprBoolOp;
use ruff_python_ast::ExprCall;
use ruff_python_ast::ExprLambda;
use ruff_python_ast::ExprName;
use ruff_python_ast::ExprSubscript;
use ruff_python_ast::ExprYield;
use ruff_python_ast::ExprYieldFrom;
use ruff_python_ast::Identifier;
use ruff_python_ast::Pattern;
use ruff_python_ast::PatternKeyword;
use ruff_python_ast::Stmt;
use ruff_python_ast::StmtClassDef;
use ruff_python_ast::StmtImportFrom;
use ruff_python_ast::StmtReturn;
use ruff_python_ast::TypeParam;
use ruff_python_ast::TypeParams;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;
use starlark_map::smallmap;
use vec1::Vec1;

use crate::ast::Ast;
use crate::binding::binding::Binding;
use crate::binding::binding::BindingAnnotation;
use crate::binding::binding::BindingClassField;
use crate::binding::binding::BindingClassMetadata;
use crate::binding::binding::BindingExpect;
use crate::binding::binding::BindingLegacyTypeParam;
use crate::binding::binding::ClassFieldInitialization;
use crate::binding::binding::ContextManagerKind;
use crate::binding::binding::FunctionBinding;
use crate::binding::binding::Key;
use crate::binding::binding::KeyAnnotation;
use crate::binding::binding::KeyClassField;
use crate::binding::binding::KeyClassMetadata;
use crate::binding::binding::KeyExpect;
use crate::binding::binding::KeyExport;
use crate::binding::binding::KeyLegacyTypeParam;
use crate::binding::binding::Keyed;
use crate::binding::binding::RaisedException;
use crate::binding::binding::SizeExpectation;
use crate::binding::binding::UnpackedPosition;
use crate::binding::narrow::NarrowOp;
use crate::binding::narrow::NarrowOps;
use crate::binding::narrow::NarrowVal;
use crate::binding::scope::Flow;
use crate::binding::scope::FlowInfo;
use crate::binding::scope::FlowStyle;
use crate::binding::scope::InstanceAttribute;
use crate::binding::scope::Loop;
use crate::binding::scope::LoopExit;
use crate::binding::scope::Scope;
use crate::binding::scope::ScopeKind;
use crate::binding::scope::Scopes;
use crate::binding::table::TableKeyed;
use crate::config::Config;
use crate::dunder;
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
use crate::types::special_form::SpecialForm;
use crate::types::types::AnyStyle;
use crate::util::display::DisplayWith;
use crate::util::prelude::SliceExt;
use crate::util::uniques::UniqueFactory;
use crate::visitors::Visitors;

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

    fn todo(&mut self, msg: &str, x: impl Ranged + Debug) {
        self.errors.todo(&self.module_info, msg, x);
    }

    fn as_special_export(&self, e: &Expr) -> Option<SpecialExport> {
        self.scopes.as_special_export(e, self.module_info.name())
    }

    fn error(&self, range: TextRange, msg: String) {
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

    fn forward_lookup(&mut self, name: &Identifier) -> Option<Binding> {
        self.lookup_name(&name.id).map(Binding::Forward)
    }

    /// Given a name appearing in an expression, create a `Usage` key for that
    /// name at the current location. The binding will indicate how to compute
    /// the type if we found that name in scope; if we do not find the name we
    /// record an error and fall back to `Any`.
    ///
    /// This function is the the core scope lookup logic for binding creation.
    fn ensure_name(&mut self, name: &Identifier, value: Option<Binding>) {
        let key = Key::Usage(ShortIdentifier::new(name));
        match value {
            Some(value) => {
                self.table.insert(key, value);
            }
            None if name.id == dunder::FILE || name.id == dunder::NAME => {
                self.table.insert(key, Binding::StrType);
            }
            None => {
                // Name wasn't found. Record a type error and fall back to `Any`.
                self.error(name.range, format!("Could not find name `{name}`"));
                self.table.insert(key, Binding::AnyType(AnyStyle::Error));
            }
        }
    }

    fn bind_comprehensions(&mut self, comprehensions: &[Comprehension]) {
        self.scopes.push(Scope::comprehension());
        for comp in comprehensions.iter() {
            self.scopes.current_mut().stat.expr_lvalue(&comp.target);
            let make_binding = |k| Binding::IterableValue(k, comp.iter.clone());
            self.bind_target(&comp.target, &make_binding, None);
        }
    }

    fn bind_lambda(&mut self, lambda: &ExprLambda) {
        self.scopes.push(Scope::function());
        if let Some(parameters) = &lambda.parameters {
            for x in parameters.iter() {
                let name = x.name();
                let bind_key = self.table.insert(
                    Key::Definition(ShortIdentifier::new(name)),
                    Binding::AnyType(AnyStyle::Implicit),
                );
                self.scopes
                    .current_mut()
                    .stat
                    .add(name.id.clone(), name.range);
                self.bind_key(&name.id, bind_key, None);
            }
        }
    }

    /// Helper to clean up an expression that does type narrowing. We merge flows for the narrowing
    /// operation and its negation, so that narrowing is limited to the body of the expression but
    /// newly defined names persist.
    fn negate_and_merge_flow(
        &mut self,
        base: Flow,
        ops: &NarrowOps,
        orelse: Option<&Expr>,
        range: TextRange,
    ) {
        let if_branch = mem::take(&mut self.scopes.current_mut().flow);
        self.scopes.current_mut().flow = base;
        self.bind_narrow_ops(&ops.negate(), range);
        self.ensure_expr_opt(orelse);
        let else_branch = mem::take(&mut self.scopes.current_mut().flow);
        self.scopes.current_mut().flow =
            self.merge_flow(vec![if_branch, else_branch], range, false);
    }

    /// Execute through the expr, ensuring every name has a binding.
    pub fn ensure_expr(&mut self, x: &Expr) {
        let new_scope = match x {
            Expr::If(x) => {
                // Ternary operation. We treat it like an if/else statement.
                let base = self.scopes.current().flow.clone();
                self.ensure_expr(&x.test);
                let narrow_ops = NarrowOps::from_expr(Some(&x.test));
                self.bind_narrow_ops(&narrow_ops, x.body.range());
                self.ensure_expr(&x.body);
                self.negate_and_merge_flow(base, &narrow_ops, Some(&x.orelse), x.range());
                return;
            }
            Expr::BoolOp(ExprBoolOp { range, op, values }) => {
                let base = self.scopes.current().flow.clone();
                let mut narrow_ops = NarrowOps::new();
                for value in values {
                    self.bind_narrow_ops(&narrow_ops, value.range());
                    self.ensure_expr(value);
                    let new_narrow_ops = NarrowOps::from_expr(Some(value));
                    match op {
                        BoolOp::And => {
                            // Every subsequent value is evaluated only if all previous values were truthy.
                            narrow_ops.and_all(new_narrow_ops);
                        }
                        BoolOp::Or => {
                            // Every subsequent value is evaluated only if all previous values were falsy.
                            narrow_ops.and_all(new_narrow_ops.negate());
                        }
                    }
                }
                self.negate_and_merge_flow(base, &narrow_ops, None, *range);
                return;
            }
            Expr::Name(x) => {
                let name = Ast::expr_name_identifier(x.clone());
                let binding = self.forward_lookup(&name);
                self.ensure_name(&name, binding);
                false
            }
            Expr::Named(x) => {
                self.scopes.current_mut().stat.expr_lvalue(&x.target);
                let make_binding = |k| Binding::Expr(k, (*x.value).clone());
                self.bind_target(&x.target, &make_binding, None);
                false
            }
            Expr::ListComp(x) => {
                self.bind_comprehensions(&x.generators);
                true
            }
            Expr::SetComp(x) => {
                self.bind_comprehensions(&x.generators);
                true
            }
            Expr::DictComp(x) => {
                self.bind_comprehensions(&x.generators);
                true
            }
            Expr::Generator(x) => {
                self.bind_comprehensions(&x.generators);
                true
            }
            Expr::Lambda(x) => {
                self.bind_lambda(x);
                true
            }
            Expr::Yield(x) => {
                self.functions
                    .last_mut()
                    .yields
                    .push(Either::Left(x.clone()));
                false
            }
            Expr::YieldFrom(x) => {
                self.functions
                    .last_mut()
                    .yields
                    .push(Either::Right(x.clone()));
                false
            }
            _ => false,
        };
        Visitors::visit_expr(x, |x| self.ensure_expr(x));
        if new_scope {
            self.scopes.pop();
        }
    }

    /// Execute through the expr, ensuring every name has a binding.
    pub fn ensure_expr_opt(&mut self, x: Option<&Expr>) {
        if let Some(x) = x {
            self.ensure_expr(x);
        }
    }

    /// Execute through the expr, ensuring every name has a binding.
    fn ensure_type(&mut self, x: &mut Expr, tparams_builder: &mut Option<LegacyTParamBuilder>) {
        match x {
            Expr::Name(x) => {
                let name = Ast::expr_name_identifier(x.clone());
                let binding = match tparams_builder {
                    Some(legacy) => legacy.forward_lookup(self, &name),
                    None => self.forward_lookup(&name),
                };
                self.ensure_name(&name, binding);
            }
            Expr::Subscript(ExprSubscript { value, .. })
                if self.as_special_export(value) == Some(SpecialExport::Literal) =>
            {
                // Don't go inside a literal, since you might find strings which are really strings, not string-types
                self.ensure_expr(x);
            }
            Expr::Subscript(ExprSubscript {
                value,
                slice: box Expr::Tuple(tup),
                ..
            }) if self.as_special_export(value) == Some(SpecialExport::Annotated)
                && !tup.is_empty() =>
            {
                // Only go inside the first argument to Annotated, the rest are non-type metadata.
                self.ensure_type(&mut *value, tparams_builder);
                self.ensure_type(&mut tup.elts[0], tparams_builder);
                for e in tup.elts[1..].iter_mut() {
                    self.ensure_expr(e);
                }
            }
            Expr::StringLiteral(literal) => {
                match Ast::parse_type_literal(literal) {
                    Ok(expr) => {
                        *x = expr;
                        // You are not allowed to nest type strings in type strings,
                        self.ensure_expr(x);
                    }
                    Err(e) => {
                        self.error(
                            literal.range,
                            format!(
                                "Could not parse type string: {}, got {e}",
                                literal.value.to_str()
                            ),
                        );
                    }
                }
            }
            _ => Visitors::visit_expr_mut(x, |x| self.ensure_type(x, tparams_builder)),
        }
    }

    /// Execute through the expr, ensuring every name has a binding.
    pub fn ensure_type_opt(
        &mut self,
        x: Option<&mut Expr>,
        tparams_builder: &mut Option<LegacyTParamBuilder>,
    ) {
        if let Some(x) = x {
            self.ensure_type(x, tparams_builder);
        }
    }

    fn bind_definition(
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
    fn bind_attr_if_self(
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
    fn bind_target(
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

    fn class_def(&mut self, mut x: StmtClassDef) {
        let body = mem::take(&mut x.body);
        let decorators = mem::take(&mut x.decorator_list);

        for x in decorators.iter() {
            self.ensure_expr(&x.expression);
        }

        self.scopes.push(Scope::class_body(x.name.clone()));

        let definition_key = self
            .table
            .types
            .0
            .insert(Key::Definition(ShortIdentifier::new(&x.name)));

        x.type_params.iter_mut().for_each(|x| {
            self.type_params(x);
        });

        let mut legacy = Some(LegacyTParamBuilder::new(x.type_params.is_some()));
        let bases = x.bases().map(|base| {
            let mut base = base.clone();
            // Forward refs are fine *inside* of a base expression in the type arguments,
            // but outermost class cannot be a forward ref.
            match &base {
                Expr::StringLiteral(v) => {
                    self.error(
                        base.range(),
                        format!(
                            "Cannot use string annotation `{}` as a base class",
                            v.value.to_str()
                        ),
                    );
                }
                _ => {}
            }
            self.ensure_type(&mut base, &mut legacy);
            base
        });

        let mut keywords = Vec::new();
        x.keywords().iter().for_each(|keyword| {
            if let Some(name) = &keyword.arg {
                self.ensure_expr(&keyword.value);
                keywords.push((name.id.clone(), keyword.value.clone()));
            } else {
                self.error(
                    keyword.range(),
                    format!(
                        "The use of unpacking in class header of `{}` is not supported",
                        x.name
                    ),
                )
            }
        });

        self.table.insert(
            KeyClassMetadata(ShortIdentifier::new(&x.name)),
            BindingClassMetadata(definition_key, bases.clone(), keywords, decorators.clone()),
        );

        let legacy_tparam_builder = legacy.unwrap();
        legacy_tparam_builder.add_name_definitions(self);

        let non_field_names = self
            .scopes
            .current()
            .flow
            .info
            .keys()
            .cloned()
            .collect::<SmallSet<_>>();

        self.scopes.current_mut().stat.stmts(
            &body,
            &self.module_info,
            false,
            self.lookup,
            self.config,
        );
        self.stmts(body);

        let last_scope = self.scopes.pop();
        let mut fields = SmallSet::new();
        for (name, info) in last_scope.flow.info.iter() {
            if non_field_names.contains(name) {
                // TODO: this incorrectly filters out fields that reuse already-in-scope names.
                continue;
            }
            // A name with flow info but not static info is a reference to something that's not a class field.
            if let Some(stat_info) = last_scope.stat.0.get(name) {
                let flow_type = Binding::Forward(
                    self.table
                        .types
                        .0
                        .insert(Key::Anywhere(name.clone(), stat_info.loc)),
                );
                let initialization = if info.is_initialized() {
                    ClassFieldInitialization::Class
                } else {
                    ClassFieldInitialization::Instance
                };
                let binding = BindingClassField {
                    class: definition_key,
                    name: name.clone(),
                    value: flow_type,
                    annotation: info.ann(),
                    range: stat_info.loc,
                    initialization,
                };
                fields.insert(name.clone());
                self.table.insert(
                    KeyClassField(ShortIdentifier::new(&x.name), name.clone()),
                    binding,
                );
            }
        }
        if let ScopeKind::ClassBody(body) = last_scope.kind {
            for (method_name, instance_attributes) in body.instance_attributes_by_method {
                if method_name == dunder::INIT {
                    for (name, InstanceAttribute(value, annotation, range)) in instance_attributes {
                        if !fields.contains(&name) {
                            fields.insert(name.clone());
                            self.table.insert(
                                KeyClassField(ShortIdentifier::new(&x.name), name.clone()),
                                BindingClassField {
                                    class: definition_key,
                                    name,
                                    value,
                                    annotation,
                                    range,
                                    initialization: ClassFieldInitialization::Instance,
                                },
                            );
                        } else if annotation.is_some() {
                            self.error(range, format!("Attribute `{name}` is declared in the class body, so the assignment here should not have an annotation."));
                        }
                    }
                }
            }
        } else {
            unreachable!("Expected class body scope, got {:?}", last_scope.kind);
        }

        let self_binding = Binding::SelfType(definition_key);
        self.table
            .insert(Key::SelfType(ShortIdentifier::new(&x.name)), self_binding);

        let legacy_tparams = legacy_tparam_builder.lookup_keys(self);

        let name = x.name.clone();
        self.bind_definition(
            &name,
            Binding::ClassDef(
                Box::new((x, fields)),
                bases.into_boxed_slice(),
                decorators.into_boxed_slice(),
                legacy_tparams.into_boxed_slice(),
            ),
            None,
        );
    }

    fn synthesize_enum_def(
        &mut self,
        class_name: Identifier,
        base_name: ExprName,
        members: &[Expr],
    ) {
        let definition_key = self
            .table
            .types
            .0
            .insert(Key::Definition(ShortIdentifier::new(&class_name)));
        self.table.insert(
            KeyClassMetadata(ShortIdentifier::new(&class_name)),
            BindingClassMetadata(definition_key, vec![Expr::Name(base_name)], vec![], vec![]),
        );
        let mut fields = SmallSet::new();
        match members {
            // Enum('Color5', 'RED, GREEN, BLUE')
            // Enum('Color6', 'RED GREEN BLUE')
            [Expr::StringLiteral(x)] => {
                let s = x.value.to_str();
                let parts: Vec<&str> = if s.contains(',') {
                    s.split(',').map(str::trim).collect()
                } else {
                    s.split_whitespace().collect()
                };
                for member in parts {
                    if is_valid_identifier(member) {
                        let member_name = Name::new(member);
                        fields.insert(member_name.clone());
                        self.table.insert(
                            KeyClassField(ShortIdentifier::new(&class_name), member_name.clone()),
                            BindingClassField {
                                class: definition_key,
                                name: member_name,
                                value: Binding::AnyType(AnyStyle::Implicit),
                                annotation: None,
                                range: x.range(),
                                initialization: ClassFieldInitialization::Class,
                            },
                        );
                    } else {
                        self.error(x.range, format!("{member} is not a valid identifier"))
                    }
                }
            }
            _ => self.error(class_name.range, "TODO enum functional syntax".to_owned()),
        }
        let self_binding = Binding::SelfType(definition_key);
        self.table.insert(
            Key::SelfType(ShortIdentifier::new(&class_name)),
            self_binding,
        );
        self.bind_definition(
            &class_name,
            Binding::FunctionalClassDef(class_name.clone(), fields),
            None,
        );
    }

    fn add_loop_exitpoint(&mut self, exit: LoopExit, range: TextRange) {
        let scope = self.scopes.current_mut();
        let flow = scope.flow.clone();
        if let Some(innermost) = scope.loops.last_mut() {
            innermost.0.push((exit, flow));
            scope.flow.no_next = true;
        } else {
            self.error(range, format!("Cannot `{exit}` outside loop"));
        }
    }

    // Traverse a pattern and bind all the names; key is the reference for the value that's being matched on
    fn bind_pattern(
        &mut self,
        subject_name: Option<&Name>,
        pattern: Pattern,
        key: Idx<Key>,
    ) -> NarrowOps {
        match pattern {
            Pattern::MatchValue(p) => {
                self.ensure_expr(&p.value);
                if let Some(subject_name) = subject_name {
                    NarrowOps(
                        smallmap! { subject_name.clone() => (NarrowOp::Eq(NarrowVal::Expr(p.value.clone())), p.range()) },
                    )
                } else {
                    NarrowOps::new()
                }
            }
            Pattern::MatchSingleton(p) => {
                let value = Ast::pattern_match_singleton_to_expr(&p);
                if let Some(subject_name) = subject_name {
                    NarrowOps(
                        smallmap! { subject_name.clone() => (NarrowOp::Is(NarrowVal::Expr(Box::new(value))), p.range()) },
                    )
                } else {
                    NarrowOps::new()
                }
            }
            Pattern::MatchAs(p) => {
                // If there's no name for this pattern, refine the variable being matched
                // If there is a new name, refine that instead
                let new_subject_name = if let Some(name) = &p.name {
                    self.bind_definition(name, Binding::Forward(key), None);
                    Some(&name.id)
                } else {
                    subject_name
                };
                if let Some(box pattern) = p.pattern {
                    self.bind_pattern(new_subject_name, pattern, key)
                } else {
                    NarrowOps::new()
                }
            }
            Pattern::MatchSequence(x) => {
                let mut narrow_ops = NarrowOps::new();
                let num_patterns = x.patterns.len();
                let mut unbounded = false;
                for (idx, x) in x.patterns.into_iter().enumerate() {
                    match x {
                        Pattern::MatchStar(p) => {
                            if let Some(name) = &p.name {
                                let position = UnpackedPosition::Slice(idx, num_patterns - idx - 1);
                                self.bind_definition(
                                    name,
                                    Binding::UnpackedValue(
                                        Box::new(Binding::Forward(key)),
                                        p.range,
                                        position,
                                    ),
                                    None,
                                );
                            }
                            unbounded = true;
                        }
                        _ => {
                            let position = if unbounded {
                                UnpackedPosition::ReverseIndex(num_patterns - idx)
                            } else {
                                UnpackedPosition::Index(idx)
                            };
                            let key = self.table.insert(
                                Key::Anon(x.range()),
                                Binding::UnpackedValue(
                                    Box::new(Binding::Forward(key)),
                                    x.range(),
                                    position,
                                ),
                            );
                            narrow_ops.and_all(self.bind_pattern(None, x, key));
                        }
                    }
                }
                let expect = if unbounded {
                    SizeExpectation::Ge(num_patterns - 1)
                } else {
                    SizeExpectation::Eq(num_patterns)
                };
                self.table.insert(
                    KeyExpect(x.range),
                    BindingExpect::UnpackedLength(Box::new(Binding::Forward(key)), x.range, expect),
                );
                narrow_ops
            }
            Pattern::MatchMapping(x) => {
                let mut narrow_ops = NarrowOps::new();
                x.keys
                    .into_iter()
                    .zip(x.patterns)
                    .for_each(|(key_expr, pattern)| {
                        let mapping_key = self.table.insert(
                            Key::Anon(key_expr.range()),
                            Binding::PatternMatchMapping(key_expr, key),
                        );
                        narrow_ops.and_all(self.bind_pattern(None, pattern, mapping_key))
                    });
                if let Some(rest) = x.rest {
                    self.bind_definition(&rest, Binding::Forward(key), None);
                }
                narrow_ops
            }
            Pattern::MatchClass(x) => {
                self.ensure_expr(&x.cls);
                let mut narrow_ops = if let Some(subject_name) = subject_name {
                    NarrowOps(
                        smallmap! { subject_name.clone() => (NarrowOp::IsInstance(NarrowVal::Expr(x.cls.clone())), x.cls.range()) },
                    )
                } else {
                    NarrowOps::new()
                };
                x.arguments
                    .patterns
                    .into_iter()
                    .enumerate()
                    .for_each(|(idx, pattern)| {
                        let attr_key = self.table.insert(
                            Key::Anon(pattern.range()),
                            Binding::PatternMatchClassPositional(
                                x.cls.clone(),
                                idx,
                                key,
                                pattern.range(),
                            ),
                        );
                        narrow_ops.and_all(self.bind_pattern(None, pattern.clone(), attr_key))
                    });
                x.arguments.keywords.into_iter().for_each(
                    |PatternKeyword {
                         range: _,
                         attr,
                         pattern,
                     }| {
                        let attr_key = self.table.insert(
                            Key::Anon(attr.range()),
                            Binding::PatternMatchClassKeyword(x.cls.clone(), attr, key),
                        );
                        narrow_ops.and_all(self.bind_pattern(None, pattern, attr_key))
                    },
                );
                narrow_ops
            }
            Pattern::MatchOr(x) => {
                let mut narrow_ops: Option<NarrowOps> = None;
                let range = x.range;
                let mut branches = Vec::new();
                let n_subpatterns = x.patterns.len();
                for (idx, pattern) in x.patterns.into_iter().enumerate() {
                    if pattern.is_irrefutable() && idx != n_subpatterns - 1 {
                        self.error(
                            pattern.range(),
                            "Only the last subpattern in MatchOr may be irrefutable".to_owned(),
                        )
                    }
                    let mut base = self.scopes.current().flow.clone();
                    let new_narrow_ops = self.bind_pattern(subject_name, pattern, key);
                    if let Some(ref mut ops) = narrow_ops {
                        ops.or_all(new_narrow_ops)
                    } else {
                        narrow_ops = Some(new_narrow_ops);
                    }
                    mem::swap(&mut self.scopes.current_mut().flow, &mut base);
                    branches.push(base);
                }
                self.scopes.current_mut().flow = self.merge_flow(branches, range, false);
                narrow_ops.unwrap_or_default()
            }
            Pattern::MatchStar(_) => NarrowOps::new(),
        }
    }

    fn bind_unimportable_names(&mut self, x: &StmtImportFrom) {
        for x in &x.names {
            if &x.name != "*" {
                let asname = x.asname.as_ref().unwrap_or(&x.name);
                // We pass None as imported_from, since we are really faking up a local error definition
                self.bind_definition(asname, Binding::AnyType(AnyStyle::Error), None);
            }
        }
    }

    fn bind_narrow_ops(&mut self, narrow_ops: &NarrowOps, use_range: TextRange) {
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

    /// Evaluate the statements and update the bindings.
    /// Every statement should end up in the bindings, perhaps with a location that is never used.
    /// Functions are coalesced into potential overloads in `fn stmts` and should not be passed in.
    fn stmt_not_function(&mut self, x: Stmt) {
        match x {
            Stmt::FunctionDef(_) => {
                // We handle 1+ functions at a time in function_defs for overloads. See `fn stmts`
                unreachable!("unexpected function definition")
            }
            Stmt::ClassDef(x) => self.class_def(x),
            Stmt::Return(x) => {
                self.ensure_expr_opt(x.value.as_deref());
                self.functions.last_mut().returns.push(x);
                self.scopes.current_mut().flow.no_next = true;
            }
            Stmt::Delete(x) => self.todo("Bindings::stmt", &x),
            Stmt::Assign(x) => {
                let name = if x.targets.len() == 1
                    && let Expr::Name(name) = &x.targets[0]
                {
                    Some(name.id.clone())
                } else {
                    None
                };
                let mut value = *x.value;
                let mut is_synthesized_class = false;
                match &mut value {
                    // Handle forward references in a TypeVar call.
                    Expr::Call(ExprCall {
                        range: _,
                        func,
                        arguments,
                    }) if self.as_special_export(func) == Some(SpecialExport::TypeVar)
                        && !arguments.is_empty() =>
                    {
                        self.ensure_expr(func);
                        // The constraints (i.e., any positional arguments after the first)
                        // and some keyword arguments are types.
                        for arg in arguments.args.iter_mut().skip(1) {
                            self.ensure_type(arg, &mut None);
                        }
                        for kw in arguments.keywords.iter_mut() {
                            if let Some(id) = &kw.arg
                                && (id.id == "bound" || id.id == "default")
                            {
                                self.ensure_type(&mut kw.value, &mut None);
                            } else {
                                self.ensure_expr(&kw.value);
                            }
                        }
                    }
                    Expr::Call(ExprCall {
                        range: _,
                        func: box ref func @ Expr::Name(ref base_name),
                        arguments,
                    }) if matches!(
                        self.as_special_export(func),
                        Some(SpecialExport::Enum | SpecialExport::IntEnum | SpecialExport::StrEnum)
                    ) && arguments.keywords.is_empty()
                        && let Some(name) = &name =>
                    {
                        self.ensure_expr(func);
                        for arg in arguments.args.iter_mut() {
                            self.ensure_expr(arg);
                        }
                        // Use the variable name, not the string literal argument
                        self.synthesize_enum_def(
                            Identifier::new(name.clone(), x.targets[0].range()),
                            base_name.clone(),
                            &arguments.args[1..],
                        );
                        is_synthesized_class = true;
                    }
                    _ => self.ensure_expr(&value),
                }
                if !is_synthesized_class {
                    for target in x.targets {
                        let make_binding = |k: Option<Idx<KeyAnnotation>>| {
                            if let Some(name) = &name {
                                Binding::NameAssign(name.clone(), k, Box::new(value.clone()))
                            } else {
                                Binding::Expr(k, value.clone())
                            }
                        };
                        self.bind_target(&target, &make_binding, Some(&value));
                        self.ensure_expr(&target);
                    }
                }
            }
            Stmt::AugAssign(x) => {
                self.ensure_expr(&x.target);
                self.ensure_expr(&x.value);
                let make_binding = |_: Option<Idx<KeyAnnotation>>| Binding::AugAssign(x.clone());
                self.bind_target(&x.target, &make_binding, None);
            }
            Stmt::AnnAssign(mut x) => match *x.target {
                Expr::Name(name) => {
                    let name = Ast::expr_name_identifier(name);
                    let ann_key = KeyAnnotation::Annotation(ShortIdentifier::new(&name));
                    self.ensure_type(&mut x.annotation, &mut None);
                    let ann_val = if let Some(special) = SpecialForm::new(&name.id, &x.annotation) {
                        BindingAnnotation::Type(special.to_type())
                    } else {
                        BindingAnnotation::AnnotateExpr(*x.annotation.clone(), None)
                    };
                    let ann_key = self.table.insert(ann_key, ann_val);

                    let (value, is_initialized) = if let Some(value) = x.value {
                        // Treat a name as initialized, but skip actually checking the value, if we are assigning `...` in a stub.
                        if self.module_info.path().is_interface()
                            && matches!(&*value, Expr::EllipsisLiteral(_))
                        {
                            (None, true)
                        } else {
                            (Some(value), true)
                        }
                    } else {
                        (None, false)
                    };

                    let binding = if let Some(mut value) = value {
                        // Handle forward references in explicit type aliases.
                        if self.as_special_export(&x.annotation) == Some(SpecialExport::TypeAlias) {
                            self.ensure_type(&mut value, &mut None);
                        } else {
                            self.ensure_expr(&value);
                        }
                        Binding::NameAssign(name.id.clone(), Some(ann_key), value)
                    } else {
                        Binding::AnnotatedType(
                            ann_key,
                            Box::new(Binding::AnyType(AnyStyle::Implicit)),
                        )
                    };
                    self.bind_definition(
                        &name,
                        binding,
                        Some(FlowStyle::Annotated {
                            ann: ann_key,
                            is_initialized,
                        }),
                    );
                }
                Expr::Attribute(attr) => {
                    self.ensure_expr(&attr.value);
                    self.ensure_type(&mut x.annotation, &mut None);
                    let ann_key = self.table.insert(
                        KeyAnnotation::AttrAnnotation(x.annotation.range()),
                        BindingAnnotation::AnnotateExpr(*x.annotation, None),
                    );
                    let value_binding = match &x.value {
                        Some(v) => Binding::Expr(None, *v.clone()),
                        None => Binding::AnyType(AnyStyle::Implicit),
                    };
                    if !self.bind_attr_if_self(&attr, value_binding, Some(ann_key)) {
                        self.errors.add(
                            &self.module_info,
                            x.range,
                            format!(
                                "Type cannot be declared in assignment to non-self attribute `{}.{}`",
                                attr.value.display_with(&self.module_info),
                                attr.attr.id,
                            ),
                        );
                    }
                    if let Some(box v) = x.value {
                        self.ensure_expr(&v);
                        self.table.insert(
                            KeyExpect(v.range()),
                            BindingExpect::CheckAssignExprToAttribute(Box::new((attr, v))),
                        );
                    }
                }
                _ => self.todo("Bindings::stmt AnnAssign", &x),
            },
            Stmt::TypeAlias(mut x) => {
                if let Expr::Name(name) = *x.name {
                    if let Some(params) = &mut x.type_params {
                        self.type_params(params);
                    }
                    self.ensure_type(&mut x.value, &mut None);
                    let binding = Binding::ScopedTypeAlias(name.id.clone(), x.type_params, x.value);
                    self.bind_definition(&Ast::expr_name_identifier(name), binding, None);
                } else {
                    self.todo("Bindings::stmt TypeAlias", &x);
                }
            }
            Stmt::For(x) => {
                self.setup_loop(x.range, &NarrowOps::new());
                self.ensure_expr(&x.iter);
                let make_binding = |k| Binding::IterableValue(k, *x.iter.clone());
                self.bind_target(&x.target, &make_binding, None);
                self.ensure_expr(&x.target);
                self.stmts(x.body);
                self.teardown_loop(x.range, &NarrowOps::new(), x.orelse);
            }
            Stmt::While(x) => {
                let narrow_ops = NarrowOps::from_expr(Some(&x.test));
                self.setup_loop(x.range, &narrow_ops);
                self.ensure_expr(&x.test);
                self.table
                    .insert(Key::Anon(x.test.range()), Binding::Expr(None, *x.test));
                self.stmts(x.body);
                self.teardown_loop(x.range, &narrow_ops, x.orelse);
            }
            Stmt::If(x) => {
                let range = x.range;
                let mut exhaustive = false;
                let mut branches = Vec::new();
                // Type narrowing operations that are carried over from one branch to the next. For example, in:
                //   if x is None:
                //     pass
                //   else:
                //     pass
                // x is bound to Narrow(x, Is(None)) in the if branch, and the negation, Narrow(x, IsNot(None)),
                // is carried over to the else branch.
                let mut negated_prev_ops = NarrowOps::new();
                for (test, body) in Ast::if_branches_owned(x) {
                    let b = self.config.evaluate_bool_opt(test.as_ref());
                    if b == Some(false) {
                        continue; // We won't pick this branch
                    }
                    let mut base = self.scopes.current().flow.clone();
                    let new_narrow_ops = NarrowOps::from_expr(test.as_ref());
                    if let Some(e) = test {
                        self.ensure_expr(&e);
                        self.table
                            .insert(Key::Anon(e.range()), Binding::Expr(None, e));
                    }
                    if let Some(stmt) = body.first() {
                        let use_range = stmt.range();
                        self.bind_narrow_ops(&negated_prev_ops, use_range);
                        self.bind_narrow_ops(&new_narrow_ops, use_range);
                    }
                    negated_prev_ops.and_all(new_narrow_ops.negate());
                    self.stmts(body);
                    mem::swap(&mut self.scopes.current_mut().flow, &mut base);
                    branches.push(base);
                    if b == Some(true) {
                        exhaustive = true;
                        break; // We picked this branch, none others stand a chance
                    }
                }
                if !exhaustive {
                    branches.push(mem::take(&mut self.scopes.current_mut().flow));
                }
                self.scopes.current_mut().flow = self.merge_flow(branches, range, false);
            }
            Stmt::With(x) => {
                let kind = if x.is_async {
                    ContextManagerKind::Async
                } else {
                    ContextManagerKind::Sync
                };
                for item in x.items {
                    self.ensure_expr(&item.context_expr);
                    if let Some(opts) = item.optional_vars {
                        let make_binding = |k: Option<Idx<KeyAnnotation>>| {
                            Binding::ContextValue(k, item.context_expr.clone(), kind)
                        };
                        self.bind_target(&opts, &make_binding, None);
                        self.ensure_expr(&opts);
                    } else {
                        self.table.insert(
                            Key::Anon(item.range()),
                            Binding::ContextValue(None, item.context_expr, kind),
                        );
                    }
                }
                self.stmts(x.body);
            }
            Stmt::Match(x) => {
                self.ensure_expr(&x.subject);
                let subject_name = if let Expr::Name(name) = &*x.subject {
                    Some(name.id.clone())
                } else {
                    None
                };
                let key = self.table.insert(
                    Key::Anon(x.subject.range()),
                    Binding::Expr(None, *x.subject),
                );
                let mut exhaustive = false;
                let range = x.range;
                let mut branches = Vec::new();
                // Type narrowing operations that are carried over from one case to the next. For example, in:
                //   match x:
                //     case None:
                //       pass
                //     case _:
                //       pass
                // x is bound to Narrow(x, Eq(None)) in the first case, and the negation, Narrow(x, NotEq(None)),
                // is carried over to the fallback case.
                let mut negated_prev_ops = NarrowOps::new();
                for case in x.cases {
                    let mut base = self.scopes.current().flow.clone();
                    if case.pattern.is_wildcard() || case.pattern.is_irrefutable() {
                        exhaustive = true;
                    }
                    let new_narrow_ops =
                        self.bind_pattern(subject_name.as_ref(), case.pattern, key);
                    self.bind_narrow_ops(&negated_prev_ops, case.range);
                    self.bind_narrow_ops(&new_narrow_ops, case.range);
                    negated_prev_ops.and_all(new_narrow_ops.negate());
                    if let Some(guard) = case.guard {
                        self.ensure_expr(&guard);
                        self.table
                            .insert(Key::Anon(guard.range()), Binding::Expr(None, *guard));
                    }
                    self.stmts(case.body);
                    mem::swap(&mut self.scopes.current_mut().flow, &mut base);
                    branches.push(base);
                    if exhaustive {
                        break;
                    }
                }
                if !exhaustive {
                    branches.push(mem::take(&mut self.scopes.current_mut().flow));
                }
                self.scopes.current_mut().flow = self.merge_flow(branches, range, false);
            }
            Stmt::Raise(x) => {
                if let Some(exc) = x.exc {
                    self.ensure_expr(&exc);
                    let raised = if let Some(cause) = x.cause {
                        self.ensure_expr(&cause);
                        RaisedException::WithCause(Box::new((*exc, *cause)))
                    } else {
                        RaisedException::WithoutCause(*exc)
                    };
                    self.table.insert(
                        KeyExpect(x.range),
                        BindingExpect::CheckRaisedException(raised),
                    );
                } else {
                    // If there's no exception raised, don't bother checking the cause.
                }
                self.scopes.current_mut().flow.no_next = true;
            }
            Stmt::Try(x) => {
                let range = x.range;
                let mut branches = Vec::new();
                let mut base = self.scopes.current().flow.clone();

                // We branch before the body, conservatively assuming that any statement can fail
                // entry -> try -> else -> finally
                //   |                     ^
                //   ----> handler --------|

                self.stmts(x.body);
                self.stmts(x.orelse);
                mem::swap(&mut self.scopes.current_mut().flow, &mut base);
                branches.push(base);

                for h in x.handlers {
                    base = self.scopes.current().flow.clone();
                    let range = h.range();
                    let h = h.except_handler().unwrap(); // Only one variant for now
                    if let Some(name) = h.name
                        && let Some(type_) = h.type_
                    {
                        self.ensure_expr(&type_);
                        self.bind_definition(
                            &name,
                            Binding::ExceptionHandler(type_, x.is_star),
                            None,
                        );
                    } else if let Some(type_) = h.type_ {
                        self.ensure_expr(&type_);
                        self.table.insert(
                            Key::Anon(range),
                            Binding::ExceptionHandler(type_, x.is_star),
                        );
                    }
                    self.stmts(h.body);
                    mem::swap(&mut self.scopes.current_mut().flow, &mut base);
                    branches.push(base);
                }

                self.scopes.current_mut().flow = self.merge_flow(branches, range, false);
                self.stmts(x.finalbody);
            }
            Stmt::Assert(x) => {
                self.ensure_expr(&x.test);
                self.bind_narrow_ops(&NarrowOps::from_expr(Some(&x.test)), x.range);
                self.table
                    .insert(Key::Anon(x.test.range()), Binding::Expr(None, *x.test));
                if let Some(msg_expr) = x.msg {
                    self.ensure_expr(&msg_expr);
                    self.table
                        .insert(Key::Anon(msg_expr.range()), Binding::Expr(None, *msg_expr));
                };
            }
            Stmt::Import(x) => {
                for x in x.names {
                    let m = ModuleName::from_name(&x.name.id);
                    if let Err(err) = self.lookup.get(m) {
                        self.error(x.range, Arc::unwrap_or_clone(err));
                    }
                    match x.asname {
                        Some(asname) => {
                            self.bind_definition(
                                &asname,
                                Binding::Module(m, m.components(), None),
                                Some(FlowStyle::ImportAs(m)),
                            );
                        }
                        None => {
                            let first = m.first_component();
                            let flow_info = self.scopes.current().flow.info.get(&first);
                            let module_key = match flow_info {
                                Some(flow_info)
                                    if matches!(
                                        flow_info.style,
                                        Some(FlowStyle::MergeableImport(_))
                                    ) =>
                                {
                                    Some(flow_info.key)
                                }
                                _ => None,
                            };
                            let key = self.table.insert(
                                Key::Import(first.clone(), x.name.range),
                                Binding::Module(m, vec![first.clone()], module_key),
                            );
                            self.bind_key(&first, key, Some(FlowStyle::MergeableImport(m)));
                        }
                    }
                }
            }
            Stmt::ImportFrom(x) => {
                if let Some(m) = self.module_info.name().new_maybe_relative(
                    self.module_info.path().is_init(),
                    x.level,
                    x.module.as_ref().map(|x| &x.id),
                ) {
                    match self.lookup.get(m) {
                        Ok(module_exports) => {
                            for x in x.names {
                                if &x.name == "*" {
                                    for name in module_exports.wildcard(self.lookup).iter() {
                                        let key = Key::Import(name.clone(), x.range);
                                        let val = if module_exports.contains(name, self.lookup) {
                                            Binding::Import(m, name.clone())
                                        } else {
                                            self.error(
                                                x.range,
                                                format!("Could not import `{name}` from `{m}`"),
                                            );
                                            Binding::AnyType(AnyStyle::Error)
                                        };
                                        let key = self.table.insert(key, val);
                                        self.bind_key(name, key, Some(FlowStyle::Import(m)));
                                    }
                                } else {
                                    let asname = x.asname.unwrap_or_else(|| x.name.clone());
                                    let val = if module_exports.contains(&x.name.id, self.lookup) {
                                        Binding::Import(m, x.name.id)
                                    } else {
                                        let x_as_module_name = m.append(&x.name.id);
                                        if self.lookup.get(x_as_module_name).is_ok() {
                                            Binding::Module(
                                                x_as_module_name,
                                                x_as_module_name.components(),
                                                None,
                                            )
                                        } else {
                                            self.error(
                                                x.range,
                                                format!(
                                                    "Could not import `{}` from `{m}`",
                                                    x.name.id
                                                ),
                                            );
                                            Binding::AnyType(AnyStyle::Error)
                                        }
                                    };
                                    self.bind_definition(&asname, val, Some(FlowStyle::Import(m)));
                                }
                            }
                        }
                        Err(err) => {
                            self.error(x.range, Arc::unwrap_or_clone(err));
                            self.bind_unimportable_names(&x);
                        }
                    }
                } else {
                    self.error(
                        x.range,
                        format!(
                            "Could not resolve relative import `{}`",
                            ".".repeat(x.level as usize)
                        ),
                    );
                    self.bind_unimportable_names(&x);
                }
            }
            Stmt::Global(x) => self.todo("Bindings::stmt", &x),
            Stmt::Nonlocal(x) => self.todo("Bindings::stmt", &x),
            Stmt::Expr(x) => {
                self.ensure_expr(&x.value);
                self.table
                    .insert(Key::Anon(x.range), Binding::Expr(None, *x.value));
            }
            Stmt::Pass(_) => { /* no-op */ }
            Stmt::Break(x) => {
                self.add_loop_exitpoint(LoopExit::Break, x.range);
            }
            Stmt::Continue(x) => {
                self.add_loop_exitpoint(LoopExit::Continue, x.range);
            }
            Stmt::IpyEscapeCommand(x) => {
                self.error(x.range, "IPython escapes are not supported".to_owned())
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

    fn setup_loop(&mut self, range: TextRange, narrow_ops: &NarrowOps) {
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

    fn teardown_loop(&mut self, range: TextRange, narrow_ops: &NarrowOps, orelse: Vec<Stmt>) {
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

    fn merge_flow(&mut self, mut xs: Vec<Flow>, range: TextRange, is_loop: bool) -> Flow {
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
    fn forward_lookup(
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

pub fn is_valid_identifier(name: &str) -> bool {
    static IDENTIFIER_REGEX: LazyLock<Regex> =
        LazyLock::new(|| Regex::new("^[a-zA-Z_][a-zA-Z0-9_]*$").unwrap());
    IDENTIFIER_REGEX.is_match(name)
}
