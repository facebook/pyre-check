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
use std::ops::Deref;
use std::sync::Arc;

use dupe::Dupe;
use itertools::Either;
use itertools::Itertools;
use parse_display::Display;
use ruff_python_ast::name::Name;
use ruff_python_ast::Comprehension;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprAttribute;
use ruff_python_ast::ExprBooleanLiteral;
use ruff_python_ast::ExprCall;
use ruff_python_ast::ExprLambda;
use ruff_python_ast::ExprName;
use ruff_python_ast::ExprNoneLiteral;
use ruff_python_ast::ExprSubscript;
use ruff_python_ast::ExprYield;
use ruff_python_ast::Identifier;
use ruff_python_ast::Parameters;
use ruff_python_ast::Pattern;
use ruff_python_ast::PatternKeyword;
use ruff_python_ast::Singleton;
use ruff_python_ast::Stmt;
use ruff_python_ast::StmtClassDef;
use ruff_python_ast::StmtFunctionDef;
use ruff_python_ast::StmtImportFrom;
use ruff_python_ast::StmtReturn;
use ruff_python_ast::StringFlags;
use ruff_python_ast::TypeParam;
use ruff_python_ast::TypeParams;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::small_map::Entry;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;
use starlark_map::smallmap;
use vec1::Vec1;

use crate::ast::Ast;
use crate::binding::binding::Binding;
use crate::binding::binding::BindingAnnotation;
use crate::binding::binding::BindingClassField;
use crate::binding::binding::BindingClassMetadata;
use crate::binding::binding::BindingLegacyTypeParam;
use crate::binding::binding::ClassFieldInitialization;
use crate::binding::binding::ContextManagerKind;
use crate::binding::binding::FunctionKind;
use crate::binding::binding::Key;
use crate::binding::binding::KeyAnnotation;
use crate::binding::binding::KeyClassField;
use crate::binding::binding::KeyClassMetadata;
use crate::binding::binding::KeyExport;
use crate::binding::binding::KeyLegacyTypeParam;
use crate::binding::binding::Keyed;
use crate::binding::binding::RaisedException;
use crate::binding::binding::SizeExpectation;
use crate::binding::binding::UnpackedPosition;
use crate::binding::narrow::NarrowOp;
use crate::binding::narrow::NarrowOps;
use crate::binding::table::TableKeyed;
use crate::binding::util::is_ellipse;
use crate::binding::util::is_never;
use crate::config::Config;
use crate::dunder;
use crate::error::collector::ErrorCollector;
use crate::export::definitions::DefinitionStyle;
use crate::export::definitions::Definitions;
use crate::export::exports::LookupExport;
use crate::graph::index::Idx;
use crate::graph::index::Index;
use crate::graph::index_map::IndexMap;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::module::short_identifier::ShortIdentifier;
use crate::table;
use crate::table_for_each;
use crate::table_try_for_each;
use crate::types::special_form::SpecialForm;
use crate::types::types::AnyStyle;
use crate::types::types::Quantified;
use crate::types::types::Type;
use crate::util::display::DisplayWith;
use crate::util::prelude::SliceExt;
use crate::util::uniques::UniqueFactory;
use crate::visitors::Visitors;

#[derive(Clone, Dupe, Debug)]
pub struct Bindings(Arc<BindingsInner>);

pub type BindingEntry<K> = (Index<K>, IndexMap<K, <K as Keyed>::Value>);

table! {
    #[derive(Debug, Clone, Default)]
    pub struct BindingTable(BindingEntry)
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

struct BindingsBuilder<'a> {
    module_info: ModuleInfo,
    modules: &'a dyn LookupExport,
    config: &'a Config,
    errors: &'a ErrorCollector,
    uniques: &'a UniqueFactory,
    scopes: Vec1<Scope>,
    /// Accumulate all the return statements
    returns: Vec<StmtReturn>,
    /// Accumulate all the yield statements
    yields: Vec<ExprYield>,
    table: BindingTable,
}

/// Many names may map to the same TextRange (e.g. from foo import *).
/// But no other static will point at the same TextRange.
#[derive(Default, Clone, Debug)]
struct Static(SmallMap<Name, StaticInfo>);

#[derive(Clone, Debug)]
struct StaticInfo {
    loc: TextRange,
    /// How many times this will be redefined
    count: usize,
    /// True if this is going to appear as a `Key::Import``.
    /// A little fiddly to keep syncronised with the other field.
    uses_key_import: bool,
}

impl Static {
    fn add_with_count(&mut self, name: Name, loc: TextRange, count: usize) -> &mut StaticInfo {
        // Use whichever one we see first
        let res = self.0.entry(name).or_insert(StaticInfo {
            loc,
            count: 0,
            uses_key_import: false,
        });
        res.count += count;
        res
    }

    fn add(&mut self, name: Name, range: TextRange) {
        self.add_with_count(name, range, 1);
    }

    fn stmts(
        &mut self,
        x: &[Stmt],
        module_info: &ModuleInfo,
        top_level: bool,
        modules: &dyn LookupExport,
        config: &Config,
    ) {
        let mut d = Definitions::new(x, module_info.name(), module_info.is_init(), config);
        if top_level && module_info.name() != ModuleName::builtins() {
            d.inject_builtins();
        }
        for (name, (range, defn, count)) in d.definitions {
            self.add_with_count(name, range, count).uses_key_import =
                defn == DefinitionStyle::ImportModule;
        }
        for (m, range) in d.import_all {
            if let Ok(exports) = modules.get(m) {
                for name in exports.wildcard(modules).iter() {
                    self.add_with_count(name.clone(), range, 1).uses_key_import = true;
                }
            }
        }
    }

    fn expr_lvalue(&mut self, x: &Expr) {
        let mut add = |name: &ExprName| self.add(name.id.clone(), name.range);
        Ast::expr_lvalue(x, &mut add);
    }
}

/// The current value of the name, plus optionally the current value of the annotation.
#[derive(Default, Clone, Debug)]
struct Flow {
    info: SmallMap<Name, FlowInfo>,
    // Should this flow be merged into the next? Flow merging occurs after constructs like branches and loops.
    no_next: bool,
}

#[derive(Debug, Clone)]
struct FlowInfo {
    key: Idx<Key>,
    /// The annotation associated with this key, if any.
    /// If there is one, all subsequent bindings must obey this annotation.
    ann: Option<Idx<KeyAnnotation>>,
    /// Am I the result of an import (which needs merging)
    is_import: bool,
    /// Am I initialized, or am I the result of an annotation with no value like `x: int`?
    is_initialized: bool,
}

impl FlowInfo {
    fn new(key: Idx<Key>, ann: Option<Idx<KeyAnnotation>>) -> Self {
        Self {
            key,
            ann,
            is_import: false,
            is_initialized: true,
        }
    }
}

#[derive(Clone, Debug)]
struct ClassBodyInner {
    name: Identifier,
    instance_attributes_by_method: SmallMap<Name, SmallMap<Name, InstanceAttribute>>,
}

impl ClassBodyInner {
    fn as_self_type_key(&self) -> Key {
        Key::SelfType(ShortIdentifier::new(&self.name))
    }
}

/// Information about an instance attribute coming from a `self` assignment
/// in a method.
#[derive(Clone, Debug)]
struct InstanceAttribute(Binding, Option<Idx<KeyAnnotation>>);

#[derive(Clone, Debug)]
struct MethodInner {
    name: Identifier,
    self_name: Option<Identifier>,
    instance_attributes: SmallMap<Name, InstanceAttribute>,
}

#[derive(Clone, Debug)]
enum ScopeKind {
    Annotation,
    ClassBody(ClassBodyInner),
    Comprehension,
    Function,
    Method(MethodInner),
    Module,
}

#[derive(Clone, Debug, Display)]
enum LoopExit {
    NeverRan,
    #[display("break")]
    Break,
    #[display("continue")]
    Continue,
}

/// Flow snapshots for all possible exitpoints from a loop.
#[derive(Clone, Debug)]
struct Loop(Vec<(LoopExit, Flow)>);

#[derive(Clone, Debug)]
struct Scope {
    stat: Static,
    flow: Flow,
    /// Are Flow types above this unreachable.
    /// Set when we enter something like a function, and can't guarantee what flow values are in scope.
    barrier: bool,
    kind: ScopeKind,
    /// Stack of for/while loops we're in. Does not include comprehensions.
    loops: Vec<Loop>,
}

impl Scope {
    fn new(barrier: bool, kind: ScopeKind) -> Self {
        Self {
            stat: Default::default(),
            flow: Default::default(),
            barrier,
            kind,
            loops: Default::default(),
        }
    }

    fn annotation() -> Self {
        Self::new(false, ScopeKind::Annotation)
    }

    fn class_body(name: Identifier) -> Self {
        Self::new(
            false,
            ScopeKind::ClassBody(ClassBodyInner {
                name,
                instance_attributes_by_method: SmallMap::new(),
            }),
        )
    }

    fn comprehension() -> Self {
        Self::new(false, ScopeKind::Comprehension)
    }

    fn function() -> Self {
        Self::new(true, ScopeKind::Function)
    }

    fn method(name: Identifier) -> Self {
        Self::new(
            true,
            ScopeKind::Method(MethodInner {
                name,
                self_name: None,
                instance_attributes: SmallMap::new(),
            }),
        )
    }

    fn module() -> Self {
        Self::new(false, ScopeKind::Module)
    }
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
                self.module_info().path().display(),
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
        modules: &dyn LookupExport,
        config: &Config,
        errors: &ErrorCollector,
        uniques: &UniqueFactory,
    ) -> Self {
        let mut builder = BindingsBuilder {
            module_info: module_info.dupe(),
            modules,
            config,
            errors,
            uniques,
            scopes: Vec1::new(Scope::module()),
            returns: Vec::new(),
            yields: Vec::new(),
            table: Default::default(),
        };
        builder
            .scopes
            .last_mut()
            .stat
            .stmts(&x, &module_info, true, modules, config);
        if module_info.name() != ModuleName::builtins() {
            builder.inject_builtins();
        }
        builder.stmts(x);
        assert_eq!(builder.scopes.len(), 1);
        let last_scope = builder.scopes.to_vec().pop().unwrap();
        for (k, static_info) in last_scope.stat.0 {
            let info = last_scope.flow.info.get(&k);
            let val = match info {
                Some(FlowInfo {
                    key,
                    ann: Some(ann),
                    ..
                }) => Binding::AnnotatedType(*ann, Box::new(Binding::Forward(*key))),
                Some(FlowInfo { key, ann: None, .. }) => Binding::Forward(*key),
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
            builder.table.insert(KeyExport(k), val);
        }
        Self(Arc::new(BindingsInner {
            module_info,
            table: builder.table,
        }))
    }
}

impl BindingTable {
    fn insert<K: Keyed>(&mut self, key: K, value: K::Value) -> Idx<K>
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
    fn stmts(&mut self, x: Vec<Stmt>) {
        for x in x {
            self.stmt(x);
        }
    }

    fn inject_builtins(&mut self) {
        let builtins_module = ModuleName::builtins();
        match self.modules.get(builtins_module) {
            Ok(builtins_export) => {
                for name in builtins_export.wildcard(self.modules).iter() {
                    let key = Key::Import(name.clone(), TextRange::default());
                    let idx = self
                        .table
                        .insert(key, Binding::Import(builtins_module, name.clone()));
                    self.bind_key(name, idx, None, false, true);
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

    fn error(&self, range: TextRange, msg: String) {
        self.errors.add(&self.module_info, range, msg);
    }

    fn lookup_name(&mut self, name: &Name) -> Option<Idx<Key>> {
        let mut barrier = false;
        for scope in self.scopes.iter().rev() {
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
        self.lookup_name(name.id()).map(Binding::Forward)
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
            None if name.as_str() == "__file__" || name.as_str() == "__name__" => {
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
            self.scopes.last_mut().stat.expr_lvalue(&comp.target);
            let make_binding = |k| Binding::IterableValue(k, comp.iter.clone());
            self.bind_target(&comp.target, &make_binding, true);
        }
    }

    fn bind_lambda(&mut self, lambda: &ExprLambda) {
        self.scopes.push(Scope::function());
        if let Some(parameters) = &lambda.parameters {
            for x in parameters.iter() {
                let name = x.name();
                let ann_key = self.table.insert(
                    KeyAnnotation::Annotation(ShortIdentifier::new(name)),
                    BindingAnnotation::Type(Type::any_implicit()),
                );
                let bind_key = self.table.insert(
                    Key::Definition(ShortIdentifier::new(name)),
                    Binding::AnnotatedType(ann_key, Box::new(Binding::AnyType(AnyStyle::Implicit))),
                );
                self.scopes.last_mut().stat.add(name.id.clone(), name.range);
                self.bind_key(&name.id, bind_key, Some(ann_key), false, true);
            }
        }
    }

    /// Execute through the expr, ensuring every name has a binding.
    fn ensure_expr(&mut self, x: &Expr) {
        let new_scope = match x {
            Expr::Name(x) => {
                let name = Ast::expr_name_identifier(x.clone());
                let binding = self.forward_lookup(&name);
                self.ensure_name(&name, binding);
                false
            }
            Expr::Named(x) => {
                self.scopes.last_mut().stat.expr_lvalue(&x.target);
                let make_binding = |k| Binding::Expr(k, (*x.value).clone());
                self.bind_target(&x.target, &make_binding, true);
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
            Expr::Yield(y) => {
                self.yields.push(y.clone());
                false
            }
            _ => false,
        };
        Visitors::visit_expr(x, |x| self.ensure_expr(x));
        if new_scope {
            self.scopes.pop().unwrap();
        }
    }

    /// Execute through the expr, ensuring every name has a binding.
    fn ensure_expr_opt(&mut self, x: Option<&Expr>) {
        if let Some(x) = x {
            self.ensure_expr(x);
        }
    }

    /// Execute through the expr, ensuring every name has a binding.
    fn ensure_type(
        &mut self,
        x: &mut Expr,
        forward_lookup: &mut impl FnMut(&mut Self, &Identifier) -> Option<Binding>,
    ) {
        match x {
            Expr::Name(x) => {
                let name = Ast::expr_name_identifier(x.clone());
                let binding = forward_lookup(self, &name);
                self.ensure_name(&name, binding);
            }
            Expr::Subscript(ExprSubscript {
                value: box Expr::Name(name),
                ..
            }) if name.id == "Literal" => {
                // Don't go inside a literal, since you might find strings which are really strings, not string-types
                self.ensure_expr(x);
            }
            Expr::Subscript(ExprSubscript {
                value: box Expr::Name(name),
                slice: box Expr::Tuple(tup),
                ..
            }) if name.id == "Annotated" && !tup.is_empty() => {
                // Only go inside the first argument to Annotated, the rest are non-type metadata.
                self.ensure_type(&mut Expr::Name(name.clone()), forward_lookup);
                self.ensure_type(&mut tup.elts[0], forward_lookup);
                for e in tup.elts[1..].iter_mut() {
                    self.ensure_expr(e);
                }
            }
            Expr::StringLiteral(literal) => {
                let mut s = literal.value.to_str().to_owned();
                if literal.value.iter().any(|x| x.flags.is_triple_quoted()) {
                    // Implicitly bracketed, so add them explicitly
                    s = format!("({s})");
                }
                // We use position information to uniquely key names, so make sure we find fresh positions.
                // Because of string escapes and splits, these might not be perfect, but they are definitely fresh
                // as they point inside the string we got rid of.
                match Ast::parse_expr(&s, literal.range.start()) {
                    Ok(expr) => {
                        *x = expr;
                        // You are not allowed to nest type strings in type strings,
                        self.ensure_expr(x);
                    }
                    Err(e) => {
                        self.error(
                            literal.range,
                            format!("Could not parse type string: {s}, got {e}"),
                        );
                    }
                }
            }
            _ => Visitors::visit_expr_mut(x, |x| self.ensure_type(x, forward_lookup)),
        }
    }

    /// Execute through the expr, ensuring every name has a binding.
    fn ensure_type_opt(
        &mut self,
        x: Option<&mut Expr>,
        forward_lookup: &mut impl FnMut(&mut Self, &Identifier) -> Option<Binding>,
    ) {
        if let Some(x) = x {
            self.ensure_type(x, forward_lookup);
        }
    }

    fn bind_definition(
        &mut self,
        name: &Identifier,
        binding: Binding,
        annotation: Option<Idx<KeyAnnotation>>,
        is_initialized: bool,
    ) -> Option<Idx<KeyAnnotation>> {
        let idx = self
            .table
            .insert(Key::Definition(ShortIdentifier::new(name)), binding);
        self.bind_key(&name.id, idx, annotation, false, is_initialized)
    }

    // `should_ensure_expr` determines whether to call `ensure_expr` recursively in `bind_target`
    fn bind_unpacking(
        &mut self,
        elts: &[Expr],
        make_binding: &dyn Fn(Option<Idx<KeyAnnotation>>) -> Binding,
        range: TextRange,
        should_ensure_expr: bool,
    ) {
        // An unpacking has zero or one splats (starred expressions).
        let mut splat = false;
        for (i, e) in elts.iter().enumerate() {
            match e {
                Expr::Starred(e) => {
                    splat = true;
                    // Counts how many elements are after the splat.
                    let j = elts.len() - i - 1;
                    let make_nested_binding = |ann: Option<Idx<KeyAnnotation>>| {
                        Binding::UnpackedValue(
                            Box::new(make_binding(ann)),
                            range,
                            UnpackedPosition::Slice(i, j),
                        )
                    };
                    self.bind_target(&e.value, &make_nested_binding, should_ensure_expr);
                }
                _ => {
                    let idx = if splat {
                        // If we've encountered a splat, we no longer know how many values have been consumed
                        // from the front, but we know how many are left at the back.
                        UnpackedPosition::ReverseIndex(elts.len() - i)
                    } else {
                        UnpackedPosition::Index(i)
                    };
                    let make_nested_binding = |ann: Option<Idx<KeyAnnotation>>| {
                        Binding::UnpackedValue(Box::new(make_binding(ann)), range, idx)
                    };
                    self.bind_target(e, &make_nested_binding, should_ensure_expr);
                }
            }
        }
        let expect = if splat {
            SizeExpectation::Ge(elts.len() - 1)
        } else {
            SizeExpectation::Eq(elts.len())
        };
        self.table.insert(
            Key::Expect(range),
            Binding::UnpackedLength(Box::new(make_binding(None)), range, expect),
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
        for scope in self.scopes.iter_mut().rev() {
            if let ScopeKind::Method(method) = &mut scope.kind
                && let Some(self_name) = &method.self_name
                && matches!(&*x.value, Expr::Name(name) if name.id == self_name.id)
            {
                if !method.instance_attributes.contains_key(&x.attr.id) {
                    method
                        .instance_attributes
                        .insert(x.attr.id.clone(), InstanceAttribute(binding, annotation));
                }
                return true;
            }
        }
        false
    }

    // `should_ensure_expr` determines whether to call `ensure_expr` recursively
    fn bind_target(
        &mut self,
        target: &Expr,
        make_binding: &dyn Fn(Option<Idx<KeyAnnotation>>) -> Binding,
        should_ensure_expr: bool,
    ) {
        let mut maybe_ensure_expr = |expr| {
            if should_ensure_expr {
                self.ensure_expr(expr)
            }
        };
        match target {
            Expr::Name(name) => {
                let key = Key::Definition(ShortIdentifier::expr_name(name));
                let idx = self.table.types.0.insert(key);
                let ann = self.bind_key(&name.id, idx, None, false, true);
                self.table.types.1.insert(idx, make_binding(ann));
            }
            Expr::Attribute(x) => {
                maybe_ensure_expr(&x.value);
                let ann = self.table.insert(
                    KeyAnnotation::AttrAnnotation(x.range),
                    BindingAnnotation::AttrType(x.clone()),
                );
                let binding = make_binding(Some(ann));
                self.bind_attr_if_self(x, binding.clone(), None);
                self.table.insert(Key::Anon(x.range), binding);
            }
            Expr::Subscript(x) => {
                maybe_ensure_expr(&x.value);
                maybe_ensure_expr(&x.slice);
                let binding = make_binding(None);
                self.table.insert(
                    Key::Anon(x.range),
                    Binding::SubscriptValue(Box::new(binding), x.clone()),
                );
            }
            Expr::Tuple(tup) => {
                self.bind_unpacking(&tup.elts, make_binding, tup.range, should_ensure_expr);
            }
            Expr::List(lst) => {
                self.bind_unpacking(&lst.elts, make_binding, lst.range, should_ensure_expr);
            }
            _ => self.todo("unrecognized assignment target", target),
        }
    }

    fn update_flow_info(
        &mut self,
        name: &Name,
        key: Idx<Key>,
        annotation: Option<Idx<KeyAnnotation>>,
        is_import: bool,
        is_initialized: bool,
    ) -> Option<Idx<KeyAnnotation>> {
        match self.scopes.last_mut().flow.info.entry(name.clone()) {
            Entry::Occupied(mut e) => {
                // if there was a previous annotation, reuse that
                let annotation = annotation.or_else(|| e.get().ann);
                *e.get_mut() = FlowInfo {
                    key,
                    ann: annotation,
                    is_import,
                    is_initialized,
                };
                annotation
            }
            Entry::Vacant(e) => {
                e.insert(FlowInfo {
                    key,
                    ann: annotation,
                    is_import,
                    is_initialized,
                });
                annotation
            }
        }
    }

    /// Return the annotation that should be used at the moment, if one was provided.
    fn bind_key(
        &mut self,
        name: &Name,
        key: Idx<Key>,
        annotation: Option<Idx<KeyAnnotation>>,
        is_import: bool,
        is_initialized: bool,
    ) -> Option<Idx<KeyAnnotation>> {
        let annotation = self.update_flow_info(name, key, annotation, is_import, is_initialized);
        let info = self.scopes.last().stat.0.get(name).unwrap_or_else(|| {
            let module = self.module_info.name();
            panic!("Name `{name}` not found in static scope of module `{module}`")
        });
        if info.count > 1 || matches!(self.scopes.last().kind, ScopeKind::ClassBody(_)) {
            self.table
                .insert_anywhere(name.clone(), info.loc)
                .1
                .insert(key);
        }
        annotation
    }

    fn type_params(&mut self, x: &mut TypeParams) -> Vec<Quantified> {
        let mut qs = Vec::new();
        for x in x.type_params.iter_mut() {
            let q = match x {
                TypeParam::TypeVar(x) => {
                    if let Some(bound) = &mut x.bound {
                        self.ensure_type(bound, &mut BindingsBuilder::forward_lookup);
                    }
                    Quantified::type_var(self.uniques)
                }
                TypeParam::ParamSpec(_) => Quantified::param_spec(self.uniques),
                TypeParam::TypeVarTuple(_) => Quantified::type_var_tuple(self.uniques),
            };
            qs.push(q);
            let name = x.name();
            self.scopes.last_mut().stat.add(name.id.clone(), name.range);
            self.bind_definition(name, Binding::TypeParameter(q), None, true);
        }
        qs
    }

    fn parameters(&mut self, x: &mut Parameters, self_type: &Option<Idx<Key>>) {
        let mut self_name = None;
        for x in x.iter() {
            let name = x.name();
            if self_type.is_some() && self_name.is_none() {
                self_name = Some(name.clone());
            }
            let ann_val = match x.annotation() {
                Some(a) => BindingAnnotation::AnnotateExpr(a.clone(), *self_type),
                None => {
                    if let Some(self_name) = &self_name
                        && name.id == *self_name.id
                    {
                        BindingAnnotation::Forward(self_type.unwrap())
                    } else {
                        BindingAnnotation::Type(Type::any_implicit())
                    }
                }
            };
            let ann_key = self.table.insert(
                KeyAnnotation::Annotation(ShortIdentifier::new(name)),
                ann_val,
            );
            let bind_key = self.table.insert(
                Key::Definition(ShortIdentifier::new(name)),
                Binding::AnnotatedType(ann_key, Box::new(Binding::AnyType(AnyStyle::Implicit))),
            );
            self.scopes.last_mut().stat.add(name.id.clone(), name.range);
            self.bind_key(&name.id, bind_key, Some(ann_key), false, true);
        }
        if let Scope {
            kind: ScopeKind::Method(method),
            ..
        } = self.scopes.last_mut()
        {
            method.self_name = self_name;
        }
    }

    fn function_def(&mut self, mut x: StmtFunctionDef) {
        let body = mem::take(&mut x.body);
        let kind = if is_ellipse(&body) {
            FunctionKind::Stub
        } else {
            FunctionKind::Impl
        };
        let mut return_annotation = mem::take(&mut x.returns);
        let return_count = self.returns.len();
        let yield_count = self.yields.len();

        let never = is_never(&body, self.config);
        if never != Some(Vec::new()) && kind == FunctionKind::Impl {
            // If we can reach the end, and the code is real (not just ellipse),
            // check None is an OK return type.
            // Note that we special case ellipse even in non-interface, as that is what Pyright does.
            self.returns.push(StmtReturn {
                range: match never.as_deref() {
                    Some([x]) => x.range(), // Try and narrow the range
                    _ => x.range,
                },
                value: None,
            });
        }
        let func_name = x.name.clone();
        let self_type = match &self.scopes.last().kind {
            ScopeKind::ClassBody(body) => Some(self.table.types.0.insert(body.as_self_type_key())),
            _ => None,
        };

        self.scopes.push(Scope::annotation());

        let tparams = x
            .type_params
            .as_mut()
            .map(|tparams| self.type_params(tparams));

        let mut legacy_tparam_builder = LegacyTParamBuilder::new(tparams.is_some());

        // We need to bind all the parameters expressions _after_ the type params, but before the parameter names,
        // which might shadow some types.
        for (param, default) in Ast::parameters_iter_mut(&mut x.parameters) {
            self.ensure_type_opt(param.annotation.as_deref_mut(), &mut |lookup_name, name| {
                legacy_tparam_builder.forward_lookup(lookup_name, name)
            });
            if let Some(default) = default {
                self.ensure_expr_opt(default.as_deref());
            }
        }
        self.ensure_type_opt(
            return_annotation.as_deref_mut(),
            &mut |lookup_name, name| legacy_tparam_builder.forward_lookup(lookup_name, name),
        );

        legacy_tparam_builder.add_name_definitions(self);

        if self_type.is_none() {
            self.scopes.push(Scope::function());
        } else {
            self.scopes.push(Scope::method(func_name.clone()));
        }

        let legacy_tparams = legacy_tparam_builder.lookup_keys(self);

        self.parameters(
            &mut x.parameters,
            if func_name.id == dunder::NEW {
                // __new__ is a staticmethod that is special-cased at runtime to not need @staticmethod decoration.
                &None
            } else {
                &self_type
            },
        );

        self.scopes.last_mut().stat.stmts(
            &body,
            &self.module_info,
            false,
            self.modules,
            self.config,
        );
        self.stmts(body);
        let func_scope = self.scopes.pop().unwrap();
        self.scopes.pop().unwrap();

        if let ScopeKind::Method(method) = &func_scope.kind
            && let ScopeKind::ClassBody(body) = &mut self.scopes.last_mut().kind
        {
            body.instance_attributes_by_method
                .insert(method.name.id.clone(), method.instance_attributes.clone());
        }

        self.bind_definition(
            &func_name,
            Binding::Function(Box::new(x), kind, legacy_tparams.into_boxed_slice()),
            None,
            true,
        );

        let mut return_exprs = Vec::new();
        while self.returns.len() > return_count {
            return_exprs.push(self.returns.pop().unwrap());
        }

        let mut yield_exprs = Vec::new();
        while self.yields.len() > yield_count {
            yield_exprs.push(self.yields.pop().unwrap());
        }

        let return_ann = return_annotation.map(|x| {
            self.table.insert(
                KeyAnnotation::ReturnAnnotation(ShortIdentifier::new(&func_name)),
                BindingAnnotation::AnnotateExpr(*x, self_type),
            )
        });
        let mut return_expr_keys = SmallSet::with_capacity(return_exprs.len());
        for x in return_exprs {
            let key = self.table.insert(
                Key::ReturnExpression(ShortIdentifier::new(&func_name), x.range),
                Binding::Expr(return_ann, return_expr(x)),
            );
            return_expr_keys.insert(key);
        }

        let mut yield_expr_keys = SmallSet::with_capacity(yield_exprs.len());
        for x in yield_exprs {
            let key = self.table.insert(
                Key::YieldExpression(ShortIdentifier::new(&func_name), x.range()),
                // collect the value of the yield expression.
                Binding::Expr(None, yield_expr(x)),
            );
            yield_expr_keys.insert(key);
        }

        let mut return_type = Binding::phi(return_expr_keys);
        let yield_type = Binding::phi(yield_expr_keys);

        if let Some(ann) = return_ann {
            return_type = Binding::AnnotatedType(ann, Box::new(return_type));
        }
        self.table.insert(
            Key::ReturnType(ShortIdentifier::new(&func_name)),
            return_type,
        );
        self.table
            .insert(Key::YieldType(ShortIdentifier::new(&func_name)), yield_type);
    }

    fn class_def(&mut self, mut x: StmtClassDef) {
        let body = mem::take(&mut x.body);

        self.scopes.push(Scope::class_body(x.name.clone()));

        let definition_key = self
            .table
            .types
            .0
            .insert(Key::Definition(ShortIdentifier::new(&x.name)));

        x.type_params.iter_mut().for_each(|x| {
            self.type_params(x);
        });

        let mut legacy_tparam_builder = LegacyTParamBuilder::new(x.type_params.is_some());
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
            self.ensure_type(&mut base, &mut |lookup_name, name| {
                legacy_tparam_builder.forward_lookup(lookup_name, name)
            });
            base
        });

        let mut keywords = SmallMap::new();
        x.keywords().iter().for_each(|keyword| {
            if let Some(name) = &keyword.arg {
                self.ensure_expr(&keyword.value);
                if keywords
                    .insert(name.id.clone(), keyword.value.clone())
                    .is_some()
                {
                    // TODO(stroxler) We should use a Vec rather than a Map in the binding
                    // so that we can still type check the values associated with
                    // duplicate keywords.
                    //
                    // For now, we get a type error from the parser but never
                    // check the expression.
                }
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
            BindingClassMetadata(definition_key, bases.clone(), keywords),
        );

        legacy_tparam_builder.add_name_definitions(self);

        let cur_scope = self.scopes.last().clone();
        let non_field_names = cur_scope.flow.info.keys().collect::<SmallSet<_>>();

        self.scopes.last_mut().stat.stmts(
            &body,
            &self.module_info,
            false,
            self.modules,
            self.config,
        );
        self.stmts(body);

        let last_scope = self.scopes.pop().unwrap();
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
                let initialization = if info.is_initialized {
                    ClassFieldInitialization::Class
                } else {
                    ClassFieldInitialization::Instance
                };
                let binding = BindingClassField(flow_type, info.ann.dupe(), initialization);
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
                    for (name, attribute) in instance_attributes {
                        if !fields.contains(&name) {
                            fields.insert(name.clone());
                            self.table.insert(
                                KeyClassField(ShortIdentifier::new(&x.name), name),
                                BindingClassField(
                                    attribute.0,
                                    attribute.1,
                                    ClassFieldInitialization::Instance,
                                ),
                            );
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

        self.bind_definition(
            &x.name.clone(),
            Binding::ClassDef(
                Box::new((x, fields)),
                bases.into_boxed_slice(),
                legacy_tparams.into_boxed_slice(),
            ),
            None,
            true,
        );
    }

    fn add_loop_exitpoint(&mut self, exit: LoopExit, range: TextRange) {
        let scope = self.scopes.last_mut();
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
                        smallmap! { subject_name.clone() => (NarrowOp::Eq(p.value.clone()), p.range()) },
                    )
                } else {
                    NarrowOps::new()
                }
            }
            Pattern::MatchSingleton(p) => {
                let value = match p.value {
                    Singleton::None => Expr::NoneLiteral(ExprNoneLiteral::default()),
                    Singleton::True | Singleton::False => {
                        Expr::BooleanLiteral(ExprBooleanLiteral {
                            range: TextRange::default(),
                            value: matches!(p.value, Singleton::True),
                        })
                    }
                };
                if let Some(subject_name) = subject_name {
                    NarrowOps(
                        smallmap! { subject_name.clone() => (NarrowOp::Is(Box::new(value)), p.range()) },
                    )
                } else {
                    NarrowOps::new()
                }
            }
            Pattern::MatchAs(p) => {
                // If there's no name for this pattern, refine the variable being matched
                // If there is a new name, refine that instead
                let new_subject_name = if let Some(name) = &p.name {
                    self.bind_definition(name, Binding::Forward(key), None, true);
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
                                    true,
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
                    Key::Expect(x.range),
                    Binding::UnpackedLength(Box::new(Binding::Forward(key)), x.range, expect),
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
                    self.bind_definition(&rest, Binding::Forward(key), None, true);
                }
                narrow_ops
            }
            Pattern::MatchClass(x) => {
                let mut narrow_ops = NarrowOps::new();
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
                    let mut base = self.scopes.last().flow.clone();
                    let new_narrow_ops = self.bind_pattern(subject_name, pattern, key);
                    if let Some(ref mut ops) = narrow_ops {
                        ops.or_all(new_narrow_ops)
                    } else {
                        narrow_ops = Some(new_narrow_ops);
                    }
                    mem::swap(&mut self.scopes.last_mut().flow, &mut base);
                    branches.push(base);
                }
                self.scopes.last_mut().flow = self.merge_flow(branches, range, false);
                narrow_ops.unwrap_or_default()
            }
            Pattern::MatchStar(_) => NarrowOps::new(),
        }
    }

    fn bind_unimportable_names(&mut self, x: &StmtImportFrom) {
        for x in &x.names {
            if &x.name != "*" {
                let asname = x.asname.as_ref().unwrap_or(&x.name);
                self.bind_definition(asname, Binding::AnyType(AnyStyle::Error), None, true);
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
                self.update_flow_info(name, binding_key, None, false, true);
            }
        }
    }

    /// Evaluate the statements and update the bindings.
    /// Every statement should end up in the bindings, perhaps with a location that is never used.
    fn stmt(&mut self, x: Stmt) {
        match x {
            Stmt::FunctionDef(x) => self.function_def(x),
            Stmt::ClassDef(x) => self.class_def(x),
            Stmt::Return(x) => {
                self.ensure_expr_opt(x.value.as_deref());
                self.returns.push(x);
                self.scopes.last_mut().flow.no_next = true;
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
                // Handle forward references in a TypeVar call.
                match &mut value {
                    Expr::Call(ExprCall {
                        range: _,
                        func: box Expr::Name(type_var),
                        arguments,
                    }) if type_var.id == "TypeVar" && !arguments.is_empty() => {
                        self.ensure_expr(&Expr::Name(type_var.clone()));
                        // The constraints (i.e., any positional arguments after the first)
                        // and some keyword arguments are types.
                        for arg in arguments.args.iter_mut().skip(1) {
                            self.ensure_type(arg, &mut BindingsBuilder::forward_lookup);
                        }
                        for kw in arguments.keywords.iter_mut() {
                            if let Some(id) = &kw.arg
                                && (id.id == "bound" || id.id == "default")
                            {
                                self.ensure_type(
                                    &mut kw.value,
                                    &mut BindingsBuilder::forward_lookup,
                                );
                            } else {
                                self.ensure_expr(&kw.value);
                            }
                        }
                    }
                    _ => self.ensure_expr(&value),
                }
                for target in x.targets {
                    let make_binding = |k: Option<Idx<KeyAnnotation>>| {
                        let b = Binding::Expr(k, value.clone());
                        if let Some(name) = &name {
                            Binding::NameAssign(name.clone(), k, Box::new(b), value.range())
                        } else {
                            b
                        }
                    };
                    self.bind_target(&target, &make_binding, true)
                }
            }
            Stmt::AugAssign(x) => {
                self.ensure_expr(&x.target);
                self.ensure_expr(&x.value);
                let make_binding = |_: Option<Idx<KeyAnnotation>>| Binding::AugAssign(x.clone());
                self.bind_target(&x.target, &make_binding, false);
            }
            Stmt::AnnAssign(mut x) => match *x.target {
                Expr::Name(name) => {
                    let name = Ast::expr_name_identifier(name);
                    let ann_key = KeyAnnotation::Annotation(ShortIdentifier::new(&name));
                    self.ensure_type(&mut x.annotation, &mut BindingsBuilder::forward_lookup);
                    let ann_val = if let Some(special) = SpecialForm::new(&name.id, &x.annotation) {
                        BindingAnnotation::Type(special.to_type())
                    } else {
                        BindingAnnotation::AnnotateExpr(*x.annotation.clone(), None)
                    };
                    let ann_key = self.table.insert(ann_key, ann_val);

                    if let Some(mut value) = x.value
                        && (!self.module_info.is_interface()
                            || !matches!(&*value, Expr::EllipsisLiteral(_)))
                    {
                        // Handle forward references in explicit type aliases.
                        if let Expr::Name(name) = *x.annotation
                            && name.id == "TypeAlias"
                        {
                            self.ensure_type(&mut value, &mut BindingsBuilder::forward_lookup);
                        } else {
                            self.ensure_expr(&value);
                        }
                        let range = value.range();
                        self.bind_definition(
                            &name.clone(),
                            Binding::NameAssign(
                                name.id,
                                Some(ann_key),
                                Box::new(Binding::Expr(Some(ann_key), *value)),
                                range,
                            ),
                            Some(ann_key),
                            true,
                        );
                    } else {
                        self.bind_definition(
                            &name,
                            Binding::AnnotatedType(
                                ann_key,
                                Box::new(Binding::AnyType(AnyStyle::Implicit)),
                            ),
                            Some(ann_key),
                            false,
                        );
                    }
                }
                Expr::Attribute(attr) => {
                    self.ensure_expr(&attr.value);
                    self.ensure_type(&mut x.annotation, &mut BindingsBuilder::forward_lookup);
                    // This is the type of the attribute.
                    let attr_key = self.table.insert(
                        KeyAnnotation::AttrAnnotation(attr.range),
                        BindingAnnotation::AttrType(attr.clone()),
                    );
                    // This is the type annotation on the assignment.
                    let ann_key = self.table.insert(
                        KeyAnnotation::AttrAnnotation(x.annotation.range()),
                        BindingAnnotation::AnnotateExpr(*x.annotation, None),
                    );
                    let value_type = match &x.value {
                        Some(v) => Binding::Expr(None, *v.clone()),
                        None => Binding::AnyType(AnyStyle::Implicit),
                    };
                    if self.bind_attr_if_self(&attr, value_type, Some(ann_key)) {
                        self.table.insert(
                            Key::Expect(attr.range),
                            Binding::Eq(ann_key, attr_key, attr.attr.id),
                        );
                    } else {
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
                    if let Some(v) = x.value {
                        self.ensure_expr(&v);
                        self.table
                            .insert(Key::Anon(v.range()), Binding::Expr(Some(ann_key), *v));
                    }
                }
                _ => self.todo("Bindings::stmt AnnAssign", &x),
            },
            Stmt::TypeAlias(mut x) => {
                if let Expr::Name(name) = *x.name {
                    if let Some(params) = &mut x.type_params {
                        self.type_params(params);
                    }
                    self.ensure_type(&mut x.value, &mut BindingsBuilder::forward_lookup);
                    let expr_binding = Binding::Expr(None, *x.value);
                    let binding = Binding::ScopedTypeAlias(
                        name.id.clone(),
                        x.type_params.map(Box::new),
                        Box::new(expr_binding),
                        x.range,
                    );
                    self.bind_definition(&Ast::expr_name_identifier(name), binding, None, true);
                } else {
                    self.todo("Bindings::stmt TypeAlias", &x);
                }
            }
            Stmt::For(x) => {
                self.setup_loop(x.range, &NarrowOps::new());
                self.ensure_expr(&x.iter);
                let make_binding = |k| Binding::IterableValue(k, *x.iter.clone());
                self.bind_target(&x.target, &make_binding, true);
                self.stmts(x.body);
                self.teardown_loop(x.range, &NarrowOps::new(), x.orelse);
            }
            Stmt::While(x) => {
                let narrow_ops = NarrowOps::from_expr(Some(*x.test.clone()));
                self.setup_loop(x.range, &narrow_ops);
                self.ensure_expr(&x.test);
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
                    let mut base = self.scopes.last().flow.clone();
                    self.ensure_expr_opt(test.as_ref());
                    let new_narrow_ops = NarrowOps::from_expr(test);
                    if let Some(stmt) = body.first() {
                        let use_range = stmt.range();
                        self.bind_narrow_ops(&negated_prev_ops, use_range);
                        self.bind_narrow_ops(&new_narrow_ops, use_range);
                    }
                    negated_prev_ops.and_all(new_narrow_ops.negate());
                    self.stmts(body);
                    mem::swap(&mut self.scopes.last_mut().flow, &mut base);
                    branches.push(base);
                    if b == Some(true) {
                        exhaustive = true;
                        break; // We picked this branch, none others stand a chance
                    }
                }
                if !exhaustive {
                    branches.push(self.scopes.last().flow.clone());
                }
                self.scopes.last_mut().flow = self.merge_flow(branches, range, false);
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
                        self.bind_target(&opts, &make_binding, true);
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
                let subject_name = if let Expr::Name(ref name) = *x.subject {
                    Some(&name.id)
                } else {
                    None
                };
                let key = self.table.insert(
                    Key::Anon(x.subject.range()),
                    Binding::Expr(None, *x.subject.clone()),
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
                    let mut base = self.scopes.last().flow.clone();
                    if case.pattern.is_wildcard() || case.pattern.is_irrefutable() {
                        exhaustive = true;
                    }
                    let new_narrow_ops = self.bind_pattern(subject_name, case.pattern, key);
                    self.bind_narrow_ops(&negated_prev_ops, case.range);
                    self.bind_narrow_ops(&new_narrow_ops, case.range);
                    negated_prev_ops.and_all(new_narrow_ops.negate());
                    if let Some(guard) = case.guard {
                        self.ensure_expr(&guard);
                        self.table
                            .insert(Key::Anon(guard.range()), Binding::Expr(None, *guard));
                    }
                    self.stmts(case.body);
                    mem::swap(&mut self.scopes.last_mut().flow, &mut base);
                    branches.push(base);
                    if exhaustive {
                        break;
                    }
                }
                if !exhaustive {
                    branches.push(self.scopes.last().flow.clone());
                }
                self.scopes.last_mut().flow = self.merge_flow(branches, range, false);
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
                    self.table
                        .insert(Key::Expect(x.range), Binding::CheckRaisedException(raised));
                } else {
                    // If there's no exception raised, don't bother checking the cause.
                }
                self.scopes.last_mut().flow.no_next = true;
            }
            Stmt::Try(x) => {
                let range = x.range;
                let mut branches = Vec::new();
                let mut base = self.scopes.last().flow.clone();

                // We branch before the body, conservatively assuming that any statement can fail
                // entry -> try -> else -> finally
                //   |                     ^
                //   ----> handler --------|

                self.stmts(x.body);
                self.stmts(x.orelse);
                mem::swap(&mut self.scopes.last_mut().flow, &mut base);
                branches.push(base);

                for h in x.handlers {
                    base = self.scopes.last().flow.clone();
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
                            true,
                        );
                    } else if let Some(type_) = h.type_ {
                        self.ensure_expr(&type_);
                        self.table.insert(
                            Key::Anon(range),
                            Binding::ExceptionHandler(type_, x.is_star),
                        );
                    }
                    self.stmts(h.body);
                    mem::swap(&mut self.scopes.last_mut().flow, &mut base);
                    branches.push(base);
                }

                self.scopes.last_mut().flow = self.merge_flow(branches, range, false);
                self.stmts(x.finalbody);
            }
            Stmt::Assert(x) => {
                self.ensure_expr(&x.test);
                self.bind_narrow_ops(&NarrowOps::from_expr(Some(*x.test.clone())), x.range);
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
                    if let Err(err) = self.modules.get(m) {
                        self.error(x.range, Arc::unwrap_or_clone(err));
                    }
                    match x.asname {
                        Some(asname) => {
                            self.bind_definition(
                                &asname,
                                Binding::Module(m, m.components(), None),
                                None,
                                true,
                            );
                        }
                        None => {
                            let first = m.first_component();
                            let flow_info = self.scopes.last().flow.info.get(&first);
                            let module_key = match flow_info {
                                Some(flow_info) if flow_info.is_import => Some(flow_info.key),
                                _ => None,
                            };
                            let key = self.table.insert(
                                Key::Import(first.clone(), x.name.range),
                                Binding::Module(m, vec![first.clone()], module_key),
                            );
                            self.bind_key(&first, key, None, true, true);
                        }
                    }
                }
            }
            Stmt::ImportFrom(x) => {
                if let Some(m) = self.module_info.name().new_maybe_relative(
                    self.module_info.is_init(),
                    x.level,
                    x.module.as_ref().map(|x| &x.id),
                ) {
                    match self.modules.get(m) {
                        Ok(module_exports) => {
                            for x in x.names {
                                if &x.name == "*" {
                                    for name in module_exports.wildcard(self.modules).iter() {
                                        let key = Key::Import(name.clone(), x.range);
                                        let val = if module_exports.contains(name, self.modules) {
                                            Binding::Import(m, name.clone())
                                        } else {
                                            self.error(
                                                x.range,
                                                format!("Could not import `{name}` from `{m}`"),
                                            );
                                            Binding::AnyType(AnyStyle::Error)
                                        };
                                        let key = self.table.insert(key, val);
                                        self.bind_key(name, key, None, false, true);
                                    }
                                } else {
                                    let asname = x.asname.unwrap_or_else(|| x.name.clone());
                                    let val = if module_exports.contains(&x.name.id, self.modules) {
                                        Binding::Import(m, x.name.id)
                                    } else {
                                        self.error(
                                            x.range,
                                            format!("Could not import `{}` from `{m}`", x.name.id),
                                        );
                                        Binding::AnyType(AnyStyle::Error)
                                    };
                                    self.bind_definition(&asname, val, None, true);
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
            Stmt::IpyEscapeCommand(x) => self.todo("Bindings::stmt", &x),
        }
    }

    /// Helper for loops, inserts a phi key for every name in the given flow.
    fn insert_phi_keys(&mut self, x: Flow, range: TextRange) -> Flow {
        let items = x
            .info
            .iter_hashed()
            .map(|x| (x.0.cloned(), x.1.ann))
            .collect::<SmallSet<_>>();
        let mut res = SmallMap::with_capacity(items.len());
        for (name, ann) in items.into_iter() {
            let key = self
                .table
                .types
                .0
                .insert(Key::Phi(name.key().clone(), range));
            res.insert_hashed(name, FlowInfo::new(key, ann));
        }
        Flow {
            info: res,
            no_next: false,
        }
    }

    fn setup_loop(&mut self, range: TextRange, narrow_ops: &NarrowOps) {
        let base = self.scopes.last().flow.clone();
        // To account for possible assignments to existing names in a loop, we
        // speculatively insert phi keys upfront.
        self.scopes.last_mut().flow = self.insert_phi_keys(base.clone(), range);
        self.bind_narrow_ops(narrow_ops, range);
        self.scopes
            .last_mut()
            .loops
            .push(Loop(vec![(LoopExit::NeverRan, base)]));
    }

    fn teardown_loop(&mut self, range: TextRange, narrow_ops: &NarrowOps, orelse: Vec<Stmt>) {
        let done = self.scopes.last_mut().loops.pop().unwrap();
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
            let (values, unordered_anns): (
                SmallSet<Idx<Key>>,
                SmallSet<Option<Idx<KeyAnnotation>>>,
            ) = visible_branches
                .iter()
                .flat_map(|x| x.info.get(name.key()).cloned().map(|x| (x.key, x.ann)))
                .unzip();
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
                                Key::Expect(other_ann.1),
                                Binding::Eq(other_ann.0, ann.0, name.deref().clone()),
                            );
                        }
                    }
                }
            }
            let key = self
                .table
                .insert(Key::Phi(name.key().clone(), range), Binding::phi(values));
            res.insert_hashed(name, FlowInfo::new(key, ann.map(|x| x.0)));
        }
        Flow {
            info: res,
            no_next: false,
        }
    }

    fn merge_loop_into_current(&mut self, mut branches: Vec<Flow>, range: TextRange) {
        branches.push(self.scopes.last().flow.clone());
        self.scopes.last_mut().flow = self.merge_flow(branches, range, true);
    }
}

/// Handle intercepting names inside either function parameter/return
/// annotations or base class lists of classes, in order to check whether they
/// point at type variable declarations and need to be converted to type
/// parameters.
struct LegacyTParamBuilder {
    /// All of the names used. Each one may or may not point at a type variable
    /// and therefore bind a legacy type parameter.
    legacy_tparams: SmallMap<Name, Option<(Identifier, Idx<Key>)>>,
    /// Are there scoped type parameters? Used to control downstream errors.
    has_scoped_tparams: bool,
}

impl LegacyTParamBuilder {
    fn new(has_scoped_tparams: bool) -> Self {
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
            .or_insert_with(|| builder.lookup_name(name.id()).map(|x| (name.clone(), x)))
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
    fn add_name_definitions(&self, builder: &mut BindingsBuilder) {
        for entry in self.legacy_tparams.values() {
            if let Some((identifier, key)) = entry {
                builder.table.insert(
                    KeyLegacyTypeParam(ShortIdentifier::new(identifier)),
                    BindingLegacyTypeParam(*key),
                );
                builder
                    .scopes
                    .last_mut()
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
                    true,
                );
            }
        }
    }

    /// Get the keys that correspond to the result of checking whether a name
    /// corresponds to a legacy type param. This is used when actually computing
    /// the final type parameters for classes and functions, which have to take
    /// all the names that *do* map to type variable declarations and combine
    /// them (potentially) with scoped type parameters.
    fn lookup_keys(&self, builder: &mut BindingsBuilder) -> Vec<Idx<KeyLegacyTypeParam>> {
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

fn return_expr(x: StmtReturn) -> Expr {
    match x.value {
        Some(x) => *x,
        None => Expr::NoneLiteral(ExprNoneLiteral { range: x.range }),
    }
}

fn yield_expr(x: ExprYield) -> Expr {
    match x.value {
        Some(x) => *x,
        None => Expr::NoneLiteral(ExprNoneLiteral { range: x.range }),
    }
}
