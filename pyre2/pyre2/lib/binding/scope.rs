/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt::Debug;

use dupe::Dupe;
use parse_display::Display;
use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprName;
use ruff_python_ast::Identifier;
use ruff_python_ast::Stmt;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;
use starlark_map::Hashed;
use vec1::Vec1;

use crate::binding::binding::Binding;
use crate::binding::binding::ClassFieldInitialValue;
use crate::binding::binding::Key;
use crate::binding::binding::KeyAnnotation;
use crate::binding::binding::KeyClass;
use crate::binding::binding::KeyClassMetadata;
use crate::binding::binding::KeyFunction;
use crate::binding::bindings::BindingTable;
use crate::export::definitions::DefinitionStyle;
use crate::export::definitions::Definitions;
use crate::export::exports::LookupExport;
use crate::export::special::SpecialEntry;
use crate::graph::index::Idx;
use crate::metadata::RuntimeMetadata;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::module::short_identifier::ShortIdentifier;
use crate::ruff::ast::Ast;
use crate::types::class::ClassIndex;

/// Many names may map to the same TextRange (e.g. from foo import *).
/// But no other static will point at the same TextRange.
#[derive(Default, Clone, Debug)]
pub struct Static(pub SmallMap<Name, StaticInfo>);

#[derive(Clone, Debug)]
pub struct StaticInfo {
    pub loc: TextRange,
    /// The location of the first annotated name for this binding, if any.
    pub annot: Option<Idx<KeyAnnotation>>,
    /// How many times this will be redefined
    pub count: usize,
    pub style: DefinitionStyle,
}

impl StaticInfo {
    pub fn as_key(&self, name: &Name) -> Key {
        if self.count == 1 {
            if matches!(self.style, DefinitionStyle::ImportModule(_)) {
                Key::Import(name.clone(), self.loc)
            } else {
                // We are constructing an identifier, but it must have been one that we saw earlier
                assert_ne!(self.loc, TextRange::default());
                Key::Definition(ShortIdentifier::new(&Identifier {
                    id: name.clone(),
                    range: self.loc,
                }))
            }
        } else {
            Key::Anywhere(name.clone(), self.loc)
        }
    }

    pub fn is_global(&self) -> bool {
        self.style == DefinitionStyle::Global
    }

    pub fn is_nonlocal(&self) -> bool {
        self.style == DefinitionStyle::Nonlocal
    }
}

impl Static {
    fn add_with_count(
        &mut self,
        name: Hashed<Name>,
        loc: TextRange,
        annot: Option<Idx<KeyAnnotation>>,
        count: usize,
    ) -> &mut StaticInfo {
        // Use whichever one we see first
        let res = self.0.entry_hashed(name).or_insert(StaticInfo {
            loc,
            annot,
            count: 0,
            style: DefinitionStyle::Local,
        });
        res.count += count;
        res
    }

    pub fn add(&mut self, name: Name, range: TextRange, annot: Option<Idx<KeyAnnotation>>) {
        self.add_with_count(Hashed::new(name), range, annot, 1);
    }

    pub fn stmts(
        &mut self,
        x: &[Stmt],
        module_info: &ModuleInfo,
        top_level: bool,
        lookup: &dyn LookupExport,
        config: &RuntimeMetadata,
        mut get_annotation_idx: impl FnMut(ShortIdentifier) -> Idx<KeyAnnotation>,
    ) {
        let mut d = Definitions::new(x, module_info.name(), module_info.path().is_init(), config);
        if top_level && module_info.name() != ModuleName::builtins() {
            d.inject_builtins();
        }

        let mut wildcards = Vec::with_capacity(d.import_all.len());
        for (m, range) in d.import_all {
            if let Ok(exports) = lookup.get(m) {
                wildcards.push((range, exports.wildcard(lookup)));
            }
        }

        // Try and avoid rehashing while we insert, with a little bit of spare space
        let capacity_guess =
            d.definitions.len() + wildcards.iter().map(|x| x.1.len()).sum::<usize>();
        self.0.reserve(((capacity_guess * 5) / 4) + 25);

        for (name, def) in d.definitions.into_iter_hashed() {
            let annot = def.annot.map(&mut get_annotation_idx);
            let info = self.add_with_count(name, def.range, annot, def.count);
            info.style = def.style;
        }
        for (range, wildcard) in wildcards {
            for name in wildcard.iter_hashed() {
                // TODO: semantics of import * and global var with same name
                self.add_with_count(name.cloned(), range, None, 1).style =
                    DefinitionStyle::ImportModule(module_info.name());
            }
        }
    }

    pub fn expr_lvalue(&mut self, x: &Expr) {
        let mut add = |name: &ExprName| self.add(name.id.clone(), name.range, None);
        Ast::expr_lvalue(x, &mut add);
    }
}

/// Flow-sensitive information about a name.
#[derive(Default, Clone, Debug)]
pub struct Flow {
    pub info: SmallMap<Name, FlowInfo>,
    // Should this flow be merged into the next? Flow merging occurs after constructs like branches and loops.
    pub no_next: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FlowStyle {
    /// Am I a type-annotated assignment in a class body?
    AnnotatedClassField { initial_value: Option<Expr> },
    /// Am I the result of an import (which needs merging).
    /// E.g. `import foo.bar` and `import foo.baz` need merging.
    /// The `ModuleName` will be the most recent entry.
    MergeableImport(ModuleName),
    /// Was I imported from somewhere (and if so, where)
    /// E.g. Both `from foo import bar` and
    /// `from foo import bar as baz` would get `(foo, bar)`.
    Import(ModuleName, Name),
    /// Am I an alias for a module import, `import foo.bar as baz`
    /// would get `foo.bar` here.
    ImportAs(ModuleName),
    /// Am I a function definition? Used to chain overload definitions.
    /// If so, does my return type have an explicit annotation?
    FunctionDef(Idx<KeyFunction>, bool),
    /// The name is possibly unbound (perhaps due to merging branches)
    PossiblyUnbound,
    /// The name is possibly uninitialized (perhaps due to merging branches)
    PossiblyUninitialized,
    /// The name was previously bound, but is now unbound due to `del`
    Unbound,
    /// The name was in an annotated declaration like `x: int` but not initialized
    Uninitialized,
}

impl FlowStyle {
    pub fn error_message(&self, name: &Identifier) -> Option<String> {
        match self {
            Self::Unbound => Some(format!("`{name}` is unbound")),
            Self::Uninitialized => Some(format!("`{name}` is uninitialized")),
            Self::PossiblyUnbound => Some(format!("`{name}` may be unbound")),
            Self::PossiblyUninitialized => Some(format!("`{name}` may be uninitialized")),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FlowInfo {
    pub key: Idx<Key>,
    pub style: Option<FlowStyle>,
}

impl FlowInfo {
    pub fn as_initial_value(&self) -> ClassFieldInitialValue {
        match self.style.as_ref() {
            Some(FlowStyle::AnnotatedClassField {
                initial_value: Some(e),
            }) => ClassFieldInitialValue::Class(Some(e.clone())),
            Some(FlowStyle::AnnotatedClassField {
                initial_value: None,
            }) => ClassFieldInitialValue::Instance(None),
            // All other styles (e.g. function def, import) indicate we do have
            // a value, but it is not coming from a simple style.
            _ => ClassFieldInitialValue::Class(None),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ClassBodyInner {
    name: Identifier,
    index: ClassIndex,
    pub instance_attributes_by_method: SmallMap<Name, SmallMap<Name, InstanceAttribute>>,
}

impl ClassBodyInner {
    pub fn as_class_key(&self) -> KeyClass {
        KeyClass(ShortIdentifier::new(&self.name))
    }

    pub fn as_class_metadata_key(&self) -> KeyClassMetadata {
        KeyClassMetadata(self.index)
    }
}

/// Information about an instance attribute coming from a `self` assignment
/// in a method.
#[derive(Clone, Debug)]
pub struct InstanceAttribute(pub Binding, pub Option<Idx<KeyAnnotation>>, pub TextRange);

#[derive(Clone, Debug)]
pub struct MethodInner {
    pub name: Identifier,
    pub self_name: Option<Identifier>,
    pub instance_attributes: SmallMap<Name, InstanceAttribute>,
}

#[derive(Clone, Debug)]
pub enum ScopeKind {
    Annotation,
    ClassBody(ClassBodyInner),
    Comprehension,
    Function,
    Method(MethodInner),
    Module,
}

#[derive(Clone, Debug, Display)]
pub enum LoopExit {
    NeverRan,
    #[display("break")]
    Break,
    #[display("continue")]
    Continue,
}

/// Flow snapshots for all possible exitpoints from a loop.
#[derive(Clone, Debug)]
pub struct Loop(pub Vec<(LoopExit, Flow)>);

#[derive(Clone, Debug)]
pub struct Scope {
    pub range: TextRange,
    /// Things that are defined in this scope, statically, e.g. `x = 1` or `def f():`.
    /// Populated at the beginning before entering the scope.
    pub stat: Static,
    /// Things that are defined in this scope as they are reached.
    /// Initially starts out empty, but is populated as statements are encountered.
    /// Updated if there are multiple assignments. E.g. `x = 1; x = 2` would update the `x` binding twice.
    /// All flow bindings will have a static binding, _usually_ in this scope, but occasionally
    /// in a parent scope (e.g. for narrowing operations).
    pub flow: Flow,
    /// Are Flow types above this unreachable.
    /// Set when we enter something like a function, and can't guarantee what flow values are in scope.
    pub barrier: bool,
    pub kind: ScopeKind,
    /// Stack of for/while loops we're in. Does not include comprehensions.
    pub loops: Vec<Loop>,
}

impl Scope {
    fn new(range: TextRange, barrier: bool, kind: ScopeKind) -> Self {
        Self {
            range,
            stat: Default::default(),
            flow: Default::default(),
            barrier,
            kind,
            loops: Default::default(),
        }
    }

    pub fn annotation(range: TextRange) -> Self {
        Self::new(range, false, ScopeKind::Annotation)
    }

    pub fn class_body(range: TextRange, index: ClassIndex, name: Identifier) -> Self {
        Self::new(
            range,
            false,
            ScopeKind::ClassBody(ClassBodyInner {
                index,
                name,
                instance_attributes_by_method: SmallMap::new(),
            }),
        )
    }

    pub fn comprehension(range: TextRange) -> Self {
        Self::new(range, false, ScopeKind::Comprehension)
    }

    pub fn function(range: TextRange) -> Self {
        Self::new(range, true, ScopeKind::Function)
    }

    pub fn method(range: TextRange, name: Identifier) -> Self {
        Self::new(
            range,
            true,
            ScopeKind::Method(MethodInner {
                name,
                self_name: None,
                instance_attributes: SmallMap::new(),
            }),
        )
    }

    fn module(range: TextRange) -> Self {
        Self::new(range, false, ScopeKind::Module)
    }
}

#[derive(Clone, Debug)]
struct ScopeTreeNode {
    scope: Scope,
    children: Vec<ScopeTreeNode>,
}

impl ScopeTreeNode {
    /// Return whether we hit a child scope with a barrier
    fn visit_available_definitions(
        &self,
        table: &BindingTable,
        position: TextSize,
        visitor: &mut impl FnMut(Idx<Key>),
    ) -> bool {
        if !self.scope.range.contains(position) {
            return false;
        }
        let mut barrier = false;
        for node in &self.children {
            let hit_barrier = node.visit_available_definitions(table, position, visitor);
            barrier = barrier || hit_barrier
        }
        if !barrier {
            for info in self.scope.flow.info.values() {
                visitor(info.key);
            }
        }
        for (name, info) in &self.scope.stat.0 {
            if let Some(key) = table.types.0.key_to_idx(&info.as_key(name)) {
                visitor(key);
            }
        }
        barrier || self.scope.barrier
    }

    fn collect_available_definitions(
        &self,
        table: &BindingTable,
        position: TextSize,
        collector: &mut SmallSet<Idx<Key>>,
    ) {
        self.visit_available_definitions(table, position, &mut |key| {
            collector.insert(key);
        });
    }
}

/// Scopes keep track of the current stack of the scopes we are in.
#[derive(Clone, Debug)]
pub struct Scopes {
    scopes: Vec1<ScopeTreeNode>,
    /// When `keep_scope_tree` flag is on, the stack will maintain a tree of all the scopes
    /// throughout the program, even if the scope has already been popped. This is useful
    /// for autocomplete purposes.
    keep_scope_tree: bool,
}

impl Scopes {
    pub fn module(range: TextRange, keep_scope_tree: bool) -> Self {
        let module_scope = Scope::module(range);
        Self {
            scopes: Vec1::new(ScopeTreeNode {
                scope: module_scope,
                children: Vec::new(),
            }),
            keep_scope_tree,
        }
    }

    pub fn current(&self) -> &Scope {
        &self.scopes.last().scope
    }

    pub fn current_mut(&mut self) -> &mut Scope {
        &mut self.current_mut_node().scope
    }

    fn current_mut_node(&mut self) -> &mut ScopeTreeNode {
        self.scopes.last_mut()
    }

    /// There is only one scope remaining, return it.
    pub fn finish(self) -> ScopeTrace {
        let (a, b) = self.scopes.split_off_last();
        assert_eq!(a.len(), 0);
        ScopeTrace(b)
    }

    pub fn push(&mut self, scope: Scope) {
        self.scopes.push(ScopeTreeNode {
            scope,
            children: Vec::new(),
        });
    }

    pub fn pop(&mut self) -> Scope {
        let ScopeTreeNode { scope, children } = self.scopes.pop().unwrap();
        if self.keep_scope_tree {
            self.current_mut_node().children.push(ScopeTreeNode {
                scope: scope.clone(),
                children,
            });
        }
        scope
    }

    pub fn iter_rev(&self) -> impl ExactSizeIterator<Item = &Scope> {
        self.scopes.iter().map(|node| &node.scope).rev()
    }

    pub fn iter_rev_mut(&mut self) -> impl ExactSizeIterator<Item = &mut Scope> {
        self.scopes.iter_mut().map(|node| &mut node.scope).rev()
    }

    pub fn update_flow_info(&mut self, name: &Name, key: Idx<Key>, style: Option<FlowStyle>) {
        self.update_flow_info_hashed(Hashed::new(name), key, style);
    }

    pub fn update_flow_info_hashed(
        &mut self,
        name: Hashed<&Name>,
        key: Idx<Key>,
        style: Option<FlowStyle>,
    ) {
        self.current_mut()
            .flow
            .info
            .insert_hashed(name.cloned(), FlowInfo { key, style });
    }

    fn get_flow_info(&self, name: &Name) -> Option<&FlowInfo> {
        let name = Hashed::new(name);
        for scope in self.iter_rev() {
            if let Some(flow) = scope.flow.info.get_hashed(name) {
                return Some(flow);
            }
        }
        None
    }

    pub fn get_flow_style(&self, name: &Name) -> Option<&FlowStyle> {
        self.get_flow_info(name)
            .and_then(|info| info.style.as_ref())
    }

    pub fn get_special_entry<'a>(&'a self, name: &Name) -> Option<SpecialEntry<'a>> {
        let flow = self.get_flow_info(name)?;
        let entry = match &flow.style {
            Some(FlowStyle::Import(m, name)) => SpecialEntry::ImportName(m.dupe(), name),
            Some(FlowStyle::MergeableImport(m) | FlowStyle::ImportAs(m)) => {
                SpecialEntry::ImportModule(m.dupe())
            }
            _ => SpecialEntry::Local,
        };
        Some(entry)
    }
}

#[derive(Clone, Debug)]
pub struct ScopeTrace(ScopeTreeNode);

impl ScopeTrace {
    pub fn toplevel_scope(&self) -> &Scope {
        &self.0.scope
    }

    pub fn available_definitions(
        &self,
        table: &BindingTable,
        position: TextSize,
    ) -> SmallSet<Idx<Key>> {
        let mut collector = SmallSet::new();
        self.0
            .collect_available_definitions(table, position, &mut collector);
        collector
    }

    pub fn definition_at_position<'a>(
        &self,
        table: &'a BindingTable,
        position: TextSize,
    ) -> Option<&'a Key> {
        let mut definition = None;
        self.0
            .visit_available_definitions(table, position, &mut |idx| {
                let key = table.types.0.idx_to_key(idx);
                match key {
                    Key::Definition(short_identifier)
                        if short_identifier.range().contains_inclusive(position) =>
                    {
                        definition = Some(key);
                    }
                    _ => {}
                }
            });
        definition
    }
}
