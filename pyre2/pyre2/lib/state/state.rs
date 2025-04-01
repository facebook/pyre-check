/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Clippy thinks ModuleData is mutable (it is), but the ArcId makes the Hash/Eq immutable
#![allow(clippy::mutable_key_type)]

use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Display;
use std::fs::File;
use std::io::BufWriter;
use std::io::Write;
use std::mem;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::MutexGuard;
use std::sync::RwLockReadGuard;
use std::time::Instant;

use dupe::Dupe;
use enum_iterator::Sequence;
use ruff_python_ast::name::Name;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;
use starlark_map::Hashed;
use tracing::debug;
use tracing::error;
use tracing::info;

use crate::alt::answers::AnswerEntry;
use crate::alt::answers::AnswerTable;
use crate::alt::answers::Answers;
use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::answers::Solutions;
use crate::alt::answers::SolutionsEntry;
use crate::alt::answers::SolutionsTable;
use crate::alt::traits::Solve;
use crate::binding::binding::KeyExport;
use crate::binding::binding::Keyed;
use crate::binding::bindings::BindingEntry;
use crate::binding::bindings::BindingTable;
use crate::binding::bindings::Bindings;
use crate::binding::table::TableKeyed;
use crate::config::ErrorConfigs;
use crate::error::kind::ErrorKind;
use crate::export::exports::ExportLocation;
use crate::export::exports::Exports;
use crate::export::exports::LookupExport;
use crate::metadata::RuntimeMetadata;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::report::debug_info::DebugInfo;
use crate::state::dirty::Dirty;
use crate::state::epoch::Epoch;
use crate::state::epoch::Epochs;
use crate::state::handle::Handle;
use crate::state::load::Load;
use crate::state::load::Loads;
use crate::state::loader::FindError;
use crate::state::loader::Loader;
use crate::state::loader::LoaderFindCache;
use crate::state::loader::LoaderId;
use crate::state::require::Require;
use crate::state::require::RequireDefault;
use crate::state::require::RequireOverride;
use crate::state::steps::Context;
use crate::state::steps::Step;
use crate::state::steps::Steps;
use crate::state::subscriber::Subscriber;
use crate::types::class::Class;
use crate::types::stdlib::Stdlib;
use crate::types::types::Type;
use crate::util::arc_id::ArcId;
use crate::util::lock::Mutex;
use crate::util::lock::RwLock;
use crate::util::locked_map::LockedMap;
use crate::util::no_hash::BuildNoHash;
use crate::util::prelude::SliceExt;
use crate::util::recurser::Recurser;
use crate::util::task_heap::TaskHeap;
use crate::util::thread_pool::ThreadPool;
use crate::util::uniques::UniqueFactory;
use crate::util::upgrade_lock::UpgradeLock;
use crate::util::upgrade_lock::UpgradeLockExclusiveGuard;
use crate::util::upgrade_lock::UpgradeLockWriteGuard;

/// `ModuleDataSnapshot` is a snapshot of `ArcId<ModuleData>` in the main state.
/// The snapshot is readonly most of the times. It will only be overwritten with updated information
/// from `Transaction` when we decide to commit a `Transaction` into the main state.
#[derive(Debug)]
struct ModuleDataSnapshot {
    handle: Handle,
    state: ModuleState,
    deps: HashMap<ModuleName, Handle, BuildNoHash>,
    rdeps: HashSet<Handle>,
}

#[derive(Debug)]
struct ModuleData {
    handle: Handle,
    state: UpgradeLock<Step, ModuleState>,
    deps: RwLock<HashMap<ModuleName, Handle, BuildNoHash>>,
    /// The reverse dependencies of this module. This is used to invalidate on change.
    /// Note that if we are only running once, e.g. on the command line, this isn't valuable.
    /// But we create it anyway for simplicity, since it doesn't seem to add much overhead.
    rdeps: Mutex<HashSet<Handle>>,
}

#[derive(Debug, Clone)]
struct ModuleState {
    require: RequireOverride,
    epochs: Epochs,
    dirty: Dirty,
    steps: Steps,
}

impl ModuleData {
    fn new(handle: Handle, now: Epoch) -> Self {
        Self {
            handle,
            state: UpgradeLock::new(ModuleState {
                require: Default::default(),
                epochs: Epochs::new(now),
                dirty: Dirty::default(),
                steps: Steps::default(),
            }),
            deps: Default::default(),
            rdeps: Default::default(),
        }
    }

    fn from_snapshot(value: &ModuleDataSnapshot) -> Self {
        ModuleData {
            handle: value.handle.dupe(),
            state: UpgradeLock::new(value.state.clone()),
            deps: RwLock::new(value.deps.clone()),
            rdeps: Mutex::new(value.rdeps.clone()),
        }
    }

    fn to_snapshot(&self) -> ModuleDataSnapshot {
        let ModuleData {
            handle,
            state,
            deps,
            rdeps,
        } = self;
        let deps = mem::take(&mut *deps.write());
        let rdeps = mem::take(&mut *rdeps.lock());
        let state = state.read().clone();
        ModuleDataSnapshot {
            handle: handle.dupe(),
            state,
            deps,
            rdeps,
        }
    }
}

/// A subset of State that contains readable information for various systems (e.g. IDE, error reporting, etc).
pub struct ReadableState {
    stdlib: SmallMap<(RuntimeMetadata, LoaderId), Arc<Stdlib>>,
    modules: HashMap<Handle, ModuleDataSnapshot>,
    loaders: SmallMap<LoaderId, Arc<LoaderFindCache<LoaderId>>>,
    /// The current epoch, gets incremented every time we recompute
    now: Epoch,
    require: RequireDefault,
}

impl ReadableState {
    fn new() -> Self {
        Self {
            stdlib: Default::default(),
            modules: Default::default(),
            loaders: Default::default(),
            now: Epoch::zero(),
            // Will be overwritten with a new default before is it used.
            require: RequireDefault::new(Require::Exports),
        }
    }

    pub fn handles(&self) -> Vec<Handle> {
        self.modules.keys().cloned().collect()
    }

    pub fn get_bindings(&self, handle: &Handle) -> Option<Bindings> {
        self.modules
            .get(handle)?
            .state
            .steps
            .answers
            .as_ref()
            .map(|x| x.0.dupe())
    }

    pub fn get_answers(&self, handle: &Handle) -> Option<Arc<Answers>> {
        self.modules
            .get(handle)?
            .state
            .steps
            .answers
            .as_ref()
            .map(|x| x.1.dupe())
    }

    pub fn get_load(&self, handle: &Handle) -> Option<Arc<Load>> {
        self.modules.get(handle)?.state.steps.load.dupe()
    }

    pub fn get_loads<'a>(&self, handles: impl IntoIterator<Item = &'a Handle>) -> Loads {
        Loads::new(handles.into_iter().filter_map(|handle| {
            self.modules
                .get(handle)?
                .state
                .steps
                .load
                .as_ref()
                .map(|x| x.dupe())
        }))
    }

    pub fn get_module_info(&self, handle: &Handle) -> Option<ModuleInfo> {
        self.get_load(handle).map(|x| x.module_info.dupe())
    }

    pub fn get_ast(&self, handle: &Handle) -> Option<Arc<ruff_python_ast::ModModule>> {
        self.modules.get(handle)?.state.steps.ast.dupe()
    }

    #[allow(dead_code)]
    pub fn get_solutions(&self, handle: &Handle) -> Option<Arc<Solutions>> {
        Some(
            self.modules
                .get(handle)?
                .state
                .steps
                .solutions
                .as_ref()?
                .dupe(),
        )
    }

    pub fn debug_info(&self, handles: &[Handle], error_configs: &ErrorConfigs) -> DebugInfo {
        let owned: Vec<(Arc<Load>, Arc<(Bindings, Arc<Answers>)>, Arc<Solutions>)> =
            handles.map(|x| {
                let module = self.modules.get(x).unwrap();
                let steps = &module.state;
                (
                    steps.steps.load.dupe().unwrap(),
                    steps.steps.answers.dupe().unwrap(),
                    steps.steps.solutions.dupe().unwrap(),
                )
            });
        DebugInfo::new(
            &owned.map(|x| (&x.0.module_info, &x.0.errors, &x.1.0, &*x.2)),
            error_configs,
        )
    }

    pub fn module_count(&self) -> usize {
        self.modules.len()
    }

    pub fn line_count(&self) -> usize {
        self.modules
            .values()
            .map(|x| {
                x.state
                    .steps
                    .load
                    .as_ref()
                    .map_or(0, |x| x.module_info.line_count())
            })
            .sum()
    }
}

/// The changes that happen during the transactions, which should either be completely discarded
/// or committed together in the end.
pub struct TransactionChanges<'a> {
    stdlib: SmallMap<(RuntimeMetadata, LoaderId), Arc<Stdlib>>,
    updated_modules: LockedMap<Handle, ArcId<ModuleData>>,
    additional_loaders: SmallMap<LoaderId, Arc<LoaderFindCache<LoaderId>>>,
    /// The current epoch, gets incremented every time we recompute
    now: Epoch,
    require: RequireDefault,
    committing_transaction_guard: MutexGuard<'a, ()>,
}

/// `Transaction` is a collection of state that's only relevant during a type checking job.
/// Most importantly, it holds `updated_modules`, which contains module information that are copied
/// over from main state, potentially with updates as a result of recheck.
/// At the end of a check, the updated modules information can be committed back to the main `State`
/// in a transaction.
pub struct Transaction<'a> {
    threads: &'a ThreadPool,
    uniques: &'a UniqueFactory,
    readable: RwLockReadGuard<'a, ReadableState>,
    stdlib: SmallMap<(RuntimeMetadata, LoaderId), Arc<Stdlib>>,
    updated_modules: LockedMap<Handle, ArcId<ModuleData>>,
    additional_loaders: SmallMap<LoaderId, Arc<LoaderFindCache<LoaderId>>>,
    /// The current epoch, gets incremented every time we recompute
    now: Epoch,
    require: RequireDefault,
    /// Items we still need to process. Stored in a max heap, so that
    /// the highest step (the module that is closest to being finished)
    /// gets picked first, ensuring we release its memory quickly.
    todo: TaskHeap<Step, ArcId<ModuleData>>,
    /// Values whose solutions changed value since the last time we recomputed
    changed: Mutex<Vec<ArcId<ModuleData>>>,
    /// Handles which are dirty
    dirty: Mutex<SmallSet<ArcId<ModuleData>>>,
    /// Thing to tell about each action.
    subscriber: Option<Box<dyn Subscriber>>,
}

impl<'a> Transaction<'a> {
    pub fn readable(&self) -> &ReadableState {
        &self.readable
    }

    pub fn import_handle(&self, handle: &Handle, module: ModuleName) -> Result<Handle, FindError> {
        Ok(Handle::new(
            module,
            self.get_cached_find_dependency(handle.loader(), module)?,
            handle.config().dupe(),
            handle.loader().dupe(),
        ))
    }

    fn clean(
        &self,
        module_data: &ArcId<ModuleData>,
        exclusive: UpgradeLockExclusiveGuard<Step, ModuleState>,
    ) {
        // We need to clean up the state.
        // If things have changed, we need to update the last_step.
        // We clear memory as an optimisation only.

        // Mark ourselves as having completed everything.
        let finish = |w: &mut ModuleState| {
            w.epochs.checked = self.now;
            w.dirty.clean();
        };
        // Rebuild stuff. Pass clear_ast to indicate we need to rebuild the AST, otherwise can reuse it (if present).
        let rebuild = |mut w: UpgradeLockWriteGuard<Step, ModuleState>, clear_ast: bool| {
            w.steps.last_step = if clear_ast || w.steps.ast.is_none() {
                if w.steps.load.is_none() {
                    None
                } else {
                    Some(Step::Load)
                }
            } else {
                Some(Step::Ast)
            };
            if clear_ast {
                w.steps.ast = None;
            }
            w.steps.exports = None;
            w.steps.answers = None;
            // Do not clear solutions, since we can use that for equality
            w.epochs.computed = self.now;
            if let Some(subscriber) = &self.subscriber {
                subscriber.start_work(module_data.handle.dupe());
            }
            let deps = mem::take(&mut *module_data.deps.write());
            finish(&mut w);
            if !deps.is_empty() {
                // Downgrade to exclusive, so other people can read from us, or we lock up.
                // But don't give up the lock entirely, so we don't recompute anything
                let _exclusive = w.exclusive();
                for dep_handle in deps.values() {
                    let removed = self
                        .get_module(dep_handle)
                        .rdeps
                        .lock()
                        .remove(&module_data.handle);
                    assert!(removed);
                }
            }
        };

        if exclusive.dirty.require {
            // We have increased the `Require` level, so redo everything to make sure
            // we capture everything.
            // Could be optimised to do less work (e.g. if you had Retain::Error before don't need to reload)
            let mut write = exclusive.write();
            write.steps.load = None;
            rebuild(write, true);
            return;
        }

        // Validate the load flag.
        if exclusive.dirty.load
            && let Some(old_load) = exclusive.steps.load.dupe()
        {
            let (code, self_error) =
                Load::load_from_path(module_data.handle.path(), module_data.handle.loader());
            if self_error.is_some() || &code != old_load.module_info.contents() {
                let mut write = exclusive.write();
                write.steps.load = Some(Arc::new(Load::load_from_data(
                    module_data.handle.module(),
                    module_data.handle.path().dupe(),
                    old_load.errors.style(),
                    code,
                    self_error,
                )));
                rebuild(write, true);
                return;
            }
            // The contents are the same, so we can just reuse the old load
        }

        if exclusive.dirty.deps {
            let write = exclusive.write();
            rebuild(write, false);
            return;
        }

        // Validate the find flag.
        if exclusive.dirty.find {
            let loader = self.get_cached_loader(module_data.handle.loader());
            let mut is_dirty = false;
            for dependency_handle in module_data.deps.read().values() {
                match loader.find_import(dependency_handle.module()) {
                    Ok(path) if &path == dependency_handle.path() => {}
                    _ => {
                        is_dirty = true;
                        break;
                    }
                }
            }
            if is_dirty {
                let write = exclusive.write();
                rebuild(write, false);
                return;
            }
        }

        // The module was not dirty. Make sure our dependencies aren't dirty either.
        let mut write = exclusive.write();
        finish(&mut write);
    }

    fn demand(&self, module_data: &ArcId<ModuleData>, step: Step) {
        let mut computed = false;
        loop {
            let reader = module_data.state.read();
            if reader.epochs.checked != self.now {
                if let Some(ex) = reader.exclusive(Step::first()) {
                    self.clean(module_data, ex);
                    // We might have done some cleaning
                    computed = true;
                }
                continue;
            }

            let todo = match reader.steps.next_step() {
                Some(todo) if todo <= step => todo,
                _ => break,
            };
            let mut exclusive = match reader.exclusive(todo) {
                Some(exclusive) => exclusive,
                None => {
                    // The world changed, we should check again
                    continue;
                }
            };
            let todo = match exclusive.steps.next_step() {
                Some(todo) if todo <= step => todo,
                _ => break,
            };

            computed = true;
            let compute = todo.compute().0(&exclusive.steps);
            let require = exclusive.require.get(self.require);
            if todo == Step::Answers && !require.keep_ast() {
                // We have captured the Ast, and must have already built Exports (we do it serially),
                // so won't need the Ast again.
                let to_drop;
                let mut writer = exclusive.write();
                to_drop = writer.steps.ast.take();
                exclusive = writer.exclusive();
                drop(to_drop);
            }

            let stdlib = self.get_stdlib(&module_data.handle);
            let loader = self.get_cached_loader(module_data.handle.loader());
            let set = compute(&Context {
                require,
                module: module_data.handle.module(),
                path: module_data.handle.path(),
                config: module_data.handle.config(),
                loader: &*loader,
                uniques: self.uniques,
                stdlib: &stdlib,
                lookup: &self.lookup(module_data.dupe()),
            });
            {
                let mut changed = false;
                let mut to_drop = None;
                let mut writer = exclusive.write();
                let mut load_result = None;
                let old_solutions = if todo == Step::Solutions {
                    writer.steps.solutions.take()
                } else {
                    None
                };
                set(&mut writer.steps);
                if todo == Step::Solutions {
                    if let Some(old) = old_solutions.as_ref()
                        && let Some(new) = writer.steps.solutions.as_ref()
                        && let Some(difference) = old.first_difference(new)
                    {
                        debug!(
                            "Exports changed for `{}`: {difference}",
                            module_data.handle.module(),
                        );
                        changed = true;
                        writer.epochs.changed = self.now;
                    }
                    if !require.keep_bindings() && !require.keep_answers() {
                        // From now on we can use the answers directly, so evict the bindings/answers.
                        to_drop = writer.steps.answers.take();
                    }
                    load_result = writer.steps.load.dupe();
                }
                drop(writer);
                // Release the lock before dropping
                drop(to_drop);
                if changed {
                    self.changed.lock().push(module_data.dupe());
                    let mut dirtied = Vec::new();
                    for x in module_data
                        .rdeps
                        .lock()
                        .iter()
                        .map(|handle| self.get_module(handle))
                    {
                        loop {
                            let reader = x.state.read();
                            if reader.epochs.computed == self.now || reader.dirty.deps {
                                // Either doesn't need setting, or already set
                                break;
                            }
                            // This can potentially race with `clean`, so make sure we use the `last` as our exclusive key,
                            // which importantly is a different key to the `first` that `clean` uses.
                            // Slight risk of a busy-loop, but better than a deadlock.
                            if let Some(exclusive) = reader.exclusive(Step::last()) {
                                if exclusive.epochs.computed == self.now || exclusive.dirty.deps {
                                    break;
                                }
                                dirtied.push(x.dupe());
                                let mut writer = exclusive.write();
                                writer.dirty.deps = true;
                                break;
                            }
                            // continue around the loop - failed to get the lock, but we really want it
                        }
                    }
                    self.dirty.lock().extend(dirtied);
                }
                if let Some(load) = load_result
                    && let Some(subscriber) = &self.subscriber
                {
                    subscriber.finish_work(module_data.handle.dupe(), load);
                }
            }
            if todo == step {
                break; // Fast path - avoid asking again since we just did it.
            }
        }
        if computed && let Some(next) = step.next() {
            // For a large benchmark, LIFO is 10Gb retained, FIFO is 13Gb.
            // Perhaps we are getting to the heart of the graph with LIFO?
            self.todo.push_lifo(next, module_data.dupe());
        }
    }

    fn get_module(&self, handle: &Handle) -> ArcId<ModuleData> {
        self.get_module_ex(handle).0
    }

    /// Return the module, plus true if the module was newly created.
    fn get_module_ex(&self, handle: &Handle) -> (ArcId<ModuleData>, bool) {
        let mut created = None;
        let res = self
            .updated_modules
            .ensure(handle, || {
                if let Some(m) = self.readable.modules.get(handle) {
                    ArcId::new(ModuleData::from_snapshot(m))
                } else {
                    let res = ArcId::new(ModuleData::new(handle.dupe(), self.now));
                    created = Some(res.dupe());
                    res
                }
            })
            .dupe();
        // Due to race conditions, we might create two ModuleData, but only the first is returned.
        // Figure out if we won the race, and thus are the person who actually did the creation.
        let created = Some(&res) == created.as_ref();
        if created && let Some(subscriber) = &self.subscriber {
            subscriber.start_work(handle.dupe());
        }
        (res, created)
    }

    fn add_error(
        &self,
        module_data: &ArcId<ModuleData>,
        range: TextRange,
        msg: String,
        kind: ErrorKind,
    ) {
        let load = module_data.state.read().steps.load.dupe().unwrap();
        load.errors.add(range, msg, kind, None);
    }

    fn lookup<'b>(&'b self, module_data: ArcId<ModuleData>) -> TransactionHandle<'b> {
        TransactionHandle {
            transaction: self,
            module_data,
        }
    }

    fn lookup_stdlib(&self, handle: &Handle, name: &Name) -> Option<Class> {
        let module_data = self.get_module(handle);
        if !self
            .lookup_export(&module_data)
            .exports(&self.lookup(module_data.dupe()))
            .contains_key(name)
        {
            self.add_error(
                &module_data,
                TextRange::default(),
                format!(
                    "Stdlib import failure, was expecting `{}` to contain `{name}`",
                    module_data.handle.module()
                ),
                ErrorKind::MissingModuleAttribute,
            );
            return None;
        }

        let t = self.lookup_answer(module_data.dupe(), &KeyExport(name.clone()));
        match t.arc_clone() {
            Type::ClassDef(cls) => Some(cls),
            ty => {
                self.add_error(
                    &module_data,
                    TextRange::default(),
                    format!(
                        "Did not expect non-class type `{ty}` for stdlib import `{}.{name}`",
                        module_data.handle.module()
                    ),
                    ErrorKind::MissingModuleAttribute,
                );
                None
            }
        }
    }

    fn lookup_export(&self, module_data: &ArcId<ModuleData>) -> Exports {
        self.demand(module_data, Step::Exports);
        let lock = module_data.state.read();
        lock.steps.exports.dupe().unwrap()
    }

    fn lookup_answer<'b, K: Solve<TransactionHandle<'b>> + Keyed<EXPORTED = true>>(
        &'b self,
        module_data: ArcId<ModuleData>,
        key: &K,
    ) -> Arc<<K as Keyed>::Answer>
    where
        AnswerTable: TableKeyed<K, Value = AnswerEntry<K>>,
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
        SolutionsTable: TableKeyed<K, Value = SolutionsEntry<K>>,
    {
        let key = Hashed::new(key);

        // Either: We have solutions (use that), or we have answers (calculate that), or we have none (demand and try again)
        // Check; demand; check - the second check is guaranteed to work.
        for _ in 0..2 {
            let lock = module_data.state.read();
            if let Some(solutions) = &lock.steps.solutions {
                return solutions.get_hashed(key).unwrap().dupe();
            } else if let Some(answers) = &lock.steps.answers {
                let load = lock.steps.load.dupe().unwrap();
                let answers = answers.dupe();
                drop(lock);
                let stdlib = self.get_stdlib(&module_data.handle);
                let lookup = self.lookup(module_data);
                return answers.1.solve_exported_key(
                    &lookup,
                    &lookup,
                    &answers.0,
                    &load.errors,
                    &stdlib,
                    self.uniques,
                    key,
                );
            }
            drop(lock);
            self.demand(&module_data, Step::Answers);
        }
        unreachable!("We demanded the answers, either answers or solutions should be present");
    }

    fn get_cached_find_dependency(
        &self,
        loader: &LoaderId,
        module: ModuleName,
    ) -> Result<ModulePath, FindError> {
        self.get_cached_loader(loader).find_import(module)
    }

    fn get_cached_loader(&self, loader: &LoaderId) -> Arc<LoaderFindCache<LoaderId>> {
        if self.readable.loaders.len() == 1 {
            // Since we know our one must exist, we can shortcut
            return self.readable.loaders.first().unwrap().1.dupe();
        }

        if let Some(loader) = self.additional_loaders.get(loader) {
            return loader.dupe();
        }
        // Safe because we always fill these in before starting
        self.readable.loaders.get(loader).unwrap().dupe()
    }

    fn get_stdlib(&self, handle: &Handle) -> Arc<Stdlib> {
        if self.readable.stdlib.len() == 1 {
            // Since we know our one must exist, we can shortcut
            return self.readable.stdlib.first().unwrap().1.dupe();
        }

        if let Some(stdlib) = self
            .stdlib
            .get(&(handle.config().dupe(), handle.loader().dupe()))
        {
            stdlib.dupe()
        } else {
            // Safe because we always run compute_stdlib first
            self.readable
                .stdlib
                .get(&(handle.config().dupe(), handle.loader().dupe()))
                .unwrap()
                .dupe()
        }
    }

    fn compute_stdlib(&mut self, configs: SmallSet<(RuntimeMetadata, LoaderId)>) {
        self.stdlib = configs
            .iter()
            .map(|k| (k.dupe(), Arc::new(Stdlib::for_bootstrapping())))
            .collect();
        let stdlibs = configs
            .iter()
            .map(|(c, l)| {
                (
                    (c.dupe(), l.dupe()),
                    Arc::new(Stdlib::new(c.version(), |module, name| {
                        let path = self.get_cached_find_dependency(l, module).ok()?;
                        self.lookup_stdlib(&Handle::new(module, path, c.dupe(), l.dupe()), name)
                    })),
                )
            })
            .collect();
        self.stdlib = stdlibs;
    }

    fn work(&self) {
        // ensure we have answers for everything, keep going until we don't discover any new modules
        self.todo.work(|_, x| {
            self.demand(&x, Step::last());
        });
    }

    fn run_step(&mut self, handles: &[(Handle, Require)], old_require: Option<RequireDefault>) {
        self.now.next();
        let configs = handles
            .iter()
            .map(|(x, _)| (x.config().dupe(), x.loader().dupe()))
            .collect::<SmallSet<_>>();
        self.compute_stdlib(configs);

        {
            let dirty = mem::take(&mut *self.dirty.lock());
            for (h, r) in handles {
                let (m, created) = self.get_module_ex(h);
                let mut state = m.state.write(Step::first()).unwrap();
                let dirty_require = match old_require {
                    None => false,
                    _ if created => false,
                    Some(old_require) => state.require.get(old_require) < *r,
                };
                state.dirty.require = dirty_require || state.dirty.require;
                state.require.set(self.require, *r);
                drop(state);
                if (created || dirty_require) && !dirty.contains(&m) {
                    self.todo.push_fifo(Step::first(), m);
                }
            }
            for x in dirty {
                self.todo.push_fifo(Step::first(), x);
            }
        }

        self.threads.spawn_many(|| self.work());
    }

    fn ensure_loaders(&mut self, handles: &[(Handle, Require)]) {
        for (h, _) in handles {
            if !self.readable.loaders.contains_key(h.loader()) {
                self.additional_loaders.insert(
                    h.loader().dupe(),
                    Arc::new(LoaderFindCache::new(h.loader().dupe())),
                );
            }
        }
    }

    fn invalidate_rdeps(&mut self, changed: &[ArcId<ModuleData>]) {
        // We need to invalidate all modules that depend on anything in changed, including transitively.
        fn f(
            state: &Transaction,
            dirty_handles: &mut SmallMap<Handle, Option<ArcId<ModuleData>>>,
            stack: &mut HashSet<Handle>,
            x: &ModuleDataSnapshot,
        ) -> bool {
            if let Some(res) = dirty_handles.get(&x.handle) {
                res.is_some()
            } else if stack.contains(&x.handle) {
                // Recursive hypothesis - do not write to dirty
                false
            } else {
                stack.insert(x.handle.dupe());
                let res = x.deps.values().any(|y| {
                    f(
                        state,
                        dirty_handles,
                        stack,
                        state.readable.modules.get(y).unwrap(),
                    )
                });
                stack.remove(&x.handle);
                dirty_handles.insert(
                    x.handle.dupe(),
                    if res {
                        Some(state.get_module(&x.handle))
                    } else {
                        None
                    },
                );
                res
            }
        }

        let mut dirty_handles = changed
            .iter()
            .map(|x| (x.handle.dupe(), Some(self.get_module(&x.handle))))
            .collect::<SmallMap<_, _>>();
        let mut stack = HashSet::new();
        for x in self.readable.modules.values() {
            f(self, &mut dirty_handles, &mut stack, x);
        }

        let mut dirty_set: std::sync::MutexGuard<'_, SmallSet<ArcId<ModuleData>>> =
            self.dirty.lock();
        for (_, dirty_module_data) in dirty_handles {
            if let Some(x) = dirty_module_data {
                x.state.write(Step::Load).unwrap().dirty.deps = true;
                dirty_set.insert(x);
            }
        }
    }

    fn run_internal(&mut self, handles: &[(Handle, Require)], old_require: RequireDefault) {
        // We first compute all the modules that are either new or have changed.
        // Then we repeatedly compute all the modules who depend on modules that changed.
        // To ensure we guarantee termination, and don't endure more than a linear overhead,
        // if we end up spotting the same module changing twice, we just invalidate
        // everything in the cycle and force it to compute.
        let mut changed_twice = SmallSet::new();

        self.ensure_loaders(handles);
        for i in 1.. {
            debug!("Running epoch {i}");
            // The first version we use the old require. We use this to trigger require changes,
            // but only once, as after we've done it once, the "old" value will no longer be accessible.
            self.run_step(handles, if i == 1 { Some(old_require) } else { None });
            let changed = mem::take(&mut *self.changed.lock());
            if changed.is_empty() {
                return;
            }
            for c in &changed {
                if !changed_twice.insert(c.dupe()) {
                    debug!("Mutable dependency cycle, invalidating the cycle");
                    // We are in a cycle of mutual dependencies, so give up.
                    // Just invalidate everything in the cycle and recompute it all.
                    self.invalidate_rdeps(&changed);
                    self.run_step(handles, None);
                    return;
                }
            }
        }
    }

    pub fn ad_hoc_solve<R: Sized, F: FnOnce(AnswersSolver<TransactionHandle>) -> R>(
        &self,
        handle: &Handle,
        solve: F,
    ) -> Option<R> {
        let module_data = self.get_module(handle);
        let lookup = self.lookup(module_data.dupe());
        let steps = &module_data.state.read().steps;
        let errors = &steps.load.as_ref()?.errors;
        let (bindings, answers) = steps.answers.as_deref().as_ref()?;
        let stdlib = self.get_stdlib(handle);
        let recurser = Recurser::new();
        let solver = AnswersSolver::new(
            &lookup,
            answers,
            errors,
            bindings,
            &lookup,
            self.uniques,
            &recurser,
            &stdlib,
        );
        let result = solve(solver);
        Some(result)
    }

    /// Called if the `find` portion of loading might have changed.
    /// E.g. you have include paths, and a new file appeared earlier on the path.
    pub fn invalidate_find(&mut self, loader: &LoaderId) {
        if let Some(cache) = self.additional_loaders.get_mut(loader) {
            *cache = Arc::new(LoaderFindCache::new(loader.dupe()));
        } else if self.readable.loaders.contains_key(loader) {
            self.additional_loaders
                .insert(loader.dupe(), Arc::new(LoaderFindCache::new(loader.dupe())));
        }
        let mut dirty_set = self.dirty.lock();
        for handle in self.readable.modules.keys() {
            if handle.loader() == loader {
                let module_data = self.get_module(handle);
                module_data.state.write(Step::Load).unwrap().dirty.find = true;
                dirty_set.insert(module_data.dupe());
            }
        }
    }

    /// Called if the `load_from_memory` portion of loading might have changed.
    /// Specify which in-memory files might have changed.
    pub fn invalidate_memory(&mut self, loader: LoaderId, files: &[PathBuf]) {
        let files = files
            .iter()
            .map(|x| ModulePath::memory(x.clone()))
            .collect::<SmallSet<_>>();
        let mut dirty_set = self.dirty.lock();
        for handle in self.readable.modules.keys() {
            if handle.loader() == &loader && files.contains(handle.path()) {
                let module_data = self.get_module(handle);
                module_data.state.write(Step::Load).unwrap().dirty.load = true;
                dirty_set.insert(module_data.dupe());
            }
        }
    }

    /// Called if the files read from the disk might have changed.
    /// Specify which files might have changed.
    /// You must use the same absolute/relative paths as were given by `find`.
    pub fn invalidate_disk(&mut self, files: &[PathBuf]) {
        // We create the set out of ModulePath as it allows us to reuse the fact `ModulePath` has cheap hash
        // when checking the modules.
        let files = files
            .iter()
            .map(|x| ModulePath::filesystem(x.clone()))
            .collect::<SmallSet<_>>();
        let mut dirty_set = self.dirty.lock();
        for handle in self.readable.modules.keys() {
            if files.contains(handle.path()) {
                let module_data = self.get_module(handle);
                module_data.state.write(Step::Load).unwrap().dirty.load = true;
                dirty_set.insert(module_data.dupe());
            }
        }
    }

    pub fn report_timings(
        &self,
        path: &Path,
        subscriber: Option<Box<dyn Subscriber>>,
    ) -> anyhow::Result<()> {
        let mut file = BufWriter::new(File::create(path)?);
        writeln!(file, "Module,Step,Seconds")?;
        file.flush()?;
        if let Some(subscriber) = &subscriber {
            for m in self.readable.modules.values() {
                subscriber.start_work(m.handle.dupe());
            }
        }
        let mut timings: SmallMap<String, f32> = SmallMap::new();
        for m in self.readable.modules.values() {
            let mut write = |step: &dyn Display, start: Instant| -> anyhow::Result<()> {
                let duration = start.elapsed().as_secs_f32();
                let step = step.to_string();
                writeln!(file, "{},{step},{duration}", m.handle.module())?;
                // Always flush, so if a user aborts we get the timings thus-far
                file.flush()?;
                *timings.entry(step).or_default() += duration;
                Ok(())
            };

            let m = self.get_module(&m.handle);
            let mut alt = Steps::default();
            let lock = m.state.read();
            let stdlib = self.get_stdlib(&m.handle);
            let loader = self.get_cached_loader(m.handle.loader());
            let ctx = Context {
                require: lock.require.get(self.require),
                module: m.handle.module(),
                path: m.handle.path(),
                config: m.handle.config(),
                loader: &*loader,
                uniques: self.uniques,
                stdlib: &stdlib,
                lookup: &self.lookup(m.dupe()),
            };
            let mut step = Step::Load; // Start at AST (Load.next)
            alt.load = lock.steps.load.dupe();
            while let Some(s) = step.next() {
                step = s;
                let start = Instant::now();
                step.compute().0(&alt)(&ctx)(&mut alt);
                write(&step, start)?;
                if step == Step::Exports {
                    let start = Instant::now();
                    let exports = alt.exports.as_ref().unwrap();
                    exports.wildcard(ctx.lookup);
                    exports.exports(ctx.lookup);
                    write(&"Exports-force", start)?;
                }
            }
            let start = Instant::now();
            let diff = lock
                .steps
                .solutions
                .as_ref()
                .unwrap()
                .first_difference(alt.solutions.as_deref().unwrap());
            write(&"Diff", start)?;
            if false {
                // Disabled code, but super useful for debugging differences
                if let Some(diff) = diff {
                    error!("Not deterministic {}: {}", m.handle.module(), diff)
                }
            }
            if let Some(subscriber) = &self.subscriber {
                subscriber.finish_work(m.handle.dupe(), alt.load.unwrap().dupe());
            }
        }
        for (step, duration) in timings {
            info!("Step {step} took {duration:.3} seconds");
        }
        Ok(())
    }

    pub fn get_exports(&self, handle: &Handle) -> Arc<SmallMap<Name, ExportLocation>> {
        let module_data = self.get_module(handle);
        self.lookup_export(&module_data)
            .exports(&self.lookup(module_data))
    }
}

pub struct TransactionHandle<'a> {
    transaction: &'a Transaction<'a>,
    module_data: ArcId<ModuleData>,
}

impl<'a> TransactionHandle<'a> {
    fn get_module(&self, module: ModuleName) -> Result<ArcId<ModuleData>, FindError> {
        if let Some(res) = self.module_data.deps.read().get(&module) {
            return Ok(self.transaction.get_module(res));
        }

        let handle = self
            .transaction
            .import_handle(&self.module_data.handle, module)?;
        let res = self.transaction.get_module(&handle);
        let mut write = self.module_data.deps.write();
        let did_insert = write.insert(module, handle).is_none();
        drop(write);
        if did_insert {
            res.rdeps.lock().insert(self.module_data.handle.dupe());
        }
        Ok(res)
    }
}

impl<'a> LookupExport for TransactionHandle<'a> {
    fn get(&self, module: ModuleName) -> Result<Exports, FindError> {
        Ok(self.transaction.lookup_export(&self.get_module(module)?))
    }
}

impl<'a> LookupAnswer for TransactionHandle<'a> {
    fn get<K: Solve<Self> + Keyed<EXPORTED = true>>(
        &self,
        module: ModuleName,
        k: &K,
    ) -> Arc<K::Answer>
    where
        AnswerTable: TableKeyed<K, Value = AnswerEntry<K>>,
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
        SolutionsTable: TableKeyed<K, Value = SolutionsEntry<K>>,
    {
        // The unwrap is safe because we must have said there were no exports,
        // so no one can be trying to get at them
        let module_data = self.get_module(module).unwrap();
        self.transaction.lookup_answer(module_data, k)
    }
}

/// A checking state that will eventually commit.
/// `State` will ensure that at most one of them can exist.
pub struct CommittingTransaction<'a> {
    transaction: Transaction<'a>,
    committing_transaction_guard: MutexGuard<'a, ()>,
}

impl<'a> AsMut<Transaction<'a>> for CommittingTransaction<'a> {
    fn as_mut(&mut self) -> &mut Transaction<'a> {
        &mut self.transaction
    }
}

impl<'a> CommittingTransaction<'a> {
    pub fn into_changes(self) -> TransactionChanges<'a> {
        let CommittingTransaction {
            transaction:
                Transaction {
                    readable,
                    stdlib,
                    updated_modules,
                    additional_loaders,
                    now,
                    require,
                    ..
                },
            committing_transaction_guard,
        } = self;
        // Drop the read lock the transaction holds.
        drop(readable);
        TransactionChanges {
            stdlib,
            updated_modules,
            additional_loaders,
            now,
            require,
            committing_transaction_guard,
        }
    }

    #[cfg(test)]
    pub fn set_subscriber(&mut self, subscriber: Option<Box<dyn Subscriber>>) {
        self.transaction.subscriber = subscriber;
    }
}

/// `State` coordinates between potential parallel operations over itself.
/// It enforces that
/// 1. There can be at most one ongoing recheck that can eventually commit.
/// 2. All the reads over the state are reads over a consistent view
///    (i.e. it won't observe a mix of state between different epochs),
///    which is enforced by
///
///     1. There can be as many concurrent reads over state as possible,
///        but they will block committing.
///     2. During the commiting of `Transaction`, all reads will be blocked.
pub struct State {
    threads: ThreadPool,
    uniques: UniqueFactory,
    state: RwLock<ReadableState>,
    committing_transaction_lock: Mutex<()>,
}

impl State {
    pub fn new() -> Self {
        Self {
            threads: ThreadPool::new(),
            uniques: UniqueFactory::new(),
            state: RwLock::new(ReadableState::new()),
            committing_transaction_lock: Mutex::new(()),
        }
    }

    pub fn new_transaction<'a>(
        &'a self,
        require: Require,
        subscriber: Option<Box<dyn Subscriber>>,
    ) -> Transaction<'a> {
        Transaction {
            threads: &self.threads,
            uniques: &self.uniques,
            readable: self.read_state(),
            stdlib: Default::default(),
            updated_modules: Default::default(),
            additional_loaders: Default::default(),
            now: self.read_state().now,
            require: RequireDefault::new(require),
            todo: Default::default(),
            changed: Default::default(),
            dirty: Default::default(),
            subscriber,
        }
    }

    pub fn transaction<'a>(&'a self) -> Transaction<'a> {
        self.new_transaction(Require::Exports, None)
    }

    pub fn new_committable_transaction<'a>(
        &'a self,
        require: Require,
        subscriber: Option<Box<dyn Subscriber>>,
    ) -> CommittingTransaction<'a> {
        let committing_transaction_guard = self.committing_transaction_lock.lock();
        let transaction = self.new_transaction(require, subscriber);
        CommittingTransaction {
            transaction,
            committing_transaction_guard,
        }
    }

    pub fn read_state<'a>(&'a self) -> RwLockReadGuard<'a, ReadableState> {
        self.state.read()
    }

    pub fn commit_transaction(&self, changes: TransactionChanges) {
        let mut state = self.state.write();
        let TransactionChanges {
            stdlib,
            updated_modules,
            additional_loaders,
            now,
            require,
            committing_transaction_guard,
        } = changes;
        state.stdlib = stdlib;
        state.now = now;
        state.require = require;
        for (handle, new_module_data) in updated_modules.iter_unordered() {
            let new_data = new_module_data.to_snapshot();
            state.modules.insert(handle.dupe(), new_data);
        }
        for (loader_id, additional_loader) in additional_loaders {
            state.loaders.insert(loader_id, additional_loader);
        }
        drop(committing_transaction_guard)
    }

    pub fn run(
        &self,
        handles: &[(Handle, Require)],
        new_require: Require,
        subscriber: Option<Box<dyn Subscriber>>,
    ) {
        let mut transaction = self.new_committable_transaction(new_require, subscriber);
        transaction
            .transaction
            .run_internal(handles, self.read_state().require);
        self.commit_transaction(transaction.into_changes());
    }

    pub fn run_with_committing_transaction(
        &self,
        mut transaction: CommittingTransaction<'_>,
        handles: &[(Handle, Require)],
    ) {
        transaction
            .transaction
            .run_internal(handles, self.read_state().require);
        self.commit_transaction(transaction.into_changes());
    }
}
