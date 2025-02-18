/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::RwLock;
use std::time::Duration;

use dupe::Dupe;
use dupe::OptionDupedExt;
use enum_iterator::Sequence;
use parking_lot::FairMutex;
use ruff_python_ast::name::Name;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use crate::alt::answers::AnswerEntry;
use crate::alt::answers::AnswerTable;
use crate::alt::answers::LookupAnswer;
use crate::alt::answers::Solutions;
use crate::alt::answers::SolutionsEntry;
use crate::alt::answers::Solve;
use crate::binding::binding::KeyExport;
use crate::binding::binding::Keyed;
use crate::binding::bindings::BindingEntry;
use crate::binding::bindings::BindingTable;
use crate::binding::bindings::Bindings;
use crate::binding::table::TableKeyed;
use crate::config::Config;
use crate::error::collector::ErrorCollector;
use crate::error::error::Error;
use crate::error::expectation::Expectation;
use crate::export::exports::Exports;
use crate::export::exports::LookupExport;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::report::debug_info::DebugInfo;
use crate::state::handle::Handle;
use crate::state::loader::FindError;
use crate::state::loader::Loader;
use crate::state::loader::LoaderFindCache;
use crate::state::loader::LoaderId;
use crate::state::steps::Context;
use crate::state::steps::ModuleSteps;
use crate::state::steps::Step;
use crate::types::class::Class;
use crate::types::stdlib::Stdlib;
use crate::types::types::Type;
use crate::util::display::number_thousands;
use crate::util::enum_heap::EnumHeap;
use crate::util::locked_map::LockedMap;
use crate::util::no_hash::BuildNoHash;
use crate::util::prelude::SliceExt;
use crate::util::uniques::UniqueFactory;

pub struct State {
    uniques: UniqueFactory,
    parallel: bool,
    stdlib: SmallMap<(Config, LoaderId), Arc<Stdlib>>,
    modules: LockedMap<Handle, Arc<ModuleState>>,
    loaders: SmallMap<LoaderId, Arc<LoaderFindCache<LoaderId>>>,
    /// Items we still need to process. Stored in a max heap, so that
    /// the highest step (the module that is closest to being finished)
    /// gets picked first, ensuring we release its memory quickly.
    todo: Mutex<EnumHeap<Step, Arc<ModuleState>>>,

    // Set to true to keep data around forever.
    retain_memory: bool,
}

struct ModuleState {
    // BIG WARNING: This must be a FairMutex or we are exposed to deadlocks.
    //
    // The FairMutex is locking an entire module, which goes through stages
    // of compute (the stages are enumerated by `Step`).
    //
    // Each stage can `demand` some previous stage of other modules, for
    // example `Answers` (which computes bindings) may require `Exports`
    // of other modules to handle wildcard imports.
    //
    // When a thread `demand`s a Step of a module, we take the lock, and
    // - we might see that what we need is already available
    // - we might see that it is not, in which case we start computing
    // - or we might pause if another thread owns the lock
    //
    // This works with a `FairMutex`: as long as we only `demand` earlier steps
    // from later steps, the demand calls form a DAG. Since `FairMutex` respects
    // the order of calls, the lock acquisition is also a DAG and we make
    // progress.
    //
    // But without a `FairMutex`, we lose control of the order and we can
    // deadlock. Suppose `foo` and `bar` each try to `from <other> import *`,
    // and consider when
    // - Thread 0 is computing bindings for `foo`
    // - Thread 1 is computing exports for `bar`
    //
    // When Thread 0 hits the `from bar import *`, it will try to take the lock
    // on `bar`, and wait. Thread 1 will produce the exports for `bar`.
    //
    // But if the mutex is not fair, Thread 1 (or some other thread) might
    // get the lock on `bar` to start computing bindings before Thread 0
    // does. If this happens, we will deadlock because Thread 0 still owns
    // the lock on `foo`, so the thread trying to compute bindings on `bar`
    // will get stuck trying to analyze `from foo import *`. Meanwhile Thread 0
    // is still waiting for `bar` and we are unable to make progress.
    lock: FairMutex<()>,
    steps: RwLock<ModuleSteps>,
    handle: Handle,
    dependencies: RwLock<HashMap<ModuleName, Arc<ModuleState>, BuildNoHash>>,
}

impl ModuleState {
    fn new(handle: Handle) -> Self {
        Self {
            lock: Default::default(),
            steps: Default::default(),
            handle,
            dependencies: Default::default(),
        }
    }
}

impl State {
    pub fn new(parallel: bool) -> Self {
        Self {
            uniques: UniqueFactory::new(),
            parallel,
            stdlib: Default::default(),
            modules: Default::default(),
            loaders: Default::default(),
            todo: Default::default(),
            retain_memory: true, // Will always be overwritten by entry points
        }
    }

    pub fn import_handle(&self, handle: &Handle, module: ModuleName) -> Result<Handle, FindError> {
        Ok(Handle::new(
            module,
            self.get_cached_find(handle.loader(), module)?,
            handle.config().dupe(),
            handle.loader().dupe(),
        ))
    }

    fn demand(&self, module_state: &Arc<ModuleState>, step: Step) {
        let mut computed = false;
        loop {
            let lock = module_state.steps.read().unwrap();
            match Step::Solutions.compute_next(&lock) {
                Some(todo) if todo <= step => {}
                _ => break,
            }
            drop(lock);
            let compute_lock = module_state.lock.try_lock_for(Duration::from_millis(100));
            if compute_lock.is_none() {
                // Typechecking empty.py with -j25 deadlocks about 1% of the time. At -j100 it deadlocks about 5% of the time.
                // I don't really understand it. But the fact we have to use FairMutex is a bit of a smell, and potentially
                // we are having races as to what "fair" means.
                //
                // We plan to rewrite much of this code soon to support incrementality, so for now, bodge it.
                // If we can't get the lock in 100ms, just give up and try from scratch.
                // Small delay plus rarely hit means this has no overall perf impact.
                continue;
            }
            let lock = module_state.steps.read().unwrap();

            // BIG WARNING: We do Step::Solutions.compute_next, NOT step.compute_next.
            // The reason being that we may evict Answers, and later ask for Answers,
            // which would then say Answers needed to be computed. But because we only ever
            // evict earlier things in the list when computing later things, if we always
            // ask from the end we get the right thing, not the evicted thing.
            let todo = match Step::Solutions.compute_next(&lock) {
                Some(todo) if todo <= step => todo,
                _ => break,
            };
            computed = true;
            let compute = todo.compute().0(&lock);
            drop(lock);
            if todo == Step::Answers && !self.retain_memory {
                // We have captured the Ast, and must have already built Exports (we do it serially),
                // so won't need the Ast again.
                module_state.steps.write().unwrap().ast.take();
            }

            let stdlib = self.get_stdlib(&module_state.handle);
            let loader = self.get_cached_loader(module_state.handle.loader());
            let set = compute(&Context {
                module: module_state.handle.module(),
                path: module_state.handle.path(),
                config: module_state.handle.config(),
                loader: &*loader,
                uniques: &self.uniques,
                stdlib: &stdlib,
                lookup: &self.lookup(module_state.dupe()),
                retain_memory: self.retain_memory,
            });
            {
                let mut to_drop = None;
                let mut module_write = module_state.steps.write().unwrap();
                set(&mut module_write);
                if todo == Step::Solutions && !self.retain_memory {
                    // From now on we can use the answers directly, so evict the bindings/answers.
                    to_drop = module_write.answers.take();
                }
                drop(module_write);
                // Release the lock before dropping
                drop(to_drop);
            }
            if todo == step {
                break; // Fast path - avoid asking again since we just did it.
            }
        }
        if computed && let Some(next) = step.next() {
            // For a large benchmark, LIFO is 10Gb retained, FIFO is 13Gb.
            // Perhaps we are getting to the heart of the graph with LIFO?
            self.todo
                .lock()
                .unwrap()
                .push_lifo(next, module_state.dupe());
        }
    }

    fn get_module(&self, handle: &Handle) -> Arc<ModuleState> {
        self.modules
            .ensure(handle, || Arc::new(ModuleState::new(handle.dupe())))
            .dupe()
    }

    fn add_error(&self, module_state: &Arc<ModuleState>, range: TextRange, msg: String) {
        let load = module_state
            .steps
            .read()
            .unwrap()
            .load
            .get()
            .unwrap()
            .dupe();
        load.errors.add(&load.module_info, range, msg);
    }

    fn lookup<'a>(&'a self, module_state: Arc<ModuleState>) -> StateHandle<'a> {
        StateHandle {
            state: self,
            module_state,
        }
    }

    fn lookup_stdlib(&self, handle: &Handle, name: &Name) -> Option<Class> {
        let module_state = self.get_module(handle);
        if !self
            .lookup_export(&module_state)
            .contains(name, &self.lookup(module_state.dupe()))
        {
            self.add_error(
                &module_state,
                TextRange::default(),
                format!(
                    "Stdlib import failure, was expecting `{}` to contain `{name}`",
                    module_state.handle.module()
                ),
            );
            return None;
        }

        let t = self.lookup_answer(module_state.dupe(), &KeyExport(name.clone()));
        match t.arc_clone() {
            Type::ClassDef(cls) => Some(cls),
            ty => {
                self.add_error(
                    &module_state,
                    TextRange::default(),
                    format!(
                        "Did not expect non-class type `{ty}` for stdlib import `{}.{name}`",
                        module_state.handle.module()
                    ),
                );
                None
            }
        }
    }

    fn lookup_export(&self, module_state: &Arc<ModuleState>) -> Exports {
        self.demand(module_state, Step::Exports);
        let lock = module_state.steps.read().unwrap();
        lock.exports.get().unwrap().dupe()
    }

    fn lookup_answer<'b, K: Solve<StateHandle<'b>> + Keyed<EXPORTED = true>>(
        &'b self,
        module_state: Arc<ModuleState>,
        key: &K,
    ) -> Arc<<K as Keyed>::Answer>
    where
        AnswerTable: TableKeyed<K, Value = AnswerEntry<K>>,
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
        Solutions: TableKeyed<K, Value = SolutionsEntry<K>>,
    {
        {
            // if we happen to have solutions available, use them instead
            if let Some(solutions) = module_state.steps.read().unwrap().solutions.get() {
                return Arc::new(TableKeyed::<K>::get(&**solutions).get(key).unwrap().clone());
            }
        }

        self.demand(&module_state, Step::Answers);
        let (load, answers) = {
            let steps = module_state.steps.read().unwrap();
            if let Some(solutions) = steps.solutions.get() {
                return Arc::new(TableKeyed::<K>::get(&**solutions).get(key).unwrap().clone());
            }
            (
                steps.load.get().unwrap().dupe(),
                steps.answers.get().unwrap().dupe(),
            )
        };
        let stdlib = self.get_stdlib(&module_state.handle);
        let lookup = self.lookup(module_state);
        answers.1.solve_key(
            &lookup,
            &lookup,
            &answers.0,
            &load.errors,
            &stdlib,
            &self.uniques,
            key,
        )
    }

    pub fn collect_errors(&self) -> Vec<Error> {
        let mut errors = Vec::new();
        for module in self.modules.values() {
            let steps = module.steps.read().unwrap();
            if let Some(load) = steps.load.get() {
                errors.extend(load.errors.collect());
            }
        }
        errors
    }

    pub fn module_count(&self) -> usize {
        self.modules.len()
    }

    pub fn count_errors(&self) -> usize {
        self.modules
            .values()
            .map(|x| {
                x.steps
                    .read()
                    .unwrap()
                    .load
                    .get()
                    .map_or(0, |x| x.errors.len())
            })
            .sum()
    }

    pub fn print_errors(&self) {
        for module in self.modules.values() {
            let steps = module.steps.read().unwrap();
            if let Some(load) = steps.load.get() {
                load.errors.print();
            }
        }
    }

    pub fn print_error_summary(&self, limit: usize) {
        let loads = self
            .modules
            .values()
            .filter_map(|x| x.steps.read().unwrap().load.get().duped())
            .collect::<Vec<_>>();
        let mut items = ErrorCollector::summarise(loads.iter().map(|x| &x.errors));
        items.reverse();
        items.truncate(limit);
        for (error, count) in items.iter().rev() {
            eprintln!("{} instances of {error}", number_thousands(*count));
        }
    }

    fn get_cached_find(
        &self,
        loader: &LoaderId,
        module: ModuleName,
    ) -> Result<ModulePath, FindError> {
        self.get_cached_loader(loader).find(module).map(|x| x.0)
    }

    fn get_cached_loader(&self, loader: &LoaderId) -> Arc<LoaderFindCache<LoaderId>> {
        if self.loaders.len() == 1 {
            // Since we know our one must exist, we can shortcut
            return self.loaders.first().unwrap().1.dupe();
        }

        // Safe because we always fill these in before starting
        self.loaders.get(loader).unwrap().dupe()
    }

    fn get_stdlib(&self, handle: &Handle) -> Arc<Stdlib> {
        if self.stdlib.len() == 1 {
            // Since we know our one must exist, we can shortcut
            return self.stdlib.first().unwrap().1.dupe();
        }

        // Safe because we always run compute_stdlib first
        self.stdlib
            .get(&(handle.config().dupe(), handle.loader().dupe()))
            .unwrap()
            .dupe()
    }

    fn compute_stdlib(&mut self, configs: SmallSet<(Config, LoaderId)>) {
        self.stdlib = configs
            .iter()
            .map(|k| (k.dupe(), Arc::new(Stdlib::for_bootstrapping())))
            .collect();
        let stdlibs = configs
            .iter()
            .map(|(c, l)| {
                (
                    (c.dupe(), l.dupe()),
                    Arc::new(Stdlib::new(|module, name| {
                        let path = self.get_cached_find(l, module).ok()?;
                        self.lookup_stdlib(&Handle::new(module, path, c.dupe(), l.dupe()), name)
                    })),
                )
            })
            .collect();
        self.stdlib = stdlibs;
    }

    fn work(&self) {
        // ensure we have answers for everything, keep going until we don't discover any new modules
        loop {
            let mut lock = self.todo.lock().unwrap();
            let x = match lock.pop() {
                Some(x) => x.1,
                None => break,
            };
            drop(lock);
            self.demand(&x, Step::last());
        }
    }

    fn run_internal(&mut self, handles: Vec<Handle>) {
        let configs = handles
            .iter()
            .map(|x| (x.config().dupe(), x.loader().dupe()))
            .collect::<SmallSet<_>>();

        self.loaders = configs
            .iter()
            .map(|x| (x.1.dupe(), Arc::new(LoaderFindCache::new(x.1.dupe()))))
            .collect::<SmallMap<_, _>>();

        {
            let mut lock = self.todo.lock().unwrap();
            for h in handles {
                lock.push_fifo(Step::first(), self.get_module(&h));
            }
        }

        self.compute_stdlib(configs);

        if self.parallel {
            rayon::scope(|s| {
                for _ in 1..rayon::current_num_threads() {
                    s.spawn(|_| self.work());
                }
                self.work();
            });
        } else {
            self.work();
        }
    }

    /// Run, collecting all errors and destroying the state.
    /// The state afterwards will be useful for timing queries.
    /// Note we grab the `mut` only to stop other people accessing us simultaneously,
    /// we don't actually need it.
    pub fn run_one_shot(&mut self, handles: Vec<Handle>) {
        self.retain_memory = false;
        self.run_internal(handles)
    }

    pub fn run(&mut self, handles: Vec<Handle>) {
        self.retain_memory = true;
        self.run_internal(handles)
    }

    pub fn handles(&self) -> Vec<Handle> {
        self.modules.keys().cloned().collect()
    }

    pub fn get_bindings(&self, handle: &Handle) -> Option<Bindings> {
        self.modules
            .get(handle)?
            .steps
            .read()
            .unwrap()
            .answers
            .get()
            .map(|x| x.0.dupe())
    }

    pub fn get_module_info(&self, handle: &Handle) -> Option<ModuleInfo> {
        self.modules
            .get(handle)?
            .steps
            .read()
            .unwrap()
            .load
            .get()
            .map(|x| x.module_info.dupe())
    }

    pub fn get_ast(&self, handle: &Handle) -> Option<Arc<ruff_python_ast::ModModule>> {
        self.modules
            .get(handle)?
            .steps
            .read()
            .unwrap()
            .ast
            .get()
            .duped()
    }

    pub fn get_solutions(&self, handle: &Handle) -> Option<Arc<Solutions>> {
        self.modules
            .get(handle)?
            .steps
            .read()
            .unwrap()
            .solutions
            .get()
            .duped()
    }

    pub fn debug_info(&self, handles: &[Handle]) -> DebugInfo {
        let owned = handles.map(|x| {
            let module = self.get_module(x);
            let steps = module.steps.read().unwrap();
            (
                steps.load.get().unwrap().dupe(),
                steps.answers.get().unwrap().dupe(),
                steps.solutions.get().unwrap().dupe(),
            )
        });
        DebugInfo::new(&owned.map(|x| (&x.0.module_info, &x.0.errors, &x.1.0, &*x.2)))
    }

    pub fn check_against_expectations(&self) -> anyhow::Result<()> {
        for module in self.modules.values() {
            let steps = module.steps.read().unwrap();
            let load = steps.load.get().unwrap();
            Expectation::parse(load.module_info.dupe(), load.module_info.contents())
                .check(&load.errors.collect())?;
        }
        Ok(())
    }

    fn invalidate_everything(&mut self) {
        *self = State::new(self.parallel);
    }

    /// Called if the `find` portion of loading might have changed.
    /// E.g. you have include paths, and a new file appeared earlier on the path.
    #[expect(dead_code)]
    pub fn invalidate_find(&mut self, loader: LoaderId) {
        let _ = loader;
        self.invalidate_everything();
    }

    /// Called if the `load_from_memory` portion of loading might have changed.
    /// Specify which in-memory files might have changed.
    pub fn invalidate_memory(&mut self, loader: LoaderId, files: &[PathBuf]) {
        let _ = loader;
        let _ = files;
        self.invalidate_everything();
    }

    /// Called if the files read from the disk might have changed.
    /// Specify which files might have changed.
    /// You must use the same absolute/relative paths as were given by `find`.
    #[expect(dead_code)]
    pub fn invalidate_disk(&mut self, files: &[PathBuf]) {
        let _ = files;
        self.invalidate_everything();
    }

    /* Notes on how to move to incremental

    /// Run, collecting all errors and leaving enough around for further queries and incremental updates.
    pub fn run_incremental(&mut self) -> Vec<Error> {
        // FIXME: Should be incremental and not retain all state.
        unimplemented!()
    }

    /// Note that a particular path has changed, and the results are potentially invalid.
    pub fn update(&mut self, path: &Path) {
        let _ = path;
        // FIXME: For now, just invalidate everything.
        self.clear();
    }
    */
}

struct StateHandle<'a> {
    state: &'a State,
    module_state: Arc<ModuleState>,
}

impl<'a> StateHandle<'a> {
    fn get_module(&self, module: ModuleName) -> Result<Arc<ModuleState>, FindError> {
        if let Some(res) = self.module_state.dependencies.read().unwrap().get(&module) {
            return Ok(res.dupe());
        }

        let handle = self
            .state
            .import_handle(&self.module_state.handle, module)?;
        let res = self.state.get_module(&handle);
        self.module_state
            .dependencies
            .write()
            .unwrap()
            .insert(module, res.dupe());
        Ok(res)
    }
}

impl<'a> LookupExport for StateHandle<'a> {
    fn get(&self, module: ModuleName) -> Result<Exports, FindError> {
        Ok(self.state.lookup_export(&self.get_module(module)?))
    }
}

impl<'a> LookupAnswer for StateHandle<'a> {
    fn get<K: Solve<Self> + Keyed<EXPORTED = true>>(
        &self,
        module: ModuleName,
        k: &K,
    ) -> Arc<K::Answer>
    where
        AnswerTable: TableKeyed<K, Value = AnswerEntry<K>>,
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
        Solutions: TableKeyed<K, Value = SolutionsEntry<K>>,
    {
        // The unwrap is safe because we must have said there were no exports,
        // so no one can be trying to get at them
        let module_state = self.get_module(module).unwrap();
        self.state.lookup_answer(module_state, k)
    }
}
