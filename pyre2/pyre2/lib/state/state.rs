/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use dupe::Dupe;
use enum_iterator::Sequence;
use ruff_python_ast::name::Name;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use crate::alt::answers::AnswerEntry;
use crate::alt::answers::AnswerTable;
use crate::alt::answers::Answers;
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
use crate::config::Config;
use crate::error::collector::ErrorCollector;
use crate::error::error::Error;
use crate::error::expectation::Expectation;
use crate::export::exports::Exports;
use crate::export::exports::LookupExport;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::module::module_path::ModulePathDetails;
use crate::report::debug_info::DebugInfo;
use crate::state::dirty::Dirty;
use crate::state::epoch::Epoch;
use crate::state::epoch::Epochs;
use crate::state::handle::Handle;
use crate::state::loader::FindError;
use crate::state::loader::Loader;
use crate::state::loader::LoaderFindCache;
use crate::state::loader::LoaderId;
use crate::state::steps::Context;
use crate::state::steps::Step;
use crate::state::steps::Steps;
use crate::types::class::Class;
use crate::types::stdlib::Stdlib;
use crate::types::types::Type;
use crate::util::display::number_thousands;
use crate::util::enum_heap::EnumHeap;
use crate::util::lock::Mutex;
use crate::util::lock::RwLock;
use crate::util::locked_map::LockedMap;
use crate::util::no_hash::BuildNoHash;
use crate::util::prelude::SliceExt;
use crate::util::uniques::UniqueFactory;
use crate::util::upgrade_lock::UpgradeLock;

pub struct State {
    uniques: UniqueFactory,
    parallel: bool,
    stdlib: SmallMap<(Config, LoaderId), Arc<Stdlib>>,
    modules: LockedMap<Handle, Arc<ModuleData>>,
    loaders: SmallMap<LoaderId, Arc<LoaderFindCache<LoaderId>>>,
    /// Items we still need to process. Stored in a max heap, so that
    /// the highest step (the module that is closest to being finished)
    /// gets picked first, ensuring we release its memory quickly.
    todo: Mutex<EnumHeap<Step, Arc<ModuleData>>>,
    /// The current epoch, gets incremented every time we recompute
    now: Epoch,

    // Set to true to keep data around forever.
    retain_memory: bool,
}

struct ModuleData {
    handle: Handle,
    state: UpgradeLock<Step, ModuleState>,
    dependencies: RwLock<HashMap<ModuleName, Arc<ModuleData>, BuildNoHash>>,
}

struct ModuleState {
    epochs: Epochs,
    dirty: Dirty,
    steps: Steps,
}

impl ModuleData {
    fn new(handle: Handle, now: Epoch) -> Self {
        Self {
            handle,
            state: UpgradeLock::new(ModuleState {
                epochs: Epochs::new(now),
                dirty: Dirty::default(),
                steps: Steps::default(),
            }),
            dependencies: Default::default(),
        }
    }
}

impl State {
    pub fn new(parallel: bool) -> Self {
        Self {
            uniques: UniqueFactory::new(),
            parallel,
            now: Epoch::zero(),
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

    fn clean(&self, module_data: &Arc<ModuleData>) {
        let exclusive;
        loop {
            match module_data.state.exclusive(Step::first()) {
                None => continue,
                Some(ex) => {
                    exclusive = ex;
                    break;
                }
            }
        }
        if exclusive.epochs.checked == self.now {
            // Someone already checked us
            return;
        }
        if exclusive.dirty.is_dirty() {
            panic!("Should make the code not dirty");
        }
        let mut write = exclusive.write();
        write.dirty.clean();
        write.epochs.checked = self.now;
    }

    fn demand(&self, module_data: &Arc<ModuleData>, step: Step) {
        let mut computed = false;
        loop {
            let reader = module_data.state.read();
            if reader.epochs.checked != self.now {
                drop(reader);
                self.clean(module_data);
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
            if todo == Step::Answers && !self.retain_memory {
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
                retain_memory: self.retain_memory,
                module: module_data.handle.module(),
                path: module_data.handle.path(),
                config: module_data.handle.config(),
                loader: &*loader,
                uniques: &self.uniques,
                stdlib: &stdlib,
                lookup: &self.lookup(module_data.dupe()),
            });
            {
                let mut to_drop = None;
                let mut writer = exclusive.write();
                set(&mut writer.steps);
                if todo == Step::Solutions {
                    writer.epochs.changed = self.now;
                    if !self.retain_memory {
                        // From now on we can use the answers directly, so evict the bindings/answers.
                        to_drop = writer.steps.answers.take();
                    }
                }
                drop(writer);
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
            self.todo.lock().push_lifo(next, module_data.dupe());
        }
    }

    fn get_module(&self, handle: &Handle) -> Arc<ModuleData> {
        self.modules
            .ensure(handle, || {
                Arc::new(ModuleData::new(handle.dupe(), self.now))
            })
            .dupe()
    }

    fn add_error(&self, module_data: &Arc<ModuleData>, range: TextRange, msg: String) {
        let load = module_data.state.read().steps.load.dupe().unwrap();
        load.errors.add(&load.module_info, range, msg);
    }

    fn lookup<'a>(&'a self, module_data: Arc<ModuleData>) -> StateHandle<'a> {
        StateHandle {
            state: self,
            module_data,
        }
    }

    fn lookup_stdlib(&self, handle: &Handle, name: &Name) -> Option<Class> {
        let module_data = self.get_module(handle);
        if !self
            .lookup_export(&module_data)
            .contains(name, &self.lookup(module_data.dupe()))
        {
            self.add_error(
                &module_data,
                TextRange::default(),
                format!(
                    "Stdlib import failure, was expecting `{}` to contain `{name}`",
                    module_data.handle.module()
                ),
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
                );
                None
            }
        }
    }

    fn lookup_export(&self, module_data: &Arc<ModuleData>) -> Exports {
        self.demand(module_data, Step::Exports);
        let lock = module_data.state.read();
        lock.steps.exports.dupe().unwrap()
    }

    fn lookup_answer<'b, K: Solve<StateHandle<'b>> + Keyed<EXPORTED = true>>(
        &'b self,
        module_data: Arc<ModuleData>,
        key: &K,
    ) -> Arc<<K as Keyed>::Answer>
    where
        AnswerTable: TableKeyed<K, Value = AnswerEntry<K>>,
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
        SolutionsTable: TableKeyed<K, Value = SolutionsEntry<K>>,
    {
        {
            // if we happen to have solutions available, use them instead
            if let Some(solutions) = &module_data.state.read().steps.solutions {
                return solutions.get(key).unwrap().dupe();
            }
        }

        self.demand(&module_data, Step::Answers);
        let (load, answers) = {
            let steps = module_data.state.read();
            if let Some(solutions) = &steps.steps.solutions {
                return solutions.get(key).unwrap().dupe();
            }
            (
                steps.steps.load.dupe().unwrap(),
                steps.steps.answers.dupe().unwrap(),
            )
        };
        let stdlib = self.get_stdlib(&module_data.handle);
        let lookup = self.lookup(module_data);
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
            let steps = module.state.read();
            if let Some(load) = &steps.steps.load {
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
                x.state
                    .read()
                    .steps
                    .load
                    .as_ref()
                    .map_or(0, |x| x.errors.len())
            })
            .sum()
    }

    pub fn print_errors(&self) {
        for module in self.modules.values() {
            let steps = module.state.read();
            if let Some(load) = &steps.steps.load {
                load.errors.print();
            }
        }
    }

    pub fn print_error_summary(&self, limit: usize) {
        let loads = self
            .modules
            .values()
            .filter_map(|x| x.state.read().steps.load.dupe())
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
            let mut lock = self.todo.lock();
            let x = match lock.pop() {
                Some(x) => x.1,
                None => break,
            };
            drop(lock);
            self.demand(&x, Step::last());
        }
    }

    fn run_internal(&mut self, handles: &[Handle]) {
        self.now.next();
        let configs = handles
            .iter()
            .map(|x| (x.config().dupe(), x.loader().dupe()))
            .collect::<SmallSet<_>>();

        self.loaders = configs
            .iter()
            .map(|x| (x.1.dupe(), Arc::new(LoaderFindCache::new(x.1.dupe()))))
            .collect::<SmallMap<_, _>>();

        {
            let mut lock = self.todo.lock();
            for h in handles {
                lock.push_fifo(Step::first(), self.get_module(h));
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
    pub fn run_one_shot(&mut self, handles: &[Handle]) {
        self.retain_memory = false;
        self.run_internal(handles)
    }

    pub fn run(&mut self, handles: &[Handle]) {
        self.retain_memory = true;
        self.run_internal(handles)
    }

    pub fn handles(&self) -> Vec<Handle> {
        self.modules.keys().cloned().collect()
    }

    pub fn get_bindings(&self, handle: &Handle) -> Option<Bindings> {
        self.modules
            .get(handle)?
            .state
            .read()
            .steps
            .answers
            .as_ref()
            .map(|x| x.0.dupe())
    }

    pub fn get_answers(&self, handle: &Handle) -> Option<Arc<Answers>> {
        self.modules
            .get(handle)?
            .state
            .read()
            .steps
            .answers
            .as_ref()
            .map(|x| x.1.dupe())
    }

    pub fn get_module_info(&self, handle: &Handle) -> Option<ModuleInfo> {
        self.modules
            .get(handle)?
            .state
            .read()
            .steps
            .load
            .as_ref()
            .map(|x| x.module_info.dupe())
    }

    pub fn get_ast(&self, handle: &Handle) -> Option<Arc<ruff_python_ast::ModModule>> {
        self.modules.get(handle)?.state.read().steps.ast.dupe()
    }

    #[allow(dead_code)]
    pub fn get_solutions(&self, handle: &Handle) -> Option<Arc<Solutions>> {
        self.modules
            .get(handle)?
            .state
            .read()
            .steps
            .solutions
            .dupe()
    }

    pub fn debug_info(&self, handles: &[Handle]) -> DebugInfo {
        let owned = handles.map(|x| {
            let module = self.get_module(x);
            let steps = module.state.read();
            (
                steps.steps.load.dupe().unwrap(),
                steps.steps.answers.dupe().unwrap(),
                steps.steps.solutions.dupe().unwrap(),
            )
        });
        DebugInfo::new(&owned.map(|x| (&x.0.module_info, &x.0.errors, &x.1.0, &*x.2)))
    }

    pub fn check_against_expectations(&self) -> anyhow::Result<()> {
        for module in self.modules.values() {
            let steps = module.state.read();
            let load = steps.steps.load.as_ref().unwrap();
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
    pub fn invalidate_find(&mut self, loader: &LoaderId) {
        if let Some(cache) = self.loaders.get_mut(loader) {
            *cache = Arc::new(LoaderFindCache::new(loader.dupe()));
        }
        for (handle, module_data) in self.modules.iter_unordered() {
            if handle.loader() == loader {
                module_data
                    .state
                    .write(Step::Load)
                    .unwrap()
                    .dirty
                    .set_dirty_find();
            }
        }

        self.invalidate_everything();
    }

    /// Called if the `load_from_memory` portion of loading might have changed.
    /// Specify which in-memory files might have changed.
    pub fn invalidate_memory(&mut self, loader: LoaderId, files: &[PathBuf]) {
        let files = SmallSet::from_iter(files);
        for (handle, module_data) in self.modules.iter_unordered() {
            if handle.loader() == &loader
                && let ModulePathDetails::Memory(x) = handle.path().details()
                && files.contains(x)
            {
                module_data
                    .state
                    .write(Step::Load)
                    .unwrap()
                    .dirty
                    .set_dirty_load();
            }
        }

        self.invalidate_everything();
    }

    /// Called if the files read from the disk might have changed.
    /// Specify which files might have changed.
    /// You must use the same absolute/relative paths as were given by `find`.
    #[expect(dead_code)]
    pub fn invalidate_disk(&mut self, files: &[PathBuf]) {
        let files = SmallSet::from_iter(files);
        for (handle, module_data) in self.modules.iter_unordered() {
            if let ModulePathDetails::FileSystem(x) = handle.path().details()
                && files.contains(x)
            {
                module_data
                    .state
                    .write(Step::Load)
                    .unwrap()
                    .dirty
                    .set_dirty_load();
            }
        }

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
    module_data: Arc<ModuleData>,
}

impl<'a> StateHandle<'a> {
    fn get_module(&self, module: ModuleName) -> Result<Arc<ModuleData>, FindError> {
        if let Some(res) = self.module_data.dependencies.read().get(&module) {
            return Ok(res.dupe());
        }

        let handle = self.state.import_handle(&self.module_data.handle, module)?;
        let res = self.state.get_module(&handle);
        self.module_data
            .dependencies
            .write()
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
        SolutionsTable: TableKeyed<K, Value = SolutionsEntry<K>>,
    {
        // The unwrap is safe because we must have said there were no exports,
        // so no one can be trying to get at them
        let module_data = self.get_module(module).unwrap();
        self.state.lookup_answer(module_data, k)
    }
}
