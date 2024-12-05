/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;
use std::sync::Mutex;
use std::sync::RwLock;

use dupe::Dupe;
use dupe::OptionDupedExt;
use enum_iterator::Sequence;
use parking_lot::FairMutex;
use ruff_python_ast::name::Name;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;
use tracing::info;

use crate::alt::answers::AnswerEntry;
use crate::alt::answers::AnswerTable;
use crate::alt::answers::LookupAnswer;
use crate::alt::answers::Solutions;
use crate::alt::answers::SolutionsEntry;
use crate::alt::answers::Solve;
use crate::alt::binding::KeyExported;
use crate::alt::bindings::BindingEntry;
use crate::alt::bindings::BindingTable;
use crate::alt::bindings::Bindings;
use crate::alt::exports::Exports;
use crate::alt::exports::LookupExport;
use crate::alt::table::Keyed;
use crate::alt::table::TableKeyed;
use crate::config::Config;
use crate::debug_info::DebugInfo;
use crate::error::collector::ErrorCollector;
use crate::error::error::Error;
use crate::expectation::Expectation;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::state::loader::Loader;
use crate::state::steps::Context;
use crate::state::steps::ModuleSteps;
use crate::state::steps::Step;
use crate::types::class::Class;
use crate::types::stdlib::Stdlib;
use crate::types::types::Type;
use crate::uniques::UniqueFactory;
use crate::util::enum_heap::EnumHeap;
use crate::util::prelude::SliceExt;

pub struct State<'a> {
    config: Config,
    loader: Box<Loader<'a>>,
    uniques: UniqueFactory,
    parallel: bool,
    print_errors_immediately: bool,
    stdlib: RwLock<Arc<Stdlib>>,
    modules: RwLock<SmallMap<ModuleName, Arc<ModuleState>>>,
    /// Items we still need to process. Stored in a max heap, so that
    /// the highest step (the module that is closest to being finished)
    /// gets picked first, ensuring we release its memory quickly.
    todo: Mutex<EnumHeap<Step, ModuleName>>,

    // Set to true to keep data around forever.
    retain_memory: bool,
}

struct ModuleState {
    // BIG WARNING: This must be a FairMutex or you run into deadlocks.
    // Imagine module Foo is having demand Solutions in one thread, and demand Exports in another.
    // If the first thread ends up computing each entry in turn, it might starve the Exports waiting.
    // If the Solutions for Foo depends on the Answers of Bar, and the Answers of Bar was the one who
    // asked for the Exports of Foo, then you get a deadlock.
    // A fair mutex means that everyone asking for Exports will be released before you move to the next step.
    lock: FairMutex<()>,
    errors: ErrorCollector,
    steps: RwLock<ModuleSteps>,
}

impl ModuleState {
    fn new(print_errors_immediately: bool) -> Self {
        Self {
            lock: FairMutex::new(()),
            errors: if print_errors_immediately {
                ErrorCollector::new()
            } else {
                ErrorCollector::new_quiet()
            },
            steps: RwLock::new(ModuleSteps::default()),
        }
    }

    fn clear(&self) {
        self.errors.clear();
        self.steps.write().unwrap().clear();
    }
}

impl<'a> State<'a> {
    pub fn new(
        modules: &[ModuleName],
        loader: Box<Loader<'a>>,
        config: Config,
        parallel: bool,
        print_errors_immediately: bool,
    ) -> Self {
        let stdlib_modules = Stdlib::required();
        Self {
            config,
            loader,
            uniques: UniqueFactory::new(),
            parallel,
            print_errors_immediately,
            stdlib: RwLock::new(Arc::new(Stdlib::for_bootstrapping())),
            modules: RwLock::new(
                modules
                    .iter()
                    .chain(&stdlib_modules)
                    .map(|x| (*x, Arc::new(ModuleState::new(print_errors_immediately))))
                    .collect(),
            ),
            todo: Mutex::new(
                modules
                    .iter()
                    .chain(&stdlib_modules)
                    .map(|x| (Step::first(), *x))
                    .collect(),
            ),
            retain_memory: true, // Will always be overwritten by entry points
        }
    }

    fn demand(&self, module: ModuleName, step: Step) {
        let module_state = self.get_module(module);
        let mut computed = false;
        loop {
            let lock = module_state.steps.read().unwrap();
            match Step::Solutions.compute_next(&lock) {
                Some(todo) if todo <= step => {}
                _ => break,
            }
            drop(lock);
            let _compute_lock = module_state.lock.lock();
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
            let imports = if todo == Step::Solutions {
                Some((
                    lock.module_info.get().unwrap().0.dupe(),
                    lock.exports.get().unwrap().dupe(),
                ))
            } else {
                None
            };
            computed = true;
            let compute = todo.compute().0(&lock);
            drop(lock);
            if todo == Step::Answers && !self.retain_memory {
                // We have captured the Ast, and must have already built Exports (we do it serially),
                // so won't need the Ast again.
                module_state.steps.write().unwrap().ast.clear();
            }

            if let Some((module_info, imports)) = imports {
                // We need a single spot to inject "I could not find import", but we want to do that
                // after we can be sure our dependencies have been loaded (we don't want to demand them for an error),
                // so we do it right at the end.
                for (importing, range) in imports.0.imports.iter() {
                    if let Some(err) = self
                        .get_module(*importing)
                        .steps
                        .read()
                        .unwrap()
                        .module_info
                        .get()
                        .and_then(|x| x.1.as_deref())
                    {
                        module_state.errors.add(
                            &module_info,
                            *range,
                            format!("Could not find import of `{}`, {err:#}", importing),
                        );
                    }
                }
            }

            let stdlib = self.stdlib.read().unwrap().dupe();
            let set = compute(&Context {
                name: module,
                config: &self.config,
                loader: &*self.loader,
                uniques: &self.uniques,
                stdlib: &stdlib,
                errors: &module_state.errors,
                lookup: self,
                retain_memory: self.retain_memory,
            });
            {
                let mut module_write = module_state.steps.write().unwrap();
                set(&mut module_write);
                if todo == Step::Solutions && !self.retain_memory {
                    // From now on we can use the answers directly, so evict the bindings/answers.
                    module_write.answers.clear();
                }
            }
            if todo == step {
                break; // Fast path - avoid asking again since we just did it.
            }
        }
        if computed && let Some(next) = step.next() {
            // For a large benchmark, LIFO is 10Gb retained, FIFO is 13Gb.
            // Perhaps we are getting to the heart of the graph with LIFO?
            self.todo.lock().unwrap().push_lifo(next, module);
        }
    }

    fn get_module(&self, module: ModuleName) -> Arc<ModuleState> {
        let lock = self.modules.read().unwrap();
        if let Some(v) = lock.get(&module) {
            return v.dupe();
        }
        drop(lock);
        self.modules
            .write()
            .unwrap()
            .entry(module)
            .or_insert_with(|| Arc::new(ModuleState::new(self.print_errors_immediately)))
            .dupe()
    }

    fn add_error(&self, module: ModuleName, range: TextRange, msg: String) {
        let m = self.get_module(module);
        m.errors.add(
            &m.steps.read().unwrap().module_info.get().unwrap().0,
            range,
            msg,
        );
    }

    fn lookup_stdlib(&self, module: ModuleName, name: &Name) -> Option<Class> {
        let t = self.lookup_answer(module, &KeyExported::Export(name.clone()));
        match t.arc_clone() {
            Type::ClassDef(cls) => Some(cls),
            ty => {
                self.add_error(
                    module,
                    TextRange::default(),
                    format!(
                        "Did not expect non-class type `{ty}` for stdlib import `{module}.{name}`"
                    ),
                );
                None
            }
        }
    }

    fn lookup_export(&self, module: ModuleName) -> Option<Exports> {
        self.demand(module, Step::Exports);
        self.get_module(module)
            .steps
            .read()
            .unwrap()
            .exports
            .get()
            .map(|x| x.1.dupe())
    }

    fn lookup_answer<'b, K: Solve<Self> + Keyed<EXPORTED = true>>(
        &'b self,
        module: ModuleName,
        key: &K,
    ) -> Arc<<K as Keyed>::Answer>
    where
        AnswerTable: TableKeyed<K, Value = AnswerEntry<K>>,
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
        Solutions: TableKeyed<K, Value = SolutionsEntry<K>>,
    {
        let module_state = self.get_module(module);
        {
            // if we happen to have solutions available, use them instead
            if let Some(solutions) = module_state.steps.read().unwrap().solutions.get() {
                return Arc::new(TableKeyed::<K>::get(&**solutions).get(key).unwrap().clone());
            }
        }

        self.demand(module, Step::Answers);
        let answers = {
            let steps = module_state.steps.read().unwrap();
            if let Some(solutions) = steps.solutions.get() {
                return Arc::new(TableKeyed::<K>::get(&**solutions).get(key).unwrap().clone());
            }
            steps.answers.get().unwrap().dupe()
        };
        let stdlib = self.stdlib.read().unwrap().dupe();
        answers.1.solve_key(
            self,
            self,
            &answers.0,
            &module_state.errors,
            &stdlib,
            &self.uniques,
            key,
        )
    }

    pub fn collect_errors(&self) -> Vec<Error> {
        let mut errors = Vec::new();
        for module in self.modules.read().unwrap().values() {
            errors.extend(module.errors.collect());
        }
        errors
    }

    pub fn count_errors(&self) -> usize {
        self.modules
            .read()
            .unwrap()
            .values()
            .map(|x| x.errors.len())
            .sum()
    }

    pub fn print_errors(&self) {
        for module in self.modules.read().unwrap().values() {
            module.errors.print();
        }
    }

    pub fn print_error_summary(&self) {
        for (error, count) in
            ErrorCollector::summarise(self.modules.read().unwrap().values().map(|x| &x.errors))
        {
            eprintln!("{count} instances of {error}");
        }
    }

    fn compute_stdlib(&self) {
        let stdlib = Arc::new(Stdlib::new(|module, name| self.lookup_stdlib(module, name)));
        *self.stdlib.write().unwrap() = stdlib;
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
            self.demand(x, Step::last());
        }
    }

    fn run_internal(&mut self) {
        self.compute_stdlib();

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
    pub fn run_one_shot(&mut self) {
        self.retain_memory = false;
        self.run_internal()
    }

    pub fn run(&mut self) {
        self.retain_memory = true;
        self.run_internal()
    }

    #[expect(dead_code)]
    fn clear(&mut self) {
        // Should we reset stdlib? Currently we don't.
        for module in self.modules.get_mut().unwrap().values_mut() {
            module.clear();
        }
    }

    pub fn modules(&self) -> Vec<ModuleName> {
        self.modules.read().unwrap().keys().copied().collect()
    }

    pub fn get_bindings(&self, module: ModuleName) -> Option<Bindings> {
        self.modules
            .read()
            .unwrap()
            .get(&module)?
            .steps
            .read()
            .unwrap()
            .answers
            .get()
            .map(|x| x.0.dupe())
    }

    pub fn get_module_info(&self, module: ModuleName) -> Option<ModuleInfo> {
        self.modules
            .read()
            .unwrap()
            .get(&module)?
            .steps
            .read()
            .unwrap()
            .module_info
            .get()
            .map(|x| x.0.dupe())
    }

    pub fn get_ast(&self, module: ModuleName) -> Option<Arc<ruff_python_ast::ModModule>> {
        self.modules
            .read()
            .unwrap()
            .get(&module)?
            .steps
            .read()
            .unwrap()
            .ast
            .get()
            .duped()
    }

    pub fn get_solutions(&self, module: ModuleName) -> Option<Arc<Solutions>> {
        self.modules
            .read()
            .unwrap()
            .get(&module)?
            .steps
            .read()
            .unwrap()
            .solutions
            .get()
            .duped()
    }

    pub fn debug_info(&self, modules: &[ModuleName]) -> DebugInfo {
        let owned = modules.map(|x| {
            let module = self.get_module(*x);
            let steps = module.steps.read().unwrap();
            (
                steps.module_info.get().unwrap().0.dupe(),
                module.dupe(),
                steps.answers.get().unwrap().dupe(),
                steps.solutions.get().unwrap().dupe(),
            )
        });
        DebugInfo::new(&owned.map(|x| (&x.0, &x.1.errors, &x.2.0, &*x.3)))
    }

    pub fn collect_checked_errors(&self) -> Vec<Error> {
        self.modules
            .read()
            .unwrap()
            .values()
            .flat_map(|x| x.errors.collect())
            .filter(|x| x.is_in_checked_module())
            .collect()
    }

    pub fn check_against_expectations(&self) -> anyhow::Result<()> {
        for (name, module) in self.modules.read().unwrap().iter() {
            info!("Check for {name}");
            let steps = module.steps.read().unwrap();
            Expectation::parse(steps.module_info.get().unwrap().0.contents())
                .check(&module.errors.collect())?;
        }
        Ok(())
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

impl LookupExport for State<'_> {
    fn get_opt(&self, module: ModuleName) -> Option<Exports> {
        self.lookup_export(module)
    }
}

impl LookupAnswer for State<'_> {
    fn get<K: Solve<Self> + Keyed<EXPORTED = true>>(
        &self,
        name: ModuleName,
        k: &K,
        _exports: &dyn LookupExport,
        _uniques: &UniqueFactory,
        _stdlib: &Stdlib,
    ) -> Arc<K::Answer>
    where
        AnswerTable: TableKeyed<K, Value = AnswerEntry<K>>,
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
        Solutions: TableKeyed<K, Value = SolutionsEntry<K>>,
    {
        self.lookup_answer(name, k)
    }
}
