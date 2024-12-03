/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;
use std::sync::RwLock;

use dupe::Dupe;
use dupe::OptionDupedExt;
use ruff_python_ast::name::Name;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;

use crate::alt::answers::AnswerEntry;
use crate::alt::answers::AnswerTable;
use crate::alt::answers::LookupAnswer;
use crate::alt::answers::Solutions;
use crate::alt::answers::SolutionsEntry;
use crate::alt::answers::Solve;
use crate::alt::binding::Exported;
use crate::alt::binding::KeyExported;
use crate::alt::bindings::BindingEntry;
use crate::alt::bindings::BindingTable;
use crate::alt::bindings::Bindings;
use crate::alt::exports::Exports;
use crate::alt::exports::LookupExport;
use crate::alt::table::Keyed;
use crate::alt::table::TableKeyed;
use crate::config::Config;
use crate::error::collector::ErrorCollector;
use crate::error::error::Error;
use crate::module::module_name::ModuleName;
use crate::state::info::Info;
use crate::state::loader::Loader;
use crate::state::steps::Context;
use crate::state::steps::ModuleSteps;
use crate::state::steps::Step;
use crate::types::class::Class;
use crate::types::stdlib::Stdlib;
use crate::types::types::Type;
use crate::uniques::UniqueFactory;

pub struct State<'a> {
    config: &'a Config,
    loader: &'a Loader<'a>,
    uniques: UniqueFactory,
    #[allow(dead_code)]
    parallel: bool,
    stdlib: RwLock<Arc<Stdlib>>,
    modules: RwLock<SmallMap<ModuleName, Arc<ModuleState>>>,

    // Set to true to keep data around forever.
    retain_memory: bool,
}

#[derive(Default)]
struct ModuleState {
    errors: Arc<ErrorCollector>,
    steps: RwLock<ModuleSteps>,
}

impl ModuleState {
    fn clear(&self) {
        self.errors.clear();
        self.steps.write().unwrap().clear();
    }
}

impl<'a> State<'a> {
    pub fn new(
        config: &'a Config,
        loader: &'a Loader<'a>,
        parallel: bool,
        modules: &[ModuleName],
    ) -> Self {
        let stdlib_modules = Stdlib::required();
        Self {
            config,
            loader,
            uniques: UniqueFactory::new(),
            parallel,
            stdlib: RwLock::new(Arc::new(Stdlib::for_bootstrapping())),
            modules: RwLock::new(
                modules
                    .iter()
                    .chain(&stdlib_modules)
                    .map(|x| (*x, Default::default()))
                    .collect(),
            ),
            retain_memory: true, // Will always be overwritten by entry points
        }
    }

    fn evict<T>(&self, info: &mut Info<T>) {
        if !self.retain_memory {
            info.clear();
        }
    }

    fn demand(&self, module: ModuleName, step: Step) {
        loop {
            let module_state = self.get_module(module);
            let lock = module_state.steps.read().unwrap();
            let todo = match step.compute_next(&lock) {
                Some(todo) => todo,
                None => return,
            };
            let compute = todo.compute().0(&lock);
            drop(lock);
            if todo == Step::Bindings {
                // We have captured the Ast, and must have already built Exports (we do it serially),
                // so won't need the Ast again.
                self.evict(&mut module_state.steps.write().unwrap().ast);
            }
            let stdlib = self.stdlib.read().unwrap().dupe();
            let set = compute(&Context {
                name: module,
                config: self.config,
                loader: self.loader,
                uniques: &self.uniques,
                stdlib: &stdlib,
                errors: &module_state.errors,
                lookup: self,
            });
            set(&mut module_state.steps.write().unwrap());
            if todo == Step::Solutions {
                // From now on we can use the answers directly, so evict the bindings/answers.
                let mut lock = module_state.steps.write().unwrap();
                self.evict(&mut lock.bindings);
                self.evict(&mut lock.answers);
            }
            if todo == step {
                return; // Fast path - avoid asking again since we just did it.
            }
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
            .or_default()
            .dupe()
    }

    fn grab<T: 'static>(
        &self,
        module: ModuleName,
        f: for<'b> fn(&'b Arc<ErrorCollector>, &'b ModuleSteps) -> T,
    ) -> T {
        let module_state = self.get_module(module);
        let lock = module_state.steps.read().unwrap();
        f(&module_state.errors, &lock)
    }

    fn lookup_stdlib(&self, module: ModuleName, name: &Name) -> Option<Class> {
        let t = self.lookup_answer(module, &KeyExported::Export(name.clone()));
        match t.arc_clone() {
            Type::ClassDef(cls) => Some(cls),
            ty => {
                let m = self.get_module(module);
                m.errors.add(
                    &m.steps.read().unwrap().module_info.get().unwrap().0,
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
        Some(self.grab(module, |_, steps| steps.exports.get().unwrap().dupe()))
    }

    fn lookup_answer<'b, K: Solve<Self> + Exported>(
        &'b self,
        module: ModuleName,
        key: &K,
    ) -> Arc<<K as Keyed>::Answer>
    where
        AnswerTable: TableKeyed<K, Value = AnswerEntry<K>>,
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
        Solutions: TableKeyed<K, Value = SolutionsEntry<K>>,
    {
        {
            // if we happen to have solutions available, use them instead
            if let Some(solutions) = self
                .get_module(module)
                .steps
                .read()
                .unwrap()
                .solutions
                .get()
            {
                return Arc::new(TableKeyed::<K>::get(&**solutions).get(key).unwrap().clone());
            }
        }

        self.demand(module, Step::Answers);
        let (errors, bindings, answers) = self.grab(module, |errors, steps| {
            (
                errors.dupe(),
                steps.bindings.get().unwrap().dupe(),
                steps.answers.get().unwrap().dupe(),
            )
        });
        let stdlib = self.stdlib.read().unwrap().dupe();
        answers.solve_key(self, self, &bindings, &errors, &stdlib, &self.uniques, key)
    }

    fn collect_errors(&self) -> Vec<Error> {
        let mut errors = Vec::new();
        for module in self.modules.read().unwrap().values() {
            errors.extend(module.errors.collect());
        }
        errors
    }

    fn compute_stdlib(&self) {
        let stdlib = Arc::new(Stdlib::new(|module, name| self.lookup_stdlib(module, name)));
        *self.stdlib.write().unwrap() = stdlib;
    }

    fn run_internal(&mut self) -> Vec<Error> {
        self.compute_stdlib();

        // ensure we have answers for everything, keep going until we don't discover any new modules
        let mut existing = 0;
        loop {
            let modules = self.modules.read().unwrap();
            let new = modules.len();
            if new == existing {
                break;
            }
            existing = new;
            let force = modules
                .iter()
                .filter(|(_, v)| v.steps.read().unwrap().solutions.get().is_none())
                .map(|(k, _)| *k)
                .collect::<Vec<_>>();
            drop(modules);
            for k in force {
                self.demand(k, Step::Solutions);
            }
        }

        self.collect_errors()
    }

    /// Run, collecting all errors and destroying the state.
    /// The state afterwards will be useful for timing queries.
    /// Note we grab the `mut` only to stop other people accessing us simultaneously,
    /// we don't actually need it.
    pub fn run_one_shot(&mut self) -> Vec<Error> {
        self.retain_memory = false;
        let errors = self.run_internal();
        self.clear();
        errors
    }

    pub fn run(&mut self) -> Vec<Error> {
        self.retain_memory = true;
        self.run_internal()
    }

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
            .bindings
            .get()
            .duped()
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
    fn get<K: Solve<Self> + Exported>(
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
