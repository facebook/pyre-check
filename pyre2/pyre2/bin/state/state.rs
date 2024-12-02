/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::Ref;
use std::cell::RefCell;
use std::cell::RefMut;
use std::mem;
use std::sync::Arc;

use dupe::Dupe;
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
    stdlib: RefCell<Arc<Stdlib>>,
    modules: RefCell<SmallMap<ModuleName, ModuleState>>,
}

#[derive(Default)]
struct ModuleState {
    errors: Arc<ErrorCollector>,
    steps: ModuleSteps,
}

impl ModuleState {
    fn clear(&mut self) {
        self.errors = Arc::new(ErrorCollector::new());
        self.steps.clear();
    }
}

impl<'a> State<'a> {
    pub fn new(
        config: &'a Config,
        loader: &'a Loader<'a>,
        parallel: bool,
        modules: &[ModuleName],
    ) -> Self {
        Self {
            config,
            loader,
            uniques: UniqueFactory::new(),
            parallel,
            stdlib: RefCell::new(Arc::new(Stdlib::for_bootstrapping())),
            modules: RefCell::new(
                modules
                    .iter()
                    .chain(&Stdlib::required())
                    .map(|x| (*x, ModuleState::default()))
                    .collect(),
            ),
        }
    }

    fn evict<T>(&self, module: ModuleName, f: fn(&mut ModuleSteps) -> &mut Info<T>) {
        f(&mut self.modules.borrow_mut().get_mut(&module).unwrap().steps).clear();
    }

    fn demand(&self, module: ModuleName, step: Step) {
        loop {
            let module_state = self.get_module(module);
            let todo = match step.compute_next(&module_state.steps) {
                Some(todo) => todo,
                None => return,
            };
            let compute = todo.compute().0(&module_state.steps);
            let errors = module_state.errors.dupe();
            mem::drop(module_state);
            if todo == Step::Bindings {
                // We have captured the Ast, and must have already built Exports (we do it serially),
                // so won't need the Ast again.
                self.evict(module, |x| &mut x.ast);
            }
            let stdlib = self.stdlib.borrow().dupe();
            let set = compute(&Context {
                name: module,
                config: self.config,
                loader: self.loader,
                uniques: &self.uniques,
                stdlib: &stdlib,
                errors: &errors,
                lookup: self,
            });
            set(&mut self.get_module_mut(module).steps);
            if todo == Step::Solutions {
                // From now on we can use the answers directly, so evict the bindings/answers.
                self.evict(module, |x| &mut x.bindings);
                self.evict(module, |x| &mut x.answers);
            }
            if todo == step {
                return; // Fast path - avoid asking again since we just did it.
            }
        }
    }

    fn get_module(&self, module: ModuleName) -> Ref<ModuleState> {
        match Ref::filter_map(self.modules.borrow(), |x| x.get(&module)) {
            Ok(v) => v,
            Err(r) => {
                mem::drop(r);
                self.modules
                    .borrow_mut()
                    .insert(module, ModuleState::default());
                Ref::map(self.modules.borrow(), |x| x.get(&module).unwrap())
            }
        }
    }

    fn get_module_mut(&self, module: ModuleName) -> RefMut<ModuleState> {
        RefMut::map(self.modules.borrow_mut(), |x| x.entry(module).or_default())
    }

    fn grab<T: 'static>(&self, module: ModuleName, f: fn(&ModuleState) -> T) -> T {
        f(&self.get_module(module))
    }

    fn lookup_stdlib(&self, module: ModuleName, name: &Name) -> Option<Class> {
        let t = self.lookup_answer(module, &KeyExported::Export(name.clone()));
        match t.arc_clone() {
            Type::ClassDef(cls) => Some(cls),
            ty => {
                let m = self.get_module(module);
                m.errors.add(
                    &m.steps.module_info.get().unwrap().0,
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
        Some(self.grab(module, |x| x.steps.exports.get().unwrap().dupe()))
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
            if let Some(solutions) = self.get_module(module).steps.solutions.get() {
                return Arc::new(TableKeyed::<K>::get(&**solutions).get(key).unwrap().clone());
            }
        }

        self.demand(module, Step::Answers);
        let (errors, bindings, answers) = self.grab(module, |x| {
            (
                x.errors.dupe(),
                x.steps.bindings.get().unwrap().dupe(),
                x.steps.answers.get().unwrap().dupe(),
            )
        });
        let stdlib = self.stdlib.borrow().dupe();
        answers.solve_key(self, self, &bindings, &errors, &stdlib, &self.uniques, key)
    }

    fn collect_errors(&self) -> Vec<Error> {
        let mut errors = Vec::new();
        for module in self.modules.borrow().values() {
            errors.extend(module.errors.collect());
        }
        errors
    }

    fn compute_stdlib(&self) {
        let stdlib = Arc::new(Stdlib::new(|module, name| self.lookup_stdlib(module, name)));
        *self.stdlib.borrow_mut() = stdlib;
    }

    /// Run, collecting all errors and destroying the state.
    /// The state afterwards will be useful for timing queries.
    /// Note we grab the `mut` only to stop other people accessing us simultaneously,
    /// we don't actually need it.
    pub fn run_one_shot(&mut self) -> Vec<Error> {
        self.compute_stdlib();
        // ensure we have answers for everything, keep going until we don't discover any new modules
        let mut existing = 0;
        loop {
            let modules = self.modules.borrow();
            let new = modules.len();
            if new == existing {
                break;
            }
            existing = new;
            let force = modules
                .iter()
                .filter(|(_, v)| v.steps.solutions.get().is_none())
                .map(|(k, _)| *k)
                .collect::<Vec<_>>();
            mem::drop(modules);
            for k in force {
                self.demand(k, Step::Solutions);
            }
        }

        let errors = self.collect_errors();
        self.clear();
        errors
    }

    fn clear(&mut self) {
        // Should we reset stdlib? Currently we don't.
        for module in self.modules.borrow_mut().values_mut() {
            module.clear();
        }
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
