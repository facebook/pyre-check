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
use std::time::Instant;

use dupe::Dupe;
use ruff_python_ast::name::Name;
use ruff_python_ast::ModModule;
use starlark_map::small_map::SmallMap;

use crate::alt::answers::AnswerEntry;
use crate::alt::answers::AnswerTable;
use crate::alt::answers::Answers;
use crate::alt::answers::LookupAnswer;
use crate::alt::answers::Solutions;
use crate::alt::answers::Solve;
use crate::alt::binding::Exported;
use crate::alt::bindings::BindingEntry;
use crate::alt::bindings::BindingTable;
use crate::alt::bindings::Bindings;
use crate::alt::exports::Exports;
use crate::alt::exports::LookupExport;
use crate::alt::loader::Loader;
use crate::alt::table::Keyed;
use crate::alt::table::TableKeyed;
use crate::config::Config;
use crate::error::collector::ErrorCollector;
use crate::error::error::Error;
use crate::expectation::Expectation;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::types::class::Class;
use crate::types::stdlib::Stdlib;
use crate::uniques::UniqueFactory;

pub struct State<'a> {
    // User supplied
    config: &'a Config,
    loader: &'a Loader<'a>,
    #[expect(dead_code)] // Always single threaded for now
    parallel: bool,
    // Calculated
    uniques: UniqueFactory,
    modules: RefCell<SmallMap<ModuleName, ModuleState>>,
    stdlib: RefCell<Info<Arc<Stdlib>>>,
}

#[derive(Debug)]
struct Info<T> {
    time: Option<(Instant, Instant)>,
    value: Option<T>,
}

impl<T> Default for Info<T> {
    fn default() -> Self {
        Self {
            time: Default::default(),
            value: Default::default(),
        }
    }
}

impl<T> Info<T> {
    fn clear(&mut self) {
        self.value = None;
    }

    fn start() -> Self {
        let now = Instant::now();
        Self {
            time: Some((now, now)),
            value: None,
        }
    }

    fn stop(self, v: T) -> Self {
        let now = Instant::now();
        Self {
            time: Some((self.time.map_or(now, |x| x.0), now)),
            value: Some(v),
        }
    }
}

#[derive(Debug, Default)]
struct ModuleState {
    /// I could not be found, so anyone who tries importing me
    /// should raise an error.
    finding_error: Option<anyhow::Error>,
    errors: Arc<ErrorCollector>,
    module_info: Info<ModuleInfo>,
    ast: Info<Arc<ModModule>>,
    expectations: Info<Arc<Expectation>>,
    exports: Info<Exports>,
    bindings: Info<Bindings>,
    answers: Info<Arc<Answers>>,
    solutions: Info<Arc<Solutions>>,
}

macro_rules! computes {
    ($self:ident, $module:ident, $field:ident) => {{
        let m = $self.get_module($module);
        if let Some(v) = &m.$field.value {
            return v.dupe();
        }
    }};
}

impl ModuleState {
    fn new() -> Self {
        Self::default()
    }

    fn clear(&mut self) {
        // Note that we retain the timing information.
        self.finding_error = None;
        self.errors = Arc::new(ErrorCollector::new());
        self.module_info.clear();
        self.ast.clear();
        self.expectations.clear();
        self.exports.clear();
        self.bindings.clear();
        self.answers.clear();
        self.solutions.clear();
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
            parallel,
            uniques: UniqueFactory::new(),
            modules: RefCell::new(
                modules
                    .iter()
                    .chain(&Stdlib::required())
                    .map(|x| (*x, ModuleState::new()))
                    .collect(),
            ),
            stdlib: RefCell::new(Info::start()),
        }
    }

    fn lookup_stdlib(&self, module: ModuleName, name: &Name) -> Option<Class> {
        self.force_answers(module).lookup_class_without_stdlib(
            &self.force_bindings(module),
            &self.force_errors(module),
            module,
            name,
            self,
            self,
            &self.uniques,
        )
    }

    fn lookup_export(&self, module: ModuleName) -> Option<Exports> {
        Some(self.force_exports(module))
    }

    fn lookup_answer<'b, K: Solve<Self> + Exported>(
        &'b self,
        module: ModuleName,
        key: &K,
    ) -> Arc<<K as Keyed>::Answer>
    where
        AnswerTable: TableKeyed<K, Value = AnswerEntry<K>>,
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
    {
        self.force_answers(module).solve_key(
            self,
            self,
            &self.force_bindings(module),
            &self.force_errors(module),
            &self.force_stdlib(),
            &self.uniques,
            key,
        )
    }

    fn collect_errors(&self) -> Vec<Error> {
        let mut errors = Vec::new();
        for module in self.modules.borrow().values() {
            errors.extend(module.errors.collect());
        }
        errors
    }

    fn get_module(&self, module: ModuleName) -> Ref<ModuleState> {
        match Ref::filter_map(self.modules.borrow(), |x| x.get(&module)) {
            Ok(v) => v,
            Err(r) => {
                mem::drop(r);
                self.modules.borrow_mut().insert(module, ModuleState::new());
                Ref::map(self.modules.borrow(), |x| x.get(&module).unwrap())
            }
        }
    }

    fn get_module_mut(&self, module: ModuleName) -> RefMut<ModuleState> {
        RefMut::map(self.modules.borrow_mut(), |x| x.entry(module).or_default())
    }

    fn force_answers(&self, module: ModuleName) -> Arc<Answers> {
        computes!(self, module, answers);
        let bindings = self.force_bindings(module);
        let info = Info::start();
        let answers = Arc::new(Answers::new(&bindings));
        self.get_module_mut(module).answers = info.stop(answers.dupe());
        answers
    }

    fn force_bindings(&self, module: ModuleName) -> Bindings {
        computes!(self, module, bindings);
        let module_info = self.force_module_info(module);
        let errors = self.force_errors(module);
        let ast = self.force_ast(module);
        let info = Info::start();
        let bindings = Bindings::new(
            ast.body.clone(),
            module_info,
            self,
            self.config,
            &errors,
            &self.uniques,
        );
        self.get_module_mut(module).bindings = info.stop(bindings.dupe());
        bindings
    }

    fn force_module_info(&self, module: ModuleName) -> ModuleInfo {
        computes!(self, module, module_info);
        let info = Info::start();
        let (load_result, should_type_check) = (self.loader)(module);
        let components = load_result.components(module);
        let module_info =
            ModuleInfo::new(module, components.path, components.code, should_type_check);
        self.get_module_mut(module).module_info = info.stop(module_info.dupe());
        self.get_module_mut(module).finding_error = components.import_error;
        module_info
    }

    fn force_errors(&self, module: ModuleName) -> Arc<ErrorCollector> {
        self.get_module(module).errors.dupe()
    }

    fn force_ast(&self, module: ModuleName) -> Arc<ModModule> {
        computes!(self, module, ast);
        let errors = self.force_errors(module);
        let module_info = self.force_module_info(module);
        let info = Info::start();
        let ast = Arc::new(module_info.parse(&errors));
        self.get_module_mut(module).ast = info.stop(ast.dupe());
        ast
    }

    fn force_exports(&self, module: ModuleName) -> Exports {
        computes!(self, module, exports);
        let module_info = self.force_module_info(module);
        let ast = self.force_ast(module);
        let info = Info::start();
        let exports = Exports::new(&ast.body, &module_info, self.config);
        self.get_module_mut(module).exports = info.stop(exports.dupe());
        exports
    }

    /// Force a particular module to be loaded, all the way to answers.
    fn force_solutions(&self, module: ModuleName) -> Arc<Solutions> {
        computes!(self, module, solutions);
        let errors = self.force_errors(module);
        let stdlib = self.force_stdlib();
        let answers = self.force_answers(module);
        let bindings = self.force_bindings(module);
        let info = Info::start();
        let solutions =
            Arc::new(answers.solve(self, self, &bindings, &errors, &stdlib, &self.uniques));
        self.get_module_mut(module).solutions = info.stop(solutions.dupe());
        solutions
    }

    fn force_stdlib(&self) -> Arc<Stdlib> {
        {
            let stdlib = self.stdlib.borrow();
            if let Some(v) = &stdlib.value {
                return v.dupe();
            }
        }
        let info = Info::start();
        let stdlib = Arc::new(Stdlib::new(|module, name| self.lookup_stdlib(module, name)));
        *self.stdlib.borrow_mut() = info.stop(stdlib.dupe());
        stdlib
    }

    /// Run, collecting all errors and destroying the state.
    /// The state afterwards will be useful for timing queries.
    /// Note we grab the `mut` only to stop other people accessing us simultaneously,
    /// we don't actually need it.
    pub fn run_one_shot(&mut self) -> Vec<Error> {
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
                .filter(|(_, v)| v.solutions.value.is_none())
                .map(|(k, _)| *k)
                .collect::<Vec<_>>();
            mem::drop(modules);
            for k in force {
                self.force_solutions(k);
            }
        }

        let errors = self.collect_errors();
        self.clear();
        errors
    }

    fn clear(&mut self) {
        self.stdlib.borrow_mut().clear();
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
    {
        self.lookup_answer(name, k)
    }
}
