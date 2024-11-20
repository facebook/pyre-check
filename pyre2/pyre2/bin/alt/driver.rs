/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::any::type_name_of_val;
use std::mem;
use std::path::PathBuf;
use std::sync::Mutex;

use dupe::Dupe;
use human_bytes::human_bytes;
use memory_stats::memory_stats;
use parse_display::Display;
use rayon::prelude::*;
use ruff_python_ast::name::Name;
use ruff_python_ast::ModModule;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;
use tracing::info;

use crate::alt::answers::Answers;
use crate::alt::answers::Solutions;
use crate::alt::answers::SolutionsEntry;
use crate::alt::binding::Key;
use crate::alt::bindings::BindingEntry;
use crate::alt::bindings::BindingTable;
use crate::alt::bindings::Bindings;
use crate::alt::exports::Exports;
use crate::alt::table::Keyed;
use crate::alt::table::TableKeyed;
use crate::ast::Ast;
use crate::config::Config;
use crate::debug;
use crate::error::collector::ErrorCollector;
use crate::error::error::Error;
use crate::expectation::Expectation;
use crate::graph::index::Idx;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::solver::Solver;
use crate::table_for_each;
#[cfg(test)]
use crate::types::mro::Mro;
use crate::types::stdlib::Stdlib;
use crate::types::types::Type;
use crate::uniques::UniqueFactory;
use crate::util::display::DisplayWith;
use crate::util::fs_anyhow;
use crate::util::prelude::SliceExt;
use crate::util::small_map;
use crate::util::timer::TimerContext;
use crate::util::transitive_closure::transitive_closure;
use crate::util::transitive_closure::transitive_closure_par;

fn timers_global_module() -> ModuleName {
    ModuleName::from_str("(global)")
}

type Timers = TimerContext<(ModuleName, Step, usize)>;

#[derive(Copy, Clone, Dupe, Debug, PartialEq, Eq, Hash, Display)]
enum Step {
    Startup,
    Load,
    Parse,
    Exports,
    Bindings,
    Solve,
    Answers,
    PrintErrors,
}

static FAKE_MODULE: &str = r#"
from typing import Any
def __getattr__(name: str) -> Any: ...
"#;

fn fake_path(module_name: ModuleName) -> PathBuf {
    // The generated fake module shouldn't have an errors, but lets make it clear
    // this is a fake path if it ever happens to leak into any output.
    PathBuf::from(format!("/fake/{module_name}.py"))
}

/// The result of trying to load a file.
pub enum LoadResult {
    Loaded(PathBuf, String),
    FailedToLoad(PathBuf, anyhow::Error),
    FailedToFind(anyhow::Error),
}

impl LoadResult {
    pub fn from_path_result(path: anyhow::Result<PathBuf>) -> Self {
        match path {
            Ok(path) => match fs_anyhow::read_to_string(&path) {
                Ok(code) => LoadResult::Loaded(path, code),
                Err(err) => LoadResult::FailedToLoad(path, err),
            },
            Err(err) => LoadResult::FailedToFind(err),
        }
    }
}

#[derive(Debug)]
struct Phase1 {
    expectations: Expectation,
    module_info: ModuleInfo,
    module: ModModule,
    exports: Exports,
    /// Set only if the path doesn't exist, so the importer is wrong
    error: Option<anyhow::Error>,
}

impl Phase1 {
    fn new(
        timers: &mut Timers,
        module_name: ModuleName,
        path: anyhow::Result<PathBuf>,
        code: String,
        module: ModModule,
        should_type_check: bool,
        config: &Config,
    ) -> Self {
        let expectations = Expectation::parse(&code);
        let (path, error) = match path {
            Ok(path) => (path, None),
            Err(err) => (fake_path(module_name), Some(err)),
        };
        let module_info = ModuleInfo::new(module_name, path, code, should_type_check);
        timers.add((module_name, Step::Startup, 0));
        let exports = Exports::new(&module.body, &module_info, config);
        timers.add((module_name, Step::Exports, 0));
        Self {
            expectations,
            module_info,
            module,
            exports,
            error,
        }
    }

    fn imports(&self) -> SmallMap<ModuleName, TextRange> {
        Ast::imports(
            &self.module,
            self.module_info.name(),
            self.module_info.is_init(),
        )
    }
}

#[derive(Debug)]
struct Phase2 {
    name: ModuleName,
    bindings: Bindings,
}

impl Phase2 {
    fn new(
        timers: &mut Timers,
        phase1: &Phase1,
        modules: &SmallMap<ModuleName, Exports>,
        uniques: &UniqueFactory,
        config: &Config,
        errors: &ErrorCollector,
    ) -> Self {
        let bindings = Bindings::new(
            phase1.module.body.clone(),
            phase1.module_info.dupe(),
            modules,
            config,
            errors,
            uniques,
        );
        timers.add((phase1.module_info.name(), Step::Bindings, bindings.len()));
        Self {
            name: phase1.module_info.name(),
            bindings,
        }
    }
}

/// `name` was imported from `importer` at position `range`
#[derive(Debug)]
struct Import {
    name: ModuleName,
    importer: ModuleName,
    range: TextRange,
}

fn run_phase1(
    timers: &mut Timers,
    modules: &[ModuleName],
    config: &Config,
    errors: &ErrorCollector,
    load: impl Fn(ModuleName) -> (LoadResult, bool) + Sync,
    debug: bool,
    parallel: bool,
) -> Vec<Phase1> {
    let mut todo = modules.to_owned();
    todo.push(ModuleName::builtins());

    // List of imports, just used to put errors in the right place
    let imports = Mutex::new(todo.map(|x| Import {
        name: *x,
        importer: *x,
        range: TextRange::default(),
    }));

    let done = (if parallel {
        transitive_closure_par
    } else {
        transitive_closure
    })(todo, |name: &ModuleName| {
        let mut timers = Timers::new();
        let mut my_errors = Vec::new();
        let (path, code, should_type_check) = {
            let (loaded, should_type_check) = load(*name);
            match loaded {
                LoadResult::Loaded(path, code) => (Ok(path), code, should_type_check),
                LoadResult::FailedToLoad(path, err) => {
                    my_errors.push((
                        TextRange::default(),
                        format!("Failed to load {name} from {}, got {err:#}", path.display()),
                    ));
                    (Ok(path), FAKE_MODULE.to_owned(), should_type_check)
                }
                LoadResult::FailedToFind(err) => {
                    (Err(err), FAKE_MODULE.to_owned(), should_type_check)
                }
            }
        };
        timers.add((*name, Step::Load, code.len()));
        let (module, parse_errors) = Ast::parse(&code);
        for err in parse_errors {
            my_errors.push((err.location, format!("Parse error: {err}")));
        }
        timers.add((*name, Step::Parse, code.len()));
        info!("Phase 1 for {name}");
        let p = Phase1::new(
            &mut timers,
            *name,
            path,
            code,
            module,
            should_type_check,
            config,
        );
        if debug {
            eprintln!("\nExports for {name}:\n{}", p.exports);
        }
        for x in my_errors {
            errors.add(&p.module_info, x.0, x.1);
        }
        let my_imports = p.imports();
        imports
            .lock()
            .unwrap()
            .extend(my_imports.iter().map(|(n, range)| Import {
                name: *n,
                importer: *name,
                range: *range,
            }));
        (p, my_imports.into_keys().collect())
    });
    // We can no longer tell parse from load, so dump it into parse which usually dominates
    timers.add((timers_global_module(), Step::Parse, 0));

    for import in imports.into_inner().unwrap() {
        let result = done.get(&import.name).unwrap();
        if let Some(err) = &result.error {
            errors.add(
                &done.get(&import.importer).unwrap().module_info,
                import.range,
                format!("Could not find import of `{}`, {err:#}", import.name),
            );
        }
    }
    done.into_values().collect()
}

fn run_phase2(
    timers: &mut Timers,
    phase1: &[Phase1],
    modules: &SmallMap<ModuleName, Exports>,
    uniques: &UniqueFactory,
    config: &Config,
    errors: &ErrorCollector,
    debug: bool,
    parallel: bool,
) -> Vec<Phase2> {
    let f = |v: &Phase1| {
        let mut timers = Timers::new();
        let module_name = v.module_info.name();
        info!("Phase 2 for {module_name}");
        let res = Phase2::new(&mut timers, v, modules, uniques, config, errors);
        if debug {
            eprintln!("\nBindings for {module_name}:\n{}", res.bindings);
        }
        res
    };
    let res = if parallel {
        phase1.par_iter().map(f).collect()
    } else {
        phase1.map(f)
    };
    timers.add((timers_global_module(), Step::Bindings, 0));
    res
}

#[derive(Debug, Default)]
pub struct Driver {
    errors: Vec<Error>,
    expectations: SmallMap<ModuleName, Expectation>,
    solutions: SmallMap<ModuleName, Solutions>,
    phases: SmallMap<ModuleName, (Phase1, Phase2)>,
}

impl Driver {
    pub fn new(
        modules: &[ModuleName],
        config: &Config,
        debug: bool,
        timings: Option<usize>,
        parallel: bool,
        load: impl Fn(ModuleName) -> (LoadResult, bool) + Sync,
    ) -> Self {
        let mut timers = Timers::new();
        let timers = &mut timers;
        let uniques = UniqueFactory::new();
        let errors = if timings.is_some() {
            // Printing errors immediately takes a variable amount of time (waiting for IO),
            // so avoid that if we are trying to get robust timings.
            ErrorCollector::new_quiet()
        } else {
            ErrorCollector::new()
        };
        let solver = Solver::new(&uniques);

        timers.add((timers_global_module(), Step::Startup, 0));
        let phase1 = run_phase1(timers, modules, config, &errors, load, debug, parallel);
        let exports = phase1
            .iter()
            .map(|v| (v.module_info.name(), v.exports.dupe()))
            .collect();

        let phase2 = run_phase2(
            timers, &phase1, &exports, &uniques, config, &errors, debug, parallel,
        );

        let answers: SmallMap<ModuleName, Answers> = phase2
            .iter()
            .map(|v| {
                let ans = Answers::new(&exports, &v.bindings);
                timers.add((v.name, Step::Answers, ans.len()));
                (v.name, ans)
            })
            .collect();
        let stdlib = make_stdlib(&answers, &errors, &uniques, &solver);
        let solutions = (if parallel {
            small_map::par_map
        } else {
            small_map::map
        })(&answers, |_, x: &Answers| {
            x.solve(&answers, &stdlib, &errors, &uniques, &solver)
        });
        timers.add((timers_global_module(), Step::Solve, 0));

        if timings.is_some() {
            errors.print();
        }
        for (error, count) in errors.summarise() {
            eprintln!("{count} instances of {error}");
        }

        // Print this on both stderr and stdout, since handy to have in either dump
        info_eprintln(format!("Total errors: {}", errors.len()));
        let printing = timers.add((timers_global_module(), Step::PrintErrors, errors.len()));

        if let Some(memory) = memory_stats() {
            eprintln!("Memory usage: {}", human_bytes(memory.physical_mem as f64));
        }

        let total = timers.total();
        info_eprintln(format!(
            "Took {total:.2?} (excluding print {:.2?})",
            total - printing
        ));
        if let Some(timings) = timings {
            Self::print_timings(timings, timers);
        }

        mem::drop(answers);
        assert_eq!(phase1.len(), phase2.len());
        let expectations = phase1
            .iter()
            .map(|v| (v.module_info.name(), v.expectations.clone()))
            .collect();
        let mut phases = SmallMap::with_capacity(phase1.len());
        for (p1, p2) in phase1.into_iter().zip(phase2) {
            assert_eq!(p1.module_info.name(), p2.name);
            phases.insert(p2.name, (p1, p2));
        }
        Driver {
            errors: errors.collect(),
            expectations,
            solutions,
            phases,
        }
    }

    fn print_timings(count: usize, timers: &mut TimerContext<(ModuleName, Step, usize)>) {
        eprintln!("Expensive operations");
        for ((module, step, number), time) in timers.ordered().iter().take(count) {
            eprintln!(
                "  {module} {step}{}: {time:.2?}",
                if *number == 0 {
                    String::new()
                } else {
                    format!(" ({number})")
                }
            );
        }
        eprintln!("Expensive modules");
        for (module, time) in timers.grouped(|x| x.0).iter().take(count) {
            eprintln!("  {module}: {time:.2?}");
        }
        eprintln!("Expensive steps");
        for (step, time) in timers.grouped(|x| x.1).iter().take(count) {
            eprintln!("  {step}: {time:.2?}");
        }
    }

    pub fn errors(&self) -> &[Error] {
        &self.errors
    }

    pub fn errors_in_checked_modules(&self) -> Vec<Error> {
        self.errors
            .iter()
            .filter(|x| x.is_in_checked_module())
            .cloned()
            .collect()
    }

    pub fn module_info(&self, module: ModuleName) -> Option<ModuleInfo> {
        self.phases.get(&module).map(|x| x.0.module_info.dupe())
    }

    pub fn check_against_expectations(&self) -> anyhow::Result<()> {
        for (name, expect) in &self.expectations {
            info!("Check for {name}");
            let errs = self
                .errors
                .iter()
                .filter(|e| e.module_name() == *name)
                .cloned()
                .collect::<Vec<_>>();
            expect.check(&errs)?;
        }
        Ok(())
    }

    #[cfg(test)]
    pub fn mro_of_export(&self, module: ModuleName, name: &str) -> Option<&Mro> {
        use ruff_python_ast::name::Name;

        use crate::alt::binding::Key;
        use crate::alt::binding::KeyMro;
        use crate::types::types::Type;

        match self.solutions.get(&module).unwrap().types.get(
            self.phases
                .get(&module)
                .unwrap()
                .1
                .bindings
                .key_to_idx(&Key::Export(Name::new(name))),
        ) {
            Some(Type::ClassDef(cls)) => {
                println!("Class {cls:?}");
                let x = self.solutions.get(&module).unwrap().mros.get(
                    self.phases
                        .get(&module)
                        .unwrap()
                        .1
                        .bindings
                        .key_to_idx(&KeyMro::Mro(cls.name().clone())),
                );
                x
            }
            _ => None,
        }
    }

    pub fn get_mod_module(&self, module: ModuleName) -> Option<&ModModule> {
        self.phases.get(&module).map(|x| &x.0.module)
    }

    pub fn get_bindings(&self, module: ModuleName) -> Option<&Bindings> {
        self.phases.get(&module).map(|x| &x.1.bindings)
    }

    pub fn get_type_for_idx(&self, module: ModuleName, idx: Idx<Key>) -> Option<&Type> {
        self.solutions.get(&module)?.types.get(idx)
    }

    pub fn debug_info(&self, modules: &[ModuleName]) -> debug::Info {
        fn f<K: Keyed>(
            t: &SolutionsEntry<K>,
            phase1: &Phase1,
            phase2: &Phase2,
            bindings: &mut Vec<debug::Binding>,
        ) where
            BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
        {
            for (idx, val) in t.iter() {
                let key = phase2.bindings.idx_to_key(idx);
                bindings.push(debug::Binding {
                    kind: type_name_of_val(key).rsplit_once(':').unwrap().1.to_owned(),
                    key: key.to_string(),
                    location: phase1.module_info.source_range(key.range()).to_string(),
                    binding: phase2
                        .bindings
                        .get(idx)
                        .display_with(&phase2.bindings)
                        .to_string(),
                    result: val.to_string(),
                })
            }
        }

        debug::Info {
            modules: modules
                .iter()
                .map(|x| {
                    let mut bindings = Vec::new();
                    let (phase1, phase2) = self.phases.get(x).unwrap();
                    table_for_each!(self.solutions.get(x).unwrap(), |t| f(
                        t,
                        phase1,
                        phase2,
                        &mut bindings
                    ));
                    let errors = self
                        .errors
                        .iter()
                        .filter(|e| e.module_name() == *x)
                        .map(|e| debug::Error {
                            location: e.source_range().to_string(),
                            message: e.msg.clone(),
                        })
                        .collect();
                    (*x, debug::Module { bindings, errors })
                })
                .collect(),
        }
    }
}

/// Some messages are useful to have in both stdout and stderr, so print them twice
fn info_eprintln(msg: String) {
    info!("{}", msg);
    eprintln!("{}", msg);
}

fn make_stdlib(
    answers: &SmallMap<ModuleName, Answers>,
    errors: &ErrorCollector,
    uniques: &UniqueFactory,
    solver: &Solver,
) -> Stdlib {
    let stdlib_modules = [
        ModuleName::builtins(),
        ModuleName::typing(),
        ModuleName::types(),
    ];
    let stdlib_answers = stdlib_modules
        .into_iter()
        .map(|module| (module, answers.get(&module).unwrap()))
        .collect::<SmallMap<_, _>>();
    let lookup_class = |module: ModuleName, name: &Name| {
        stdlib_answers
            .get(&module)
            .unwrap()
            .lookup_class_without_stdlib(module, name, answers, errors, uniques, solver)
    };
    Stdlib::new(lookup_class)
}
