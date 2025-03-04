/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use anyhow::anyhow;
use dupe::Dupe;
use enum_iterator::Sequence;
use parse_display::Display;
use paste::paste;
use ruff_python_ast::ModModule;
use ruff_text_size::TextRange;

use crate::alt::answers::Answers;
use crate::alt::answers::LookupAnswer;
use crate::alt::answers::Solutions;
use crate::alt::id_cache::IdCacheHistory;
use crate::binding::bindings::Bindings;
use crate::error::collector::ErrorCollector;
use crate::error::kind::ErrorKind;
use crate::error::style::ErrorStyle;
use crate::export::exports::Exports;
use crate::export::exports::LookupExport;
use crate::metadata::RuntimeMetadata;
use crate::module::bundled::typeshed;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::module::module_path::ModulePathDetails;
use crate::solver::solver::Solver;
use crate::state::loader::Loader;
use crate::types::stdlib::Stdlib;
use crate::util::fs_anyhow;
use crate::util::uniques::UniqueFactory;

pub struct Context<'a, Lookup> {
    pub retain_memory: bool,
    pub module: ModuleName,
    pub path: &'a ModulePath,
    pub config: &'a RuntimeMetadata,
    pub loader: &'a dyn Loader,
    pub uniques: &'a UniqueFactory,
    pub stdlib: &'a Stdlib,
    pub lookup: &'a Lookup,
}

#[derive(Debug)]
pub struct Load {
    pub errors: ErrorCollector,
    pub module_info: ModuleInfo,
}

impl Load {
    /// Return the code for this module, and whether there was an error while loading (a self-error).
    pub fn load_from_path(
        path: &ModulePath,
        loader: &dyn Loader,
    ) -> (Arc<String>, Option<anyhow::Error>) {
        let res = match path.details() {
            ModulePathDetails::FileSystem(path) => fs_anyhow::read_to_string(path).map(Arc::new),
            ModulePathDetails::Namespace(_) => Ok(Arc::new("".to_owned())),
            ModulePathDetails::Memory(path) => loader
                .load_from_memory(path)
                .ok_or_else(|| anyhow!("memory path not found")),
            ModulePathDetails::BundledTypeshed(path) => typeshed().and_then(|x| {
                x.load(path)
                    .ok_or_else(|| anyhow!("bundled typeshed problem"))
            }),
        };
        match res {
            Err(err) => (Arc::new(String::new()), Some(err)),
            Ok(res) => (res, None),
        }
    }

    pub fn load_from_data(
        name: ModuleName,
        path: ModulePath,
        error_style: ErrorStyle,
        code: Arc<String>,
        self_error: Option<anyhow::Error>,
    ) -> Self {
        let module_info = ModuleInfo::new(name, path, code);
        let errors = ErrorCollector::new(module_info.dupe(), error_style);
        if let Some(err) = self_error {
            errors.add(
                TextRange::default(),
                format!(
                    "Failed to load `{name}` from `{}`, got {err:#}",
                    module_info.path()
                ),
                ErrorKind::Unknown,
                None,
            );
        }
        Self {
            errors,
            module_info,
        }
    }
}

#[derive(Debug, Default)]
pub struct Steps {
    /// The last step that was computed.
    /// None means no steps have been computed yet.
    pub last_step: Option<Step>,
    pub load: Option<Arc<Load>>,
    pub ast: Option<Arc<ModModule>>,
    pub exports: Option<Exports>,
    pub answers: Option<Arc<(Bindings, Arc<Answers>)>>,
    pub solutions: Option<Arc<(IdCacheHistory, Arc<Solutions>)>>,
}

impl Steps {
    // The next step to compute, if any.
    pub fn next_step(&self) -> Option<Step> {
        match self.last_step {
            None => Some(Step::first()),
            Some(last) => last.next(),
        }
    }
}

#[derive(
    Debug, Clone, Copy, Dupe, Eq, PartialEq, PartialOrd, Ord, Display, Sequence
)]
pub enum Step {
    Load,
    Ast,
    Exports,
    Answers,
    Solutions,
}

pub struct ComputeStep<Lookup: LookupExport + LookupAnswer>(
    /// First you get given the `ModuleSteps`, from which you should grab what you need (cloning it).
    /// Second you get given the configs, from which you should compute the result.
    /// Thrid you get given the `ModuleSteps` to update.
    pub Box<dyn Fn(&Steps) -> Box<dyn FnOnce(&Context<Lookup>) -> Box<dyn FnOnce(&mut Steps)>>>,
);

macro_rules! compute_step {
    (<$ty:ty> $output:ident = $($input:ident),* $(,)? $(#$old_input:ident),*) => {
        ComputeStep(Box::new(|steps: &Steps| {
            let _ = steps; // Not used if $input is empty.
            $(let $input = steps.$input.dupe().unwrap();)*
            $(let $old_input = steps.$old_input.dupe();)*
            Box::new(move |ctx: &Context<$ty>| {
                let res = paste! { Step::[<step_ $output>] }(ctx, $($input,)* $($old_input,)* );
                Box::new(move |steps: &mut Steps| {
                    steps.$output = Some(res);
                    steps.last_step = Some(paste! { Step::[<$output:camel>] });
                })
            })
        }))
    };
}

// The steps within this module are all marked `inline(never)` and given
// globally unique names, so they are much easier to find in the profile.
impl Step {
    pub fn first() -> Self {
        Sequence::first().unwrap()
    }

    pub fn last() -> Self {
        Sequence::last().unwrap()
    }

    pub fn compute<Lookup: LookupExport + LookupAnswer>(self) -> ComputeStep<Lookup> {
        match self {
            Step::Load => compute_step!(<Lookup> load =),
            Step::Ast => compute_step!(<Lookup> ast = load),
            Step::Exports => compute_step!(<Lookup> exports = load, ast),
            Step::Answers => compute_step!(<Lookup> answers = load, ast, exports, #solutions),
            Step::Solutions => compute_step!(<Lookup> solutions = load, answers),
        }
    }

    #[inline(never)]
    fn step_load<Lookup>(ctx: &Context<Lookup>) -> Arc<Load> {
        let error_style = match ctx.loader.find(ctx.module) {
            Ok((_, s)) => s,
            Err(_) => {
                // We shouldn't reach here, as we must be able to load the module to get here.
                // But if we do, delayed is fairly safe.
                ErrorStyle::Delayed
            }
        };
        let (code, self_error) = Load::load_from_path(ctx.path, ctx.loader);
        Arc::new(Load::load_from_data(
            ctx.module,
            ctx.path.dupe(),
            error_style,
            code,
            self_error,
        ))
    }

    #[inline(never)]
    fn step_ast<Lookup>(_ctx: &Context<Lookup>, load: Arc<Load>) -> Arc<ModModule> {
        Arc::new(load.module_info.parse(&load.errors))
    }

    #[inline(never)]
    fn step_exports<Lookup>(
        ctx: &Context<Lookup>,
        load: Arc<Load>,
        ast: Arc<ModModule>,
    ) -> Exports {
        Exports::new(&ast.body, &load.module_info, ctx.config)
    }

    #[inline(never)]
    fn step_answers<Lookup: LookupExport>(
        ctx: &Context<Lookup>,
        load: Arc<Load>,
        ast: Arc<ModModule>,
        exports: Exports,
        previous_solutions: Option<Arc<(IdCacheHistory, Arc<Solutions>)>>,
    ) -> Arc<(Bindings, Arc<Answers>)> {
        let solver = Solver::new();
        let enable_trace = ctx.retain_memory;
        let bindings = Bindings::new(
            ast.range,
            Arc::unwrap_or_clone(ast).body,
            load.module_info.dupe(),
            exports,
            &solver,
            ctx.lookup,
            ctx.config,
            &load.errors,
            ctx.uniques,
            enable_trace,
        );
        let history = previous_solutions
            .as_ref()
            .map(|s| s.0.clone())
            .unwrap_or_default();
        let answers = Answers::new(&bindings, solver, history, enable_trace);
        Arc::new((bindings, Arc::new(answers)))
    }

    #[inline(never)]
    fn step_solutions<Lookup: LookupExport + LookupAnswer>(
        ctx: &Context<Lookup>,
        load: Arc<Load>,
        answers: Arc<(Bindings, Arc<Answers>)>,
    ) -> Arc<(IdCacheHistory, Arc<Solutions>)> {
        let solutions = answers.1.solve(
            ctx.lookup,
            ctx.lookup,
            &answers.0,
            &load.errors,
            ctx.stdlib,
            ctx.uniques,
        );
        let history = answers.1.id_cache_history();
        Arc::new((history, Arc::new(solutions)))
    }
}
