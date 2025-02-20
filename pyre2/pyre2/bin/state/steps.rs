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
use ruff_python_ast::ModModule;
use ruff_text_size::TextRange;

use crate::alt::answers::Answers;
use crate::alt::answers::LookupAnswer;
use crate::alt::answers::Solutions;
use crate::binding::bindings::Bindings;
use crate::config::Config;
use crate::error::collector::ErrorCollector;
use crate::error::style::ErrorStyle;
use crate::export::exports::Exports;
use crate::export::exports::LookupExport;
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
    pub module: ModuleName,
    pub path: &'a ModulePath,
    pub config: &'a Config,
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

#[derive(Debug, Default)]
pub struct Steps {
    pub load: Option<Arc<Load>>,
    pub ast: Option<Arc<ModModule>>,
    pub exports: Option<Exports>,
    pub answers: Option<Arc<(Bindings, Arc<Answers>)>>,
    pub solutions: Option<Arc<Solutions>>,
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
    pub  Box<
        dyn for<'a> Fn(&Steps) -> Box<dyn FnOnce(&Context<Lookup>) -> Box<dyn FnOnce(&mut Steps)>>,
    >,
);

macro_rules! compute_step {
    (<$ty:ty> $output:ident = $($input:ident),*) => {
        ComputeStep(Box::new(|steps: &Steps| {
            let _ = steps; // Not used if $input is empty.
            $(let $input = steps.$input.dupe().unwrap();)*
            Box::new(move |ctx: &Context<$ty>| {
                let res = Step::$output(ctx, $($input),*);
                Box::new(move |steps: &mut Steps| {
                    steps.$output = Some(res);
                })
            })
        }))
    };
}

impl Step {
    pub fn check(self, steps: &Steps) -> bool {
        match self {
            Step::Load => steps.load.is_some(),
            Step::Ast => steps.ast.is_some(),
            Step::Exports => steps.exports.is_some(),
            Step::Answers => steps.answers.is_some(),
            Step::Solutions => steps.solutions.is_some(),
        }
    }

    pub fn first() -> Self {
        Sequence::first().unwrap()
    }

    pub fn last() -> Self {
        Sequence::last().unwrap()
    }

    /// If you want to have access to the given step, the next step to compute is returned.
    /// None means the step is already available.
    pub fn compute_next(self, steps: &Steps) -> Option<Self> {
        if self.check(steps) {
            return None;
        }
        let mut res = self;
        while let Some(prev) = res.previous()
            && !prev.check(steps)
        {
            res = prev;
        }
        Some(res)
    }

    pub fn compute<Lookup: LookupExport + LookupAnswer>(self) -> ComputeStep<Lookup> {
        match self {
            Step::Load => compute_step!(<Lookup> load =),
            Step::Ast => compute_step!(<Lookup> ast = load),
            Step::Exports => compute_step!(<Lookup> exports = load, ast),
            Step::Answers => compute_step!(<Lookup> answers = load, ast, exports),
            Step::Solutions => compute_step!(<Lookup> solutions = load, answers),
        }
    }

    fn load<Lookup>(ctx: &Context<Lookup>) -> Arc<Load> {
        let error_style = match ctx.loader.find(ctx.module) {
            Ok((_, s)) => s,
            Err(_) => {
                // We shouldn't reach here, as we must be able to load the module to get here.
                // But if we do, delayed is fairly safe.
                ErrorStyle::Delayed
            }
        };

        let res = match ctx.path.details() {
            ModulePathDetails::FileSystem(path) => fs_anyhow::read_to_string(path).map(Arc::new),
            ModulePathDetails::Namespace(_) => Ok(Arc::new("".to_owned())),
            ModulePathDetails::Memory(path) => ctx
                .loader
                .load_from_memory(path)
                .ok_or_else(|| anyhow!("memory path not found")),
            ModulePathDetails::BundledTypeshed(path) => typeshed().and_then(|x| {
                x.load(path)
                    .ok_or_else(|| anyhow!("bundled typeshed problem"))
            }),
        };
        let (code, self_error) = match res {
            Err(err) => (Arc::new(String::new()), Some(err)),
            Ok(res) => (res, None),
        };

        let module_info = ModuleInfo::new(ctx.module, ctx.path.dupe(), code);
        let errors = ErrorCollector::new(error_style);
        if let Some(err) = self_error {
            errors.add(
                &module_info,
                TextRange::default(),
                format!(
                    "Failed to load `{}` from `{}`, got {err:#}",
                    ctx.module,
                    module_info.path()
                ),
            );
        }

        Arc::new(Load {
            errors,
            module_info,
        })
    }

    fn ast<Lookup>(_ctx: &Context<Lookup>, load: Arc<Load>) -> Arc<ModModule> {
        Arc::new(load.module_info.parse(&load.errors))
    }

    fn exports<Lookup>(ctx: &Context<Lookup>, load: Arc<Load>, ast: Arc<ModModule>) -> Exports {
        Exports::new(&ast.body, &load.module_info, ctx.config)
    }

    fn answers<Lookup: LookupExport>(
        ctx: &Context<Lookup>,
        load: Arc<Load>,
        ast: Arc<ModModule>,
        exports: Exports,
    ) -> Arc<(Bindings, Arc<Answers>)> {
        let solver = Solver::new();
        let bindings = Bindings::new(
            Arc::unwrap_or_clone(ast).body,
            load.module_info.dupe(),
            exports,
            &solver,
            ctx.lookup,
            ctx.config,
            &load.errors,
            ctx.uniques,
        );
        let answers = Answers::new(&bindings, solver);
        Arc::new((bindings, Arc::new(answers)))
    }

    fn solutions<Lookup: LookupExport + LookupAnswer>(
        ctx: &Context<Lookup>,
        load: Arc<Load>,
        answers: Arc<(Bindings, Arc<Answers>)>,
    ) -> Arc<Solutions> {
        Arc::new(answers.1.solve(
            ctx.lookup,
            ctx.lookup,
            &answers.0,
            &load.errors,
            ctx.stdlib,
            ctx.uniques,
        ))
    }
}
