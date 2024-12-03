/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use enum_iterator::Sequence;
use parse_display::Display;
use ruff_python_ast::ModModule;

use crate::alt::answers::Answers;
use crate::alt::answers::LookupAnswer;
use crate::alt::answers::Solutions;
use crate::alt::bindings::Bindings;
use crate::alt::exports::Exports;
use crate::alt::exports::LookupExport;
use crate::config::Config;
use crate::error::collector::ErrorCollector;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::state::info::Info;
use crate::state::loader::Loader;
use crate::types::stdlib::Stdlib;
use crate::uniques::UniqueFactory;

pub struct Context<'a, Lookup> {
    pub name: ModuleName,
    pub config: &'a Config,
    pub loader: &'a Loader<'a>,
    pub uniques: &'a UniqueFactory,
    pub stdlib: &'a Stdlib,
    pub errors: &'a ErrorCollector,
    pub lookup: &'a Lookup,
}

#[derive(Debug, Default)]
pub struct ModuleSteps {
    pub module_info: Info<(ModuleInfo, Arc<Option<anyhow::Error>>)>,
    pub ast: Info<Arc<ModModule>>,
    pub exports: Info<Exports>,
    pub bindings: Info<Bindings>,
    pub answers: Info<Arc<Answers>>,
    pub solutions: Info<Arc<Solutions>>,
}

impl ModuleSteps {
    pub fn clear(&mut self) {
        self.module_info.clear();
        self.ast.clear();
        self.exports.clear();
        self.bindings.clear();
        self.answers.clear();
        self.solutions.clear();
    }
}

#[derive(
    Debug, Clone, Copy, Dupe, Eq, PartialEq, PartialOrd, Ord, Display, Sequence
)]
pub enum Step {
    ModuleInfo,
    Ast,
    Exports,
    Bindings,
    Answers,
    Solutions,
}

pub struct ComputeStep<Lookup: LookupExport + LookupAnswer>(
    /// First you get given the `ModuleSteps`, from which you should grab what you need (cloning it).
    /// Second you get given the configs, from which you should compute the result.
    /// Thrid you get given the `ModuleSteps` to update.
    pub  Box<
        dyn for<'a> Fn(
            &ModuleSteps,
        )
            -> Box<dyn FnOnce(&Context<Lookup>) -> Box<dyn FnOnce(&mut ModuleSteps)>>,
    >,
);

macro_rules! compute_step {
    (<$ty:ty> $output:ident = $($input:ident),*) => {
        ComputeStep(Box::new(|steps: &ModuleSteps| {
            let _ = steps; // Not used if $input is empty.
            $(let $input = steps.$input.get().unwrap().dupe();)*
            Box::new(move |ctx: &Context<$ty>| {
                let info = Info::with(move || Step::$output(ctx, $($input),*));
                Box::new(move |steps: &mut ModuleSteps| {
                    steps.$output = info;
                })
            })
        }))
    };
}

impl Step {
    pub fn check(self, steps: &ModuleSteps) -> bool {
        match self {
            Step::ModuleInfo => steps.module_info.is_some(),
            Step::Ast => steps.ast.is_some(),
            Step::Exports => steps.exports.is_some(),
            Step::Bindings => steps.bindings.is_some(),
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
    pub fn compute_next(self, steps: &ModuleSteps) -> Option<Self> {
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
            Step::ModuleInfo => compute_step!(<Lookup> module_info =),
            Step::Ast => compute_step!(<Lookup> ast = module_info),
            Step::Exports => compute_step!(<Lookup> exports = module_info, ast),
            Step::Bindings => compute_step!(<Lookup> bindings = module_info, ast),
            Step::Answers => compute_step!(<Lookup> answers = bindings),
            Step::Solutions => compute_step!(<Lookup> solutions = bindings, answers),
        }
    }

    fn module_info<Lookup>(ctx: &Context<Lookup>) -> (ModuleInfo, Arc<Option<anyhow::Error>>) {
        let (load_result, should_type_check) = (ctx.loader)(ctx.name);
        let components = load_result.components(ctx.name);
        let module_info = ModuleInfo::new(
            ctx.name,
            components.path,
            components.code,
            should_type_check,
        );
        (module_info, Arc::new(components.import_error))
    }

    fn ast<Lookup>(
        ctx: &Context<Lookup>,
        module_info: (ModuleInfo, Arc<Option<anyhow::Error>>),
    ) -> Arc<ModModule> {
        Arc::new(module_info.0.parse(ctx.errors))
    }

    fn exports<Lookup>(
        ctx: &Context<Lookup>,
        module_info: (ModuleInfo, Arc<Option<anyhow::Error>>),
        ast: Arc<ModModule>,
    ) -> Exports {
        Exports::new(&ast.body, &module_info.0, ctx.config)
    }

    fn bindings<Lookup: LookupExport>(
        ctx: &Context<Lookup>,
        module_info: (ModuleInfo, Arc<Option<anyhow::Error>>),
        ast: Arc<ModModule>,
    ) -> Bindings {
        Bindings::new(
            Arc::unwrap_or_clone(ast).body,
            module_info.0,
            ctx.lookup,
            ctx.config,
            ctx.errors,
            ctx.uniques,
        )
    }

    fn answers<Lookup>(_ctx: &Context<Lookup>, bindings: Bindings) -> Arc<Answers> {
        Arc::new(Answers::new(&bindings))
    }

    fn solutions<Lookup: LookupExport + LookupAnswer>(
        ctx: &Context<Lookup>,
        bindings: Bindings,
        answers: Arc<Answers>,
    ) -> Arc<Solutions> {
        Arc::new(answers.solve(
            ctx.lookup,
            ctx.lookup,
            &bindings,
            ctx.errors,
            ctx.stdlib,
            ctx.uniques,
            true,
        ))
    }
}
