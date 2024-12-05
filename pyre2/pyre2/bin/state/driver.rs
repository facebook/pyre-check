/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use ruff_python_ast::ModModule;
use tracing::info;

use crate::alt::answers::Solutions;
use crate::alt::binding::Key;
use crate::alt::bindings::Bindings;
use crate::config::Config;
use crate::error::error::Error;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::state::loader::Loader;
use crate::state::state::State;
#[cfg(test)]
use crate::types::class_metadata::ClassMetadata;
use crate::types::types::Type;

pub struct Driver(State<'static>);

impl Driver {
    pub fn new(
        modules: &[ModuleName],
        loader: Box<Loader<'static>>,
        config: &Config,
        parallel: bool,
        timings: Option<usize>,
    ) -> Self {
        let mut state = State::new(modules, loader, config.clone(), parallel, timings.is_none());
        state.run();
        if timings.is_some() {
            state.print_errors();
        }
        // Print this on both stderr and stdout, since handy to have in either dump
        info_eprintln(format!("Total errors: {}", state.count_errors()));
        Driver(state)
    }

    pub fn errors(&self) -> Vec<Error> {
        self.0.collect_errors()
    }

    pub fn errors_in_checked_modules(&self) -> Vec<Error> {
        self.errors()
            .iter()
            .filter(|x| x.is_in_checked_module())
            .cloned()
            .collect()
    }

    pub fn module_info(&self, module: ModuleName) -> Option<ModuleInfo> {
        self.0.get_module_info(module)
    }

    #[cfg(test)]
    pub fn check_against_expectations(&self) -> anyhow::Result<()> {
        self.0.check_against_expectations()
    }

    #[cfg(test)]
    pub fn class_metadata_of_export(
        &self,
        module: ModuleName,
        name: &str,
    ) -> Option<ClassMetadata> {
        use ruff_python_ast::name::Name;

        use crate::alt::binding::KeyClassMetadata;
        use crate::alt::binding::KeyExported;
        use crate::module::short_identifier::ShortIdentifier;

        let solutions = self.get_solutions(module).unwrap();

        match solutions
            .exported_types
            .get(&KeyExported::Export(Name::new(name)))
        {
            Some(Type::ClassDef(cls)) => {
                println!("Class {cls:?}");
                let x = solutions
                    .mros
                    .get(&KeyClassMetadata(ShortIdentifier::new(cls.name())));
                x.cloned()
            }
            _ => None,
        }
    }

    pub fn get_mod_module(&self, module: ModuleName) -> Option<Arc<ModModule>> {
        self.0.get_ast(module)
    }

    pub fn get_bindings(&self, module: ModuleName) -> Option<Bindings> {
        self.0.get_bindings(module)
    }

    pub fn get_solutions(&self, module: ModuleName) -> Option<Arc<Solutions>> {
        self.0.get_solutions(module)
    }

    pub fn get_type(&self, module: ModuleName, key: &Key) -> Option<Type> {
        self.get_solutions(module)?.types.get(key).cloned()
    }
}

/// Some messages are useful to have in both stdout and stderr, so print them twice
fn info_eprintln(msg: String) {
    info!("{}", msg);
    eprintln!("{}", msg);
}
