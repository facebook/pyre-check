/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use tracing::info;

use crate::config::Config;
use crate::error::error::Error;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::state::loader::Loader;
use crate::state::state::State;

pub struct Driver(pub State<'static>);

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

    pub fn module_info(&self, module: ModuleName) -> Option<ModuleInfo> {
        self.0.get_module_info(module)
    }
}

/// Some messages are useful to have in both stdout and stderr, so print them twice
fn info_eprintln(msg: String) {
    info!("{}", msg);
    eprintln!("{}", msg);
}
