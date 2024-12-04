/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ffi::OsStr;
use std::mem;
use std::path::PathBuf;
use std::time::Duration;
use std::time::Instant;

use anyhow::Context as _;
use clap::Parser;
use starlark_map::small_map::SmallMap;

use crate::commands::common::CommonArgs;
use crate::commands::util::default_include;
use crate::commands::util::find_module;
use crate::commands::util::module_from_path;
use crate::config::Config;
use crate::error::legacy::LegacyErrors;
use crate::report;
use crate::state::driver::Driver;
use crate::state::loader::LoadResult;
use crate::state::state::State;
use crate::util::display::number_thousands;
use crate::util::fs_anyhow;
use crate::util::memory::MemoryUsageTrace;

#[derive(Debug, Parser, Clone)]
pub struct Args {
    files: Vec<PathBuf>,
    #[arg(long = "output", short = 'o')]
    output_path: Option<PathBuf>,
    #[clap(long = "include", short = 'I')]
    include: Vec<PathBuf>,
    #[clap(long = "repeat", default_value = "1")]
    repeat: usize,
    /// Produce debugging information about the type checking process.
    #[clap(long = "debug-info")]
    debug_info: Option<PathBuf>,
    #[clap(long = "state2")]
    state2: bool,
    #[clap(long = "report-binding-memory")]
    report_binding_memory: Option<PathBuf>,

    #[clap(flatten)]
    common: CommonArgs,
}

impl Args {
    pub fn run(self) -> anyhow::Result<()> {
        let mut last = Ok(());
        for _ in 0..self.repeat {
            last = run_once(self.clone());
        }
        last
    }
}

pub fn run_once(args: Args) -> anyhow::Result<()> {
    let include = if args.include.is_empty() {
        default_include()?
    } else {
        args.include
    };

    if args.files.is_empty() {
        return Ok(());
    }

    let to_check = args
        .files
        .iter()
        .map(|x| (module_from_path(x), x.clone()))
        .collect::<SmallMap<_, _>>();
    let load = |name| {
        let path = match to_check.get(&name) {
            Some(path) => Ok(path.clone()),
            None => find_module(name, &include),
        };
        (LoadResult::from_path_result(path), true)
    };
    let modules = to_check.keys().copied().collect::<Vec<_>>();
    let config = Config::default();
    if args.state2 {
        let mut memory_trace = MemoryUsageTrace::start(Duration::from_secs_f32(0.1));
        let start = Instant::now();
        let mut state = State::new(
            &config,
            &load,
            args.common.parallel(),
            args.common.timings.is_none(),
            &modules,
        );
        if args.report_binding_memory.is_none() {
            state.run_one_shot()
        } else {
            state.run()
        };
        let error_count = state.count_errors();
        let computing = start.elapsed();
        if args.common.timings.is_some() {
            state.print_errors();
        }
        let printing = start.elapsed();
        memory_trace.stop();
        state.print_error_summary();
        eprintln!(
            "{} errors, took {printing:.2?} ({computing:.2?} without printing errors), peak memory {}",
            number_thousands(error_count),
            memory_trace.peak()
        );
        if let Some(path) = args.report_binding_memory {
            fs_anyhow::write(
                &path,
                report::binding_memory::binding_memory(&state).as_bytes(),
            )?;
        }
        if args.repeat == 1 {
            // We have allocated a bunch of memory, that we will never need, and are exiting the program.
            // Rather than deallocate it, just leak it, and let the OS clean up for us.
            mem::forget(state);
        }
    } else {
        let driver = Driver::new(
            &modules,
            &Config::default(),
            args.common.timings,
            args.common.parallel(),
            &load,
        );
        if let Some(debug_info) = args.debug_info {
            let mut output = serde_json::to_string_pretty(&driver.debug_info(&modules))?;
            if debug_info.extension() == Some(OsStr::new("js")) {
                output = format!("var data = {output}");
            }
            fs_anyhow::write(&debug_info, output.as_bytes())?;
        }
        if let Some(path) = args.output_path {
            let errors = driver.errors_in_checked_modules();
            let legacy_errors = LegacyErrors::from_errors(&errors);
            let output_bytes = serde_json::to_string_pretty(&legacy_errors)
                .with_context(|| "failed to serialize JSON value to bytes")?;
            fs_anyhow::write(&path, output_bytes.as_bytes())?;
        } else {
            driver.check_against_expectations()?;
        }
    }
    Ok(())
}
