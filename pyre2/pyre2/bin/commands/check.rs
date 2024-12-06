/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ffi::OsStr;
use std::fs::File;
use std::io::BufWriter;
use std::io::Write;
use std::mem;
use std::path::PathBuf;
use std::str::FromStr;
use std::time::Duration;
use std::time::Instant;

use anyhow::Context as _;
use clap::Parser;
use starlark_map::small_map::SmallMap;
use tracing::info;

use crate::commands::common::CommonArgs;
use crate::commands::util::default_include;
use crate::commands::util::find_module;
use crate::commands::util::module_from_path;
use crate::config::Config;
use crate::config::PythonVersion;
use crate::error::legacy::LegacyErrors;
use crate::error::style::ErrorStyle;
use crate::report;
use crate::state::loader::LoadResult;
use crate::state::state::State;
use crate::util::display::number_thousands;
use crate::util::fs_anyhow;
use crate::util::memory::MemoryUsageTrace;

#[derive(Debug, Parser, Clone)]
pub struct Args {
    files: Vec<PathBuf>,
    #[arg(long, short = 'o')]
    output: Option<PathBuf>,
    #[clap(long, short = 'I')]
    include: Vec<PathBuf>,
    /// Write the errors to a file, instead of printing them.
    #[clap(long)]
    report_errors: Option<PathBuf>,
    /// Produce debugging information about the type checking process.
    #[clap(long)]
    debug_info: Option<PathBuf>,
    #[clap(long)]
    report_binding_memory: Option<PathBuf>,
    #[clap(
        long,
        default_missing_value = "5",
        require_equals = true,
        num_args = 0..=1
    )]
    summarize_errors: Option<usize>,
    #[clap(long)]
    python_version: Option<String>,

    #[clap(flatten)]
    common: CommonArgs,
}

impl Args {
    pub fn run(self, allow_forget: bool) -> anyhow::Result<()> {
        let args = self;
        let include = if args.include.is_empty() {
            default_include()?
        } else {
            args.include
        };

        if args.files.is_empty() {
            return Ok(());
        }

        let mut to_check = SmallMap::with_capacity(args.files.len());
        for file in &args.files {
            let module = module_from_path(file, &include);
            if let Some(old_file) = to_check.insert(module, file) {
                return Err(anyhow::anyhow!(
                    "Two files map to the same module: `{}` and `{}` both map to `{module}`",
                    file.display(),
                    old_file.display()
                ));
            }
        }
        let error_style = if args.report_errors.is_some() {
            ErrorStyle::Delayed
        } else {
            ErrorStyle::Immediate
        };
        let load = |name| {
            let path = match to_check.get(&name) {
                Some(path) => Ok((*path).clone()),
                None => find_module(name, &include),
            };
            (LoadResult::from_path_result(path), error_style)
        };
        let modules = to_check.keys().copied().collect::<Vec<_>>();
        let config = match &args.python_version {
            None => Config::default(),
            Some(version) => Config::new(PythonVersion::from_str(version)?, "linux".to_owned()),
        };

        let mut memory_trace = MemoryUsageTrace::start(Duration::from_secs_f32(0.1));
        let start = Instant::now();
        let mut state = State::new(Box::new(load), config, args.common.parallel());
        if args.report_binding_memory.is_none() && args.debug_info.is_none() {
            state.run_one_shot(&modules)
        } else {
            state.run(&modules)
        };
        let error_count = state.count_errors();
        let computing = start.elapsed();
        if let Some(file) = args.report_errors {
            let mut file = BufWriter::new(File::create(file)?);
            for e in state.collect_errors() {
                writeln!(file, "{e}")?;
            }
            file.flush()?;
        }
        let printing = start.elapsed();
        memory_trace.stop();
        if let Some(limit) = args.summarize_errors {
            state.print_error_summary(limit);
        }
        info!(
            "{} errors, took {printing:.2?} ({computing:.2?} without printing errors), peak memory {}",
            number_thousands(error_count),
            memory_trace.peak()
        );
        if let Some(debug_info) = args.debug_info {
            let mut output = serde_json::to_string_pretty(&state.debug_info(&modules))?;
            if debug_info.extension() == Some(OsStr::new("js")) {
                output = format!("var data = {output}");
            }
            fs_anyhow::write(&debug_info, output.as_bytes())?;
        }
        if let Some(path) = args.report_binding_memory {
            fs_anyhow::write(
                &path,
                report::binding_memory::binding_memory(&state).as_bytes(),
            )?;
        }
        if let Some(path) = args.output {
            let errors = state.collect_errors();
            let legacy_errors = LegacyErrors::from_errors(&errors);
            let output_bytes = serde_json::to_string_pretty(&legacy_errors)
                .with_context(|| "failed to serialize JSON value to bytes")?;
            fs_anyhow::write(&path, output_bytes.as_bytes())?;
        } else {
            state.check_against_expectations()?;
        }
        if allow_forget {
            // We have allocated a bunch of memory, that we will never need, and are exiting the program.
            // Rather than deallocate it, just leak it, and let the OS clean up for us.
            mem::forget(state);
        }
        Ok(())
    }
}
