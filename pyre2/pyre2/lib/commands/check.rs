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
use std::path::Path;
use std::path::PathBuf;
use std::str::FromStr;
use std::time::Duration;
use std::time::Instant;

use anyhow::Context as _;
use clap::Parser;
use clap::ValueEnum;
use dupe::Dupe;
use starlark_map::small_map::SmallMap;
use tracing::info;

use crate::clap_env;
use crate::commands::common::CommonArgs;
use crate::commands::util::module_from_path;
use crate::config::ConfigFile;
use crate::error::error::Error;
use crate::error::legacy::LegacyErrors;
use crate::error::style::ErrorStyle;
use crate::metadata::PythonVersion;
use crate::metadata::RuntimeMetadata;
use crate::module::bundled::typeshed;
use crate::module::finder::find_module;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::report;
use crate::run::CommandExitStatus;
use crate::state::handle::Handle;
use crate::state::loader::FindError;
use crate::state::loader::Loader;
use crate::state::loader::LoaderId;
use crate::state::state::State;
use crate::util::display::number_thousands;
use crate::util::forgetter::Forgetter;
use crate::util::fs_anyhow;
use crate::util::globs::Globs;
use crate::util::memory::MemoryUsageTrace;
use crate::util::prelude::VecExt;
use crate::util::watcher::Watcher;

#[derive(Debug, Clone, ValueEnum, Default)]
enum OutputFormat {
    #[default]
    Text,
    Json,
}

#[derive(Debug, Parser, Clone)]
pub struct Args {
    files: Vec<String>,
    /// Write the errors to a file, instead of printing them.
    #[arg(long, short = 'o', env = clap_env("OUTPUT"))]
    output: Option<PathBuf>,
    #[clap(long, short = 'I', env = clap_env("INCLUDE"))]
    include: Vec<PathBuf>,
    #[clap(long, value_enum, default_value_t, env = clap_env("OUTPUT_FORMAT"))]
    output_format: OutputFormat,
    /// Check all reachable modules, not just the ones that are passed in explicitly on CLI positional arguments.
    #[clap(long, short = 'a', env = clap_env("CHECK_ALL"))]
    check_all: bool,
    /// Watch for file changes and re-check them.
    #[clap(long, env = clap_env("WATCH"))]
    pub watch: bool,
    /// Produce debugging information about the type checking process.
    #[clap(long, env = clap_env("DEBUG_INFO"))]
    debug_info: Option<PathBuf>,
    #[clap(long, env = clap_env("REPORT_BINDING_MEMORY"))]
    report_binding_memory: Option<PathBuf>,
    #[clap(
        long,
        default_missing_value = "5",
        require_equals = true,
        num_args = 0..=1,
        env = clap_env("SUMMARIZE_ERRORS")
    )]
    summarize_errors: Option<usize>,
    #[clap(long, env = clap_env("PYTHON_VERSION"))]
    python_version: Option<String>,
    /// Check against any `E:` lines in the file.
    #[clap(long, env = clap_env("EXPECTATIONS"))]
    expectations: bool,

    #[clap(flatten)]
    common: CommonArgs,
}

#[derive(Debug, Clone)]
struct CheckLoader {
    sources: SmallMap<ModuleName, PathBuf>,
    search_roots: Vec<PathBuf>,
    error_style_for_sources: ErrorStyle,
    error_style_for_dependencies: ErrorStyle,
}

impl Loader for CheckLoader {
    fn find(&self, module: ModuleName) -> Result<(ModulePath, ErrorStyle), FindError> {
        if let Some(path) = self.sources.get(&module) {
            // FIXME: Because we pre-created these handles, the only real reason to do this
            // is for the error-style, which is a pretty weird reason to do it.
            Ok((
                ModulePath::filesystem(path.clone()),
                self.error_style_for_sources,
            ))
        } else if let Some(path) = find_module(module, &self.search_roots) {
            Ok((path, self.error_style_for_dependencies))
        } else if let Some(path) = typeshed().map_err(FindError::new)?.find(module) {
            Ok((path, self.error_style_for_dependencies))
        } else {
            Err(FindError::search_path(&self.search_roots))
        }
    }
}

impl OutputFormat {
    fn write_error_text_to_file(path: &Path, errors: &[Error]) -> anyhow::Result<()> {
        let mut file = BufWriter::new(File::create(path)?);
        for e in errors {
            writeln!(file, "{e}")?;
        }
        file.flush()?;
        Ok(())
    }

    fn write_error_json_to_file(path: &Path, errors: &[Error]) -> anyhow::Result<()> {
        let legacy_errors = LegacyErrors::from_errors(errors);
        let output_bytes = serde_json::to_string_pretty(&legacy_errors)
            .with_context(|| "failed to serialize JSON value to bytes")?;
        fs_anyhow::write(path, output_bytes.as_bytes())?;
        Ok(())
    }

    fn write_errors_to_file(&self, path: &Path, errors: &[Error]) -> anyhow::Result<()> {
        match self {
            Self::Text => Self::write_error_text_to_file(path, errors),
            Self::Json => Self::write_error_json_to_file(path, errors),
        }
    }
}

impl Args {
    pub fn run(
        self,
        watcher: Option<Box<dyn Watcher>>,
        config_finder: &dyn Fn(&Path) -> ConfigFile,
        allow_forget: bool,
    ) -> anyhow::Result<CommandExitStatus> {
        if let Some(watcher) = watcher {
            self.run_watch(watcher, config_finder)?;
            Ok(CommandExitStatus::Success)
        } else {
            self.run_inner(config_finder, allow_forget)
        }
    }

    fn run_watch(
        self,
        mut watcher: Box<dyn Watcher>,
        config_finder: &dyn Fn(&Path) -> ConfigFile,
    ) -> anyhow::Result<()> {
        for path in Globs::new(self.files.clone()).roots() {
            watcher.watch_dir(&path)?;
        }
        loop {
            let res = self.clone().run_inner(config_finder, false);
            if let Err(e) = res {
                eprintln!("{e:#}");
            }
            loop {
                let events = watcher.wait()?;
                if events.iter().any(|x| !x.kind.is_access()) {
                    break;
                }
            }
        }
    }

    fn run_inner(
        self,
        // TODO: use this to calculate the config for each checked file
        _config_finder: &dyn Fn(&Path) -> ConfigFile,
        allow_forget: bool,
    ) -> anyhow::Result<CommandExitStatus> {
        let args = self;
        let include = args.include;

        let files = Globs::new(args.files).resolve()?;
        if files.is_empty() {
            return Ok(CommandExitStatus::Success);
        }

        let mut to_check = SmallMap::with_capacity(files.len());
        for file in &files {
            let module = module_from_path(file, &include);
            to_check.entry(module).or_insert_with(|| file.clone());
        }
        let config = match &args.python_version {
            None => RuntimeMetadata::default(),
            Some(version) => {
                RuntimeMetadata::new(PythonVersion::from_str(version)?, "linux".to_owned())
            }
        };
        let error_style_for_sources = ErrorStyle::Delayed;
        let loader = LoaderId::new(CheckLoader {
            sources: to_check.clone(),
            search_roots: include.clone(),
            error_style_for_sources,
            error_style_for_dependencies: if args.check_all {
                error_style_for_sources
            } else {
                ErrorStyle::Never
            },
        });
        let handles = files.into_map(|x| {
            Handle::new(
                module_from_path(&x, &include),
                ModulePath::filesystem(x),
                config.dupe(),
                loader.dupe(),
            )
        });

        let mut memory_trace = MemoryUsageTrace::start(Duration::from_secs_f32(0.1));
        let start = Instant::now();
        let state = State::new(args.common.parallel());
        let mut holder = Forgetter::new(state, allow_forget);
        let state = holder.as_mut();

        if args.report_binding_memory.is_none() && args.debug_info.is_none() {
            state.run_one_shot(&handles, None)
        } else {
            state.run(&handles, None)
        };
        let computing = start.elapsed();
        if let Some(path) = args.output {
            let errors = state.collect_errors();
            args.output_format.write_errors_to_file(&path, &errors)?;
        } else {
            state.print_errors();
        }
        let printing = start.elapsed();
        memory_trace.stop();
        if let Some(limit) = args.summarize_errors {
            state.print_error_summary(limit);
        }
        let count_errors = state.count_errors();
        info!(
            "{} errors, {} modules, took {printing:.2?} ({computing:.2?} without printing errors), peak memory {}",
            number_thousands(count_errors),
            number_thousands(state.module_count()),
            memory_trace.peak()
        );
        if let Some(debug_info) = args.debug_info {
            let mut output = serde_json::to_string_pretty(&state.debug_info(&handles))?;
            if debug_info.extension() == Some(OsStr::new("js")) {
                output = format!("var data = {output}");
            }
            fs_anyhow::write(&debug_info, output.as_bytes())?;
        }
        if let Some(path) = args.report_binding_memory {
            fs_anyhow::write(
                &path,
                report::binding_memory::binding_memory(state).as_bytes(),
            )?;
        }
        if args.expectations {
            state.check_against_expectations()?;
            Ok(CommandExitStatus::Success)
        } else if count_errors > 0 {
            Ok(CommandExitStatus::UserError)
        } else {
            Ok(CommandExitStatus::Success)
        }
    }
}
