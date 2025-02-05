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
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

use anyhow::Context as _;
use clap::Parser;
use clap::ValueEnum;
use itertools::Either;
use starlark_map::small_map::Entry;
use starlark_map::small_map::SmallMap;
use tracing::info;

use crate::commands::common::CommonArgs;
use crate::commands::util::module_from_path;
use crate::config::Config;
use crate::config::PythonVersion;
use crate::error::error::Error;
use crate::error::legacy::LegacyErrors;
use crate::error::style::ErrorStyle;
use crate::module::bundled::BundledTypeshed;
use crate::module::finder::find_module;
use crate::module::module_name::ModuleName;
use crate::module::module_path::ModulePath;
use crate::report;
use crate::state::loader::Loader;
use crate::state::loader::LoaderId;
use crate::state::state::State;
use crate::util::display::number_thousands;
use crate::util::forgetter::Forgetter;
use crate::util::fs_anyhow;
use crate::util::memory::MemoryUsageTrace;

#[derive(Debug, Clone, ValueEnum, Default)]
enum OutputFormat {
    #[default]
    Text,
    Json,
}

#[derive(Debug, Parser, Clone)]
pub struct Args {
    files: Vec<PathBuf>,
    /// Write the errors to a file, instead of printing them.
    #[arg(long, short = 'o')]
    output: Option<PathBuf>,
    #[clap(long, short = 'I')]
    include: Vec<PathBuf>,
    #[clap(long, value_enum, default_value_t)]
    output_format: OutputFormat,
    /// Check all reachable modules, not just the ones that are passed in explicitly on CLI positional arguments.
    #[clap(long, short = 'a')]
    check_all: bool,
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

#[derive(Debug, Clone)]
struct CheckLoader {
    sources: SmallMap<ModuleName, PathBuf>,
    typeshed: BundledTypeshed,
    search_roots: Vec<PathBuf>,
    error_style_for_sources: ErrorStyle,
    error_style_for_dependencies: ErrorStyle,
}

impl Loader for CheckLoader {
    fn load(
        &self,
        name: ModuleName,
    ) -> anyhow::Result<(ModulePath, Either<Arc<String>, PathBuf>, ErrorStyle)> {
        if let Some(path) = self.sources.get(&name) {
            Ok((
                ModulePath::filesystem(path.clone()),
                Either::Right(path.clone()),
                self.error_style_for_sources,
            ))
        } else if let Some(path) = find_module(name, &self.search_roots) {
            Ok((
                ModulePath::filesystem(path.clone()),
                Either::Right(path),
                self.error_style_for_dependencies,
            ))
        } else if let Some((path, content)) = self.typeshed.find(name) {
            Ok((
                path,
                Either::Left(content),
                self.error_style_for_dependencies,
            ))
        } else {
            Err(anyhow::anyhow!("Could not find path for `{name}`"))
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
    pub fn run(self, allow_forget: bool) -> anyhow::Result<()> {
        let args = self;
        let include = args.include;

        if args.files.is_empty() {
            return Ok(());
        }

        let mut to_check = SmallMap::with_capacity(args.files.len());
        for file in args.files {
            let module = module_from_path(&file, &include);
            match to_check.entry(module) {
                Entry::Vacant(new_entry) => {
                    new_entry.insert(file);
                }
                Entry::Occupied(old_entry) => {
                    return Err(anyhow::anyhow!(
                        "Two files map to the same module: `{}` and `{}` both map to `{module}`",
                        file.display(),
                        old_entry.get().display()
                    ));
                }
            }
        }
        let modules = to_check.keys().copied().collect::<Vec<_>>();
        let bundled_typeshed = BundledTypeshed::new()?;
        let config = match &args.python_version {
            None => Config::default(),
            Some(version) => Config::new(PythonVersion::from_str(version)?, "linux".to_owned()),
        };
        let error_style_for_sources = if args.output.is_some() {
            ErrorStyle::Delayed
        } else {
            ErrorStyle::Immediate
        };

        let mut memory_trace = MemoryUsageTrace::start(Duration::from_secs_f32(0.1));
        let start = Instant::now();
        let state = State::new(
            LoaderId::new(CheckLoader {
                sources: to_check,
                typeshed: bundled_typeshed,
                search_roots: include,
                error_style_for_sources,
                error_style_for_dependencies: if args.check_all {
                    error_style_for_sources
                } else {
                    ErrorStyle::Never
                },
            }),
            config,
            args.common.parallel(),
        );
        let mut holder = Forgetter::new(state, allow_forget);
        let state = holder.as_mut();

        if args.report_binding_memory.is_none() && args.debug_info.is_none() {
            state.run_one_shot(&modules)
        } else {
            state.run(&modules)
        };
        let computing = start.elapsed();
        if let Some(path) = args.output {
            let errors = state.collect_errors();
            args.output_format.write_errors_to_file(&path, &errors)?;
        } else {
            state.check_against_expectations()?;
        }
        let printing = start.elapsed();
        memory_trace.stop();
        if let Some(limit) = args.summarize_errors {
            state.print_error_summary(limit);
        }
        info!(
            "{} errors, {} modules, took {printing:.2?} ({computing:.2?} without printing errors), peak memory {}",
            number_thousands(state.count_errors()),
            number_thousands(state.module_count()),
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
                report::binding_memory::binding_memory(state).as_bytes(),
            )?;
        }
        Ok(())
    }
}
