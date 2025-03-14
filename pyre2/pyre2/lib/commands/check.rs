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
use std::time::Duration;
use std::time::Instant;

use anyhow::Context as _;
use clap::Parser;
use clap::ValueEnum;
use dupe::Dupe;
use starlark_map::small_map::SmallMap;
use tracing::info;

use crate::clap_env;
use crate::commands::util::module_from_path;
use crate::config::set_if_some;
use crate::config::ConfigFile;
use crate::error::error::Error;
use crate::error::legacy::LegacyErrors;
use crate::metadata::PythonVersion;
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
use crate::state::require::Require;
use crate::state::state::State;
use crate::state::subscriber::ProgressBarSubscriber;
use crate::util::display::number_thousands;
use crate::util::forgetter::Forgetter;
use crate::util::fs_anyhow;
use crate::util::listing::FileList;
use crate::util::memory::MemoryUsageTrace;
use crate::util::prelude::SliceExt;
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
    /// Write the errors to a file, instead of printing them.
    #[arg(long, short = 'o', env = clap_env("OUTPUT"))]
    output: Option<PathBuf>,
    #[clap(long, short = 'I', env = clap_env("INCLUDE"))]
    include: Option<Vec<PathBuf>>,
    #[clap(long, value_enum, default_value_t, env = clap_env("OUTPUT_FORMAT"))]
    output_format: OutputFormat,
    /// Check all reachable modules, not just the ones that are passed in explicitly on CLI positional arguments.
    #[clap(long, short = 'a', env = clap_env("CHECK_ALL"))]
    check_all: bool,
    #[clap(long, env = clap_env("PYTHON_VERSION"))]
    python_version: Option<PythonVersion>,
    #[clap(long, env = clap_env("PLATFORM"))]
    python_platform: Option<String>,
    #[clap(long, env = clap_env("SITE_PACKAGE_PATH"))]
    site_package_path: Option<Vec<PathBuf>>,
    /// Produce debugging information about the type checking process.
    #[clap(long, env = clap_env("DEBUG_INFO"))]
    debug_info: Option<PathBuf>,
    #[clap(long, env = clap_env("REPORT_BINDING_MEMORY"))]
    report_binding_memory: Option<PathBuf>,
    #[clap(long, env = clap_env("REPORT_TRACE"))]
    report_trace: Option<PathBuf>,
    /// Count the number of each error kind. Prints the top N errors, sorted by count, or all errors if N is not specified.
    #[clap(
        long,
        default_missing_value = "5",
        require_equals = true,
        num_args = 0..=1,
        env = clap_env("COUNT_ERRORS")
    )]
    count_errors: Option<usize>,
    /// Check against any `E:` lines in the file.
    #[clap(long, env = clap_env("EXPECTATIONS"))]
    expectations: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct LoaderInputs {
    search_roots: Vec<PathBuf>,
    site_package_path: Vec<PathBuf>,
}

#[derive(Debug, Clone)]
struct CheckLoader {
    loader_inputs: LoaderInputs,
}

impl Loader for CheckLoader {
    fn find_import(&self, module: ModuleName) -> Result<ModulePath, FindError> {
        if let Some(path) = find_module(module, &self.loader_inputs.search_roots) {
            Ok(path)
        } else if let Some(path) = typeshed().map_err(FindError::new)?.find(module) {
            Ok(path)
        } else if let Some(path) = find_module(module, &self.loader_inputs.site_package_path) {
            Ok(path)
        } else {
            Err(FindError::search_path(
                &self.loader_inputs.search_roots,
                &self.loader_inputs.site_package_path,
            ))
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
        fn f(path: &Path, errors: &[Error]) -> anyhow::Result<()> {
            let legacy_errors = LegacyErrors::from_errors(errors);
            let mut file = BufWriter::new(File::create(path)?);
            serde_json::to_writer_pretty(&mut file, &legacy_errors)?;
            Ok(file.flush()?)
        }
        f(path, errors)
            .with_context(|| format!("while writing JSON errors to `{}`", path.display()))
    }

    fn write_errors_to_file(&self, path: &Path, errors: &[Error]) -> anyhow::Result<()> {
        match self {
            Self::Text => Self::write_error_text_to_file(path, errors),
            Self::Json => Self::write_error_json_to_file(path, errors),
        }
    }
}

fn create_loader(loader_inputs: LoaderInputs) -> LoaderId {
    LoaderId::new(CheckLoader { loader_inputs })
}

impl Args {
    pub fn run_once(
        self,
        files_to_check: impl FileList + Clone,
        config_finder: &dyn Fn(&Path) -> ConfigFile,
        allow_forget: bool,
    ) -> anyhow::Result<CommandExitStatus> {
        self.run_inner(files_to_check, config_finder, allow_forget)
    }

    pub async fn run_watch(
        self,
        mut watcher: Box<dyn Watcher>,
        files_to_check: impl FileList + Clone,
        config_finder: &dyn Fn(&Path) -> ConfigFile,
    ) -> anyhow::Result<()> {
        loop {
            let res = self
                .clone()
                .run_inner(files_to_check.clone(), config_finder, false);
            if let Err(e) = res {
                eprintln!("{e:#}");
            }
            loop {
                let events = watcher.wait().await?;
                if events.iter().any(|x| !x.kind.is_access()) {
                    break;
                }
            }
        }
    }

    fn override_config(&self, config: &mut ConfigFile) {
        set_if_some(&mut config.python_platform, self.python_platform.as_ref());
        set_if_some(&mut config.python_version, self.python_version.as_ref());
        set_if_some(&mut config.search_roots, self.include.as_ref());
        set_if_some(
            &mut config.site_package_path,
            self.site_package_path.as_ref(),
        );
    }

    fn run_inner(
        self,
        files_to_check: impl FileList,
        config_finder: &dyn Fn(&Path) -> ConfigFile,
        allow_forget: bool,
    ) -> anyhow::Result<CommandExitStatus> {
        let args = self;

        let expanded_file_list = files_to_check.files()?;
        if expanded_file_list.is_empty() {
            return Ok(CommandExitStatus::Success);
        }

        let files_and_configs = expanded_file_list.into_map(|path| {
            let mut config = config_finder(&path);
            args.override_config(&mut config);
            (path, config)
        });
        // We want to partition the files to check by their associated search roots, so we can
        // create a separate loader for each partition.
        let mut partition_by_loader_inputs: SmallMap<LoaderInputs, Vec<(PathBuf, ConfigFile)>> =
            SmallMap::new();
        for (path, config) in files_and_configs {
            let key = LoaderInputs {
                search_roots: config.search_roots.clone(),
                site_package_path: config.site_package_path.clone(),
            };
            partition_by_loader_inputs
                .entry(key)
                .or_default()
                .push((path, config));
        }

        let retain = args.report_binding_memory.is_some()
            || args.debug_info.is_some()
            || args.report_trace.is_some();

        let specified_require = if retain {
            Require::Everything
        } else {
            Require::Errors
        };
        let default_require = if retain {
            Require::Everything
        } else if args.check_all {
            Require::Errors
        } else {
            Require::Exports
        };

        let handles: Vec<(Handle, Require)> = partition_by_loader_inputs
            .into_iter()
            .flat_map(|(loader_inputs, files_and_configs)| {
                let files_with_module_name_and_metadata =
                    files_and_configs.into_map(|(path, config)| {
                        let module_name = module_from_path(&path, &loader_inputs.search_roots);
                        (path, module_name, config.get_runtime_metadata())
                    });
                let loader = create_loader(loader_inputs);
                files_with_module_name_and_metadata.into_map(
                    |(path, module_name, runtime_metadata)| {
                        (
                            Handle::new(
                                module_name,
                                ModulePath::filesystem(path),
                                runtime_metadata,
                                loader.dupe(),
                            ),
                            specified_require,
                        )
                    },
                )
            })
            .collect();

        let progress = Box::new(ProgressBarSubscriber::new());
        let mut memory_trace = MemoryUsageTrace::start(Duration::from_secs_f32(0.1));
        let start = Instant::now();
        let state = State::new();
        let mut holder = Forgetter::new(state, allow_forget);
        let state = holder.as_mut();

        state.run(&handles, default_require, Some(progress));
        let computing = start.elapsed();
        if let Some(path) = args.output {
            let errors = state.collect_errors();
            args.output_format.write_errors_to_file(&path, &errors)?;
        } else {
            state.print_errors();
        }
        let printing = start.elapsed();
        memory_trace.stop();
        if let Some(limit) = args.count_errors {
            state.print_error_counts(limit);
        }
        let error_count = state.count_errors();
        info!(
            "{} errors, {} modules, {} lines, took {printing:.2?} ({computing:.2?} without printing errors), peak memory {}",
            number_thousands(error_count),
            number_thousands(state.module_count()),
            number_thousands(state.line_count()),
            memory_trace.peak()
        );
        if let Some(debug_info) = args.debug_info {
            let mut output =
                serde_json::to_string_pretty(&state.debug_info(&handles.map(|x| x.0.dupe())))?;
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
        if let Some(path) = args.report_trace {
            fs_anyhow::write(&path, report::trace::trace(state).as_bytes())?;
        }
        if args.expectations {
            state.check_against_expectations()?;
            Ok(CommandExitStatus::Success)
        } else if error_count > 0 {
            Ok(CommandExitStatus::UserError)
        } else {
            Ok(CommandExitStatus::Success)
        }
    }
}
