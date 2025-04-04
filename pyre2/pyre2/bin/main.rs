/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::backtrace::Backtrace;
use std::env::args_os;
use std::path::Path;
use std::path::PathBuf;
use std::process::ExitCode;

use anyhow::anyhow;
use anyhow::Context as _;
use clap::Parser;
use clap::Subcommand;
use path_absolutize::Absolutize;
use pyrefly::clap_env;
use pyrefly::fs_upward_search::first_match;
use pyrefly::get_args_expanded;
use pyrefly::globs::FilteredGlobs;
use pyrefly::globs::Globs;
use pyrefly::init_thread_pool;
use pyrefly::init_tracing;
use pyrefly::run::BuckCheckArgs;
use pyrefly::run::CheckArgs;
use pyrefly::run::CommandExitStatus;
use pyrefly::run::CommonGlobalArgs;
use pyrefly::run::LspArgs;
use pyrefly::ConfigFile;
use pyrefly::NotifyWatcher;

#[derive(Debug, Parser)]
#[command(name = "pyrefly")]
#[command(about = "Next generation of Pyre type checker", long_about = None)]
#[command(version)]
struct Args {
    /// Enable verbose logging.
    #[clap(long = "verbose", short = 'v', global = true, env = clap_env("VERBOSE"))]
    verbose: bool,

    /// Set this to true to run profiling of fast jobs.
    /// Will run the command repeatedly.
    #[clap(long = "profiling", global = true, hide = true, env = clap_env("PROFILING"))]
    profiling: bool,

    #[clap(flatten)]
    common: CommonGlobalArgs,

    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Clone, Subcommand)]
enum Command {
    /// Full type checking on a file or a project
    Check {
        /// Files to check (glob supported).
        /// If no file is specified, switch to project-checking mode where the files to
        /// check are determined from the closest configuration file.
        /// When supplied, `project_excludes` in any config files loaded for these files to check
        /// are ignored, and we use the default excludes unless overridden with the `--project-excludes` flag.
        files: Vec<String>,
        /// Files to exclude when type checking.
        #[clap(long, env = clap_env("PROJECT_EXCLUDES"))]
        project_excludes: Option<Vec<String>>,
        /// Watch for file changes and re-check them.
        #[clap(long, env = clap_env("WATCH"), conflicts_with = "check_all")]
        watch: bool,

        /// Explicitly set the Pyre configuration to use when type checking or starting a language server.
        /// It is an error to pass this flag in "single-file checking mode".
        /// When not set, Pyre will perform an upward-filesystem-walk approach to find the nearest
        /// pyre.toml or 'pyproject.toml with `tool.pyre` section'. If no config is found, Pyre exits with error.
        /// If both a pyre.toml and valid pyproject.toml are found, pyre.toml takes precedence.
        #[clap(long, short, env = clap_env("CONFIG"))]
        config: Option<std::path::PathBuf>,

        #[clap(flatten)]
        args: CheckArgs,
    },

    /// Entry point for Buck integration
    BuckCheck(BuckCheckArgs),

    /// Start an LSP server
    Lsp(LspArgs),
}

fn exit_on_panic() {
    std::panic::set_hook(Box::new(move |info| {
        eprintln!("Thread panicked, shutting down: {}", info);
        eprintln!("Backtrace:\n{}", Backtrace::force_capture());
        std::process::exit(1);
    }));
}

fn get_open_source_config(file: &Path) -> anyhow::Result<ConfigFile> {
    ConfigFile::from_file(file, true).map_err(|err| {
        let file_str = file.display();
        anyhow!("Failed to parse configuration at {file_str}: {err}")
    })
}

fn get_implicit_config_path_from(path: &Path) -> anyhow::Result<PathBuf> {
    if let Some(search_result) = first_match(path, &["pyrefly.toml", "pyproject.toml"]) {
        Ok(search_result.to_path_buf())
    } else {
        Err(anyhow!(
            "Cannot locate a config file using upward-searching heuristics from `{}`",
            path.display()
        ))
    }
}

fn to_exit_code(status: CommandExitStatus) -> ExitCode {
    match status {
        CommandExitStatus::Success => ExitCode::SUCCESS,
        CommandExitStatus::UserError => ExitCode::FAILURE,
        // Exit code 2 is reserved for Meta-internal usages
        CommandExitStatus::InfraError => ExitCode::from(3),
    }
}

async fn run_check(
    args: pyrefly::run::CheckArgs,
    watch: bool,
    files_to_check: FilteredGlobs,
    config_finder: &impl Fn(&Path) -> ConfigFile,
    allow_forget: bool,
) -> anyhow::Result<CommandExitStatus> {
    if watch {
        let mut watcher = NotifyWatcher::new()?;
        for path in files_to_check.roots() {
            watcher.watch_dir(&path)?;
        }
        args.run_watch(watcher, files_to_check, config_finder)
            .await?;
        Ok(CommandExitStatus::Success)
    } else {
        args.run_once(files_to_check, config_finder, allow_forget)
    }
}

fn get_implicit_config_for_project() -> ConfigFile {
    fn get_config_path() -> anyhow::Result<ConfigFile> {
        let current_dir = std::env::current_dir().context("cannot identify current dir")?;
        let config_path = get_implicit_config_path_from(&current_dir)?;
        get_open_source_config(&config_path)
    }
    get_config_path().unwrap_or_else(|err| {
        tracing::debug!("{err}. Default configuration will be used as fallback.");
        ConfigFile::default()
    })
}

async fn run_check_on_project(
    watch: bool,
    config: Option<PathBuf>,
    project_excludes: Option<Vec<String>>,
    args: pyrefly::run::CheckArgs,
    allow_forget: bool,
) -> anyhow::Result<CommandExitStatus> {
    let config = if let Some(explicit_config_path) = config {
        tracing::info!(
            "Using config file explicitly provided at `{}`",
            explicit_config_path.display()
        );
        get_open_source_config(&explicit_config_path)?
    } else {
        get_implicit_config_for_project()
    };
    let project_excludes =
        project_excludes.map_or_else(|| config.project_excludes.clone(), Globs::new);
    run_check(
        args,
        watch,
        FilteredGlobs::new(config.project_includes.clone(), project_excludes),
        &|_| config.clone(),
        allow_forget,
    )
    .await
}

fn get_implicit_config_for_file(path: &Path) -> ConfigFile {
    fn get_implicit_config_path(path: &Path) -> anyhow::Result<ConfigFile> {
        let parent_dir = path
            .parent()
            .with_context(|| format!("Path `{}` has no parent directory", path.display()))?
            .absolutize()
            .with_context(|| format!("Path `{}` cannot be absolutized", path.display()))?;
        let config_path = get_implicit_config_path_from(parent_dir.as_ref())?;
        get_open_source_config(&config_path)
    }
    get_implicit_config_path(path).unwrap_or_else(|err| {
        tracing::debug!("{err}. Default configuration will be used as fallback.");
        ConfigFile::default()
    })
}

async fn run_check_on_files(
    files_to_check: Globs,
    project_excludes: Option<Vec<String>>,
    watch: bool,
    args: pyrefly::run::CheckArgs,
    allow_forget: bool,
) -> anyhow::Result<CommandExitStatus> {
    let project_excludes =
        project_excludes.map_or_else(ConfigFile::default_project_excludes, Globs::new);
    let files_to_check = files_to_check.from_root(PathBuf::new().absolutize()?.as_ref());
    run_check(
        args,
        watch,
        FilteredGlobs::new(files_to_check, project_excludes),
        &|path| get_implicit_config_for_file(path),
        allow_forget,
    )
    .await
}

async fn run_command(command: Command, allow_forget: bool) -> anyhow::Result<CommandExitStatus> {
    match command {
        Command::Check {
            files,
            project_excludes,
            watch,
            config,
            args,
        } => {
            if !files.is_empty() && config.is_some() {
                anyhow::bail!("Can either supply `FILES...` OR `--config/-c`, not both.")
            }
            if files.is_empty() {
                run_check_on_project(watch, config, project_excludes, args, allow_forget).await
            } else {
                run_check_on_files(
                    Globs::new(files),
                    project_excludes,
                    watch,
                    args,
                    allow_forget,
                )
                .await
            }
        }
        Command::BuckCheck(args) => args.run(),
        Command::Lsp(args) => args.run(Vec::new()),
    }
}

/// Run based on the command line arguments.
async fn run() -> anyhow::Result<ExitCode> {
    let args = Args::parse_from(get_args_expanded(args_os())?);
    if args.profiling {
        loop {
            let _ = run_command(args.command.clone(), false).await;
        }
    } else {
        init_tracing(args.verbose, false);
        init_thread_pool(args.common.threads);
        run_command(args.command, true).await.map(to_exit_code)
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> ExitCode {
    exit_on_panic();
    let res = run().await;
    match res {
        Ok(code) => code,
        Err(e) => {
            // If you return a Result from main, and RUST_BACKTRACE=1 is set, then
            // it will print a backtrace - which is not what we want.
            eprintln!("{:#}", e);
            ExitCode::FAILURE
        }
    }
}
