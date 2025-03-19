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
use clap::Parser;
use clap::Subcommand;
use pyre2::clap_env;
use pyre2::get_args_expanded;
use pyre2::init_thread_pool;
use pyre2::init_tracing;
use pyre2::run::BuckCheckArgs;
use pyre2::run::CheckArgs;
use pyre2::run::CommandExitStatus;
use pyre2::run::LspArgs;
use pyre2::ConfigFile;
use pyre2::Globs;
use pyre2::NotifyWatcher;

#[derive(Debug, Parser)]
#[command(name = "pyre2")]
#[command(about = "Next generation of Pyre type checker", long_about = None)]
struct Args {
    /// Enable verbose logging.
    #[clap(long = "verbose", short = 'v', global = true, env = clap_env("VERBOSE"))]
    verbose: bool,

    /// Set this to true to run profiling of fast jobs.
    /// Will run the command repeatedly.
    #[clap(long = "profiling", global = true, hide = true, env = clap_env("PROFILING"))]
    profiling: bool,

    /// Number of threads to use for parallelization.
    #[clap(long, short = 'j', default_value = "0", global = true, env = clap_env("THREADS"))]
    threads: usize,

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
        files: Vec<String>,
        /// Watch for file changes and re-check them.
        #[clap(long, env = clap_env("WATCH"))]
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
    // TODO: Implement upward-searching for open source config.
    ConfigFile::from_file(file).map_err(|err| {
        let file_str = file.display();
        anyhow!("Failed to parse configuration at {file_str}: {err}")
    })
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
    args: pyre2::run::CheckArgs,
    watch: bool,
    files_to_check: Globs,
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

async fn run_check_on_project(
    watch: bool,
    config: Option<PathBuf>,
    args: pyre2::run::CheckArgs,
    allow_forget: bool,
) -> anyhow::Result<CommandExitStatus> {
    let config = config
        .map(|c| get_open_source_config(c.as_path()))
        .transpose()?
        .unwrap_or_default();
    run_check(
        args,
        watch,
        config.project_includes.clone(),
        &|_| config.clone(),
        allow_forget,
    )
    .await
}

async fn run_check_on_files(
    files_to_check: Globs,
    watch: bool,
    args: pyre2::run::CheckArgs,
    allow_forget: bool,
) -> anyhow::Result<CommandExitStatus> {
    run_check(
        args,
        watch,
        files_to_check,
        // TODO(connernilsen): replace this when we have search paths working
        &|_| ConfigFile::default(),
        allow_forget,
    )
    .await
}

async fn run_command(command: Command, allow_forget: bool) -> anyhow::Result<CommandExitStatus> {
    match command {
        Command::Check {
            files,
            watch,
            config,
            args,
        } => {
            if !files.is_empty() && config.is_some() {
                anyhow::bail!("Can either supply `FILES...` OR `--config/-c`, not both.")
            }
            if files.is_empty() {
                run_check_on_project(watch, config, args, allow_forget).await
            } else {
                run_check_on_files(Globs::new(files), watch, args, allow_forget).await
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
        init_thread_pool(if args.threads == 0 {
            None
        } else {
            Some(args.threads)
        });
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
