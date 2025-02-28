/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

mod notify_watcher;

use std::backtrace::Backtrace;
use std::env::args_os;
use std::path::Path;
use std::path::PathBuf;
use std::process::ExitCode;

use clap::Parser;
use pyre2::clap_env;
use pyre2::get_args_expanded;
use pyre2::init_tracing;
use pyre2::run::Command;
use pyre2::run::CommandExitStatus;
use pyre2::ConfigFile;
use pyre2::Globs;
use pyre2::Watcher;

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

    #[command(subcommand)]
    command: Command,
}

fn exit_on_panic() {
    std::panic::set_hook(Box::new(move |info| {
        eprintln!("Thread panicked, shutting down: {}", info);
        eprintln!("Backtrace:\n{}", Backtrace::force_capture());
        std::process::exit(1);
    }));
}

fn get_open_source_config(_: &Path) -> ConfigFile {
    // TODO: Implement upward-searching for open source config.
    ConfigFile
}

fn to_exit_code(status: CommandExitStatus) -> ExitCode {
    match status {
        CommandExitStatus::Success => ExitCode::SUCCESS,
        CommandExitStatus::UserError => ExitCode::FAILURE,
    }
}

fn run_check_on_project(
    _watcher: Option<Box<dyn Watcher>>,
    _config_file: Option<PathBuf>,
    _args: pyre2::run::CheckArgs,
    _allow_forget: bool,
) -> anyhow::Result<CommandExitStatus> {
    panic!("Project-checking mode has not been implemented yet")
}

fn run_check_on_files(
    files_to_check: Globs,
    watcher: Option<Box<dyn Watcher>>,
    args: pyre2::run::CheckArgs,
    allow_forget: bool,
) -> anyhow::Result<CommandExitStatus> {
    args.run(
        watcher,
        files_to_check,
        &get_open_source_config,
        allow_forget,
    )
}

fn run_command(command: Command, allow_forget: bool) -> anyhow::Result<CommandExitStatus> {
    match command {
        Command::Check {
            files,
            watch,
            config_file,
            args,
        } => {
            let watcher: Option<Box<dyn Watcher>> = if watch {
                Some(Box::new(notify_watcher::NotifyWatcher::new()?))
            } else {
                None
            };
            if !files.is_empty() && config_file.is_some() {
                panic!("Can either supply `FILES...` OR `--config-file`, not both.")
            }
            if files.is_empty() {
                run_check_on_project(watcher, config_file, args, allow_forget)
            } else {
                run_check_on_files(Globs::new(files), watcher, args, allow_forget)
            }
        }
        Command::BuckCheck(args) => args.run(),
        Command::Lsp(args) => args.run(),
    }
}

/// Run based on the command line arguments.
fn run() -> anyhow::Result<ExitCode> {
    let args = Args::parse_from(get_args_expanded(args_os())?);
    if args.profiling {
        loop {
            let _ = run_command(args.command.clone(), false);
        }
    } else {
        init_tracing(args.verbose, false);
        run_command(args.command, true).map(to_exit_code)
    }
}

pub fn main() -> ExitCode {
    exit_on_panic();
    let res = run();
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
