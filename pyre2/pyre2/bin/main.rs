#![warn(clippy::all)]
#![allow(clippy::enum_variant_names)]
#![allow(clippy::manual_flatten)]
#![allow(clippy::match_like_matches_macro)]
#![allow(clippy::module_inception)]
#![allow(clippy::needless_lifetimes)]
#![allow(clippy::single_match)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::type_complexity)]
#![allow(clippy::wrong_self_convention)]
#![feature(box_patterns)]
#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(associated_type_defaults)]

use util::panic::exit_on_panic;

use crate::commands::run::run;

mod alt;
mod ast;
mod commands;
mod config;
mod debug;
mod dunder;
mod error;
mod expectation;
mod graph;
mod module;
mod solver;
mod subset;
mod test;
mod type_order;
mod types;
mod uniques;
mod util;
mod visitors;

fn main() {
    exit_on_panic();
    let res = run();
    if let Err(e) = res {
        // If you return a Result from main, and RUST_BACKTRACE=1 is set, then
        // it will print a backtrace - which is not what we want.
        eprintln!("{:#}", e);
        std::process::exit(1);
    }
}
