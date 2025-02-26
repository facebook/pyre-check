/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

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
#![deny(clippy::cloned_instead_of_copied)]
#![deny(clippy::inefficient_to_string)]
#![deny(clippy::str_to_string)]
#![deny(clippy::string_to_string)]
#![deny(clippy::trivially_copy_pass_by_ref)]
#![deny(clippy::derive_partial_eq_without_eq)]
#![feature(box_patterns)]
#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(associated_type_defaults)]
#![feature(associated_const_equality)]
#![feature(once_wait)]

mod alt;
mod ast;
mod binding;
mod commands;
mod config;
mod dunder;
mod error;
mod export;
mod graph;
mod metadata;
mod module;
mod report;
mod solver;
mod state;
mod test;
mod types;
mod util;
mod visitors;

pub use crate::commands::run;
pub use crate::config::ConfigFile;
pub use crate::util::args::get_args_expanded;
pub use crate::util::trace::init_tracing;
pub use crate::util::watcher::Watcher;
