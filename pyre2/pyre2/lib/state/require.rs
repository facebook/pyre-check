/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;

use crate::state::epoch::Epoch;

/// How much information do we require about a module?
#[derive(Debug, Clone, Dupe, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Require {
    /// We require nothing about the module.
    /// It's only purpose is to provide information about dependencies, namely Exports.
    Exports,
    /// We want to know what errors this module produces.
    Errors,
    /// We want to retain all information about this module in memory,
    /// including the AST and bindings/answers.
    Everything,
}

impl Require {
    pub fn compute_errors(self) -> bool {
        self >= Require::Errors
    }

    /// Currently we only have one state for all memory values, so don't distinguish.
    /// But we might want to be more fine-grained in the future.
    fn keep_memory(self) -> bool {
        self >= Require::Everything
    }

    pub fn keep_answers_trace(self) -> bool {
        self.keep_memory()
    }

    pub fn keep_ast(self) -> bool {
        self.keep_memory()
    }

    pub fn keep_bindings(self) -> bool {
        self.keep_memory()
    }

    pub fn keep_answers(self) -> bool {
        self.keep_memory()
    }
}

/// The value for `Require` that we use if nothing is otherwise set.
#[derive(Debug, Clone, Dupe, Copy)]
pub struct RequireDefault(Epoch, Require);

impl RequireDefault {
    pub fn new(default: Require) -> Self {
        let mut epoch = Epoch::zero();
        epoch.next();
        Self(epoch, default)
    }
}

/// An override for a `RequireDefault` value.
/// If the override was set since the epoch, we use the override value,
/// otherwise we use the default value.
#[derive(Debug, Clone, Dupe, Copy)]
pub struct RequireOverride(Epoch, Require);

impl Default for RequireOverride {
    fn default() -> Self {
        Self::new()
    }
}

impl RequireOverride {
    pub fn new() -> Self {
        Self(Epoch::zero(), Require::Exports)
    }

    pub fn set(&mut self, default: RequireDefault, value: Require) {
        self.0 = default.0;
        self.1 = value;
    }

    pub fn get(self, default: RequireDefault) -> Require {
        if self.0 == default.0 {
            self.1
        } else {
            default.1
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_require() {
        let default = RequireDefault::new(Require::Errors);
        let mut over = RequireOverride::new();
        assert_eq!(over.get(default), Require::Errors);
        over.set(default, Require::Everything);
        assert_eq!(over.get(default), Require::Everything);
    }
}
