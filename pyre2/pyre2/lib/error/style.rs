/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;

/// How should we print out errors for a particular module.
#[derive(Debug, Clone, Dupe, Copy, PartialEq, Eq)]
pub enum ErrorStyle {
    /// Will not be printed immediately, but will be stored.
    Delayed,
    /// Errors will not be printed or stored.
    /// Where possible, the errors will not even be computed.
    Never,
}
