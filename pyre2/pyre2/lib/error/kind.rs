/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Copy, Dupe)]
pub enum ErrorKind {
    Unknown,
}
