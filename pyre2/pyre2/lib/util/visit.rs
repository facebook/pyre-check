/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/// Visitors based on <https://ndmitchell.com/#uniplate_30_sep_2007>.
///
/// Should call the function on all immediate `To` children of `Self`.
/// As a special case, if `Self == To` then it should descend one layer.
pub trait Visit<To = Self> {
    fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a To));
}

/// Like `Visit`, but mutably.
pub trait VisitMut<To = Self> {
    fn visit_mut<'a>(&'a mut self, f: &mut dyn FnMut(&'a mut To));
}
