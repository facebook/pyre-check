/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![allow(dead_code)] // General utilities, not always used

use std::hash::Hash;

use rayon::prelude::*;
use starlark_map::small_map::SmallMap;

pub fn map<'a, K: Clone + Eq + Hash, V, V2>(
    mp: &'a SmallMap<K, V>,
    mut f: impl FnMut(K, &'a V) -> V2,
) -> SmallMap<K, V2> {
    mp.iter()
        .map(|(k, v)| (k.clone(), f(k.clone(), v)))
        .collect()
}

pub fn into_map<K: Clone + Eq + Hash, V, V2>(
    mp: SmallMap<K, V>,
    mut f: impl FnMut(K, V) -> V2,
) -> SmallMap<K, V2> {
    mp.into_iter().map(|(k, v)| (k.clone(), f(k, v))).collect()
}

pub fn par_map<'a, K: Clone + Eq + Hash + Send + Sync, V: Sync, V2: Send>(
    mp: &'a SmallMap<K, V>,
    f: impl Fn(K, &'a V) -> V2 + Sync,
) -> SmallMap<K, V2> {
    mp.iter()
        .collect::<Vec<_>>()
        .as_slice()
        .par_iter()
        .map(|(k, v)| ((*k).clone(), f((*k).clone(), v)))
        .collect::<Vec<_>>()
        .into_iter()
        .collect()
}
