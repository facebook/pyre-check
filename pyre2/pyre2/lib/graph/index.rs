/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;
use std::num::NonZeroU32;

use dupe::Dupe;
use starlark_map::small_set::SmallSet;
use starlark_map::Hashed;

#[derive(Clone, Debug)]
pub struct Index<K> {
    map: SmallSet<K>,
}

impl<K> Default for Index<K> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct Idx<K> {
    // We use a NonZero to have an optimised representation for Option<Idx>.
    // We treat it as 0-based, and inc/dec as we store.
    idx: NonZeroU32,
    phantom: PhantomData<K>,
}

impl<K> Clone for Idx<K> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<K> Copy for Idx<K> {}

impl<K> Dupe for Idx<K> {}

impl<K> Idx<K> {
    /// Should be used cautiously - make sure this is really a valid index first.
    pub fn new(idx: usize) -> Self {
        Idx {
            idx: NonZeroU32::new((idx + 1) as u32).unwrap(),
            phantom: PhantomData,
        }
    }

    pub fn idx(self) -> usize {
        (self.idx.get() - 1) as usize
    }
}

impl<K> Index<K> {
    pub fn new() -> Self {
        Index {
            map: SmallSet::new(),
        }
    }

    pub fn items(&self) -> impl ExactSizeIterator<Item = (Idx<K>, &K)> {
        self.map.iter().enumerate().map(|(i, k)| (Idx::new(i), k))
    }
}

impl<K: Eq + Hash> Index<K> {
    pub fn insert(&mut self, k: K) -> Idx<K> {
        let h = Hashed::new(k);
        match self.map.get_index_of_hashed(h.as_ref()) {
            Some(idx) => Idx::new(idx),
            None => {
                let idx = Idx::new(self.map.len());
                self.map.insert_hashed_unique_unchecked(h);
                idx
            }
        }
    }

    pub fn key_to_idx(&self, k: &K) -> Option<Idx<K>> {
        self.map.get_index_of(k).map(Idx::new)
    }

    pub fn idx_to_key(&self, idx: Idx<K>) -> &K {
        self.map.get_index(idx.idx()).unwrap()
    }

    /// Does the index contain an element. Should be used very rarely.
    #[expect(dead_code)] // Logically part of the API
    fn contains(&self, key: &K) -> bool {
        self.map.contains(key)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_index_insert_twice() {
        let mut index = Index::new();
        let idx1 = index.insert(1);
        let idx2 = index.insert(3);
        let idx3 = index.insert(1);
        assert_eq!(idx1, idx3);
        assert_ne!(idx1, idx2);
        assert_eq!(
            index.items().collect::<Vec<_>>(),
            vec![(idx1, &1), (idx2, &3)]
        );
    }
}
