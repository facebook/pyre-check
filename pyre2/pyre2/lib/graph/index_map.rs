/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt::Debug;
use std::marker::PhantomData;
use std::mem;

use crate::graph::index::Idx;

/// A mapping from `Idx<K>` to `V`.
/// All the `Idx` values must be obtained from the same `Index`.
#[derive(Debug, Clone)]
pub struct IndexMap<K, V> {
    items: Vec<Option<V>>,
    phantom: PhantomData<K>,
}

impl<K, V> Default for IndexMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> IndexMap<K, V> {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            phantom: PhantomData,
        }
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn get(&self, key: Idx<K>) -> Option<&V> {
        match self.items.get(key.idx()) {
            Some(Some(v)) => Some(v),
            _ => None,
        }
    }

    pub fn get_mut(&mut self, key: Idx<K>) -> Option<&mut V> {
        match self.items.get_mut(key.idx()) {
            Some(Some(v)) => Some(v),
            _ => None,
        }
    }

    pub fn get_exists(&self, key: Idx<K>) -> &V {
        self.get(key).unwrap()
    }

    pub fn insert_once(&mut self, key: Idx<K>, value: V) {
        assert!(self.insert(key, value).is_none());
    }

    pub fn insert(&mut self, key: Idx<K>, value: V) -> Option<V> {
        match self.items.get_mut(key.idx()) {
            Some(v) => mem::replace(v, Some(value)),
            None => {
                self.items.resize_with(key.idx() + 1, || None);
                self.items[key.idx()] = Some(value);
                None
            }
        }
    }

    pub fn insert_if_missing(&mut self, key: Idx<K>, f: impl Fn() -> V) -> &mut V {
        if self.items.len() <= key.idx() {
            self.items.resize_with(key.idx() + 1, || None);
        }
        match &mut self.items[key.idx()] {
            Some(v) => v,
            v => {
                *v = Some(f());
                v.as_mut().unwrap()
            }
        }
    }

    pub fn reserve(&mut self, capacity: usize) {
        self.items.resize_with(self.items.len() + capacity, || None);
    }

    #[expect(dead_code)] // Logically part of the API
    fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.items.iter_mut().filter_map(|v| v.as_mut())
    }

    pub fn iter(&self) -> impl Iterator<Item = (Idx<K>, &V)> {
        self.items
            .iter()
            .enumerate()
            .filter_map(|(i, v)| v.as_ref().map(|v| (Idx::new(i), v)))
    }
}
