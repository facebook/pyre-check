/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A max-heap for values represented by an enum.

use std::collections::VecDeque;
use std::iter;
use std::marker::PhantomData;

use enum_iterator::Sequence;

pub struct EnumHeap<K, V> {
    /// store the highest item last
    items: Vec<VecDeque<V>>,
    phantom: PhantomData<K>,
}

impl<K: Sequence, V> Default for EnumHeap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Sequence, V> EnumHeap<K, V> {
    fn new() -> Self {
        let mut items = Vec::with_capacity(K::CARDINALITY);
        for _ in 0..K::CARDINALITY {
            items.push(VecDeque::new());
        }
        Self {
            items,
            phantom: PhantomData,
        }
    }

    pub fn pop(&mut self) -> Option<(K, V)> {
        for (i, k) in enum_iterator::reverse_all().enumerate() {
            if let Some(v) = self.items[i].pop_front() {
                return Some((k, v));
            }
        }
        None
    }

    /// LIFO = Last In First Out
    pub fn push_lifo(&mut self, k: K, v: V) {
        let i = iter::successors(K::next(&k), K::next).count();
        self.items[i].push_front(v);
    }

    /// FIFO = First In First Out
    pub fn push_fifo(&mut self, k: K, v: V) {
        let i = iter::successors(K::next(&k), K::next).count();
        self.items[i].push_back(v);
    }
}

impl<K: Sequence, V> FromIterator<(K, V)> for EnumHeap<K, V> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let mut res = Self::new();
        for (k, v) in iter {
            res.push_fifo(k, v);
        }
        res
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Sequence, Eq, PartialEq, Debug)]
    enum Colors {
        Red,
        Green,
        Blue,
    }

    #[test]
    fn test_heap() {
        let mut heap = EnumHeap::new();
        heap.push_fifo(Colors::Blue, 1);
        heap.push_fifo(Colors::Red, 4);
        heap.push_fifo(Colors::Blue, 2);
        heap.push_lifo(Colors::Blue, 0);
        heap.push_fifo(Colors::Green, 3);
        assert_eq!(heap.pop(), Some((Colors::Blue, 0)));
        assert_eq!(heap.pop(), Some((Colors::Blue, 1)));
        assert_eq!(heap.pop(), Some((Colors::Blue, 2)));
        assert_eq!(heap.pop(), Some((Colors::Green, 3)));
        assert_eq!(heap.pop(), Some((Colors::Red, 4)));
        assert_eq!(heap.pop(), None);
    }
}
