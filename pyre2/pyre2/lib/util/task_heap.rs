/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A max-heap for tasks that can be executed.

use std::cmp;
use std::collections::BinaryHeap;

use crate::util::lock::Condvar;
use crate::util::lock::Mutex;

/// A heap of tasks, where `K` represents the priority of the task and `V` represents the task.
/// Add tasks with `push_lifo` and `push_fifo`, and process them with `work`.
pub struct TaskHeap<K, V> {
    inner: Mutex<TaskHeapInner<K, V>>,
    condition: Condvar,
}

/// Inner state of the TaskHeap, protected by a mutex.
struct TaskHeapInner<K, V> {
    /// The heap of items.
    heap: BinaryHeap<Item<K, V>>,
    /// Counter used to maintain FIFO/LIFO ordering.
    counter: usize,
    /// Number of workers currently executing a task.
    active_workers: usize,
    /// Number of workers waiting for a task to come in.
    paused_workers: usize,
}

/// An item in the heap, with a priority (which includes the task priority and sequence number) and a value.
struct Item<K, V> {
    /// The priority of the item, as a pair of the task priority and a sequence number.
    /// The sequence number is used for FIFO/LIFO ordering:
    /// - Positive values are for LIFO (higher values come first).
    /// - Negative values are for FIFO (lower values come first).
    priority: (K, isize),
    /// The value of the item.
    value: V,
}

impl<K: Ord, V> Ord for Item<K, V> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.priority.cmp(&other.priority)
    }
}

impl<K: Ord, V> PartialOrd for Item<K, V> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<K: Ord, V> PartialEq for Item<K, V> {
    fn eq(&self, other: &Self) -> bool {
        self.priority == other.priority
    }
}

impl<K: Ord, V> Eq for Item<K, V> {}

impl<K: Ord, V> Default for TaskHeap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Ord, V> TaskHeap<K, V> {
    pub fn new() -> Self {
        Self {
            inner: Mutex::new(TaskHeapInner {
                heap: BinaryHeap::new(),
                counter: 0,
                active_workers: 0,
                paused_workers: 0,
            }),
            condition: Condvar::new(),
        }
    }

    /// Push a task into the heap with specified ordering.
    /// If `is_lifo` is true, the task will be processed in LIFO order for equal values of `K`.
    /// If `is_lifo` is false, the task will be processed in FIFO order for equal values of `K`.
    pub fn push(&self, k: K, v: V, is_lifo: bool) {
        let mut inner = self.inner.lock();
        let sequence = inner.counter as isize;
        inner.counter += 1;

        // For LIFO, use positive sequence; for FIFO, use negative sequence
        let sequence = if is_lifo { sequence } else { -sequence };

        inner.heap.push(Item {
            priority: (k, sequence),
            value: v,
        });

        if inner.paused_workers > 0 {
            self.condition.notify_one();
        }
    }

    /// Push a task into the heap. For equal values of `K`, the task will be processed in LIFO order.
    #[allow(dead_code)]
    pub fn push_lifo(&self, k: K, v: V) {
        self.push(k, v, true)
    }

    /// Push a task into the heap. For equal values of `K`, the task will be processed in FIFO order.
    #[allow(dead_code)]
    pub fn push_fifo(&self, k: K, v: V) {
        self.push(k, v, false)
    }

    /// Process items in the heap, in order of decreasing `K`.
    /// Continues processing successive items until all items have been processed,
    /// and there are no active calls to any `f` from all `work` calls.
    ///
    /// This method can be called from multiple threads simultaneously, and the callback `f`
    /// can safely call `push_fifo` or `push_lifo` on this `TaskHeap`.
    #[allow(dead_code)]
    pub fn work(&self, mut f: impl FnMut(K, V)) {
        let mut lock = self.inner.lock();
        loop {
            match lock.heap.pop() {
                Some(item) => {
                    lock.active_workers += 1;
                    drop(lock);
                    f(item.priority.0, item.value);
                    lock = self.inner.lock();
                    lock.active_workers -= 1;
                }
                None => {
                    if lock.active_workers == 0 {
                        self.condition.notify_all();
                        break;
                    }
                    lock.paused_workers += 1;
                    lock = self
                        .condition
                        .wait_while(lock, |x| x.heap.is_empty() && x.active_workers > 0);
                    lock.paused_workers -= 1;
                }
            }
        }
    }
}

impl<K: Ord, V> FromIterator<(K, V)> for TaskHeap<K, V> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let heap = TaskHeap::new();
        for (k, v) in iter {
            heap.push_fifo(k, v);
        }
        heap
    }
}

#[cfg(test)]
mod tests {

    use std::num::NonZero;
    use std::sync::atomic::AtomicUsize;
    use std::sync::atomic::Ordering;
    use std::thread::sleep;
    use std::time::Duration;

    use super::*;
    use crate::util::lock::Mutex;
    use crate::util::thread_pool::ThreadCount;
    use crate::util::thread_pool::ThreadPool;

    fn wait() {
        // at 100ms we occasionally lose the race condition, so sleep for a bit longer
        sleep(Duration::from_secs(1));
    }

    #[test]
    fn test_lifo_ordering() {
        let heap = TaskHeap::new();
        heap.push_lifo(1, "a");
        heap.push_lifo(1, "b");
        heap.push_lifo(1, "c");

        let mut results = Vec::new();
        heap.work(|_, v| results.push(v));

        // LIFO order for equal priorities: c, b, a
        assert_eq!(results, vec!["c", "b", "a"]);
    }

    #[test]
    fn test_fifo_ordering() {
        let heap = TaskHeap::new();
        heap.push_fifo(1, "a");
        heap.push_fifo(1, "b");
        heap.push_fifo(1, "c");

        let mut results = Vec::new();
        heap.work(|_, v| results.push(v));

        // FIFO order for equal priorities: a, b, c
        assert_eq!(results, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_priority_ordering() {
        let heap = TaskHeap::new();
        heap.push_fifo(1, "low");
        heap.push_fifo(3, "high");
        heap.push_fifo(2, "medium");

        let mut results = Vec::new();
        heap.work(|_, v| results.push(v));

        // Higher priority first: high, medium, low
        assert_eq!(results, vec!["high", "medium", "low"]);
    }

    #[test]
    fn test_nested_push() {
        let heap = TaskHeap::new();
        heap.push_fifo(1, "initial");

        let mut results = Vec::new();
        heap.work(|_, v| {
            results.push(v);
            if v == "initial" {
                heap.push_fifo(2, "nested");
            }
        });

        // Should process both the initial item and the nested item
        assert_eq!(results, vec!["initial", "nested"]);
    }

    #[test]
    fn test_pausing() {
        let heap = TaskHeap::new();
        heap.push_fifo(1, ());
        let threads =
            ThreadPool::with_thread_count(ThreadCount::NumThreads(NonZero::new(2).unwrap()));
        let executed = Mutex::new(Vec::new());
        let thread_count = AtomicUsize::new(0);
        threads.spawn_many(|| {
            let thread_index = thread_count.fetch_add(1, Ordering::SeqCst);
            heap.work(|k, _| {
                executed.lock().push(thread_index);
                if k == 1 {
                    wait(); // Make sure we give the second time chance to go to sleep
                    heap.push_fifo(2, ());
                    wait(); // Make sure we sleep enough for the second thread to grab it
                }
            });
        });
        let executed = executed.into_inner();
        assert_ne!(executed[0], executed[1]); // Both threads did something
    }

    #[test]
    fn test_parallel() {
        let heap = TaskHeap::new();
        heap.push_fifo(1, ());
        heap.push_fifo(2, ());
        let threads =
            ThreadPool::with_thread_count(ThreadCount::NumThreads(NonZero::new(2).unwrap()));
        let executed = Mutex::new(Vec::new());
        let thread_count = AtomicUsize::new(0);
        threads.spawn_many(|| {
            let thread_index = thread_count.fetch_add(1, Ordering::SeqCst);
            heap.work(|_, _| {
                executed.lock().push(thread_index);
                wait(); // Ensure the other thread gets a chance
            });
        });
        let executed = executed.into_inner();
        assert_ne!(executed[0], executed[1]); // Both threads did something
    }
}
