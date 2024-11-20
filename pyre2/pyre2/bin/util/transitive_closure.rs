#![allow(dead_code)] // General utilities, not always used

use std::hash::Hash;
use std::sync::Mutex;

use rayon::scope;
use rayon::Scope;
use starlark_map::small_map::Entry;
use starlark_map::small_map::SmallMap;

use crate::util::small_map;

pub fn transitive_closure<K: Eq + Hash, V>(
    keys: Vec<K>,
    mut f: impl FnMut(&K) -> (V, Vec<K>),
) -> SmallMap<K, V> {
    let mut todo = keys;
    let mut done = SmallMap::new();
    while let Some(k) = todo.pop() {
        if done.contains_key(&k) {
            continue;
        }
        let (v, deps) = f(&k);
        todo.extend(deps);
        done.insert(k, v);
    }
    done
}

pub fn transitive_closure_par<K: Clone + Eq + Hash + Send + Sync, V: Send + Sync>(
    keys: Vec<K>,
    f: impl Fn(&K) -> (V, Vec<K>) + Sync + Send,
) -> SmallMap<K, V> {
    fn run_one<'scope, K: Clone + Eq + Hash + Send + Sync, V: Send>(
        key: K,
        f: &'scope (dyn Fn(&K) -> (V, Vec<K>) + Sync + Send),
        scope: &Scope<'scope>,
        done: &'scope Mutex<SmallMap<K, Option<V>>>,
    ) {
        let (v, todo) = f(&key);
        done.lock().unwrap().insert(key, Some(v));
        run_many(todo, f, scope, done);
    }

    fn run_many<'scope, K: Clone + Eq + Hash + Send + Sync, V: Send>(
        keys: Vec<K>,
        f: &'scope (dyn Fn(&K) -> (V, Vec<K>) + Sync + Send),
        scope: &Scope<'scope>,
        done: &'scope Mutex<SmallMap<K, Option<V>>>,
    ) {
        let mut lock = done.lock().unwrap();
        for k in keys {
            if let Entry::Vacant(e) = lock.entry(k.clone()) {
                e.insert(None);
                scope.spawn(move |scope| run_one(k, f, scope, done));
            }
        }
    }

    let done = Mutex::new(SmallMap::new());
    scope(|scope| run_many(keys, &f, scope, &done));
    small_map::into_map(done.into_inner().unwrap(), |_, v| v.unwrap())
}
