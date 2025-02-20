/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![allow(dead_code)] // General utilities, not always used

//! Originally copied from <https://crates.io/crates/gazebo>.
//! Provide `map` and `try_map` methods for slices.

/// Optimised collect iterator into Vec, which might be a Result.
///
/// If we do a standard .collect() on the iterator it will never have a good size hint,
/// as the lower bound will always be zero, so might reallocate several times.
/// We know the Vec will either be thrown away, or exactly `len`, so aim if we do allocate,
/// make sure it is at `len`. However, if the first element throws an error, we don't need
/// to allocate at all, so special case that.
fn collect_result<T, E>(mut it: impl ExactSizeIterator<Item = Result<T, E>>) -> Result<Vec<T>, E> {
    match it.next() {
        None => Ok(Vec::new()),
        Some(Err(e)) => Err(e),
        Some(Ok(x)) => {
            // +1 for the element we have already consumed
            let mut res = Vec::with_capacity(it.len() + 1);
            res.push(x);
            for x in it {
                res.push(x?);
            }
            Ok(res)
        }
    }
}

pub trait SliceExt {
    type Item;

    /// A shorthand for `iter().map(f).collect::<Vec<_>>()`. For example:
    ///
    /// ```ignore
    /// use td_util::prelude::*;
    /// assert_eq!([1, 2, 3][..].map(|x| x * x), vec![1, 4, 9]);
    /// assert_eq!(vec![1, 2, 3].map(|x| x * x), vec![1, 4, 9]);
    /// ```
    ///
    /// Note that from Rust 1.55.0 there is a `map` method on
    /// arrays (e.g. `[T; N]`) so you'll need to explicitly convert
    /// arrays to slices with the `[..]` operation.
    fn map<'a, B, F>(&'a self, f: F) -> Vec<B>
    where
        F: FnMut(&'a Self::Item) -> B;

    /// A shorthand for `iter().map(f).collect::<Result<Vec<_>, _>>()`. For example:
    ///
    /// ```ignore
    /// use td_util::prelude::*;
    /// assert_eq!(
    ///     [1, 2, 3].try_map(|x| Ok(x * x)),
    ///     Ok::<_, bool>(vec![1, 4, 9])
    /// );
    /// assert_eq!(
    ///     [1, 2, -3].try_map(|x| if *x > 0 { Ok(x * x) } else { Err(false) }),
    ///     Err(false)
    /// );
    /// ```
    ///
    /// This function will be generalised to [`Try`](std::ops::Try) once it has been
    /// standardised.
    fn try_map<'a, B, E, F>(&'a self, f: F) -> Result<Vec<B>, E>
    where
        F: FnMut(&'a Self::Item) -> Result<B, E>;
}

impl<T> SliceExt for [T] {
    type Item = T;

    fn map<'a, B, F>(&'a self, f: F) -> Vec<B>
    where
        F: FnMut(&'a Self::Item) -> B,
    {
        self.iter().map(f).collect()
    }

    fn try_map<'a, B, E, F>(&'a self, f: F) -> Result<Vec<B>, E>
    where
        F: FnMut(&'a Self::Item) -> Result<B, E>,
    {
        collect_result(self.iter().map(f))
    }
}

/// Extension traits on [`Vec`].
pub trait VecExt {
    type Item;

    /// A shorthand for `into_iter().map(f).collect::<Vec<_>>()`. For example:
    ///
    /// ```ignore
    /// use td_util::prelude::*;
    /// assert_eq!(vec![1, 2, 3].into_map(|x| x * x), vec![1, 4, 9]);
    /// ```
    fn into_map<B, F>(self, f: F) -> Vec<B>
    where
        F: FnMut(Self::Item) -> B;

    /// A shorthand for `into_iter().map(f).collect::<Result<Vec<_>, _>>()`. For example:
    ///
    /// ```ignore
    /// use td_util::prelude::*;
    /// assert_eq!(
    ///     vec![1, 2, 3].into_try_map(|x| Ok(x * x)),
    ///     Ok::<_, bool>(vec![1, 4, 9])
    /// );
    /// assert_eq!(
    ///     vec![1, 2, -3].into_try_map(|x| if x > 0 { Ok(x * x) } else { Err(false) }),
    ///     Err(false)
    /// );
    /// ```
    ///
    /// This function will be generalised to [`Try`](std::ops::Try) once it has been
    /// standardised.
    fn into_try_map<B, E, F>(self, f: F) -> Result<Vec<B>, E>
    where
        F: FnMut(Self::Item) -> Result<B, E>;
}

impl<T> VecExt for Vec<T> {
    type Item = T;

    fn into_map<B, F>(self, f: F) -> Vec<B>
    where
        F: FnMut(Self::Item) -> B,
    {
        self.into_iter().map(f).collect()
    }

    fn into_try_map<B, E, F>(self, f: F) -> Result<Vec<B>, E>
    where
        F: FnMut(Self::Item) -> Result<B, E>,
    {
        collect_result(self.into_iter().map(f))
    }
}
