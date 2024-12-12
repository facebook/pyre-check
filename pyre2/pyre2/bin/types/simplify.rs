/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::types::types::Type;

pub fn unions(xs: Vec<Type>) -> Type {
    if xs.is_empty() {
        return Type::never();
    }
    fn flatten(xs: Vec<Type>, res: &mut Vec<Type>) {
        for x in xs {
            match x {
                Type::Union(xs) => flatten(xs, res),
                _ => res.push(x),
            }
        }
    }
    let mut res = Vec::with_capacity(xs.len());
    flatten(xs, &mut res);

    res.sort();
    res.dedup();
    if res.len() == 1 {
        res.pop().unwrap()
    } else {
        Type::Union(res)
    }
}
