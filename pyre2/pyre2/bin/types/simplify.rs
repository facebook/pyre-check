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
                Type::Never(_) => {}
                _ => res.push(x),
            }
        }
    }
    let mut res = Vec::with_capacity(xs.len());
    flatten(xs, &mut res);

    res.sort();
    res.dedup();
    if res.is_empty() {
        // can happen if `flatten` drops all elements
        Type::never()
    } else if res.len() == 1 {
        res.pop().unwrap()
    } else {
        Type::Union(res)
    }
}

#[cfg(test)]
mod tests {
    use crate::types::simplify::unions;
    use crate::types::types::NeverStyle;
    use crate::types::types::Type;

    #[test]
    fn test_flatten_nevers() {
        let xs = vec![
            Type::Never(NeverStyle::Never),
            Type::Never(NeverStyle::NoReturn),
        ];
        let res = unions(xs);
        assert_eq!(res, Type::never());
    }
}
