/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::types::tuple::Tuple;
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

fn flatten_unpacked_concrete_tuples(elts: Vec<Type>) -> Vec<Type> {
    let mut result = Vec::new();
    for elt in elts {
        match elt {
            Type::Unpack(box Type::Tuple(Tuple::Concrete(elts))) => {
                result.extend(elts);
            }
            _ => result.push(elt),
        }
    }
    result
}

// After a TypeVarTuple gets substituted with a tuple type, try to simplify the type
pub fn simplify_tuples(tuple: Tuple) -> Type {
    match tuple {
        Tuple::Concrete(elts) => {
            Type::Tuple(Tuple::Concrete(flatten_unpacked_concrete_tuples(elts)))
        }
        Tuple::Unpacked(box (prefix, middle, suffix)) => match middle {
            Type::Tuple(Tuple::Concrete(elts)) => {
                Type::Tuple(Tuple::Concrete(flatten_unpacked_concrete_tuples(
                    prefix
                        .into_iter()
                        .chain(elts)
                        .chain(suffix)
                        .collect::<Vec<_>>(),
                )))
            }
            Type::Tuple(Tuple::Unpacked(box (m_prefix, m_middle, m_suffix))) => {
                let mut new_prefix = flatten_unpacked_concrete_tuples(prefix);
                new_prefix.extend(flatten_unpacked_concrete_tuples(m_prefix));
                let mut new_suffix = flatten_unpacked_concrete_tuples(m_suffix);
                new_suffix.extend(flatten_unpacked_concrete_tuples(suffix));
                Type::Tuple(Tuple::Unpacked(Box::new((
                    new_prefix, m_middle, new_suffix,
                ))))
            }
            _ => Type::Tuple(Tuple::Unpacked(Box::new((
                flatten_unpacked_concrete_tuples(prefix),
                middle,
                flatten_unpacked_concrete_tuples(suffix),
            )))),
        },
        _ => Type::Tuple(tuple),
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
