use std::fmt;
use std::fmt::Display;

use crate::types::types::Type;
use crate::util::display::commas_iter;

/*
Eventually this will have to be generalized enough to handle at least four cases:

1. the gradually-typed tuple tuple[Any, ...]
2. normal tuples as are handled here
3. variadic tuples with a splatted typevartuple variable
4. indefinite-length tuples tuple[int, ...] (whose length is supposed to be treated soundly, not gradually, IIRC)
*/

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Tuple {
    // tuple[t1, t2]
    Concrete(Vec<Type>),
    // tuple[t1, ...]
    Unbounded(Box<Type>),
}

impl Tuple {
    pub fn concrete(elts: Vec<Type>) -> Self {
        Self::Concrete(elts)
    }

    pub fn unbounded(elt: Type) -> Self {
        Self::Unbounded(Box::new(elt))
    }

    pub fn fmt_with_type<'a, D: Display + 'a>(
        &'a self,
        f: &mut fmt::Formatter<'_>,
        wrap: impl Fn(&'a Type) -> D,
    ) -> fmt::Result {
        let content = match self {
            Self::Concrete(elts) => {
                if elts.is_empty() {
                    "()".to_string()
                } else {
                    format!("{}", commas_iter(|| elts.iter().map(&wrap)))
                }
            }
            Self::Unbounded(ty) => format!("{}, ...", wrap(ty)),
        };
        write!(f, "tuple[{content}]")
    }

    pub fn visit<'a>(&'a self, mut f: impl FnMut(&'a Type)) {
        match self {
            Self::Concrete(elts) => elts.iter().for_each(f),
            Self::Unbounded(ty) => f(ty),
        }
    }

    pub fn visit_mut<'a>(&'a mut self, mut f: impl FnMut(&'a mut Type)) {
        match self {
            Self::Concrete(elts) => elts.iter_mut().for_each(f),
            Self::Unbounded(ty) => f(ty),
        }
    }
}
