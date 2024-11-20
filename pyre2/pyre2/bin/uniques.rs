//! Unique values produced by a factory.
//! Typically used to produce fresh variables.

use std::fmt;
use std::fmt::Display;
use std::sync::Mutex;

use dupe::Dupe;

/// Vend fresh unique `Var`s.
/// Deliberately NOT Clone.
#[derive(Debug)]
pub struct UniqueFactory {
    // For each Var we maintain a hint about what it means.
    hints: Mutex<Vec<String>>,
}

impl Default for UniqueFactory {
    fn default() -> Self {
        Self::new()
    }
}

/// A unique value, provided two given values were produced by the same factory.
#[derive(Debug, Copy, Dupe, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unique(usize);

impl Display for Unique {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 == 0 {
            write!(f, "_")
        } else {
            write!(f, "{}", self.0)
        }
    }
}

impl Unique {
    pub fn zero() -> Self {
        // Safe because we create every UniqueFactory with a zero.
        Self(0)
    }

    #[allow(dead_code)] // Only used in dead code
    pub fn display_with_hint<'a>(
        self,
        prefix: &'static str,
        uniques: &'a UniqueFactory,
    ) -> impl Display + 'a {
        struct X<'a>(Unique, &'static str, &'a UniqueFactory);
        impl<'a> Display for X<'a> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}{}[{}]", self.0, self.1, self.2.hint(self.0))
            }
        }
        X(self, prefix, uniques)
    }
}

impl UniqueFactory {
    pub fn new() -> Self {
        Self {
            hints: Mutex::new(vec!["zero".to_owned()]),
        }
    }

    pub fn fresh(&self, hint: String) -> Unique {
        let mut me = self.hints.lock().unwrap();
        let v = Unique(me.len());
        me.push(hint);
        v
    }

    #[allow(dead_code)] // Only used in dead code
    fn hint(&self, var: Unique) -> String {
        self.hints.lock().unwrap()[var.0].clone()
    }
}
