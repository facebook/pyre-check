use ruff_python_ast::name::Name;

pub const ITER: Name = Name::new_static("__iter__");
pub const NEXT: Name = Name::new_static("__next__");
pub const GETITEM: Name = Name::new_static("__getitem__");
pub const SETITEM: Name = Name::new_static("__setitem__");
pub const ENTER: Name = Name::new_static("__enter__");
pub const EXIT: Name = Name::new_static("__exit__");
pub const AENTER: Name = Name::new_static("__aenter__");
pub const AEXIT: Name = Name::new_static("__aexit__");
