use crate::simple_test;

simple_test!(
    test_primitive_subtyping,
    r#"
class A: pass
class B(A): pass
class C(B): pass
class D: pass

b: A = B()
c: A = C()
oops: A = D()  # E: EXPECTED D <: A
"#,
);

simple_test!(
    test_object_is_top,
    r#"
class A: pass

a: object = A()
s: object = ""
"#,
);

simple_test!(
    test_simple_generic_subtyping,
    r#"
class A[T]: pass
class B[T](A[T]): pass
class C(B[int]): pass
class D[T]: pass

b: A[int] = B[int]()
c: A[int] = C()
oops0: A[int] = D[int]()  # E: EXPECTED D[int] <: A[int]
oops1: A[int] = A[str]()  # E: EXPECTED A[str] <: A[int]
"#,
);

simple_test!(
    test_simple_class_object_subtyping,
    r#"
class A: pass
class B(A): pass

a: type[A] = B
b: type[B] = A  # E: EXPECTED type[A] <: type[B]
"#,
);

simple_test!(
    test_generic_class_object_subtyping,
    r#"
class A[T]: pass
class B[T](A[T]): pass

a0: type[A] = B
b0: type[B] = A  # E: EXPECTED type[A] <: type[B[Error]]

a1: type[A[int]] = B
b1: type[B[int]] = A  # E: EXPECTED type[A] <: type[B[int]]

a2: type[A] = B[int]
b2: type[B] = A[int]  # E: EXPECTED type[A[int]] <: type[B[Error]]
"#,
);
