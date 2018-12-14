(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Test
open TypeOrder


let (!) name =
  Type.Primitive ~~name


let connect ?(parameters = []) order ~predecessor ~successor =
  connect ~parameters order ~predecessor ~successor


(* Butterfly:
    0 - 2
      X
    1 - 3 *)
let butterfly =
  let order = Builder.create () |> TypeOrder.handler in
  insert order Type.Bottom;
  insert order Type.Top;
  insert order !"0";
  insert order !"1";
  insert order !"2";
  insert order !"3";
  connect order ~predecessor:!"2" ~successor:Type.Top;
  connect order ~predecessor:!"3" ~successor:Type.Top;
  connect order ~predecessor:!"0" ~successor:!"2";
  connect order ~predecessor:!"0" ~successor:!"3";
  connect order ~predecessor:!"1" ~successor:!"2";
  connect order ~predecessor:!"1" ~successor:!"3";
  connect order ~predecessor:Type.Bottom ~successor:!"0";
  connect order ~predecessor:Type.Bottom ~successor:!"1";
  order


(*          0 - 3
            |   |   \
            BOTTOM  - b - 1      TOP
            |  \       /
            4 -- 2 ---           *)
let order =
  let bottom = !"bottom" in
  let order = Builder.create () |> TypeOrder.handler in
  insert order Type.Bottom;
  insert order bottom;
  insert order Type.Top;
  insert order !"0";
  insert order !"1";
  insert order !"2";
  insert order !"3";
  insert order !"4";
  insert order !"5";
  connect order ~predecessor:!"0" ~successor:!"3";
  connect order ~predecessor:!"1" ~successor:!"3";
  connect order ~predecessor:!"4" ~successor:!"2";
  connect order ~predecessor:!"3" ~successor:Type.Top;
  connect order ~predecessor:!"2" ~successor:Type.Top;
  connect order ~predecessor:Type.Bottom ~successor:bottom;
  connect order ~predecessor:bottom ~successor:!"0";
  connect order ~predecessor:bottom ~successor:!"1";
  connect order ~predecessor:bottom ~successor:!"2";
  connect order ~predecessor:bottom ~successor:!"4";
  order


(*
   TOP
    |
    A
   / \
  B   C
   \ /
    D
    |
 BOTTOM
*)
let diamond_order =
  let order = Builder.create () |> TypeOrder.handler in
  insert order Type.Bottom;
  insert order Type.Top;
  insert order !"A";
  insert order !"B";
  insert order !"C";
  insert order !"D";
  connect order ~predecessor:Type.Bottom ~successor:!"D";
  connect order ~predecessor:!"D" ~successor:!"B";
  connect order ~predecessor:!"D" ~successor:!"C";
  connect order ~predecessor:!"B" ~successor:!"A";
  connect order ~predecessor:!"C" ~successor:!"A";
  connect order ~predecessor:!"A" ~successor:Type.Top;
  order


let disconnected_order =
  let order = Builder.create () |> TypeOrder.handler in
  insert order Type.Bottom;
  insert order Type.Top;
  insert order !"A";
  insert order !"B";
  order

(*
   TOP
    |
    A
   /|
  B |
   \|
    C
    |
 BOTTOM
*)
let triangle_order =
  let order = Builder.create () |> TypeOrder.handler in
  insert order Type.Bottom;
  insert order Type.Top;
  insert order !"A";
  insert order !"B";
  insert order !"C";
  connect order ~predecessor:Type.Bottom ~successor:!"B";
  connect order ~predecessor:!"B" ~successor:!"A";
  connect order ~predecessor:!"A" ~successor:Type.Top;
  connect order ~predecessor:!"C" ~successor:!"B";
  connect order ~predecessor:!"C" ~successor:!"A";
  order


let variance_order =
  let order = Builder.create () |> TypeOrder.handler in
  let add_simple annotation =
    insert order annotation;
    connect order ~predecessor:Type.Bottom ~successor:annotation;
    connect order ~predecessor:annotation ~successor:Type.Top
  in

  insert order Type.Bottom;
  insert order Type.Object;
  insert order Type.Top;
  add_simple (Type.string);
  insert order Type.integer;
  insert order Type.float;
  connect order ~predecessor:Type.Bottom ~successor:Type.integer;
  connect order ~predecessor:Type.integer ~successor:Type.float;
  connect order ~predecessor:Type.float ~successor:Type.Top;
  insert order !"typing.Generic";

  (* Variance examples borrowed from https://www.python.org/dev/peps/pep-0483 *)
  let variable_t = Type.variable "_T" in
  let variable_t_co = Type.variable "_T_co" ~variance:Covariant in
  let variable_t_contra = Type.variable "_T_contra" ~variance:Contravariant in
  add_simple variable_t;
  add_simple variable_t_co;
  add_simple variable_t_contra;
  insert order !"LinkedList";
  insert order !"Box";
  insert order !"Sink";
  connect
    order
    ~predecessor:!"LinkedList"
    ~successor:!"typing.Generic"
    ~parameters:[variable_t];
  connect
    order
    ~predecessor:!"Box"
    ~successor:!"typing.Generic"
    ~parameters:[variable_t_co];
  connect
    order
    ~predecessor:!"Sink"
    ~successor:!"typing.Generic"
    ~parameters:[variable_t_contra];
  insert order !"Base";
  insert order !"Derived";
  connect
    order
    ~predecessor:!"Base"
    ~successor:!"typing.Generic"
    ~parameters:[variable_t_contra];
  connect
    order
    ~predecessor:!"Derived"
    ~successor:!"Base"
    ~parameters:[variable_t_co];
  connect
    order
    ~predecessor:!"Derived"
    ~successor:!"typing.Generic"
    ~parameters:[variable_t_co];
  order


(* A much more complicated set of rules, to explore the full combination of generic types.
   These rules define a situation like this:

   _T_co = covariant
   _T_contra = contravariant

   class A(Generic[_T_co, _T_contra])
   class B(A[_T_contra, _T_co])

   Hence the graph:

      /--  A[int, int]    <  A[float, int]  ----\
      |         V                   V           |
   /--|--  A[int, float]  <  A[float, float]  --|---\
   |  V                                         V   |
   |  |                                         |   |
   V  \--  B[int, int]    >  B[float, int]  ----/   V
   |            ^                   ^               |
   \----   B[int, float]  >  B[float, float]  ------/


   Additionally, classes C and D are defined as follows:

   class C(B[int, int])
   class D(B[float, float])
*)
let multiplane_variance_order =
  let order = Builder.create () |> TypeOrder.handler in
  let add_simple annotation =
    insert order annotation;
    connect order ~predecessor:Type.Bottom ~successor:annotation;
    connect order ~predecessor:annotation ~successor:Type.Top
  in

  insert order Type.Bottom;
  insert order Type.Object;
  insert order Type.Top;
  add_simple (Type.string);
  insert order Type.integer;
  insert order Type.float;
  connect order ~predecessor:Type.Bottom ~successor:Type.integer;
  connect order ~predecessor:Type.integer ~successor:Type.float;
  connect order ~predecessor:Type.float ~successor:Type.Top;
  insert order !"typing.Generic";

  let variable_t_co = Type.variable "_T_co" ~variance:Covariant in
  let variable_t_contra = Type.variable "_T_contra" ~variance:Contravariant in
  add_simple variable_t_co;
  add_simple variable_t_contra;
  insert order !"A";
  insert order !"B";
  insert order !"C";
  insert order !"D";
  connect
    order
    ~predecessor:!"A"
    ~successor:!"typing.Generic"
    ~parameters:[variable_t_co; variable_t_contra];
  connect
    order
    ~predecessor:!"B"
    ~successor:!"A"
    ~parameters:[variable_t_contra; variable_t_co];
  connect
    order
    ~predecessor:!"B"
    ~successor:!"typing.Generic"
    ~parameters:[variable_t_contra; variable_t_co];
  connect
    order
    ~predecessor:!"C"
    ~successor:!"B"
    ~parameters:[Type.integer; Type.integer];
  connect
    order
    ~predecessor:!"D"
    ~successor:!"B"
    ~parameters:[Type.float; Type.float];
  order


(* A type order where types A and B have parallel planes.
   These rules define a situation like this:

   _T_co = covariant
   _T_contra = contravariant

   class A(Generic[_T_co, _T_contra])
   class B(A[_T_co, _T_contra])

   Hence the graph:

      /--  A[int, int]    <  A[float, int]  ----\
      |         V                   V           |
   /--|--  A[int, float]  <  A[float, float]  --|---\
   |  V                                         V   |
   |  |                                         |   |
   V  \--  B[int, int]    <  B[float, int]  ----/   V
   |            V                   V               |
   \----   B[int, float]  <  B[float, float]  ------/


   Additionally, classes C and D are defined as follows:

   class C(B[int, int])
   class D(B[float, float])
*)
let parallel_planes_variance_order =
  let order = Builder.create () |> TypeOrder.handler in
  let add_simple annotation =
    insert order annotation;
    connect order ~predecessor:Type.Bottom ~successor:annotation;
    connect order ~predecessor:annotation ~successor:Type.Top
  in

  insert order Type.Bottom;
  insert order Type.Object;
  insert order Type.Top;
  add_simple (Type.string);
  insert order Type.integer;
  insert order Type.float;
  connect order ~predecessor:Type.Bottom ~successor:Type.integer;
  connect order ~predecessor:Type.integer ~successor:Type.float;
  connect order ~predecessor:Type.float ~successor:Type.Top;
  insert order !"typing.Generic";

  let variable_t_co = Type.variable "_T_co" ~variance:Covariant in
  let variable_t_contra = Type.variable "_T_contra" ~variance:Contravariant in
  add_simple variable_t_co;
  add_simple variable_t_contra;
  insert order !"A";
  insert order !"B";
  insert order !"C";
  insert order !"D";
  connect
    order
    ~predecessor:!"A"
    ~successor:!"typing.Generic"
    ~parameters:[variable_t_co; variable_t_contra];
  connect
    order
    ~predecessor:!"B"
    ~successor:!"A"
    ~parameters:[variable_t_co; variable_t_contra];
  connect
    order
    ~predecessor:!"B"
    ~successor:!"typing.Generic"
    ~parameters:[variable_t_co; variable_t_contra];
  connect
    order
    ~predecessor:!"C"
    ~successor:!"B"
    ~parameters:[Type.integer; Type.integer];
  connect
    order
    ~predecessor:!"D"
    ~successor:!"B"
    ~parameters:[Type.float; Type.float];
  order


let default =
  let order = Builder.default () |> TypeOrder.handler in
  let variable = Type.variable "_T" in
  insert order variable;
  connect order ~predecessor:Type.Bottom ~successor:variable;
  connect order ~predecessor:variable ~successor:Type.Top;
  let other_variable = Type.variable "_T2" in
  insert order other_variable;
  connect order ~predecessor:Type.Bottom ~successor:other_variable;
  connect order ~predecessor:other_variable ~successor:Type.Top;
  let variable_covariant = Type.variable "_T_co" ~variance:Covariant in
  insert order variable_covariant;
  connect order ~predecessor:Type.Bottom ~successor:variable_covariant;
  connect order ~predecessor:variable_covariant ~successor:Type.Top;

  insert order !"list";
  insert order !"typing.Sized";
  connect order ~predecessor:Type.Bottom ~successor:!"list";
  connect order ~predecessor:!"list" ~successor:!"typing.Sized";
  connect order ~predecessor:!"list" ~successor:!"typing.Generic" ~parameters:[variable];
  connect order ~predecessor:!"typing.Sized" ~successor:Type.Object;

  insert order !"set";
  connect order ~predecessor:Type.Bottom ~successor:!"set";
  connect order ~predecessor:!"set" ~successor:!"typing.Sized";
  connect order ~predecessor:!"set" ~successor:!"typing.Generic" ~parameters:[variable];

  insert order !"typing.Iterator";
  connect order ~predecessor:Type.Bottom ~successor:!"typing.Iterator";
  connect order ~predecessor:!"list" ~successor:!"typing.Iterator" ~parameters:[variable];
  connect order
    ~predecessor:!"typing.Iterator"
    ~successor:!"typing.Generic"
    ~parameters:[variable_covariant];
  connect order ~predecessor:!"typing.Iterator" ~successor:Type.Top;

  insert order !"typing.Iterable";
  connect order ~predecessor:Type.Bottom ~successor:!"typing.Iterable";
  connect order
    ~predecessor:!"typing.Iterator"
    ~successor:!"typing.Iterable"
    ~parameters:[variable_covariant];
  connect order
    ~predecessor:!"typing.Iterable"
    ~successor:!"typing.Generic"
    ~parameters:[variable_covariant];
  connect order ~predecessor:!"typing.Iterable" ~successor:Type.Top;

  insert order !"tuple";
  connect order ~predecessor:Type.Bottom ~successor:!"tuple";
  connect order ~predecessor:!"tuple" ~successor:!"typing.Iterator" ~parameters:[variable];
  connect order ~predecessor:!"tuple" ~successor:!"typing.Generic" ~parameters:[variable];

  insert order !"str";
  connect order ~predecessor:Type.Bottom ~successor:!"str";
  connect order ~predecessor:!"str" ~successor:!"typing.Iterable" ~parameters:[!"str"];
  connect order ~predecessor:!"str" ~successor:!"typing.Generic" ~parameters:[!"str"];

  insert order !"dict";
  connect order ~predecessor:Type.Bottom ~successor:!"dict";
  connect order ~predecessor:!"dict" ~successor:Type.Object ~parameters:[variable; other_variable];
  connect
    order
    ~predecessor:!"dict"
    ~successor:!"typing.Generic"
    ~parameters:[variable; other_variable];
  connect order ~predecessor:!"dict" ~successor:!"typing.Iterator" ~parameters:[variable];

  order


let test_default _ =
  let order = Builder.default () |> TypeOrder.handler in
  assert_true (less_or_equal order ~left:Type.Bottom ~right:Type.Bottom);
  assert_true (less_or_equal order ~left:Type.Bottom ~right:Type.Top);
  assert_true (less_or_equal order ~left:Type.Top ~right:Type.Top);
  assert_true (less_or_equal order ~left:Type.Top ~right:Type.Top);
  assert_false (less_or_equal order ~left:Type.Top ~right:Type.Bottom);
  assert_true (less_or_equal order ~left:Type.Deleted ~right:Type.Top);
  assert_true (less_or_equal order ~left:Type.Object ~right:Type.Deleted);

  (* Test special forms. *)
  let assert_has_special_form primitive_name =
    assert_true (TypeOrder.contains order (Type.Primitive (Identifier.create primitive_name)))
  in
  assert_has_special_form "typing.Tuple";
  assert_has_special_form "typing.Generic";
  assert_has_special_form "typing.Protocol";
  assert_has_special_form "typing.Callable";
  assert_has_special_form "typing.ClassVar";

  (* Mock. *)
  assert_true (less_or_equal order ~left:(Type.primitive "unittest.mock.Base") ~right:Type.Top);
  assert_true
    (less_or_equal order ~left:(Type.primitive "unittest.mock.NonCallableMock") ~right:Type.Top);

  (* Numerical types. *)
  assert_true (less_or_equal order ~left:Type.integer ~right:Type.integer);
  assert_false (less_or_equal order ~left:Type.float ~right:Type.integer);
  assert_true (less_or_equal order ~left:Type.integer ~right:Type.float);
  assert_true (less_or_equal order ~left:Type.integer ~right:Type.complex);
  assert_false (less_or_equal order ~left:Type.complex ~right:Type.integer);
  assert_true (less_or_equal order ~left:Type.float ~right:Type.complex);
  assert_false (less_or_equal order ~left:Type.complex ~right:Type.float);

  assert_true (less_or_equal order ~left:Type.integer ~right:(Type.primitive "numbers.Integral"));
  assert_true (less_or_equal order ~left:Type.integer ~right:(Type.primitive "numbers.Rational"));
  assert_true (less_or_equal order ~left:Type.integer ~right:(Type.primitive "numbers.Number"));
  assert_true (less_or_equal order ~left:Type.float ~right:(Type.primitive "numbers.Real"));
  assert_true (less_or_equal order ~left:Type.float ~right:(Type.primitive "numbers.Rational"));
  assert_true (less_or_equal order ~left:Type.float ~right:(Type.primitive "numbers.Complex"));
  assert_true (less_or_equal order ~left:Type.float ~right:(Type.primitive "numbers.Number"));
  assert_false (less_or_equal order ~left:Type.float ~right:(Type.primitive "numbers.Integral"));
  assert_true (less_or_equal order ~left:Type.complex ~right:(Type.primitive "numbers.Complex"));
  assert_false (less_or_equal order ~left:Type.complex ~right:(Type.primitive "numbers.Real"));

  (* Test join. *)
  assert_equal (join order Type.integer Type.integer) Type.integer;
  assert_equal (join order Type.float Type.integer) Type.float;
  assert_equal (join order Type.integer Type.float) Type.float;
  assert_equal (join order Type.integer Type.complex) Type.complex;
  assert_equal (join order Type.float Type.complex) Type.complex;

  (* Test meet. *)
  assert_equal (meet order Type.integer Type.integer) Type.integer;
  assert_equal (meet order Type.float Type.integer) Type.integer;
  assert_equal (meet order Type.integer Type.float) Type.integer;
  assert_equal (meet order Type.integer Type.complex) Type.integer;
  assert_equal (meet order Type.float Type.complex) Type.float


let test_method_resolution_order_linearize _ =
  let assert_method_resolution_order ((module Handler: Handler) as order) annotation expected =
    assert_equal
      ~printer:(List.fold ~init:"" ~f:(fun sofar next -> sofar ^ (Type.show next) ^ " "))
      expected
      (method_resolution_order_linearize
         order
         annotation
         ~get_successors:(Handler.find (Handler.edges ())))
  in
  assert_method_resolution_order butterfly !"3" [!"3"; Type.Top];
  assert_method_resolution_order butterfly !"0" [!"0"; !"3"; !"2"; Type.Top];
  assert_method_resolution_order diamond_order !"D" [!"D"; !"C"; !"B"; !"A"; Type.Top];
  (* The subclass gets chosen first even if after the superclass when both are inherited. *)
  assert_method_resolution_order triangle_order !"C" [!"C"; !"B"; !"A"; Type.Top]


let test_successors _ =
  (* Butterfly:
      0 - 2
        X
      1 - 3 *)
  assert_equal (successors butterfly !"3") [Type.Top];
  assert_equal (successors butterfly !"0") [!"3"; !"2"; Type.Top];

  (*          0 - 3
              /   /   \
              BOTTOM - 1      TOP
              |  \       /
              4 -- 2 ---           *)
  assert_equal (successors order !"3") [Type.Top];
  assert_equal (successors order !"0") [!"3"; Type.Top];
  assert_equal
    (successors order !"bottom")
    [
      !"4";
      !"2";
      !"1";
      !"0";
      !"3";
      Type.Top;
    ];

  (*  BOTTOM - Iterator[_T] - Iterable[_T] - Generic[_T] - Object
                    \                            /
                      --------------------------                    *)
  let order =
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Bottom;
    insert order Type.Object;
    insert order Type.Top;
    insert order !"typing.Iterator";
    insert order !"typing.Iterable";
    insert order !"typing.Generic";
    connect order ~predecessor:Type.Bottom ~successor:!"typing.Iterable";
    connect
      order
      ~predecessor:!"typing.Iterator"
      ~successor:!"typing.Iterable"
      ~parameters:[Type.variable "_T"];
    connect
      order
      ~predecessor:!"typing.Iterator"
      ~successor:!"typing.Generic"
      ~parameters:[Type.variable "_T"];
    connect
      order
      ~predecessor:!"typing.Iterable"
      ~successor:!"typing.Generic"
      ~parameters:[Type.variable "_T"];
    connect order ~predecessor:!"typing.Generic" ~successor:Type.Object;
    order in

  assert_equal
    (successors
       order
       (Type.parametric "typing.Iterable" [Type.integer]))
    [
      Type.parametric "typing.Generic" [Type.integer];
      Type.Object;
    ];

  assert_equal
    (successors
       order
       (Type.parametric "typing.Iterator" [Type.integer]))
    [
      Type.parametric "typing.Iterable" [Type.integer];
      Type.parametric "typing.Generic" [Type.integer];
      Type.Object;
    ]


let test_predecessors _ =
  (* Butterfly:
      0 - 2
        X
      1 - 3 *)
  assert_equal (predecessors butterfly !"0") [Type.Bottom];
  assert_equal (predecessors butterfly !"3") [!"1"; !"0"; Type.Bottom];

  (*          0 - 3
              /   /   \
              BOTTOM - 1      TOP
              |  \       /
              4 -- 2 ---           *)
  assert_equal (predecessors order !"0") [!"bottom"; Type.Bottom];
  assert_equal (predecessors order !"3") [!"1"; !"0"; !"bottom"; Type.Bottom]


let test_greatest _ =
  let smaller_than value = function
    | Type.Primitive name ->
        begin
          try
            (Identifier.show name |> Int.of_string) < value
          with _ ->
            false
        end
    | _ -> false
  in
  assert_equal (greatest butterfly ~matches:(smaller_than 3)) [!"2"];
  assert_equal (greatest butterfly ~matches:(smaller_than 2)) [!"0"; !"1"]


let test_less_or_equal _ =
  (* Primitive types. *)
  assert_true (less_or_equal order ~left:Type.Bottom ~right:Type.Top);
  assert_false (less_or_equal order ~left:Type.Top ~right:Type.Bottom);

  assert_true (less_or_equal order ~left:!"0" ~right:!"0");

  assert_true (less_or_equal order ~left:Type.Bottom ~right:!"0");
  assert_true (less_or_equal order ~left:Type.Bottom ~right:!"1");
  assert_true (less_or_equal order ~left:Type.Bottom ~right:!"2");
  assert_true (less_or_equal order ~left:Type.Bottom ~right:!"3");

  assert_false (less_or_equal order ~left:!"3" ~right:Type.Bottom);
  assert_false (less_or_equal order ~left:!"2" ~right:Type.Bottom);
  assert_false (less_or_equal order ~left:!"1" ~right:Type.Bottom);
  assert_false (less_or_equal order ~left:!"0" ~right:Type.Bottom);

  assert_true (less_or_equal order ~left:!"0" ~right:!"3");
  assert_true (less_or_equal order ~left:!"1" ~right:!"3");
  assert_false (less_or_equal order ~left:!"2" ~right:!"3");

  assert_true (less_or_equal default ~left:!"list" ~right:!"typing.Sized");
  assert_true
    (less_or_equal default ~left:(Type.list Type.integer) ~right:!"typing.Sized");

  (* Parametric types. *)
  assert_true
    (less_or_equal default ~left:(Type.list Type.integer) ~right:(Type.iterator Type.integer));
  assert_false
    (less_or_equal default ~left:(Type.list Type.float) ~right:(Type.iterator Type.integer));
  assert_true
    (less_or_equal default ~left:(Type.iterator Type.integer) ~right:(Type.iterable Type.integer));
  assert_true
    (less_or_equal default ~left:(Type.iterator Type.integer) ~right:(Type.iterable Type.float));

  (* Mixed primitive and parametric types. *)
  assert_true
    (less_or_equal
       default
       ~left:(Type.string)
       ~right:(Type.iterable Type.string));

  (* Mixed tuple and parametric types. *)
  assert_true
    (less_or_equal
       default
       ~left:(Type.tuple [Type.integer; Type.integer])
       ~right:(Type.iterator Type.integer));
  assert_false
    (less_or_equal
       default
       ~left:(Type.tuple [Type.integer; Type.float])
       ~right:(Type.iterator Type.integer));
  assert_true
    (less_or_equal
       default
       ~left:(Type.tuple [Type.integer; Type.float])
       ~right:(Type.iterator Type.float));

  assert_true
    (less_or_equal
       default
       ~left:(Type.Tuple (Type.Unbounded Type.integer))
       ~right:(Type.iterator Type.integer));
  assert_false
    (less_or_equal
       default
       ~left:(Type.Tuple (Type.Unbounded Type.float))
       ~right:(Type.iterator Type.integer));

  assert_true
    (less_or_equal
       default
       ~left:(Type.Primitive ~~"tuple")
       ~right:(Type.Tuple (Type.Unbounded Type.float)));
  assert_true
    (less_or_equal
       default
       ~left:(Type.Tuple (Type.Bounded [Type.integer; Type.integer]))
       ~right:(Type.parametric "tuple" [Type.integer]));

  (* Union types *)
  assert_true
    (less_or_equal
       default
       ~left:(Type.Optional Type.string)
       ~right:(Type.Union [Type.integer; Type.Optional Type.string]));

  (* Undeclared. *)
  assert_false (less_or_equal default ~left:(Type.undeclared) ~right:(Type.Top));
  assert_false (less_or_equal default ~left:(Type.Top) ~right:(Type.undeclared));

  (* Tuples. *)
  assert_true
    (less_or_equal
       default
       ~left:(Type.tuple [Type.integer; Type.integer])
       ~right:(Type.Tuple (Type.Unbounded Type.integer)));
  assert_true
    (less_or_equal
       default
       ~left:(Type.tuple [Type.integer; Type.integer])
       ~right:(Type.Tuple (Type.Unbounded Type.float)));
  assert_true
    (less_or_equal
       default
       ~left:(Type.tuple [Type.integer; Type.float])
       ~right:(Type.Tuple (Type.Unbounded Type.float)));

  let order =
    let order = Builder.create () |> TypeOrder.handler in
    let add_simple annotation =
      insert order annotation;
      connect order ~predecessor:Type.Bottom ~successor:annotation;
      connect order ~predecessor:annotation ~successor:Type.Top
    in

    insert order Type.Bottom;
    insert order Type.Object;
    insert order Type.Top;
    add_simple (Type.variable "_1");
    add_simple (Type.variable "_2");
    add_simple (Type.variable "_T");
    add_simple (Type.string);
    insert order Type.integer;
    insert order Type.float;
    connect order ~predecessor:Type.Bottom ~successor:Type.integer;
    connect order ~predecessor:Type.integer ~successor:Type.float;
    connect order ~predecessor:Type.float ~successor:Type.Top;
    add_simple !"tuple";
    insert order !"A";
    insert order !"B";
    insert order !"C";
    insert order !"typing.Generic";
    insert order !"FloatToStrCallable";
    insert order !"ParametricCallableToStr";
    insert order !"typing.Callable";
    connect order ~predecessor:Type.Bottom ~successor:!"A";

    connect
      order
      ~predecessor:!"A"
      ~successor:!"typing.Generic"
      ~parameters:[Type.variable "_1"; Type.variable "_2"];
    connect
      order
      ~predecessor:!"B"
      ~successor:!"typing.Generic"
      ~parameters:[Type.variable "_T"];
    connect
      order
      ~predecessor:!"C"
      ~successor:!"typing.Generic"
      ~parameters:[Type.variable "_T"];
    connect order ~predecessor:!"typing.Generic" ~successor:Type.Object;
    connect
      order
      ~predecessor:!"A"
      ~successor:!"B"
      ~parameters:[Type.tuple [Type.variable "_1"; Type.variable "_2"]];
    connect
      order
      ~predecessor:!"B"
      ~successor:!"C"
      ~parameters:[Type.union [Type.variable "_T"; Type.float]];
    connect order ~predecessor:!"typing.Generic" ~successor:Type.Object;
    connect order ~predecessor:Type.Bottom ~successor:!"FloatToStrCallable";
    connect
      order
      ~parameters:[parse_callable "typing.Callable[[float], str]"]
      ~predecessor:!"FloatToStrCallable"
      ~successor:!"typing.Callable";
    connect order ~predecessor:!"typing.Callable" ~successor:Type.Top;
    connect order ~predecessor:Type.Bottom ~successor:!"ParametricCallableToStr";
    let callable =
      let aliases annotation =
        match Type.show annotation with
        | "_T" ->
            Some (Type.variable "_T")
        | _ ->
            None
      in
      parse_callable ~aliases "typing.Callable[[_T], str]"
    in
    connect
      order
      ~parameters:[callable]
      ~predecessor:!"ParametricCallableToStr"
      ~successor:!"typing.Callable";
    connect
      order
      ~parameters:[Type.variable "_T"]
      ~predecessor:!"ParametricCallableToStr"
      ~successor:!"typing.Generic";

    order
  in
  assert_true
    (less_or_equal
       order
       ~left:(Type.parametric "A" [Type.integer; Type.string])
       ~right:(Type.parametric "B" [Type.tuple [Type.integer; Type.string]]));

  assert_false
    (less_or_equal
       order
       ~left:(Type.parametric "A" [Type.integer; Type.string])
       ~right:(Type.tuple [Type.integer; Type.string]));

  assert_true
    (less_or_equal
       order
       ~left:(Type.parametric "A" [Type.integer; Type.string])
       ~right:(
         Type.parametric
           "C"
           [Type.union [Type.tuple [Type.integer; Type.string]; Type.float]]));

  assert_false
    (less_or_equal
       order
       ~left:(Type.parametric "A" [Type.string; Type.integer])
       ~right:(
         Type.parametric
           "C"
           [Type.union [Type.tuple [Type.integer; Type.string]; Type.float]]));

  (* Variables. *)
  assert_true (less_or_equal order ~left:(Type.variable "T") ~right:Type.Object);
  assert_false (less_or_equal order ~left:(Type.variable "T") ~right:Type.integer);
  assert_true (less_or_equal order ~left:Type.Object ~right:(Type.variable "T"));
  assert_true (less_or_equal order ~left:Type.integer ~right:(Type.variable "T"));
  assert_true
    (less_or_equal
       order
       ~left:(Type.variable "T")
       ~right:(Type.union [Type.string; Type.variable "T"]));

  assert_true
    (less_or_equal
       order
       ~left:Type.integer
       ~right:(Type.variable ~constraints:(Type.Explicit [Type.float; Type.integer]) "T"));
  assert_true
    (less_or_equal
       order
       ~left:(Type.variable ~constraints:(Type.Explicit [Type.float; Type.string]) "T")
       ~right:(Type.union [Type.float; Type.string]));
  assert_false
    (less_or_equal
       order
       ~left:(Type.variable ~constraints:(Type.Explicit [Type.float; Type.string]) "T")
       ~right:(Type.union [Type.float]));

  assert_false
    (less_or_equal
       order
       ~left:Type.integer
       ~right:(Type.variable ~constraints:(Type.Bound Type.float) "T"));
  assert_false
    (less_or_equal
       order
       ~left:Type.float
       ~right:(Type.variable ~constraints:(Type.Bound Type.integer) "T"));

  assert_true
    (less_or_equal
       order
       ~left:(Type.union [Type.string; Type.integer])
       ~right:(Type.variable ~constraints:(Type.Explicit [Type.string; Type.integer]) "T"));
  assert_false
    (less_or_equal
       order
       ~left:Type.integer
       ~right:(Type.variable ~constraints:(Type.Explicit [Type.string]) "T"));

  (* Behavioral subtyping of callables. *)
  let less_or_equal order ~left ~right =
    less_or_equal order ~left:(parse_callable left) ~right:(parse_callable right)
  in
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable[[int], int]"
       ~right:"typing.Callable[[int], int]");
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable[[str], int]"
       ~right:"typing.Callable[[int], int]");
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable[[int], int]"
       ~right:"typing.Callable[[str], int]");
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable[[int], str]"
       ~right:"typing.Callable[[int], int]");
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable[[int], float]"
       ~right:"typing.Callable[[int], int]");
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable[[int], int]"
       ~right:"typing.Callable[[int], float]");
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable[[float], int]"
       ~right:"typing.Callable[[int], int]");
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable[[int], int]"
       ~right:"typing.Callable[[float], int]");

  (* Named vs. anonymous callables. *)
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable[[int], int]"
       ~right:"typing.Callable('foo')[[int], int]");
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable[[str], int]"
       ~right:"typing.Callable('foo')[[int], int]");
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable('foo')[[int], int]"
       ~right:"typing.Callable[[int], int]");
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable('foo')[[str], int]"
       ~right:"typing.Callable[[int], int]");

  (* Named callables. *)
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable('foo')[[int], int]"
       ~right:"typing.Callable('foo')[[int], int]");
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable('bar')[[str], int]"
       ~right:"typing.Callable('foo')[[int], int]");
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable('foo')[[int], int]"
       ~right:"typing.Callable('bar')[[int], int]");
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable('foo')[[str], int]"
       ~right:"typing.Callable('foo')[[int], int]");

  (* Undefined callables. *)
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable[..., int]"
       ~right:"typing.Callable[..., float]");
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable[[int], int]"
       ~right:"typing.Callable[..., int]");
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable[..., int]"
       ~right:"typing.Callable[[int], float]");

  (* Callable classes. *)
  assert_true
    (less_or_equal
       order
       ~left:"FloatToStrCallable"
       ~right:"typing.Callable[[float], str]");
  (* Subtyping is handled properly for callable classes. *)
  assert_true
    (less_or_equal
       order
       ~left:"FloatToStrCallable"
       ~right:"typing.Callable[[int], str]");
  assert_false
    (less_or_equal
       order
       ~left:"FloatToStrCallable"
       ~right:"typing.Callable[[float], int]");
  (* Parametric classes are also callables. *)
  assert_true
    (less_or_equal
       order
       ~left:"ParametricCallableToStr[int]"
       ~right:"typing.Callable[[int], str]");
  assert_true
    (less_or_equal
       order
       ~left:"ParametricCallableToStr[float]"
       ~right:"typing.Callable[[int], str]");
  assert_false
    (less_or_equal
       order
       ~left:"ParametricCallableToStr[int]"
       ~right:"typing.Callable[[float], str]");
  assert_false
    (less_or_equal
       order
       ~left:"ParametricCallableToStr[int]"
       ~right:"typing.Callable[[int], int]");
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable[[Variable(args, int)], str]"
       ~right:"typing.Callable[[int], str]");
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable[[Variable(args, int)], str]"
       ~right:"typing.Callable[[int, int], str]");
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable[[Variable(args, str)], str]"
       ~right:"typing.Callable[[int], str]");
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable[[Variable(args, int)], str]"
       ~right:"typing.Callable[[Named(arg, int)], str]");

  (* Callables with keyword arguments. *)
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable[[Keywords(kwargs, int)], str]"
       ~right:"typing.Callable[[Named(arg, int)], str]");
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable[[Variable(args, int), Keywords(kwargs, int)], str]"
       ~right:"typing.Callable[[Named(arg, int)], str]");
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable[[Variable(args, str), Keywords(kwargs, int)], str]"
       ~right:"typing.Callable[[Named(arg, int)], str]");


  (* TypedDictionaries *)
  assert_true
    (less_or_equal
       order
       ~left:"mypy_extensions.TypedDict[('Alpha', ('foo', str), ('bar', int), ('baz', int))]"
       ~right:"mypy_extensions.TypedDict[('Beta', ('foo', str), ('bar', int))]");
  assert_false
    (less_or_equal
       order
       ~left:"mypy_extensions.TypedDict[('Alpha', ('foo', str), ('bar', float))]"
       ~right:"mypy_extensions.TypedDict[('Beta', ('foo', str), ('bar', int))]");
  assert_true
    (less_or_equal
       order
       ~left:"mypy_extensions.TypedDict[('Alpha', ('bar', int), ('foo', str))]"
       ~right:"mypy_extensions.TypedDict[('Beta', ('foo', str), ('bar', int))]");

  assert_true
    (less_or_equal
       order
       ~left:"mypy_extensions.TypedDict[('Alpha', ('bar', int), ('foo', int))]"
       ~right:"typing.Mapping[str, typing.Any]");
  assert_false
    (less_or_equal
       order
       ~left:"mypy_extensions.TypedDict[('Alpha', ('bar', int), ('foo', int))]"
       ~right:"typing.Mapping[str, int]");
  assert_false
    (less_or_equal
       order
       ~left:"typing.Mapping[str, typing.Any]"
       ~right:"mypy_extensions.TypedDict[('Alpha', ('bar', int), ('foo', int))]");
  assert_false
    (less_or_equal
       order
       ~left:"mypy_extensions.TypedDict[('Alpha', ('bar', int), ('foo', int))]"
       ~right:"dict[str, typing.Any]")


let test_less_or_equal_variance _ =
  let assert_strict_less ~order ~right ~left =
    assert_true (less_or_equal order ~left ~right);
    assert_false (less_or_equal order ~left:right ~right:left)
  in
  (* Invariant. *)
  assert_false
    (less_or_equal
       variance_order
       ~left:(Type.parametric "LinkedList" [Type.integer])
       ~right:(Type.parametric "LinkedList" [Type.float]));
  assert_false
    (less_or_equal
       variance_order
       ~left:(Type.parametric "LinkedList" [Type.float])
       ~right:(Type.parametric "LinkedList" [Type.integer]));
  (* Covariant. *)
  assert_true
    (less_or_equal
       variance_order
       ~left:(Type.parametric "Box" [Type.integer])
       ~right:(Type.parametric "Box" [Type.float]));
  assert_false
    (less_or_equal
       variance_order
       ~left:(Type.parametric "Box" [Type.float])
       ~right:(Type.parametric "Box" [Type.integer]));
  (* Contravariant. *)
  assert_false
    (less_or_equal
       variance_order
       ~left:(Type.parametric "Sink" [Type.integer])
       ~right:(Type.parametric "Sink" [Type.float]));
  assert_true
    (less_or_equal
       variance_order
       ~left:(Type.parametric "Sink" [Type.float])
       ~right:(Type.parametric "Sink" [Type.integer]));
  (* More complex rules. *)
  assert_strict_less
    ~order:variance_order
    ~left:(Type.parametric "Derived" [Type.integer])
    ~right:(Type.parametric "Derived" [Type.float]);
  assert_strict_less
    ~order:variance_order
    ~left:(Type.parametric "Derived" [Type.integer])
    ~right:(Type.parametric "Base" [Type.integer]);
  assert_strict_less
    ~order:variance_order
    ~left:(Type.parametric "Derived" [Type.float])
    ~right:(Type.parametric "Base" [Type.float]);
  assert_strict_less
    ~order:variance_order
    ~left:(Type.parametric "Base" [Type.float])
    ~right:(Type.parametric "Base" [Type.integer]);
  assert_strict_less
    ~order:variance_order
    ~left:(Type.parametric "Derived" [Type.integer])
    ~right:(Type.parametric "Base" [Type.float]);
  assert_strict_less
    ~order:variance_order
    ~left:(Type.parametric "Derived" [Type.float])
    ~right:(Type.parametric "Base" [Type.integer]);
  (* Multiplane variance. *)
  assert_strict_less
    ~order:multiplane_variance_order
    ~left:(Type.parametric "A" [Type.integer; Type.float])
    ~right:(Type.parametric "A" [Type.float; Type.integer]);
  assert_strict_less
    ~order:multiplane_variance_order
    ~left:(Type.parametric "B" [Type.float; Type.integer])
    ~right:(Type.parametric "B" [Type.integer; Type.float]);
  assert_strict_less
    ~order:multiplane_variance_order
    ~left:(Type.parametric "B" [Type.integer; Type.integer])
    ~right:(Type.parametric "A" [Type.integer; Type.integer]);
  assert_strict_less
    ~order:multiplane_variance_order
    ~left:(Type.parametric "B" [Type.integer; Type.integer])
    ~right:(Type.parametric "A" [Type.integer; Type.float]);
  assert_strict_less
    ~order:multiplane_variance_order
    ~left:(Type.parametric "B" [Type.integer; Type.integer])
    ~right:(Type.parametric "A" [Type.float; Type.float]);
  assert_strict_less
    ~order:multiplane_variance_order
    ~left:(Type.parametric "B" [Type.float; Type.float])
    ~right:(Type.parametric "A" [Type.integer; Type.integer]);
  assert_strict_less
    ~order:multiplane_variance_order
    ~left:(Type.parametric "B" [Type.float; Type.float])
    ~right:(Type.parametric "A" [Type.integer; Type.float]);
  assert_strict_less
    ~order:multiplane_variance_order
    ~left:(Type.parametric "B" [Type.float; Type.float])
    ~right:(Type.parametric "A" [Type.float; Type.float]);
  assert_strict_less
    ~order:multiplane_variance_order
    ~left:(Type.parametric "B" [Type.float; Type.integer])
    ~right:(Type.parametric "A" [Type.integer; Type.integer]);
  assert_strict_less
    ~order:multiplane_variance_order
    ~left:(Type.parametric "B" [Type.float; Type.integer])
    ~right:(Type.parametric "A" [Type.integer; Type.float]);
  assert_strict_less
    ~order:multiplane_variance_order
    ~left:(Type.parametric "B" [Type.float; Type.integer])
    ~right:(Type.parametric "A" [Type.float; Type.float]);
  assert_strict_less
    ~order:multiplane_variance_order
    ~left:(Type.parametric "B" [Type.float; Type.integer])
    ~right:(Type.parametric "A" [Type.float; Type.integer]);
  assert_strict_less
    ~order:multiplane_variance_order
    ~left:(Type.parametric "C" [])
    ~right:(Type.parametric "A" [Type.float; Type.float]);
  assert_strict_less
    ~order:multiplane_variance_order
    ~left:!"C"
    ~right:(Type.parametric "A" [Type.float; Type.float]);
  assert_strict_less
    ~order:multiplane_variance_order
    ~left:(Type.parametric "D" [])
    ~right:(Type.parametric "A" [Type.integer; Type.integer]);
  assert_strict_less
    ~order:multiplane_variance_order
    ~left:!"D"
    ~right:(Type.parametric "A" [Type.integer; Type.integer]);
  assert_false
    (less_or_equal
       parallel_planes_variance_order
       ~left:(Type.parametric "C" [])
       ~right:(Type.parametric "A" [Type.float; Type.float]));
  assert_false
    (less_or_equal
       parallel_planes_variance_order
       ~left:!"C"
       ~right:(Type.parametric "A" [Type.float; Type.float]));
  assert_false
    (less_or_equal
       parallel_planes_variance_order
       ~left:(Type.parametric "D" [])
       ~right:(Type.parametric "A" [Type.integer; Type.integer]));
  assert_false
    (less_or_equal
       parallel_planes_variance_order
       ~left:!"D"
       ~right:(Type.parametric "A" [Type.integer; Type.integer]));
  ()


let test_join _ =
  let assert_join ?(order = default) ?(aliases = (fun _ -> None)) left right expected =
    let parse_annotation source =
      let integer = try Int.of_string source |> ignore; true with _ -> false in
      if integer then
        Type.Primitive ~~source
      else
        parse_single_expression source
        |> Type.create ~aliases
    in
    assert_equal
      ~printer:Type.show
      ~cmp:Type.equal
      (parse_annotation expected)
      (join order (parse_annotation left) (parse_annotation right))
  in


  (* Primitive types. *)
  assert_join "list" "typing.Sized" "typing.Sized";
  assert_join "typing.Sized" "list" "typing.Sized";
  assert_join "typing.List[int]" "typing.Sized" "typing.Sized";
  assert_join "int" "str" "typing.Union[int, str]";

  (* Parametric types. *)
  assert_join "typing.List[float]" "typing.List[float]" "typing.List[float]";
  assert_join "typing.List[float]" "typing.List[int]" "typing.List[typing.Any]";
  assert_join "typing.List[int]" "typing.Iterator[int]" "typing.Iterator[int]";
  assert_join "typing.Iterator[int]" "typing.List[int]" "typing.Iterator[int]";
  assert_join "typing.List[float]" "typing.Iterator[int]" "typing.Iterator[float]";
  assert_join "typing.List[float]" "float[int]" "typing.Any";

  (* Optionals. *)
  assert_join "str" "typing.Optional[str]" "typing.Optional[str]";
  assert_join "str" "typing.Optional[$bottom]" "typing.Optional[str]";
  assert_join "typing.Optional[$bottom]" "str" "typing.Optional[str]";

  (* Handles `[] or optional_list`. *)
  assert_join "typing.List[$bottom]" "typing.Optional[typing.List[int]]" "typing.List[int]";
  assert_join "typing.Optional[typing.List[int]]" "typing.List[$bottom]" "typing.List[int]";
  assert_join "typing.Optional[typing.Set[int]]" "typing.Set[$bottom]" "typing.Set[int]";

  (* Union types. *)
  assert_join
    "typing.Optional[bool]"
    "typing.Union[int, typing.Optional[bool]]"
    "typing.Union[int, typing.Optional[bool]]";
  assert_join "typing.Union[int, str]" "typing.Union[int, bytes]" "typing.Union[int, str, bytes]";
  assert_join
    "typing.Union[int, str]"
    "typing.Optional[$bottom]"
    "typing.Optional[typing.Union[int, str]]";

  assert_join
    "typing.Dict[str, str]"
    "typing.Dict[str, typing.List[str]]"
    "typing.Dict[str, typing.Any]";

  assert_join "typing.Union[typing.List[int], typing.Set[int]]" "typing.Sized" "typing.Sized";
  assert_join "typing.Tuple[int, ...]" "typing.Iterable[int]" "typing.Iterable[int]";
  assert_join "typing.Tuple[str, ...]" "typing.Iterator[str]" "typing.Iterator[str]";
  assert_join
    "typing.Tuple[int, ...]"
    "typing.Iterable[str]"
    "typing.Iterable[typing.Union[int, str]]";

  assert_join
    "typing.Optional[float]"
    "typing.Union[float, int]"
    "typing.Optional[typing.Union[float, int]]";

  (* Undeclared. *)
  assert_join "typing.Undeclared" "int" "typing.Union[typing.Undeclared, int]";
  assert_join "int" "typing.Undeclared" "typing.Union[typing.Undeclared, int]";
  let assert_join_types ?(order = default) left right expected =
    assert_equal
      ~printer:Type.show
      ~cmp:Type.equal
      expected
      (join order left right)
  in
  assert_join_types
    Type.undeclared
    Type.Top
    (Type.Union [Type.undeclared; Type.Top]);
  assert_join_types
    Type.Top
    Type.undeclared
    (Type.Union [Type.undeclared; Type.Top]);
  assert_join_types
    ~order
    !"0"
    Type.undeclared
    (Type.Union [!"0"; Type.undeclared]);
  assert_join_types
    ~order
    Type.undeclared
    !"0"
    (Type.Union [!"0"; Type.undeclared]);

  assert_join
    "typing.Tuple[int, int]"
    "typing.Tuple[int, int, str]"
    "typing.Union[typing.Tuple[int, int], typing.Tuple[int, int, str]]";

  let order =
    let order = Builder.create () |> TypeOrder.handler in
    let add_simple annotation =
      insert order annotation;
      connect order ~predecessor:Type.Bottom ~successor:annotation;
      connect order ~predecessor:annotation ~successor:Type.Top
    in

    insert order Type.Bottom;
    insert order Type.Object;
    insert order Type.Top;
    add_simple (Type.variable "_1");
    add_simple (Type.variable "_2");
    add_simple (Type.variable "_T");
    add_simple (Type.string);
    insert order Type.integer;
    insert order Type.float;
    insert order !"A";
    insert order !"B";
    insert order !"C";
    insert order !"CallableClass";
    insert order !"ParametricCallableToStr";
    insert order !"typing.Callable";
    insert order !"typing.Generic";

    connect order ~predecessor:Type.Bottom ~successor:Type.integer;
    connect order ~predecessor:Type.integer ~successor:Type.float;
    connect order ~predecessor:Type.float ~successor:Type.Top;

    connect order ~predecessor:Type.Bottom ~successor:!"A";

    connect
      order
      ~predecessor:!"A"
      ~successor:!"B"
      ~parameters:[Type.tuple [Type.variable "_1"; Type.variable "_2"]];
    connect
      order
      ~predecessor:!"A"
      ~successor:!"typing.Generic"
      ~parameters:[Type.variable "_1"; Type.variable "_2"];
    connect
      order
      ~predecessor:!"B"
      ~successor:!"typing.Generic"
      ~parameters:[Type.variable "_T"];
    connect
      order
      ~predecessor:!"B"
      ~successor:!"C"
      ~parameters:[Type.union [Type.variable "_T"; Type.float]];
    connect
      order
      ~predecessor:!"C"
      ~successor:!"typing.Generic"
      ~parameters:[Type.variable "_T"];
    connect order ~predecessor:!"typing.Generic" ~successor:Type.Object;
    connect order ~predecessor:Type.Bottom ~successor:!"CallableClass";
    connect
      order
      ~parameters:[parse_callable "typing.Callable[[int], str]"]
      ~predecessor:!"CallableClass"
      ~successor:!"typing.Callable";
    connect order ~predecessor:!"typing.Callable" ~successor:Type.Top;
    let callable =
      let aliases annotation =
        match Type.show annotation with
        | "_T" ->
            Some (Type.variable "_T")
        | _ ->
            None
      in
      parse_callable ~aliases "typing.Callable[[_T], str]"
    in
    connect order ~predecessor:Type.Bottom ~successor:!"ParametricCallableToStr";
    connect
      order
      ~parameters:[callable]
      ~predecessor:!"ParametricCallableToStr"
      ~successor:!"typing.Callable";
    connect
      order
      ~parameters:[Type.variable "_T"]
      ~predecessor:!"ParametricCallableToStr"
      ~successor:!"typing.Generic";

    order
  in
  let aliases =
    Type.Table.of_alist_exn [
      Type.primitive "_1", Type.variable "_1";
      Type.primitive "_2", Type.variable "_2";
      Type.primitive "_T", Type.variable "_T";
    ]
    |> Type.Table.find
  in

  assert_join
    ~order
    ~aliases
    "A[int, str]"
    "C[$bottom]"
    "C[typing.Union[float, typing.Tuple[int, str]]]";

  assert_join ~order:disconnected_order "A" "B" "typing.Any";

  assert_join
    "typing.Type[int]"
    "typing.Type[str]"
    "typing.Type[typing.Union[int, str]]";

  (* Callables. *)
  assert_join
    "typing.Callable[..., int]"
    "typing.Callable[..., str]"
    "typing.Callable[..., typing.Union[int, str]]";
  assert_join
    "typing.Callable[..., int]"
    "typing.Callable[..., $bottom]"
    "typing.Callable[..., int]";

  assert_join
    "typing.Callable('derp')[..., int]"
    "typing.Callable('derp')[..., int]"
    "typing.Callable('derp')[..., int]";
  assert_join
    "typing.Callable('derp')[..., int]"
    "typing.Callable('other')[..., int]"
    "typing.Callable[..., int]";

  (* Do not join with overloads. *)
  assert_join "typing.Callable[..., int][[..., str]]" "typing.Callable[..., int]" "typing.Any";

  assert_join
    "typing.Callable[[Named(a, int), Named(b, str)], int]"
    "typing.Callable[[Named(a, int), Named(b, str)], int]"
    "typing.Callable[[Named(a, int), Named(b, str)], int]";
  assert_join
    "typing.Callable[[Named(a, int)], int]"
    "typing.Callable[[int], int]"
    "typing.Callable[[int], int]";

  (* Behavioral subtyping is preserved. *)
  assert_join
    "typing.Callable[[Named(a, str)], int]"
    "typing.Callable[[Named(a, int)], int]"
    "typing.Callable[[Named(a, $bottom)], int]";
  assert_join
    "typing.Callable[..., int]"
    "typing.Callable[..., $bottom]"
    "typing.Callable[..., int]";
  assert_join
    "typing.Callable[[int], int]"
    "typing.Callable[[Named(a, int)], int]"
    "typing.Callable[[int], int]";
  assert_join
    "typing.Callable[[Named(b, int)], int]"
    "typing.Callable[[Named(a, int)], int]"
    "typing.Any";

  (* Classes with __call__ are callables. *)
  assert_join
    ~order
    "CallableClass"
    "typing.Callable[[int], str]"
    "typing.Callable[[int], str]";
  assert_join
    ~order
    "typing.Callable[[int], str]"
    "CallableClass"
    "typing.Callable[[int], str]";
  assert_join
    ~order
    "typing.Callable[[int], int]"
    "CallableClass"
    "typing.Callable[[int], typing.Union[int, str]]";
  assert_join
    ~order
    "ParametricCallableToStr[int]"
    "typing.Callable[[int], str]"
    "typing.Callable[[int], str]";
  assert_join
    ~order
    "typing.Callable[[int], str]"
    "ParametricCallableToStr[int]"
    "typing.Callable[[int], str]";
  assert_join
    ~order
    "typing.Callable[[int], int]"
    "ParametricCallableToStr[int]"
    "typing.Callable[[int], typing.Union[int, str]]";

  assert_join
    ~order
    "ParametricCallableToStr[int]"
    "typing.Callable[[int], str]"
    "typing.Callable[[int], str]";
  assert_join
    ~order
    "typing.Callable[[float], str]"
    "ParametricCallableToStr[int]"
    "typing.Callable[[int], str]";
  assert_join
    ~order
    "typing.Callable[[int], str]"
    "ParametricCallableToStr[float]"
    "typing.Callable[[int], str]";
  assert_join
    ~order
    "typing.Callable[[int], int]"
    "ParametricCallableToStr[int]"
    "typing.Callable[[int], typing.Union[int, str]]";

  (* TypedDictionaries *)
  assert_join
    "mypy_extensions.TypedDict[('Alpha', ('bar', int), ('foo', str), ('ben', int))]"
    "mypy_extensions.TypedDict[('Beta', ('foo', str), ('bar', int), ('baz', int))]"
    "mypy_extensions.TypedDict[('$anonymous', ('foo', str), ('bar', int))]";
  assert_join
    "mypy_extensions.TypedDict[('Alpha', ('bar', int), ('ben', int))]"
    "mypy_extensions.TypedDict[('Beta', ('foo', str))]"
    "mypy_extensions.TypedDict[('$anonymous', )]";
  assert_join
    "mypy_extensions.TypedDict[('Alpha', ('bar', int), ('foo', str), ('ben', int))]"
    "mypy_extensions.TypedDict[('Beta', ('foo', int))]"
    "typing.Mapping[str, typing.Any]";
  assert_join
    "mypy_extensions.TypedDict[('Alpha', ('bar', str), ('foo', str), ('ben', str))]"
    "typing.Mapping[str, str]"
    "typing.Mapping[str, typing.Any]";
  assert_join
    "mypy_extensions.TypedDict[('Alpha', ('bar', str), ('foo', str), ('ben', str))]"
    "typing.Mapping[int, str]"
    "typing.Any";
  assert_join
    "mypy_extensions.TypedDict[('Alpha', ('bar', str), ('foo', str), ('ben', str))]"
    "typing.Dict[str, str]"
    "typing.Any";

  (* Variables. *)
  assert_equal
    (join order Type.integer (Type.variable "T"))
    (Type.union [Type.integer; Type.variable "T"]);
  assert_equal
    (join order Type.integer (Type.variable ~constraints:(Type.Bound Type.string) "T"))
    (Type.union [Type.string; Type.integer]);
  assert_equal
    (join
       order
       Type.string
       (Type.variable ~constraints:(Type.Explicit [Type.float; Type.integer]) "T"))
    (Type.union [Type.float; Type.integer; Type.string]);

  (* Variance. *)
  let variance_aliases =
    Type.Table.of_alist_exn [
      Type.primitive "_T", Type.variable "_T";
      Type.primitive "_T_co", Type.variable "_T_co" ~variance:Covariant;
      Type.primitive "_T_contra", Type.variable "_T_contra" ~variance:Contravariant;
    ]
    |> Type.Table.find
  in
  assert_join
    ~order:variance_order
    ~aliases:variance_aliases
    "Derived[int]"
    "Base[int]"
    "Base[int]";
  assert_join
    ~order:variance_order
    ~aliases:variance_aliases
    "Derived[float]"
    "Base[float]"
    "Base[float]";
  assert_join
    ~order:variance_order
    ~aliases:variance_aliases
    "Derived[int]"
    "Base[float]"
    "Base[float]";
  assert_join
    ~order:variance_order
    ~aliases:variance_aliases
    "Derived[float]"
    "Base[int]"
    "Base[int]";
  assert_join
    ~order:multiplane_variance_order
    ~aliases:variance_aliases
    "B[int, float]"
    "A[int, float]"
    "A[int, float]";
  assert_join
    ~order:multiplane_variance_order
    ~aliases:variance_aliases
    "B[int, int]"
    "A[int, float]"
    "A[int, float]";
  assert_join
    ~order:multiplane_variance_order
    ~aliases:variance_aliases
    "B[float, int]"
    "A[int, float]"
    "A[int, float]";
  assert_join
    ~order:multiplane_variance_order
    ~aliases:variance_aliases
    "B[float, float]"
    "A[int, float]"
    "A[int, float]";
  assert_join
    ~order:multiplane_variance_order
    ~aliases:variance_aliases
    "B[int, float]"
    "A[float, float]"
    "A[float, float]";
  assert_join
    ~order:multiplane_variance_order
    ~aliases:variance_aliases
    "B[int, int]"
    "A[float, float]"
    "A[float, float]";
  assert_join
    ~order:multiplane_variance_order
    ~aliases:variance_aliases
    "B[float, int]"
    "A[float, float]"
    "A[float, float]";
  assert_join
    ~order:multiplane_variance_order
    ~aliases:variance_aliases
    "B[float, float]"
    "A[float, float]"
    "A[float, float]";
  assert_join
    ~order:parallel_planes_variance_order
    ~aliases:variance_aliases
    "B[float, float]"
    "A[int, float]"
    "A[float, float]";
  assert_join
    ~order:parallel_planes_variance_order
    ~aliases:variance_aliases
    "B[float, float]"
    "A[int, int]"
    "A[float, int]";
  ()


let test_meet _ =
  let assert_meet ?(order = default) ?(aliases = (fun _ -> None)) left right expected =
    let parse_annotation source =
      let integer = try Int.of_string source |> ignore; true with _ -> false in
      if integer then
        Type.Primitive ~~source
      else
        parse_single_expression source
        |> Type.create ~aliases
    in
    assert_equal
      ~printer:Type.show
      ~cmp:Type.equal
      (parse_annotation expected)
      (meet order (parse_annotation left) (parse_annotation right))
  in

  (* Special elements. *)
  assert_meet "typing.List[float]" "typing.Any" "typing.List[float]";

  (* Primitive types. *)
  assert_meet "list" "typing.Sized" "list";
  assert_meet "typing.Sized" "list" "list";
  assert_meet "typing.List[int]" "typing.Sized" "typing.List[int]";

  (* Unions. *)
  assert_meet "typing.Union[int, str]" "typing.Union[int, bytes]" "int";
  assert_meet "typing.Union[int, str]" "typing.Union[str, int]" "typing.Union[int, str]";
  assert_meet "typing.Union[int, str]" "typing.Union[int, typing.Optional[str]]" "int";
  assert_meet
    "typing.Union[int, typing.Optional[str]]"
    "typing.Optional[str]"
    "typing.Optional[str]";

  (* Parametric types. *)
  assert_meet "typing.List[int]" "typing.Iterator[int]" "typing.List[int]";
  assert_meet "typing.List[float]" "typing.Iterator[int]" "typing.List[$bottom]";
  assert_meet "typing.List[float]" "float[int]" "$bottom";
  assert_meet
    "typing.Dict[str, str]"
    "typing.Dict[str, typing.List[str]]"
    "typing.Dict[str, $bottom]";

  assert_meet ~order:disconnected_order "A" "B" "$bottom";

  (* TypedDictionaries *)
  assert_meet
    "mypy_extensions.TypedDict[('Alpha', ('bar', int), ('foo', str), ('ben', int))]"
    "mypy_extensions.TypedDict[('Beta', ('foo', str), ('bar', int), ('baz', int))]"
    ("mypy_extensions.TypedDict" ^
     "[('$anonymous', ('bar', int), ('baz', int), ('ben', int), ('foo', str))]");
  assert_meet
    "mypy_extensions.TypedDict[('Alpha', ('bar', int), ('foo', str), ('ben', int))]"
    "mypy_extensions.TypedDict[('Beta', ('foo', int))]"
    "$bottom";
  assert_meet
    "mypy_extensions.TypedDict[('Alpha', ('bar', int), ('foo', str), ('ben', int))]"
    "typing.Mapping[str, typing.Any]"
    "$bottom";

  (* Variables. *)
  assert_equal (meet default Type.integer (Type.variable "T")) Type.integer;
  assert_equal
    (meet
       default
       Type.integer
       (Type.variable ~constraints:(Type.Bound Type.float) "T"))
    (Type.variable ~constraints:(Type.Bound Type.integer) "T");
  assert_equal
    (meet
       default
       Type.string
       (Type.variable ~constraints:(Type.Explicit [Type.float; Type.string]) "T"))
    Type.string;

  (* Variance. *)
  assert_meet
    ~order:variance_order
    "Derived[int]"
    "Base[int]"
    "Derived[int]";
  assert_meet
    ~order:variance_order
    "Derived[float]"
    "Base[float]"
    "Derived[float]";
  assert_meet
    ~order:variance_order
    "Derived[int]"
    "Base[float]"
    "Derived[int]";
  assert_meet
    ~order:variance_order
    "Derived[float]"
    "Base[int]"
    "Derived[float]";
  assert_meet
    ~order:multiplane_variance_order
    "B[int, float]"
    "A[int, float]"
    "B[int, float]";
  assert_meet
    ~order:multiplane_variance_order
    "B[int, int]"
    "A[int, float]"
    "B[int, int]";
  assert_meet
    ~order:multiplane_variance_order
    "B[float, int]"
    "A[int, float]"
    "B[float, int]";
  assert_meet
    ~order:multiplane_variance_order
    "B[float, float]"
    "A[int, float]"
    "B[float, float]";
  assert_meet
    ~order:multiplane_variance_order
    "B[int, float]"
    "A[float, float]"
    "B[int, float]";
  assert_meet
    ~order:multiplane_variance_order
    "B[int, int]"
    "A[float, float]"
    "B[int, int]";
  assert_meet
    ~order:multiplane_variance_order
    "B[float, int]"
    "A[float, float]"
    "B[float, int]";
  assert_meet
    ~order:multiplane_variance_order
    "B[float, float]"
    "A[float, float]"
    "B[float, float]";
  assert_meet
    ~order:parallel_planes_variance_order
    "B[float, float]"
    "A[int, float]"
    "B[int, float]";
  assert_meet
    ~order:parallel_planes_variance_order
    "B[float, float]"
    "A[int, int]"
    "B[int, float]";
  ()


let test_least_upper_bound _ =
  assert_equal (least_upper_bound order Type.Bottom Type.Bottom) [Type.Bottom];

  assert_equal (least_upper_bound order Type.Bottom !"0") [!"0"];
  assert_equal (least_upper_bound order Type.Bottom !"1") [!"1"];
  assert_equal (least_upper_bound order !"3" !"1") [!"3"];
  assert_equal (least_upper_bound order !"4" !"bottom") [!"4"];

  assert_equal (least_upper_bound order !"0" !"1") [!"3"];
  assert_equal (least_upper_bound order !"0" !"2") [Type.Top];
  assert_equal (least_upper_bound order !"0" !"2") [Type.Top];

  assert_equal (least_upper_bound order Type.Top Type.Top) [Type.Top];

  assert_equal (least_upper_bound butterfly !"0" !"1") [!"2"; !"3"]


let test_greatest_lower_bound _ =
  assert_equal (greatest_lower_bound diamond_order Type.Bottom Type.Bottom) [Type.Bottom];

  assert_equal (greatest_lower_bound diamond_order Type.Bottom !"D") [Type.Bottom];
  assert_equal (greatest_lower_bound diamond_order Type.Bottom !"A") [Type.Bottom];
  assert_equal (greatest_lower_bound diamond_order !"A" !"C") [!"C"];
  assert_equal (greatest_lower_bound diamond_order !"A" !"B") [!"B"];
  assert_equal (greatest_lower_bound diamond_order !"A" !"D") [!"D"];
  assert_equal (greatest_lower_bound diamond_order !"B" !"D") [!"D"];
  assert_equal (greatest_lower_bound diamond_order !"B" !"C") [!"D"];

  assert_equal (greatest_lower_bound diamond_order Type.Top !"B") [!"B"];

  assert_equal (greatest_lower_bound diamond_order Type.Top Type.Top) [Type.Top];

  assert_equal (greatest_lower_bound butterfly !"2" !"3") [!"0"; !"1"]


let test_instantiate_parameters _ =
  assert_equal
    (instantiate_successors_parameters
       default
       ~source:(Type.list Type.string)
       ~target:(!"typing.Iterator"))
    (Some [Type.string]);

  assert_equal
    (instantiate_successors_parameters
       default
       ~source:(Type.dictionary ~key:Type.integer ~value:Type.string)
       ~target:(!"typing.Iterator"))
    (Some [Type.integer]);

  assert_equal
    (instantiate_successors_parameters default ~source:(Type.string) ~target:!"typing.Iterable")
    (Some [Type.string])


let test_deduplicate _ =
  let (module Handler: TypeOrder.Handler) =
    let order =
      Builder.create ()
      |> TypeOrder.handler
    in
    insert order Type.Bottom;
    insert order Type.Top;
    insert order !"0";
    insert order !"1";
    connect order ~parameters:[Type.Top; Type.Top] ~predecessor:!"0" ~successor:!"1";
    connect order ~parameters:[Type.Top] ~predecessor:!"0" ~successor:!"1";
    deduplicate order ~annotations:[!"0"; !"1"];
    order
  in
  let index_of annotation =
    Handler.find_unsafe (Handler.indices ()) annotation
  in
  let assert_targets edges from target parameters =
    assert_equal
      ~printer:(List.to_string ~f:Target.show)
      (Handler.find_unsafe edges (index_of !from))
      [{ Target.target = index_of !target; parameters }]
  in
  assert_targets (Handler.edges ()) "0" "1" [Type.Top];
  assert_targets (Handler.backedges ()) "1" "0" [Type.Top]


let test_remove_extra_edges _ =
  (* 0 -> 1 -> 2 -> 3
     |----^         ^
     |--------------^
  *)
  let (module Handler: TypeOrder.Handler) =
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Bottom;
    insert order Type.Top;
    insert order !"0";
    insert order !"1";
    insert order !"2";
    insert order !"3";
    connect order ~predecessor:!"0" ~successor:!"1";
    connect order ~predecessor:!"0" ~successor:!"3";
    connect order ~predecessor:!"1" ~successor:!"2";
    connect order ~predecessor:!"2" ~successor:!"3";
    remove_extra_edges order ~bottom:!"0" ~top:!"3" [!"0"; !"1"; !"2"; !"3"];
    order
  in
  let zero_index = Handler.find_unsafe (Handler.indices ()) !"0" in
  let one_index = Handler.find_unsafe (Handler.indices ()) !"1" in
  let two_index = Handler.find_unsafe (Handler.indices ()) !"2" in
  let three_index = Handler.find_unsafe (Handler.indices ()) !"3" in
  assert_equal
    (Handler.find_unsafe (Handler.edges ()) zero_index)
    [{ Target.target = one_index; parameters = []}];
  assert_equal
    (Handler.find_unsafe (Handler.backedges ()) three_index)
    [{ Target.target = two_index; parameters = []}]


let test_connect_annotations_to_top _ =
  (* Partial partial order:
      0 - 2
      |
      1   3 *)
  let order =
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Bottom;
    insert order Type.Top;
    insert order !"0";
    insert order !"1";
    insert order !"2";
    insert order !"3";
    connect order ~predecessor:!"0" ~successor:!"2";
    connect order ~predecessor:!"0" ~successor:!"1";
    connect_annotations_to_top order ~top:!"3" [!"0"; !"1"; !"2"; !"3"];
    order in

  assert_equal
    (least_upper_bound order !"1" !"2")
    [!"3"];

  (* Ensure that the backedge gets added as well *)
  assert_equal
    (greatest_lower_bound order !"1" !"3")
    [!"1"]


let test_add_backedges _ =
  (*
     TOP
      |
      A
     / \
    B   C
     \ /
      D
      |
   BOTTOM
  *)
  let (module Handler: TypeOrder.Handler) =
    (* Don't add backedges when connecting *)
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Bottom;
    insert order Type.Top;
    insert order !"A";
    insert order !"B";
    insert order !"C";
    insert order !"D";
    TypeOrder.connect order ~add_backedge:false ~predecessor:Type.Bottom ~successor:!"D";
    TypeOrder.connect order ~add_backedge:false ~predecessor:!"D" ~successor:!"B";
    TypeOrder.connect order ~add_backedge:false ~predecessor:!"D" ~successor:!"C";
    TypeOrder.connect order ~add_backedge:false ~predecessor:!"B" ~successor:!"A";
    TypeOrder.connect order ~add_backedge:false ~predecessor:!"C" ~successor:!"A";
    TypeOrder.connect order ~add_backedge:false ~predecessor:!"A" ~successor:Type.Top;
    order
  in
  let assert_backedges annotation number_of_backedges =
    let index = Handler.find_unsafe (Handler.indices ()) annotation in
    match Handler.find (Handler.backedges ()) index with
    | None ->
        assert_equal number_of_backedges 0
    | Some backedges ->
        assert_equal number_of_backedges (List.length backedges)
  in
  assert_backedges !"A" 0;
  assert_backedges !"B" 0;
  assert_backedges !"C" 0;
  assert_backedges !"D" 0;

  TypeOrder.add_backedges (module Handler) ~bottom:Type.Bottom;
  assert_backedges !"A" 2;
  assert_backedges !"B" 1;
  assert_backedges !"C" 1;
  assert_backedges !"D" 1;

  (* Ensure that backedges only get added once by re-adding. *)
  TypeOrder.add_backedges (module Handler) ~bottom:Type.Bottom;
  assert_backedges !"A" 2;
  assert_backedges !"B" 1


let test_check_integrity _ =
  check_integrity order;
  check_integrity butterfly;

  (* 0 <-> 1 *)
  let order =
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Bottom;
    insert order Type.Top;
    insert order !"0";
    insert order !"1";
    connect order ~predecessor:!"0" ~successor:!"1";
    connect order ~predecessor:!"1" ~successor:!"0";
    order in
  assert_raises TypeOrder.Cyclic (fun _ -> check_integrity order);

  (* 0 -> 1
     ^    |
      \   v
        - 2 -> 3 *)
  let order =
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Bottom;
    insert order Type.Top;
    insert order !"0";
    insert order !"1";
    insert order !"2";
    insert order !"3";
    connect order ~predecessor:!"0" ~successor:!"1";
    connect order ~predecessor:!"1" ~successor:!"2";
    connect order ~predecessor:!"2" ~successor:!"0";
    connect order ~predecessor:!"2" ~successor:!"3";
    order in
  assert_raises TypeOrder.Cyclic (fun _ -> check_integrity order);

  let order =
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Bottom;
    insert order !"0";
    order in
  assert_raises TypeOrder.Incomplete (fun _ -> check_integrity order);

  let order =
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Top;
    insert order !"0";
    order in
  assert_raises TypeOrder.Incomplete (fun _ -> check_integrity order)


let test_to_dot _ =
  let order =
    let order = Builder.create () |> TypeOrder.handler in
    insert order !"0";
    insert order !"1";
    insert order !"2";
    insert order !"3";
    insert order Type.Bottom;
    insert order Type.Top;
    connect order ~predecessor:!"0" ~successor:!"2";
    connect order ~predecessor:!"0" ~successor:!"1" ~parameters:[Type.string];
    connect_annotations_to_top order ~top:!"3" [!"0"; !"1"; !"2"; !"3"];
    order in
  let (module Handler) = order in
  assert_equal
    ~printer:ident
    ({|
       digraph {
         0[label="0"]
         1[label="1"]
         2[label="2"]
         3[label="3"]
         4[label="undefined"]
         5[label="unknown"]
         0 -> 1[label="(str)"]
         0 -> 2
         1 -> 3
         2 -> 3
       }
     |}
     |> Test.trim_extra_indentation)
    ("\n" ^ TypeOrder.to_dot order)


let test_variables _ =
  let order =
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Bottom;
    insert order Type.Top;
    insert order Type.generic;
    insert order !"A";
    insert order !"B";
    connect ~parameters:[Type.variable "T"] order ~predecessor:!"A" ~successor:Type.generic;
    connect order ~predecessor:Type.Bottom ~successor:!"A";
    connect order ~predecessor:Type.Bottom ~successor:!"B";
    connect order ~predecessor:!"B" ~successor:Type.Top;
    connect order ~predecessor:Type.generic ~successor:Type.Top;
    order
  in
  let assert_variables ~expected source =
    let aliases = fun _ -> None in
    let annotation =
      parse_single_expression source
      |> Type.create ~aliases
    in
    assert_equal expected (TypeOrder.variables order annotation)
  in
  assert_variables ~expected:None "B";
  assert_variables ~expected:(Some [Type.variable "T"]) "A";
  assert_variables ~expected:(Some [Type.variable "T"]) "A[int]";
  assert_variables ~expected:None "Nonexistent"


let () =
  "order">:::[
    "default">::test_default;
    "method_resolution_order_linearize">::test_method_resolution_order_linearize;
    "successors">::test_successors;
    "predecessors">::test_predecessors;
    "greatest">::test_greatest;
    "less_or_equal">::test_less_or_equal;
    "less_or_equal_variance">::test_less_or_equal_variance;
    "join">::test_join;
    "meet">::test_meet;
    "least_upper_bound">::test_least_upper_bound;
    "greatest_lower_bound">::test_greatest_lower_bound;
    "instantiate_parameters">::test_instantiate_parameters;
    "add_backedges">::test_add_backedges;
    "deduplicate">::test_deduplicate;
    "remove_extra_edges">::test_remove_extra_edges;
    "connect_annotations_to_top">::test_connect_annotations_to_top;
    "check_integrity">::test_check_integrity;
    "to_dot">::test_to_dot;
    "variables">::test_variables;
  ]
  |> Test.run
