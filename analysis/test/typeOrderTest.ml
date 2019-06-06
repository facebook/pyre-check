(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Pyre
open Analysis
open Ast
open Test
open TypeOrder
open Annotated

let ( ! ) name = Type.Primitive name

let connect ?(parameters = []) handler ~predecessor ~successor =
  connect ~parameters handler ~predecessor ~successor


let parse_attributes ~parse_annotation ~class_name =
  let parse_attribute (name, annotation) =
    { Attribute.annotation = Annotation.create (parse_annotation annotation);
      async = false;
      class_attribute = false;
      defined = true;
      final = false;
      initialized = true;
      name;
      parent = Type.Primitive class_name;
      property = None;
      static = false;
      value = Ast.Node.create_with_default_location Ast.Expression.Ellipsis
    }
    |> Ast.Node.create_with_default_location
  in
  List.map ~f:parse_attribute


let less_or_equal
    ?(constructor = fun _ -> None)
    ?(attributes = fun _ -> None)
    ?(is_protocol = fun _ -> false)
    handler
  =
  less_or_equal
    { handler;
      constructor;
      attributes;
      is_protocol;
      any_is_bottom = false;
      protocol_assumptions = ProtocolAssumptions.empty
    }


let is_compatible_with ?(constructor = fun _ -> None)
                       handler =
  is_compatible_with
    { handler;
      constructor;
      attributes = (fun _ -> None);
      is_protocol = (fun _ -> false);
      any_is_bottom = false;
      protocol_assumptions = ProtocolAssumptions.empty
    }


let join ?(constructor = fun _ -> None)
         handler =
  join
    { handler;
      constructor;
      attributes = (fun _ -> None);
      is_protocol = (fun _ -> false);
      any_is_bottom = false;
      protocol_assumptions = ProtocolAssumptions.empty
    }


let meet ?(constructor = fun _ -> None)
         handler =
  meet
    { handler;
      constructor;
      attributes = (fun _ -> None);
      is_protocol = (fun _ -> false);
      any_is_bottom = false;
      protocol_assumptions = ProtocolAssumptions.empty
    }


(* Butterfly:
 *  0 - 2
 *    X
 *  1 - 3 *)
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
 *          |   |   \
 *          BOTTOM  - b - 1      TOP
 *          |  \       /
 *          4 -- 2 --- *)
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
 *   TOP
 *    |
 *    A
 *   / \
 *  B   C
 *   \ /
 *    D
 *    |
 * BOTTOM
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
 *   TOP
 *    |
 *    A
 *   /|
 *  B |
 *   \|
 *    C
 *    |
 * BOTTOM
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
  insert order Type.Any;
  insert order Type.Top;
  insert order Type.object_primitive;
  insert order Type.bool;
  add_simple Type.string;
  insert order Type.integer;
  insert order Type.float;
  connect order ~predecessor:Type.Bottom ~successor:Type.integer;
  connect order ~predecessor:Type.integer ~successor:Type.float;
  connect order ~predecessor:Type.float ~successor:Type.Top;
  insert order !"typing.Generic";
  (* Variance examples borrowed from https://www.python.org/dev/peps/pep-0483 *)
  let variable_t = Type.variable "_T" in
  let variable_t_2 = Type.variable "_T_2" in
  let variable_t_co = Type.variable "_T_co" ~variance:Covariant in
  let variable_t_contra = Type.variable "_T_contra" ~variance:Contravariant in
  add_simple variable_t;
  add_simple variable_t_co;
  add_simple variable_t_contra;
  insert order !"LinkedList";
  insert order !"Map";
  insert order !"Box";
  insert order !"Sink";
  connect order ~predecessor:!"LinkedList" ~successor:!"typing.Generic" ~parameters:[variable_t];
  connect
    order
    ~predecessor:!"Map"
    ~successor:!"typing.Generic"
    ~parameters:[variable_t; variable_t_2];
  connect order ~predecessor:!"Box" ~successor:!"typing.Generic" ~parameters:[variable_t_co];
  connect order ~predecessor:!"Sink" ~successor:!"typing.Generic" ~parameters:[variable_t_contra];
  insert order !"Base";
  insert order !"Derived";
  connect order ~predecessor:!"Base" ~successor:!"typing.Generic" ~parameters:[variable_t_contra];
  connect order ~predecessor:!"Derived" ~successor:!"Base" ~parameters:[variable_t_co];
  connect order ~predecessor:!"Derived" ~successor:!"typing.Generic" ~parameters:[variable_t_co];
  order


(* A much more complicated set of rules, to explore the full combination of generic types.
   These rules define a situation like this:

 * _T_co = covariant
 * _T_contra = contravariant

 * class A(Generic[_T_co, _T_contra])
 * class B(A[_T_contra, _T_co])

 * Hence the graph:

 *     /--  A[int, int]    <  A[float, int]  ----\
 *     |         V                   V           |
 *  /--|--  A[int, float]  <  A[float, float]  --|---\
 *  |  V                                         V   |
 *  |  |                                         |   |
 *  V  \--  B[int, int]    >  B[float, int]  ----/   V
 *  |            ^                   ^               |
 *  \----   B[int, float]  >  B[float, float]  ------/


 * Additionally, classes C and D are defined as follows:

 * class C(B[int, int])
 * class D(B[float, float])
 *)
let multiplane_variance_order =
  let order = Builder.create () |> TypeOrder.handler in
  let add_simple annotation =
    insert order annotation;
    connect order ~predecessor:Type.Bottom ~successor:annotation;
    connect order ~predecessor:annotation ~successor:Type.Top
  in
  insert order Type.Bottom;
  insert order Type.Any;
  insert order Type.Top;
  add_simple Type.string;
  insert order Type.integer;
  insert order Type.float;
  insert order Type.object_primitive;
  insert order Type.bool;
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
  connect order ~predecessor:!"B" ~successor:!"A" ~parameters:[variable_t_contra; variable_t_co];
  connect
    order
    ~predecessor:!"B"
    ~successor:!"typing.Generic"
    ~parameters:[variable_t_contra; variable_t_co];
  connect order ~predecessor:!"C" ~successor:!"B" ~parameters:[Type.integer; Type.integer];
  connect order ~predecessor:!"D" ~successor:!"B" ~parameters:[Type.float; Type.float];
  order


(* A type order where types A and B have parallel planes.
   These rules define a situation like this:

 *  _T_co = covariant
 * _T_contra = contravariant

 * class A(Generic[_T_co, _T_contra])
 * class B(A[_T_co, _T_contra])

 *  Hence the graph:

 *      /--  A[int, int]    <  A[float, int]  ----\
 *      |         V                   V           |
 *   /--|--  A[int, float]  <  A[float, float]  --|---\
 *   |  V                                         V   |
 *   |  |                                         |   |
 *   V  \--  B[int, int]    <  B[float, int]  ----/   V
 *   |            V                   V               |
 *   \----   B[int, float]  <  B[float, float]  ------/


 * Additionally, classes C and D are defined as follows:

 * class C(B[int, int])
 * class D(B[float, float])
 *)
let parallel_planes_variance_order =
  let order = Builder.create () |> TypeOrder.handler in
  let add_simple annotation =
    insert order annotation;
    connect order ~predecessor:Type.Bottom ~successor:annotation;
    connect order ~predecessor:annotation ~successor:Type.Top
  in
  insert order Type.Bottom;
  insert order Type.Any;
  insert order Type.Top;
  add_simple Type.string;
  insert order Type.integer;
  insert order Type.float;
  insert order Type.object_primitive;
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
  connect order ~predecessor:!"B" ~successor:!"A" ~parameters:[variable_t_co; variable_t_contra];
  connect
    order
    ~predecessor:!"B"
    ~successor:!"typing.Generic"
    ~parameters:[variable_t_co; variable_t_contra];
  connect order ~predecessor:!"C" ~successor:!"B" ~parameters:[Type.integer; Type.integer];
  connect order ~predecessor:!"D" ~successor:!"B" ~parameters:[Type.float; Type.float];
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
  insert order !"typing.Sequence";
  connect order ~predecessor:!"typing.Sequence" ~successor:!"typing.Generic" ~parameters:[variable];
  insert order !"list";
  insert order !"typing.Sized";
  connect order ~predecessor:Type.Bottom ~successor:!"list";
  connect order ~predecessor:!"list" ~successor:!"typing.Sized";
  connect order ~predecessor:!"list" ~successor:!"typing.Generic" ~parameters:[variable];
  connect order ~predecessor:!"typing.Sized" ~successor:Type.Any;
  connect order ~predecessor:!"list" ~successor:!"typing.Sequence" ~parameters:[variable];
  insert order !"typing.AbstractSet";
  insert order !"set";
  connect order ~predecessor:Type.Bottom ~successor:!"set";
  connect order ~predecessor:!"set" ~successor:!"typing.Sized";
  connect order ~predecessor:!"set" ~successor:!"typing.Generic" ~parameters:[variable];
  connect
    order
    ~predecessor:!"typing.AbstractSet"
    ~successor:!"typing.Generic"
    ~parameters:[variable];
  connect order ~predecessor:!"set" ~successor:!"typing.AbstractSet" ~parameters:[variable];
  insert order !"typing.Iterator";
  connect order ~predecessor:Type.Bottom ~successor:!"typing.Iterator";
  connect order ~predecessor:!"list" ~successor:!"typing.Iterator" ~parameters:[variable];
  connect
    order
    ~predecessor:!"typing.Iterator"
    ~successor:!"typing.Generic"
    ~parameters:[variable_covariant];
  connect order ~predecessor:!"typing.Iterator" ~successor:Type.Top;
  insert order !"typing.Iterable";
  connect order ~predecessor:Type.Bottom ~successor:!"typing.Iterable";
  connect
    order
    ~predecessor:!"typing.Iterator"
    ~successor:!"typing.Iterable"
    ~parameters:[variable_covariant];
  connect
    order
    ~predecessor:!"typing.Iterable"
    ~successor:!"typing.Generic"
    ~parameters:[variable_covariant];
  connect order ~predecessor:!"typing.Iterable" ~successor:Type.Top;
  connect order ~predecessor:!"list" ~successor:!"typing.Iterable" ~parameters:[variable];
  insert order !"tuple";
  connect order ~predecessor:Type.Bottom ~successor:!"tuple";
  connect order ~predecessor:!"tuple" ~successor:!"typing.Iterator" ~parameters:[variable];
  connect order ~predecessor:!"tuple" ~successor:!"typing.Generic" ~parameters:[variable];
  insert order !"typing.Generator";
  connect order ~predecessor:Type.Bottom ~successor:!"typing.Generator";
  connect
    order
    ~predecessor:!"typing.Generator"
    ~successor:!"typing.Iterator"
    ~parameters:[variable];
  connect order ~predecessor:!"typing.Generator" ~successor:!"typing.Generic" ~parameters:[variable];
  insert order !"str";
  connect order ~predecessor:Type.Bottom ~successor:!"str";
  connect order ~predecessor:!"str" ~successor:!"typing.Iterable" ~parameters:[!"str"];
  insert order !"AnyIterable";
  connect order ~predecessor:Type.Bottom ~successor:!"AnyIterable";
  connect order ~predecessor:!"AnyIterable" ~successor:!"typing.Iterable";
  insert order !"typing.Mapping";
  connect
    order
    ~predecessor:!"typing.Mapping"
    ~successor:!"typing.Generic"
    ~parameters:[variable; other_variable];
  insert order !"dict";
  connect order ~predecessor:!"dict" ~successor:Type.Any ~parameters:[variable; other_variable];
  connect
    order
    ~predecessor:!"dict"
    ~successor:!"typing.Generic"
    ~parameters:[variable; other_variable];
  connect
    order
    ~predecessor:!"dict"
    ~successor:!"typing.Mapping"
    ~parameters:[variable; other_variable];
  connect order ~predecessor:!"dict" ~successor:!"typing.Iterator" ~parameters:[variable];
  insert order !"collections.OrderedDict";
  connect order ~predecessor:Type.Bottom ~successor:!"collections.OrderedDict";
  connect
    order
    ~predecessor:!"collections.OrderedDict"
    ~successor:!"typing.Generic"
    ~parameters:[variable; other_variable];
  connect
    order
    ~predecessor:!"collections.OrderedDict"
    ~successor:!"dict"
    ~parameters:[variable; other_variable];
  insert order !"PartiallySpecifiedDict";
  connect order ~predecessor:Type.Bottom ~successor:!"PartiallySpecifiedDict";
  connect order ~predecessor:!"PartiallySpecifiedDict" ~successor:!"dict" ~parameters:[!"int"];
  insert order !"OverSpecifiedDict";
  connect order ~predecessor:Type.Bottom ~successor:!"OverSpecifiedDict";
  connect
    order
    ~predecessor:!"OverSpecifiedDict"
    ~successor:!"dict"
    ~parameters:[!"int"; !"int"; !"str"];
  insert order !"GenericContainer";
  connect
    order
    ~predecessor:!"GenericContainer"
    ~successor:!"typing.Generic"
    ~parameters:[variable; other_variable];
  connect order ~predecessor:!"GenericContainer" ~successor:Type.Any;
  insert order !"NonGenericContainerChild";
  connect
    order
    ~predecessor:!"NonGenericContainerChild"
    ~successor:!"GenericContainer"
    ~parameters:[!"int"; !"str"];
  connect order ~predecessor:Type.Bottom ~successor:!"NonGenericContainerChild";
  order


let test_default _ =
  let order = Builder.default () |> TypeOrder.handler in
  assert_true (less_or_equal order ~left:Type.Bottom ~right:Type.Bottom);
  assert_true (less_or_equal order ~left:Type.Bottom ~right:Type.Top);
  assert_true (less_or_equal order ~left:Type.Top ~right:Type.Top);
  assert_true (less_or_equal order ~left:Type.Top ~right:Type.Top);
  assert_false (less_or_equal order ~left:Type.Top ~right:Type.Bottom);
  (* Test special forms. *)
  let assert_has_special_form primitive_name =
    assert_true (TypeOrder.contains order (Type.Primitive primitive_name))
  in
  assert_has_special_form "typing.Tuple";
  assert_has_special_form "typing.Generic";
  assert_has_special_form "typing.Protocol";
  assert_has_special_form "typing.Callable";
  assert_has_special_form "typing.ClassVar";
  assert_has_special_form "typing.Final";
  (* Object *)
  assert_true (less_or_equal order ~left:(Type.optional Type.integer) ~right:Type.object_primitive);
  assert_true (less_or_equal order ~left:(Type.list Type.integer) ~right:Type.object_primitive);
  assert_false
    (less_or_equal order ~left:Type.object_primitive ~right:(Type.optional Type.integer));
  (* Mock. *)
  assert_true (less_or_equal order ~left:(Type.Primitive "unittest.mock.Base") ~right:Type.Top);
  assert_true
    (less_or_equal order ~left:(Type.Primitive "unittest.mock.NonCallableMock") ~right:Type.Top);
  (* Numerical types. *)
  assert_true (less_or_equal order ~left:Type.integer ~right:Type.integer);
  assert_false (less_or_equal order ~left:Type.float ~right:Type.integer);
  assert_true (less_or_equal order ~left:Type.integer ~right:Type.float);
  assert_true (less_or_equal order ~left:Type.integer ~right:Type.complex);
  assert_false (less_or_equal order ~left:Type.complex ~right:Type.integer);
  assert_true (less_or_equal order ~left:Type.float ~right:Type.complex);
  assert_false (less_or_equal order ~left:Type.complex ~right:Type.float);
  assert_true (less_or_equal order ~left:Type.integer ~right:(Type.Primitive "numbers.Integral"));
  assert_true (less_or_equal order ~left:Type.integer ~right:(Type.Primitive "numbers.Rational"));
  assert_true (less_or_equal order ~left:Type.integer ~right:(Type.Primitive "numbers.Number"));
  assert_true (less_or_equal order ~left:Type.float ~right:(Type.Primitive "numbers.Real"));
  assert_true (less_or_equal order ~left:Type.float ~right:(Type.Primitive "numbers.Rational"));
  assert_true (less_or_equal order ~left:Type.float ~right:(Type.Primitive "numbers.Complex"));
  assert_true (less_or_equal order ~left:Type.float ~right:(Type.Primitive "numbers.Number"));
  assert_false (less_or_equal order ~left:Type.float ~right:(Type.Primitive "numbers.Integral"));
  assert_true (less_or_equal order ~left:Type.complex ~right:(Type.Primitive "numbers.Complex"));
  assert_false (less_or_equal order ~left:Type.complex ~right:(Type.Primitive "numbers.Real"));
  (* Test join. *)
  assert_type_equal (join order Type.integer Type.integer) Type.integer;
  assert_type_equal (join order Type.float Type.integer) Type.float;
  assert_type_equal (join order Type.integer Type.float) Type.float;
  assert_type_equal (join order Type.integer Type.complex) Type.complex;
  assert_type_equal (join order Type.float Type.complex) Type.complex;
  (* Test meet. *)
  assert_type_equal (meet order Type.integer Type.integer) Type.integer;
  assert_type_equal (meet order Type.float Type.integer) Type.integer;
  assert_type_equal (meet order Type.integer Type.float) Type.integer;
  assert_type_equal (meet order Type.integer Type.complex) Type.integer;
  assert_type_equal (meet order Type.float Type.complex) Type.float


let test_method_resolution_order_linearize _ =
  let assert_method_resolution_order ((module Handler : Handler) as order) annotation expected =
    assert_equal
      ~printer:(List.fold ~init:"" ~f:(fun sofar next -> sofar ^ Type.show_primitive next ^ " "))
      expected
      (method_resolution_order_linearize
         order
         annotation
         ~get_successors:(Handler.find (Handler.edges ())))
  in
  assert_method_resolution_order butterfly "3" ["3"];
  assert_method_resolution_order butterfly "0" ["0"; "3"; "2"];
  assert_method_resolution_order diamond_order "D" ["D"; "C"; "B"; "A"];
  (* The subclass gets chosen first even if after the superclass when both are inherited. *)
  assert_method_resolution_order triangle_order "C" ["C"; "B"; "A"]


let test_successors _ =
  (* Butterfly:
   *  0 - 2
   *    X
   *  1 - 3 *)
  assert_equal (successors butterfly "3") [];
  assert_equal (successors butterfly "0") ["3"; "2"];
  (*          0 - 3
   *          /   /   \
   *          BOTTOM - 1      TOP
   *          |  \       /
   *          4 -- 2 --- *)
  assert_equal (successors order "3") [];
  assert_equal (successors order "0") ["3"];
  assert_equal (successors order "bottom") ["4"; "2"; "1"; "0"; "3"]


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
  assert_true (less_or_equal default ~left:(Type.list Type.integer) ~right:!"typing.Sized");
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
  assert_true (less_or_equal default ~left:Type.string ~right:(Type.iterable Type.string));
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
       ~left:(Type.Primitive "tuple")
       ~right:(Type.Tuple (Type.Unbounded Type.float)));
  assert_true
    (less_or_equal
       default
       ~left:(Type.Tuple (Type.Bounded (Concrete [Type.integer; Type.integer])))
       ~right:(Type.parametric "tuple" [Type.integer]));
  (* Union types *)
  assert_true
    (less_or_equal
       default
       ~left:(Type.Optional Type.string)
       ~right:(Type.Union [Type.integer; Type.Optional Type.string]));
  (* Undeclared. *)
  assert_false (less_or_equal default ~left:Type.undeclared ~right:Type.Top);
  assert_false (less_or_equal default ~left:Type.Top ~right:Type.undeclared);
  assert_false (less_or_equal default ~left:Type.undeclared ~right:Type.Bottom);
  assert_true (less_or_equal default ~left:Type.Bottom ~right:Type.undeclared);
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
  let list_variadic =
    Type.Variable.Variadic.List.create "Ts" |> Type.Variable.Variadic.List.mark_as_bound
  in
  assert_false
    (less_or_equal
       default
       ~left:(Type.Tuple (Bounded (Variable list_variadic)))
       ~right:(Type.Tuple (Type.Unbounded Type.integer)));
  assert_true
    (less_or_equal
       default
       ~left:(Type.Tuple (Bounded (Variable list_variadic)))
       ~right:(Type.Tuple (Type.Unbounded Type.object_primitive)));
  assert_true
    (less_or_equal
       default
       ~left:(Type.Tuple (Bounded (Concrete [Type.integer; Type.string])))
       ~right:(Type.Tuple (Bounded Any)));
  assert_true
    (less_or_equal
       default
       ~left:(Type.Tuple (Bounded Any))
       ~right:(Type.Tuple (Bounded (Concrete [Type.integer; Type.string]))));
  let order =
    let order = Builder.create () |> TypeOrder.handler in
    let add_simple annotation =
      insert order annotation;
      connect order ~predecessor:Type.Bottom ~successor:annotation;
      connect order ~predecessor:annotation ~successor:Type.Top
    in
    insert order Type.Bottom;
    insert order Type.Any;
    insert order Type.Top;
    insert order Type.object_primitive;
    add_simple (Type.variable "_1");
    add_simple (Type.variable "_2");
    add_simple (Type.variable "_T");
    add_simple Type.string;
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
    connect order ~predecessor:!"B" ~successor:!"typing.Generic" ~parameters:[Type.variable "_T"];
    connect order ~predecessor:!"C" ~successor:!"typing.Generic" ~parameters:[Type.variable "_T"];
    connect order ~predecessor:!"typing.Generic" ~successor:Type.Any;
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
    connect order ~predecessor:!"typing.Generic" ~successor:Type.Any;
    connect order ~predecessor:Type.Bottom ~successor:!"FloatToStrCallable";
    connect
      order
      ~parameters:[parse_callable "typing.Callable[[float], str]"]
      ~predecessor:!"FloatToStrCallable"
      ~successor:!"typing.Callable";
    connect order ~predecessor:!"typing.Callable" ~successor:Type.Top;
    connect order ~predecessor:Type.Bottom ~successor:!"ParametricCallableToStr";
    let callable =
      let aliases = function
        | "_T" -> Some (Type.variable "_T")
        | _ -> None
      in
      let aliases = create_type_alias_table aliases in
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
    let typed_dictionary = Type.Primitive "TypedDictionary" in
    let typing_mapping = Type.Primitive "typing.Mapping" in
    insert order typed_dictionary;
    insert order typing_mapping;
    connect order ~predecessor:Type.Bottom ~successor:typed_dictionary;
    connect
      order
      ~predecessor:typed_dictionary
      ~parameters:[Type.string; Type.Any]
      ~successor:typing_mapping;
    connect
      order
      ~parameters:[Type.variable "_T"; Type.variable "_T2"]
      ~predecessor:typing_mapping
      ~successor:!"typing.Generic";
    insert order (Type.Primitive "dict");
    insert order (Type.Primitive "MatchesProtocol");
    insert order (Type.Primitive "DoesNotMatchProtocol");
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
       ~right:
         (Type.parametric "C" [Type.union [Type.tuple [Type.integer; Type.string]; Type.float]]));
  assert_false
    (less_or_equal
       order
       ~left:(Type.parametric "A" [Type.string; Type.integer])
       ~right:
         (Type.parametric "C" [Type.union [Type.tuple [Type.integer; Type.string]; Type.float]]));
  (* Variables. *)
  assert_true (less_or_equal order ~left:(Type.variable "T") ~right:Type.Any);
  assert_false (less_or_equal order ~left:(Type.variable "T") ~right:Type.integer);
  assert_false (less_or_equal order ~left:Type.Any ~right:(Type.variable "T"));
  assert_false (less_or_equal order ~left:Type.integer ~right:(Type.variable "T"));
  assert_true
    (less_or_equal
       order
       ~left:(Type.variable "T")
       ~right:(Type.union [Type.string; Type.variable "T"]));
  assert_false
    (less_or_equal
       order
       ~left:Type.integer
       ~right:
         (Type.variable ~constraints:(Type.Variable.Unary.Explicit [Type.float; Type.integer]) "T"));
  assert_true
    (less_or_equal
       order
       ~left:
         (Type.variable ~constraints:(Type.Variable.Unary.Explicit [Type.float; Type.string]) "T")
       ~right:(Type.union [Type.float; Type.string]));
  assert_true
    (less_or_equal
       order
       ~left:
         (Type.variable ~constraints:(Type.Variable.Unary.Explicit [Type.float; Type.string]) "T")
       ~right:(Type.union [Type.float; Type.string; !"A"]));
  assert_false
    (less_or_equal
       order
       ~left:
         (Type.variable ~constraints:(Type.Variable.Unary.Explicit [Type.float; Type.string]) "T")
       ~right:(Type.union [Type.float]));
  assert_true
    (less_or_equal
       order
       ~left:
         (Type.variable
            ~constraints:(Type.Variable.Unary.Bound (Type.union [Type.float; Type.string]))
            "T")
       ~right:(Type.union [Type.float; Type.string; !"A"]));
  assert_false
    (less_or_equal
       order
       ~left:Type.string
       ~right:(Type.variable ~constraints:(Type.Variable.Unary.Bound Type.string) "T"));
  let float_string_variable =
    Type.variable ~constraints:(Type.Variable.Unary.Explicit [Type.float; Type.string]) "T"
  in
  assert_true
    (less_or_equal
       order
       ~left:float_string_variable
       ~right:(Type.union [float_string_variable; !"A"]));
  assert_true
    (less_or_equal
       order
       ~left:(Type.variable ~constraints:(Type.Variable.Unary.Bound !"A") "T")
       ~right:
         (Type.union [Type.variable ~constraints:(Type.Variable.Unary.Bound !"A") "T"; Type.string]));
  assert_true
    (less_or_equal
       order
       ~left:(Type.variable ~constraints:(Type.Variable.Unary.Bound !"A") "T")
       ~right:(Type.optional (Type.variable ~constraints:(Type.Variable.Unary.Bound !"A") "T")));
  assert_true
    (less_or_equal
       order
       ~left:(Type.variable ~constraints:(Type.Variable.Unary.Bound (Type.optional !"A")) "T")
       ~right:(Type.optional !"A"));
  assert_true
    (less_or_equal
       order
       ~left:(Type.variable ~constraints:(Type.Variable.Unary.Bound Type.integer) "T")
       ~right:(Type.union [Type.float; Type.string]));
  assert_true
    (less_or_equal
       order
       ~left:(Type.variable ~constraints:(Type.Variable.Unary.Bound Type.integer) "T")
       ~right:
         (Type.union
            [Type.variable ~constraints:(Type.Variable.Unary.Bound Type.integer) "T"; Type.string]));
  assert_true
    (less_or_equal
       order
       ~left:(Type.variable ~constraints:Type.Variable.Unary.Unconstrained "T")
       ~right:Type.Top);
  assert_true
    (less_or_equal
       order
       ~left:(Type.variable ~constraints:Type.Variable.Unary.Unconstrained "T")
       ~right:
         (Type.union [Type.variable ~constraints:Type.Variable.Unary.Unconstrained "T"; Type.string]));
  assert_true
    (less_or_equal
       order
       ~left:
         (Type.variable
            ~constraints:(Type.Variable.Unary.Bound (Type.union [Type.float; Type.string]))
            "T")
       ~right:(Type.union [Type.float; Type.string]));
  assert_false
    (less_or_equal
       order
       ~left:Type.integer
       ~right:(Type.variable ~constraints:(Type.Variable.Unary.Bound Type.float) "T"));
  assert_false
    (less_or_equal
       order
       ~left:Type.float
       ~right:(Type.variable ~constraints:(Type.Variable.Unary.Bound Type.integer) "T"));
  assert_false
    (less_or_equal
       order
       ~left:(Type.union [Type.string; Type.integer])
       ~right:
         (Type.variable ~constraints:(Type.Variable.Unary.Explicit [Type.string; Type.integer]) "T"));
  assert_false
    (less_or_equal
       order
       ~left:Type.integer
       ~right:(Type.variable ~constraints:(Type.Variable.Unary.Explicit [Type.string]) "T"));
  assert_false
    (less_or_equal
       order
       ~left:Type.integer
       ~right:(Type.variable ~constraints:Type.Variable.Unary.LiteralIntegers "T"));
  assert_true
    (less_or_equal
       order
       ~left:(Type.variable ~constraints:Type.Variable.Unary.LiteralIntegers "T")
       ~right:Type.integer);
  (* Behavioral subtyping of callables. *)
  let less_or_equal ?attributes ?is_protocol order ~left ~right =
    let aliases = function
      | "T_Unconstrained" -> Some (Type.variable "T_Unconstrained")
      | "T_int_bool" ->
          Some
            (Type.variable
               "T_int_bool"
               ~constraints:(Type.Variable.Unary.Explicit [Type.integer; Type.bool]))
      | _ -> None
    in
    let aliases = create_type_alias_table aliases in
    less_or_equal
      ?attributes
      ?is_protocol
      order
      ~left:(parse_callable ~aliases left)
      ~right:(parse_callable ~aliases right)
  in
  assert_true
    (less_or_equal order ~left:"typing.Callable[[int], int]" ~right:"typing.Callable[[int], int]");
  assert_false
    (less_or_equal order ~left:"typing.Callable[[str], int]" ~right:"typing.Callable[[int], int]");
  assert_false
    (less_or_equal order ~left:"typing.Callable[[int], int]" ~right:"typing.Callable[[str], int]");
  assert_false
    (less_or_equal order ~left:"typing.Callable[[int], str]" ~right:"typing.Callable[[int], int]");
  assert_false
    (less_or_equal order ~left:"typing.Callable[[int], float]" ~right:"typing.Callable[[int], int]");
  assert_true
    (less_or_equal order ~left:"typing.Callable[[int], int]" ~right:"typing.Callable[[int], float]");
  assert_true
    (less_or_equal order ~left:"typing.Callable[[float], int]" ~right:"typing.Callable[[int], int]");
  assert_false
    (less_or_equal order ~left:"typing.Callable[[int], int]" ~right:"typing.Callable[[float], int]");
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
    (less_or_equal order ~left:"typing.Callable[..., int]" ~right:"typing.Callable[..., float]");
  assert_true
    (less_or_equal order ~left:"typing.Callable[[int], int]" ~right:"typing.Callable[..., int]");
  assert_true
    (less_or_equal order ~left:"typing.Callable[..., int]" ~right:"typing.Callable[[int], float]");
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable[[Variable(object), Keywords(object)], int]"
       ~right:"typing.Callable[..., int]");
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable[[Variable(int), Keywords(object)], int]"
       ~right:"typing.Callable[..., int]");
  (* Callable classes. *)
  assert_true
    (less_or_equal order ~left:"FloatToStrCallable" ~right:"typing.Callable[[float], str]");
  (* Subtyping is handled properly for callable classes. *)
  assert_true (less_or_equal order ~left:"FloatToStrCallable" ~right:"typing.Callable[[int], str]");
  assert_false
    (less_or_equal order ~left:"FloatToStrCallable" ~right:"typing.Callable[[float], int]");
  (* Parametric classes are also callables. *)
  assert_true
    (less_or_equal order ~left:"ParametricCallableToStr[int]" ~right:"typing.Callable[[int], str]");
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
    (less_or_equal order ~left:"ParametricCallableToStr[int]" ~right:"typing.Callable[[int], int]");
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable[[Variable(int)], str]"
       ~right:"typing.Callable[[int], str]");
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable[[Variable(int)], str]"
       ~right:"typing.Callable[[int, int], str]");
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable[[Variable(str)], str]"
       ~right:"typing.Callable[[int], str]");
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable[[Variable(int)], str]"
       ~right:"typing.Callable[[Named(arg, int)], str]");
  (* Callables with keyword arguments. *)
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable[[Keywords(int)], str]"
       ~right:"typing.Callable[[Named(arg, int)], str]");
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable[[Variable(int), Keywords(int)], str]"
       ~right:"typing.Callable[[Named(arg, int)], str]");
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable[[Variable(str), Keywords(int)], str]"
       ~right:"typing.Callable[[Named(arg, int)], str]");
  (* Generic callables *)
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable[[Named(arg, T_Unconstrained)], T_Unconstrained]"
       ~right:"typing.Callable[[Named(arg, int)], str]");
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable[[Named(arg, T_Unconstrained)], T_Unconstrained]"
       ~right:"typing.Callable[[Named(arg, int)], int]");
  assert_false
    (less_or_equal
       order
       ~right:"typing.Callable[[Named(arg, int)], str]"
       ~left:"typing.Callable[[T_Unconstrained], T_Unconstrained]");
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable[[Named(arg, T_int_bool)], T_int_bool]"
       ~right:"typing.Callable[[Named(arg, int)], int]");
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable[[Named(arg, T_int_bool)], T_int_bool]"
       ~right:"typing.Callable[[Named(arg, str)], str]");
  (* Callables with overloads *)
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable[..., $bottom][[[int], int][[float], float]]"
       ~right:"typing.Callable[[int], int]");
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable[[object], object][[[str], int][[int], str]]"
       ~right:"typing.Callable[[object], object][[[int], str]]");
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable[[object], object][[[str], int][[int], str]]"
       ~right:"typing.Callable[[object], object][[[str], str]]");
  (* TypedDictionaries *)
  assert_true
    (less_or_equal
       order
       ~left:"mypy_extensions.TypedDict[('Alpha', True, ('foo', str), ('bar', int), ('baz', int))]"
       ~right:"mypy_extensions.TypedDict[('Beta', True, ('foo', str), ('bar', int))]");
  assert_true
    (less_or_equal
       order
       ~left:
         "mypy_extensions.TypedDict[('Alpha', False, ('foo', str), ('bar', int), ('baz', int))]"
       ~right:"mypy_extensions.TypedDict[('Beta', False, ('foo', str), ('bar', int))]");
  assert_false
    (less_or_equal
       order
       ~left:
         "mypy_extensions.TypedDict[('Alpha', False, ('foo', str), ('bar', int), ('baz', int))]"
       ~right:"mypy_extensions.TypedDict[('Beta', True, ('foo', str), ('bar', int))]");
  assert_false
    (less_or_equal
       order
       ~left:"mypy_extensions.TypedDict[('Alpha', True, ('foo', str), ('bar', int), ('baz', int))]"
       ~right:"mypy_extensions.TypedDict[('Beta', False, ('foo', str), ('bar', int))]");
  assert_false
    (less_or_equal
       order
       ~left:"mypy_extensions.TypedDict[('Alpha', True, ('foo', str), ('bar', float))]"
       ~right:"mypy_extensions.TypedDict[('Beta', True, ('foo', str), ('bar', int))]");
  assert_true
    (less_or_equal
       order
       ~left:"mypy_extensions.TypedDict[('Alpha', True, ('bar', int), ('foo', str))]"
       ~right:"mypy_extensions.TypedDict[('Beta', True, ('foo', str), ('bar', int))]");
  assert_true
    (less_or_equal
       order
       ~left:"mypy_extensions.TypedDict[('Alpha', True, ('bar', int), ('foo', int))]"
       ~right:"typing.Mapping[str, typing.Any]");
  assert_true
    (less_or_equal
       order
       ~left:"mypy_extensions.TypedDict[('Alpha', False, ('bar', int), ('foo', int))]"
       ~right:"typing.Mapping[str, typing.Any]");
  assert_false
    (less_or_equal
       order
       ~left:"mypy_extensions.TypedDict[('Alpha', True, ('bar', int), ('foo', int))]"
       ~right:"typing.Mapping[str, int]");
  assert_false
    (less_or_equal
       order
       ~left:"typing.Mapping[str, typing.Any]"
       ~right:"mypy_extensions.TypedDict[('Alpha', True, ('bar', int), ('foo', int))]");
  assert_false
    (less_or_equal
       order
       ~left:"mypy_extensions.TypedDict[('Alpha', True, ('bar', int), ('foo', int))]"
       ~right:"dict[str, typing.Any]");
  (* Literals *)
  assert_true
    (less_or_equal
       order
       ~left:"typing_extensions.Literal['a']"
       ~right:"typing_extensions.Literal['a']");
  assert_true (less_or_equal order ~left:"typing_extensions.Literal['a']" ~right:"str");
  assert_false (less_or_equal order ~left:"str" ~right:"typing_extensions.Literal['a']");
  assert_true
    (less_or_equal
       order
       ~left:"typing_extensions.Literal['a']"
       ~right:"typing_extensions.Literal['a', 'b']");
  (* Callback protocols *)
  let parse_annotation =
    let aliases = function
      | "_T" -> Some (Type.variable "_T")
      | _ -> None
    in
    let aliases = create_type_alias_table aliases in
    parse_callable ~aliases
  in
  let is_protocol = function
    | Type.Primitive "MatchesProtocol"
    | Type.Primitive "DoesNotMatchProtocol"
    | Type.Parametric { name = "B"; _ } ->
        true
    | _ -> false
  in
  let attributes = function
    | Type.Primitive "MatchesProtocol" ->
        Some
          (parse_attributes
             ~parse_annotation
             ~class_name:"MatchesProtocol"
             ["__call__", "typing.Callable[[int], str]"])
    | Type.Primitive "DoesNotMatchProtocol" ->
        Some
          (parse_attributes
             ~parse_annotation
             ~class_name:"DoesNotMatchProtocol"
             ["__call__", "typing.Callable[[str], int]"])
    | Type.Primitive "B" ->
        Some
          (parse_attributes
             ~parse_annotation
             ~class_name:"B"
             ["__call__", "typing.Callable[[_T], str]"])
    | _ -> failwith "getting attributes for wrong class"
  in
  assert_true
    (less_or_equal
       order
       ~is_protocol
       ~attributes
       ~left:"typing.Callable[[int], str]"
       ~right:"MatchesProtocol");
  assert_false
    (less_or_equal
       order
       ~is_protocol
       ~attributes
       ~left:"typing.Callable[[int], str]"
       ~right:"DoesNotMatchProtocol");
  assert_true
    (less_or_equal
       order
       ~is_protocol
       ~attributes
       ~left:"typing.Callable[[int], str]"
       ~right:"B[int]");
  assert_false
    (less_or_equal
       order
       ~is_protocol
       ~attributes
       ~left:"typing.Callable[[int], str]"
       ~right:"B[str]");
  let assert_less_or_equal ?(source = "") ~left ~right expected_result =
    let resolution =
      let source = parse source |> Preprocessing.preprocess in
      AnnotatedTest.populate_with_sources (source :: Test.typeshed_stubs ())
      |> fun environment -> TypeCheck.resolution environment ()
    in
    let parse_annotation annotation =
      annotation |> parse_single_expression |> Resolution.parse_annotation resolution
    in
    let left, right = parse_annotation left, parse_annotation right in
    assert_equal
      ~printer:(Printf.sprintf "%B")
      expected_result
      (Resolution.less_or_equal resolution ~left ~right)
  in
  assert_less_or_equal
    ~source:
      {|
      from typing import Generic, TypeVar
      T1 = TypeVar("T1")
      T2 = TypeVar("T2")
      class GenericBase(Generic[T1, T2]): pass
      class NonGenericChild(GenericBase): pass
    |}
    ~left:"NonGenericChild"
    ~right:"GenericBase[typing.Any, typing.Any]"
    true;
  (* This should get filtered by mismatch with any postprocessing *)
  assert_less_or_equal
    ~source:
      {|
      from typing import Generic, TypeVar
      T1 = TypeVar("T1")
      T2 = TypeVar("T2")
      class GenericBase(Generic[T1, T2]): pass
      class NonGenericChild(GenericBase): pass
    |}
    ~left:"NonGenericChild"
    ~right:"GenericBase[int, str]"
    false;
  assert_less_or_equal
    ~source:
      {|
      from typing import Generic, TypeVar
      T1 = TypeVar("T1", contravariant=True)
      T2 = TypeVar("T2", contravariant=True)
      class GenericBase(Generic[T1, T2]): pass
      class NonGenericChild(GenericBase): pass
    |}
    ~left:"GenericBase[typing.Any, typing.Any]"
    ~right:"GenericBase[int, str]"
    true;
  assert_less_or_equal
    ~source:
      {|
      from typing import Generic, TypeVar
      T1 = TypeVar("T1", contravariant=True)
      T2 = TypeVar("T2", contravariant=True)
      class GenericBase(Generic[T1, T2]): pass
      class NonGenericChild(GenericBase): pass
    |}
    ~left:"NonGenericChild"
    ~right:"GenericBase[int, str]"
    true;
  assert_less_or_equal
    ~source:
      {|
      from typing import Generic, TypeVar
      T1 = TypeVar("T1")
      T2 = TypeVar("T2")
      class GenericBase(Generic[T1, T2]): pass
      class NonGenericChild(GenericBase): pass
      class Grandchild(NonGenericChild): pass
    |}
    ~left:"Grandchild"
    ~right:"GenericBase[typing.Any, typing.Any]"
    true;
  ()


let test_is_compatible_with _ =
  let assert_is_compatible ?(order = default) left right =
    assert_true (is_compatible_with order ~left ~right)
  in
  let assert_not_compatible ?(order = default) left right =
    assert_false (is_compatible_with order ~left ~right)
  in
  let list_of_integer = Type.list Type.integer in
  let list_of_float = Type.list Type.float in
  let list_of_string = Type.list Type.string in
  let list_of_top = Type.list Type.Top in
  let list_of_any = Type.list Type.Any in
  let iterable_of_integer = Type.iterable Type.integer in
  let iterable_of_any = Type.iterable Type.Any in
  (* Any *)
  assert_is_compatible list_of_integer Type.Any;
  assert_is_compatible Type.Any list_of_integer;
  assert_is_compatible Type.none Type.Any;
  assert_is_compatible Type.Any Type.none;
  assert_is_compatible (Type.Primitive "A") Type.Any;
  assert_is_compatible Type.Any (Type.Primitive "A");
  assert_is_compatible Type.Top Type.Any;
  assert_is_compatible Type.Any Type.Top;
  assert_is_compatible list_of_integer list_of_any;
  assert_is_compatible list_of_any list_of_integer;
  assert_is_compatible Type.Any Type.Any;
  (* Top *)
  assert_is_compatible list_of_integer Type.Top;
  assert_not_compatible Type.Top list_of_integer;
  assert_is_compatible Type.none Type.Top;
  assert_not_compatible Type.Top Type.none;
  assert_is_compatible (Type.Primitive "A") Type.Top;
  assert_not_compatible Type.Top (Type.Primitive "A");
  assert_is_compatible list_of_integer list_of_top;
  assert_not_compatible list_of_top list_of_integer;
  assert_is_compatible Type.Top Type.Top;
  (* Basic *)
  assert_is_compatible list_of_integer list_of_integer;
  assert_is_compatible list_of_integer list_of_float;
  assert_not_compatible list_of_float list_of_integer;
  assert_not_compatible list_of_integer list_of_string;
  (* Optional *)
  assert_is_compatible Type.none (Type.optional Type.Any);
  assert_is_compatible (Type.optional Type.Any) Type.none;
  assert_is_compatible Type.none (Type.optional Type.Top);
  assert_not_compatible (Type.optional Type.Top) Type.none;
  assert_is_compatible list_of_integer (Type.optional list_of_integer);
  assert_is_compatible (Type.optional list_of_integer) (Type.optional list_of_integer);
  assert_is_compatible list_of_integer (Type.optional list_of_float);
  assert_is_compatible (Type.optional list_of_integer) (Type.optional list_of_float);
  assert_not_compatible list_of_float (Type.optional list_of_integer);
  assert_not_compatible list_of_integer (Type.optional list_of_string);
  (* Tuple *)
  assert_is_compatible
    (Type.tuple [list_of_integer; list_of_string])
    (Type.tuple [list_of_integer; list_of_string]);
  assert_is_compatible
    (Type.tuple [list_of_integer; list_of_string])
    (Type.tuple [list_of_float; list_of_string]);
  assert_is_compatible
    (Type.tuple [list_of_string; list_of_integer])
    (Type.tuple [list_of_string; list_of_float]);
  assert_is_compatible
    (Type.tuple [list_of_integer; list_of_integer])
    (Type.tuple [list_of_float; list_of_float]);
  assert_is_compatible
    (Type.tuple [list_of_integer; list_of_integer])
    (Type.Tuple (Unbounded list_of_integer));
  assert_is_compatible
    (Type.tuple [list_of_integer; list_of_integer])
    (Type.Tuple (Unbounded list_of_float));
  assert_not_compatible
    (Type.tuple [list_of_integer; list_of_string])
    (Type.tuple [list_of_string; list_of_string]);
  assert_not_compatible
    (Type.tuple [list_of_float; list_of_integer])
    (Type.tuple [list_of_integer; list_of_float]);
  assert_not_compatible
    (Type.tuple [list_of_string; list_of_integer])
    (Type.Tuple (Unbounded list_of_float));
  (* Union *)
  assert_is_compatible list_of_integer (Type.union [list_of_integer]);
  assert_is_compatible list_of_integer (Type.union [list_of_float]);
  assert_is_compatible list_of_float (Type.union [list_of_float; list_of_integer]);
  assert_is_compatible list_of_string (Type.union [list_of_float; list_of_string]);
  assert_is_compatible list_of_string (Type.union [list_of_float; Type.optional list_of_string]);
  assert_not_compatible list_of_string (Type.union [list_of_float; list_of_integer]);
  assert_is_compatible
    (Type.union [Type.integer; Type.string])
    (Type.union [Type.integer; Type.string]);
  assert_is_compatible
    (Type.union [Type.integer; Type.string])
    (Type.union [Type.string; Type.integer]);
  assert_is_compatible
    (Type.set (Type.union [Type.integer; Type.string]))
    (Type.set (Type.union [Type.string; Type.integer]));
  assert_is_compatible
    (Type.union [Type.integer; list_of_integer])
    (Type.union [Type.integer; Type.integer; list_of_integer]);
  assert_not_compatible (Type.union [Type.integer; Type.string]) Type.integer;
  assert_not_compatible
    (Type.union [Type.integer; Type.string; list_of_float])
    (Type.union [Type.integer; list_of_float]);
  assert_not_compatible
    (Type.set (Type.union [Type.integer; Type.string]))
    (Type.set (Type.union [list_of_string; list_of_integer]));
  (* Parametric *)
  assert_is_compatible
    (Type.dictionary ~key:list_of_integer ~value:list_of_string)
    (Type.dictionary ~key:list_of_integer ~value:list_of_string);
  assert_is_compatible
    (Type.dictionary ~key:list_of_integer ~value:list_of_string)
    (Type.dictionary ~key:list_of_float ~value:list_of_string);
  assert_is_compatible
    (Type.dictionary ~key:list_of_string ~value:list_of_integer)
    (Type.dictionary ~key:list_of_string ~value:list_of_float);
  assert_is_compatible
    (Type.dictionary ~key:list_of_integer ~value:list_of_integer)
    (Type.dictionary ~key:list_of_float ~value:list_of_float);
  assert_not_compatible
    (Type.dictionary ~key:list_of_integer ~value:list_of_integer)
    (Type.dictionary ~key:list_of_string ~value:list_of_integer);
  assert_not_compatible
    (Type.dictionary ~key:list_of_string ~value:list_of_string)
    (Type.dictionary ~key:list_of_string ~value:list_of_integer);
  assert_not_compatible (Type.dictionary ~key:list_of_string ~value:list_of_integer) list_of_string;
  assert_not_compatible list_of_string (Type.dictionary ~key:list_of_string ~value:list_of_integer);
  assert_is_compatible
    (Type.dictionary ~key:list_of_integer ~value:list_of_string)
    (Type.dictionary ~key:list_of_any ~value:list_of_any);
  assert_is_compatible
    (Type.dictionary ~key:list_of_any ~value:list_of_any)
    (Type.dictionary ~key:list_of_integer ~value:list_of_string);
  assert_is_compatible
    (Type.dictionary ~key:list_of_integer ~value:list_of_string)
    (Type.dictionary ~key:list_of_top ~value:list_of_top);
  assert_not_compatible
    (Type.dictionary ~key:list_of_top ~value:list_of_top)
    (Type.dictionary ~key:list_of_integer ~value:list_of_string);
  assert_is_compatible list_of_integer iterable_of_integer;
  assert_not_compatible list_of_string iterable_of_integer;
  assert_not_compatible iterable_of_integer list_of_integer;
  assert_is_compatible list_of_any iterable_of_any;
  assert_not_compatible iterable_of_any list_of_any;
  assert_is_compatible list_of_any iterable_of_integer;
  assert_is_compatible list_of_integer iterable_of_any;
  ()


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
  assert_false
    (less_or_equal
       variance_order
       ~left:(Type.parametric "LinkedList" [Type.integer])
       ~right:(Type.parametric "LinkedList" [Type.Any]));
  assert_false
    (less_or_equal
       variance_order
       ~left:(Type.parametric "LinkedList" [Type.Any])
       ~right:(Type.parametric "LinkedList" [Type.integer]));
  assert_strict_less
    ~order:variance_order
    ~left:(Type.parametric "LinkedList" [Type.integer])
    ~right:(Type.parametric "LinkedList" [Type.Top]);
  (* Covariant. *)
  assert_strict_less
    ~order:variance_order
    ~left:(Type.parametric "Box" [Type.integer])
    ~right:(Type.parametric "Box" [Type.float]);
  assert_strict_less
    ~order:variance_order
    ~left:(Type.parametric "Box" [Type.integer])
    ~right:(Type.parametric "Box" [Type.Any]);
  (* Contravariant. *)
  assert_strict_less
    ~order:variance_order
    ~left:(Type.parametric "Sink" [Type.float])
    ~right:(Type.parametric "Sink" [Type.integer]);
  assert_strict_less
    ~order:variance_order
    ~left:(Type.parametric "Sink" [Type.Any])
    ~right:(Type.parametric "Sink" [Type.integer]);
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
  let assert_join ?(order = default) ?(aliases = fun _ -> None) left right expected =
    let parse_annotation source =
      let integer =
        try
          Int.of_string source |> ignore;
          true
        with
        | _ -> false
      in
      if integer then
        Type.Primitive source
      else
        parse_single_expression source |> Type.create ~aliases
    in
    assert_type_equal
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
  assert_join "typing.List[float]" "float[int]" "typing.Union[typing.List[float], float[int]]";
  (* TODO(T41082573) throw here instead of unioning *)
  assert_join "typing.Tuple[int, int]" "typing.Iterator[int]" "typing.Iterator[int]";
  let bound_list_variadic =
    Type.Variable.Variadic.List.create "Ts" |> Type.Variable.Variadic.List.mark_as_bound
  in
  assert_join
    ~aliases:(function
      | "Ts" -> Some (Type.VariableAlias (ListVariadic bound_list_variadic))
      | _ -> None)
    "typing.Tuple[Ts]"
    "typing.Tuple[float, ...]"
    "typing.Tuple[object, ...]";
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
    assert_type_equal expected (join order left right)
  in
  assert_join_types Type.undeclared Type.Top (Type.Union [Type.undeclared; Type.Top]);
  assert_join_types Type.Top Type.undeclared (Type.Union [Type.undeclared; Type.Top]);
  assert_join_types Type.undeclared Type.Bottom Type.undeclared;
  assert_join_types Type.Bottom Type.undeclared Type.undeclared;
  assert_join_types ~order !"0" Type.undeclared (Type.Union [!"0"; Type.undeclared]);
  assert_join_types ~order Type.undeclared !"0" (Type.Union [!"0"; Type.undeclared]);
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
    insert order Type.Any;
    insert order Type.Top;
    add_simple Type.object_primitive;
    add_simple (Type.variable "_1");
    add_simple (Type.variable "_2");
    add_simple (Type.variable "_T");
    add_simple Type.string;
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
    connect order ~predecessor:Type.float ~successor:Type.object_primitive;
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
    connect order ~predecessor:!"B" ~successor:!"typing.Generic" ~parameters:[Type.variable "_T"];
    connect
      order
      ~predecessor:!"B"
      ~successor:!"C"
      ~parameters:[Type.union [Type.variable "_T"; Type.float]];
    connect order ~predecessor:!"C" ~successor:!"typing.Generic" ~parameters:[Type.variable "_T"];
    connect order ~predecessor:!"typing.Generic" ~successor:Type.Any;
    connect order ~predecessor:Type.Bottom ~successor:!"CallableClass";
    connect
      order
      ~parameters:[parse_callable "typing.Callable[[int], str]"]
      ~predecessor:!"CallableClass"
      ~successor:!"typing.Callable";
    connect order ~predecessor:!"typing.Callable" ~successor:Type.Top;
    let callable =
      let aliases = function
        | "_T" -> Some (Type.variable "_T")
        | _ -> None
      in
      let aliases = create_type_alias_table aliases in
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
    Identifier.Table.of_alist_exn
      ["_1", Type.variable "_1"; "_2", Type.variable "_2"; "_T", Type.variable "_T"]
    |> Identifier.Table.find
  in
  let aliases = create_type_alias_table aliases in
  assert_join
    ~order
    ~aliases
    "A[int, str]"
    "C[$bottom]"
    "C[typing.Union[float, typing.Tuple[int, str]]]";
  assert_join ~order:disconnected_order "A" "B" "typing.Union[A, B]";
  assert_join "typing.Type[int]" "typing.Type[str]" "typing.Type[typing.Union[int, str]]";
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
  assert_join
    "typing.Callable[..., int][[..., str]]"
    "typing.Callable[..., int]"
    "typing.Union[typing.Callable[..., int][[..., str]], typing.Callable[..., int]]";
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
    "typing.Union[typing.Callable[[Named(b, int)], int], typing.Callable[[Named(a, int)], int]]";
  assert_join
    "typing.Callable[..., typing.Any]"
    "typing.Callable[[int], int]"
    "typing.Callable[[int], typing.Any]";
  (* Classes with __call__ are callables. *)
  assert_join ~order "CallableClass" "typing.Callable[[int], str]" "typing.Callable[[int], str]";
  assert_join ~order "typing.Callable[[int], str]" "CallableClass" "typing.Callable[[int], str]";
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
    "mypy_extensions.TypedDict[('Alpha', True, ('bar', int), ('foo', str), ('ben', int))]"
    "mypy_extensions.TypedDict[('Beta', True, ('foo', str), ('bar', int), ('baz', int))]"
    "mypy_extensions.TypedDict[('$anonymous', True, ('foo', str), ('bar', int))]";
  assert_join
    "mypy_extensions.TypedDict[('Alpha', False, ('bar', int), ('foo', str), ('ben', int))]"
    "mypy_extensions.TypedDict[('Beta', False, ('foo', str), ('bar', int), ('baz', int))]"
    "mypy_extensions.TypedDict[('$anonymous', False, ('foo', str), ('bar', int))]";
  assert_join
    "mypy_extensions.TypedDict[('Alpha', True, ('bar', int), ('ben', int))]"
    "mypy_extensions.TypedDict[('Beta', True, ('foo', str))]"
    "mypy_extensions.TypedDict[('$anonymous', True)]";
  assert_join
    "mypy_extensions.TypedDict[('Alpha', True, ('bar', int), ('foo', str), ('ben', int))]"
    "mypy_extensions.TypedDict[('Beta', True, ('foo', int))]"
    "typing.Mapping[str, typing.Any]";
  assert_join
    "mypy_extensions.TypedDict[('Alpha', True, ('bar', int), ('foo', str), ('ben', int))]"
    "mypy_extensions.TypedDict[('Beta', False, ('foo', str), ('bar', int), ('baz', int))]"
    "typing.Mapping[str, typing.Any]";
  assert_join
    "mypy_extensions.TypedDict[('Alpha', True, ('bar', str), ('foo', str), ('ben', str))]"
    "typing.Mapping[str, str]"
    "typing.Mapping[str, typing.Any]";
  assert_join
    "mypy_extensions.TypedDict[('Alpha', False, ('bar', str), ('foo', str), ('ben', str))]"
    "typing.Mapping[str, str]"
    "typing.Mapping[str, typing.Any]";
  assert_join
    "mypy_extensions.TypedDict[('Alpha', True, ('bar', str), ('foo', str), ('ben', str))]"
    "typing.Mapping[int, str]"
    "typing.Mapping[typing.Any, typing.Any]";
  assert_join
    "mypy_extensions.TypedDict[('Alpha', True, ('bar', str), ('foo', str), ('ben', str))]"
    "typing.Dict[str, str]"
    ( "typing.Union["
    ^ "mypy_extensions.TypedDict[('Alpha', True, ('bar', str), ('foo', str), ('ben', str))], "
    ^ "typing.Dict[str, str]"
    ^ "]" );
  (* Variables. *)
  assert_type_equal
    (join order Type.integer (Type.variable "T"))
    (Type.union [Type.integer; Type.variable "T"]);
  assert_type_equal
    (join
       order
       Type.integer
       (Type.variable ~constraints:(Type.Variable.Unary.Bound Type.string) "T"))
    (Type.union
       [Type.integer; Type.variable ~constraints:(Type.Variable.Unary.Bound Type.string) "T"]);
  assert_type_equal
    (join
       order
       Type.string
       (Type.variable ~constraints:(Type.Variable.Unary.Explicit [Type.float; Type.integer]) "T"))
    (Type.union
       [ Type.string;
         Type.variable ~constraints:(Type.Variable.Unary.Explicit [Type.float; Type.integer]) "T"
       ]);
  assert_type_equal
    (join order Type.string (Type.variable ~constraints:Type.Variable.Unary.LiteralIntegers "T"))
    (Type.union [Type.string; Type.variable ~constraints:Type.Variable.Unary.LiteralIntegers "T"]);
  assert_type_equal
    (join
       order
       (Type.literal_integer 7)
       (Type.variable ~constraints:Type.Variable.Unary.LiteralIntegers "T"))
    (Type.union
       [Type.literal_integer 7; Type.variable ~constraints:Type.Variable.Unary.LiteralIntegers "T"]);
  (* Variance. *)
  assert_type_equal
    (join
       variance_order
       (Type.parametric "LinkedList" [Type.integer])
       (Type.parametric "LinkedList" [Type.Top]))
    (Type.parametric "LinkedList" [Type.Top]);
  assert_type_equal
    (join
       variance_order
       (Type.parametric "LinkedList" [Type.Top])
       (Type.parametric "LinkedList" [Type.integer]))
    (Type.parametric "LinkedList" [Type.Top]);
  assert_type_equal
    (join
       variance_order
       (Type.parametric "LinkedList" [Type.Bottom])
       (Type.parametric "LinkedList" [Type.Top]))
    (Type.parametric "LinkedList" [Type.Top]);
  assert_type_equal
    (join
       variance_order
       (Type.parametric "LinkedList" [Type.Top])
       (Type.parametric "LinkedList" [Type.Bottom]))
    (Type.parametric "LinkedList" [Type.Top]);
  assert_type_equal
    (join
       variance_order
       (Type.parametric "LinkedList" [Type.Any])
       (Type.parametric "LinkedList" [Type.Top]))
    (Type.parametric "LinkedList" [Type.Top]);
  assert_type_equal
    (join
       variance_order
       (Type.parametric "LinkedList" [Type.Top])
       (Type.parametric "LinkedList" [Type.Any]))
    (Type.parametric "LinkedList" [Type.Top]);
  assert_type_equal
    (join
       variance_order
       (Type.parametric "LinkedList" [Type.Top])
       (Type.parametric "LinkedList" [Type.Top]))
    (Type.parametric "LinkedList" [Type.Top]);
  assert_type_equal
    (join
       variance_order
       (Type.parametric "Map" [Type.integer; Type.integer])
       (Type.parametric "Map" [Type.Top; Type.Top]))
    (Type.parametric "Map" [Type.Top; Type.Top]);
  assert_type_equal
    (join
       variance_order
       (Type.parametric "Map" [Type.integer; Type.integer])
       (Type.parametric "Map" [Type.Top; Type.integer]))
    (Type.parametric "Map" [Type.Top; Type.integer]);
  assert_type_equal
    (join
       variance_order
       (Type.parametric "Map" [Type.integer; Type.integer])
       (Type.parametric "Map" [Type.Top; Type.string]))
    (Type.parametric "Map" [Type.Top; Type.Any]);
  assert_type_equal
    (join
       variance_order
       (Type.parametric "LinkedList" [Type.integer])
       (Type.parametric "LinkedList" [Type.Any]))
    (Type.parametric "LinkedList" [Type.Any]);
  assert_type_equal
    (join
       variance_order
       (Type.parametric "LinkedList" [Type.Any])
       (Type.parametric "LinkedList" [Type.integer]))
    (Type.parametric "LinkedList" [Type.Any]);
  let variance_aliases =
    Identifier.Table.of_alist_exn
      [ "_T", Type.variable "_T";
        "_T_co", Type.variable "_T_co" ~variance:Covariant;
        "_T_contra", Type.variable "_T_contra" ~variance:Contravariant ]
    |> Identifier.Table.find
  in
  let variance_aliases = create_type_alias_table variance_aliases in
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
  (* Literals *)
  assert_type_equal
    (join order (Type.literal_string "A") (Type.literal_string "A"))
    (Type.literal_string "A");
  assert_type_equal (join order (Type.literal_string "A") (Type.literal_string "B")) Type.string;
  assert_type_equal
    (join order (Type.literal_string "A") Type.integer)
    (Type.union [Type.string; Type.integer]);
  let assert_join ?(source = "") ~left ~right expected_result =
    let resolution =
      let source = parse source |> Preprocessing.preprocess in
      AnnotatedTest.populate_with_sources (source :: Test.typeshed_stubs ())
      |> fun environment -> TypeCheck.resolution environment ()
    in
    let parse_annotation annotation =
      annotation |> parse_single_expression |> Resolution.parse_annotation resolution
    in
    let left, right = parse_annotation left, parse_annotation right in
    assert_type_equal (parse_annotation expected_result) (Resolution.join resolution left right)
  in
  assert_join
    ~source:
      {|
      from typing import Generic, TypeVar
      T1 = TypeVar("T1", covariant=True)
      T2 = TypeVar("T2", covariant=True)
      class GenericBase(Generic[T1, T2]): pass
      class NonGenericChild(GenericBase): pass
    |}
    ~left:"NonGenericChild"
    ~right:"GenericBase[int, str]"
    "GenericBase[typing.Any, typing.Any]";
  ()


let test_meet _ =
  let assert_meet ?(order = default) ?(aliases = fun _ -> None) left right expected =
    let parse_annotation source =
      let integer =
        try
          Int.of_string source |> ignore;
          true
        with
        | _ -> false
      in
      if integer then
        Type.Primitive source
      else
        parse_single_expression source |> Type.create ~aliases
    in
    assert_type_equal
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
  (* TODO(T39185893): current implementation of meet has some limitations which need to be fixed *)
  assert_meet "typing.Union[int, str]" "typing.Union[int, typing.Optional[str]]" "$bottom";
  assert_meet "typing.Union[int, typing.Optional[str]]" "typing.Optional[str]" "$bottom";
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
    "mypy_extensions.TypedDict[('Alpha', True, ('bar', int), ('foo', str), ('ben', int))]"
    "mypy_extensions.TypedDict[('Beta', True, ('foo', str), ('bar', int), ('baz', int))]"
    ( "mypy_extensions.TypedDict"
    ^ "[('$anonymous', True, ('bar', int), ('baz', int), ('ben', int), ('foo', str))]" );
  assert_meet
    "mypy_extensions.TypedDict[('Alpha', False, ('bar', int), ('foo', str), ('ben', int))]"
    "mypy_extensions.TypedDict[('Beta', False, ('foo', str), ('bar', int), ('baz', int))]"
    ( "mypy_extensions.TypedDict"
    ^ "[('$anonymous', False, ('bar', int), ('baz', int), ('ben', int), ('foo', str))]" );
  assert_meet
    "mypy_extensions.TypedDict[('Alpha', True, ('bar', int), ('foo', str), ('ben', int))]"
    "mypy_extensions.TypedDict[('Beta', True, ('foo', int))]"
    "$bottom";
  assert_meet
    "mypy_extensions.TypedDict[('Alpha', False, ('bar', int), ('foo', str), ('ben', int))]"
    "mypy_extensions.TypedDict[('Beta', True, ('foo', str), ('bar', int), ('baz', int))]"
    "$bottom";
  assert_meet
    "mypy_extensions.TypedDict[('Alpha', True, ('bar', int), ('foo', str), ('ben', int))]"
    "typing.Mapping[str, typing.Any]"
    "$bottom";
  (* Variables. *)
  assert_type_equal (meet default Type.integer (Type.variable "T")) Type.Bottom;
  assert_type_equal
    (meet
       default
       Type.integer
       (Type.variable ~constraints:(Type.Variable.Unary.Bound Type.float) "T"))
    Type.Bottom;
  assert_type_equal
    (meet
       default
       Type.string
       (Type.variable ~constraints:(Type.Variable.Unary.Explicit [Type.float; Type.string]) "T"))
    Type.Bottom;
  (* Undeclared. *)
  assert_type_equal (meet default Type.undeclared Type.Bottom) Type.Bottom;
  assert_type_equal (meet default Type.Bottom Type.undeclared) Type.Bottom;
  (* Variance. *)
  assert_type_equal
    (meet
       variance_order
       (Type.parametric "LinkedList" [Type.integer])
       (Type.parametric "LinkedList" [Type.Top]))
    (Type.parametric "LinkedList" [Type.integer]);
  assert_type_equal
    (meet
       variance_order
       (Type.parametric "LinkedList" [Type.Top])
       (Type.parametric "LinkedList" [Type.integer]))
    (Type.parametric "LinkedList" [Type.integer]);
  assert_type_equal
    (meet
       variance_order
       (Type.parametric "LinkedList" [Type.integer])
       (Type.parametric "LinkedList" [Type.Any]))
    (Type.parametric "LinkedList" [Type.Bottom]);
  assert_type_equal
    (meet
       variance_order
       (Type.parametric "LinkedList" [Type.Any])
       (Type.parametric "LinkedList" [Type.integer]))
    (Type.parametric "LinkedList" [Type.Bottom]);
  assert_meet ~order:variance_order "Derived[int]" "Base[int]" "Derived[int]";
  assert_meet ~order:variance_order "Derived[float]" "Base[float]" "Derived[float]";
  assert_meet ~order:variance_order "Derived[int]" "Base[float]" "Derived[int]";
  assert_meet ~order:variance_order "Derived[float]" "Base[int]" "Derived[float]";
  assert_meet ~order:multiplane_variance_order "B[int, float]" "A[int, float]" "B[int, float]";
  assert_meet ~order:multiplane_variance_order "B[int, int]" "A[int, float]" "B[int, int]";
  assert_meet ~order:multiplane_variance_order "B[float, int]" "A[int, float]" "B[float, int]";
  assert_meet ~order:multiplane_variance_order "B[float, float]" "A[int, float]" "B[float, float]";
  assert_meet ~order:multiplane_variance_order "B[int, float]" "A[float, float]" "B[int, float]";
  assert_meet ~order:multiplane_variance_order "B[int, int]" "A[float, float]" "B[int, int]";
  assert_meet ~order:multiplane_variance_order "B[float, int]" "A[float, float]" "B[float, int]";
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
  assert_meet ~order:parallel_planes_variance_order "B[float, float]" "A[int, int]" "B[int, float]";
  let make_potentially_inconsistent_order ~x_before_y =
    (* Corresponds to
     *  T = typing.TypeVar("T")
     * T1 = typing.TypeVar("T1")
     * class B(typing.Generic[T, T1]): pass
     * class A(typing.Generic[T]): pass
     * class X(B[T, str]): pass
     * class Y(B[int, str]): pass
     * class M(A[T], X[T], Y[T]): pass *)
    let order = Builder.create () |> TypeOrder.handler in
    insert order !"A";
    insert order !"B";
    insert order !"X";
    insert order !"Y";
    insert order !"M";
    insert order Type.generic_primitive;
    insert order Type.string;
    insert order Type.integer;
    let variable = Type.Variable (Type.Variable.Unary.create "T") in
    let variable2 = Type.Variable (Type.Variable.Unary.create "T2") in
    connect order ~predecessor:!"M" ~successor:Type.generic_primitive ~parameters:[variable];
    connect order ~predecessor:!"M" ~successor:!"A" ~parameters:[variable];
    connect order ~predecessor:!"M" ~successor:!"X" ~parameters:[variable];
    connect order ~predecessor:!"M" ~successor:!"Y" ~parameters:[variable];
    connect order ~predecessor:!"A" ~successor:Type.generic_primitive ~parameters:[variable];
    let connect_x () =
      connect order ~predecessor:!"X" ~successor:Type.generic_primitive ~parameters:[variable];
      connect order ~predecessor:!"X" ~successor:!"B" ~parameters:[variable; Type.string]
    in
    if x_before_y then connect_x ();
    connect order ~predecessor:!"Y" ~successor:Type.generic_primitive ~parameters:[variable];
    connect order ~predecessor:!"Y" ~successor:!"B" ~parameters:[Type.integer; variable];
    if not x_before_y then connect_x ();
    connect
      order
      ~predecessor:!"B"
      ~successor:Type.generic_primitive
      ~parameters:[variable; variable2];
    order
  in
  assert_meet
    ~order:(make_potentially_inconsistent_order ~x_before_y:true)
    "B[int, str]"
    "A[str]"
    "M[str]";
  assert_meet
    ~order:(make_potentially_inconsistent_order ~x_before_y:false)
    "B[int, str]"
    "A[str]"
    "M[str]";
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
  assert_equal (least_upper_bound butterfly !"0" !"1") [!"3"; !"2"]


let test_greatest_lower_bound _ =
  let assert_greatest_lower_bound ~order type1 type2 expected =
    let actual = greatest_lower_bound order type1 type2 |> List.sort ~compare:Type.compare in
    assert_equal ~printer:(List.to_string ~f:Type.show) actual expected
  in
  assert_greatest_lower_bound ~order:diamond_order Type.Bottom Type.Bottom [Type.Bottom];
  assert_greatest_lower_bound ~order:diamond_order Type.Bottom !"D" [Type.Bottom];
  assert_greatest_lower_bound ~order:diamond_order Type.Bottom !"A" [Type.Bottom];
  assert_greatest_lower_bound ~order:diamond_order !"A" !"C" [!"C"];
  assert_greatest_lower_bound ~order:diamond_order !"A" !"B" [!"B"];
  assert_greatest_lower_bound ~order:diamond_order !"A" !"D" [!"D"];
  assert_greatest_lower_bound ~order:diamond_order !"B" !"D" [!"D"];
  assert_greatest_lower_bound ~order:diamond_order !"B" !"C" [!"D"];
  assert_greatest_lower_bound ~order:diamond_order Type.Top !"B" [!"B"];
  assert_greatest_lower_bound ~order:diamond_order Type.Top Type.Top [Type.Top];
  assert_greatest_lower_bound ~order:butterfly !"2" !"3" [!"0"; !"1"]


let test_instantiate_parameters _ =
  let order =
    { handler = default;
      constructor = (fun _ -> None);
      attributes = (fun _ -> None);
      is_protocol = (fun _ -> false);
      any_is_bottom = false;
      protocol_assumptions = ProtocolAssumptions.empty
    }
  in
  assert_equal
    (instantiate_successors_parameters
       order
       ~source:(Type.list Type.string)
       ~target:!"typing.Iterator")
    (Some [Type.string]);
  assert_equal
    (instantiate_successors_parameters
       order
       ~source:(Type.dictionary ~key:Type.integer ~value:Type.string)
       ~target:!"typing.Iterator")
    (Some [Type.integer]);
  assert_equal
    (instantiate_successors_parameters order ~source:Type.string ~target:!"typing.Iterable")
    (Some [Type.string]);
  assert_equal
    (instantiate_successors_parameters
       order
       ~source:(Type.tuple [Type.integer; Type.integer])
       ~target:!"typing.Iterable")
    (Some [Type.integer]);
  assert_equal
    (instantiate_successors_parameters order ~source:!"AnyIterable" ~target:!"typing.Iterable")
    (Some [Type.Any]);
  (* If you're not completely specified, fill all with anys *)
  assert_equal
    (instantiate_successors_parameters order ~source:!"PartiallySpecifiedDict" ~target:!"dict")
    (Some [Type.Any; Type.Any]);
  (* If you're over-specified, fill all with anys *)
  assert_equal
    (instantiate_successors_parameters order ~source:!"OverSpecifiedDict" ~target:!"dict")
    (Some [Type.Any; Type.Any]);
  (* Don't do a search when starting from bottom *)
  assert_equal
    (instantiate_successors_parameters
       order
       ~source:!"NonGenericContainerChild"
       ~target:!"GenericContainer")
    (Some [Type.integer; Type.string]);
  assert_equal
    (instantiate_successors_parameters order ~source:Type.Bottom ~target:!"GenericContainer")
    (Some [Type.Any; Type.Any]);
  ()


let test_deduplicate _ =
  let (module Handler : TypeOrder.Handler) =
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Bottom;
    insert order Type.Top;
    insert order !"0";
    insert order !"1";
    connect order ~parameters:[Type.Top; Type.Top] ~predecessor:!"0" ~successor:!"1";
    connect order ~parameters:[Type.Top] ~predecessor:!"0" ~successor:!"1";
    deduplicate order ~annotations:[!"0"; !"1"];
    order
  in
  let index_of annotation = Handler.find_unsafe (Handler.indices ()) annotation in
  let module TargetAsserter (ListOrSet : TypeOrder.Target.ListOrSet) = struct
    let assert_targets edges from target parameters create =
      assert_equal
        ~cmp:ListOrSet.equal
        ~printer:(ListOrSet.to_string ~f:Target.show)
        (Handler.find_unsafe edges (index_of !from))
        (create { Target.target = index_of !target; parameters })
  end
  in
  let module ForwardAsserter = TargetAsserter (TypeOrder.Target.List) in
  let module BackwardsAsserter = TargetAsserter (TypeOrder.Target.Set) in
  ForwardAsserter.assert_targets (Handler.edges ()) "0" "1" [Type.Top] (fun target -> [target]);
  BackwardsAsserter.assert_targets (Handler.backedges ()) "1" "0" [Type.Top] (fun target ->
      TypeOrder.Target.Set.of_list [target])


let test_remove_extra_edges _ =
  (* 0 -> 1 -> 2 -> 3
   *  |----^         ^
   *  |--------------^
   *)
  let (module Handler : TypeOrder.Handler) =
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
    [{ Target.target = one_index; parameters = [] }];
  assert_equal
    ~cmp:TypeOrder.Target.Set.equal
    (Handler.find_unsafe (Handler.backedges ()) three_index)
    (TypeOrder.Target.Set.of_list [{ Target.target = two_index; parameters = [] }])


let test_connect_annotations_to_top _ =
  (* Partial partial order:
   *  0 - 2
   *  |
   *  1   3 *)
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
    order
  in
  assert_equal (least_upper_bound order !"1" !"2") [!"3"];
  (* Ensure that the backedge gets added as well *)
  assert_equal (greatest_lower_bound order !"1" !"3") [!"1"]


let test_sort_bottom_edges _ =
  (* Partial partial order:
   *  0 - 2
   *  |
   *  1   3 *)
  let (module Handler : Handler) =
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Bottom;
    insert order Type.Top;
    insert order !"0";
    insert order !"1";
    insert order !"2";
    insert order !"3";
    connect order ~predecessor:Type.Bottom ~successor:!"0";
    connect order ~predecessor:Type.Bottom ~successor:!"2";
    connect order ~predecessor:Type.Bottom ~successor:!"3";
    connect order ~predecessor:Type.Bottom ~successor:!"1";
    connect order ~predecessor:!"0" ~successor:Type.Top;
    connect order ~predecessor:!"3" ~successor:Type.Top;
    connect order ~predecessor:!"2" ~successor:Type.Top;
    connect order ~predecessor:!"1" ~successor:Type.Top;
    order
  in
  let assert_bottom_edges expected =
    let bottom_edges =
      Handler.find_unsafe (Handler.edges ()) (Handler.find_unsafe (Handler.indices ()) Type.Bottom)
      |> List.map ~f:(fun { Target.target; _ } ->
             Handler.find_unsafe (Handler.annotations ()) target)
      |> List.map ~f:Type.show
    in
    assert_equal ~printer:(List.to_string ~f:ident) expected bottom_edges
  in
  assert_bottom_edges ["1"; "3"; "2"; "0"];
  (* We sort by target, which is not necessarily alphabetical. *)
  TypeOrder.sort_bottom_edges (module Handler) ~bottom:Type.Bottom;
  assert_bottom_edges ["1"; "2"; "3"; "0"]


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
    order
  in
  assert_raises TypeOrder.Cyclic (fun _ -> check_integrity order);
  (* 0 -> 1
   * ^    |
   *  \   v
   * .  - 2 -> 3 *)
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
    order
  in
  assert_raises TypeOrder.Cyclic (fun _ -> check_integrity order);
  let order =
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Bottom;
    insert order !"0";
    order
  in
  assert_raises TypeOrder.Incomplete (fun _ -> check_integrity order);
  let order =
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Top;
    insert order !"0";
    order
  in
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
    order
  in
  let (module Handler) = order in
  assert_equal
    ~printer:ident
    ( {|
        digraph {
          129913994[label="undefined"]
          360125662[label="0"]
          544641955[label="3"]
          648017920[label="unknown"]
          874630001[label="2"]
          1061160138[label="1"]
          360125662 -> 874630001
          360125662 -> 1061160138[label="(str)"]
          874630001 -> 544641955
          1061160138 -> 544641955
        }
     |}
    |> Test.trim_extra_indentation )
    ("\n" ^ TypeOrder.to_dot order)


let test_variables _ =
  let order =
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Bottom;
    insert order Type.Top;
    insert order Type.generic_primitive;
    insert order !"A";
    insert order !"B";
    connect
      order
      ~parameters:[Type.variable "T"]
      ~predecessor:!"A"
      ~successor:Type.generic_primitive;
    connect order ~predecessor:Type.Bottom ~successor:!"A";
    connect order ~predecessor:Type.Bottom ~successor:!"B";
    connect order ~predecessor:!"B" ~successor:Type.Top;
    connect order ~predecessor:Type.generic_primitive ~successor:Type.Top;
    order
  in
  let assert_variables ~expected source =
    let aliases _ = None in
    let annotation = parse_single_expression source |> Type.create ~aliases in
    assert_equal expected (TypeOrder.variables order annotation)
  in
  assert_variables ~expected:None "B";
  assert_variables ~expected:(Some [Type.variable "T"]) "A";
  assert_variables ~expected:(Some [Type.variable "T"]) "A[int]";
  assert_variables ~expected:None "Nonexistent"


let test_is_instantiated _ =
  let order =
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Bottom;
    insert order Type.Top;
    insert order Type.generic_primitive;
    insert order !"A";
    insert order !"B";
    connect order ~predecessor:Type.Bottom ~successor:!"A";
    connect order ~predecessor:Type.Bottom ~successor:!"B";
    connect order ~predecessor:!"A" ~successor:Type.Top;
    connect order ~predecessor:!"B" ~successor:Type.Top;
    order
  in
  assert_true (TypeOrder.is_instantiated order (Type.Primitive "A"));
  assert_true (TypeOrder.is_instantiated order (Type.Primitive "B"));
  assert_false (TypeOrder.is_instantiated order (Type.Primitive "C"));
  assert_true (TypeOrder.is_instantiated order (Type.parametric "A" [Type.Primitive "B"]));
  assert_true (TypeOrder.is_instantiated order (Type.parametric "A" [Type.Primitive "A"]));
  assert_true
    (TypeOrder.is_instantiated order (Type.parametric "A" [Type.Primitive "A"; Type.Primitive "B"]));
  assert_false
    (TypeOrder.is_instantiated order (Type.parametric "A" [Type.Primitive "C"; Type.Primitive "B"]));
  assert_false
    (TypeOrder.is_instantiated order (Type.parametric "C" [Type.Primitive "A"; Type.Primitive "B"]))


let test_solve_less_or_equal _ =
  let environment =
    let configuration = Configuration.Analysis.create () in
    let populate source =
      let environment =
        let environment = Environment.Builder.create () in
        Test.populate
          ~configuration
          (Environment.handler environment)
          (parse source :: typeshed_stubs ());
        environment
      in
      Environment.handler environment
    in
    populate
      {|
      class C: ...
      class D(C): ...
      class Q: ...
      T_Unconstrained = typing.TypeVar('T_Unconstrained')
      T_Bound_C = typing.TypeVar('T_Bound_C', bound=C)
      T_Bound_D = typing.TypeVar('T_Bound_D', bound=D)
      T_Bound_Union = typing.TypeVar('T_Bound_Union', bound=typing.Union[int, str])
      T_Bound_Union_C_Q = typing.TypeVar('T_Bound_Union_C_Q', bound=typing.Union[C, Q])
      T_Bound_Union = typing.TypeVar('T_Bound_Union', bound=typing.Union[int, str])
      T_C_Q = typing.TypeVar('T_C_Q', C, Q)
      T_D_Q = typing.TypeVar('T_D_Q', D, Q)
      T_C_Q_int = typing.TypeVar('T_C_Q_int', C, Q, int)
      V = pyre_extensions.ParameterSpecification("V")
      Ts = pyre_extensions.ListVariadic("Ts")
      T2s = pyre_extensions.ListVariadic("T2s")

      T = typing.TypeVar('T')
      T1 = typing.TypeVar('T1')
      T2 = typing.TypeVar('T2')
      T3 = typing.TypeVar('T3')
      T4 = typing.TypeVar('T4')
      class G_invariant(typing.Generic[T]):
        pass
      T_Covariant = typing.TypeVar('T_Cov', covariant=True)
      class G_covariant(typing.Generic[T_Covariant]):
        pass

      class Constructable:
        def Constructable.__init__(self, x: int) -> None:
          pass
    |}
  in
  let resolution = TypeCheck.resolution environment () in
  let assert_solve
      ~left
      ~right
      ?(is_protocol = fun _ -> false)
      ?(attributes = fun _ -> None)
      ?constraints
      ?(leave_unbound_in_left = [])
      ?(postprocess = Type.Variable.mark_all_variables_as_bound)
      expected
    =
    let handler =
      let constructor instantiated =
        Resolution.class_definition resolution instantiated
        >>| Class.create
        >>| Class.constructor ~instantiated ~resolution
      in
      { handler = Resolution.order resolution;
        constructor;
        attributes;
        is_protocol;
        any_is_bottom = false;
        protocol_assumptions = ProtocolAssumptions.empty
      }
    in
    let parse_annotation annotation =
      annotation
      |> parse_single_expression
      |> Resolution.parse_annotation
           ~allow_untracked:true
           ~allow_invalid_type_parameters:true
           resolution
    in
    let left =
      let constraints annotation =
        match annotation with
        | Type.Variable { variable = variable_name; _ }
          when not (List.exists leave_unbound_in_left ~f:(( = ) variable_name)) ->
            Some (Type.Variable.mark_all_variables_as_bound annotation)
        | _ -> None
      in
      parse_annotation left |> Type.instantiate ~constraints
    in
    let right = parse_annotation right in
    let expected =
      let parse_pairs pairs =
        let parse_pair (variable, value) =
          match parse_annotation variable with
          | Type.Variable variable ->
              Type.Variable.UnaryPair (variable, parse_annotation value |> postprocess)
          | Type.Primitive primitive -> (
              let (module Handler : Environment.Handler) = environment in
              let parse_parameters parameters =
                match
                  parse_annotation (Printf.sprintf "typing.Callable[%s, typing.Any]" parameters)
                with
                | Type.Callable { implementation = { parameters; _ }; _ } -> parameters
                | _ -> failwith "impossible"
              in
              let parse_ordered_types ordered =
                match parse_annotation (Printf.sprintf "typing.Tuple[%s]" ordered) with
                | Type.Tuple (Bounded ordered) -> ordered
                | _ -> failwith "impossible"
              in
              match Handler.aliases primitive with
              | Some (Type.VariableAlias (ParameterVariadic variable)) ->
                  Type.Variable.ParameterVariadicPair (variable, parse_parameters value)
              | Some (Type.VariableAlias (ListVariadic variable)) ->
                  Type.Variable.ListVariadicPair (variable, parse_ordered_types value)
              | _ -> failwith "not available" )
          | _ -> failwith "not a variable"
        in
        List.map pairs ~f:parse_pair
      in
      List.map expected ~f:parse_pairs |> List.map ~f:TypeConstraints.Solution.create
    in
    let constraints =
      let add_bounds sofar (key, (lower_bound, upper_bound)) =
        let variable =
          match parse_annotation key with
          | Type.Variable variable -> variable
          | _ -> failwith "not a variable"
        in
        let unwrap optional =
          Option.value_exn ~message:"given pre-constraints are invalid" optional
        in
        let sofar =
          lower_bound
          >>| parse_annotation
          >>| postprocess
          >>| (fun bound -> Type.Variable.UnaryPair (variable, bound))
          >>| (fun pair -> OrderedConstraints.add_lower_bound sofar ~order:handler ~pair |> unwrap)
          |> Option.value ~default:sofar
        in
        upper_bound
        >>| parse_annotation
        >>| postprocess
        >>| (fun bound -> Type.Variable.UnaryPair (variable, bound))
        >>| (fun pair -> OrderedConstraints.add_lower_bound sofar ~order:handler ~pair |> unwrap)
        |> Option.value ~default:sofar
      in
      constraints
      >>| List.fold ~init:TypeConstraints.empty ~f:add_bounds
      |> Option.value ~default:TypeConstraints.empty
    in
    let list_of_maps_compare left right =
      let and_map_equal sofar left right = sofar && TypeConstraints.Solution.equal left right in
      match List.fold2 left right ~init:true ~f:and_map_equal with
      | List.Or_unequal_lengths.Ok comparison -> comparison
      | List.Or_unequal_lengths.Unequal_lengths -> false
    in
    let list_of_map_print map =
      map
      |> List.map ~f:TypeConstraints.Solution.show
      |> String.concat ~sep:";\n"
      |> Printf.sprintf "{\n%s\n}"
    in
    assert_equal
      ~cmp:list_of_maps_compare
      ~printer:list_of_map_print
      expected
      ( solve_less_or_equal handler ~constraints ~left ~right
      |> List.filter_map ~f:(OrderedConstraints.solve ~order:handler) )
  in
  assert_solve
    ~leave_unbound_in_left:["T_Unconstrained"]
    ~left:"typing.Optional[T_Unconstrained]"
    ~right:"object"
    [[]];
  assert_solve ~left:"C" ~right:"T_Unconstrained" [["T_Unconstrained", "C"]];
  assert_solve ~left:"D" ~right:"T_Unconstrained" [["T_Unconstrained", "D"]];
  assert_solve ~left:"Q" ~right:"T_Unconstrained" [["T_Unconstrained", "Q"]];
  assert_solve ~left:"C" ~right:"T_Bound_C" [["T_Bound_C", "C"]];
  assert_solve ~left:"D" ~right:"T_Bound_C" [["T_Bound_C", "D"]];
  assert_solve ~left:"Q" ~right:"T_Bound_C" [];
  assert_solve ~left:"C" ~right:"T_Bound_D" [];
  assert_solve ~left:"C" ~right:"T_C_Q" [["T_C_Q", "C"]];
  (* An explicit type variable can only be bound to its constraints *)
  assert_solve ~left:"D" ~right:"T_C_Q" [["T_C_Q", "C"]];
  assert_solve ~left:"C" ~right:"T_D_Q" [];
  assert_solve
    ~left:"typing.Union[int, G_invariant[str], str]"
    ~right:"T_Unconstrained"
    [["T_Unconstrained", "typing.Union[int, G_invariant[str], str]"]];
  assert_solve ~left:"typing.Union[D, C]" ~right:"T_Bound_C" [["T_Bound_C", "C"]];
  assert_solve
    ~constraints:["T_Unconstrained", (Some "Q", None)]
    ~left:"G_invariant[C]"
    ~right:"G_invariant[T_Unconstrained]"
    [];
  assert_solve
    ~constraints:["T_Unconstrained", (Some "Q", None)]
    ~left:"G_covariant[C]"
    ~right:"G_covariant[T_Unconstrained]"
    [["T_Unconstrained", "typing.Union[Q, C]"]];
  assert_solve
    ~left:"typing.Optional[C]"
    ~right:"typing.Optional[T_Unconstrained]"
    [["T_Unconstrained", "C"]];
  assert_solve ~left:"C" ~right:"typing.Optional[T_Unconstrained]" [["T_Unconstrained", "C"]];
  assert_solve
    ~left:"typing.Tuple[C, ...]"
    ~right:"typing.Tuple[T_Unconstrained, ...]"
    [["T_Unconstrained", "C"]];
  assert_solve
    ~left:"typing.Tuple[C, Q, D]"
    ~right:"typing.Tuple[T_Unconstrained, T_Unconstrained, C]"
    [["T_Unconstrained", "typing.Union[C, Q]"]];
  assert_solve
    ~left:"typing.Tuple[D, ...]"
    ~right:"typing.Tuple[T_Unconstrained, T_Unconstrained, C]"
    [["T_Unconstrained", "D"]];
  assert_solve
    ~left:"typing.Tuple[C, Q, D]"
    ~right:"typing.Tuple[T_Unconstrained, ...]"
    [["T_Unconstrained", "typing.Union[C, Q]"]];
  assert_solve
    ~left:"G_covariant[C]"
    ~right:"typing.Union[G_covariant[T_Unconstrained], int]"
    [["T_Unconstrained", "C"]];
  assert_solve
    ~left:"typing.Type[int]"
    ~right:"typing.Callable[[], T_Unconstrained]"
    (* there are two int constructor overloads *)
    [["T_Unconstrained", "int"]; ["T_Unconstrained", "int"]];
  assert_solve
    ~left:"typing.Optional[typing.Tuple[C, Q, typing.Callable[[D, int], C]]]"
    ~right:"typing.Optional[typing.Tuple[T, T, typing.Callable[[T, T], T]]]"
    [];
  assert_solve
    ~left:"typing.Optional[typing.Tuple[C, C, typing.Callable[[C, C], C]]]"
    ~right:"typing.Optional[typing.Tuple[T, T, typing.Callable[[T, T], T]]]"
    [["T", "C"]];
  (* Bound => Bound *)
  assert_solve ~left:"T_Bound_D" ~right:"T_Bound_C" [["T_Bound_C", "T_Bound_D"]];
  assert_solve ~left:"T_Bound_C" ~right:"T_Bound_D" [];
  assert_solve ~left:"T_Bound_Union" ~right:"T_Bound_Union" [["T_Bound_Union", "T_Bound_Union"]];
  (* Bound => Explicit *)
  assert_solve ~left:"T_Bound_D" ~right:"T_C_Q" [["T_C_Q", "C"]];
  assert_solve ~left:"T_Bound_C" ~right:"T_D_Q" [];
  (* Explicit => Bound *)
  assert_solve ~left:"T_D_Q" ~right:"T_Bound_Union_C_Q" [["T_Bound_Union_C_Q", "T_D_Q"]];
  assert_solve ~left:"T_D_Q" ~right:"T_Bound_D" [];
  (* Explicit => Explicit *)
  assert_solve ~left:"T_C_Q" ~right:"T_C_Q_int" [["T_C_Q_int", "T_C_Q"]];
  (* This one is theoretically solvable, but only if we're willing to introduce dependent variables
     as the only sound solution here would be
   *  T_C_Q_int => T_new <: C if T_D_Q is D, Q if T_D_Q is Q *)
  assert_solve ~left:"T_D_Q" ~right:"T_C_Q_int" [];
  assert_solve
    ~leave_unbound_in_left:["T_Unconstrained"]
    ~left:"typing.Callable[[T_Unconstrained], T_Unconstrained]"
    ~right:"typing.Callable[[int], int]"
    [[]];
  assert_solve
    ~left:"typing.Callable[[int], int]"
    ~right:"typing.Callable[[T_Unconstrained], T_Unconstrained]"
    [["T_Unconstrained", "int"]];
  assert_solve
    ~leave_unbound_in_left:["T"]
    ~left:"typing.Callable[[T], T]"
    ~right:"typing.Callable[[T_Unconstrained], T_Unconstrained]"
    [[]];
  assert_solve
    ~leave_unbound_in_left:["T"]
    ~left:"typing.Callable[[T], G_invariant[T]]"
    ~right:"typing.Callable[[T_Unconstrained], T_Unconstrained]"
    [];
  assert_solve
    ~leave_unbound_in_left:["T1"]
    ~left:"typing.Callable[[T1], typing.Tuple[T1, T2]]"
    ~right:"typing.Callable[[T3], typing.Tuple[T3, T4]]"
    [["T4", "T2"]];
  assert_solve
    ~left:"typing.Type[Constructable]"
    ~right:"typing.Callable[[T3], T4]"
    [["T3", "int"; "T4", "Constructable"]];
  assert_solve
    ~left:"typing.Callable[[typing.Union[int, str]], str]"
    ~right:"typing.Callable[[int], T4]"
    [["T4", "str"]];
  assert_solve
    ~left:"typing.Callable[[typing.Union[int, str], int], str]"
    ~right:"typing.Callable[[int, T3], T4]"
    [["T3", "int"; "T4", "str"]];
  assert_solve
    ~leave_unbound_in_left:["T3"]
    ~left:"typing.Callable[[T3], T3]"
    ~right:"typing.Callable[[typing.Union[int, str]], object][[[int], T1][[str], T2]] "
    [["T2", "str"; "T1", "int"]];
  (* Callback protocols *)
  let parse_annotation annotation =
    annotation |> parse_single_expression |> Resolution.parse_annotation resolution
  in
  let is_protocol = function
    | Type.Parametric { name = "G_invariant"; _ } -> true
    | _ -> false
  in
  let attributes = function
    | Type.Primitive "G_invariant" ->
        Some
          (parse_attributes
             ~parse_annotation
             ~class_name:"B"
             ["__call__", "typing.Callable[[T], str]"])
    | _ -> failwith "getting attributes for wrong class"
  in
  assert_solve
    ~is_protocol
    ~attributes
    ~left:"typing.Callable[[int], str]"
    ~right:"G_invariant[T1]"
    [["T1", "int"]];
  (* Multiple options *)
  assert_solve
    ~left:"typing.List[int]"
    ~right:"typing.Union[typing.List[T1], T1]"
    [["T1", "int"]; ["T1", "typing.List[int]"]];
  assert_solve
    ~left:"typing.Tuple[typing.List[int], typing.List[int]]"
    ~right:"typing.Tuple[typing.Union[typing.List[T1], T1], T1]"
    [["T1", "typing.List[int]"]];
  assert_solve
    ~left:"typing.Tuple[typing.List[int], typing.List[int]]"
    ~right:"typing.Tuple[typing.Union[typing.List[T1], T1], T1]"
    [["T1", "typing.List[int]"]];
  assert_solve
    ~left:
      ( "typing.Callable[[typing.Union[int, str]], typing.Union[int, str]]"
      ^ "[[[int], str][[str], int]]" )
    ~right:"typing.Callable[[T3], T4]"
    [ ["T3", "int"; "T4", "str"];
      ["T3", "str"; "T4", "int"];
      ["T3", "typing.Union[int, str]"; "T4", "typing.Union[int, str]"] ];
  (* Free Variable <-> Free Variable constraints *)
  assert_solve
    ~postprocess:Fn.id
    ~leave_unbound_in_left:["T1"]
    ~left:"T1"
    ~right:"T2"
    [["T2", "T1"]; ["T1", "T2"]];
  assert_solve
    ~postprocess:Fn.id
    ~leave_unbound_in_left:["T"]
    ~left:"typing.Callable[[T], T]"
    ~right:"typing.Callable[[T1], T2]"
    [["T2", "T1"]; ["T1", "T2"]];
  assert_solve
    ~leave_unbound_in_left:["T"]
    ~left:"typing.Tuple[typing.Callable[[T], T], int]"
    ~right:"typing.Tuple[typing.Callable[[T1], T2], T1]"
    [["T2", "int"; "T1", "int"]];
  assert_solve
    ~left:"typing.Callable[[int, int], int]"
    ~right:"typing.Callable[V, int]"
    [["V", "[int, int]"]];
  (* We need to ensure that return values are still checked *)
  assert_solve ~left:"typing.Callable[[int, int], int]" ~right:"typing.Callable[V, str]" [];
  assert_solve
    ~left:"typing.Callable[[int, int], int]"
    ~right:"typing.Callable[V, T1]"
    [["T1", "int"; "V", "[int, int]"]];
  (* We should be able to capture the same parameter set twice *)
  assert_solve
    ~left:"typing.Tuple[typing.Callable[[int, int], int], typing.Callable[[int, int], int]]"
    ~right:"typing.Tuple[typing.Callable[V, int], typing.Callable[V, int]]"
    [["V", "[int, int]"]];
  (* We currently do not find a way to take both [int, int] and [int, str]. A correct solution
     would be [int, Intersection[int, str]]. This is probably not desired *)
  assert_solve
    ~left:"typing.Tuple[typing.Callable[[int, int], int], typing.Callable[[int, str], int]]"
    ~right:"typing.Tuple[typing.Callable[V, int], typing.Callable[V, int]]"
    [];
  (* There is no common interface between a callable that requires exactly two arguments and one
     that requires exactly one *)
  assert_solve
    ~left:"typing.Tuple[typing.Callable[[int, int], int], typing.Callable[[int], int]]"
    ~right:"typing.Tuple[typing.Callable[V, int], typing.Callable[V, int]]"
    [];
  assert_solve
    ~left:"typing.Tuple[typing.Callable[[int, int], int], typing.Callable[[int], int]]"
    ~right:"typing.Tuple[typing.Callable[V, int], typing.Callable[V, int]]"
    [];
  assert_solve
    ~left:"typing.Tuple[int, str, bool]"
    ~right:"typing.Tuple[Ts]"
    [["Ts", "int, str, bool"]];
  assert_solve
    ~left:"typing.Tuple[Ts]"
    ~right:"typing.Tuple[int, str, bool]"
    [["Ts", "int, str, bool"]];
  assert_solve
    ~left:"typing.Tuple[typing.Tuple[int, str], typing.Tuple[bool, str]]"
    ~right:"typing.Tuple[typing.Tuple[Ts], typing.Tuple[Ts]]"
    [["Ts", "typing.Union[bool, int], str"]];
  assert_solve
    ~left:"typing.Tuple[typing.Tuple[int, str], typing.Tuple[bool, str, int]]"
    ~right:"typing.Tuple[typing.Tuple[Ts], typing.Tuple[Ts]]"
    [];
  assert_solve ~left:"typing.Tuple[Ts]" ~right:"typing.Tuple[T2s]" [["T2s", "Ts"]; ["Ts", "T2s"]];
  assert_solve ~left:"typing.Tuple[...]" ~right:"typing.Tuple[Ts]" [["Ts", "..."]];
  ()


let test_is_consistent_with _ =
  let is_consistent_with =
    let order =
      { handler = default;
        constructor = (fun _ -> None);
        attributes = (fun _ -> None);
        is_protocol = (fun _ -> false);
        any_is_bottom = false;
        protocol_assumptions = ProtocolAssumptions.empty
      }
    in
    is_consistent_with order
  in
  assert_true (is_consistent_with Type.Bottom Type.Top);
  assert_false (is_consistent_with Type.integer Type.string);
  assert_true (is_consistent_with Type.Any Type.string);
  assert_true (is_consistent_with Type.integer Type.Any);
  assert_false (is_consistent_with (Type.Optional Type.integer) (Type.Optional Type.string));
  assert_true (is_consistent_with (Type.Optional Type.Any) (Type.Optional Type.string));
  assert_false (is_consistent_with (Type.list Type.integer) (Type.list Type.string));
  assert_true (is_consistent_with (Type.list Type.Any) (Type.list Type.string));
  assert_false
    (is_consistent_with
       (Type.dictionary ~key:Type.string ~value:Type.integer)
       (Type.dictionary ~key:Type.string ~value:Type.string));
  assert_true
    (is_consistent_with
       (Type.dictionary ~key:Type.string ~value:Type.Any)
       (Type.dictionary ~key:Type.string ~value:Type.string));
  assert_true
    (is_consistent_with
       (Type.dictionary ~key:Type.Any ~value:Type.Any)
       (Type.dictionary ~key:Type.string ~value:(Type.list Type.integer)));
  assert_true
    (is_consistent_with
       (Type.dictionary ~key:Type.Any ~value:Type.Any)
       (Type.dictionary
          ~key:Type.string
          ~value:(Type.dictionary ~key:Type.string ~value:Type.integer)));
  assert_true
    (is_consistent_with
       (Type.dictionary ~key:Type.Any ~value:Type.Any)
       (Type.Optional (Type.dictionary ~key:Type.string ~value:Type.string)));
  assert_true
    (is_consistent_with
       (Type.dictionary ~key:Type.Any ~value:Type.bool)
       (Type.parametric "typing.Mapping" [Type.integer; Type.bool]));
  assert_false
    (is_consistent_with
       (Type.dictionary ~key:Type.Any ~value:Type.bool)
       (Type.parametric "collections.OrderedDict" [Type.integer; Type.bool]));
  assert_false
    (is_consistent_with
       (Type.dictionary ~key:Type.integer ~value:Type.bool)
       (Type.parametric "collections.OrderedDict" [Type.Any; Type.bool]));
  assert_true
    (is_consistent_with
       (Type.parametric "collections.OrderedDict" [Type.integer; Type.bool])
       (Type.dictionary ~key:Type.Any ~value:Type.bool));
  assert_true
    (is_consistent_with
       (Type.parametric "collections.OrderedDict" [Type.Any; Type.bool])
       (Type.dictionary ~key:Type.integer ~value:Type.bool));
  assert_true (is_consistent_with (Type.list Type.Any) (Type.iterable Type.string));
  assert_true (is_consistent_with (Type.list Type.integer) (Type.sequence Type.Any));
  assert_true (is_consistent_with (Type.iterable Type.string) (Type.Optional Type.Any));
  assert_false (is_consistent_with (Type.iterable Type.string) (Type.Optional Type.string));
  assert_false (is_consistent_with (Type.iterable Type.string) (Type.list Type.Any));
  assert_false (is_consistent_with (Type.iterable Type.Any) (Type.list Type.string));
  assert_false (is_consistent_with (Type.iterable Type.integer) (Type.set Type.Any));
  assert_false
    (is_consistent_with
       (Type.parametric "typing.AbstractSet" [Type.object_primitive])
       (Type.set Type.Any));
  assert_true
    (is_consistent_with
       (Type.set Type.Any)
       (Type.parametric "typing.AbstractSet" [Type.object_primitive]));
  assert_false
    (is_consistent_with
       (Type.tuple [Type.string; Type.string])
       (Type.tuple [Type.string; Type.integer]));
  assert_true
    (is_consistent_with
       (Type.tuple [Type.string; Type.string])
       (Type.tuple [Type.string; Type.Any]));
  assert_false
    (is_consistent_with
       (Type.Tuple (Type.Unbounded Type.integer))
       (Type.Tuple (Type.Unbounded Type.string)));
  assert_true
    (is_consistent_with
       (Type.Tuple (Type.Unbounded Type.integer))
       (Type.Tuple (Type.Unbounded Type.Any)));
  assert_true
    (is_consistent_with
       (Type.Tuple (Type.Bounded (Concrete [Type.integer; Type.Any])))
       (Type.Tuple (Type.Unbounded Type.integer)));
  assert_true
    (is_consistent_with
       (Type.Tuple (Type.Bounded (Concrete [Type.integer; Type.string])))
       (Type.Tuple (Type.Unbounded Type.Any)));
  assert_false
    (is_consistent_with
       (Type.Tuple (Type.Bounded (Concrete [Type.integer; Type.string])))
       (Type.Tuple (Type.Unbounded Type.string)));
  assert_false
    (is_consistent_with
       (Type.union [Type.integer; Type.string])
       (Type.union [Type.integer; Type.float]));
  assert_true
    (is_consistent_with
       (Type.union [Type.integer; Type.string])
       (Type.union [Type.integer; Type.Any]));
  assert_true (is_consistent_with (Type.union [Type.integer; Type.Any]) Type.integer);
  assert_false (is_consistent_with (Type.iterator Type.integer) (Type.generator Type.Any));
  assert_true (is_consistent_with (Type.generator Type.Any) (Type.iterator Type.integer));
  assert_false
    (is_consistent_with
       (Type.iterator (Type.list Type.integer))
       (Type.generator (Type.list Type.Any)));
  assert_true
    (is_consistent_with
       (Type.generator (Type.list Type.Any))
       (Type.iterator (Type.list Type.integer)));
  assert_false (is_consistent_with (Type.iterator Type.integer) (Type.generator Type.float));
  assert_false
    (is_consistent_with (Type.Union [Type.list Type.integer; Type.string]) (Type.list Type.Any));
  assert_true (is_consistent_with (Type.Callable.create ~annotation:Type.integer ()) Type.Any);
  assert_true (is_consistent_with Type.Any (Type.Callable.create ~annotation:Type.integer ()));
  assert_true
    (is_consistent_with
       Type.Any
       (Type.union [Type.integer; Type.Callable.create ~annotation:Type.integer ()]));
  assert_true
    (is_consistent_with
       (parse_callable "typing.Callable[[typing.Any], int]")
       (parse_callable "typing.Callable[[str], int]"));
  assert_true
    (is_consistent_with
       (parse_callable "typing.Callable[[int], typing.Any]")
       (parse_callable "typing.Callable[[int], int]"));
  assert_false
    (is_consistent_with
       (parse_callable "typing.Callable[[int], typing.Any]")
       (parse_callable "typing.Callable[[str], int]"));
  assert_false
    (is_consistent_with
       (parse_callable "typing.Callable[[typing.Any, typing.Any], typing.Any]")
       (parse_callable "typing.Callable[[typing.Any], typing.Any]"))


let test_instantiate_protocol_parameters _ =
  let assert_instantiate_protocol_parameters
      ?context
      ~classes
      ~protocols
      ~candidate
      ~protocol
      expected
    =
    let resolution =
      let configuration = Configuration.Analysis.create () in
      let environment =
        let environment = Environment.Builder.create () in
        let source = context >>| parse |> Option.to_list in
        Test.populate ~configuration (Environment.handler environment) (source @ typeshed_stubs ());
        environment
      in
      TypeCheck.resolution (Environment.handler environment) ()
    in
    let parse_annotation annotation =
      annotation
      |> parse_single_expression
      |> Resolution.parse_annotation
           resolution
           ~allow_untracked:true
           ~allow_invalid_type_parameters:true
    in
    let optional_list_printer optional_list =
      optional_list
      >>| List.map ~f:Type.show
      >>| String.concat ~sep:", "
      >>| Printf.sprintf "[%s]"
      |> Option.value ~default:"None"
    in
    let parse_attributes =
      let parse_class (class_name, attributes) =
        class_name, parse_attributes attributes ~class_name ~parse_annotation
      in
      List.map ~f:parse_class
    in
    let order =
      let classes, protocols = parse_attributes classes, parse_attributes protocols in
      let attributes = function
        | Type.Primitive primitive ->
            List.Assoc.find (classes @ protocols) primitive ~equal:String.equal
        | _ -> None
      in
      let is_protocol annotation =
        match Type.split annotation with
        | Type.Primitive primitive, _ -> List.Assoc.mem protocols primitive ~equal:String.equal
        | _ -> false
      in
      { handler = Resolution.order resolution;
        constructor = (fun _ -> None);
        attributes;
        is_protocol;
        any_is_bottom = false;
        protocol_assumptions = ProtocolAssumptions.empty
      }
    in
    assert_equal
      ~printer:optional_list_printer
      (expected >>| List.map ~f:parse_annotation)
      (instantiate_protocol_parameters order ~candidate:(parse_annotation candidate) ~protocol)
  in
  (* Simple attribute protocols *)
  assert_instantiate_protocol_parameters
    ~context:"class P(): pass"
    ~classes:["A", []]
    ~protocols:["P", []]
    ~candidate:"A"
    ~protocol:"P"
    (Some []);
  assert_instantiate_protocol_parameters
    ~context:"class P(): pass"
    ~classes:["A", ["prop", "int"]]
    ~protocols:["P", ["prop", "int"]]
    ~candidate:"A"
    ~protocol:"P"
    (Some []);
  assert_instantiate_protocol_parameters
    ~context:"class P(): pass"
    ~classes:["A", ["prop", "str"]]
    ~protocols:["P", ["prop", "int"]]
    ~candidate:"A"
    ~protocol:"P"
    None;
  assert_instantiate_protocol_parameters
    ~context:{|
      T1 = typing.TypeVar("T1")
      class P(typing.Generic[T1]): pass
    |}
    ~classes:["A", ["prop", "int"]]
    ~protocols:["P", ["prop", "T1"]]
    ~candidate:"A"
    ~protocol:"P"
    (Some ["int"]);
  (* Simple method protocols *)
  assert_instantiate_protocol_parameters
    ~context:"class P(): pass"
    ~classes:["A", ["method", "typing.Callable[[int], str]"]]
    ~protocols:["P", ["method", "typing.Callable[[int], str]"]]
    ~candidate:"A"
    ~protocol:"P"
    (Some []);
  assert_instantiate_protocol_parameters
    ~context:"class P(): pass"
    ~classes:["A", ["othermethod", "typing.Callable[[int], str]"]]
    ~protocols:["P", ["method", "typing.Callable[[int], str]"]]
    ~candidate:"A"
    ~protocol:"P"
    None;
  assert_instantiate_protocol_parameters
    ~context:"class P(): pass"
    ~classes:["A", ["method", "typing.Callable[[int], str]"]]
    ~protocols:
      ["P", ["method", "typing.Callable[[int], str]"; "othermethod", "typing.Callable[[int], str]"]]
    ~candidate:"A"
    ~protocol:"P"
    None;
  assert_instantiate_protocol_parameters
    ~context:{|
      T1 = typing.TypeVar("T1")
      class P(typing.Generic[T1]): pass
    |}
    ~classes:["A", ["method", "typing.Callable[[int], str]"]]
    ~protocols:["P", ["method", "typing.Callable[[int], T1]"]]
    ~candidate:"A"
    ~protocol:"P"
    (Some ["str"]);
  (* Primitive recursive protocol, primitive recursive candidate *)
  assert_instantiate_protocol_parameters
    ~context:"class P(): pass"
    ~classes:["A", ["prop", "A"]]
    ~protocols:["P", ["prop", "P"]]
    ~candidate:"A"
    ~protocol:"P"
    (Some []);
  assert_instantiate_protocol_parameters
    ~context:"class P(): pass"
    ~classes:["A", ["prop", "int"]]
    ~protocols:["P", ["prop", "P"]]
    ~candidate:"A"
    ~protocol:"P"
    None;
  assert_instantiate_protocol_parameters
    ~context:{|
      T1 = typing.TypeVar("T1")
      class P(typing.Generic[T1]): pass
    |}
    ~classes:["A", ["prop", "int"; "recursive_prop", "A"]]
    ~protocols:["P", ["prop", "T1"; "recursive_prop", "P[T1]"]]
    ~candidate:"A"
    ~protocol:"P"
    (Some ["int"]);
  (* Ideally this would work, but avoiding for now *)
  assert_instantiate_protocol_parameters
    ~context:{|
      T1 = typing.TypeVar("T1")
      class P(typing.Generic[T1]): pass
    |}
    ~classes:["A", ["prop", "int"; "recursive_prop", "A"]]
    ~protocols:["P", ["prop", "T1"; "recursive_prop", "P[int]"]]
    ~candidate:"A"
    ~protocol:"P"
    None;
  (* Protocol depends on other protocol *)
  assert_instantiate_protocol_parameters
    ~context:{|
      class P1(): pass
      class P2(): pass
    |}
    ~classes:["A", ["prop", "B"]; "B", ["prop", "int"]]
    ~protocols:["P1", ["prop", "P2"]; "P2", ["prop", "int"]]
    ~candidate:"A"
    ~protocol:"P1"
    (Some []);
  ()


let test_disconnect_successors _ =
  let order () =
    let order = Builder.create () |> TypeOrder.handler in
    insert order Type.Bottom;
    insert order Type.Top;
    insert order !"a";
    insert order !"b";
    insert order !"1";
    insert order !"2";
    connect order ~predecessor:Type.Bottom ~successor:!"a";
    connect order ~predecessor:Type.Bottom ~successor:!"b";
    connect order ~predecessor:!"a" ~successor:!"1";
    connect order ~predecessor:!"b" ~successor:!"1";
    connect order ~predecessor:!"1" ~successor:!"2";
    connect order ~predecessor:!"2" ~successor:Type.Top;
    order
  in
  let assert_backedges_equal wrapped_left unwrapped_right =
    assert_equal
      ~cmp:TypeOrder.Target.Set.equal
      wrapped_left
      (TypeOrder.Target.Set.of_list unwrapped_right)
  in
  let () =
    let (module Handler) = order () in
    let index key = Handler.find_unsafe (Handler.indices ()) !key in
    TypeOrder.disconnect_successors (module Handler) [!"1"];
    assert_equal (Handler.find_unsafe (Handler.edges ()) (index "1")) [];
    assert_backedges_equal (Handler.find_unsafe (Handler.backedges ()) (index "2")) [];
    assert_equal
      (Handler.find_unsafe (Handler.edges ()) (index "a"))
      [{ Target.target = index "1"; parameters = [] }]
  in
  let () =
    let (module Handler) = order () in
    let index key = Handler.find_unsafe (Handler.indices ()) !key in
    TypeOrder.disconnect_successors (module Handler) [!"a"];
    assert_equal (Handler.find_unsafe (Handler.edges ()) (index "a")) [];
    assert_backedges_equal
      (Handler.find_unsafe (Handler.backedges ()) (index "1"))
      [{ Target.target = index "b"; parameters = [] }]
  in
  let () =
    let (module Handler) = order () in
    let index key = Handler.find_unsafe (Handler.indices ()) !key in
    TypeOrder.disconnect_successors (module Handler) [!"b"];
    assert_equal (Handler.find_unsafe (Handler.edges ()) (index "b")) [];
    assert_backedges_equal
      (Handler.find_unsafe (Handler.backedges ()) (index "1"))
      [{ Target.target = index "a"; parameters = [] }]
  in
  let () =
    let (module Handler) = order () in
    let index key = Handler.find_unsafe (Handler.indices ()) !key in
    TypeOrder.disconnect_successors (module Handler) [!"a"; !"b"];
    assert_equal (Handler.find_unsafe (Handler.edges ()) (index "a")) [];
    assert_equal (Handler.find_unsafe (Handler.edges ()) (index "b")) [];
    assert_backedges_equal (Handler.find_unsafe (Handler.backedges ()) (index "1")) []
  in
  ()


let test_mark_escaped_as_escaped _ =
  let resolution =
    let configuration = Configuration.Analysis.create () in
    let populate source =
      let environment =
        let environment = Environment.Builder.create () in
        Test.populate
          ~configuration
          (Environment.handler environment)
          (parse source :: typeshed_stubs ());
        environment
      in
      Environment.handler environment
    in
    populate
      {|
        T = typing.TypeVar('T')
        class G_invariant(typing.Generic[T]):
          pass
      |}
    |> fun environment -> TypeCheck.resolution environment ()
  in
  let left =
    let variable = Type.variable "T" in
    let parameters =
      Type.Callable.Defined [Named { name = "a"; annotation = variable; default = true }]
    in
    Type.Callable.create ~annotation:(Type.parametric "G_invariant" [variable]) ~parameters ()
  in
  let right =
    let variable = Type.variable "T_Unconstrained" in
    Type.Callable.create ~annotation:variable ~parameters:(Type.Callable.Defined []) ()
  in
  let result =
    let handler =
      { handler = Resolution.order resolution;
        constructor = (fun _ -> None);
        attributes = (fun _ -> None);
        is_protocol = (fun _ -> false);
        any_is_bottom = false;
        protocol_assumptions = ProtocolAssumptions.empty
      }
    in
    let constraints = TypeConstraints.empty in
    solve_less_or_equal handler ~constraints ~left ~right
    |> List.filter_map ~f:(OrderedConstraints.solve ~order:handler)
  in
  match result with
  | [result] ->
      let instantiated =
        TypeConstraints.Solution.instantiate result (Type.variable "T_Unconstrained")
      in
      assert_equal
        ~printer:Type.show
        ~cmp:Type.equal
        (Type.Variable.convert_all_escaped_free_variables_to_anys instantiated)
        (Type.parametric "G_invariant" [Type.Any])
  | _ -> assert_failure "wrong number of solutions"


let () =
  "order"
  >::: [ "check_integrity" >:: test_check_integrity;
         "connect_annotations_to_top" >:: test_connect_annotations_to_top;
         "deduplicate" >:: test_deduplicate;
         "default" >:: test_default;
         "disconnect_successors" >:: test_disconnect_successors;
         "greatest_lower_bound" >:: test_greatest_lower_bound;
         "instantiate_parameters" >:: test_instantiate_parameters;
         "is_instantiated" >:: test_is_instantiated;
         "join" >:: test_join;
         "least_upper_bound" >:: test_least_upper_bound;
         "less_or_equal" >:: test_less_or_equal;
         "less_or_equal_variance" >:: test_less_or_equal_variance;
         "is_compatible_with" >:: test_is_compatible_with;
         "meet" >:: test_meet;
         "method_resolution_order_linearize" >:: test_method_resolution_order_linearize;
         "remove_extra_edges" >:: test_remove_extra_edges;
         "sort_bottom_edges" >:: test_sort_bottom_edges;
         "successors" >:: test_successors;
         "to_dot" >:: test_to_dot;
         "variables" >:: test_variables;
         "solve_less_or_equal" >:: test_solve_less_or_equal;
         "is_consistent_with" >:: test_is_consistent_with;
         "instantiate_protocol_parameters" >:: test_instantiate_protocol_parameters;
         "marks_escaped_as_escaped" >:: test_mark_escaped_as_escaped ]
  |> Test.run
