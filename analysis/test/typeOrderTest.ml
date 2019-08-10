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

let ( ! ) concretes = Type.OrderedTypes.Concrete concretes

let environment ?source context =
  let _, _, environment =
    let sources = Option.value_map source ~f:(fun source -> ["__init__.py", source]) ~default:[] in
    ScratchProject.setup ~context sources |> ScratchProject.build_environment
  in
  environment


let resolution ?source context =
  let environment = environment ?source context in
  Environment.resolution environment ()


let concrete_connect ?parameters =
  let parameters = parameters >>| fun parameters -> Type.OrderedTypes.Concrete parameters in
  MockClassHierarchyHandler.connect ?parameters


let parse_attributes ~parse_annotation ~class_name =
  let parse_attribute (name, annotation) =
    {
      Attribute.annotation = Annotation.create (parse_annotation annotation);
      async = false;
      class_attribute = false;
      defined = true;
      final = false;
      initialized = true;
      name;
      parent = Type.Primitive class_name;
      property = None;
      static = false;
      value = Ast.Node.create_with_default_location Ast.Expression.Ellipsis;
    }
    |> Ast.Node.create_with_default_location
  in
  List.map ~f:parse_attribute


let less_or_equal
    ?(constructor = fun _ ~protocol_assumptions:_ -> None)
    ?(attributes = fun _ ~protocol_assumptions:_ -> None)
    ?(is_protocol = fun _ ~protocol_assumptions:_ -> false)
    handler
  =
  always_less_or_equal
    {
      handler;
      constructor;
      attributes;
      is_protocol;
      any_is_bottom = false;
      protocol_assumptions = ProtocolAssumptions.empty;
    }


let is_compatible_with ?(constructor = fun _ ~protocol_assumptions:_ -> None) handler =
  is_compatible_with
    {
      handler;
      constructor;
      attributes = (fun _ ~protocol_assumptions:_ -> None);
      is_protocol = (fun _ ~protocol_assumptions:_ -> false);
      any_is_bottom = false;
      protocol_assumptions = ProtocolAssumptions.empty;
    }


let join ?(constructor = fun _ ~protocol_assumptions:_ -> None) handler =
  join
    {
      handler;
      constructor;
      attributes = (fun _ ~protocol_assumptions:_ -> None);
      is_protocol = (fun _ ~protocol_assumptions:_ -> false);
      any_is_bottom = false;
      protocol_assumptions = ProtocolAssumptions.empty;
    }


let meet ?(constructor = fun _ ~protocol_assumptions:_ -> None) handler =
  meet
    {
      handler;
      constructor;
      attributes = (fun _ ~protocol_assumptions:_ -> None);
      is_protocol = (fun _ ~protocol_assumptions:_ -> false);
      any_is_bottom = false;
      protocol_assumptions = ProtocolAssumptions.empty;
    }


(*          0 - 3
 *          |   |   \
 *          BOTTOM  - b - 1      TOP
 *          |  \       /
 *          4 -- 2 --- *)
let order =
  let bottom = "bottom" in
  let order = MockClassHierarchyHandler.create () in
  let open MockClassHierarchyHandler in
  insert order bottom;
  insert order "0";
  insert order "1";
  insert order "2";
  insert order "3";
  insert order "4";
  insert order "5";
  connect order ~predecessor:"0" ~successor:"3";
  connect order ~predecessor:"1" ~successor:"3";
  connect order ~predecessor:"4" ~successor:"2";
  connect order ~predecessor:bottom ~successor:"0";
  connect order ~predecessor:bottom ~successor:"1";
  connect order ~predecessor:bottom ~successor:"2";
  connect order ~predecessor:bottom ~successor:"4";
  handler order


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
let disconnected_order =
  let order = MockClassHierarchyHandler.create () in
  let open MockClassHierarchyHandler in
  insert order "A";
  insert order "B";
  handler order


let variance_order =
  let order = MockClassHierarchyHandler.create () in
  let open MockClassHierarchyHandler in
  insert order "object";
  insert order "bool";
  insert order "str";
  insert order "int";
  insert order "float";
  connect order ~predecessor:"int" ~successor:"float";
  insert order "typing.Generic";

  (* Variance examples borrowed from https://www.python.org/dev/peps/pep-0483 *)
  let variable_t = Type.variable "_T" in
  let variable_t_2 = Type.variable "_T_2" in
  let variable_t_co = Type.variable "_T_co" ~variance:Covariant in
  let variable_t_contra = Type.variable "_T_contra" ~variance:Contravariant in
  insert order "LinkedList";
  insert order "Map";
  insert order "Box";
  insert order "Sink";
  concrete_connect
    order
    ~predecessor:"LinkedList"
    ~successor:"typing.Generic"
    ~parameters:[variable_t];
  concrete_connect
    order
    ~predecessor:"Map"
    ~successor:"typing.Generic"
    ~parameters:[variable_t; variable_t_2];
  concrete_connect order ~predecessor:"Box" ~successor:"typing.Generic" ~parameters:[variable_t_co];
  concrete_connect
    order
    ~predecessor:"Sink"
    ~successor:"typing.Generic"
    ~parameters:[variable_t_contra];
  insert order "Base";
  insert order "Derived";
  concrete_connect
    order
    ~predecessor:"Base"
    ~successor:"typing.Generic"
    ~parameters:[variable_t_contra];
  concrete_connect order ~predecessor:"Derived" ~successor:"Base" ~parameters:[variable_t_co];
  concrete_connect
    order
    ~predecessor:"Derived"
    ~successor:"typing.Generic"
    ~parameters:[variable_t_co];
  handler order


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
  let order = MockClassHierarchyHandler.create () in
  let open MockClassHierarchyHandler in
  insert order "str";
  insert order "int";
  insert order "float";
  insert order "object";
  insert order "bool";
  connect order ~predecessor:"int" ~successor:"float";
  insert order "typing.Generic";
  let variable_t_co = Type.variable "_T_co" ~variance:Covariant in
  let variable_t_contra = Type.variable "_T_contra" ~variance:Contravariant in
  insert order "A";
  insert order "B";
  insert order "C";
  insert order "D";
  concrete_connect
    order
    ~predecessor:"A"
    ~successor:"typing.Generic"
    ~parameters:[variable_t_co; variable_t_contra];
  concrete_connect
    order
    ~predecessor:"B"
    ~successor:"A"
    ~parameters:[variable_t_contra; variable_t_co];
  concrete_connect
    order
    ~predecessor:"B"
    ~successor:"typing.Generic"
    ~parameters:[variable_t_contra; variable_t_co];
  concrete_connect order ~predecessor:"C" ~successor:"B" ~parameters:[Type.integer; Type.integer];
  concrete_connect order ~predecessor:"D" ~successor:"B" ~parameters:[Type.float; Type.float];
  handler order


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
  let order = MockClassHierarchyHandler.create () in
  let open MockClassHierarchyHandler in
  insert order "str";
  insert order "int";
  insert order "float";
  insert order "object";
  connect order ~predecessor:"int" ~successor:"float";
  insert order "typing.Generic";
  let variable_t_co = Type.variable "_T_co" ~variance:Covariant in
  let variable_t_contra = Type.variable "_T_contra" ~variance:Contravariant in
  insert order "A";
  insert order "B";
  insert order "C";
  insert order "D";
  concrete_connect
    order
    ~predecessor:"A"
    ~successor:"typing.Generic"
    ~parameters:[variable_t_co; variable_t_contra];
  concrete_connect
    order
    ~predecessor:"B"
    ~successor:"A"
    ~parameters:[variable_t_co; variable_t_contra];
  concrete_connect
    order
    ~predecessor:"B"
    ~successor:"typing.Generic"
    ~parameters:[variable_t_co; variable_t_contra];
  concrete_connect order ~predecessor:"C" ~successor:"B" ~parameters:[Type.integer; Type.integer];
  concrete_connect order ~predecessor:"D" ~successor:"B" ~parameters:[Type.float; Type.float];
  handler order


let default =
  let order = MockClassHierarchyHandler.create () in
  let open MockClassHierarchyHandler in
  insert order "typing.Generic";
  insert order "int";
  insert order "str";
  insert order "bool";
  insert order "float";
  insert order "object";
  connect order ~predecessor:"int" ~successor:"float";
  connect order ~predecessor:"float" ~successor:"object";
  let type_builtin = "type" in
  let type_variable = Type.Variable (Type.Variable.Unary.create "_T") in
  insert order type_builtin;
  connect
    order
    ~predecessor:type_builtin
    ~parameters:(Concrete [type_variable])
    ~successor:"typing.Generic";
  let typed_dictionary = "TypedDictionary" in
  let non_total_typed_dictionary = "NonTotalTypedDictionary" in
  let typing_mapping = "typing.Mapping" in
  insert order non_total_typed_dictionary;
  insert order typed_dictionary;
  insert order typing_mapping;
  connect order ~predecessor:non_total_typed_dictionary ~successor:typed_dictionary;
  connect
    order
    ~predecessor:typed_dictionary
    ~parameters:(Concrete [Type.string; Type.Any])
    ~successor:typing_mapping;
  let variable = Type.variable "_T" in
  let other_variable = Type.variable "_T2" in
  let variable_covariant = Type.variable "_T_co" ~variance:Covariant in
  insert order "typing.Sequence";
  concrete_connect
    order
    ~predecessor:"typing.Sequence"
    ~successor:"typing.Generic"
    ~parameters:[variable];
  insert order "list";
  insert order "typing.Sized";
  connect order ~predecessor:"list" ~successor:"typing.Sized";
  concrete_connect order ~predecessor:"list" ~successor:"typing.Generic" ~parameters:[variable];

  concrete_connect order ~predecessor:"list" ~successor:"typing.Sequence" ~parameters:[variable];
  insert order "typing.AbstractSet";
  insert order "set";
  connect order ~predecessor:"set" ~successor:"typing.Sized";
  concrete_connect order ~predecessor:"set" ~successor:"typing.Generic" ~parameters:[variable];
  concrete_connect
    order
    ~predecessor:"typing.AbstractSet"
    ~successor:"typing.Generic"
    ~parameters:[variable];
  concrete_connect order ~predecessor:"set" ~successor:"typing.AbstractSet" ~parameters:[variable];
  insert order "typing.Iterator";
  concrete_connect order ~predecessor:"list" ~successor:"typing.Iterator" ~parameters:[variable];
  concrete_connect
    order
    ~predecessor:"typing.Iterator"
    ~successor:"typing.Generic"
    ~parameters:[variable_covariant];
  insert order "typing.Iterable";
  concrete_connect
    order
    ~predecessor:"typing.Iterator"
    ~successor:"typing.Iterable"
    ~parameters:[variable_covariant];
  concrete_connect
    order
    ~predecessor:"typing.Iterable"
    ~successor:"typing.Generic"
    ~parameters:[variable_covariant];
  concrete_connect order ~predecessor:"list" ~successor:"typing.Iterable" ~parameters:[variable];
  insert order "tuple";
  concrete_connect order ~predecessor:"tuple" ~successor:"typing.Iterator" ~parameters:[variable];
  concrete_connect order ~predecessor:"tuple" ~successor:"typing.Generic" ~parameters:[variable];
  insert order "typing.Generator";
  concrete_connect
    order
    ~predecessor:"typing.Generator"
    ~successor:"typing.Iterator"
    ~parameters:[variable];
  concrete_connect
    order
    ~predecessor:"typing.Generator"
    ~successor:"typing.Generic"
    ~parameters:[variable];
  insert order "str";
  concrete_connect
    order
    ~predecessor:"str"
    ~successor:"typing.Iterable"
    ~parameters:[Type.Primitive "str"];
  insert order "AnyIterable";
  connect order ~predecessor:"AnyIterable" ~successor:"typing.Iterable";
  insert order "typing.Mapping";
  concrete_connect
    order
    ~predecessor:"typing.Mapping"
    ~successor:"typing.Generic"
    ~parameters:[variable; other_variable];
  insert order "dict";

  concrete_connect
    order
    ~predecessor:"dict"
    ~successor:"typing.Generic"
    ~parameters:[variable; other_variable];
  concrete_connect
    order
    ~predecessor:"dict"
    ~successor:"typing.Mapping"
    ~parameters:[variable; other_variable];
  concrete_connect order ~predecessor:"dict" ~successor:"typing.Iterator" ~parameters:[variable];
  insert order "collections.OrderedDict";
  concrete_connect
    order
    ~predecessor:"collections.OrderedDict"
    ~successor:"typing.Generic"
    ~parameters:[variable; other_variable];
  concrete_connect
    order
    ~predecessor:"collections.OrderedDict"
    ~successor:"dict"
    ~parameters:[variable; other_variable];
  insert order "PartiallySpecifiedDict";
  concrete_connect
    order
    ~predecessor:"PartiallySpecifiedDict"
    ~successor:"dict"
    ~parameters:[Primitive "int"];
  insert order "OverSpecifiedDict";
  concrete_connect
    order
    ~predecessor:"OverSpecifiedDict"
    ~successor:"dict"
    ~parameters:[Primitive "int"; Primitive "int"; Primitive "str"];
  insert order "GenericContainer";
  concrete_connect
    order
    ~predecessor:"GenericContainer"
    ~successor:"typing.Generic"
    ~parameters:[variable; other_variable];

  insert order "NonGenericContainerChild";
  concrete_connect
    order
    ~predecessor:"NonGenericContainerChild"
    ~successor:"GenericContainer"
    ~parameters:[Primitive "int"; Primitive "str"];

  insert order "DifferentGenericContainer";
  concrete_connect
    order
    ~predecessor:"DifferentGenericContainer"
    ~successor:"typing.Generic"
    ~parameters:[variable; other_variable];
  insert order "CommonNonGenericChild";
  concrete_connect
    order
    ~predecessor:"CommonNonGenericChild"
    ~successor:"GenericContainer"
    ~parameters:[Primitive "int"; Primitive "str"];
  concrete_connect
    order
    ~predecessor:"CommonNonGenericChild"
    ~successor:"DifferentGenericContainer"
    ~parameters:[Primitive "int"; Primitive "str"];
  handler order


let ( !! ) name = Type.Primitive name

let test_less_or_equal context =
  (* Primitive types. *)
  assert_true (less_or_equal order ~left:Type.Bottom ~right:Type.Top);
  assert_false (less_or_equal order ~left:Type.Top ~right:Type.Bottom);
  assert_true (less_or_equal order ~left:!!"0" ~right:!!"0");
  assert_true (less_or_equal order ~left:Type.Bottom ~right:!!"0");
  assert_true (less_or_equal order ~left:Type.Bottom ~right:!!"1");
  assert_true (less_or_equal order ~left:Type.Bottom ~right:!!"2");
  assert_true (less_or_equal order ~left:Type.Bottom ~right:!!"3");
  assert_false (less_or_equal order ~left:!!"3" ~right:Type.Bottom);
  assert_false (less_or_equal order ~left:!!"2" ~right:Type.Bottom);
  assert_false (less_or_equal order ~left:!!"1" ~right:Type.Bottom);
  assert_false (less_or_equal order ~left:!!"0" ~right:Type.Bottom);
  assert_true (less_or_equal order ~left:!!"0" ~right:!!"3");
  assert_true (less_or_equal order ~left:!!"1" ~right:!!"3");
  assert_false (less_or_equal order ~left:!!"2" ~right:!!"3");
  assert_true (less_or_equal default ~left:!!"list" ~right:!!"typing.Sized");
  assert_true (less_or_equal default ~left:(Type.list Type.integer) ~right:!!"typing.Sized");

  (* Annotated types. *)
  assert_true (less_or_equal default ~left:(Type.annotated Type.integer) ~right:Type.float);
  assert_true (less_or_equal default ~left:Type.integer ~right:(Type.annotated Type.float));
  assert_true
    (less_or_equal default ~left:(Type.annotated Type.integer) ~right:(Type.annotated Type.float));

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
       ~right:(Type.parametric "tuple" ![Type.integer]));

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
    Type.Variable.Variadic.List.create "Ts"
    |> Type.Variable.Variadic.List.mark_as_bound
    |> Type.Variable.Variadic.List.self_reference
  in
  assert_false
    (less_or_equal
       default
       ~left:(Type.Tuple (Bounded list_variadic))
       ~right:(Type.Tuple (Type.Unbounded Type.integer)));
  assert_true
    (less_or_equal
       default
       ~left:(Type.Tuple (Bounded list_variadic))
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
    let order = MockClassHierarchyHandler.create () in
    let open MockClassHierarchyHandler in
    insert order "object";

    insert order "str";
    insert order "int";
    insert order "float";
    concrete_connect order ~predecessor:"int" ~successor:"float";
    insert order "tuple";
    insert order "A";
    insert order "B";
    insert order "C";
    insert order "typing.Generic";
    insert order "FloatToStrCallable";
    insert order "ParametricCallableToStr";
    insert order "typing.Callable";
    concrete_connect
      order
      ~predecessor:"A"
      ~successor:"typing.Generic"
      ~parameters:[Type.variable "_1"; Type.variable "_2"];
    concrete_connect
      order
      ~predecessor:"B"
      ~successor:"typing.Generic"
      ~parameters:[Type.variable "_T"];
    concrete_connect
      order
      ~predecessor:"C"
      ~successor:"typing.Generic"
      ~parameters:[Type.variable "_T"];

    concrete_connect
      order
      ~predecessor:"A"
      ~successor:"B"
      ~parameters:[Type.tuple [Type.variable "_1"; Type.variable "_2"]];
    concrete_connect
      order
      ~predecessor:"B"
      ~successor:"C"
      ~parameters:[Type.union [Type.variable "_T"; Type.float]];

    concrete_connect
      order
      ~parameters:[parse_callable "typing.Callable[[float], str]"]
      ~predecessor:"FloatToStrCallable"
      ~successor:"typing.Callable";
    let callable =
      let aliases = function
        | "_T" -> Some (Type.variable "_T")
        | _ -> None
      in
      let aliases = create_type_alias_table aliases in
      parse_callable ~aliases "typing.Callable[[_T], str]"
    in
    concrete_connect
      order
      ~parameters:[callable]
      ~predecessor:"ParametricCallableToStr"
      ~successor:"typing.Callable";
    concrete_connect
      order
      ~parameters:[Type.variable "_T"]
      ~predecessor:"ParametricCallableToStr"
      ~successor:"typing.Generic";
    let typed_dictionary = "TypedDictionary" in
    let non_total_typed_dictionary = "NonTotalTypedDictionary" in
    let typing_mapping = "typing.Mapping" in
    insert order typed_dictionary;
    insert order non_total_typed_dictionary;
    insert order typing_mapping;
    concrete_connect order ~predecessor:non_total_typed_dictionary ~successor:typed_dictionary;
    concrete_connect
      order
      ~predecessor:typed_dictionary
      ~parameters:[Type.string; Type.Any]
      ~successor:typing_mapping;
    concrete_connect
      order
      ~parameters:[Type.variable "_T"; Type.variable "_T2"]
      ~predecessor:typing_mapping
      ~successor:"typing.Generic";
    insert order "dict";
    insert order "MatchesProtocol";
    insert order "DoesNotMatchProtocol";
    handler order
  in
  assert_true
    (less_or_equal
       order
       ~left:(Type.parametric "A" ![Type.integer; Type.string])
       ~right:(Type.parametric "B" ![Type.tuple [Type.integer; Type.string]]));
  assert_false
    (less_or_equal
       order
       ~left:(Type.parametric "A" ![Type.integer; Type.string])
       ~right:(Type.tuple [Type.integer; Type.string]));
  assert_true
    (less_or_equal
       order
       ~left:(Type.parametric "A" ![Type.integer; Type.string])
       ~right:
         (Type.parametric "C" ![Type.union [Type.tuple [Type.integer; Type.string]; Type.float]]));
  assert_false
    (less_or_equal
       order
       ~left:(Type.parametric "A" ![Type.string; Type.integer])
       ~right:
         (Type.parametric "C" ![Type.union [Type.tuple [Type.integer; Type.string]; Type.float]]));

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
       ~right:(Type.variable ~constraints:(Type.Variable.Explicit [Type.float; Type.integer]) "T"));
  assert_true
    (less_or_equal
       order
       ~left:(Type.variable ~constraints:(Type.Variable.Explicit [Type.float; Type.string]) "T")
       ~right:(Type.union [Type.float; Type.string]));
  assert_true
    (less_or_equal
       order
       ~left:(Type.variable ~constraints:(Type.Variable.Explicit [Type.float; Type.string]) "T")
       ~right:(Type.union [Type.float; Type.string; !!"A"]));
  assert_false
    (less_or_equal
       order
       ~left:(Type.variable ~constraints:(Type.Variable.Explicit [Type.float; Type.string]) "T")
       ~right:(Type.union [Type.float]));
  assert_true
    (less_or_equal
       order
       ~left:
         (Type.variable
            ~constraints:(Type.Variable.Bound (Type.union [Type.float; Type.string]))
            "T")
       ~right:(Type.union [Type.float; Type.string; !!"A"]));
  assert_false
    (less_or_equal
       order
       ~left:Type.string
       ~right:(Type.variable ~constraints:(Type.Variable.Bound Type.string) "T"));
  let float_string_variable =
    Type.variable ~constraints:(Type.Variable.Explicit [Type.float; Type.string]) "T"
  in
  assert_true
    (less_or_equal
       order
       ~left:float_string_variable
       ~right:(Type.union [float_string_variable; !!"A"]));
  assert_true
    (less_or_equal
       order
       ~left:(Type.variable ~constraints:(Type.Variable.Bound !!"A") "T")
       ~right:
         (Type.union [Type.variable ~constraints:(Type.Variable.Bound !!"A") "T"; Type.string]));
  assert_true
    (less_or_equal
       order
       ~left:(Type.variable ~constraints:(Type.Variable.Bound !!"A") "T")
       ~right:(Type.optional (Type.variable ~constraints:(Type.Variable.Bound !!"A") "T")));
  assert_true
    (less_or_equal
       order
       ~left:(Type.variable ~constraints:(Type.Variable.Bound (Type.optional !!"A")) "T")
       ~right:(Type.optional !!"A"));
  assert_true
    (less_or_equal
       order
       ~left:(Type.variable ~constraints:(Type.Variable.Bound Type.integer) "T")
       ~right:(Type.union [Type.float; Type.string]));
  assert_true
    (less_or_equal
       order
       ~left:(Type.variable ~constraints:(Type.Variable.Bound Type.integer) "T")
       ~right:
         (Type.union
            [Type.variable ~constraints:(Type.Variable.Bound Type.integer) "T"; Type.string]));
  assert_true
    (less_or_equal
       order
       ~left:(Type.variable ~constraints:Type.Variable.Unconstrained "T")
       ~right:Type.Top);
  assert_true
    (less_or_equal
       order
       ~left:(Type.variable ~constraints:Type.Variable.Unconstrained "T")
       ~right:
         (Type.union [Type.variable ~constraints:Type.Variable.Unconstrained "T"; Type.string]));
  assert_true
    (less_or_equal
       order
       ~left:
         (Type.variable
            ~constraints:(Type.Variable.Bound (Type.union [Type.float; Type.string]))
            "T")
       ~right:(Type.union [Type.float; Type.string]));
  assert_false
    (less_or_equal
       order
       ~left:Type.integer
       ~right:(Type.variable ~constraints:(Type.Variable.Bound Type.float) "T"));
  assert_false
    (less_or_equal
       order
       ~left:Type.float
       ~right:(Type.variable ~constraints:(Type.Variable.Bound Type.integer) "T"));
  assert_false
    (less_or_equal
       order
       ~left:(Type.union [Type.string; Type.integer])
       ~right:(Type.variable ~constraints:(Type.Variable.Explicit [Type.string; Type.integer]) "T"));
  assert_false
    (less_or_equal
       order
       ~left:Type.integer
       ~right:(Type.variable ~constraints:(Type.Variable.Explicit [Type.string]) "T"));
  assert_false
    (less_or_equal
       order
       ~left:Type.integer
       ~right:(Type.variable ~constraints:Type.Variable.LiteralIntegers "T"));
  assert_true
    (less_or_equal
       order
       ~left:(Type.variable ~constraints:Type.Variable.LiteralIntegers "T")
       ~right:Type.integer);

  (* Behavioral subtyping of callables. *)
  let less_or_equal ?attributes ?is_protocol order ~left ~right =
    let aliases = function
      | "T_Unconstrained" -> Some (Type.variable "T_Unconstrained")
      | "T_int_bool" ->
          Some
            (Type.variable
               "T_int_bool"
               ~constraints:(Type.Variable.Explicit [Type.integer; Type.bool]))
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
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable[[Named(arg, T_Unconstrained)], T_Unconstrained]"
       ~right:"typing.Callable[[Named(arg, int)], int]");
  assert_false
    (less_or_equal
       order
       ~right:"typing.Callable[[Named(arg, int)], str]"
       ~left:"typing.Callable[[T_Unconstrained], T_Unconstrained]");
  assert_false
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
  let is_protocol annotation ~protocol_assumptions:_ =
    match annotation with
    | Type.Primitive "MatchesProtocol"
    | Type.Primitive "DoesNotMatchProtocol"
    | Type.Parametric { name = "B"; _ } ->
        true
    | _ -> false
  in
  let attributes annotation ~protocol_assumptions:_ =
    match annotation with
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
    let resolution = resolution ~source context in
    let parse_annotation annotation =
      annotation |> parse_single_expression |> GlobalResolution.parse_annotation resolution
    in
    let left, right = parse_annotation left, parse_annotation right in
    assert_equal
      ~printer:(Printf.sprintf "%B")
      expected_result
      (GlobalResolution.less_or_equal resolution ~left ~right)
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
    assert_equal
      ~printer:(fun pair -> Format.sprintf "%B, %B" (fst pair) (snd pair))
      (true, false)
      (less_or_equal order ~left ~right, less_or_equal order ~left:right ~right:left)
  in
  (* Invariant. *)
  assert_false
    (less_or_equal
       variance_order
       ~left:(Type.parametric "LinkedList" ![Type.integer])
       ~right:(Type.parametric "LinkedList" ![Type.float]));
  assert_false
    (less_or_equal
       variance_order
       ~left:(Type.parametric "LinkedList" ![Type.float])
       ~right:(Type.parametric "LinkedList" ![Type.integer]));
  assert_false
    (less_or_equal
       variance_order
       ~left:(Type.parametric "LinkedList" ![Type.integer])
       ~right:(Type.parametric "LinkedList" ![Type.Any]));
  assert_false
    (less_or_equal
       variance_order
       ~left:(Type.parametric "LinkedList" ![Type.Any])
       ~right:(Type.parametric "LinkedList" ![Type.integer]));
  assert_strict_less
    ~order:variance_order
    ~left:(Type.parametric "LinkedList" ![Type.integer])
    ~right:(Type.parametric "LinkedList" ![Type.Top]);

  (* Covariant. *)
  assert_strict_less
    ~order:variance_order
    ~left:(Type.parametric "Box" ![Type.integer])
    ~right:(Type.parametric "Box" ![Type.float]);
  assert_strict_less
    ~order:variance_order
    ~left:(Type.parametric "Box" ![Type.integer])
    ~right:(Type.parametric "Box" ![Type.Any]);

  (* Contravariant. *)
  assert_strict_less
    ~order:variance_order
    ~left:(Type.parametric "Sink" ![Type.float])
    ~right:(Type.parametric "Sink" ![Type.integer]);
  assert_strict_less
    ~order:variance_order
    ~left:(Type.parametric "Sink" ![Type.Any])
    ~right:(Type.parametric "Sink" ![Type.integer]);

  (* TODO (T45909999): Revisit these tests and only keep the useful ones *)
  let _obsolete_tests () =
    (* More complex rules. *)
    assert_strict_less
      ~order:variance_order
      ~left:(Type.parametric "Derived" ![Type.integer])
      ~right:(Type.parametric "Derived" ![Type.float]);
    assert_strict_less
      ~order:variance_order
      ~left:(Type.parametric "Derived" ![Type.integer])
      ~right:(Type.parametric "Base" ![Type.integer]);
    assert_strict_less
      ~order:variance_order
      ~left:(Type.parametric "Derived" ![Type.float])
      ~right:(Type.parametric "Base" ![Type.float]);
    assert_strict_less
      ~order:variance_order
      ~left:(Type.parametric "Base" ![Type.float])
      ~right:(Type.parametric "Base" ![Type.integer]);
    assert_strict_less
      ~order:variance_order
      ~left:(Type.parametric "Derived" ![Type.integer])
      ~right:(Type.parametric "Base" ![Type.float]);
    assert_strict_less
      ~order:variance_order
      ~left:(Type.parametric "Derived" ![Type.float])
      ~right:(Type.parametric "Base" ![Type.integer]);

    (* Multiplane variance. *)
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "A" ![Type.integer; Type.float])
      ~right:(Type.parametric "A" ![Type.float; Type.integer]);
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "B" ![Type.float; Type.integer])
      ~right:(Type.parametric "B" ![Type.integer; Type.float]);
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "B" ![Type.integer; Type.integer])
      ~right:(Type.parametric "A" ![Type.integer; Type.integer]);
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "B" ![Type.integer; Type.integer])
      ~right:(Type.parametric "A" ![Type.integer; Type.float]);
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "B" ![Type.integer; Type.integer])
      ~right:(Type.parametric "A" ![Type.float; Type.float]);
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "B" ![Type.float; Type.float])
      ~right:(Type.parametric "A" ![Type.integer; Type.integer]);
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "B" ![Type.float; Type.float])
      ~right:(Type.parametric "A" ![Type.integer; Type.float]);
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "B" ![Type.float; Type.float])
      ~right:(Type.parametric "A" ![Type.float; Type.float]);
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "B" ![Type.float; Type.integer])
      ~right:(Type.parametric "A" ![Type.integer; Type.integer]);
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "B" ![Type.float; Type.integer])
      ~right:(Type.parametric "A" ![Type.integer; Type.float]);
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "B" ![Type.float; Type.integer])
      ~right:(Type.parametric "A" ![Type.float; Type.float]);
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "B" ![Type.float; Type.integer])
      ~right:(Type.parametric "A" ![Type.float; Type.integer]);
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "C" ![])
      ~right:(Type.parametric "A" ![Type.float; Type.float]);
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:!!"C"
      ~right:(Type.parametric "A" ![Type.float; Type.float]);
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "D" ![])
      ~right:(Type.parametric "A" ![Type.integer; Type.integer]);
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:!!"D"
      ~right:(Type.parametric "A" ![Type.integer; Type.integer]);
    assert_false
      (less_or_equal
         parallel_planes_variance_order
         ~left:(Type.parametric "C" ![])
         ~right:(Type.parametric "A" ![Type.float; Type.float]));
    assert_false
      (less_or_equal
         parallel_planes_variance_order
         ~left:!!"C"
         ~right:(Type.parametric "A" ![Type.float; Type.float]));
    assert_false
      (less_or_equal
         parallel_planes_variance_order
         ~left:(Type.parametric "D" ![])
         ~right:(Type.parametric "A" ![Type.integer; Type.integer]));
    assert_false
      (less_or_equal
         parallel_planes_variance_order
         ~left:!!"D"
         ~right:(Type.parametric "A" ![Type.integer; Type.integer]))
  in
  ()


let test_join context =
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

  (* Annotated types. *)
  assert_join "typing.Annotated[int]" "float" "typing.Annotated[float]";
  assert_join "typing.Annotated[int]" "typing.Annotated[float]" "typing.Annotated[float]";

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
  assert_join_types ~order !!"0" Type.undeclared (Type.Union [!!"0"; Type.undeclared]);
  assert_join_types ~order Type.undeclared !!"0" (Type.Union [!!"0"; Type.undeclared]);
  assert_join
    "typing.Tuple[int, int]"
    "typing.Tuple[int, int, str]"
    "typing.Union[typing.Tuple[int, int], typing.Tuple[int, int, str]]";
  let order =
    let order = MockClassHierarchyHandler.create () in
    let open MockClassHierarchyHandler in
    insert order "object";

    insert order "str";
    insert order "int";
    insert order "float";
    insert order "A";
    insert order "B";
    insert order "C";
    insert order "CallableClass";
    insert order "ParametricCallableToStr";
    insert order "typing.Callable";
    insert order "typing.Generic";
    concrete_connect order ~predecessor:"int" ~successor:"float";
    concrete_connect order ~predecessor:"float" ~successor:"object";
    concrete_connect
      order
      ~predecessor:"A"
      ~successor:"B"
      ~parameters:[Type.tuple [Type.variable "_1"; Type.variable "_2"]];
    concrete_connect
      order
      ~predecessor:"A"
      ~successor:"typing.Generic"
      ~parameters:[Type.variable "_1"; Type.variable "_2"];
    concrete_connect
      order
      ~predecessor:"B"
      ~successor:"typing.Generic"
      ~parameters:[Type.variable "_T"];
    concrete_connect
      order
      ~predecessor:"B"
      ~successor:"C"
      ~parameters:[Type.union [Type.variable "_T"; Type.float]];
    concrete_connect
      order
      ~predecessor:"C"
      ~successor:"typing.Generic"
      ~parameters:[Type.variable "_T"];

    concrete_connect
      order
      ~parameters:[parse_callable "typing.Callable[[int], str]"]
      ~predecessor:"CallableClass"
      ~successor:"typing.Callable";
    let callable =
      let aliases = function
        | "_T" -> Some (Type.variable "_T")
        | _ -> None
      in
      let aliases = create_type_alias_table aliases in
      parse_callable ~aliases "typing.Callable[[_T], str]"
    in
    concrete_connect
      order
      ~parameters:[callable]
      ~predecessor:"ParametricCallableToStr"
      ~successor:"typing.Callable";
    concrete_connect
      order
      ~parameters:[Type.variable "_T"]
      ~predecessor:"ParametricCallableToStr"
      ~successor:"typing.Generic";
    handler order
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
    (join order Type.integer (Type.variable ~constraints:(Type.Variable.Bound Type.string) "T"))
    (Type.union [Type.integer; Type.variable ~constraints:(Type.Variable.Bound Type.string) "T"]);
  assert_type_equal
    (join
       order
       Type.string
       (Type.variable ~constraints:(Type.Variable.Explicit [Type.float; Type.integer]) "T"))
    (Type.union
       [ Type.string;
         Type.variable ~constraints:(Type.Variable.Explicit [Type.float; Type.integer]) "T" ]);
  assert_type_equal
    (join order Type.string (Type.variable ~constraints:Type.Variable.LiteralIntegers "T"))
    (Type.union [Type.string; Type.variable ~constraints:Type.Variable.LiteralIntegers "T"]);
  assert_type_equal
    (join
       order
       (Type.literal_integer 7)
       (Type.variable ~constraints:Type.Variable.LiteralIntegers "T"))
    (Type.union
       [Type.literal_integer 7; Type.variable ~constraints:Type.Variable.LiteralIntegers "T"]);

  (* Variance. *)
  assert_type_equal
    (join
       variance_order
       (Type.parametric "LinkedList" ![Type.integer])
       (Type.parametric "LinkedList" ![Type.Top]))
    (Type.parametric "LinkedList" ![Type.Top]);
  assert_type_equal
    (join
       variance_order
       (Type.parametric "LinkedList" ![Type.Top])
       (Type.parametric "LinkedList" ![Type.integer]))
    (Type.parametric "LinkedList" ![Type.Top]);
  assert_type_equal
    (join
       variance_order
       (Type.parametric "LinkedList" ![Type.Bottom])
       (Type.parametric "LinkedList" ![Type.Top]))
    (Type.parametric "LinkedList" ![Type.Top]);
  assert_type_equal
    (join
       variance_order
       (Type.parametric "LinkedList" ![Type.Top])
       (Type.parametric "LinkedList" ![Type.Bottom]))
    (Type.parametric "LinkedList" ![Type.Top]);
  assert_type_equal
    (join
       variance_order
       (Type.parametric "LinkedList" ![Type.Any])
       (Type.parametric "LinkedList" ![Type.Top]))
    (Type.parametric "LinkedList" ![Type.Top]);
  assert_type_equal
    (join
       variance_order
       (Type.parametric "LinkedList" ![Type.Top])
       (Type.parametric "LinkedList" ![Type.Any]))
    (Type.parametric "LinkedList" ![Type.Top]);
  assert_type_equal
    (join
       variance_order
       (Type.parametric "LinkedList" ![Type.Top])
       (Type.parametric "LinkedList" ![Type.Top]))
    (Type.parametric "LinkedList" ![Type.Top]);
  assert_type_equal
    (join
       variance_order
       (Type.parametric "Map" ![Type.integer; Type.integer])
       (Type.parametric "Map" ![Type.Top; Type.Top]))
    (Type.parametric "Map" ![Type.Top; Type.Top]);
  assert_type_equal
    (join
       variance_order
       (Type.parametric "Map" ![Type.integer; Type.integer])
       (Type.parametric "Map" ![Type.Top; Type.integer]))
    (Type.parametric "Map" ![Type.Top; Type.integer]);
  assert_type_equal
    (join
       variance_order
       (Type.parametric "Map" ![Type.integer; Type.integer])
       (Type.parametric "Map" ![Type.Top; Type.string]))
    (Type.parametric "Map" ![Type.Top; Type.Any]);
  assert_type_equal
    (join
       variance_order
       (Type.parametric "LinkedList" ![Type.integer])
       (Type.parametric "LinkedList" ![Type.Any]))
    (Type.parametric "LinkedList" ![Type.Any]);
  assert_type_equal
    (join
       variance_order
       (Type.parametric "LinkedList" ![Type.Any])
       (Type.parametric "LinkedList" ![Type.integer]))
    (Type.parametric "LinkedList" ![Type.Any]);
  let variance_aliases =
    Identifier.Table.of_alist_exn
      [ "_T", Type.variable "_T";
        "_T_co", Type.variable "_T_co" ~variance:Covariant;
        "_T_contra", Type.variable "_T_contra" ~variance:Contravariant ]
    |> Identifier.Table.find
  in
  (* TODO (T45909999): Revisit these tests and only keep the useful ones *)
  let _obsolete_variance_tests () =
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
      "A[float, int]"
  in
  (* Literals *)
  assert_type_equal
    (join order (Type.literal_string "A") (Type.literal_string "A"))
    (Type.literal_string "A");
  assert_type_equal (join order (Type.literal_string "A") (Type.literal_string "B")) Type.string;
  assert_type_equal
    (join order (Type.literal_string "A") Type.integer)
    (Type.union [Type.string; Type.integer]);
  let assert_join ?(source = "") ~left ~right expected_result =
    let resolution = resolution ~source context in
    let parse_annotation annotation =
      annotation |> parse_single_expression |> GlobalResolution.parse_annotation resolution
    in
    let left, right = parse_annotation left, parse_annotation right in
    assert_type_equal
      (parse_annotation expected_result)
      (GlobalResolution.join resolution left right)
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

  (* Annotated types. *)
  assert_meet "typing.Annotated[int]" "float" "typing.Annotated[int]";
  assert_meet "typing.Annotated[int]" "typing.Annotated[float]" "typing.Annotated[int]";

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
  assert_meet
    "GenericContainer[int, str]"
    "DifferentGenericContainer[int, str]"
    "CommonNonGenericChild";
  assert_meet "GenericContainer[int, str]" "DifferentGenericContainer[str, int]" "$bottom";

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
    (meet default Type.integer (Type.variable ~constraints:(Type.Variable.Bound Type.float) "T"))
    Type.Bottom;
  assert_type_equal
    (meet
       default
       Type.string
       (Type.variable ~constraints:(Type.Variable.Explicit [Type.float; Type.string]) "T"))
    Type.Bottom;

  (* Undeclared. *)
  assert_type_equal (meet default Type.undeclared Type.Bottom) Type.Bottom;
  assert_type_equal (meet default Type.Bottom Type.undeclared) Type.Bottom;

  (* Variance. *)
  assert_type_equal
    (meet
       variance_order
       (Type.parametric "LinkedList" ![Type.integer])
       (Type.parametric "LinkedList" ![Type.Top]))
    (Type.parametric "LinkedList" ![Type.integer]);
  assert_type_equal
    (meet
       variance_order
       (Type.parametric "LinkedList" ![Type.Top])
       (Type.parametric "LinkedList" ![Type.integer]))
    (Type.parametric "LinkedList" ![Type.integer]);
  assert_type_equal
    (meet
       variance_order
       (Type.parametric "LinkedList" ![Type.integer])
       (Type.parametric "LinkedList" ![Type.Any]))
    (Type.parametric "LinkedList" ![Type.Bottom]);
  assert_type_equal
    (meet
       variance_order
       (Type.parametric "LinkedList" ![Type.Any])
       (Type.parametric "LinkedList" ![Type.integer]))
    (Type.parametric "LinkedList" ![Type.Bottom]);

  (* TODO (T45909999): Revisit these tests and only keep the useful ones *)
  let _obsolete_tests () =
    assert_meet ~order:variance_order "Derived[int]" "Base[int]" "Derived[int]";
    assert_meet ~order:variance_order "Derived[float]" "Base[float]" "Derived[float]";
    assert_meet ~order:variance_order "Derived[int]" "Base[float]" "Derived[int]";
    assert_meet ~order:variance_order "Derived[float]" "Base[int]" "Derived[float]";
    assert_meet ~order:multiplane_variance_order "B[int, float]" "A[int, float]" "B[int, float]";
    assert_meet ~order:multiplane_variance_order "B[int, int]" "A[int, float]" "B[int, int]";
    assert_meet ~order:multiplane_variance_order "B[float, int]" "A[int, float]" "B[float, int]";
    assert_meet
      ~order:multiplane_variance_order
      "B[float, float]"
      "A[int, float]"
      "B[float, float]";
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
    assert_meet
      ~order:parallel_planes_variance_order
      "B[float, float]"
      "A[int, int]"
      "B[int, float]"
  in
  let make_potentially_inconsistent_order ~x_before_y =
    (* Corresponds to
     *  T = typing.TypeVar("T")
     * T1 = typing.TypeVar("T1")
     * class B(typing.Generic[T, T1]): pass
     * class A(typing.Generic[T]): pass
     * class X(B[T, str]): pass
     * class Y(B[int, str]): pass
     * class M(A[T], X[T], Y[T]): pass *)
    let order = MockClassHierarchyHandler.create () in
    let open MockClassHierarchyHandler in
    insert order "A";
    insert order "B";
    insert order "X";
    insert order "Y";
    insert order "M";
    insert order "typing.Generic";
    insert order "str";
    insert order "int";
    let variable = Type.Variable (Type.Variable.Unary.create "T") in
    let variable2 = Type.Variable (Type.Variable.Unary.create "T2") in
    concrete_connect order ~predecessor:"M" ~successor:"typing.Generic" ~parameters:[variable];
    concrete_connect order ~predecessor:"M" ~successor:"A" ~parameters:[variable];
    concrete_connect order ~predecessor:"M" ~successor:"X" ~parameters:[variable];
    concrete_connect order ~predecessor:"M" ~successor:"Y" ~parameters:[variable];
    concrete_connect order ~predecessor:"A" ~successor:"typing.Generic" ~parameters:[variable];
    let connect_x () =
      concrete_connect order ~predecessor:"X" ~successor:"typing.Generic" ~parameters:[variable];
      concrete_connect order ~predecessor:"X" ~successor:"B" ~parameters:[variable; Type.string]
    in
    if x_before_y then connect_x ();
    concrete_connect order ~predecessor:"Y" ~successor:"typing.Generic" ~parameters:[variable];
    concrete_connect order ~predecessor:"Y" ~successor:"B" ~parameters:[Type.integer; variable];
    if not x_before_y then connect_x ();
    concrete_connect
      order
      ~predecessor:"B"
      ~successor:"typing.Generic"
      ~parameters:[variable; variable2];
    handler order
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


let test_solve_less_or_equal context =
  let environment =
    environment
      ~source:
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

      class UserDefinedVariadic(typing.Generic[Ts]):
        pass

      class UserDefinedVariadicSimpleChild(UserDefinedVariadic[Ts]):
        pass

      class UserDefinedVariadicMapChild(UserDefinedVariadic[pyre_extensions.type_variable_operators.Map[typing.List, Ts]]):
        pass
    |}
      context
  in
  let resolution = Environment.resolution environment () in
  let default_postprocess annotation = Type.Variable.mark_all_variables_as_bound annotation in
  let assert_solve
      ~left
      ~right
      ?(is_protocol = fun _ ~protocol_assumptions:_ -> false)
      ?(attributes = fun _ ~protocol_assumptions:_ -> None)
      ?constraints
      ?(leave_unbound_in_left = [])
      ?(postprocess = default_postprocess)
      expected
    =
    let handler =
      let constructor instantiated ~protocol_assumptions:_ =
        GlobalResolution.class_definition resolution instantiated
        >>| Class.create
        >>| Class.constructor ~instantiated ~resolution
      in
      let handler = Environment.resolution environment () |> GlobalResolution.class_hierarchy in
      {
        handler;
        constructor;
        attributes;
        is_protocol;
        any_is_bottom = false;
        protocol_assumptions = ProtocolAssumptions.empty;
      }
    in
    let parse_annotation annotation =
      annotation
      |> parse_single_expression
      |> GlobalResolution.parse_annotation
           ~allow_untracked:true
           ~allow_invalid_type_parameters:true
           resolution
    in
    let left =
      let mark_unary ({ Type.Variable.Unary.variable = name; _ } as variable) =
        if List.mem leave_unbound_in_left name ~equal:Identifier.equal then
          None
        else
          Some (Type.Variable (Type.Variable.Unary.mark_as_bound variable))
      in
      let mark_parameter_variadic variable =
        if
          List.mem
            leave_unbound_in_left
            (Type.Variable.Variadic.Parameters.name variable)
            ~equal:Identifier.equal
        then
          None
        else
          Some
            (Type.Callable.ParameterVariadicTypeVariable
               (Type.Variable.Variadic.Parameters.mark_as_bound variable))
      in
      let mark_list_variadic variable =
        if
          List.mem
            leave_unbound_in_left
            (Type.Variable.Variadic.List.name variable)
            ~equal:Identifier.equal
        then
          None
        else
          Some
            (Type.Variable.Variadic.List.self_reference
               (Type.Variable.Variadic.List.mark_as_bound variable))
      in
      parse_annotation left
      |> Type.Variable.GlobalTransforms.Unary.replace_all mark_unary
      |> Type.Variable.GlobalTransforms.ParameterVariadic.replace_all mark_parameter_variadic
      |> Type.Variable.GlobalTransforms.ListVariadic.replace_all mark_list_variadic
    in
    let right = parse_annotation right in
    let expected =
      let parse_pairs pairs =
        let parse_pair (variable, value) =
          match parse_annotation variable with
          | Type.Variable variable ->
              Type.Variable.UnaryPair (variable, parse_annotation value |> postprocess)
          | Type.Primitive primitive -> (
              let parse_parameters parameters =
                match
                  parse_annotation (Printf.sprintf "typing.Callable[%s, typing.Any]" parameters)
                with
                | Type.Callable { implementation = { parameters; _ }; _ } -> parameters
                | _ -> failwith "impossible"
              in
              let parse_ordered_types ordered =
                if ordered = "" then
                  Type.OrderedTypes.Concrete []
                else
                  match parse_annotation (Printf.sprintf "typing.Tuple[%s]" ordered) with
                  | Type.Tuple (Bounded ordered) -> ordered
                  | _ -> failwith "impossible"
              in
              let global_resolution = Environment.resolution environment () in
              match GlobalResolution.aliases global_resolution primitive with
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

  (* Annotated types. *)
  assert_solve ~left:"typing.Annotated[C]" ~right:"T_Unconstrained" [["T_Unconstrained", "C"]];
  assert_solve ~left:"C" ~right:"typing.Annotated[T_Unconstrained]" [["T_Unconstrained", "C"]];

  (* An explicit type variable can only be bound to its constraints *)
  assert_solve ~left:"D" ~right:"T_C_Q" [["T_C_Q", "C"]];
  assert_solve ~left:"C" ~right:"T_D_Q" [];
  assert_solve
    ~left:"typing.Union[int, G_invariant[str], str]"
    ~right:"T_Unconstrained"
    [["T_Unconstrained", "typing.Union[int, G_invariant[str], str]"]];
  assert_solve ~left:"typing.Union[D, C]" ~right:"T_Bound_C" [["T_Bound_C", "typing.Union[D, C]"]];
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
    [];
  assert_solve
    ~left:"typing.Tuple[C, Q, D]"
    ~right:"typing.Tuple[T_Unconstrained, ...]"
    [["T_Unconstrained", "typing.Union[C, D, Q]"]];
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
    annotation |> parse_single_expression |> GlobalResolution.parse_annotation resolution
  in
  let is_protocol annotation ~protocol_assumptions:_ =
    match annotation with
    | Type.Parametric { name = "G_invariant"; _ } -> true
    | _ -> false
  in
  let attributes annotation ~protocol_assumptions:_ =
    match annotation with
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
    ~leave_unbound_in_left:["Ts"]
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
  assert_solve
    ~leave_unbound_in_left:["Ts"]
    ~left:"typing.Tuple[Ts]"
    ~right:"typing.Tuple[T2s]"
    [["T2s", "Ts"]; ["Ts", "T2s"]];

  assert_solve ~left:"typing.Tuple[...]" ~right:"typing.Tuple[Ts]" [[]];
  assert_solve
    ~left:"typing.Callable[[int, str, bool], int]"
    ~right:"typing.Callable[[Ts], int]"
    [["Ts", "int, str, bool"]];

  (* This does not bind anything to Ts because the rule is that we assume that type variables in
     Callable types in the left are from the scope of that callable, while type variables in
     callable types in the right are from an outer scope. This remaining asymmetry is required in
     order to make passing generic functions into generic higher order functions work without
     marking scopes explicitly. *)
  assert_solve
    ~leave_unbound_in_left:["Ts"]
    ~left:"typing.Callable[[Ts], int]"
    ~right:"typing.Callable[[int, str, bool], int]"
    [[]];

  (* This is the situation we are supporting with the above odd behavior *)
  assert_solve
    ~leave_unbound_in_left:["Ts"]
    ~left:"typing.Callable[[Ts], typing.Tuple[Ts]]"
    ~right:"typing.Callable[[int, str, bool], typing.Tuple[int, str, bool]]"
    [[]];
  assert_solve
    ~left:"typing.Callable[[int, str, bool], int]"
    ~right:"typing.Callable[[int, str, bool, Variable(Ts)], int]"
    [["Ts", ""]];
  assert_solve
    ~left:"typing.Callable[[Named(A, int), Named(B, str)], int]"
    ~right:"typing.Callable[[Ts], int]"
    [["Ts", "int, str"]];
  assert_solve
    ~leave_unbound_in_left:["Ts"]
    ~left:"typing.Callable[[Ts], int]"
    ~right:"typing.Callable[[Named(A, int), Named(B, str)], int]"
    [];

  (* Map operator *)
  assert_solve
    ~left:"typing.Tuple[typing.List[int], typing.List[str], typing.List[bool]]"
    ~right:"typing.Tuple[pyre_extensions.type_variable_operators.Map[typing.List, Ts]]"
    [["Ts", "int, str, bool"]];
  assert_solve
    ~left:"typing.Tuple[typing.List[int], typing.List[str], typing.List[bool]]"
    ~right:"typing.Tuple[pyre_extensions.type_variable_operators.Map[typing.Iterable, Ts]]"
    [["Ts", "int, str, bool"]];
  assert_solve
    ~left:"typing.Tuple[typing.Iterable[int], typing.Iterable[str], typing.Iterable[bool]]"
    ~right:"typing.Tuple[pyre_extensions.type_variable_operators.Map[typing.List, Ts]]"
    [];
  assert_solve
    ~leave_unbound_in_left:["Ts"]
    ~left:"typing.Tuple[pyre_extensions.type_variable_operators.Map[typing.List, Ts]]"
    ~right:"typing.Tuple[pyre_extensions.type_variable_operators.Map[typing.List, Ts]]"
    [[]];

  (* We are not handling comparing two different maps *)
  assert_solve
    ~left:"typing.Tuple[pyre_extensions.type_variable_operators.Map[typing.List, Ts]]"
    ~right:"typing.Tuple[pyre_extensions.type_variable_operators.Map[typing.List, Ts]]"
    [];
  assert_solve
    ~leave_unbound_in_left:["Ts"]
    ~left:"typing.Tuple[pyre_extensions.type_variable_operators.Map[typing.List, Ts]]"
    ~right:"typing.Tuple[typing.Iterable[int], typing.Iterable[str], typing.Iterable[bool]]"
    [["Ts", "int, str, bool"]];
  assert_solve
    ~leave_unbound_in_left:["Ts"]
    ~left:"typing.Tuple[pyre_extensions.type_variable_operators.Map[typing.List, Ts]]"
    ~right:"typing.Tuple[typing.Iterable[int], typing.Iterable[str], bool]"
    [];
  assert_solve
    ~left:"typing.Tuple[pyre_extensions.type_variable_operators.Map[typing.List, Ts]]"
    ~right:"typing.Tuple[typing.List[object], typing.List[object]]"
    [];
  assert_solve
    ~left:"typing.Tuple[pyre_extensions.type_variable_operators.Map[typing.List, Ts]]"
    ~right:"typing.Iterable[object]"
    [[]];
  assert_solve
    ~left:
      "typing.Tuple[typing.Tuple[typing.List[int], typing.List[str]], \
       typing.Tuple[typing.List[int], typing.List[str]]]"
    ~right:
      "typing.Tuple[typing.Tuple[pyre_extensions.type_variable_operators.Map[typing.List, Ts]], \
       typing.Tuple[pyre_extensions.type_variable_operators.Map[typing.List, Ts]]]"
    [["Ts", "int, str"]];
  assert_solve
    ~left:
      "typing.Tuple[typing.Tuple[typing.List[int], typing.List[str]], \
       typing.Tuple[typing.List[float], typing.List[str]]]"
    ~right:
      "typing.Tuple[typing.Tuple[pyre_extensions.type_variable_operators.Map[typing.List, Ts]], \
       typing.Tuple[pyre_extensions.type_variable_operators.Map[typing.List, Ts]]]"
    [];

  (* We currently assume all mappers are invariant *)
  assert_solve
    ~left:
      "typing.Tuple[typing.Tuple[typing.Iterable[int], typing.Iterable[str]], \
       typing.Tuple[typing.Iterable[float], typing.Iterable[str]]]"
    ~right:
      "typing.Tuple[typing.Tuple[pyre_extensions.type_variable_operators.Map[typing.Iterable, \
       Ts]], typing.Tuple[pyre_extensions.type_variable_operators.Map[typing.Iterable, Ts]]]"
    [];
  assert_solve
    ~left:"typing.Callable[[typing.List[int], typing.List[str], typing.List[bool]], int]"
    ~right:
      "typing.Callable[[Variable(pyre_extensions.type_variable_operators.Map[typing.List, Ts])], \
       int]"
    [["Ts", "int, str, bool"]];

  assert_solve ~left:"UserDefinedVariadic[int, str]" ~right:"UserDefinedVariadic[int, str]" [[]];
  assert_solve
    ~left:"UserDefinedVariadic[int, str]"
    ~right:"UserDefinedVariadic[int, T]"
    [["T", "str"]];
  assert_solve
    ~left:"UserDefinedVariadic[int, str]"
    ~right:"UserDefinedVariadic[int, str, bool]"
    [];

  (* All variadics are invariant for now *)
  assert_solve ~left:"UserDefinedVariadic[int, str]" ~right:"UserDefinedVariadic[float, str]" [];
  assert_solve ~left:"UserDefinedVariadic[...]" ~right:"UserDefinedVariadic[int, str]" [[]];
  assert_solve
    ~left:"UserDefinedVariadicSimpleChild[int, str]"
    ~right:"UserDefinedVariadic[int, str]"
    [[]];
  assert_solve
    ~left:"UserDefinedVariadicSimpleChild[int, str]"
    ~right:"UserDefinedVariadic[int, T]"
    [["T", "str"]];
  assert_solve
    ~left:"UserDefinedVariadicMapChild[int, str]"
    ~right:"UserDefinedVariadic[typing.List[int], typing.List[str]]"
    [[]];
  assert_solve
    ~left:"UserDefinedVariadicMapChild[int, str]"
    ~right:"UserDefinedVariadic[T, typing.List[str]]"
    [["T", "typing.List[int]"]];
  assert_solve
    ~left:"UserDefinedVariadicMapChild[int, str]"
    ~right:"UserDefinedVariadic[Ts]"
    [["Ts", "typing.List[int], typing.List[str]"]];
  assert_solve
    ~left:"UserDefinedVariadicMapChild[int, str]"
    ~right:"UserDefinedVariadic[pyre_extensions.type_variable_operators.Map[typing.List, Ts]]"
    [["Ts", "int, str"]];
  assert_solve
    ~left:"typing.Tuple[int, str, float, bool]"
    ~right:"typing.Tuple[pyre_extensions.type_variable_operators.Concatenate[int, Ts, bool]]"
    [["Ts", "str, float"]];
  assert_solve
    ~left:"typing.Tuple[str, int, bool, float]"
    ~right:"typing.Tuple[pyre_extensions.type_variable_operators.Concatenate[int, Ts, bool]]"
    [];
  assert_solve
    ~left:"typing.Tuple[int, typing.List[str], typing.List[float], bool]"
    ~right:
      "typing.Tuple[pyre_extensions.type_variable_operators.Concatenate[int, \
       pyre_extensions.type_variable_operators.Map[list, Ts], bool]]"
    [["Ts", "str, float"]];
  ()


let test_is_consistent_with _ =
  let is_consistent_with =
    let order =
      {
        handler = default;
        constructor = (fun _ ~protocol_assumptions:_ -> None);
        attributes = (fun _ ~protocol_assumptions:_ -> None);
        is_protocol = (fun _ ~protocol_assumptions:_ -> false);
        any_is_bottom = false;
        protocol_assumptions = ProtocolAssumptions.empty;
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
       (Type.parametric "typing.Mapping" ![Type.integer; Type.bool]));
  assert_false
    (is_consistent_with
       (Type.dictionary ~key:Type.Any ~value:Type.bool)
       (Type.parametric "collections.OrderedDict" ![Type.integer; Type.bool]));
  assert_false
    (is_consistent_with
       (Type.dictionary ~key:Type.integer ~value:Type.bool)
       (Type.parametric "collections.OrderedDict" ![Type.Any; Type.bool]));
  assert_true
    (is_consistent_with
       (Type.parametric "collections.OrderedDict" ![Type.integer; Type.bool])
       (Type.dictionary ~key:Type.Any ~value:Type.bool));
  assert_true
    (is_consistent_with
       (Type.parametric "collections.OrderedDict" ![Type.Any; Type.bool])
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
       (Type.parametric "typing.AbstractSet" ![Type.object_primitive])
       (Type.set Type.Any));
  assert_true
    (is_consistent_with
       (Type.set Type.Any)
       (Type.parametric "typing.AbstractSet" ![Type.object_primitive]));
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


let test_instantiate_protocol_parameters context =
  let assert_instantiate_protocol_parameters
      ?source
      ~classes
      ~protocols
      ~candidate
      ~protocol
      expected
    =
    let environment = environment ?source context in
    let resolution = Environment.resolution environment () in
    let parse_annotation annotation =
      annotation
      |> parse_single_expression
      |> GlobalResolution.parse_annotation
           resolution
           ~allow_untracked:true
           ~allow_invalid_type_parameters:true
    in
    let optional_ordered_types_printer optional =
      optional
      >>| Format.asprintf "%a" Type.OrderedTypes.pp_concise
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
      let attributes annotation ~protocol_assumptions:_ =
        match annotation with
        | Type.Primitive primitive ->
            List.Assoc.find (classes @ protocols) primitive ~equal:String.equal
        | _ -> None
      in
      let is_protocol annotation ~protocol_assumptions:_ =
        match Type.split annotation with
        | Type.Primitive primitive, _ -> List.Assoc.mem protocols primitive ~equal:String.equal
        | _ -> false
      in
      let handler = Environment.resolution environment () |> GlobalResolution.class_hierarchy in
      {
        handler;
        constructor = (fun _ ~protocol_assumptions:_ -> None);
        attributes;
        is_protocol;
        any_is_bottom = false;
        protocol_assumptions = ProtocolAssumptions.empty;
      }
    in
    assert_equal
      ~printer:optional_ordered_types_printer
      expected
      (instantiate_protocol_parameters order ~candidate:(parse_annotation candidate) ~protocol)
  in
  (* Simple attribute protocols *)
  assert_instantiate_protocol_parameters
    ~source:"class P(): pass"
    ~classes:["A", []]
    ~protocols:["P", []]
    ~candidate:"A"
    ~protocol:"P"
    (Some (Concrete []));
  assert_instantiate_protocol_parameters
    ~source:"class P(): pass"
    ~classes:["A", ["prop", "int"]]
    ~protocols:["P", ["prop", "int"]]
    ~candidate:"A"
    ~protocol:"P"
    (Some (Concrete []));
  assert_instantiate_protocol_parameters
    ~source:"class P(): pass"
    ~classes:["A", ["prop", "str"]]
    ~protocols:["P", ["prop", "int"]]
    ~candidate:"A"
    ~protocol:"P"
    None;
  assert_instantiate_protocol_parameters
    ~source:{|
      T1 = typing.TypeVar("T1")
      class P(typing.Generic[T1]): pass
    |}
    ~classes:["A", ["prop", "int"]]
    ~protocols:["P", ["prop", "T1"]]
    ~candidate:"A"
    ~protocol:"P"
    (Some (Concrete [Type.integer]));

  (* Simple method protocols *)
  assert_instantiate_protocol_parameters
    ~source:"class P(): pass"
    ~classes:["A", ["method", "typing.Callable[[int], str]"]]
    ~protocols:["P", ["method", "typing.Callable[[int], str]"]]
    ~candidate:"A"
    ~protocol:"P"
    (Some (Concrete []));
  assert_instantiate_protocol_parameters
    ~source:"class P(): pass"
    ~classes:["A", ["othermethod", "typing.Callable[[int], str]"]]
    ~protocols:["P", ["method", "typing.Callable[[int], str]"]]
    ~candidate:"A"
    ~protocol:"P"
    None;
  assert_instantiate_protocol_parameters
    ~source:"class P(): pass"
    ~classes:["A", ["method", "typing.Callable[[int], str]"]]
    ~protocols:
      ["P", ["method", "typing.Callable[[int], str]"; "othermethod", "typing.Callable[[int], str]"]]
    ~candidate:"A"
    ~protocol:"P"
    None;
  assert_instantiate_protocol_parameters
    ~source:{|
      T1 = typing.TypeVar("T1")
      class P(typing.Generic[T1]): pass
    |}
    ~classes:["A", ["method", "typing.Callable[[int], str]"]]
    ~protocols:["P", ["method", "typing.Callable[[int], T1]"]]
    ~candidate:"A"
    ~protocol:"P"
    (Some (Concrete [Type.string]));

  (* Primitive recursive protocol, primitive recursive candidate *)
  assert_instantiate_protocol_parameters
    ~source:"class P(): pass"
    ~classes:["A", ["prop", "A"]]
    ~protocols:["P", ["prop", "P"]]
    ~candidate:"A"
    ~protocol:"P"
    (Some (Concrete []));
  assert_instantiate_protocol_parameters
    ~source:"class P(): pass"
    ~classes:["A", ["prop", "int"]]
    ~protocols:["P", ["prop", "P"]]
    ~candidate:"A"
    ~protocol:"P"
    None;
  assert_instantiate_protocol_parameters
    ~source:{|
      T1 = typing.TypeVar("T1")
      class P(typing.Generic[T1]): pass
    |}
    ~classes:["A", ["prop", "int"; "recursive_prop", "A"]]
    ~protocols:["P", ["prop", "T1"; "recursive_prop", "P[T1]"]]
    ~candidate:"A"
    ~protocol:"P"
    (Some (Concrete [Type.integer]));

  assert_instantiate_protocol_parameters
    ~source:{|
      T1 = typing.TypeVar("T1")
      class P(typing.Generic[T1]): pass
    |}
    ~classes:["A", ["prop", "int"; "recursive_prop", "A"]]
    ~protocols:["P", ["prop", "T1"; "recursive_prop", "P[int]"]]
    ~candidate:"A"
    ~protocol:"P"
    (Some (Concrete [Type.integer]));

  (* Protocol depends on other protocol *)
  assert_instantiate_protocol_parameters
    ~source:{|
      class P1(): pass
      class P2(): pass
    |}
    ~classes:["A", ["prop", "B"]; "B", ["prop", "int"]]
    ~protocols:["P1", ["prop", "P2"]; "P2", ["prop", "int"]]
    ~candidate:"A"
    ~protocol:"P1"
    (Some (Concrete []));
  assert_instantiate_protocol_parameters
    ~source:
      {|
      Ts = pyre_extensions.ListVariadic("Ts")
      class VariadicProtocol(typing.Generic[Ts]): pass
    |}
    ~classes:["A", ["prop", "typing.Tuple[int, str]"]]
    ~protocols:["VariadicProtocol", ["prop", "typing.Tuple[Ts]"]]
    ~candidate:"A"
    ~protocol:"VariadicProtocol"
    (Some (Concrete [Type.integer; Type.string]));
  assert_instantiate_protocol_parameters
    ~source:
      {|
      Ts = pyre_extensions.ListVariadic("Ts")
      class VariadicProtocol(typing.Generic[Ts]): pass
    |}
    ~classes:["A", ["method", "typing.Callable[[int, str], bool]"]]
    ~protocols:["VariadicProtocol", ["method", "typing.Callable[[Ts], bool]"]]
    ~candidate:"A"
    ~protocol:"VariadicProtocol"
    (Some (Concrete [Type.integer; Type.string]));
  ()


let test_mark_escaped_as_escaped context =
  let environment =
    environment
      ~source:
        {|
        T = typing.TypeVar('T')
        class G_invariant(typing.Generic[T]):
          pass
      |}
      context
  in
  let left =
    let variable = Type.variable "T" in
    let parameters =
      Type.Callable.Defined [Named { name = "a"; annotation = variable; default = true }]
    in
    Type.Callable.create ~annotation:(Type.parametric "G_invariant" ![variable]) ~parameters ()
  in
  let right =
    let variable = Type.variable "T_Unconstrained" in
    Type.Callable.create ~annotation:variable ~parameters:(Type.Callable.Defined []) ()
  in
  let result =
    let handler = Environment.resolution environment () |> GlobalResolution.class_hierarchy in
    let handler =
      {
        handler;
        constructor = (fun _ ~protocol_assumptions:_ -> None);
        attributes = (fun _ ~protocol_assumptions:_ -> None);
        is_protocol = (fun _ ~protocol_assumptions:_ -> false);
        any_is_bottom = false;
        protocol_assumptions = ProtocolAssumptions.empty;
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
        (Type.parametric "G_invariant" ![Type.Any])
  | _ -> assert_failure "wrong number of solutions"


let () =
  "order"
  >::: [ "join" >:: test_join;
         "less_or_equal" >:: test_less_or_equal;
         "less_or_equal_variance" >:: test_less_or_equal_variance;
         "is_compatible_with" >:: test_is_compatible_with;
         "meet" >:: test_meet;
         "solve_less_or_equal" >:: test_solve_less_or_equal;
         "is_consistent_with" >:: test_is_consistent_with;
         "instantiate_protocol_parameters" >:: test_instantiate_protocol_parameters;
         "marks_escaped_as_escaped" >:: test_mark_escaped_as_escaped ]
  |> Test.run
