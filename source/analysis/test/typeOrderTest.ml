(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Pyre
open Analysis
open Ast
open Test
open TypeOrder
open Annotated
open Assumptions

let ( ! ) concretes = List.map concretes ~f:(fun single -> Type.Parameter.Single single)

let environment ?source context =
  let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
    let sources = Option.value_map source ~f:(fun source -> ["test.py", source]) ~default:[] in
    ScratchProject.setup ~context sources |> ScratchProject.build_global_environment
  in
  global_environment


let resolution ?source context =
  let environment = environment ?source context in
  GlobalResolution.create environment


let concrete_connect ?parameters =
  let parameters = parameters >>| List.map ~f:(fun single -> Type.Parameter.Single single) in
  MockClassHierarchyHandler.connect ?parameters


let make_attributes ~class_name =
  let parse_attribute (name, annotation) =
    Attribute.create
      ~annotation
      ~original_annotation:annotation
      ~uninstantiated_annotation:(Some annotation)
      ~visibility:ReadWrite
      ~abstract:false
      ~async_property:false
      ~class_variable:false
      ~defined:true
      ~initialized:OnClass
      ~parent:class_name
      ~property:false
      ~name
      ~undecorated_signature:None
      ~problem:None
  in
  List.map ~f:parse_attribute


let parse_attributes ~class_name ~parse_annotation attributes =
  List.map attributes ~f:(fun (name, annotation) -> name, parse_annotation annotation)
  |> make_attributes ~class_name


let get_typed_dictionary _ = None

let hierarchy class_hierarchy_handler =
  {
    ConstraintsSet.instantiate_successors_parameters =
      ClassHierarchy.instantiate_successors_parameters class_hierarchy_handler;
    is_transitive_successor = ClassHierarchy.is_transitive_successor class_hierarchy_handler;
    variables = ClassHierarchy.variables class_hierarchy_handler;
    least_upper_bound = ClassHierarchy.least_upper_bound class_hierarchy_handler;
  }


let attribute_from_attributes attributes =
  let attribute annotation ~assumptions ~name =
    let find attribute = String.equal (Annotated.Attribute.name attribute) name in
    attributes annotation ~assumptions >>= List.find ~f:find
  in
  attribute


let less_or_equal
    ?(attributes = fun _ ~assumptions:_ -> None)
    ?(is_protocol = fun _ ~protocol_assumptions:_ -> false)
    handler
  =
  always_less_or_equal
    {
      ConstraintsSet.class_hierarchy = hierarchy handler;
      all_attributes = attributes;
      attribute = attribute_from_attributes attributes;
      is_protocol;
      assumptions =
        {
          protocol_assumptions = ProtocolAssumptions.empty;
          callable_assumptions = CallableAssumptions.empty;
          decorator_assumptions = DecoratorAssumptions.empty;
        };
      get_typed_dictionary;
      metaclass = (fun _ ~assumptions:_ -> Some (Type.Primitive "type"));
    }


let is_compatible_with handler =
  is_compatible_with
    {
      ConstraintsSet.class_hierarchy = hierarchy handler;
      all_attributes = (fun _ ~assumptions:_ -> None);
      attribute = (fun _ ~assumptions:_ ~name:_ -> None);
      is_protocol = (fun _ ~protocol_assumptions:_ -> false);
      assumptions =
        {
          protocol_assumptions = ProtocolAssumptions.empty;
          callable_assumptions = CallableAssumptions.empty;
          decorator_assumptions = DecoratorAssumptions.empty;
        };
      get_typed_dictionary;
      metaclass = (fun _ ~assumptions:_ -> Some (Type.Primitive "type"));
    }


let join ?(attributes = fun _ ~assumptions:_ -> None) handler =
  join
    {
      ConstraintsSet.class_hierarchy = hierarchy handler;
      all_attributes = attributes;
      attribute = attribute_from_attributes attributes;
      is_protocol = (fun _ ~protocol_assumptions:_ -> false);
      assumptions =
        {
          protocol_assumptions = ProtocolAssumptions.empty;
          callable_assumptions = CallableAssumptions.empty;
          decorator_assumptions = DecoratorAssumptions.empty;
        };
      get_typed_dictionary;
      metaclass = (fun _ ~assumptions:_ -> Some (Type.Primitive "type"));
    }


let meet handler =
  meet
    {
      ConstraintsSet.class_hierarchy = hierarchy handler;
      all_attributes = (fun _ ~assumptions:_ -> None);
      attribute = (fun _ ~assumptions:_ ~name:_ -> None);
      is_protocol = (fun _ ~protocol_assumptions:_ -> false);
      assumptions =
        {
          protocol_assumptions = ProtocolAssumptions.empty;
          callable_assumptions = CallableAssumptions.empty;
          decorator_assumptions = DecoratorAssumptions.empty;
        };
      get_typed_dictionary;
      metaclass = (fun _ ~assumptions:_ -> Some (Type.Primitive "type"));
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
  insert order "bytes";
  insert order "bool";
  insert order "float";
  insert order "object";
  connect order ~predecessor:"str" ~successor:"object";
  connect order ~predecessor:"bytes" ~successor:"object";
  connect order ~predecessor:"int" ~successor:"float";
  connect order ~predecessor:"float" ~successor:"object";
  let type_builtin = "type" in
  let type_variable = Type.Variable (Type.Variable.Unary.create "_T") in
  insert order type_builtin;
  connect
    order
    ~predecessor:type_builtin
    ~parameters:[Single type_variable]
    ~successor:"typing.Generic";
  let typed_dictionary = "TypedDictionary" in
  let non_total_typed_dictionary = "NonTotalTypedDictionary" in
  let typing_mapping = "typing.Mapping" in
  insert order non_total_typed_dictionary;
  insert order typed_dictionary;
  insert order typing_mapping;
  connect order ~predecessor:non_total_typed_dictionary ~successor:typed_dictionary;
  concrete_connect
    order
    ~predecessor:typed_dictionary
    ~parameters:[Type.string; Type.object_primitive]
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
  concrete_connect
    order
    ~predecessor:"str"
    ~successor:"typing.Iterable"
    ~parameters:[Type.Primitive "str"];
  connect order ~predecessor:"typing.Iterable" ~successor:"object";
  insert order "AnyIterable";
  connect order ~predecessor:"AnyIterable" ~successor:"typing.Iterable";
  insert order "typing.Mapping";
  concrete_connect
    order
    ~predecessor:"typing.Mapping"
    ~successor:"typing.Generic"
    ~parameters:[variable; variable_covariant];
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
       ~left:(Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer))
       ~right:(Type.iterator Type.integer));
  assert_false
    (less_or_equal
       default
       ~left:(Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.float))
       ~right:(Type.iterator Type.integer));
  assert_true
    (less_or_equal
       default
       ~left:(Type.Primitive "tuple")
       ~right:(Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.float)));
  assert_true
    (less_or_equal
       default
       ~left:(Type.Tuple (Concrete [Type.integer; Type.integer]))
       ~right:(Type.parametric "tuple" ![Type.integer]));

  (* Union types *)
  assert_true (less_or_equal default ~left:Type.NoneType ~right:(Type.optional Type.integer));
  assert_true
    (less_or_equal
       default
       ~left:(Type.optional Type.string)
       ~right:(Type.Union [Type.integer; Type.optional Type.string]));

  (* Tuples. *)
  assert_true
    (less_or_equal
       default
       ~left:(Type.tuple [Type.integer; Type.integer])
       ~right:(Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer)));
  assert_true
    (less_or_equal
       default
       ~left:(Type.tuple [Type.integer; Type.integer])
       ~right:(Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.float)));
  assert_true
    (less_or_equal
       default
       ~left:(Type.tuple [Type.integer; Type.float])
       ~right:(Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.float)));
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
      ~parameters:[Type.string; Type.object_primitive]
      ~successor:typing_mapping;
    concrete_connect
      order
      ~parameters:[Type.variable "_T"; Type.variable ~variance:Covariant "_TCov"]
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
  assert_true (less_or_equal order ~left:Type.Any ~right:(Type.variable "T"));
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
       ~right:(Type.union [Type.variable ~constraints:(Type.Variable.Bound !!"A") "T"; Type.string]));
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
       ~right:(Type.union [Type.variable ~constraints:Type.Variable.Unconstrained "T"; Type.string]));
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
    let attributes annotation ~assumptions:_ =
      let parse_annotation =
        let aliases = function
          | "_T" -> Some (Type.variable "_T")
          | _ -> None
        in
        let aliases = create_type_alias_table aliases in
        parse_callable ~aliases
      in
      match annotation with
      | Type.Primitive "FloatToStrCallable" ->
          Some
            (parse_attributes
               ~parse_annotation
               ~class_name:"MatchesProtocol"
               ["__call__", "typing.Callable[[float], str]"])
      | Type.Parametric
          { name = "ParametricCallableToStr"; parameters = [Single (Primitive parameter)] } ->
          let callable = Format.sprintf "typing.Callable[[%s], str]" parameter in
          Some
            (parse_attributes
               ~parse_annotation
               ~class_name:"MatchesProtocol"
               ["__call__", callable])
      | annotation -> (
          match attributes with
          | Some attributes -> attributes annotation
          | None -> failwith ("getting attributes for wrong class" ^ Type.show annotation))
    in
    let aliases = create_type_alias_table aliases in
    less_or_equal
      ~attributes
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
  assert_false
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
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable('foo')[[int], int]"
       ~right:"typing.Callable('bar')[[int], int]");
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable('foo')[[str], int]"
       ~right:"typing.Callable('foo')[[int], int]");

  (* Callables with keyword-only parameters. *)
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable[[Named(foo, bool, default)], int]"
       ~right:"typing.Callable[[KeywordOnly(foo, bool, default)], int]");
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable[[Named(foo, bool, default)], int]"
       ~right:"typing.Callable[[KeywordOnly(foo, bool)], int]");
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable[[Named(foo, bool)], int]"
       ~right:"typing.Callable[[KeywordOnly(foo, bool)], int]");
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable[[Named(foo, bool)], int]"
       ~right:"typing.Callable[[KeywordOnly(foo, bool, default)], int]");

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
  assert_true
    (less_or_equal
       order
       ~left:"typing.Callable[[Variable(int), Keywords(int)], int]"
       ~right:"typing.Callable[[Keywords(int)], int]");

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
       ~left:"typing.Callable[[object], object][[[str], int][[int], str]]"
       ~right:"typing.Callable[[object], object][[[int], str]]");
  assert_false
    (less_or_equal
       order
       ~left:"typing.Callable[[object], object][[[str], int][[int], str]]"
       ~right:"typing.Callable[[object], object][[[str], str]]");

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
  let attributes annotation =
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
    | Type.Parametric { name = "B"; _ } ->
        Some
          (parse_attributes
             ~parse_annotation
             ~class_name:"B"
             ["__call__", "typing.Callable[[_T], str]"])
    | Type.Callable _ ->
        Some (make_attributes ~class_name:"typing.Callable" ["__call__", annotation])
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
      annotation
      (* Preprocess literal TypedDict syntax. *)
      |> parse_single_expression ~preprocess:true
      |> GlobalResolution.parse_annotation resolution
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
    ~left:"test.NonGenericChild"
    ~right:"test.GenericBase[typing.Any, typing.Any]"
    true;

  assert_less_or_equal
    ~source:
      {|
      from typing import Generic, TypeVar
      T1 = TypeVar("T1")
      T2 = TypeVar("T2")
      class GenericBase(Generic[T1, T2]): pass
      class NonGenericChild(GenericBase): pass
    |}
    ~left:"test.NonGenericChild"
    ~right:"test.GenericBase[int, str]"
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
    ~left:"test.GenericBase[typing.Any, typing.Any]"
    ~right:"test.GenericBase[int, str]"
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
    ~left:"test.NonGenericChild"
    ~right:"test.GenericBase[int, str]"
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
    ~left:"test.Grandchild"
    ~right:"test.GenericBase[typing.Any, typing.Any]"
    true;

  (* TypedDictionaries *)
  assert_less_or_equal
    ~source:
      {|
      from mypy_extensions import TypedDict
      class Alpha(TypedDict):
        foo: str
        bar: int
        baz: int
      class Beta(TypedDict):
        foo: str
        bar: int
    |}
    ~left:"test.Alpha"
    ~right:"test.Beta"
    true;
  assert_less_or_equal
    ~source:
      {|
      from mypy_extensions import TypedDict
      class Alpha(TypedDict, total=False):
        foo: str
        bar: int
        baz: int
      class Beta(TypedDict, total=False):
        foo: str
        bar: int
    |}
    ~left:"test.Alpha"
    ~right:"test.Beta"
    true;
  assert_less_or_equal
    ~source:
      {|
      from mypy_extensions import TypedDict
      class Alpha(TypedDict, total=False):
        foo: str
        bar: int
        baz: int
      class Beta(TypedDict):
        foo: str
        bar: int
    |}
    ~left:"test.Alpha"
    ~right:"test.Beta"
    false;
  assert_less_or_equal
    ~source:
      {|
      from mypy_extensions import TypedDict
      class Alpha(TypedDict):
        foo: str
        bar: int
        baz: int
      class Beta(TypedDict, total=False):
        foo: str
        bar: int
    |}
    ~left:"test.Alpha"
    ~right:"test.Beta"
    false;
  assert_less_or_equal
    ~source:
      {|
      from mypy_extensions import TypedDict
      class Alpha(TypedDict):
        foo: str
        bar: float
      class Beta(TypedDict):
        foo: str
        bar: int
    |}
    ~left:"test.Alpha"
    ~right:"test.Beta"
    false;
  assert_less_or_equal
    ~source:
      {|
      from mypy_extensions import TypedDict
      class Alpha(TypedDict):
        bar: int
        foo: str
      class Beta(TypedDict):
        foo: str
        bar: int
    |}
    ~left:"test.Alpha"
    ~right:"test.Beta"
    true;
  assert_less_or_equal
    ~source:
      {|
      from mypy_extensions import TypedDict
      class Alpha(TypedDict):
        bar: int
        foo: int
    |}
    ~left:"test.Alpha"
    ~right:"typing.Mapping[str, typing.Any]"
    true;

  assert_less_or_equal
    ~source:
      {|
      from mypy_extensions import TypedDict
      class Alpha(TypedDict, total=False):
        bar: int
        foo: int
    |}
    ~left:"test.Alpha"
    ~right:"typing.Mapping[str, typing.Any]"
    true;
  assert_less_or_equal
    ~source:
      {|
      from mypy_extensions import TypedDict
      class Alpha(TypedDict):
        bar: int
        foo: int
    |}
    ~left:"test.Alpha"
    ~right:"typing.Mapping[str, int]"
    false;
  assert_less_or_equal
    ~source:
      {|
      from mypy_extensions import TypedDict
      class Alpha(TypedDict):
        bar: int
        foo: int
    |}
    ~left:"typing.Mapping[str, typing.Any]"
    ~right:"test.Alpha"
    false;
  assert_less_or_equal
    ~source:
      {|
      from mypy_extensions import TypedDict
      class Alpha(TypedDict):
        bar: int
        foo: int
    |}
    ~left:"test.Alpha"
    ~right:"dict[str, typing.Any]"
    false;
  assert_less_or_equal
    ~source:
      {|
      from mypy_extensions import TypedDict
      class Base(TypedDict):
        foo: str
      class BarNotRequired(Base, total=False):
        bar: int
      class BarRequired(Base):
        bar: int
    |}
    ~left:"test.BarNotRequired"
    ~right:"test.BarRequired"
    false;
  assert_less_or_equal
    ~source:
      {|
      from mypy_extensions import TypedDict
      class Base(TypedDict):
        foo: str
      class BarNotRequired(Base, total=False):
        bar: int
      class BarRequired(Base):
        bar: int
    |}
    ~left:"test.BarRequired"
    ~right:"test.BarNotRequired"
    false;
  assert_less_or_equal
    ~source:
      {|
      from mypy_extensions import TypedDict
      class Base(TypedDict):
        foo: str
      class BarNotRequired(Base, total=False):
        bar: int
      class BarNotPresent(TypedDict):
        foo: str
    |}
    ~left:"test.BarNotRequired"
    ~right:"test.BarNotPresent"
    true;
  assert_less_or_equal
    ~source:
      {|
      from mypy_extensions import TypedDict
      class Base(TypedDict):
        foo: str
      class BarRequired(Base):
        bar: int
      class BarNotPresent(TypedDict):
        foo: str
    |}
    ~left:"test.BarRequired"
    ~right:"test.BarNotPresent"
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
    (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation list_of_integer));
  assert_is_compatible
    (Type.tuple [list_of_integer; list_of_integer])
    (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation list_of_float));
  assert_not_compatible
    (Type.tuple [list_of_integer; list_of_string])
    (Type.tuple [list_of_string; list_of_string]);
  assert_not_compatible
    (Type.tuple [list_of_float; list_of_integer])
    (Type.tuple [list_of_integer; list_of_float]);
  assert_not_compatible
    (Type.tuple [list_of_string; list_of_integer])
    (Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation list_of_float));

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
  assert_true
    (less_or_equal
       variance_order
       ~left:(Type.parametric "LinkedList" ![Type.integer])
       ~right:(Type.parametric "LinkedList" ![Type.Any]));
  assert_true
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
  assert_true
    (less_or_equal
       variance_order
       ~left:(Type.parametric "Box" ![Type.integer])
       ~right:(Type.parametric "Box" ![Type.Any]));

  (* Contravariant. *)
  assert_strict_less
    ~order:variance_order
    ~left:(Type.parametric "Sink" ![Type.float])
    ~right:(Type.parametric "Sink" ![Type.integer]);
  assert_true
    (less_or_equal
       variance_order
       ~left:(Type.parametric "Sink" ![Type.Any])
       ~right:(Type.parametric "Sink" ![Type.integer]));

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
  let assert_join ?(order = default) ?(aliases = Type.empty_aliases) left right expected =
    let parse_annotation = function
      | "$bottom" -> Type.Bottom
      | _ as source -> parse_single_expression source |> Type.create ~aliases
    in
    let attributes annotation ~assumptions:_ =
      let parse_annotation =
        let aliases = function
          | "_T" -> Some (Type.variable "_T")
          | _ -> None
        in
        let aliases = create_type_alias_table aliases in
        parse_callable ~aliases
      in
      match annotation with
      | Type.Primitive "CallableClass" ->
          Some
            (parse_attributes
               ~parse_annotation
               ~class_name:"MatchesProtocol"
               ["__call__", "typing.Callable[[int], str]"])
      | Type.Parametric
          { name = "ParametricCallableToStr"; parameters = [Single (Primitive parameter)] } ->
          let callable = Format.sprintf "typing.Callable[[%s], str]" parameter in
          Some
            (parse_attributes
               ~parse_annotation
               ~class_name:"MatchesProtocol"
               ["__call__", callable])
      | annotation -> failwith ("getting attributes for wrong class" ^ Type.show annotation)
    in
    assert_type_equal
      (parse_annotation expected)
      (join ~attributes order (parse_annotation left) (parse_annotation right));
    assert_type_equal
      (parse_annotation expected)
      (join ~attributes order (parse_annotation right) (parse_annotation left))
  in
  (* Primitive types. *)
  assert_join "list" "typing.Sized" "typing.Sized";
  assert_join "typing.Sized" "list" "typing.Sized";
  assert_join "typing.List[int]" "typing.Sized" "typing.Sized";
  assert_join "int" "str" "typing.Union[int, str]";

  (* Parametric types. *)
  assert_join "typing.List[float]" "typing.List[float]" "typing.List[float]";
  assert_join
    "typing.List[float]"
    "typing.List[int]"
    "typing.Union[typing.List[float], typing.List[int]]";
  assert_join "typing.List[int]" "typing.Iterator[int]" "typing.Iterator[int]";
  assert_join "typing.Iterator[int]" "typing.List[int]" "typing.Iterator[int]";
  assert_join "typing.List[float]" "typing.Iterator[int]" "typing.Iterator[float]";
  assert_join "typing.List[float]" "float[int]" "typing.Union[typing.List[float], float[int]]";

  (* Annotated types. *)
  assert_join "typing.Annotated[int]" "float" "typing.Annotated[float]";
  assert_join "typing.Annotated[int]" "typing.Annotated[float]" "typing.Annotated[float]";

  assert_join "typing_extensions.Annotated[int]" "float" "typing_extensions.Annotated[float]";
  assert_join
    "typing_extensions.Annotated[int]"
    "typing_extensions.Annotated[float]"
    "typing_extensions.Annotated[float]";

  (* TODO(T41082573) throw here instead of unioning *)
  assert_join "typing.Tuple[int, int]" "typing.Iterator[int]" "typing.Iterator[int]";

  let variadic = Type.Variable.Variadic.Tuple.create "Ts" in
  assert_join
    ~aliases:(fun ?replace_unbound_parameters_with_any:_ name ->
      match name with
      | "Ts" -> Some (Type.VariableAlias (Type.Variable.TupleVariadic variadic))
      | _ -> None)
    "typing.Tuple[pyre_extensions.Unpack[Ts]]"
    "typing.Tuple[int, ...]"
    "typing.Tuple[object, ...]";

  (* Optionals. *)
  assert_join "str" "typing.Optional[str]" "typing.Optional[str]";

  (* Handles `[] or optional_list`. *)
  assert_join "typing.List[int]" "typing.List[typing.Any]" "typing.List[typing.Any]";
  assert_join "typing.List[typing.Any]" "typing.List[int]" "typing.List[typing.Any]";

  (* Union types. *)
  assert_join
    "typing.Optional[bool]"
    "typing.Union[int, typing.Optional[bool]]"
    "typing.Union[int, typing.Optional[bool]]";
  assert_join "typing.Union[int, str]" "typing.Union[int, bytes]" "typing.Union[int, str, bytes]";
  assert_join "typing.Union[int, str]" "None" "typing.Union[int, str, None]";
  assert_join
    "typing.Dict[str, str]"
    "typing.Dict[str, typing.List[str]]"
    "typing.Union[typing.Dict[str, typing.List[str]], typing.Dict[str, str]]";
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
  assert_join
    "typing.List[typing.Any]"
    "typing.Union[typing.List[int], typing.List[str]]"
    "typing.List[typing.Any]";

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
      ~parameters:[Type.variable "_T"]
      ~predecessor:"ParametricCallableToStr"
      ~successor:"typing.Generic";
    handler order
  in
  assert_join ~order:disconnected_order "A" "B" "typing.Union[A, B]";
  assert_join "typing.Type[int]" "typing.Type[str]" "typing.Type[typing.Union[int, str]]";

  (* Callables. *)
  assert_join
    "typing.Callable[..., int]"
    "typing.Callable[..., str]"
    "typing.Callable[..., typing.Union[int, str]]";
  assert_join
    "typing.Callable('derp')[..., int]"
    "typing.Callable('derp')[..., int]"
    "typing.Callable('derp')[..., int]";
  assert_join
    "typing.Callable('derp')[..., int]"
    "typing.Callable('other')[..., int]"
    "typing.Union[typing.Callable(derp)[..., int], typing.Callable(other)[..., int]]";

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
       [
         Type.string;
         Type.variable ~constraints:(Type.Variable.Explicit [Type.float; Type.integer]) "T";
       ]);
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
    (Type.union
       [
         Type.parametric "Map" ![Type.integer; Type.integer];
         Type.parametric "Map" ![Type.Top; Type.string];
       ]);
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
  (* Contravariant *)
  assert_type_equal
    (join
       variance_order
       (Type.parametric "Sink" ![Type.integer])
       (Type.parametric "Sink" ![Type.string]))
    (Type.Union [Type.parametric "Sink" ![Type.integer]; Type.parametric "Sink" ![Type.string]]);
  let variance_aliases =
    Identifier.Table.of_alist_exn
      [
        "_T", Type.variable "_T";
        "_T_co", Type.variable "_T_co" ~variance:Covariant;
        "_T_contra", Type.variable "_T_contra" ~variance:Contravariant;
      ]
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
  assert_type_equal
    (join order (Type.literal_string "A") (Type.literal_string "B"))
    (Type.Literal (String AnyLiteral));
  assert_type_equal (join order (Type.literal_string "A") Type.string) Type.string;
  assert_type_equal
    (join order (Type.literal_string "A") Type.integer)
    (Type.union [Type.string; Type.integer]);
  assert_type_equal
    (join order (Type.Literal (String AnyLiteral)) (Type.Literal (String AnyLiteral)))
    (Type.Literal (String AnyLiteral));
  assert_type_equal
    (join order (Type.Literal (String AnyLiteral)) (Type.literal_string "hello"))
    (Type.Literal (String AnyLiteral));
  assert_type_equal (join order (Type.Literal (String AnyLiteral)) Type.string) Type.string;
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
  let assert_join_direct ?(source = "") ~left ~right expected_annotation =
    let resolution = resolution ~source context in
    let parse_annotation annotation =
      annotation |> parse_single_expression |> GlobalResolution.parse_annotation resolution
    in
    let left, right = parse_annotation left, parse_annotation right in
    assert_type_equal expected_annotation (GlobalResolution.join resolution left right)
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
    ~left:"test.NonGenericChild"
    ~right:"test.GenericBase[int, str]"
    "test.GenericBase[int, str]";
  let recursive_alias_source =
    {|
      from typing import Tuple, Union

      Tree = Union[int, Tuple["Tree", "Tree"]]
      Tree2 = Union[int, Tuple["Tree2", "Tree2"]]
      TreeWithStr = Union[str, Tuple["TreeWithStr", "TreeWithStr"]]
      TreeWithStrAndInt = Union[str, int, Tuple["TreeWithStrAndInt", "TreeWithStrAndInt"]]
    |}
  in
  assert_join ~source:recursive_alias_source ~left:"test.Tree" ~right:"test.Tree" "test.Tree";
  assert_join ~source:recursive_alias_source ~left:"test.Tree" ~right:"int" "test.Tree";
  assert_join ~source:recursive_alias_source ~left:"int" ~right:"test.Tree" "test.Tree";
  assert_join
    ~source:recursive_alias_source
    ~left:"test.Tree"
    ~right:"str"
    "typing.Union[test.Tree, str]";

  Type.RecursiveType.Namespace.reset ();
  let fresh_name = Type.RecursiveType.Namespace.create_fresh_name () in
  Type.RecursiveType.Namespace.reset ();
  assert_join_direct
    ~source:recursive_alias_source
    ~left:"test.Tree"
    ~right:"test.Tree2"
    (Type.RecursiveType.create
       ~name:fresh_name
       ~body:
         (Type.union
            [Type.integer; Type.tuple [Type.Primitive fresh_name; Type.Primitive fresh_name]]));
  Type.RecursiveType.Namespace.reset ();
  assert_join_direct
    ~source:recursive_alias_source
    ~left:"test.Tree"
    ~right:"test.TreeWithStrAndInt"
    (Type.RecursiveType.create
       ~name:fresh_name
       ~body:
         (Type.union
            [
              Type.integer;
              Type.string;
              Type.tuple [Type.Primitive fresh_name; Type.Primitive fresh_name];
            ]));
  Type.RecursiveType.Namespace.reset ();
  assert_join_direct
    ~source:recursive_alias_source
    ~left:"test.Tree"
    ~right:"test.TreeWithStr"
    (Type.RecursiveType.create
       ~name:fresh_name
       ~body:
         (Type.union
            [
              Type.integer;
              Type.string;
              Type.tuple [Type.Primitive fresh_name; Type.Primitive fresh_name];
            ]));
  ()


let test_meet _ =
  let assert_meet ?(order = default) ?(aliases = Type.empty_aliases) left right expected =
    let parse_annotation = function
      | "$bottom" -> Type.Bottom
      | _ as source -> parse_single_expression source |> Type.create ~aliases
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

  assert_meet "typing_extensions.Annotated[int]" "float" "typing_extensions.Annotated[int]";
  assert_meet
    "typing_extensions.Annotated[int]"
    "typing_extensions.Annotated[float]"
    "typing_extensions.Annotated[int]";

  (* Unions. *)
  assert_meet "typing.Union[int, str]" "typing.Union[int, bytes]" "int";
  assert_meet "typing.Union[int, str]" "typing.Union[str, int]" "typing.Union[int, str]";
  assert_meet "typing.Union[int, str]" "float" "int";
  assert_meet "typing.Union[int, str]" "typing.List[int]" "$bottom";
  assert_meet "typing.Union[int, str]" "typing.Union[float, bool]" "int";
  assert_meet "typing.Union[int, str]" "typing.Union[int, bool]" "int";

  assert_meet
    "typing.Union[int, str]"
    "typing.Union[int, typing.Optional[str]]"
    "typing.Union[int, str]";
  assert_meet
    "typing.Union[int, typing.Optional[str]]"
    "typing.Optional[str]"
    "typing.Optional[str]";

  (* Parametric types. *)
  assert_meet "typing.List[int]" "typing.Iterator[int]" "typing.List[int]";
  assert_meet "typing.List[float]" "typing.Iterator[int]" "$bottom";
  assert_meet "typing.List[float]" "float[int]" "$bottom";
  assert_meet "typing.Dict[str, str]" "typing.Dict[str, typing.List[str]]" "$bottom";
  assert_meet ~order:disconnected_order "A" "B" "$bottom";
  assert_meet "GenericContainer[int, str]" "DifferentGenericContainer[int, str]" "$bottom";
  assert_meet "GenericContainer[int, str]" "DifferentGenericContainer[str, int]" "$bottom";

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
    (Type.parametric "LinkedList" ![Type.integer]);
  assert_type_equal
    (meet
       variance_order
       (Type.parametric "LinkedList" ![Type.Any])
       (Type.parametric "LinkedList" ![Type.integer]))
    (Type.parametric "LinkedList" ![Type.Any]);

  (* TODO (T45909999): Revisit these tests and only keep the useful ones *)
  let _obsolete_tests () =
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
    "$bottom";
  assert_meet
    ~order:(make_potentially_inconsistent_order ~x_before_y:false)
    "B[int, str]"
    "A[str]"
    "$bottom";

  let tree_annotation =
    Type.RecursiveType.create
      ~name:"Tree"
      ~body:(Type.union [Type.integer; Type.tuple [Type.Primitive "Tree"; Type.Primitive "Tree"]])
  in
  let tree_annotation2 =
    Type.RecursiveType.create
      ~name:"Tree2"
      ~body:(Type.union [Type.integer; Type.tuple [Type.Primitive "Tree2"; Type.Primitive "Tree2"]])
  in
  let tree_annotation_with_string =
    Type.RecursiveType.create
      ~name:"Tree2"
      ~body:
        (Type.union
           [Type.integer; Type.string; Type.tuple [Type.Primitive "Tree2"; Type.Primitive "Tree2"]])
  in
  let non_tree =
    Type.RecursiveType.create
      ~name:"NonTree"
      ~body:(Type.union [Type.bool; Type.tuple [Type.Primitive "NonTree"]])
  in
  (* Recursive types. *)
  assert_type_equal tree_annotation (meet default tree_annotation tree_annotation);
  assert_type_equal tree_annotation (meet default tree_annotation tree_annotation2);
  assert_type_equal tree_annotation (meet default tree_annotation tree_annotation_with_string);
  assert_type_equal Type.integer (meet default tree_annotation Type.integer);
  assert_type_equal Type.Bottom (meet default tree_annotation Type.string);
  assert_type_equal Type.Bottom (meet default tree_annotation non_tree);
  ()


let test_meet_callable _ =
  let assert_meet ?(order = default) left right expected =
    assert_type_equal expected (meet order left right)
  in
  let named_int_to_int =
    Type.Callable.create
      ~name:(Reference.create "name")
      ~parameters:
        (Defined
           [
             Type.Callable.Parameter.Named { name = "a"; annotation = Type.integer; default = false };
           ])
      ~annotation:Type.integer
      ()
  in
  assert_meet named_int_to_int named_int_to_int named_int_to_int;
  let anonymous_str_to_int =
    Type.Callable.create
      ~parameters:
        (Defined
           [Type.Callable.Parameter.Named { name = "a"; annotation = Type.string; default = false }])
      ~annotation:Type.integer
      ()
  in
  let anonymous_positional_only_str_to_int =
    Type.Callable.create
      ~parameters:
        (Defined
           [
             Type.Callable.Parameter.PositionalOnly
               { index = 0; annotation = Type.string; default = false };
           ])
      ~annotation:Type.integer
      ()
  in
  let anonymous_int_to_float =
    Type.Callable.create
      ~parameters:
        (Defined
           [
             Type.Callable.Parameter.Named { name = "a"; annotation = Type.integer; default = false };
           ])
      ~annotation:Type.float
      ()
  in
  let anonymous_union_int_str_to_int =
    Type.Callable.create
      ~parameters:
        (Defined
           [
             Type.Callable.Parameter.Named
               { name = "a"; annotation = Type.union [Type.integer; Type.string]; default = false };
           ])
      ~annotation:Type.integer
      ()
  in
  assert_meet anonymous_int_to_float anonymous_str_to_int anonymous_union_int_str_to_int;
  assert_meet anonymous_positional_only_str_to_int anonymous_str_to_int anonymous_str_to_int;

  let anonymous_undefined_to_object = Type.Callable.create ~annotation:Type.object_primitive () in
  let anonymous_undefined_to_int = Type.Callable.create ~annotation:Type.integer () in
  let anonymous_str_named_b_to_int =
    Type.Callable.create
      ~parameters:
        (Defined
           [Type.Callable.Parameter.Named { name = "b"; annotation = Type.string; default = false }])
      ~annotation:Type.integer
      ()
  in
  assert_meet anonymous_str_to_int anonymous_str_named_b_to_int anonymous_undefined_to_int;
  assert_meet
    anonymous_positional_only_str_to_int
    anonymous_undefined_to_object
    anonymous_undefined_to_int;
  assert_meet anonymous_str_to_int anonymous_undefined_to_object anonymous_undefined_to_int;
  assert_meet named_int_to_int anonymous_undefined_to_object anonymous_undefined_to_int;
  assert_meet anonymous_undefined_to_object named_int_to_int anonymous_undefined_to_int;
  assert_meet named_int_to_int anonymous_str_to_int anonymous_union_int_str_to_int;
  assert_meet anonymous_str_to_int named_int_to_int anonymous_union_int_str_to_int;

  let overloaded_str_to_int =
    Type.Callable.create
      ~parameters:
        (Defined
           [Type.Callable.Parameter.Named { name = "a"; annotation = Type.string; default = false }])
      ~annotation:(Type.union [Type.integer; Type.string])
      ~overloads:
        [
          {
            parameters =
              Defined
                [
                  Type.Callable.Parameter.Named
                    { name = "a"; annotation = Type.string; default = false };
                ];
            annotation = Type.integer;
          };
        ]
      ()
  in
  assert_meet overloaded_str_to_int overloaded_str_to_int overloaded_str_to_int;
  assert_meet overloaded_str_to_int anonymous_str_to_int Type.Bottom;
  ()


let () =
  "order"
  >::: [
         "join" >:: test_join;
         "less_or_equal" >:: test_less_or_equal;
         "less_or_equal_variance" >:: test_less_or_equal_variance;
         "is_compatible_with" >:: test_is_compatible_with;
         "meet" >:: test_meet;
         "meet_callable" >:: test_meet_callable;
       ]
  |> Test.run
