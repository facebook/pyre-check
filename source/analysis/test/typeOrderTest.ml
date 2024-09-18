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
open CycleDetection

let ( ! ) concretes = List.map concretes ~f:(fun single -> Type.Argument.Single single)

let variable_aliases name =
  match name with
  | "Ts" -> Some (Type.Variable.TypeVarTupleVariable (Type.Variable.TypeVarTuple.create name))
  | _ -> None


let environment ?source context =
  let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
    let sources = Option.value_map source ~f:(fun source -> ["test.py", source]) ~default:[] in
    ScratchProject.setup ~context sources |> ScratchProject.build_global_environment
  in
  global_environment


let resolution ?source context =
  let environment = environment ?source context in
  GlobalResolution.create environment


let concrete_connect ?arguments =
  let arguments = arguments >>| List.map ~f:(fun single -> Type.Argument.Single single) in
  MockClassHierarchyHandler.connect ?arguments


let concrete_connect_with_variance ~arguments_with_variances =
  let arguments_with_variances =
    arguments_with_variances
    |> List.map ~f:(fun (single, variance) -> Type.Argument.Single single, variance)
  in
  MockClassHierarchyHandler.connect_with_variance ~arguments_with_variances


let make_attributes ~class_name =
  let parse_attribute (name, annotation) =
    AnnotatedAttribute.create_instantiated
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
  in
  List.map ~f:parse_attribute


let parse_attributes ~class_name ~parse_annotation attributes =
  List.map attributes ~f:(fun (name, annotation) -> name, parse_annotation annotation)
  |> make_attributes ~class_name


let get_typed_dictionary _ = None

let get_named_tuple_fields _ = None

let hierarchy class_hierarchy_handler =
  let has_transitive_successor ~successor predecessor =
    match
      ClassHierarchy.method_resolution_order_linearize
        ~get_successors:(ClassHierarchy.parents_of class_hierarchy_handler)
        predecessor
    with
    | Result.Error error ->
        let message =
          Format.asprintf
            "Invalid type order setup for %s: %a"
            predecessor
            Sexp.pp_hum
            (ClassHierarchy.MethodResolutionOrderError.sexp_of_t error)
        in
        failwith message
    | Result.Ok mro_of_source ->
        List.exists mro_of_source ~f:(fun current -> Type.Primitive.equal successor current)
  in
  let least_upper_bound left right =
    let get_successors = ClassHierarchy.parents_of class_hierarchy_handler in
    match
      ( ClassHierarchy.method_resolution_order_linearize ~get_successors left,
        ClassHierarchy.method_resolution_order_linearize ~get_successors right )
    with
    | Result.Error error, _ ->
        let message =
          Format.asprintf
            "Invalid type order setup for %s: %a"
            left
            Sexp.pp_hum
            (ClassHierarchy.MethodResolutionOrderError.sexp_of_t error)
        in
        failwith message
    | _, Result.Error error ->
        let message =
          Format.asprintf
            "Invalid type order setup for %s: %a"
            right
            Sexp.pp_hum
            (ClassHierarchy.MethodResolutionOrderError.sexp_of_t error)
        in
        failwith message
    | Result.Ok left_mro, Result.Ok right_mro ->
        let right_mro = String.Hash_set.of_list right_mro in
        List.find left_mro ~f:(Hash_set.mem right_mro)
  in
  {
    ConstraintsSet.instantiate_successors_parameters =
      ClassHierarchy.instantiate_successors_parameters class_hierarchy_handler;
    has_transitive_successor;
    generic_parameters = ClassHierarchy.generic_parameters class_hierarchy_handler;
    least_upper_bound;
  }


let attribute_from_attributes attributes =
  let attribute annotation ~cycle_detections ~name =
    let find attribute = String.equal (AnnotatedAttribute.name attribute) name in
    attributes annotation ~cycle_detections >>= List.find ~f:find
  in
  attribute


let less_or_equal
    ?(attributes = fun _ ~cycle_detections:_ -> None)
    ?(is_protocol = fun _ -> false)
    handler
  =
  always_less_or_equal
    {
      ConstraintsSet.class_hierarchy = hierarchy handler;
      instantiated_attributes = attributes;
      attribute = attribute_from_attributes attributes;
      is_protocol;
      cycle_detections =
        {
          assumed_protocol_instantiations = AssumedProtocolInstantiations.empty;
          assumed_callable_types = AssumedCallableTypes.empty;
          decorators_being_resolved = DecoratorsBeingResolved.empty;
        };
      get_typed_dictionary;
      get_named_tuple_fields;
      metaclass = (fun _ ~cycle_detections:_ -> Some (Type.Primitive "type"));
    }


let join ?(attributes = fun _ ~cycle_detections:_ -> None) handler =
  join
    {
      ConstraintsSet.class_hierarchy = hierarchy handler;
      instantiated_attributes = attributes;
      attribute = attribute_from_attributes attributes;
      is_protocol = (fun _ -> false);
      cycle_detections =
        {
          assumed_protocol_instantiations = AssumedProtocolInstantiations.empty;
          assumed_callable_types = AssumedCallableTypes.empty;
          decorators_being_resolved = DecoratorsBeingResolved.empty;
        };
      get_typed_dictionary;
      get_named_tuple_fields;
      metaclass = (fun _ ~cycle_detections:_ -> Some (Type.Primitive "type"));
    }


let meet handler =
  meet
    {
      ConstraintsSet.class_hierarchy = hierarchy handler;
      instantiated_attributes = (fun _ ~cycle_detections:_ -> None);
      attribute = (fun _ ~cycle_detections:_ ~name:_ -> None);
      is_protocol = (fun _ -> false);
      cycle_detections =
        {
          assumed_protocol_instantiations = AssumedProtocolInstantiations.empty;
          assumed_callable_types = AssumedCallableTypes.empty;
          decorators_being_resolved = DecoratorsBeingResolved.empty;
        };
      get_typed_dictionary;
      get_named_tuple_fields;
      metaclass = (fun _ ~cycle_detections:_ -> Some (Type.Primitive "type"));
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
  let variable_t_co = Type.variable "_T_co" in
  let variable_t_contra = Type.variable "_T_contra" in
  insert order "LinkedList";
  insert order "Map";
  insert order "Box";
  insert order "Sink";
  concrete_connect
    order
    ~predecessor:"LinkedList"
    ~successor:"typing.Generic"
    ~arguments:[variable_t];
  concrete_connect
    order
    ~predecessor:"Map"
    ~successor:"typing.Generic"
    ~arguments:[variable_t; variable_t_2];
  concrete_connect_with_variance
    order
    ~predecessor:"Box"
    ~successor:"typing.Generic"
    ~arguments_with_variances:[variable_t_co, Type.Record.Variance.Covariant];
  concrete_connect_with_variance
    order
    ~predecessor:"Sink"
    ~successor:"typing.Generic"
    ~arguments_with_variances:[variable_t_contra, Type.Record.Variance.Contravariant];
  insert order "Base";
  insert order "Derived";
  concrete_connect_with_variance
    order
    ~predecessor:"Base"
    ~successor:"typing.Generic"
    ~arguments_with_variances:[variable_t_contra, Type.Record.Variance.Contravariant];
  concrete_connect_with_variance
    order
    ~predecessor:"Derived"
    ~successor:"Base"
    ~arguments_with_variances:[variable_t_co, Type.Record.Variance.Covariant];
  concrete_connect_with_variance
    order
    ~predecessor:"Derived"
    ~successor:"typing.Generic"
    ~arguments_with_variances:[variable_t_co, Type.Record.Variance.Covariant];
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
  let variable_t_co = Type.variable "_T_co" in
  let variable_t_contra = Type.variable "_T_contra" in
  insert order "A";
  insert order "B";
  insert order "C";
  insert order "D";
  concrete_connect_with_variance
    order
    ~predecessor:"A"
    ~successor:"typing.Generic"
    ~arguments_with_variances:[variable_t_co, Covariant; variable_t_contra, Contravariant];
  concrete_connect_with_variance
    order
    ~predecessor:"B"
    ~successor:"A"
    ~arguments_with_variances:[variable_t_contra, Contravariant; variable_t_co, Covariant];
  concrete_connect_with_variance
    order
    ~predecessor:"B"
    ~successor:"typing.Generic"
    ~arguments_with_variances:[variable_t_contra, Contravariant; variable_t_co, Covariant];
  concrete_connect order ~predecessor:"C" ~successor:"B" ~arguments:[Type.integer; Type.integer];
  concrete_connect order ~predecessor:"D" ~successor:"B" ~arguments:[Type.float; Type.float];
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
  let variable_t_co = Type.variable "_T_co" in
  let variable_t_contra = Type.variable "_T_contra" in
  insert order "A";
  insert order "B";
  insert order "C";
  insert order "D";
  concrete_connect_with_variance
    order
    ~predecessor:"A"
    ~successor:"typing.Generic"
    ~arguments_with_variances:[variable_t_co, Covariant; variable_t_contra, Contravariant];
  concrete_connect_with_variance
    order
    ~predecessor:"B"
    ~successor:"A"
    ~arguments_with_variances:[variable_t_co, Covariant; variable_t_contra, Contravariant];
  concrete_connect_with_variance
    order
    ~predecessor:"B"
    ~successor:"typing.Generic"
    ~arguments_with_variances:[variable_t_co, Covariant; variable_t_contra, Contravariant];
  concrete_connect order ~predecessor:"C" ~successor:"B" ~arguments:[Type.integer; Type.integer];
  concrete_connect order ~predecessor:"D" ~successor:"B" ~arguments:[Type.float; Type.float];
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
  let type_variable = Type.Variable (Type.Variable.TypeVar.create "_T") in
  insert order type_builtin;
  connect
    order
    ~predecessor:type_builtin
    ~arguments:[Single type_variable]
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
    ~arguments:[Type.string; Type.object_primitive]
    ~successor:typing_mapping;
  let variable = Type.variable "_T" in
  let other_variable = Type.variable "_T2" in
  let variable_covariant = Type.variable "_T_co" in
  insert order "typing.Sequence";
  concrete_connect
    order
    ~predecessor:"typing.Sequence"
    ~successor:"typing.Generic"
    ~arguments:[variable];
  insert order "list";
  insert order "typing.Sized";
  connect order ~predecessor:"list" ~successor:"typing.Sized";
  concrete_connect order ~predecessor:"list" ~successor:"typing.Generic" ~arguments:[variable];
  concrete_connect order ~predecessor:"list" ~successor:"typing.Sequence" ~arguments:[variable];

  insert order "typing.AbstractSet";
  insert order "set";
  connect order ~predecessor:"set" ~successor:"typing.Sized";
  concrete_connect order ~predecessor:"set" ~successor:"typing.Generic" ~arguments:[variable];
  concrete_connect
    order
    ~predecessor:"typing.AbstractSet"
    ~successor:"typing.Generic"
    ~arguments:[variable];
  concrete_connect order ~predecessor:"set" ~successor:"typing.AbstractSet" ~arguments:[variable];
  insert order "typing.Iterator";
  concrete_connect_with_variance
    order
    ~predecessor:"typing.Iterator"
    ~successor:"typing.Generic"
    ~arguments_with_variances:[variable_covariant, Covariant];
  insert order "typing.Iterable";
  concrete_connect_with_variance
    order
    ~predecessor:"typing.Iterator"
    ~successor:"typing.Iterable"
    ~arguments_with_variances:[variable_covariant, Covariant];
  concrete_connect_with_variance
    order
    ~predecessor:"typing.Iterable"
    ~successor:"typing.Generic"
    ~arguments_with_variances:[variable_covariant, Covariant];
  concrete_connect order ~predecessor:"list" ~successor:"typing.Iterable" ~arguments:[variable];
  concrete_connect order ~predecessor:"list" ~successor:"typing.Iterator" ~arguments:[variable];
  insert order "tuple";
  concrete_connect order ~predecessor:"tuple" ~successor:"typing.Generic" ~arguments:[variable];
  concrete_connect order ~predecessor:"tuple" ~successor:"typing.Iterator" ~arguments:[variable];
  insert order "typing.Generator";
  concrete_connect
    order
    ~predecessor:"typing.Generator"
    ~successor:"typing.Iterator"
    ~arguments:[variable];
  concrete_connect
    order
    ~predecessor:"typing.Generator"
    ~successor:"typing.Generic"
    ~arguments:[variable];
  concrete_connect
    order
    ~predecessor:"str"
    ~successor:"typing.Iterable"
    ~arguments:[Type.Primitive "str"];
  connect order ~predecessor:"typing.Iterable" ~successor:"object";
  insert order "AnyIterable";
  connect order ~predecessor:"AnyIterable" ~successor:"typing.Iterable";
  insert order "typing.Mapping";
  concrete_connect_with_variance
    order
    ~predecessor:"typing.Mapping"
    ~successor:"typing.Generic"
    ~arguments_with_variances:[variable, Invariant; variable_covariant, Covariant];
  insert order "dict";

  concrete_connect
    order
    ~predecessor:"dict"
    ~successor:"typing.Generic"
    ~arguments:[variable; other_variable];
  concrete_connect
    order
    ~predecessor:"dict"
    ~successor:"typing.Mapping"
    ~arguments:[variable; other_variable];
  concrete_connect order ~predecessor:"dict" ~successor:"typing.Iterator" ~arguments:[variable];
  insert order "collections.OrderedDict";
  concrete_connect
    order
    ~predecessor:"collections.OrderedDict"
    ~successor:"typing.Generic"
    ~arguments:[variable; other_variable];
  concrete_connect
    order
    ~predecessor:"collections.OrderedDict"
    ~successor:"dict"
    ~arguments:[variable; other_variable];
  insert order "PartiallySpecifiedDict";
  concrete_connect
    order
    ~predecessor:"PartiallySpecifiedDict"
    ~successor:"dict"
    ~arguments:[Primitive "int"];
  insert order "OverSpecifiedDict";
  concrete_connect
    order
    ~predecessor:"OverSpecifiedDict"
    ~successor:"dict"
    ~arguments:[Primitive "int"; Primitive "int"; Primitive "str"];
  insert order "GenericContainer";
  concrete_connect
    order
    ~predecessor:"GenericContainer"
    ~successor:"typing.Generic"
    ~arguments:[variable; other_variable];

  insert order "NonGenericContainerChild";
  concrete_connect
    order
    ~predecessor:"NonGenericContainerChild"
    ~successor:"GenericContainer"
    ~arguments:[Primitive "int"; Primitive "str"];

  insert order "DifferentGenericContainer";
  concrete_connect
    order
    ~predecessor:"DifferentGenericContainer"
    ~successor:"typing.Generic"
    ~arguments:[variable; other_variable];
  insert order "CommonNonGenericChild";
  concrete_connect
    order
    ~predecessor:"CommonNonGenericChild"
    ~successor:"GenericContainer"
    ~arguments:[Primitive "int"; Primitive "str"];
  concrete_connect
    order
    ~predecessor:"CommonNonGenericChild"
    ~successor:"DifferentGenericContainer"
    ~arguments:[Primitive "int"; Primitive "str"];
  insert order "Child";
  insert order "Base";
  connect order ~predecessor:"Child" ~successor:"Base";
  connect order ~predecessor:"Base" ~successor:"object";
  handler order


let ( !! ) name = Type.Primitive name

let test_less_or_equal_primitives =
  let assert_false value _ = assert_false value in
  let assert_true value _ = assert_true value in
  test_list
    [
      (* Primitive types. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true (less_or_equal order ~left:Type.Bottom ~right:Type.Top);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false (less_or_equal order ~left:Type.Top ~right:Type.Bottom);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true (less_or_equal order ~left:!!"0" ~right:!!"0");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true (less_or_equal order ~left:Type.Bottom ~right:!!"0");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true (less_or_equal order ~left:Type.Bottom ~right:!!"1");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true (less_or_equal order ~left:Type.Bottom ~right:!!"2");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true (less_or_equal order ~left:Type.Bottom ~right:!!"3");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false (less_or_equal order ~left:!!"3" ~right:Type.Bottom);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false (less_or_equal order ~left:!!"2" ~right:Type.Bottom);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false (less_or_equal order ~left:!!"1" ~right:Type.Bottom);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false (less_or_equal order ~left:!!"0" ~right:Type.Bottom);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true (less_or_equal order ~left:!!"0" ~right:!!"3");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true (less_or_equal order ~left:!!"1" ~right:!!"3");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false (less_or_equal order ~left:!!"2" ~right:!!"3");
    ]


let test_less_or_equal =
  let float_string_variable =
    Type.variable
      ~constraints:(Type.Record.TypeVarConstraints.Explicit [Type.float; Type.string])
      "T"
  in
  (* Behavioral subtyping of callables. *)
  let parse_less_or_equal ?attributes ?is_protocol order ~left ~right =
    let aliases = function
      | "T_Unconstrained" -> Some (Type.variable "T_Unconstrained")
      | "T_int_bool" ->
          Some
            (Type.variable
               "T_int_bool"
               ~constraints:(Type.Record.TypeVarConstraints.Explicit [Type.integer; Type.bool]))
      | _ -> None
    in
    let attributes annotation ~cycle_detections:_ =
      let parse_annotation =
        let aliases = function
          | "_T" -> Some (Type.variable "_T")
          | _ -> None
        in
        let aliases = create_type_alias_table aliases in
        let resolved_aliases ?replace_unbound_parameters_with_any:_ name =
          match aliases name with
          | Some (TypeAliasEnvironment.RawAlias.TypeAlias t) -> Some t
          | _ -> None
        in
        parse_callable ~aliases:resolved_aliases
      in
      match annotation with
      | Type.Primitive "FloatToStrCallable" ->
          Some
            (parse_attributes
               ~parse_annotation
               ~class_name:"MatchesProtocol"
               ["__call__", "typing.Callable[[float], str]"])
      | Type.Parametric
          { name = "ParametricCallableToStr"; arguments = [Single (Primitive argument)] } ->
          let callable = Format.sprintf "typing.Callable[[%s], str]" argument in
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
    let resolved_aliases ?replace_unbound_parameters_with_any:_ name =
      match aliases name with
      | Some (TypeAliasEnvironment.RawAlias.TypeAlias t) -> Some t
      | _ -> None
    in
    less_or_equal
      ~attributes
      ?is_protocol
      order
      ~left:(parse_callable ~aliases:resolved_aliases left)
      ~right:(parse_callable ~aliases:resolved_aliases right)
  in
  (* Callback protocols *)
  let parse_annotation =
    let aliases = function
      | "_T" -> Some (Type.variable "_T")
      | _ -> None
    in
    let aliases = create_type_alias_table aliases in
    let resolved_aliases ?replace_unbound_parameters_with_any:_ name =
      match aliases name with
      | Some (TypeAliasEnvironment.RawAlias.TypeAlias t) -> Some t
      | _ -> None
    in
    parse_callable ~aliases:resolved_aliases
  in
  let is_protocol annotation =
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
  let assert_less_or_equal ?(source = "") ~left ~right expected_result context =
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
      ~arguments:[Type.variable "_1"; Type.variable "_2"];
    concrete_connect
      order
      ~predecessor:"B"
      ~successor:"typing.Generic"
      ~arguments:[Type.variable "_T"];
    concrete_connect
      order
      ~predecessor:"C"
      ~successor:"typing.Generic"
      ~arguments:[Type.variable "_T"];

    concrete_connect
      order
      ~predecessor:"A"
      ~successor:"B"
      ~arguments:[Type.tuple [Type.variable "_1"; Type.variable "_2"]];
    concrete_connect
      order
      ~predecessor:"B"
      ~successor:"C"
      ~arguments:[Type.union [Type.variable "_T"; Type.float]];

    concrete_connect
      order
      ~arguments:[Type.variable "_T"]
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
      ~arguments:[Type.string; Type.object_primitive]
      ~successor:typing_mapping;
    concrete_connect_with_variance
      order
      ~arguments_with_variances:[Type.variable "_T", Invariant; Type.variable "_TCov", Covariant]
      ~predecessor:typing_mapping
      ~successor:"typing.Generic";
    insert order "dict";
    insert order "MatchesProtocol";
    insert order "DoesNotMatchProtocol";
    handler order
  in
  let assert_false value _ = assert_false value in
  let assert_true value _ = assert_true value in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true (less_or_equal default ~left:!!"list" ~right:!!"typing.Sized");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true (less_or_equal default ~left:(Type.list Type.integer) ~right:!!"typing.Sized");
      (* ReadOnly types. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (less_or_equal
              default
              ~left:(Type.PyreReadOnly.create (Type.Primitive "Child"))
              ~right:(Type.Primitive "Base"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              default
              ~left:(Type.Primitive "Child")
              ~right:(Type.PyreReadOnly.create (Type.Primitive "Base")));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              default
              ~left:(Type.PyreReadOnly.create (Type.Primitive "Child"))
              ~right:(Type.PyreReadOnly.create (Type.Primitive "Base")));
      (* Parametric types. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              default
              ~left:(Type.list Type.integer)
              ~right:(Type.iterator Type.integer));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (less_or_equal default ~left:(Type.list Type.float) ~right:(Type.iterator Type.integer));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              default
              ~left:(Type.iterator Type.integer)
              ~right:(Type.iterable Type.integer));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              default
              ~left:(Type.iterator Type.integer)
              ~right:(Type.iterable Type.float));
      (* Mixed primitive and parametric types. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true (less_or_equal default ~left:Type.string ~right:(Type.iterable Type.string));
      (* Mixed tuple and parametric types. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              default
              ~left:(Type.tuple [Type.integer; Type.integer])
              ~right:(Type.iterator Type.integer));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (less_or_equal
              default
              ~left:(Type.tuple [Type.integer; Type.float])
              ~right:(Type.iterator Type.integer));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              default
              ~left:(Type.tuple [Type.integer; Type.float])
              ~right:(Type.iterator Type.float));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              default
              ~left:(Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer))
              ~right:(Type.iterator Type.integer));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (less_or_equal
              default
              ~left:(Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.float))
              ~right:(Type.iterator Type.integer));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              default
              ~left:(Type.Primitive "tuple")
              ~right:(Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.float)));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              default
              ~left:(Type.Tuple (Concrete [Type.integer; Type.integer]))
              ~right:(Type.parametric "tuple" ![Type.integer]));
      (* Union types *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true (less_or_equal default ~left:Type.NoneType ~right:(Type.optional Type.integer));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              default
              ~left:(Type.optional Type.string)
              ~right:(Type.Union [Type.integer; Type.optional Type.string]));
      (* Tuples. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              default
              ~left:(Type.tuple [Type.integer; Type.integer])
              ~right:(Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.integer)));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              default
              ~left:(Type.tuple [Type.integer; Type.integer])
              ~right:(Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.float)));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              default
              ~left:(Type.tuple [Type.integer; Type.float])
              ~right:(Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.float)));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              order
              ~left:(Type.parametric "A" ![Type.integer; Type.string])
              ~right:(Type.parametric "B" ![Type.tuple [Type.integer; Type.string]]));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (less_or_equal
              order
              ~left:(Type.parametric "A" ![Type.integer; Type.string])
              ~right:(Type.tuple [Type.integer; Type.string]));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              order
              ~left:(Type.parametric "A" ![Type.integer; Type.string])
              ~right:
                (Type.parametric
                   "C"
                   ![Type.union [Type.tuple [Type.integer; Type.string]; Type.float]]));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (less_or_equal
              order
              ~left:(Type.parametric "A" ![Type.string; Type.integer])
              ~right:
                (Type.parametric
                   "C"
                   ![Type.union [Type.tuple [Type.integer; Type.string]; Type.float]]));
      (* Variables. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true (less_or_equal order ~left:(Type.variable "T") ~right:Type.Any);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false (less_or_equal order ~left:(Type.variable "T") ~right:Type.integer);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true (less_or_equal order ~left:Type.Any ~right:(Type.variable "T"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false (less_or_equal order ~left:Type.integer ~right:(Type.variable "T"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              order
              ~left:(Type.variable "T")
              ~right:(Type.union [Type.string; Type.variable "T"]));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (less_or_equal
              order
              ~left:Type.integer
              ~right:
                (Type.variable
                   ~constraints:(Type.Record.TypeVarConstraints.Explicit [Type.float; Type.integer])
                   "T"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              order
              ~left:
                (Type.variable
                   ~constraints:(Type.Record.TypeVarConstraints.Explicit [Type.float; Type.string])
                   "T")
              ~right:(Type.union [Type.float; Type.string]));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              order
              ~left:
                (Type.variable
                   ~constraints:(Type.Record.TypeVarConstraints.Explicit [Type.float; Type.string])
                   "T")
              ~right:(Type.union [Type.float; Type.string; !!"A"]));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (less_or_equal
              order
              ~left:
                (Type.variable
                   ~constraints:(Type.Record.TypeVarConstraints.Explicit [Type.float; Type.string])
                   "T")
              ~right:(Type.union [Type.float]));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              order
              ~left:
                (Type.variable
                   ~constraints:
                     (Type.Record.TypeVarConstraints.Bound (Type.union [Type.float; Type.string]))
                   "T")
              ~right:(Type.union [Type.float; Type.string; !!"A"]));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (less_or_equal
              order
              ~left:Type.string
              ~right:
                (Type.variable ~constraints:(Type.Record.TypeVarConstraints.Bound Type.string) "T"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              order
              ~left:float_string_variable
              ~right:(Type.union [float_string_variable; !!"A"]));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              order
              ~left:(Type.variable ~constraints:(Type.Record.TypeVarConstraints.Bound !!"A") "T")
              ~right:
                (Type.union
                   [
                     Type.variable ~constraints:(Type.Record.TypeVarConstraints.Bound !!"A") "T";
                     Type.string;
                   ]));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              order
              ~left:(Type.variable ~constraints:(Type.Record.TypeVarConstraints.Bound !!"A") "T")
              ~right:
                (Type.optional
                   (Type.variable ~constraints:(Type.Record.TypeVarConstraints.Bound !!"A") "T")));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              order
              ~left:
                (Type.variable
                   ~constraints:(Type.Record.TypeVarConstraints.Bound (Type.optional !!"A"))
                   "T")
              ~right:(Type.optional !!"A"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              order
              ~left:
                (Type.variable ~constraints:(Type.Record.TypeVarConstraints.Bound Type.integer) "T")
              ~right:(Type.union [Type.float; Type.string]));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              order
              ~left:
                (Type.variable ~constraints:(Type.Record.TypeVarConstraints.Bound Type.integer) "T")
              ~right:
                (Type.union
                   [
                     Type.variable
                       ~constraints:(Type.Record.TypeVarConstraints.Bound Type.integer)
                       "T";
                     Type.string;
                   ]));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              order
              ~left:(Type.variable ~constraints:Type.Record.TypeVarConstraints.Unconstrained "T")
              ~right:Type.Top);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              order
              ~left:(Type.variable ~constraints:Type.Record.TypeVarConstraints.Unconstrained "T")
              ~right:
                (Type.union
                   [
                     Type.variable ~constraints:Type.Record.TypeVarConstraints.Unconstrained "T";
                     Type.string;
                   ]));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              order
              ~left:
                (Type.variable
                   ~constraints:
                     (Type.Record.TypeVarConstraints.Bound (Type.union [Type.float; Type.string]))
                   "T")
              ~right:(Type.union [Type.float; Type.string]));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (less_or_equal
              order
              ~left:Type.integer
              ~right:
                (Type.variable ~constraints:(Type.Record.TypeVarConstraints.Bound Type.float) "T"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (less_or_equal
              order
              ~left:Type.float
              ~right:
                (Type.variable ~constraints:(Type.Record.TypeVarConstraints.Bound Type.integer) "T"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (less_or_equal
              order
              ~left:(Type.union [Type.string; Type.integer])
              ~right:
                (Type.variable
                   ~constraints:
                     (Type.Record.TypeVarConstraints.Explicit [Type.string; Type.integer])
                   "T"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (less_or_equal
              order
              ~left:Type.integer
              ~right:
                (Type.variable
                   ~constraints:(Type.Record.TypeVarConstraints.Explicit [Type.string])
                   "T"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (less_or_equal
              order
              ~left:Type.integer
              ~right:(Type.variable ~constraints:Type.Record.TypeVarConstraints.LiteralIntegers "T"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              order
              ~left:(Type.variable ~constraints:Type.Record.TypeVarConstraints.LiteralIntegers "T")
              ~right:Type.integer);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[int], int]"
              ~right:"typing.Callable[[int], int]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[str], int]"
              ~right:"typing.Callable[[int], int]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[int], int]"
              ~right:"typing.Callable[[str], int]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[int], str]"
              ~right:"typing.Callable[[int], int]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[int], float]"
              ~right:"typing.Callable[[int], int]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[int], int]"
              ~right:"typing.Callable[[int], float]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[float], int]"
              ~right:"typing.Callable[[int], int]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[int], int]"
              ~right:"typing.Callable[[float], int]");
      (* Named vs. anonymous callables. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[int], int]"
              ~right:"typing.Callable('foo')[[int], int]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[str], int]"
              ~right:"typing.Callable('foo')[[int], int]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"typing.Callable('foo')[[int], int]"
              ~right:"typing.Callable[[int], int]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~left:"typing.Callable('foo')[[str], int]"
              ~right:"typing.Callable[[int], int]");
      (* Named callables. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"typing.Callable('foo')[[int], int]"
              ~right:"typing.Callable('foo')[[int], int]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~left:"typing.Callable('bar')[[str], int]"
              ~right:"typing.Callable('foo')[[int], int]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~left:"typing.Callable('foo')[[int], int]"
              ~right:"typing.Callable('bar')[[int], int]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"typing.Callable('foo')[[str], int]"
              ~right:"typing.Callable('foo')[[int], int]");
      (* Callables with keyword-only parameters. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[Named(foo, bool, default)], int]"
              ~right:"typing.Callable[[KeywordOnly(foo, bool, default)], int]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[Named(foo, bool, default)], int]"
              ~right:"typing.Callable[[KeywordOnly(foo, bool)], int]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[Named(foo, bool)], int]"
              ~right:"typing.Callable[[KeywordOnly(foo, bool)], int]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[Named(foo, bool)], int]"
              ~right:"typing.Callable[[KeywordOnly(foo, bool, default)], int]");
      (* Undefined callables. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"typing.Callable[..., int]"
              ~right:"typing.Callable[..., float]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[int], int]"
              ~right:"typing.Callable[..., int]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"typing.Callable[..., int]"
              ~right:"typing.Callable[[int], float]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[Variable(object), Keywords(object)], int]"
              ~right:"typing.Callable[..., int]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[Variable(int), Keywords(object)], int]"
              ~right:"typing.Callable[..., int]");
      (* Callable classes. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"FloatToStrCallable"
              ~right:"typing.Callable[[float], str]");
      (* Subtyping is handled properly for callable classes. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"FloatToStrCallable"
              ~right:"typing.Callable[[int], str]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~left:"FloatToStrCallable"
              ~right:"typing.Callable[[float], int]");
      (* Parametric classes are also callables. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"ParametricCallableToStr[int]"
              ~right:"typing.Callable[[int], str]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"ParametricCallableToStr[float]"
              ~right:"typing.Callable[[int], str]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~left:"ParametricCallableToStr[int]"
              ~right:"typing.Callable[[float], str]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~left:"ParametricCallableToStr[int]"
              ~right:"typing.Callable[[int], int]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[Variable(int)], str]"
              ~right:"typing.Callable[[int], str]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[Variable(int)], str]"
              ~right:"typing.Callable[[int, int], str]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[Variable(str)], str]"
              ~right:"typing.Callable[[int], str]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[Variable(int)], str]"
              ~right:"typing.Callable[[Named(arg, int)], str]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[Variable(int), Keywords(int)], int]"
              ~right:"typing.Callable[[Keywords(int)], int]");
      (* Callables with keyword arguments. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[Keywords(int)], str]"
              ~right:"typing.Callable[[Named(arg, int)], str]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[Variable(int), Keywords(int)], str]"
              ~right:"typing.Callable[[Named(arg, int)], str]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[Variable(str), Keywords(int)], str]"
              ~right:"typing.Callable[[Named(arg, int)], str]");
      (* Generic callables *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[Named(arg, T_Unconstrained)], T_Unconstrained]"
              ~right:"typing.Callable[[Named(arg, int)], str]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[Named(arg, T_Unconstrained)], T_Unconstrained]"
              ~right:"typing.Callable[[Named(arg, int)], int]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~right:"typing.Callable[[Named(arg, int)], str]"
              ~left:"typing.Callable[[T_Unconstrained], T_Unconstrained]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[Named(arg, T_int_bool)], T_int_bool]"
              ~right:"typing.Callable[[Named(arg, int)], int]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[Named(arg, T_int_bool)], T_int_bool]"
              ~right:"typing.Callable[[Named(arg, str)], str]");
      (* Callables with overloads *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[object], object][[[str], int][[int], str]]"
              ~right:"typing.Callable[[object], object][[[int], str]]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~left:"typing.Callable[[object], object][[[str], int][[int], str]]"
              ~right:"typing.Callable[[object], object][[[str], str]]");
      (* Literals *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"typing_extensions.Literal['a']"
              ~right:"typing_extensions.Literal['a']");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true (parse_less_or_equal order ~left:"typing_extensions.Literal['a']" ~right:"str");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal order ~left:"str" ~right:"typing_extensions.Literal['a']");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~left:"typing_extensions.Literal['a']"
              ~right:"typing_extensions.Literal['a', 'b']");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~is_protocol
              ~attributes
              ~left:"typing.Callable[[int], str]"
              ~right:"MatchesProtocol");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~is_protocol
              ~attributes
              ~left:"typing.Callable[[int], str]"
              ~right:"DoesNotMatchProtocol");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (parse_less_or_equal
              order
              ~is_protocol
              ~attributes
              ~left:"typing.Callable[[int], str]"
              ~right:"B[int]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (parse_less_or_equal
              order
              ~is_protocol
              ~attributes
              ~left:"typing.Callable[[int], str]"
              ~right:"B[str]");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_less_or_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_less_or_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_less_or_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_less_or_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_less_or_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_less_or_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_less_or_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_less_or_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_less_or_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_less_or_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_less_or_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_less_or_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_less_or_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_less_or_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_less_or_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_less_or_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_less_or_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_less_or_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_less_or_equal
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_less_or_equal
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
    ]


let test_less_or_equal_variance =
  let assert_strict_less ~order ~right ~left _ =
    assert_equal
      ~printer:(fun pair -> Format.sprintf "%B, %B" (fst pair) (snd pair))
      (true, false)
      (less_or_equal order ~left ~right, less_or_equal order ~left:right ~right:left)
  in
  (* TODO (T45909999): Revisit these tests and only keep the useful ones *)
  let _obsolete_tests context =
    (* More complex rules. *)
    assert_strict_less
      ~order:variance_order
      ~left:(Type.parametric "Derived" ![Type.integer])
      ~right:(Type.parametric "Derived" ![Type.float])
      context;
    assert_strict_less
      ~order:variance_order
      ~left:(Type.parametric "Derived" ![Type.integer])
      ~right:(Type.parametric "Base" ![Type.integer])
      context;
    assert_strict_less
      ~order:variance_order
      ~left:(Type.parametric "Derived" ![Type.float])
      ~right:(Type.parametric "Base" ![Type.float])
      context;
    assert_strict_less
      ~order:variance_order
      ~left:(Type.parametric "Base" ![Type.float])
      ~right:(Type.parametric "Base" ![Type.integer])
      context;
    assert_strict_less
      ~order:variance_order
      ~left:(Type.parametric "Derived" ![Type.integer])
      ~right:(Type.parametric "Base" ![Type.float])
      context;
    assert_strict_less
      ~order:variance_order
      ~left:(Type.parametric "Derived" ![Type.float])
      ~right:(Type.parametric "Base" ![Type.integer])
      context;

    (* Multiplane variance. *)
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "A" ![Type.integer; Type.float])
      ~right:(Type.parametric "A" ![Type.float; Type.integer])
      context;
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "B" ![Type.float; Type.integer])
      ~right:(Type.parametric "B" ![Type.integer; Type.float])
      context;
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "B" ![Type.integer; Type.integer])
      ~right:(Type.parametric "A" ![Type.integer; Type.integer])
      context;
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "B" ![Type.integer; Type.integer])
      ~right:(Type.parametric "A" ![Type.integer; Type.float])
      context;
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "B" ![Type.integer; Type.integer])
      ~right:(Type.parametric "A" ![Type.float; Type.float])
      context;
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "B" ![Type.float; Type.float])
      ~right:(Type.parametric "A" ![Type.integer; Type.integer])
      context;
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "B" ![Type.float; Type.float])
      ~right:(Type.parametric "A" ![Type.integer; Type.float])
      context;
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "B" ![Type.float; Type.float])
      ~right:(Type.parametric "A" ![Type.float; Type.float])
      context;
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "B" ![Type.float; Type.integer])
      ~right:(Type.parametric "A" ![Type.integer; Type.integer])
      context;
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "B" ![Type.float; Type.integer])
      ~right:(Type.parametric "A" ![Type.integer; Type.float])
      context;
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "B" ![Type.float; Type.integer])
      ~right:(Type.parametric "A" ![Type.float; Type.float])
      context;
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "B" ![Type.float; Type.integer])
      ~right:(Type.parametric "A" ![Type.float; Type.integer])
      context;
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "C" ![])
      ~right:(Type.parametric "A" ![Type.float; Type.float])
      context;
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:!!"C"
      ~right:(Type.parametric "A" ![Type.float; Type.float])
      context;
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:(Type.parametric "D" ![])
      ~right:(Type.parametric "A" ![Type.integer; Type.integer])
      context;
    assert_strict_less
      ~order:multiplane_variance_order
      ~left:!!"D"
      ~right:(Type.parametric "A" ![Type.integer; Type.integer])
      context;
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
  let assert_false value _ = assert_false value in
  let assert_true value _ = assert_true value in
  test_list
    [
      (* Invariant. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (less_or_equal
              variance_order
              ~left:(Type.parametric "LinkedList" ![Type.integer])
              ~right:(Type.parametric "LinkedList" ![Type.float]));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_false
           (less_or_equal
              variance_order
              ~left:(Type.parametric "LinkedList" ![Type.float])
              ~right:(Type.parametric "LinkedList" ![Type.integer]));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              variance_order
              ~left:(Type.parametric "LinkedList" ![Type.integer])
              ~right:(Type.parametric "LinkedList" ![Type.Any]));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              variance_order
              ~left:(Type.parametric "LinkedList" ![Type.Any])
              ~right:(Type.parametric "LinkedList" ![Type.integer]));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_less
           ~order:variance_order
           ~left:(Type.parametric "LinkedList" ![Type.integer])
           ~right:(Type.parametric "LinkedList" ![Type.Top]);
      (* Covariant. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_less
           ~order:variance_order
           ~left:(Type.parametric "Box" ![Type.integer])
           ~right:(Type.parametric "Box" ![Type.float]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              variance_order
              ~left:(Type.parametric "Box" ![Type.integer])
              ~right:(Type.parametric "Box" ![Type.Any]));
      (* Contravariant. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_strict_less
           ~order:variance_order
           ~left:(Type.parametric "Sink" ![Type.float])
           ~right:(Type.parametric "Sink" ![Type.integer]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_true
           (less_or_equal
              variance_order
              ~left:(Type.parametric "Sink" ![Type.Any])
              ~right:(Type.parametric "Sink" ![Type.integer]));
    ]


let test_join =
  let assert_join ?(order = default) ?(aliases = Type.resolved_empty_aliases) left right expected _ =
    let parse_annotation = function
      | "$bottom" -> Type.Bottom
      | _ as source ->
          parse_single_expression source |> Type.create ~variables:variable_aliases ~aliases
    in
    let attributes annotation ~cycle_detections:_ =
      let parse_annotation =
        let aliases = function
          | "_T" -> Some (Type.variable "_T")
          | _ -> None
        in
        let aliases = create_type_alias_table aliases in
        let resolved_aliases ?replace_unbound_parameters_with_any:_ name =
          match aliases name with
          | Some (TypeAliasEnvironment.RawAlias.TypeAlias t) -> Some t
          | _ -> None
        in
        parse_callable ~aliases:resolved_aliases
      in
      match annotation with
      | Type.Primitive "CallableClass" ->
          Some
            (parse_attributes
               ~parse_annotation
               ~class_name:"MatchesProtocol"
               ["__call__", "typing.Callable[[int], str]"])
      | Type.Parametric
          { name = "ParametricCallableToStr"; arguments = [Single (Primitive argument)] } ->
          let callable = Format.sprintf "typing.Callable[[%s], str]" argument in
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
    (* Test that `join` is commutative. *)
    assert_type_equal
      (parse_annotation expected)
      (join ~attributes order (parse_annotation right) (parse_annotation left))
  in
  let variance_aliases =
    Identifier.Table.of_alist_exn
      [
        "_T", Type.variable "_T";
        "_T_co", Type.variable "_T_co";
        "_T_contra", Type.variable "_T_contra";
      ]
    |> Hashtbl.find
  in
  (* TODO (T45909999): Revisit these tests and only keep the useful ones *)
  let _obsolete_variance_tests context =
    let variance_aliases = create_type_alias_table variance_aliases in
    let variance_aliases ?replace_unbound_parameters_with_any:_ name =
      match variance_aliases name with
      | Some (TypeAliasEnvironment.RawAlias.TypeAlias t) -> Some t
      | _ -> None
    in
    assert_join
      ~order:variance_order
      ~aliases:variance_aliases
      "Derived[int]"
      "Base[int]"
      "Base[int]"
      context;
    assert_join
      ~order:variance_order
      ~aliases:variance_aliases
      "Derived[float]"
      "Base[float]"
      "Base[float]"
      context;
    assert_join
      ~order:variance_order
      ~aliases:variance_aliases
      "Derived[int]"
      "Base[float]"
      "Base[float]"
      context;
    assert_join
      ~order:variance_order
      ~aliases:variance_aliases
      "Derived[float]"
      "Base[int]"
      "Base[int]"
      context;
    assert_join
      ~order:multiplane_variance_order
      ~aliases:variance_aliases
      "B[int, float]"
      "A[int, float]"
      "A[int, float]"
      context;
    assert_join
      ~order:multiplane_variance_order
      ~aliases:variance_aliases
      "B[int, int]"
      "A[int, float]"
      "A[int, float]"
      context;
    assert_join
      ~order:multiplane_variance_order
      ~aliases:variance_aliases
      "B[float, int]"
      "A[int, float]"
      "A[int, float]"
      context;
    assert_join
      ~order:multiplane_variance_order
      ~aliases:variance_aliases
      "B[float, float]"
      "A[int, float]"
      "A[int, float]"
      context;
    assert_join
      ~order:multiplane_variance_order
      ~aliases:variance_aliases
      "B[int, float]"
      "A[float, float]"
      "A[float, float]"
      context;
    assert_join
      ~order:multiplane_variance_order
      ~aliases:variance_aliases
      "B[int, int]"
      "A[float, float]"
      "A[float, float]"
      context;
    assert_join
      ~order:multiplane_variance_order
      ~aliases:variance_aliases
      "B[float, int]"
      "A[float, float]"
      "A[float, float]"
      context;
    assert_join
      ~order:multiplane_variance_order
      ~aliases:variance_aliases
      "B[float, float]"
      "A[float, float]"
      "A[float, float]"
      context;
    assert_join
      ~order:parallel_planes_variance_order
      ~aliases:variance_aliases
      "B[float, float]"
      "A[int, float]"
      "A[float, float]"
      context;
    assert_join
      ~order:parallel_planes_variance_order
      ~aliases:variance_aliases
      "B[float, float]"
      "A[int, int]"
      "A[float, int]"
      context
  in
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
      ~arguments:[Type.tuple [Type.variable "_1"; Type.variable "_2"]];
    concrete_connect
      order
      ~predecessor:"A"
      ~successor:"typing.Generic"
      ~arguments:[Type.variable "_1"; Type.variable "_2"];
    concrete_connect
      order
      ~predecessor:"B"
      ~successor:"typing.Generic"
      ~arguments:[Type.variable "_T"];
    concrete_connect
      order
      ~predecessor:"B"
      ~successor:"C"
      ~arguments:[Type.union [Type.variable "_T"; Type.float]];
    concrete_connect
      order
      ~predecessor:"C"
      ~successor:"typing.Generic"
      ~arguments:[Type.variable "_T"];

    concrete_connect
      order
      ~arguments:[Type.variable "_T"]
      ~predecessor:"ParametricCallableToStr"
      ~successor:"typing.Generic";
    handler order
  in
  let assert_type_equal left right _ = assert_type_equal left right in
  let assert_join_type_equal order left right expected =
    assert_type_equal (join order left right) expected
  in
  test_list
    [
      (* Primitive types. *)
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_join "list" "typing.Sized" "typing.Sized";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_join "typing.Sized" "list" "typing.Sized";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join "typing.List[int]" "typing.Sized" "typing.Sized";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_join "int" "str" "typing.Union[int, str]";
      (* Parametric types. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join "typing.List[float]" "typing.List[float]" "typing.List[float]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           "typing.List[float]"
           "typing.List[int]"
           "typing.Union[typing.List[float], typing.List[int]]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join "typing.List[int]" "typing.Iterator[int]" "typing.Iterator[int]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join "typing.Iterator[int]" "typing.List[int]" "typing.Iterator[int]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join "typing.List[float]" "typing.Iterator[int]" "typing.Iterator[float]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           "typing.List[float]"
           "float[int]"
           "typing.Union[typing.List[float], float[int]]";
      (* Annotated types. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join "typing.Annotated[int, 1]" "float" "typing.Annotated[float, 1]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           "typing.Annotated[int, 1]"
           "typing.Annotated[float, 1]"
           "typing.Annotated[float, 1]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           "typing_extensions.Annotated[int, 1]"
           "float"
           "typing_extensions.Annotated[float, 1]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           "typing_extensions.Annotated[int, 1]"
           "typing_extensions.Annotated[float, 1]"
           "typing_extensions.Annotated[float, 1]";
      (* TODO(T41082573) throw here instead of unioning *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join "typing.Tuple[int, int]" "typing.Iterator[int]" "typing.Iterator[int]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           ~aliases:Type.resolved_empty_aliases
           "typing.Tuple[typing.Unpack[Ts]]"
           "typing.Tuple[int, ...]"
           "typing.Tuple[object, ...]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           ~aliases:Type.resolved_empty_aliases
           "typing.Tuple[pyre_extensions.Unpack[Ts]]"
           "typing.Tuple[int, ...]"
           "typing.Tuple[object, ...]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           ~aliases:Type.resolved_empty_aliases
           "typing.Tuple[typing_extensions.Unpack[Ts]]"
           "typing.Tuple[int, ...]"
           "typing.Tuple[object, ...]";
      (* Optionals. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join "str" "typing.Optional[str]" "typing.Optional[str]";
      (* Handles `[] or optional_list`. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join "typing.List[int]" "typing.List[typing.Any]" "typing.List[typing.Any]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join "typing.List[typing.Any]" "typing.List[int]" "typing.List[typing.Any]";
      (* Union types. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           "typing.Optional[bool]"
           "typing.Union[int, typing.Optional[bool]]"
           "typing.Union[int, typing.Optional[bool]]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           "typing.Union[int, str]"
           "typing.Union[int, bytes]"
           "typing.Union[int, str, bytes]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join "typing.Union[int, str]" "None" "typing.Union[int, str, None]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           "typing.Dict[str, str]"
           "typing.Dict[str, typing.List[str]]"
           "typing.Union[typing.Dict[str, typing.List[str]], typing.Dict[str, str]]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join "typing.Union[typing.List[int], typing.Set[int]]" "typing.Sized" "typing.Sized";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join "typing.Tuple[int, ...]" "typing.Iterable[int]" "typing.Iterable[int]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join "typing.Tuple[str, ...]" "typing.Iterator[str]" "typing.Iterator[str]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           "typing.Tuple[int, ...]"
           "typing.Iterable[str]"
           "typing.Iterable[typing.Union[int, str]]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           "typing.Optional[float]"
           "typing.Union[float, int]"
           "typing.Optional[typing.Union[float, int]]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           "typing.List[typing.Any]"
           "typing.Union[typing.List[int], typing.List[str]]"
           "typing.List[typing.Any]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           "typing.Tuple[int, int]"
           "typing.Tuple[int, int, str]"
           "typing.Union[typing.Tuple[int, int], typing.Tuple[int, int, str]]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join ~order:disconnected_order "A" "B" "typing.Union[A, B]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join "typing.Type[int]" "typing.Type[str]" "typing.Type[typing.Union[int, str]]";
      (* Callables. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           "typing.Callable[..., int]"
           "typing.Callable[..., str]"
           "typing.Callable[..., typing.Union[int, str]]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           "typing.Callable('derp')[..., int]"
           "typing.Callable('derp')[..., int]"
           "typing.Callable('derp')[..., int]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           "typing.Callable('derp')[..., int]"
           "typing.Callable('other')[..., int]"
           "typing.Union[typing.Callable(derp)[..., int], typing.Callable(other)[..., int]]";
      (* Do not join with overloads. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           "typing.Callable[..., int][[..., str]]"
           "typing.Callable[..., int]"
           "typing.Union[typing.Callable[..., int][[..., str]], typing.Callable[..., int]]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           "typing.Callable[[Named(a, int), Named(b, str)], int]"
           "typing.Callable[[Named(a, int), Named(b, str)], int]"
           "typing.Callable[[Named(a, int), Named(b, str)], int]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           "typing.Callable[[Named(a, int)], int]"
           "typing.Callable[[int], int]"
           "typing.Callable[[int], int]";
      (* Behavioral subtyping is preserved. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           "typing.Callable[[int], int]"
           "typing.Callable[[Named(a, int)], int]"
           "typing.Callable[[int], int]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           "typing.Callable[[Named(b, int)], int]"
           "typing.Callable[[Named(a, int)], int]"
           "typing.Union[typing.Callable[[Named(b, int)], int], typing.Callable[[Named(a, int)], \
            int]]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           "typing.Callable[..., typing.Any]"
           "typing.Callable[[int], int]"
           "typing.Callable[[int], typing.Any]";
      (* Classes with __call__ are callables. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           ~order
           "CallableClass"
           "typing.Callable[[int], str]"
           "typing.Callable[[int], str]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           ~order
           "typing.Callable[[int], str]"
           "CallableClass"
           "typing.Callable[[int], str]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           ~order
           "typing.Callable[[int], int]"
           "CallableClass"
           "typing.Callable[[int], typing.Union[int, str]]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           ~order
           "ParametricCallableToStr[int]"
           "typing.Callable[[int], str]"
           "typing.Callable[[int], str]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           ~order
           "typing.Callable[[int], str]"
           "ParametricCallableToStr[int]"
           "typing.Callable[[int], str]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           ~order
           "typing.Callable[[int], int]"
           "ParametricCallableToStr[int]"
           "typing.Callable[[int], typing.Union[int, str]]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           ~order
           "ParametricCallableToStr[int]"
           "typing.Callable[[int], str]"
           "typing.Callable[[int], str]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           ~order
           "typing.Callable[[float], str]"
           "ParametricCallableToStr[int]"
           "typing.Callable[[int], str]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           ~order
           "typing.Callable[[int], str]"
           "ParametricCallableToStr[float]"
           "typing.Callable[[int], str]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           ~order
           "typing.Callable[[int], int]"
           "ParametricCallableToStr[int]"
           "typing.Callable[[int], typing.Union[int, str]]";
      (* We just preserve the `ReadOnly` wrapper if either type has it. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join "pyre_extensions.ReadOnly[Child]" "Base" "pyre_extensions.ReadOnly[Base]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           "pyre_extensions.ReadOnly[Child]"
           "pyre_extensions.ReadOnly[Base]"
           "pyre_extensions.ReadOnly[Base]";
      (* Variables. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           order
           Type.integer
           (Type.variable "T")
           (Type.union [Type.integer; Type.variable "T"]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           order
           Type.integer
           (Type.variable ~constraints:(Type.Record.TypeVarConstraints.Bound Type.string) "T")
           (Type.union
              [
                Type.integer;
                Type.variable ~constraints:(Type.Record.TypeVarConstraints.Bound Type.string) "T";
              ]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           order
           Type.string
           (Type.variable
              ~constraints:(Type.Record.TypeVarConstraints.Explicit [Type.float; Type.integer])
              "T")
           (Type.union
              [
                Type.string;
                Type.variable
                  ~constraints:(Type.Record.TypeVarConstraints.Explicit [Type.float; Type.integer])
                  "T";
              ]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           order
           Type.string
           (Type.variable ~constraints:Type.Record.TypeVarConstraints.LiteralIntegers "T")
           (Type.union
              [
                Type.string;
                Type.variable ~constraints:Type.Record.TypeVarConstraints.LiteralIntegers "T";
              ]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           order
           (Type.literal_integer 7)
           (Type.variable ~constraints:Type.Record.TypeVarConstraints.LiteralIntegers "T")
           (Type.union
              [
                Type.literal_integer 7;
                Type.variable ~constraints:Type.Record.TypeVarConstraints.LiteralIntegers "T";
              ]);
      (* Variance. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           variance_order
           (Type.parametric "LinkedList" ![Type.integer])
           (Type.parametric "LinkedList" ![Type.Top])
           (Type.parametric "LinkedList" ![Type.Top]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           variance_order
           (Type.parametric "LinkedList" ![Type.Top])
           (Type.parametric "LinkedList" ![Type.integer])
           (Type.parametric "LinkedList" ![Type.Top]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           variance_order
           (Type.parametric "LinkedList" ![Type.Bottom])
           (Type.parametric "LinkedList" ![Type.Top])
           (Type.parametric "LinkedList" ![Type.Top]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           variance_order
           (Type.parametric "LinkedList" ![Type.Top])
           (Type.parametric "LinkedList" ![Type.Bottom])
           (Type.parametric "LinkedList" ![Type.Top]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           variance_order
           (Type.parametric "LinkedList" ![Type.Any])
           (Type.parametric "LinkedList" ![Type.Top])
           (Type.parametric "LinkedList" ![Type.Top]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           variance_order
           (Type.parametric "LinkedList" ![Type.Top])
           (Type.parametric "LinkedList" ![Type.Any])
           (Type.parametric "LinkedList" ![Type.Top]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           variance_order
           (Type.parametric "LinkedList" ![Type.Top])
           (Type.parametric "LinkedList" ![Type.Top])
           (Type.parametric "LinkedList" ![Type.Top]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           variance_order
           (Type.parametric "Map" ![Type.integer; Type.integer])
           (Type.parametric "Map" ![Type.Top; Type.Top])
           (Type.parametric "Map" ![Type.Top; Type.Top]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           variance_order
           (Type.parametric "Map" ![Type.integer; Type.integer])
           (Type.parametric "Map" ![Type.Top; Type.integer])
           (Type.parametric "Map" ![Type.Top; Type.integer]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           variance_order
           (Type.parametric "Map" ![Type.integer; Type.integer])
           (Type.parametric "Map" ![Type.Top; Type.string])
           (Type.union
              [
                Type.parametric "Map" ![Type.integer; Type.integer];
                Type.parametric "Map" ![Type.Top; Type.string];
              ]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           variance_order
           (Type.parametric "LinkedList" ![Type.integer])
           (Type.parametric "LinkedList" ![Type.Any])
           (Type.parametric "LinkedList" ![Type.Any]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           variance_order
           (Type.parametric "LinkedList" ![Type.Any])
           (Type.parametric "LinkedList" ![Type.integer])
           (Type.parametric "LinkedList" ![Type.Any]);
      (* Contravariant *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           variance_order
           (Type.parametric "Sink" ![Type.integer])
           (Type.parametric "Sink" ![Type.string])
           (Type.Union
              [Type.parametric "Sink" ![Type.integer]; Type.parametric "Sink" ![Type.string]]);
      (* Literals *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           order
           (Type.literal_string "A")
           (Type.literal_string "A")
           (Type.literal_string "A");
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           order
           (Type.literal_string "A")
           (Type.literal_string "B")
           (Type.Literal (String AnyLiteral));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_equal (join order (Type.literal_string "A") Type.string) Type.string;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           order
           (Type.literal_string "A")
           Type.integer
           (Type.union [Type.string; Type.integer]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           order
           (Type.Literal (String AnyLiteral))
           (Type.Literal (String AnyLiteral))
           (Type.Literal (String AnyLiteral));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_type_equal
           order
           (Type.Literal (String AnyLiteral))
           (Type.literal_string "hello")
           (Type.Literal (String AnyLiteral));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_equal (join order (Type.Literal (String AnyLiteral)) Type.string) Type.string;
    ]


(* Test using the type order created by `GlobalResolution`. *)
let test_join_with_full_type_order =
  let parse_annotation ~resolution annotation =
    annotation |> parse_single_expression |> GlobalResolution.parse_annotation resolution
  in
  let assert_join_direct ~source ~left ~right expected_annotation context =
    let resolution = resolution ~source context in
    let left, right = parse_annotation ~resolution left, parse_annotation ~resolution right in
    assert_type_equal expected_annotation (GlobalResolution.join resolution left right)
  in
  let assert_join ?(source = "") ~left ~right expected_result context =
    let resolution = resolution ~source context in
    assert_join_direct ~source ~left ~right (parse_annotation ~resolution expected_result) context
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
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
    ]


let test_join_recursive_types =
  let parse_annotation ~resolution annotation =
    annotation |> parse_single_expression |> GlobalResolution.parse_annotation resolution
  in
  let assert_join_direct ~source ~left ~right make_expected_annotation context =
    Type.RecursiveType.Namespace.reset ();
    let fresh_name = Type.RecursiveType.Namespace.create_fresh_name () in
    let expected_annotation = make_expected_annotation fresh_name in
    let resolution = resolution ~source context in
    let left, right = parse_annotation ~resolution left, parse_annotation ~resolution right in
    (* Note: It's not enough to naively check `Type.equal` or even check if they are
       alpha-equivalent (same body but different recursive type names). That would miss the cases
       where one of the recursive types happens to be unrolled a few more times than the other, even
       though both are equivalent to each other.

       So, we need to use `less-or-equal` both ways. *)
    let are_recursive_types_equivalent left right =
      GlobalResolution.less_or_equal resolution ~left ~right
      && GlobalResolution.less_or_equal resolution ~left:right ~right:left
    in
    assert_equal
      ~printer:Type.show
      ~cmp:are_recursive_types_equivalent
      expected_annotation
      (GlobalResolution.join resolution left right);
    (* Test that `join` is commutative. *)
    assert_equal
      ~printer:Type.show
      ~cmp:are_recursive_types_equivalent
      expected_annotation
      (GlobalResolution.join resolution right left)
  in
  let assert_join ?(source = "") ~left ~right expected_result context =
    let resolution = resolution ~source context in
    assert_join_direct
      ~source
      ~left
      ~right
      (fun _ -> parse_annotation ~resolution expected_result)
      context
  in
  let recursive_alias_source =
    {|
      from typing import Tuple, Union

      Tree = Union[int, Tuple["Tree", "Tree"]]
      Tree2 = Union[int, Tuple["Tree2", "Tree2"]]
      TreeWithStr = Union[str, Tuple["TreeWithStr", "TreeWithStr"]]
      TreeWithStrAndInt = Union[str, int, Tuple["TreeWithStrAndInt", "TreeWithStrAndInt"]]
    |}
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join ~source:recursive_alias_source ~left:"test.Tree" ~right:"test.Tree" "test.Tree";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join ~source:recursive_alias_source ~left:"test.Tree" ~right:"int" "test.Tree";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join ~source:recursive_alias_source ~left:"int" ~right:"test.Tree" "test.Tree";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join
           ~source:recursive_alias_source
           ~left:"test.Tree"
           ~right:"str"
           "typing.Union[test.Tree, str]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_direct
           ~source:recursive_alias_source
           ~left:"test.Tree"
           ~right:"test.Tree2"
           (fun fresh_name ->
             Type.RecursiveType.create
               ~name:fresh_name
               ~body:
                 (Type.union
                    [
                      Type.integer; Type.tuple [Type.Primitive fresh_name; Type.Primitive fresh_name];
                    ]));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_direct
           ~source:recursive_alias_source
           ~left:"test.Tree"
           ~right:"test.TreeWithStrAndInt"
           (fun fresh_name ->
             Type.RecursiveType.create
               ~name:fresh_name
               ~body:
                 (Type.union
                    [
                      Type.integer;
                      Type.string;
                      Type.tuple [Type.Primitive fresh_name; Type.Primitive fresh_name];
                    ]));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_join_direct
           ~source:recursive_alias_source
           ~left:"test.Tree"
           ~right:"test.TreeWithStr"
           (fun fresh_name ->
             Type.RecursiveType.create
               ~name:fresh_name
               ~body:
                 (Type.union
                    [
                      Type.integer;
                      Type.string;
                      Type.tuple [Type.Primitive fresh_name; Type.Primitive fresh_name];
                    ]));
    ]


let test_meet =
  let assert_meet ?(order = default) ?(aliases = Type.resolved_empty_aliases) left right expected _ =
    let parse_annotation = function
      | "$bottom" -> Type.Bottom
      | _ as source ->
          parse_single_expression source |> Type.create ~variables:variable_aliases ~aliases
    in
    assert_type_equal
      (parse_annotation expected)
      (meet order (parse_annotation left) (parse_annotation right));
    (* Test that `meet` is commutative. *)
    assert_type_equal
      (parse_annotation expected)
      (meet order (parse_annotation right) (parse_annotation left))
  in
  let assert_type_equal left right _ = assert_type_equal left right in
  (* TODO (T45909999): Revisit these tests and only keep the useful ones *)
  let _obsolete_tests () context =
    assert_meet ~order:variance_order "Derived[int]" "Base[int]" "Derived[int]" context;
    assert_meet ~order:variance_order "Derived[float]" "Base[float]" "Derived[float]" context;
    assert_meet ~order:variance_order "Derived[int]" "Base[float]" "Derived[int]" context;
    assert_meet ~order:variance_order "Derived[float]" "Base[int]" "Derived[float]" context;
    assert_meet
      ~order:multiplane_variance_order
      "B[int, float]"
      "A[int, float]"
      "B[int, float]"
      context;
    assert_meet ~order:multiplane_variance_order "B[int, int]" "A[int, float]" "B[int, int]" context;
    assert_meet
      ~order:multiplane_variance_order
      "B[float, int]"
      "A[int, float]"
      "B[float, int]"
      context;
    assert_meet
      ~order:multiplane_variance_order
      "B[float, float]"
      "A[int, float]"
      "B[float, float]"
      context;
    assert_meet
      ~order:multiplane_variance_order
      "B[int, float]"
      "A[float, float]"
      "B[int, float]"
      context;
    assert_meet
      ~order:multiplane_variance_order
      "B[int, int]"
      "A[float, float]"
      "B[int, int]"
      context;
    assert_meet
      ~order:multiplane_variance_order
      "B[float, int]"
      "A[float, float]"
      "B[float, int]"
      context;
    assert_meet
      ~order:multiplane_variance_order
      "B[float, float]"
      "A[float, float]"
      "B[float, float]"
      context;
    assert_meet
      ~order:parallel_planes_variance_order
      "B[float, float]"
      "A[int, float]"
      "B[int, float]"
      context;
    assert_meet
      ~order:parallel_planes_variance_order
      "B[float, float]"
      "A[int, int]"
      "B[int, float]"
      context
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
    let variable = Type.Variable (Type.Variable.TypeVar.create "T") in
    let variable2 = Type.Variable (Type.Variable.TypeVar.create "T2") in
    concrete_connect order ~predecessor:"M" ~successor:"typing.Generic" ~arguments:[variable];
    concrete_connect order ~predecessor:"M" ~successor:"A" ~arguments:[variable];
    concrete_connect order ~predecessor:"M" ~successor:"X" ~arguments:[variable];
    concrete_connect order ~predecessor:"M" ~successor:"Y" ~arguments:[variable];
    concrete_connect order ~predecessor:"A" ~successor:"typing.Generic" ~arguments:[variable];
    let connect_x () =
      concrete_connect order ~predecessor:"X" ~successor:"typing.Generic" ~arguments:[variable];
      concrete_connect order ~predecessor:"X" ~successor:"B" ~arguments:[variable; Type.string]
    in
    if x_before_y then connect_x ();
    concrete_connect order ~predecessor:"Y" ~successor:"typing.Generic" ~arguments:[variable];
    concrete_connect order ~predecessor:"Y" ~successor:"B" ~arguments:[Type.integer; variable];
    if not x_before_y then connect_x ();
    concrete_connect
      order
      ~predecessor:"B"
      ~successor:"typing.Generic"
      ~arguments:[variable; variable2];
    handler order
  in
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
  test_list
    [
      (* Special elements. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet "typing.List[float]" "typing.Any" "typing.List[float]";
      (* Primitive types. *)
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_meet "list" "typing.Sized" "list";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet "typing.List[int]" "typing.Sized" "typing.List[int]";
      (* Annotated types. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet "typing.Annotated[int, 1]" "float" "typing.Annotated[int, 1]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet
           "typing.Annotated[int, 1]"
           "typing.Annotated[float, 1]"
           "typing.Annotated[int, 1]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet
           "typing_extensions.Annotated[int, 1]"
           "float"
           "typing_extensions.Annotated[int, 1]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet
           "typing_extensions.Annotated[int, 1]"
           "typing_extensions.Annotated[float, 1]"
           "typing_extensions.Annotated[int, 1]";
      (* ReadOnly. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet "pyre_extensions.ReadOnly[Child]" "Base" "Child";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet
           "pyre_extensions.ReadOnly[Child]"
           "pyre_extensions.ReadOnly[Base]"
           "pyre_extensions.ReadOnly[Child]";
      (* Unions. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet "typing.Union[int, str]" "typing.Union[int, bytes]" "int";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet "typing.Union[int, str]" "typing.Union[str, int]" "typing.Union[int, str]";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_meet "typing.Union[int, str]" "float" "int";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet "typing.Union[int, str]" "typing.List[int]" "$bottom";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet "typing.Union[int, str]" "typing.Union[float, bool]" "int";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet "typing.Union[int, str]" "typing.Union[int, bool]" "int";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_meet "None" "typing.Optional[str]" "None";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet
           "typing.Union[int, str]"
           "typing.Union[int, typing.Optional[str]]"
           "typing.Union[int, str]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet
           "typing.Union[int, typing.Optional[str]]"
           "typing.Optional[str]"
           "typing.Optional[str]";
      (* Parametric types. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet "typing.List[int]" "typing.Iterator[int]" "typing.List[int]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet "typing.List[float]" "typing.Iterator[int]" "$bottom";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet "typing.List[float]" "float[int]" "$bottom";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet "typing.Dict[str, str]" "typing.Dict[str, typing.List[str]]" "$bottom";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet ~order:disconnected_order "A" "B" "$bottom";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet "GenericContainer[int, str]" "DifferentGenericContainer[int, str]" "$bottom";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet "GenericContainer[int, str]" "DifferentGenericContainer[str, int]" "$bottom";
      (* Variables. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_equal (meet default Type.integer (Type.variable "T")) Type.Bottom;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_equal
           (meet
              default
              Type.integer
              (Type.variable ~constraints:(Type.Record.TypeVarConstraints.Bound Type.float) "T"))
           Type.Bottom;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_equal
           (meet
              default
              Type.string
              (Type.variable
                 ~constraints:(Type.Record.TypeVarConstraints.Explicit [Type.float; Type.string])
                 "T"))
           Type.Bottom;
      (* Variance. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_equal
           (meet
              variance_order
              (Type.parametric "LinkedList" ![Type.integer])
              (Type.parametric "LinkedList" ![Type.Top]))
           (Type.parametric "LinkedList" ![Type.integer]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_equal
           (meet
              variance_order
              (Type.parametric "LinkedList" ![Type.Top])
              (Type.parametric "LinkedList" ![Type.integer]))
           (Type.parametric "LinkedList" ![Type.integer]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_equal
           (meet
              variance_order
              (Type.parametric "LinkedList" ![Type.integer])
              (Type.parametric "LinkedList" ![Type.Any]))
           (Type.parametric "LinkedList" ![Type.integer]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_equal
           (meet
              variance_order
              (Type.parametric "LinkedList" ![Type.Any])
              (Type.parametric "LinkedList" ![Type.integer]))
           (Type.parametric "LinkedList" ![Type.Any]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet
           ~order:(make_potentially_inconsistent_order ~x_before_y:true)
           "B[int, str]"
           "A[str]"
           "$bottom";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet
           ~order:(make_potentially_inconsistent_order ~x_before_y:false)
           "B[int, str]"
           "A[str]"
           "$bottom";
      (* Recursive types. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_equal tree_annotation (meet default tree_annotation tree_annotation);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_equal tree_annotation (meet default tree_annotation tree_annotation2);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_equal
           tree_annotation
           (meet default tree_annotation tree_annotation_with_string);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_equal Type.integer (meet default tree_annotation Type.integer);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_equal Type.Bottom (meet default tree_annotation Type.string);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_equal Type.Bottom (meet default tree_annotation non_tree);
    ]


let test_meet_callable =
  let assert_meet ?(order = default) left right expected _ =
    assert_type_equal expected (meet order left right);
    assert_type_equal expected (meet order right left)
  in
  let named_int_to_int =
    Type.Callable.create
      ~name:(Reference.create "name")
      ~parameters:
        (Defined
           [
             Type.Callable.CallableParamType.Named
               { name = "a"; annotation = Type.integer; default = false };
           ])
      ~annotation:Type.integer
      ()
  in
  let anonymous_str_to_int =
    Type.Callable.create
      ~parameters:
        (Defined
           [
             Type.Callable.CallableParamType.Named
               { name = "a"; annotation = Type.string; default = false };
           ])
      ~annotation:Type.integer
      ()
  in
  let anonymous_positional_only_str_to_int =
    Type.Callable.create
      ~parameters:
        (Defined
           [
             Type.Callable.CallableParamType.PositionalOnly
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
             Type.Callable.CallableParamType.Named
               { name = "a"; annotation = Type.integer; default = false };
           ])
      ~annotation:Type.float
      ()
  in
  let anonymous_union_int_str_to_int =
    Type.Callable.create
      ~parameters:
        (Defined
           [
             Type.Callable.CallableParamType.Named
               { name = "a"; annotation = Type.union [Type.integer; Type.string]; default = false };
           ])
      ~annotation:Type.integer
      ()
  in
  let overloaded_str_to_int =
    Type.Callable.create
      ~parameters:
        (Defined
           [
             Type.Callable.CallableParamType.Named
               { name = "a"; annotation = Type.string; default = false };
           ])
      ~annotation:(Type.union [Type.integer; Type.string])
      ~overloads:
        [
          {
            parameters =
              Defined
                [
                  Type.Callable.CallableParamType.Named
                    { name = "a"; annotation = Type.string; default = false };
                ];
            annotation = Type.integer;
          };
        ]
      ()
  in
  let anonymous_undefined_to_object = Type.Callable.create ~annotation:Type.object_primitive () in
  let anonymous_undefined_to_int = Type.Callable.create ~annotation:Type.integer () in
  let anonymous_str_named_b_to_int =
    Type.Callable.create
      ~parameters:
        (Defined
           [
             Type.Callable.CallableParamType.Named
               { name = "b"; annotation = Type.string; default = false };
           ])
      ~annotation:Type.integer
      ()
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet named_int_to_int named_int_to_int named_int_to_int;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet anonymous_int_to_float anonymous_str_to_int anonymous_union_int_str_to_int;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet anonymous_positional_only_str_to_int anonymous_str_to_int anonymous_str_to_int;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet anonymous_str_to_int anonymous_str_named_b_to_int anonymous_undefined_to_int;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet
           anonymous_positional_only_str_to_int
           anonymous_undefined_to_object
           anonymous_undefined_to_int;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet anonymous_str_to_int anonymous_undefined_to_object anonymous_undefined_to_int;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet named_int_to_int anonymous_undefined_to_object anonymous_undefined_to_int;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet anonymous_undefined_to_object named_int_to_int anonymous_undefined_to_int;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet named_int_to_int anonymous_str_to_int anonymous_union_int_str_to_int;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet anonymous_str_to_int named_int_to_int anonymous_union_int_str_to_int;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet overloaded_str_to_int overloaded_str_to_int overloaded_str_to_int;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_meet overloaded_str_to_int anonymous_str_to_int Type.Bottom;
    ]


let () =
  "order"
  >::: [
         test_join;
         test_join_with_full_type_order;
         test_join_recursive_types;
         test_less_or_equal;
         test_less_or_equal_primitives;
         test_less_or_equal_variance;
         test_meet;
         test_meet_callable;
       ]
  |> Test.run
