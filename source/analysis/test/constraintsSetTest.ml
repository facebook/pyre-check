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

let environment ?source context =
  let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
    let sources = Option.value_map source ~f:(fun source -> ["test.py", source]) ~default:[] in
    ScratchProject.setup ~context sources |> ScratchProject.build_global_environment
  in
  global_environment


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


let make_assert_functions context =
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
        def __init__(self, x: int) -> None:
          pass

      class Parent: pass
      class ChildA(Parent): pass
      class ChildB(Parent): pass

      class Meta(type):
        pass

      class HasMeta(metaclass=Meta):
        pass

      Ts = pyre_extensions.TypeVarTuple("Ts")
      Ts2 = pyre_extensions.TypeVarTuple("Ts2")

      class Tensor(typing.Generic[T, pyre_extensions.Unpack[Ts]]): ...
    |}
      context
  in
  let resolution = GlobalResolution.create environment in
  let default_postprocess annotation = Type.Variable.mark_all_variables_as_bound annotation in
  let prep annotation =
    let aliases ?replace_unbound_parameters_with_any:_ a =
      let s =
        [
          "C";
          "D";
          "Q";
          "T_Unconstrained";
          "T_Bound_C";
          "T_Bound_D";
          "T_Bound_Union";
          "T_Bound_Union_C_Q";
          "T_Bound_Union";
          "T_C_Q";
          "T_D_Q";
          "T_C_Q_int";
          "V";
          "T";
          "T1";
          "T2";
          "T3";
          "T4";
          "G_invariant";
          "G_covariant";
          "T_Covariant";
          "Constructable";
          "UserDefinedVariadic";
          "UserDefinedVariadicSimpleChild";
          "UserDefinedVariadicMapChild";
          "Ts";
          "Ts2";
          "Tensor";
        ]
        |> Type.Primitive.Set.of_list
      in
      if Type.Primitive.Set.mem s a then
        Some (Type.TypeAlias (Type.Primitive ("test." ^ a)))
      else
        GlobalResolution.aliases resolution a
    in
    annotation |> Type.create ~aliases |> Type.expression
  in
  let parse_annotation annotation ~do_prep =
    annotation
    |> String.substr_replace_all ~pattern:"typing.Callable[V" ~with_:"typing.Callable[test.V"
    |> parse_single_expression
    |> (if do_prep then prep else Fn.id)
    |> GlobalResolution.parse_annotation ~validation:NoValidation resolution
  in
  let assert_add_direct
      ~left
      ~right
      ?(is_protocol = fun _ ~protocol_assumptions:_ -> false)
      ?(attributes = fun _ ~assumptions:_ -> None)
      ?constraints
      ?(postprocess = default_postprocess)
      ?(do_prep = true)
      expected
    =
    let handler =
      let class_hierarchy =
        GlobalResolution.create environment |> GlobalResolution.class_hierarchy
      in
      let metaclass name ~assumptions:_ = GlobalResolution.metaclass ~resolution name in
      let order =
        {
          ConstraintsSet.class_hierarchy = hierarchy class_hierarchy;
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
          metaclass;
        }
      in
      let attributes annotation ~assumptions =
        match attributes annotation ~assumptions with
        | Some attributes -> Some attributes
        | None -> (
            match Type.resolve_class annotation with
            | Some [{ instantiated; accessed_through_class; class_name }] ->
                GlobalResolution.attributes
                  ~transitive:true
                  ~resolution
                  ~accessed_through_class
                  class_name
                >>| List.map
                      ~f:
                        (GlobalResolution.instantiate_attribute
                           ~resolution
                           ~instantiated
                           ~accessed_through_class)
            | _ -> None)
      in
      { order with all_attributes = attributes; attribute = attribute_from_attributes attributes }
    in
    let parse_annotation = parse_annotation ~do_prep in
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
                match parse_annotation ordered with
                | Type.Tuple ordered_type -> ordered_type
                | _ -> failwith "expected tuple"
              in
              let global_resolution = GlobalResolution.create environment in
              match GlobalResolution.aliases global_resolution primitive with
              | Some (Type.VariableAlias (ParameterVariadic variable)) ->
                  Type.Variable.ParameterVariadicPair (variable, parse_parameters value)
              | Some (Type.VariableAlias (TupleVariadic variable)) -> (
                  match Type.Tuple (parse_ordered_types value) |> postprocess with
                  | Type.Tuple ordered_type ->
                      Type.Variable.TupleVariadicPair (variable, ordered_type)
                  | _ -> failwith "expected a tuple")
              | _ -> failwith "not available")
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
      (TypeOrder.OrderedConstraintsSet.add
         [constraints]
         ~new_constraint:(LessOrEqual { left; right })
         ~order:handler
      |> List.filter_map ~f:(OrderedConstraints.solve ~order:handler))
  in
  let assert_add ?(do_prep = true) ?(leave_unbound_in_left = []) ~left ~right =
    let parse_annotation = parse_annotation ~do_prep in
    let leave_unbound_in_left = List.map leave_unbound_in_left ~f:(fun a -> "test." ^ a) in
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
               { head = []; variable = Type.Variable.Variadic.Parameters.mark_as_bound variable })
      in
      let mark_tuple_variadic variable =
        if
          List.mem
            leave_unbound_in_left
            (Type.Variable.Variadic.Tuple.name variable)
            ~equal:Identifier.equal
        then
          None
        else
          Some
            (Type.Variable.Variadic.Tuple.self_reference
               (Type.Variable.Variadic.Tuple.mark_as_bound variable))
      in
      parse_annotation left
      |> Type.Variable.GlobalTransforms.Unary.replace_all mark_unary
      |> Type.Variable.GlobalTransforms.ParameterVariadic.replace_all mark_parameter_variadic
      |> Type.Variable.GlobalTransforms.TupleVariadic.replace_all mark_tuple_variadic
    in
    let right = parse_annotation right in
    assert_add_direct ~left ~right ~do_prep
  in
  assert_add, assert_add_direct, prep, resolution


let test_add_constraint context =
  let assert_add, assert_add_direct, prep, resolution = make_assert_functions context in
  assert_add
    ~leave_unbound_in_left:["T_Unconstrained"]
    ~left:"typing.Optional[T_Unconstrained]"
    ~right:"object"
    [[]];
  assert_add ~left:"C" ~right:"T_Unconstrained" [["T_Unconstrained", "C"]];
  assert_add ~left:"D" ~right:"T_Unconstrained" [["T_Unconstrained", "D"]];
  assert_add ~left:"Q" ~right:"T_Unconstrained" [["T_Unconstrained", "Q"]];
  assert_add ~left:"C" ~right:"T_Bound_C" [["T_Bound_C", "C"]];
  assert_add ~left:"D" ~right:"T_Bound_C" [["T_Bound_C", "D"]];
  assert_add ~left:"Q" ~right:"T_Bound_C" [];
  assert_add ~left:"C" ~right:"T_Bound_D" [];
  assert_add ~left:"C" ~right:"T_C_Q" [["T_C_Q", "C"]];

  assert_add
    ~leave_unbound_in_left:["T_Unconstrained"]
    ~left:"T_Unconstrained"
    ~right:"typing.Any"
    [["T_Unconstrained", "typing.Any"]];
  assert_add
    ~leave_unbound_in_left:["T_Bound_C"]
    ~left:"T_Bound_C"
    ~right:"typing.Any"
    [["T_Bound_C", "typing.Any"]];
  assert_add
    ~leave_unbound_in_left:["T_C_Q"]
    ~left:"T_C_Q"
    ~right:"typing.Any"
    [["T_C_Q", "typing.Any"]];

  assert_add
    ~leave_unbound_in_left:["T_Unconstrained"]
    ~left:"typing.List[T_Unconstrained]"
    ~right:"typing.Any"
    [["T_Unconstrained", "typing.Any"]];

  assert_add
    ~leave_unbound_in_left:["T_Unconstrained"]
    ~left:"typing.Tuple[T_Unconstrained, T_Unconstrained]"
    ~right:"typing.Tuple[typing.Any, int]"
    [["T_Unconstrained", "int"]];

  assert_add ~left:"typing.Any" ~right:"T_Unconstrained" [["T_Unconstrained", "typing.Any"]];
  assert_add ~left:"typing.Any" ~right:"T_Bound_C" [["T_Bound_C", "typing.Any"]];
  assert_add ~left:"typing.Any" ~right:"T_C_Q" [["T_C_Q", "typing.Any"]];

  assert_add
    ~left:"typing.Any"
    ~right:"typing.List[T_Unconstrained]"
    [["T_Unconstrained", "typing.Any"]];

  assert_add
    ~left:"typing.Tuple[typing.Any, int]"
    ~right:"typing.Tuple[T_Unconstrained, T_Unconstrained]"
    [["T_Unconstrained", "int"]];

  (* Annotated types. *)
  assert_add ~left:"typing.Annotated[C]" ~right:"T_Unconstrained" [["T_Unconstrained", "C"]];
  assert_add ~left:"C" ~right:"typing.Annotated[T_Unconstrained]" [["T_Unconstrained", "C"]];

  assert_add
    ~left:"typing_extensions.Annotated[C]"
    ~right:"T_Unconstrained"
    [["T_Unconstrained", "C"]];
  assert_add
    ~left:"C"
    ~right:"typing_extensions.Annotated[T_Unconstrained]"
    [["T_Unconstrained", "C"]];

  (* An explicit type variable can only be bound to its constraints *)
  assert_add ~left:"D" ~right:"T_C_Q" [["T_C_Q", "C"]];
  assert_add ~left:"C" ~right:"T_D_Q" [];
  assert_add
    ~left:"typing.Union[int, G_invariant[str], str]"
    ~right:"T_Unconstrained"
    [["T_Unconstrained", "typing.Union[int, G_invariant[str], str]"]];
  assert_add ~left:"typing.Union[D, C]" ~right:"T_Bound_C" [["T_Bound_C", "typing.Union[D, C]"]];
  assert_add
    ~constraints:["T_Unconstrained", (Some "Q", None)]
    ~left:"G_invariant[C]"
    ~right:"G_invariant[T_Unconstrained]"
    [];
  assert_add
    ~constraints:["T_Unconstrained", (Some "Q", None)]
    ~left:"G_covariant[C]"
    ~right:"G_covariant[T_Unconstrained]"
    [["T_Unconstrained", "typing.Union[Q, C]"]];
  assert_add
    ~left:"typing.Optional[C]"
    ~right:"typing.Optional[T_Unconstrained]"
    [["T_Unconstrained", "C"]];
  assert_add ~left:"C" ~right:"typing.Optional[T_Unconstrained]" [["T_Unconstrained", "C"]];
  assert_add
    ~left:"typing.Tuple[C, ...]"
    ~right:"typing.Tuple[T_Unconstrained, ...]"
    [["T_Unconstrained", "C"]];
  assert_add
    ~left:"typing.Tuple[C, Q, D]"
    ~right:"typing.Tuple[T_Unconstrained, T_Unconstrained, C]"
    [["T_Unconstrained", "typing.Union[C, Q]"]];
  assert_add
    ~left:"typing.Tuple[D, ...]"
    ~right:"typing.Tuple[T_Unconstrained, T_Unconstrained, C]"
    [["T_Unconstrained", "D"]];
  assert_add
    ~left:"typing.Tuple[C, Q, D]"
    ~right:"typing.Tuple[T_Unconstrained, ...]"
    [["T_Unconstrained", "typing.Union[C, Q]"]];
  assert_add
    ~left:"G_covariant[C]"
    ~right:"typing.Union[G_covariant[T_Unconstrained], int]"
    [["T_Unconstrained", "C"]];
  assert_add
    ~left:"typing.Type[int]"
    ~right:"typing.Callable[[], T_Unconstrained]"
    (* there are two int constructor overloads *)
    [["T_Unconstrained", "int"]; ["T_Unconstrained", "int"]];
  assert_add
    ~left:"typing.Optional[typing.Tuple[C, Q, typing.Callable[[D, int], C]]]"
    ~right:"typing.Optional[typing.Tuple[T, T, typing.Callable[[T, T], T]]]"
    [];
  assert_add
    ~left:"typing.Optional[typing.Tuple[C, C, typing.Callable[[C, C], C]]]"
    ~right:"typing.Optional[typing.Tuple[T, T, typing.Callable[[T, T], T]]]"
    [["T", "C"]];

  (* Bound => Bound *)
  assert_add ~left:"T_Bound_D" ~right:"T_Bound_C" [["T_Bound_C", "T_Bound_D"]];
  assert_add ~left:"T_Bound_C" ~right:"T_Bound_D" [];
  assert_add ~left:"T_Bound_Union" ~right:"T_Bound_Union" [["T_Bound_Union", "T_Bound_Union"]];

  (* Bound => Explicit *)
  assert_add ~left:"T_Bound_D" ~right:"T_C_Q" [["T_C_Q", "C"]];
  assert_add ~left:"T_Bound_C" ~right:"T_D_Q" [];

  (* Explicit => Bound *)
  assert_add ~left:"T_D_Q" ~right:"T_Bound_Union_C_Q" [["T_Bound_Union_C_Q", "T_D_Q"]];
  assert_add ~left:"T_D_Q" ~right:"T_Bound_D" [];

  (* Explicit => Explicit *)
  assert_add ~left:"T_C_Q" ~right:"T_C_Q_int" [["T_C_Q_int", "T_C_Q"]];

  (* This one is theoretically solvable, but only if we're willing to introduce dependent variables
     as the only sound solution here would be
   *  T_C_Q_int => T_new <: C if T_D_Q is D, Q if T_D_Q is Q *)
  assert_add ~left:"T_D_Q" ~right:"T_C_Q_int" [];
  assert_add
    ~leave_unbound_in_left:["T_Unconstrained"]
    ~left:"typing.Callable[[T_Unconstrained], T_Unconstrained]"
    ~right:"typing.Callable[[int], int]"
    [[]];
  assert_add
    ~left:"typing.Callable[[int], int]"
    ~right:"typing.Callable[[T_Unconstrained], T_Unconstrained]"
    [["T_Unconstrained", "int"]];
  assert_add
    ~leave_unbound_in_left:["T"]
    ~left:"typing.Callable[[T], T]"
    ~right:"typing.Callable[[T_Unconstrained], T_Unconstrained]"
    [[]];
  assert_add
    ~leave_unbound_in_left:["T"]
    ~left:"typing.Callable[[T], G_invariant[T]]"
    ~right:"typing.Callable[[T_Unconstrained], T_Unconstrained]"
    [];
  assert_add
    ~leave_unbound_in_left:["T1"]
    ~left:"typing.Callable[[T1], typing.Tuple[T1, T2]]"
    ~right:"typing.Callable[[T3], typing.Tuple[T3, T4]]"
    [["T4", "T2"]];
  assert_add
    ~left:"typing.Type[Constructable]"
    ~right:"typing.Callable[[T3], T4]"
    [["T3", "int"; "T4", "Constructable"]];
  assert_add
    ~left:"typing.Callable[[typing.Union[int, str]], str]"
    ~right:"typing.Callable[[int], T4]"
    [["T4", "str"]];
  assert_add
    ~left:"typing.Callable[[typing.Union[int, str], int], str]"
    ~right:"typing.Callable[[int, T3], T4]"
    [["T3", "int"; "T4", "str"]];
  assert_add
    ~do_prep:false
    ~leave_unbound_in_left:["T3"]
    ~left:"typing.Callable[[test.T3], test.T3]"
    ~right:"typing.Callable[[typing.Union[int, str]], object][[[int], test.T1][[str], test.T2]] "
    [["test.T2", "str"; "test.T1", "int"]];
  assert_add
    ~leave_unbound_in_left:["T_Unconstrained"]
    ~left:"typing.Callable[[Variable(T_Unconstrained), Keywords(T_Unconstrained)], T_Unconstrained]"
    ~right:"typing.Callable[[Named(a, int), Named(b, str)], T1]"
    [["test.T1", "typing.Union[int, str]"]];
  assert_add
    ~leave_unbound_in_left:["T_Unconstrained"]
    ~left:
      "typing.Callable[[Variable(typing.Sequence[T_Unconstrained]), \
       Keywords(typing.Sequence[T_Unconstrained])], T_Unconstrained]"
    ~right:"typing.Callable[[Named(a, int), Named(b, str)], T1]"
    [];
  assert_add
    ~leave_unbound_in_left:["T_Unconstrained"]
    ~left:
      "typing.Callable[[Variable(typing.Sequence[T_Unconstrained]), \
       Keywords(typing.Sequence[T_Unconstrained])], T_Unconstrained]"
    ~right:"typing.Callable[[Named(a, typing.List[int]), Named(b, typing.List[str])], T1]"
    [["test.T1", "typing.Union[int, str]"]];

  (* Callback protocols *)
  let parse_annotation annotation =
    annotation |> parse_single_expression |> prep |> GlobalResolution.parse_annotation resolution
  in
  let is_protocol annotation ~protocol_assumptions:_ =
    match annotation with
    | Type.Parametric { name = "test.G_invariant"; _ } -> true
    | _ -> false
  in
  let attributes annotation ~assumptions:_ =
    match annotation with
    | Type.Parametric { name = "test.G_invariant"; _ } ->
        Some
          (parse_attributes
             ~parse_annotation
             ~class_name:"B"
             ["__call__", "typing.Callable[[T], str]"])
    | Type.Callable _ ->
        Some (make_attributes ~class_name:"typing.Callable" ["__call__", annotation])
    | _ -> failwith "getting attributes for wrong class"
  in
  assert_add
    ~is_protocol
    ~attributes
    ~left:"typing.Callable[[int], str]"
    ~right:"G_invariant[T1]"
    [["T1", "int"]];
  let attributes annotation ~assumptions:_ =
    match annotation with
    | Type.Primitive "HasBoundMethodCall" ->
        Some
          (parse_attributes
             ~parse_annotation
             ~class_name:"HasBoundMethodCall"
             ["__call__", "BoundMethod[typing.Callable[[int, str], bool], int]"])
    | _ -> None
  in
  assert_add ~attributes ~left:"HasBoundMethodCall" ~right:"typing.Callable[[str], bool]" [[]];
  assert_add ~attributes ~left:"HasBoundMethodCall" ~right:"typing.Callable[[int], bool]" [];

  (* Multiple options *)
  assert_add ~left:"int" ~right:"typing.Union[T1, T2]" [["T1", "int"]; ["T2", "int"]];
  assert_add ~left:"int" ~right:"typing.Union[T1, T2, int]" [[]];
  assert_add
    ~left:"typing.List[int]"
    ~right:"typing.Union[typing.List[T1], T1]"
    [["T1", "int"]; ["T1", "typing.List[int]"]];
  assert_add ~left:"typing.List[int]" ~right:"typing.Union[typing.List[int], T1]" [[]];
  assert_add
    ~left:"typing.Tuple[typing.List[int], typing.List[int]]"
    ~right:"typing.Tuple[typing.Union[typing.List[T1], T1], T1]"
    [["T1", "typing.List[int]"]];
  assert_add
    ~left:"typing.Tuple[typing.List[int], typing.List[int]]"
    ~right:"typing.Tuple[typing.Union[typing.List[T1], T1], T1]"
    [["T1", "typing.List[int]"]];
  assert_add
    ~do_prep:false
    ~left:
      ("typing.Callable[[typing.Union[int, str]], typing.Union[int, str]]"
      ^ "[[[int], str][[str], int]]")
    ~right:"typing.Callable[[test.T3], test.T4]"
    [
      ["test.T3", "int"; "test.T4", "str"];
      ["test.T3", "str"; "test.T4", "int"];
      ["test.T3", "typing.Union[int, str]"; "test.T4", "typing.Union[int, str]"];
    ];

  (* Free Variable <-> Free Variable constraints *)
  assert_add
    ~postprocess:Fn.id
    ~leave_unbound_in_left:["T1"]
    ~left:"T1"
    ~right:"T2"
    [["T2", "T1"]; ["T1", "T2"]];
  assert_add
    ~postprocess:Fn.id
    ~leave_unbound_in_left:["T"]
    ~left:"typing.Callable[[T], T]"
    ~right:"typing.Callable[[T1], T2]"
    [["T2", "T1"]; ["T1", "T2"]];
  assert_add
    ~leave_unbound_in_left:["T"]
    ~left:"typing.Tuple[typing.Callable[[T], T], int]"
    ~right:"typing.Tuple[typing.Callable[[T1], T2], T1]"
    [["T2", "int"; "T1", "int"]];
  assert_add
    ~left:"typing.Callable[[int, int], int]"
    ~right:"typing.Callable[V, int]"
    [["V", "[int, int]"]];

  (* We need to ensure that return values are still checked *)
  assert_add ~left:"typing.Callable[[int, int], int]" ~right:"typing.Callable[V, str]" [];
  assert_add
    ~left:"typing.Callable[[int, int], int]"
    ~right:"typing.Callable[V, T1]"
    [["T1", "int"; "V", "[int, int]"]];

  (* We should be able to capture the same parameter set twice *)
  assert_add
    ~left:"typing.Tuple[typing.Callable[[int, int], int], typing.Callable[[int, int], int]]"
    ~right:"typing.Tuple[typing.Callable[V, int], typing.Callable[V, int]]"
    [["V", "[int, int]"]];

  (* We currently do not find a way to take both [int, int] and [int, str]. A correct solution would
     be [int, Intersection[int, str]]. This is probably not desired *)
  assert_add
    ~left:"typing.Tuple[typing.Callable[[int, int], int], typing.Callable[[int, str], int]]"
    ~right:"typing.Tuple[typing.Callable[V, int], typing.Callable[V, int]]"
    [];

  (* There is no common interface between a callable that requires exactly two arguments and one
     that requires exactly one *)
  assert_add
    ~left:"typing.Tuple[typing.Callable[[int, int], int], typing.Callable[[int], int]]"
    ~right:"typing.Tuple[typing.Callable[V, int], typing.Callable[V, int]]"
    [];
  assert_add
    ~left:"typing.Tuple[typing.Callable[[int, int], int], typing.Callable[[int], int]]"
    ~right:"typing.Tuple[typing.Callable[V, int], typing.Callable[V, int]]"
    [];

  assert_add ~left:"typing.Callable[..., int]" ~right:"typing.Callable[..., object]" [[]];
  assert_add
    ~left:"typing.Callable[..., int]"
    ~right:"typing.Callable[[Named(a, int), Named(b, str)], int]"
    [[]];
  assert_add
    ~left:"typing.Callable[[Named(a, int), Named(b, str)], int]"
    ~right:"typing.Callable[[int, str], int]"
    [[]];

  assert_add
    ~left:"typing.Union[typing.Type[test.ChildA], typing.Type[test.ChildB]]"
    ~right:"typing.Callable[[], test.Parent]"
    [[]];
  assert_add ~left:"typing.Type[test.ChildA]" ~right:"test.Meta" [];
  assert_add ~left:"typing.Type[test.HasMeta]" ~right:"test.Meta" [[]];
  (* BoundMethods are Callable, but Callables can't replace a BoundMethod *)
  assert_add
    ~left:"BoundMethod[typing.Callable[[int, str], bool], int]"
    ~right:"typing.Callable[[str], bool]"
    [[]];
  assert_add
    ~left:"typing.Callable[[str], bool]"
    ~right:"BoundMethod[typing.Callable[[int, str], bool], int]"
    [];

  let { Type.Variable.Variadic.Parameters.Components.positional_component; keyword_component } =
    Type.Variable.Variadic.Parameters.create "TParams"
    |> Type.Variable.Variadic.Parameters.decompose
  in

  assert_add_direct
    ~left:positional_component
    ~right:(parse_annotation "typing.Tuple[object, ...]")
    [[]];
  assert_add_direct
    ~left:positional_component
    ~right:(parse_annotation "typing.Iterable[object]")
    [[]];
  assert_add_direct ~left:positional_component ~right:(parse_annotation "typing.Iterable[int]") [];

  assert_add_direct
    ~left:keyword_component
    ~right:(parse_annotation "typing.Mapping[str, object]")
    [[]];
  assert_add_direct ~left:keyword_component ~right:(parse_annotation "typing.Mapping[str, int]") [];

  (* Literals. *)
  assert_add ~left:{| typing_extensions.Literal["hello"] |} ~right:"str" [[]];
  assert_add
    ~left:{| typing_extensions.Literal["hello"] |}
    ~right:{| typing_extensions.Literal["world"] |}
    [];
  assert_add
    ~left:{| typing_extensions.Literal["hello"] |}
    ~right:"typing_extensions.Literal[str]"
    [[]];
  assert_add
    ~left:{| typing_extensions.Literal[str] |}
    ~right:{| typing_extensions.Literal[str] |}
    [[]];
  assert_add ~left:{| typing_extensions.Literal[str] |} ~right:"str" [[]];
  assert_add ~left:"str" ~right:{| typing_extensions.Literal["hello"] |} [];
  assert_add ~left:"str" ~right:{| typing_extensions.Literal[str] |} [];
  ()


let test_add_constraint_recursive_type context =
  let _, assert_add_direct, _, _ = make_assert_functions context in
  let tree_annotation =
    Type.RecursiveType.create
      ~name:"test.Tree"
      ~body:
        (Type.union
           [Type.integer; Type.tuple [Type.Primitive "test.Tree"; Type.Primitive "test.Tree"]])
  in
  assert_add_direct ~left:tree_annotation ~right:tree_annotation [[]];
  assert_add_direct ~left:Type.integer ~right:tree_annotation [[]];
  assert_add_direct ~left:Type.string ~right:tree_annotation [];
  assert_add_direct ~left:(Type.tuple [Type.integer; Type.integer]) ~right:tree_annotation [[]];
  assert_add_direct
    ~left:(Type.union [Type.integer; Type.tuple [Type.integer; Type.integer]])
    ~right:tree_annotation
    [[]];
  assert_add_direct
    ~left:(Type.tuple [Type.integer; Type.tuple [Type.integer; Type.integer]])
    ~right:tree_annotation
    [[]];
  (* Should fail because of the string. *)
  assert_add_direct
    ~left:(Type.tuple [Type.integer; Type.tuple [Type.integer; Type.string]])
    ~right:tree_annotation
    [];

  assert_add_direct ~left:tree_annotation ~right:Type.integer [];
  assert_add_direct ~left:tree_annotation ~right:(Type.union [Type.integer; tree_annotation]) [[]];
  let isomorphic_tree_annotation =
    Type.RecursiveType.create
      ~name:"test.IsomorphicTree"
      ~body:
        (Type.union
           [
             Type.integer;
             Type.tuple [Type.Primitive "test.IsomorphicTree"; Type.Primitive "test.IsomorphicTree"];
           ])
  in
  assert_add_direct ~left:tree_annotation ~right:isomorphic_tree_annotation [[]];

  let json_annotation =
    Type.RecursiveType.create
      ~name:"test.JSON"
      ~body:
        (Type.union
           [
             Type.integer;
             Type.parametric
               "typing.Mapping"
               [Single Type.string; Single (Type.Primitive "test.JSON")];
           ])
  in
  assert_add_direct ~left:json_annotation ~right:json_annotation [[]];
  assert_add_direct ~left:Type.integer ~right:json_annotation [[]];
  assert_add_direct ~left:Type.string ~right:json_annotation [];
  assert_add_direct
    ~left:(Type.dictionary ~key:Type.string ~value:Type.integer)
    ~right:json_annotation
    [[]];
  assert_add_direct
    ~left:
      (Type.dictionary
         ~key:Type.string
         ~value:(Type.dictionary ~key:Type.string ~value:Type.integer))
    ~right:json_annotation
    [[]];
  ()


let test_add_constraint_type_variable_tuple context =
  let assert_add, assert_add_direct, _, _ = make_assert_functions context in
  assert_add
    ~left:"typing.Tuple[int, str, bool]"
    ~right:"typing.Tuple[int, pyre_extensions.Unpack[Ts]]"
    [["Ts", "typing.Tuple[str, bool]"]];
  assert_add
    ~left:"typing.Tuple[int, str, bool]"
    ~right:"typing.Tuple[int, pyre_extensions.Unpack[Ts], bool]"
    [["Ts", "typing.Tuple[str]"]];
  assert_add
    ~left:"typing.Tuple[int]"
    ~right:"typing.Tuple[int, pyre_extensions.Unpack[Ts], bool]"
    [];
  assert_add
    ~left:"typing.Tuple[int, str, bool]"
    ~right:"typing.Tuple[pyre_extensions.Unpack[Ts], T]"
    [["Ts", "typing.Tuple[int, str]"; "T", "bool"]];
  assert_add
    ~leave_unbound_in_left:["Ts"]
    ~left:"typing.Tuple[int, pyre_extensions.Unpack[Ts]]"
    ~right:"typing.Tuple[int, str, bool]"
    [["Ts", "typing.Tuple[str, bool]"]];
  (* Ts is bound on the left side. We fail because Ts could be any tuple. *)
  assert_add
    ~left:"typing.Tuple[int, pyre_extensions.Unpack[Ts]]"
    ~right:"typing.Tuple[int, str, bool]"
    [];
  (* This ends up checking `[int, str] <: *Ts (bound)`, which is not valid. *)
  assert_add
    ~left:"typing.Callable[[typing.Tuple[pyre_extensions.Unpack[Ts]]], bool]"
    ~right:"typing.Callable[[typing.Tuple[int, str]], bool]"
    [];
  assert_add
    ~left:"typing.Tuple[pyre_extensions.Unpack[Ts]]"
    ~right:"typing.Tuple[pyre_extensions.Unpack[Ts2]]"
    [["Ts2", "typing.Tuple[pyre_extensions.Unpack[Ts]]"]];
  assert_add
    ~left:"typing.Tuple[int, pyre_extensions.Unpack[Ts], str]"
    ~right:"typing.Tuple[pyre_extensions.Unpack[Ts2]]"
    [["Ts2", "typing.Tuple[int, pyre_extensions.Unpack[Ts], str]"]];
  assert_add
    ~left:"typing.Tuple[int, bool, pyre_extensions.Unpack[Ts], bool, str]"
    ~right:"typing.Tuple[int, pyre_extensions.Unpack[Ts2], str]"
    [["Ts2", "typing.Tuple[bool, pyre_extensions.Unpack[Ts], bool]"]];
  (* No solution because the middle portion is `[*Ts (bound)] <: [bool, *Ts2 (free), bool]`. The
     bound `Ts` may not start with `bool`. *)
  assert_add
    ~left:"typing.Tuple[int, pyre_extensions.Unpack[Ts], str]"
    ~right:"typing.Tuple[int, bool, pyre_extensions.Unpack[Ts2], bool, str]"
    [];
  let variadic = Type.Variable.Variadic.Tuple.create "test.Ts" in
  let variadic2 = Type.Variable.Variadic.Tuple.create "test.Ts2" in
  assert_add_direct
    ~left:
      (Type.Tuple
         (Type.OrderedTypes.Concatenation
            (Type.OrderedTypes.Concatenation.create ~prefix:[] ~suffix:[] variadic)))
    ~right:
      (Type.Tuple
         (Type.OrderedTypes.Concatenation
            (Type.OrderedTypes.Concatenation.create
               ~prefix:[Type.integer]
               ~suffix:[Type.string]
               variadic2))
      |> Type.Variable.mark_all_variables_as_bound)
    [["Ts", "typing.Tuple[int, pyre_extensions.Unpack[Ts2], str]"]];
  (* Tuple is covariant. *)
  assert_add ~left:"typing.Tuple[int, str]" ~right:"typing.Tuple[float, str]" [[]];
  assert_add
    ~left:"typing.Tuple[int, pyre_extensions.Unpack[Ts]]"
    ~right:"typing.Tuple[object, ...]"
    [[]];

  (* Parametric types. *)
  assert_add
    ~left:"test.Tensor[str, bool, str]"
    ~right:"test.Tensor[str, pyre_extensions.Unpack[Ts]]"
    [["Ts", "typing.Tuple[bool, str]"]];
  (* Tensor is invariant in the datatype. *)
  assert_add ~left:"Tensor[int, int]" ~right:"Tensor[float, int]" [];
  (* Tensor is covariant in the shape, since the shape is immutable. *)
  assert_add ~left:"Tensor[int, int, int]" ~right:"Tensor[int, float, float]" [[]];
  assert_add
    ~left:"test.Tensor[str, int]"
    ~right:"test.Tensor[str, pyre_extensions.Unpack[typing.Tuple[int, ...]]]"
    [[]];

  (* Callable. *)
  assert_add
    ~left:"typing.Callable[[int, str, bool], None]"
    ~right:"typing.Callable[[int, pyre_extensions.Unpack[Ts]], None]"
    [["Ts", "typing.Tuple[str, bool]"]];
  assert_add
    ~left:"typing.Callable[[int, bool, pyre_extensions.Unpack[Ts], bool, str], None]"
    ~right:"typing.Callable[[int, pyre_extensions.Unpack[Ts2], str], None]"
    [["Ts2", "typing.Tuple[bool, pyre_extensions.Unpack[Ts], bool]"]];
  (* List of empty list means we found a satisfying solution. *)
  assert_add
    ~left:
      "typing.Callable[[int, pyre_extensions.Unpack[Ts]], typing.Tuple[pyre_extensions.Unpack[Ts]]]"
    ~right:"typing.Callable[[int, str, bool], typing.Tuple[str, bool]]"
    ~leave_unbound_in_left:["Ts"]
    [[]];

  (* Unbounded tuples. *)
  assert_add
    ~left:"typing.Tuple[int, ...]"
    ~right:"typing.Tuple[pyre_extensions.Unpack[Ts]]"
    [["Ts", "typing.Tuple[pyre_extensions.Unpack[typing.Tuple[int, ...]]]"]];
  assert_add ~left:"typing.Tuple[int, ...]" ~right:"typing.Tuple[T]" [["T", "int"]];
  assert_add
    ~left:"typing.Tuple[int, str, pyre_extensions.Unpack[typing.Tuple[str, ...]], bool]"
    ~right:"typing.Tuple[int, pyre_extensions.Unpack[Ts], bool]"
    [["Ts", "typing.Tuple[str, pyre_extensions.Unpack[typing.Tuple[str, ...]]]"]];
  assert_add
    ~left:"typing.Tuple[int, pyre_extensions.Unpack[Ts], bool]"
    ~right:"typing.Tuple[int, str, pyre_extensions.Unpack[typing.Tuple[str, ...]], bool]"
    ~leave_unbound_in_left:["Ts"]
    [["Ts", "typing.Tuple[str, pyre_extensions.Unpack[typing.Tuple[str, ...]]]"]];
  assert_add
    ~left:"typing.Tuple[int, str, pyre_extensions.Unpack[typing.Tuple[str, ...]], str, bool]"
    ~right:"typing.Tuple[int, pyre_extensions.Unpack[typing.Tuple[T, ...]], bool]"
    [["T", "str"]];
  assert_add
    ~left:"typing.Tuple[int, pyre_extensions.Unpack[typing.Tuple[str, ...]], str]"
    ~right:"typing.Tuple[T1, T2, T3, T4]"
    [["T4", "str"; "T3", "str"; "T2", "str"; "T1", "int"]];
  assert_add ~left:"typing.Tuple[int, int]" ~right:"typing.Tuple[int, ...]" [[]];
  assert_add ~left:"typing.Tuple[str, ...]" ~right:"typing.Tuple[int]" [];

  (* Stretch the unbounded tuple to meet the expected length. *)
  assert_add
    ~left:"typing.Tuple[int, ...]"
    ~right:"typing.Tuple[int, pyre_extensions.Unpack[Ts]]"
    [["Ts", "typing.Tuple[int, ...]"]];
  assert_add
    ~left:"typing.Tuple[int, ...]"
    ~right:"typing.Tuple[pyre_extensions.Unpack[Ts], int]"
    [["Ts", "typing.Tuple[int, ...]"]];
  assert_add
    ~left:"typing.Tuple[int, ...]"
    ~right:"typing.Tuple[pyre_extensions.Unpack[Ts], str]"
    [];
  assert_add
    ~left:"typing.Tuple[int, pyre_extensions.Unpack[typing.Tuple[str, ...]], bool]"
    ~right:"typing.Tuple[int, str, pyre_extensions.Unpack[typing.Tuple[str, ...]], bool]"
    [[]];
  assert_add
    ~left:"typing.Tuple[int, str, pyre_extensions.Unpack[typing.Tuple[str, ...]], bool]"
    ~right:"typing.Tuple[int, pyre_extensions.Unpack[typing.Tuple[str, ...]], T, bool]"
    [["T", "str"]];
  assert_add
    ~left:"typing.Tuple[int, str, pyre_extensions.Unpack[typing.Tuple[str, ...]], bool]"
    ~right:"typing.Tuple[int, pyre_extensions.Unpack[typing.Tuple[str, ...]], int, bool]"
    [];
  ()


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
    let resolution = GlobalResolution.create environment in
    let substitute name =
      name
      |> String.substr_replace_all ~pattern:"P" ~with_:"test.P"
      |> String.substr_replace_all ~pattern:"T1" ~with_:"test.T1"
      |> String.substr_replace_all ~pattern:"VariadicCol" ~with_:"test.VariadicCol"
    in
    let protocol = substitute protocol in
    let parse_annotation annotation =
      annotation
      |> substitute
      |> parse_single_expression
      |> GlobalResolution.parse_annotation resolution ~validation:NoValidation
    in
    let optional_ordered_types_printer optional =
      optional
      >>| Format.asprintf "%a" (Type.pp_parameters ~pp_type:Type.pp)
      |> Option.value ~default:"None"
    in
    let parse_attributes =
      let parse_class (class_name, attributes) =
        substitute class_name, parse_attributes attributes ~class_name ~parse_annotation
      in
      List.map ~f:parse_class
    in
    let order =
      let classes, protocols = parse_attributes classes, parse_attributes protocols in
      let attributes annotation ~assumptions:_ =
        match annotation with
        | Type.Parametric { name = primitive; _ }
        | Type.Primitive primitive ->
            List.Assoc.find (classes @ protocols) primitive ~equal:String.equal
        | _ -> None
      in
      let is_protocol annotation ~protocol_assumptions:_ =
        match Type.split annotation with
        | Type.Primitive primitive, _ -> List.Assoc.mem protocols primitive ~equal:String.equal
        | _ -> false
      in
      let handler = GlobalResolution.class_hierarchy resolution in
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
    (Some []);
  assert_instantiate_protocol_parameters
    ~source:"class P(): pass"
    ~classes:["A", ["prop", "int"]]
    ~protocols:["P", ["prop", "int"]]
    ~candidate:"A"
    ~protocol:"P"
    (Some []);
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
    (Some [Single Type.integer]);

  (* Simple method protocols *)
  assert_instantiate_protocol_parameters
    ~source:"class P(): pass"
    ~classes:["A", ["method", "typing.Callable[[int], str]"]]
    ~protocols:["P", ["method", "typing.Callable[[int], str]"]]
    ~candidate:"A"
    ~protocol:"P"
    (Some []);
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
    (Some ![Type.string]);

  (* Primitive recursive protocol, primitive recursive candidate *)
  assert_instantiate_protocol_parameters
    ~source:"class P(): pass"
    ~classes:["A", ["prop", "A"]]
    ~protocols:["P", ["prop", "P"]]
    ~candidate:"A"
    ~protocol:"P"
    (Some []);
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
    (Some ![Type.integer]);

  assert_instantiate_protocol_parameters
    ~source:{|
      T1 = typing.TypeVar("T1")
      class P(typing.Generic[T1]): pass
    |}
    ~classes:["A", ["prop", "int"; "recursive_prop", "A"]]
    ~protocols:["P", ["prop", "T1"; "recursive_prop", "P[int]"]]
    ~candidate:"A"
    ~protocol:"P"
    (Some ![Type.integer]);

  (* Protocol implicitly implemented via `__getattr__` *)
  assert_instantiate_protocol_parameters
    ~source:{|
      class P(): pass
    |}
    ~classes:["A", ["__getattr__", "typing.Callable[[], typing.Any]"]]
    ~protocols:["P", ["method", "typing.Callable[[int], str]"; "prop", "int"]]
    ~candidate:"A"
    ~protocol:"P"
    (Some []);

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
    (Some []);
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
    let class_hierarchy = GlobalResolution.create environment |> GlobalResolution.class_hierarchy in
    let order =
      {
        ConstraintsSet.class_hierarchy = hierarchy class_hierarchy;
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
    in
    TypeOrder.OrderedConstraintsSet.add
      ConstraintsSet.empty
      ~new_constraint:(LessOrEqual { left; right })
      ~order
    |> List.filter_map ~f:(OrderedConstraints.solve ~order)
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
  >::: [
         "add_constraint" >:: test_add_constraint;
         "add_constraint_recursive_type" >:: test_add_constraint_recursive_type;
         "add_constraint_type_variable_tuple" >:: test_add_constraint_type_variable_tuple;
         "instantiate_protocol_parameters" >:: test_instantiate_protocol_parameters;
         "marks_escaped_as_escaped" >:: test_mark_escaped_as_escaped;
       ]
  |> Test.run
