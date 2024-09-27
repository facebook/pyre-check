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

let environment ?source context =
  let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
    let sources = Option.value_map source ~f:(fun source -> ["test.py", source]) ~default:[] in
    ScratchProject.setup ~context sources |> ScratchProject.build_global_environment
  in
  global_environment


let hierarchy global_environment =
  let class_metadata_environment =
    AnnotatedGlobalEnvironment.ReadOnly.class_metadata_environment global_environment
  in
  let global_resolution = GlobalResolution.create global_environment in
  let class_hierarchy_handler =
    ClassSuccessorMetadataEnvironment.ReadOnly.class_hierarchy_environment
      class_metadata_environment
    |> ClassHierarchyEnvironment.ReadOnly.class_hierarchy
  in
  let has_transitive_successor = GlobalResolution.has_transitive_successor global_resolution in
  {
    ConstraintsSet.instantiate_successors_parameters =
      ClassHierarchy.instantiate_successors_parameters class_hierarchy_handler;
    has_transitive_successor;
    generic_parameters = ClassHierarchy.generic_parameters class_hierarchy_handler;
    least_upper_bound =
      ClassSuccessorMetadataEnvironment.ReadOnly.least_upper_bound class_metadata_environment;
  }


let attribute_from_attributes attributes =
  let attribute annotation ~cycle_detections ~name =
    let find attribute = String.equal (AnnotatedAttribute.name attribute) name in
    attributes annotation ~cycle_detections >>= List.find ~f:find
  in
  attribute


let make_assert_functions context =
  let environment =
    environment
      ~source:
        {|
      from typing import overload, Any, Generic

      class Base: ...
      class Child(Base): ...
      class Unrelated: ...
      T_Unconstrained = typing.TypeVar('T_Unconstrained')
      T_Bound_Base = typing.TypeVar('T_Bound_Base', bound=Base)
      T_Bound_Child = typing.TypeVar('T_Bound_Child', bound=Child)
      T_Bound_Union = typing.TypeVar('T_Bound_Union', bound=typing.Union[int, str])
      T_Bound_Union_Base_Unrelated = typing.TypeVar(
          'T_Bound_Union_Base_Unrelated',
          bound=typing.Union[Base, Unrelated]
      )
      T_Base_Unrelated = typing.TypeVar('T_Base_Unrelated', Base, Unrelated)
      T_Bound_ReadOnly = typing.TypeVar('T_Bound_ReadOnly', bound=pyre_extensions.ReadOnly[object])
      T_Bound_object = typing.TypeVar("T", object)
      T_Child_Unrelated = typing.TypeVar('T_Child_Unrelated', Child, Unrelated)
      T_Base_Unrelated_int = typing.TypeVar('T_Base_Unrelated_int', Base, Unrelated, int)
      V = pyre_extensions.ParameterSpecification("V")
      P = pyre_extensions.ParameterSpecification("P")
      P2 = pyre_extensions.ParameterSpecification("P2")

      T = typing.TypeVar('T')
      T1 = typing.TypeVar('T1')
      T2 = typing.TypeVar('T2')
      T3 = typing.TypeVar('T3')
      T4 = typing.TypeVar('T4')
      class G_invariant(typing.Generic[T]):
        pass
      T_Baseovariant = typing.TypeVar('T_Baseov', covariant=True)
      class G_covariant(typing.Generic[T_Baseovariant]):
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

      Ts = typing.TypeVarTuple("Ts")
      Ts2 = typing.TypeVarTuple("Ts2")

      class Tensor(typing.Generic[T, typing.Unpack[Ts]]): ...

      class ClassWithOverloadedConstructor(Generic[T]):
          @overload
          def __new__(cls, __x: str=...) -> ClassWithOverloadedConstructor[str]: ...
          @overload
          def __new__(cls, __x: int) -> ClassWithOverloadedConstructor[int]: ...
          def __new__(cls, __x: object=...) -> ClassWithOverloadedConstructor[Any]: ...
    |}
      context
  in
  let resolution = GlobalResolution.create environment in
  let default_postprocess annotation = Type.Variable.mark_all_variables_as_bound annotation in
  let prep annotation =
    let s =
      [
        "Base";
        "Child";
        "Unrelated";
        "T_Unconstrained";
        "T_Bound_Base";
        "T_Bound_Child";
        "T_Bound_Union_Base_Unrelated";
        "T_Bound_Union";
        "T_Bound_ReadOnly";
        "T_Bound_object";
        "T_Base_Unrelated";
        "T_Child_Unrelated";
        "T_Base_Unrelated_int";
        "V";
        "P";
        "P2";
        "T";
        "T1";
        "T2";
        "T3";
        "T4";
        "G_invariant";
        "G_covariant";
        "T_Baseovariant";
        "ClassWithOverloadedConstructor";
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
    let aliases ?replace_unbound_parameters_with_any:_ a =
      if Set.mem s a then
        Some (Type.Primitive ("test." ^ a))
      else
        GlobalResolution.get_type_alias resolution a
    in
    let variables ?replace_unbound_parameters_with_any:_ a =
      GlobalResolution.get_variable resolution a
    in
    annotation |> Type.create ~variables ~aliases |> Type.expression
  in
  let parse_annotation annotation ~do_prep =
    annotation
    |> String.substr_replace_all ~pattern:"typing.Callable[V" ~with_:"typing.Callable[test.V"
    |> parse_single_expression
    |> (if do_prep then prep else Fn.id)
    |> GlobalResolution.parse_annotation ~validation:NoValidation resolution
  in
  (* Assert that solving the constraint `left <: right` leads to `expected_solutions` for any free
     type variables in them. *)
  let assert_less_or_equal_direct
      ~left
      ~right
      ?(is_protocol = fun _ -> false)
      ?(attributes = fun _ ~cycle_detections:_ -> None)
      ?constraints
      ?(postprocess = default_postprocess)
      ?(do_prep = true)
      ~expected_solutions
      ()
    =
    let handler =
      let metaclass name ~cycle_detections:_ = GlobalResolution.metaclass resolution name in
      let order =
        {
          ConstraintsSet.class_hierarchy = hierarchy environment;
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
          metaclass;
          variance_map = AttributeResolution.variance_map;
        }
      in
      let attributes annotation ~cycle_detections =
        match attributes annotation ~cycle_detections with
        | Some attributes -> Some attributes
        | None -> (
            match Type.class_attribute_lookups_for_type annotation with
            | Some
                [{ type_for_lookup; accessed_through_class; class_name; accessed_through_readonly }]
              ->
                GlobalResolution.uninstantiated_attributes
                  resolution
                  ~transitive:true
                  ~accessed_through_class
                  class_name
                >>| List.map
                      ~f:
                        (GlobalResolution.instantiate_attribute
                           resolution
                           ~type_for_lookup
                           ~accessed_through_class
                           ~accessed_through_readonly)
            | _ -> None)
      in
      {
        order with
        instantiated_attributes = attributes;
        attribute = attribute_from_attributes attributes;
      }
    in
    let parse_annotation = parse_annotation ~do_prep in
    let expected =
      let parse_pairs pairs =
        let parse_pair (variable, value) =
          match parse_annotation variable with
          | Type.Variable variable ->
              Type.Variable.TypeVarPair (variable, parse_annotation value |> postprocess)
          | Type.Primitive primitive -> (
              let parse_parameters parameters =
                match
                  parse_annotation (Printf.sprintf "typing.Callable[%s, typing.Any]" parameters)
                  |> postprocess
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
              match GlobalResolution.get_variable global_resolution primitive with
              | Some variable_declaration -> (
                  match variable_declaration with
                  | ParamSpecVariable variable ->
                      Type.Variable.ParamSpecPair (variable, parse_parameters value)
                  | TypeVarTupleVariable variable -> (
                      match Type.Tuple (parse_ordered_types value) |> postprocess with
                      | Type.Tuple ordered_type ->
                          Type.Variable.TypeVarTuplePair (variable, ordered_type)
                      | _ -> failwith "expected a tuple")
                  | TypeVarVariable _ -> failwith "expected a param spec or type var tuple")
              | _ -> failwith "not available")
          | _ -> failwith "not a variable"
        in
        List.map pairs ~f:parse_pair
      in
      List.map expected_solutions ~f:parse_pairs |> List.map ~f:TypeConstraints.Solution.create
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
          >>| (fun bound -> Type.Variable.TypeVarPair (variable, bound))
          >>| (fun pair -> OrderedConstraints.add_lower_bound sofar ~order:handler ~pair |> unwrap)
          |> Option.value ~default:sofar
        in
        upper_bound
        >>| parse_annotation
        >>| postprocess
        >>| (fun bound -> Type.Variable.TypeVarPair (variable, bound))
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
      (TypeOrder.OrderedConstraintsSet.add_and_simplify
         [constraints]
         ~new_constraint:(LessOrEqual { left; right })
         ~order:handler
      |> List.filter_map ~f:(OrderedConstraints.solve ~order:handler))
  in
  let assert_less_or_equal ?(do_prep = true) ?(leave_unbound_in_left = []) ~left ~right =
    let parse_annotation = parse_annotation ~do_prep in
    let leave_unbound_in_left = List.map leave_unbound_in_left ~f:(fun a -> "test." ^ a) in
    let left =
      let mark_unary ({ Type.Variable.TypeVar.name; _ } as variable) =
        if List.mem leave_unbound_in_left name ~equal:Identifier.equal then
          None
        else
          Some (Type.Variable (Type.Variable.TypeVar.mark_as_bound variable))
      in
      let mark_parameter_variadic variable =
        if
          List.mem
            leave_unbound_in_left
            (Type.Variable.ParamSpec.name variable)
            ~equal:Identifier.equal
        then
          None
        else
          Some
            (Type.Callable.FromParamSpec
               { head = []; variable = Type.Variable.ParamSpec.mark_as_bound variable })
      in
      let mark_tuple_variadic variable =
        if
          List.mem
            leave_unbound_in_left
            (Type.Variable.TypeVarTuple.name variable)
            ~equal:Identifier.equal
        then
          None
        else
          Some
            (Type.Variable.TypeVarTuple.self_reference
               (Type.Variable.TypeVarTuple.mark_as_bound variable))
      in
      parse_annotation left
      |> Type.Variable.GlobalTransforms.TypeVar.replace_all mark_unary
      |> Type.Variable.GlobalTransforms.ParamSpec.replace_all mark_parameter_variadic
      |> Type.Variable.GlobalTransforms.TypeVarTuple.replace_all mark_tuple_variadic
    in
    let right = parse_annotation right in
    assert_less_or_equal_direct ~left ~right ~do_prep
  in
  assert_less_or_equal, assert_less_or_equal_direct, prep, resolution


let test_add_constraint context =
  let assert_less_or_equal, assert_less_or_equal_direct, prep, resolution =
    make_assert_functions context
  in
  assert_less_or_equal
    ~leave_unbound_in_left:["T_Unconstrained"]
    ~left:"typing.Optional[T_Unconstrained]"
    ~right:"object"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:"Base"
    ~right:"T_Unconstrained"
    ~expected_solutions:[["T_Unconstrained", "Base"]]
    ();
  assert_less_or_equal
    ~left:"Child"
    ~right:"T_Unconstrained"
    ~expected_solutions:[["T_Unconstrained", "Child"]]
    ();
  assert_less_or_equal
    ~left:"Unrelated"
    ~right:"T_Unconstrained"
    ~expected_solutions:[["T_Unconstrained", "Unrelated"]]
    ();
  assert_less_or_equal
    ~left:"Base"
    ~right:"T_Bound_Base"
    ~expected_solutions:[["T_Bound_Base", "Base"]]
    ();
  assert_less_or_equal
    ~left:"Child"
    ~right:"T_Bound_Base"
    ~expected_solutions:[["T_Bound_Base", "Child"]]
    ();
  assert_less_or_equal ~left:"Unrelated" ~right:"T_Bound_Base" ~expected_solutions:[] ();
  assert_less_or_equal ~left:"Base" ~right:"T_Bound_Child" ~expected_solutions:[] ();
  assert_less_or_equal
    ~left:"Base"
    ~right:"T_Base_Unrelated"
    ~expected_solutions:[["T_Base_Unrelated", "Base"]]
    ();

  assert_less_or_equal
    ~leave_unbound_in_left:["T_Unconstrained"]
    ~left:"T_Unconstrained"
    ~right:"typing.Any"
    ~expected_solutions:[["T_Unconstrained", "typing.Any"]]
    ();
  assert_less_or_equal
    ~leave_unbound_in_left:["T_Bound_Base"]
    ~left:"T_Bound_Base"
    ~right:"typing.Any"
    ~expected_solutions:[["T_Bound_Base", "typing.Any"]]
    ();
  assert_less_or_equal
    ~leave_unbound_in_left:["T_Base_Unrelated"]
    ~left:"T_Base_Unrelated"
    ~right:"typing.Any"
    ~expected_solutions:[["T_Base_Unrelated", "typing.Any"]]
    ();

  assert_less_or_equal
    ~leave_unbound_in_left:["T_Unconstrained"]
    ~left:"typing.List[T_Unconstrained]"
    ~right:"typing.Any"
    ~expected_solutions:[["T_Unconstrained", "typing.Any"]]
    ();

  assert_less_or_equal
    ~leave_unbound_in_left:["T_Unconstrained"]
    ~left:"typing.Tuple[T_Unconstrained, T_Unconstrained]"
    ~right:"typing.Tuple[typing.Any, int]"
    ~expected_solutions:[["T_Unconstrained", "int"]]
    ();

  assert_less_or_equal
    ~left:"typing.Any"
    ~right:"T_Unconstrained"
    ~expected_solutions:[["T_Unconstrained", "typing.Any"]]
    ();
  assert_less_or_equal
    ~left:"typing.Any"
    ~right:"T_Bound_Base"
    ~expected_solutions:[["T_Bound_Base", "typing.Any"]]
    ();
  assert_less_or_equal
    ~left:"typing.Any"
    ~right:"T_Base_Unrelated"
    ~expected_solutions:[["T_Base_Unrelated", "typing.Any"]]
    ();

  assert_less_or_equal
    ~left:"typing.Any"
    ~right:"typing.List[T_Unconstrained]"
    ~expected_solutions:[["T_Unconstrained", "typing.Any"]]
    ();

  assert_less_or_equal
    ~left:"typing.Tuple[typing.Any, int]"
    ~right:"typing.Tuple[T_Unconstrained, T_Unconstrained]"
    ~expected_solutions:[["T_Unconstrained", "int"]]
    ();

  (* Annotated types. *)
  assert_less_or_equal
    ~left:"typing.Annotated[Base, 1]"
    ~right:"T_Unconstrained"
    ~expected_solutions:[["T_Unconstrained", "Base"]]
    ();
  assert_less_or_equal
    ~left:"Base"
    ~right:"typing.Annotated[T_Unconstrained, 1]"
    ~expected_solutions:[["T_Unconstrained", "Base"]]
    ();

  assert_less_or_equal
    ~left:"typing_extensions.Annotated[Base, 1]"
    ~right:"T_Unconstrained"
    ~expected_solutions:[["T_Unconstrained", "Base"]]
    ();
  assert_less_or_equal
    ~left:"Base"
    ~right:"typing_extensions.Annotated[T_Unconstrained, 1]"
    ~expected_solutions:[["T_Unconstrained", "Base"]]
    ();

  (* An explicit type variable can only be bound to its constraints *)
  assert_less_or_equal
    ~left:"Child"
    ~right:"T_Base_Unrelated"
    ~expected_solutions:[["T_Base_Unrelated", "Base"]]
    ();
  assert_less_or_equal ~left:"Base" ~right:"T_Child_Unrelated" ~expected_solutions:[] ();
  assert_less_or_equal
    ~left:"typing.Union[int, G_invariant[str], str]"
    ~right:"T_Unconstrained"
    ~expected_solutions:[["T_Unconstrained", "typing.Union[int, G_invariant[str], str]"]]
    ();
  assert_less_or_equal
    ~left:"typing.Union[Child, Base]"
    ~right:"T_Bound_Base"
    ~expected_solutions:[["T_Bound_Base", "typing.Union[Child, Base]"]]
    ();
  assert_less_or_equal
    ~constraints:["T_Unconstrained", (Some "Unrelated", None)]
    ~left:"G_invariant[Base]"
    ~right:"G_invariant[T_Unconstrained]"
    ~expected_solutions:[]
    ();
  assert_less_or_equal
    ~constraints:["T_Unconstrained", (Some "Unrelated", None)]
    ~left:"G_covariant[Base]"
    ~right:"G_covariant[T_Unconstrained]"
    ~expected_solutions:[["T_Unconstrained", "typing.Union[Unrelated, Base]"]]
    ();
  assert_less_or_equal
    ~left:"typing.Optional[Base]"
    ~right:"typing.Optional[T_Unconstrained]"
    ~expected_solutions:[["T_Unconstrained", "Base"]]
    ();
  assert_less_or_equal
    ~left:"Base"
    ~right:"typing.Optional[T_Unconstrained]"
    ~expected_solutions:[["T_Unconstrained", "Base"]]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[Base, ...]"
    ~right:"typing.Tuple[T_Unconstrained, ...]"
    ~expected_solutions:[["T_Unconstrained", "Base"]]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[Base, Unrelated, Child]"
    ~right:"typing.Tuple[T_Unconstrained, T_Unconstrained, Base]"
    ~expected_solutions:[["T_Unconstrained", "typing.Union[Base, Unrelated]"]]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[Child, ...]"
    ~right:"typing.Tuple[T_Unconstrained, T_Unconstrained, Base]"
    ~expected_solutions:[["T_Unconstrained", "Child"]]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[Base, Unrelated, Child]"
    ~right:"typing.Tuple[T_Unconstrained, ...]"
    ~expected_solutions:[["T_Unconstrained", "typing.Union[Base, Unrelated]"]]
    ();
  assert_less_or_equal
    ~left:"G_covariant[Base]"
    ~right:"typing.Union[G_covariant[T_Unconstrained], int]"
    ~expected_solutions:[["T_Unconstrained", "Base"]]
    ();
  (* `ClassWithOverloadedConstructor.__new__` has two overloads, one accepting zero arguments and
     another accepting one argument. *)
  assert_less_or_equal
    ~left:"typing.Type[ClassWithOverloadedConstructor]"
    ~right:"typing.Callable[[], T_Unconstrained]"
    ~expected_solutions:[["T_Unconstrained", "ClassWithOverloadedConstructor[str]"]]
    ();
  assert_less_or_equal
    ~left:"typing.Type[ClassWithOverloadedConstructor]"
    ~right:"typing.Callable[[int], T_Unconstrained]"
    ~expected_solutions:[["T_Unconstrained", "ClassWithOverloadedConstructor[int]"]]
    ();
  assert_less_or_equal
    ~left:"typing.Optional[typing.Tuple[Base, Unrelated, typing.Callable[[Child, int], Base]]]"
    ~right:"typing.Optional[typing.Tuple[T, T, typing.Callable[[T, T], T]]]"
    ~expected_solutions:[]
    ();
  assert_less_or_equal
    ~left:"typing.Optional[typing.Tuple[Base, Base, typing.Callable[[Base, Base], Base]]]"
    ~right:"typing.Optional[typing.Tuple[T, T, typing.Callable[[T, T], T]]]"
    ~expected_solutions:[["T", "Base"]]
    ();
  assert_less_or_equal
    ~left:"T_Unconstrained"
    ~right:"typing.Optional[T_Unconstrained]"
    ~expected_solutions:[["T_Unconstrained", "T_Unconstrained"]; ["T_Unconstrained", "object"]]
    ();
  assert_less_or_equal
    ~left:"T_Bound_Union"
    ~right:"typing.Union[int, str]"
    ~expected_solutions:[[]]
    ();

  (* Bound => Bound *)
  assert_less_or_equal
    ~left:"T_Bound_Child"
    ~right:"T_Bound_Base"
    ~expected_solutions:[["T_Bound_Base", "T_Bound_Child"]]
    ();
  assert_less_or_equal ~left:"T_Bound_Base" ~right:"T_Bound_Child" ~expected_solutions:[] ();
  assert_less_or_equal
    ~left:"T_Bound_Union"
    ~right:"T_Bound_Union"
    ~expected_solutions:[["T_Bound_Union", "T_Bound_Union"]]
    ();

  (* Bound => Explicit *)
  assert_less_or_equal
    ~left:"T_Bound_Child"
    ~right:"T_Base_Unrelated"
    ~expected_solutions:[["T_Base_Unrelated", "Base"]]
    ();
  assert_less_or_equal ~left:"T_Bound_Base" ~right:"T_Child_Unrelated" ~expected_solutions:[] ();

  (* Explicit => Bound *)
  assert_less_or_equal
    ~left:"T_Child_Unrelated"
    ~right:"T_Bound_Union_Base_Unrelated"
    ~expected_solutions:[["T_Bound_Union_Base_Unrelated", "T_Child_Unrelated"]]
    ();
  assert_less_or_equal ~left:"T_Child_Unrelated" ~right:"T_Bound_Child" ~expected_solutions:[] ();

  (* Explicit => Explicit *)
  assert_less_or_equal
    ~left:"T_Base_Unrelated"
    ~right:"T_Base_Unrelated_int"
    ~expected_solutions:[["T_Base_Unrelated_int", "T_Base_Unrelated"]]
    ();

  (* This one is theoretically solvable, but only if we're willing to introduce dependent variables
     as the only sound solution here would be
   *  T_Base_Unrelated_int => T_new <: Base if T_Child_Unrelated is Child, Unrelated if T_Child_Unrelated is Unrelated *)
  assert_less_or_equal
    ~left:"T_Child_Unrelated"
    ~right:"T_Base_Unrelated_int"
    ~expected_solutions:[]
    ();
  assert_less_or_equal
    ~leave_unbound_in_left:["T_Unconstrained"]
    ~left:"typing.Callable[[T_Unconstrained], T_Unconstrained]"
    ~right:"typing.Callable[[int], int]"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:"typing.Callable[[int], int]"
    ~right:"typing.Callable[[T_Unconstrained], T_Unconstrained]"
    ~expected_solutions:[["T_Unconstrained", "int"]]
    ();
  assert_less_or_equal
    ~leave_unbound_in_left:["T"]
    ~left:"typing.Callable[[T], T]"
    ~right:"typing.Callable[[T_Unconstrained], T_Unconstrained]"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~leave_unbound_in_left:["T"]
    ~left:"typing.Callable[[T], G_invariant[T]]"
    ~right:"typing.Callable[[T_Unconstrained], T_Unconstrained]"
    ~expected_solutions:[]
    ();
  assert_less_or_equal
    ~leave_unbound_in_left:["T1"]
    ~left:"typing.Callable[[T1], typing.Tuple[T1, T2]]"
    ~right:"typing.Callable[[T3], typing.Tuple[T3, T4]]"
    ~expected_solutions:[["T4", "T2"]]
    ();
  assert_less_or_equal
    ~left:"typing.Type[Constructable]"
    ~right:"typing.Callable[[T3], T4]"
    ~expected_solutions:[["T3", "int"; "T4", "Constructable"]]
    ();
  assert_less_or_equal
    ~left:"typing.Callable[[typing.Union[int, str]], str]"
    ~right:"typing.Callable[[int], T4]"
    ~expected_solutions:[["T4", "str"]]
    ();
  assert_less_or_equal
    ~left:"typing.Callable[[typing.Union[int, str], int], str]"
    ~right:"typing.Callable[[int, T3], T4]"
    ~expected_solutions:[["T3", "int"; "T4", "str"]]
    ();
  assert_less_or_equal
    ~do_prep:false
    ~leave_unbound_in_left:["T3"]
    ~left:"typing.Callable[[test.T3], test.T3]"
    ~right:"typing.Callable[[typing.Union[int, str]], object][[[int], test.T1][[str], test.T2]] "
    ~expected_solutions:[["test.T2", "str"; "test.T1", "int"]]
    ();
  assert_less_or_equal
    ~leave_unbound_in_left:["T_Unconstrained"]
    ~left:"typing.Callable[[Variable(T_Unconstrained), Keywords(T_Unconstrained)], T_Unconstrained]"
    ~right:"typing.Callable[[Named(a, int), Named(b, str)], T1]"
    ~expected_solutions:[["test.T1", "typing.Union[int, str]"]]
    ();
  assert_less_or_equal
    ~leave_unbound_in_left:["T_Unconstrained"]
    ~left:
      "typing.Callable[[Variable(typing.Sequence[T_Unconstrained]), \
       Keywords(typing.Sequence[T_Unconstrained])], T_Unconstrained]"
    ~right:"typing.Callable[[Named(a, int), Named(b, str)], T1]"
    ~expected_solutions:[]
    ();
  assert_less_or_equal
    ~leave_unbound_in_left:["T_Unconstrained"]
    ~left:
      "typing.Callable[[Variable(typing.Sequence[T_Unconstrained]), \
       Keywords(typing.Sequence[T_Unconstrained])], T_Unconstrained]"
    ~right:"typing.Callable[[Named(a, typing.List[int]), Named(b, typing.List[str])], T1]"
    ~expected_solutions:[["test.T1", "typing.Union[int, str]"]]
    ();

  (* Callback protocols *)
  let parse_annotation annotation =
    annotation |> parse_single_expression |> prep |> GlobalResolution.parse_annotation resolution
  in
  let is_protocol annotation =
    match annotation with
    | Type.Parametric { name = "test.G_invariant"; _ } -> true
    | _ -> false
  in
  let attributes annotation ~cycle_detections:_ =
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
  assert_less_or_equal
    ~is_protocol
    ~attributes
    ~left:"typing.Callable[[int], str]"
    ~right:"G_invariant[T1]"
    ~expected_solutions:[["T1", "int"]]
    ();
  let attributes annotation ~cycle_detections:_ =
    match annotation with
    | Type.Primitive "HasBoundMethodCall" ->
        Some
          (parse_attributes
             ~parse_annotation
             ~class_name:"HasBoundMethodCall"
             ["__call__", "BoundMethod[typing.Callable[[int, str], bool], int]"])
    | _ -> None
  in
  assert_less_or_equal
    ~attributes
    ~left:"HasBoundMethodCall"
    ~right:"typing.Callable[[str], bool]"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~attributes
    ~left:"HasBoundMethodCall"
    ~right:"typing.Callable[[int], bool]"
    ~expected_solutions:[]
    ();

  (* Multiple options *)
  assert_less_or_equal
    ~left:"int"
    ~right:"typing.Union[T1, T2]"
    ~expected_solutions:[["T1", "int"]; ["T2", "int"]]
    ();
  assert_less_or_equal ~left:"int" ~right:"typing.Union[T1, T2, int]" ~expected_solutions:[[]] ();
  assert_less_or_equal
    ~left:"typing.List[int]"
    ~right:"typing.Union[typing.List[T1], T1]"
    ~expected_solutions:[["T1", "int"]; ["T1", "typing.List[int]"]]
    ();
  assert_less_or_equal
    ~left:"typing.List[int]"
    ~right:"typing.Union[typing.List[int], T1]"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[typing.List[int], typing.List[int]]"
    ~right:"typing.Tuple[typing.Union[typing.List[T1], T1], T1]"
    ~expected_solutions:[["T1", "typing.List[int]"]]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[typing.List[int], typing.List[int]]"
    ~right:"typing.Tuple[typing.Union[typing.List[T1], T1], T1]"
    ~expected_solutions:[["T1", "typing.List[int]"]]
    ();
  assert_less_or_equal
    ~do_prep:false
    ~left:
      ("typing.Callable[[typing.Union[int, str]], typing.Union[int, str]]"
      ^ "[[[int], str][[str], int]]")
    ~right:"typing.Callable[[test.T3], test.T4]"
    ~expected_solutions:[["test.T3", "int"; "test.T4", "str"]; ["test.T3", "str"; "test.T4", "int"]]
    ();

  (* Free Variable <-> Free Variable constraints *)
  assert_less_or_equal
    ~postprocess:Fn.id
    ~leave_unbound_in_left:["T1"]
    ~left:"T1"
    ~right:"T2"
    ~expected_solutions:[["T2", "T1"]; ["T1", "T2"]]
    ();
  assert_less_or_equal
    ~postprocess:Fn.id
    ~leave_unbound_in_left:["T"]
    ~left:"typing.Callable[[T], T]"
    ~right:"typing.Callable[[T1], T2]"
    ~expected_solutions:[["T2", "T1"]; ["T1", "T2"]]
    ();
  assert_less_or_equal
    ~leave_unbound_in_left:["T"]
    ~left:"typing.Tuple[typing.Callable[[T], T], int]"
    ~right:"typing.Tuple[typing.Callable[[T1], T2], T1]"
    ~expected_solutions:[["T2", "int"; "T1", "int"]]
    ();
  assert_less_or_equal
    ~left:"typing.Callable[[int, int], int]"
    ~right:"typing.Callable[V, int]"
    ~expected_solutions:[["V", "[int, int]"]]
    ();

  (* We need to ensure that return values are still checked *)
  assert_less_or_equal
    ~left:"typing.Callable[[int, int], int]"
    ~right:"typing.Callable[V, str]"
    ~expected_solutions:[]
    ();
  assert_less_or_equal
    ~left:"typing.Callable[[int, int], int]"
    ~right:"typing.Callable[V, T1]"
    ~expected_solutions:[["T1", "int"; "V", "[int, int]"]]
    ();

  (* We should be able to capture the same parameter set twice *)
  assert_less_or_equal
    ~left:"typing.Tuple[typing.Callable[[int, int], int], typing.Callable[[int, int], int]]"
    ~right:"typing.Tuple[typing.Callable[V, int], typing.Callable[V, int]]"
    ~expected_solutions:[["V", "[int, int]"]]
    ();

  (* We currently do not find a way to take both [int, int] and [int, str]. A correct solution would
     be [int, Intersection[int, str]]. This is probably not desired *)
  assert_less_or_equal
    ~left:"typing.Tuple[typing.Callable[[int, int], int], typing.Callable[[int, str], int]]"
    ~right:"typing.Tuple[typing.Callable[V, int], typing.Callable[V, int]]"
    ~expected_solutions:[]
    ();

  (* There is no common interface between a callable that requires exactly two arguments and one
     that requires exactly one *)
  assert_less_or_equal
    ~left:"typing.Tuple[typing.Callable[[int, int], int], typing.Callable[[int], int]]"
    ~right:"typing.Tuple[typing.Callable[V, int], typing.Callable[V, int]]"
    ~expected_solutions:[]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[typing.Callable[[int, int], int], typing.Callable[[int], int]]"
    ~right:"typing.Tuple[typing.Callable[V, int], typing.Callable[V, int]]"
    ~expected_solutions:[]
    ();

  assert_less_or_equal
    ~left:"typing.Callable[..., int]"
    ~right:"typing.Callable[..., object]"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:"typing.Callable[..., int]"
    ~right:"typing.Callable[[Named(a, int), Named(b, str)], int]"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:"typing.Callable[[Named(a, int), Named(b, str)], int]"
    ~right:"typing.Callable[[int, str], int]"
    ~expected_solutions:[[]]
    ();

  assert_less_or_equal
    ~left:"typing.Union[typing.Type[test.ChildA], typing.Type[test.ChildB]]"
    ~right:"typing.Callable[[], test.Parent]"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal ~left:"typing.Type[test.ChildA]" ~right:"test.Meta" ~expected_solutions:[] ();
  assert_less_or_equal
    ~left:"typing.Type[test.HasMeta]"
    ~right:"test.Meta"
    ~expected_solutions:[[]]
    ();
  (* BoundMethods are Callable, but Callables can't replace a BoundMethod *)
  assert_less_or_equal
    ~left:"BoundMethod[typing.Callable[[int, str], bool], int]"
    ~right:"typing.Callable[[str], bool]"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:"typing.Callable[[str], bool]"
    ~right:"BoundMethod[typing.Callable[[int, str], bool], int]"
    ~expected_solutions:[]
    ();

  let { Type.Variable.ParamSpec.Components.positional_component; keyword_component } =
    Type.Variable.ParamSpec.create "TParams" |> Type.Variable.ParamSpec.decompose
  in

  assert_less_or_equal_direct
    ~left:positional_component
    ~right:(parse_annotation "typing.Tuple[object, ...]")
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal_direct
    ~left:positional_component
    ~right:(parse_annotation "typing.Iterable[object]")
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal_direct
    ~left:positional_component
    ~right:(parse_annotation "typing.Iterable[int]")
    ~expected_solutions:[]
    ();

  assert_less_or_equal_direct
    ~left:keyword_component
    ~right:(parse_annotation "typing.Mapping[str, object]")
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal_direct
    ~left:keyword_component
    ~right:(parse_annotation "typing.Mapping[str, int]")
    ~expected_solutions:[]
    ();

  (* Literals. *)
  assert_less_or_equal
    ~left:{| typing_extensions.Literal["hello"] |}
    ~right:"str"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:{| typing_extensions.Literal["hello"] |}
    ~right:{| typing_extensions.Literal["world"] |}
    ~expected_solutions:[]
    ();
  assert_less_or_equal
    ~left:{| typing_extensions.Literal["hello"] |}
    ~right:"typing_extensions.Literal[str]"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:{| typing_extensions.Literal[str] |}
    ~right:{| typing_extensions.Literal[str] |}
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:{| typing_extensions.Literal[str] |}
    ~right:"str"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:"str"
    ~right:{| typing_extensions.Literal["hello"] |}
    ~expected_solutions:[]
    ();
  assert_less_or_equal
    ~left:"str"
    ~right:{| typing_extensions.Literal[str] |}
    ~expected_solutions:[]
    ();

  (* Top type. *)
  assert_less_or_equal_direct ~left:Type.Top ~right:Type.none ~expected_solutions:[] ();
  ()


let test_add_constraint_recursive_type context =
  let _, assert_less_or_equal_direct, _, _ = make_assert_functions context in
  let tree_annotation =
    Type.RecursiveType.create
      ~name:"test.Tree"
      ~body:
        (Type.union
           [Type.integer; Type.tuple [Type.Primitive "test.Tree"; Type.Primitive "test.Tree"]])
  in
  assert_less_or_equal_direct
    ~left:tree_annotation
    ~right:tree_annotation
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal_direct ~left:Type.integer ~right:tree_annotation ~expected_solutions:[[]] ();
  assert_less_or_equal_direct ~left:Type.string ~right:tree_annotation ~expected_solutions:[] ();
  assert_less_or_equal_direct ~left:Type.Top ~right:tree_annotation ~expected_solutions:[] ();
  assert_less_or_equal_direct
    ~left:(Type.tuple [Type.integer; Type.integer])
    ~right:tree_annotation
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal_direct
    ~left:(Type.union [Type.integer; Type.tuple [Type.integer; Type.integer]])
    ~right:tree_annotation
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal_direct
    ~left:(Type.tuple [Type.integer; Type.tuple [Type.integer; Type.integer]])
    ~right:tree_annotation
    ~expected_solutions:[[]]
    ();
  (* Should fail because of the string. *)
  assert_less_or_equal_direct
    ~left:(Type.tuple [Type.integer; Type.tuple [Type.integer; Type.string]])
    ~right:tree_annotation
    ~expected_solutions:[]
    ();

  assert_less_or_equal_direct ~left:tree_annotation ~right:Type.integer ~expected_solutions:[] ();
  assert_less_or_equal_direct
    ~left:tree_annotation
    ~right:(Type.union [Type.integer; tree_annotation])
    ~expected_solutions:[[]]
    ();
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
  assert_less_or_equal_direct
    ~left:tree_annotation
    ~right:isomorphic_tree_annotation
    ~expected_solutions:[[]]
    ();

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
  assert_less_or_equal_direct
    ~left:json_annotation
    ~right:json_annotation
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal_direct ~left:Type.integer ~right:json_annotation ~expected_solutions:[[]] ();
  assert_less_or_equal_direct ~left:Type.string ~right:json_annotation ~expected_solutions:[] ();
  assert_less_or_equal_direct
    ~left:(Type.dictionary ~key:Type.string ~value:Type.integer)
    ~right:json_annotation
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal_direct
    ~left:
      (Type.dictionary
         ~key:Type.string
         ~value:(Type.dictionary ~key:Type.string ~value:Type.integer))
    ~right:json_annotation
    ~expected_solutions:[[]]
    ();
  let readonly_tree_annotation =
    Type.RecursiveType.create
      ~name:"test.Tree"
      ~body:
        (Type.union
           [
             Type.PyreReadOnly.create Type.string;
             Type.tuple [Type.Primitive "test.Tree"; Type.Primitive "test.Tree"];
           ])
  in
  assert_less_or_equal_direct
    ~left:(Type.PyreReadOnly.create Type.string)
    ~right:readonly_tree_annotation
    ~expected_solutions:[[]]
    ();
  ()


let test_add_constraint_type_variable_tuple context =
  let assert_less_or_equal, assert_less_or_equal_direct, _, _ = make_assert_functions context in
  assert_less_or_equal
    ~left:"typing.Tuple[int, str, bool]"
    ~right:"typing.Tuple[int, typing.Unpack[Ts]]"
    ~expected_solutions:[["Ts", "typing.Tuple[str, bool]"]]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[int, str, bool]"
    ~right:"typing.Tuple[int, typing.Unpack[Ts], bool]"
    ~expected_solutions:[["Ts", "typing.Tuple[str]"]]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[int]"
    ~right:"typing.Tuple[int, typing.Unpack[Ts], bool]"
    ~expected_solutions:[]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[int, str, bool]"
    ~right:"typing.Tuple[typing.Unpack[Ts], T]"
    ~expected_solutions:[["Ts", "typing.Tuple[int, str]"; "T", "bool"]]
    ();
  assert_less_or_equal
    ~leave_unbound_in_left:["Ts"]
    ~left:"typing.Tuple[int, typing.Unpack[Ts]]"
    ~right:"typing.Tuple[int, str, bool]"
    ~expected_solutions:[["Ts", "typing.Tuple[str, bool]"]]
    ();
  (* Ts is bound on the left side. We fail because Ts could be any tuple. *)
  assert_less_or_equal
    ~left:"typing.Tuple[int, typing.Unpack[Ts]]"
    ~right:"typing.Tuple[int, str, bool]"
    ~expected_solutions:[]
    ();
  (* This ends up checking `[int, str] <: *Ts (bound)`, which is not valid. *)
  assert_less_or_equal
    ~left:"typing.Callable[[typing.Tuple[typing.Unpack[Ts]]], bool]"
    ~right:"typing.Callable[[typing.Tuple[int, str]], bool]"
    ~expected_solutions:[]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[typing.Unpack[Ts]]"
    ~right:"typing.Tuple[typing.Unpack[Ts2]]"
    ~expected_solutions:[["Ts2", "typing.Tuple[typing.Unpack[Ts]]"]]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[int, typing.Unpack[Ts], str]"
    ~right:"typing.Tuple[typing.Unpack[Ts2]]"
    ~expected_solutions:[["Ts2", "typing.Tuple[int, typing.Unpack[Ts], str]"]]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[int, bool, typing.Unpack[Ts], bool, str]"
    ~right:"typing.Tuple[int, typing.Unpack[Ts2], str]"
    ~expected_solutions:[["Ts2", "typing.Tuple[bool, typing.Unpack[Ts], bool]"]]
    ();
  (* No solution because the middle portion is `[*Ts (bound)] <: [bool, *Ts2 (free), bool]`. The
     bound `Ts` may not start with `bool`. *)
  assert_less_or_equal
    ~left:"typing.Tuple[int, typing.Unpack[Ts], str]"
    ~right:"typing.Tuple[int, bool, typing.Unpack[Ts2], bool, str]"
    ~expected_solutions:[]
    ();
  let variadic = Type.Variable.TypeVarTuple.create "test.Ts" in
  let variadic2 = Type.Variable.TypeVarTuple.create "test.Ts2" in
  assert_less_or_equal_direct
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
    ~expected_solutions:[["Ts", "typing.Tuple[int, typing.Unpack[Ts2], str]"]]
    ();
  (* Tuple is covariant. *)
  assert_less_or_equal
    ~left:"typing.Tuple[int, str]"
    ~right:"typing.Tuple[float, str]"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[int, typing.Unpack[Ts]]"
    ~right:"typing.Tuple[object, ...]"
    ~expected_solutions:[[]]
    ();

  (* Parametric types. *)
  assert_less_or_equal
    ~left:"test.Tensor[str, bool, str]"
    ~right:"test.Tensor[str, typing.Unpack[Ts]]"
    ~expected_solutions:[["Ts", "typing.Tuple[bool, str]"]]
    ();
  (* Tensor is invariant in the datatype. *)
  assert_less_or_equal
    ~left:"Tensor[int, int]"
    ~right:"Tensor[float, int]"
    ~expected_solutions:[]
    ();
  (* Tensor is covariant in the shape, since the shape is immutable. *)
  assert_less_or_equal
    ~left:"Tensor[int, int, int]"
    ~right:"Tensor[int, float, float]"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:"test.Tensor[str, int]"
    ~right:"test.Tensor[str, typing.Unpack[typing.Tuple[int, ...]]]"
    ~expected_solutions:[[]]
    ();

  (* Callable. *)
  assert_less_or_equal
    ~left:"typing.Callable[[int, str, bool], None]"
    ~right:"typing.Callable[[int, typing.Unpack[Ts]], None]"
    ~expected_solutions:[["Ts", "typing.Tuple[str, bool]"]]
    ();
  assert_less_or_equal
    ~left:"typing.Callable[[int, bool, typing.Unpack[Ts], bool, str], None]"
    ~right:"typing.Callable[[int, typing.Unpack[Ts2], str], None]"
    ~expected_solutions:[["Ts2", "typing.Tuple[bool, typing.Unpack[Ts], bool]"]]
    ();
  (* List of empty list means we found a satisfying solution. *)
  assert_less_or_equal
    ~left:"typing.Callable[[int, typing.Unpack[Ts]], typing.Tuple[typing.Unpack[Ts]]]"
    ~right:"typing.Callable[[int, str, bool], typing.Tuple[str, bool]]"
    ~leave_unbound_in_left:["Ts"]
    ~expected_solutions:[[]]
    ();

  (* Unbounded tuples. *)
  assert_less_or_equal
    ~left:"typing.Tuple[int, ...]"
    ~right:"typing.Tuple[typing.Unpack[Ts]]"
    ~expected_solutions:[["Ts", "typing.Tuple[typing.Unpack[typing.Tuple[int, ...]]]"]]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[int, ...]"
    ~right:"typing.Tuple[T]"
    ~expected_solutions:[["T", "int"]]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[int, str, typing.Unpack[typing.Tuple[str, ...]], bool]"
    ~right:"typing.Tuple[int, typing.Unpack[Ts], bool]"
    ~expected_solutions:[["Ts", "typing.Tuple[str, typing.Unpack[typing.Tuple[str, ...]]]"]]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[int, typing.Unpack[Ts], bool]"
    ~right:"typing.Tuple[int, str, typing.Unpack[typing.Tuple[str, ...]], bool]"
    ~leave_unbound_in_left:["Ts"]
    ~expected_solutions:[["Ts", "typing.Tuple[str, typing.Unpack[typing.Tuple[str, ...]]]"]]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[int, str, typing.Unpack[typing.Tuple[str, ...]], str, bool]"
    ~right:"typing.Tuple[int, typing.Unpack[typing.Tuple[T, ...]], bool]"
    ~expected_solutions:[["T", "str"]]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[int, typing.Unpack[typing.Tuple[str, ...]], str]"
    ~right:"typing.Tuple[T1, T2, T3, T4]"
    ~expected_solutions:[["T4", "str"; "T3", "str"; "T2", "str"; "T1", "int"]]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[int, int]"
    ~right:"typing.Tuple[int, ...]"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[str, ...]"
    ~right:"typing.Tuple[int]"
    ~expected_solutions:[]
    ();

  (* Stretch the unbounded tuple to meet the expected length. *)
  assert_less_or_equal
    ~left:"typing.Tuple[int, ...]"
    ~right:"typing.Tuple[int, typing.Unpack[Ts]]"
    ~expected_solutions:[["Ts", "typing.Tuple[int, ...]"]]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[int, ...]"
    ~right:"typing.Tuple[typing.Unpack[Ts], int]"
    ~expected_solutions:[["Ts", "typing.Tuple[int, ...]"]]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[int, ...]"
    ~right:"typing.Tuple[typing.Unpack[Ts], str]"
    ~expected_solutions:[]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[int, typing.Unpack[typing.Tuple[str, ...]], bool]"
    ~right:"typing.Tuple[int, str, typing.Unpack[typing.Tuple[str, ...]], bool]"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[int, str, typing.Unpack[typing.Tuple[str, ...]], bool]"
    ~right:"typing.Tuple[int, typing.Unpack[typing.Tuple[str, ...]], T, bool]"
    ~expected_solutions:[["T", "str"]]
    ();
  assert_less_or_equal
    ~left:"typing.Tuple[int, str, typing.Unpack[typing.Tuple[str, ...]], bool]"
    ~right:"typing.Tuple[int, typing.Unpack[typing.Tuple[str, ...]], int, bool]"
    ~expected_solutions:[]
    ();
  ()


let test_add_constraint_readonly context =
  let assert_less_or_equal, assert_less_or_equal_direct, _, _ = make_assert_functions context in
  assert_less_or_equal
    ~left:"pyre_extensions.ReadOnly[Base]"
    ~right:"Base"
    ~expected_solutions:[]
    ();
  assert_less_or_equal
    ~left:"Child"
    ~right:"pyre_extensions.ReadOnly[Base]"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:"pyre_extensions.ReadOnly[Child]"
    ~right:"pyre_extensions.ReadOnly[Base]"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:"pyre_extensions.ReadOnly[Child]"
    ~right:"typing.Union[int, pyre_extensions.ReadOnly[Base]]"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:"pyre_extensions.ReadOnly[Base]"
    ~right:"T_Unconstrained"
    ~expected_solutions:[["T_Unconstrained", "pyre_extensions.ReadOnly[Base]"]]
    ();
  assert_less_or_equal
    ~left:"Base"
    ~right:"pyre_extensions.ReadOnly[T_Unconstrained]"
    ~expected_solutions:[["T_Unconstrained", "Base"]]
    ();
  assert_less_or_equal
    ~left:"pyre_extensions.ReadOnly[Base]"
    ~right:"T_Bound_ReadOnly"
    ~expected_solutions:[["T_Bound_ReadOnly", "pyre_extensions.ReadOnly[Base]"]]
    ();
  assert_less_or_equal
    ~left:"T_Unconstrained"
    ~right:"pyre_extensions.ReadOnly[Base]"
    ~leave_unbound_in_left:["T_Unconstrained"]
    ~expected_solutions:[["T_Unconstrained", "pyre_extensions.ReadOnly[Base]"]]
    ();
  assert_less_or_equal
    ~left:"pyre_extensions.ReadOnly[Base]"
    ~right:"T_Bound_object"
    ~expected_solutions:[]
    ();
  assert_less_or_equal
    ~left:"pyre_extensions.ReadOnly[Base]"
    ~right:"typing.Any"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:"typing.Any"
    ~right:"pyre_extensions.ReadOnly[T]"
    ~expected_solutions:[["T", "typing.Any"]]
    ();
  assert_less_or_equal
    ~left:"pyre_extensions.ReadOnly[Base]"
    ~right:"object"
    ~expected_solutions:[]
    ();
  assert_less_or_equal
    ~left:"typing.Union[pyre_extensions.ReadOnly[Base], str]"
    ~right:"object"
    ~expected_solutions:[]
    ();
  assert_less_or_equal
    ~left:"object"
    ~right:"pyre_extensions.ReadOnly[object]"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal_direct
    ~left:Type.Bottom
    ~right:(Type.PyreReadOnly.create (Type.Primitive "Base"))
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal_direct
    ~left:(Type.PyreReadOnly.create (Type.Primitive "Base"))
    ~right:Type.Bottom
    ~expected_solutions:[]
    ();
  assert_less_or_equal_direct
    ~left:Type.Top
    ~right:(Type.PyreReadOnly.create Type.object_primitive)
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:"None"
    ~right:"pyre_extensions.ReadOnly[object]"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:"None"
    ~right:"pyre_extensions.ReadOnly[Base]"
    ~expected_solutions:[]
    ();
  assert_less_or_equal
    ~left:"typing.Union[pyre_extensions.ReadOnly[Base], pyre_extensions.ReadOnly[Child]]"
    ~right:"pyre_extensions.ReadOnly[Base]"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:"typing.Optional[pyre_extensions.ReadOnly[Base]]"
    ~right:"pyre_extensions.ReadOnly[object]"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:"typing.Optional[pyre_extensions.ReadOnly[Base]]"
    ~right:"pyre_extensions.ReadOnly[Base]"
    ~expected_solutions:[]
    ();
  ()


let test_add_constraint_parameter_specification context =
  let assert_less_or_equal, _, _, _ = make_assert_functions context in
  assert_less_or_equal
    ~left:"typing.Callable[P, None]"
    ~right:"typing.Callable[P2, None]"
    ~expected_solutions:[["P2", "P"]]
    ();
  assert_less_or_equal
    ~leave_unbound_in_left:["P"]
    ~left:"typing.Callable[P, None]"
    ~right:"typing.Callable[P2, None]"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~left:"typing.Callable[pyre_extensions.type_variable_operators.Concatenate[int, P], None]"
    ~right:"typing.Callable[pyre_extensions.type_variable_operators.Concatenate[int, P2], None]"
    ~expected_solutions:[["P2", "P"]]
    ();
  assert_less_or_equal
    ~left:"typing.Callable[typing_extensions.Concatenate[int, P], None]"
    ~right:"typing.Callable[typing_extensions.Concatenate[int, P2], None]"
    ~expected_solutions:[["P2", "P"]]
    ();
  assert_less_or_equal
    ~left:"typing.Callable[typing.Concatenate[int, P], None]"
    ~right:"typing.Callable[typing.Concatenate[int, P2], None]"
    ~expected_solutions:[["P2", "P"]]
    ();
  assert_less_or_equal
    ~leave_unbound_in_left:["P"]
    ~left:"typing.Callable[typing.Concatenate[int, str, P], None]"
    ~right:"typing.Callable[typing.Concatenate[int, str, P2], None]"
    ~expected_solutions:[[]]
    ();
  assert_less_or_equal
    ~leave_unbound_in_left:["P"]
    ~left:"typing.Callable[typing.Concatenate[int, int, P], None]"
    ~right:"typing.Callable[typing.Concatenate[int, str, P2], None]"
    ~expected_solutions:[]
    ();
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
      optional >>| Format.asprintf "%a" Type.Argument.pp_list |> Option.value ~default:"None"
    in
    let parse_attributes =
      let parse_class (class_name, attributes) =
        substitute class_name, parse_attributes attributes ~class_name ~parse_annotation
      in
      List.map ~f:parse_class
    in
    let order =
      let classes, protocols = parse_attributes classes, parse_attributes protocols in
      let attributes annotation ~cycle_detections:_ =
        match annotation with
        | Type.Parametric { name = primitive; _ }
        | Type.Primitive primitive ->
            List.Assoc.find (classes @ protocols) primitive ~equal:String.equal
        | _ -> None
      in
      let is_protocol annotation =
        match Type.split annotation with
        | Type.Primitive primitive, _ -> List.Assoc.mem protocols primitive ~equal:String.equal
        | _ -> false
      in
      {
        ConstraintsSet.class_hierarchy = hierarchy environment;
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
        variance_map = AttributeResolution.variance_map;
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
    let order =
      {
        ConstraintsSet.class_hierarchy = hierarchy environment;
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
        variance_map = AttributeResolution.variance_map;
      }
    in
    TypeOrder.OrderedConstraintsSet.add_and_simplify
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
         "add_constraint_readonly" >:: test_add_constraint_readonly;
         "add_constraint_parameter_specification" >:: test_add_constraint_parameter_specification;
         "instantiate_protocol_parameters" >:: test_instantiate_protocol_parameters;
         "marks_escaped_as_escaped" >:: test_mark_escaped_as_escaped;
       ]
  |> Test.run
