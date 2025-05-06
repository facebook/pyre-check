(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* A GlobalResolution.t is a wrapper around all the environment layers prior
 * to TypeEnvironment. It serves two purposes:
 * (a) It abstracts the details of accessing lower environment layers into a single
 *     interface representing all global-scope information for Pyre analysis
 * (b) It allows callers to specify a `dependency` that will be used in all
 *     environment accesses. This makes it easy to avoid forgetting to pass a
 *     dependency in very complex logic like typeCheck.ml because the dependency
 *     only needs to be specified once when we create the GlobalResolution.t.
 *)

open Core
open Pyre
open Ast

type t = {
  dependency: SharedMemoryKeys.DependencyKey.registered option;
  annotated_global_environment: AnnotatedGlobalEnvironment.ReadOnly.t;
}

let create ?dependency annotated_global_environment = { annotated_global_environment; dependency }

let annotated_global_environment { annotated_global_environment; _ } = annotated_global_environment

let function_definition_environment resolution =
  annotated_global_environment resolution
  |> AnnotatedGlobalEnvironment.ReadOnly.function_definition_environment


let attribute_resolution resolution =
  function_definition_environment resolution
  |> FunctionDefinitionEnvironment.ReadOnly.attribute_resolution


let class_metadata_environment resolution =
  annotated_global_environment resolution
  |> AnnotatedGlobalEnvironment.ReadOnly.class_metadata_environment


let class_hierarchy_environment resolution =
  class_metadata_environment resolution
  |> ClassSuccessorMetadataEnvironment.ReadOnly.class_hierarchy_environment


let alias_environment resolution =
  ClassHierarchyEnvironment.ReadOnly.alias_environment (class_hierarchy_environment resolution)


let unannotated_global_environment resolution =
  alias_environment resolution |> TypeAliasEnvironment.ReadOnly.unannotated_global_environment


let source_code_api ({ dependency; _ } as resolution) =
  match dependency with
  | Some dependency ->
      unannotated_global_environment resolution
      |> UnannotatedGlobalEnvironment.ReadOnly.get_tracked_source_code_api ~dependency
  | None ->
      unannotated_global_environment resolution
      |> UnannotatedGlobalEnvironment.ReadOnly.get_untracked_source_code_api


(* Note that both of the path lookups are not dependency tracked! It turns out they are only used
   for special things like error messages where it winds up not mattering, but this is a very sharp
   edge in our incremental system. *)

let module_path_of_qualifier resolution =
  source_code_api resolution |> SourceCodeApi.module_path_of_qualifier


let relative_path_of_qualifier resolution =
  source_code_api resolution |> SourceCodeApi.relative_path_of_qualifier


let source_of_qualifier resolution = source_code_api resolution |> SourceCodeApi.source_of_qualifier

let is_protocol ({ dependency; _ } as resolution) annotation =
  UnannotatedGlobalEnvironment.ReadOnly.is_protocol
    (unannotated_global_environment resolution)
    ?dependency
    annotation


let is_special_form ({ dependency; _ } as resolution) annotation =
  UnannotatedGlobalEnvironment.ReadOnly.is_special_form
    (unannotated_global_environment resolution)
    ?dependency
    annotation


let first_matching_class_decorator ({ dependency; _ } as resolution) =
  UnannotatedGlobalEnvironment.ReadOnly.first_matching_class_decorator
    (unannotated_global_environment resolution)
    ?dependency


let get_class_summary ({ dependency; _ } as resolution) =
  UnannotatedGlobalEnvironment.ReadOnly.get_class_summary
    (unannotated_global_environment resolution)
    ?dependency


(* This will return an empty list if the qualifier isn't part of the project we are type
   checking. *)
let get_define_names_for_qualifier_in_project ({ dependency; _ } as resolution) =
  FunctionDefinitionEnvironment.ReadOnly.define_names_of_qualifier
    ?dependency
    (function_definition_environment resolution)


(* This will return None if called on a function definition that is not part of the project we are
   type checking (i.e. defined in dependencies). *)
let get_function_definition_in_project ({ dependency; _ } as resolution) =
  FunctionDefinitionEnvironment.ReadOnly.function_definition
    ?dependency
    (function_definition_environment resolution)


(* This will return None if called on a function definition that is not part of the project we are
   type checking (i.e. defined in dependencies). *)
let get_define_body_in_project resolution name =
  get_function_definition_in_project resolution name >>= fun { FunctionDefinition.body; _ } -> body


let module_exists ({ dependency; _ } as resolution) =
  UnannotatedGlobalEnvironment.ReadOnly.module_exists
    ?dependency
    (unannotated_global_environment resolution)


let class_exists ({ dependency; _ } as resolution) =
  UnannotatedGlobalEnvironment.ReadOnly.class_exists
    (unannotated_global_environment resolution)
    ?dependency


let get_module_metadata ({ dependency; _ } as resolution) =
  UnannotatedGlobalEnvironment.ReadOnly.get_module_metadata
    ?dependency
    (unannotated_global_environment resolution)


let resolve_exports ({ dependency; _ } as resolution) ?from reference =
  UnannotatedGlobalEnvironment.ReadOnly.resolve_exports
    ?dependency
    (unannotated_global_environment resolution)
    ?from
    reference


let get_type_alias ({ dependency; _ } as resolution) =
  TypeAliasEnvironment.ReadOnly.get_type_alias ?dependency (alias_environment resolution)


let get_variable ({ dependency; _ } as resolution) =
  TypeAliasEnvironment.ReadOnly.get_variable ?dependency (alias_environment resolution)


let parse_annotation_without_sanitizing_type_arguments ({ dependency; _ } as resolution) =
  TypeAliasEnvironment.ReadOnly.parse_annotation_without_sanitizing_type_arguments
    ?dependency
    (alias_environment resolution)


let param_spec_from_vararg_annotations ({ dependency; _ } as resolution) =
  TypeAliasEnvironment.ReadOnly.param_spec_from_vararg_annotations
    (alias_environment resolution)
    ?dependency
    ()


let class_hierarchy ({ dependency; _ } as resolution) =
  ClassHierarchyEnvironment.ReadOnly.class_hierarchy
    ?dependency
    (class_hierarchy_environment resolution)


let generic_parameters ({ dependency; _ } as resolution) =
  ClassHierarchyEnvironment.ReadOnly.generic_parameters
    ~empty_for_nongeneric:false
    ?dependency
    (class_hierarchy_environment resolution)


let generic_parameters_as_variables ({ dependency; _ } as resolution) =
  ClassHierarchyEnvironment.ReadOnly.generic_parameters_as_variables
    ~empty_for_nongeneric:false
    ?dependency
    (class_hierarchy_environment resolution)


let has_transitive_successor resolution ~successor predecessor =
  ClassSuccessorMetadataEnvironment.ReadOnly.has_transitive_successor
    (class_metadata_environment resolution)
    ~successor
    predecessor


let successors ({ dependency; _ } as resolution) =
  ClassSuccessorMetadataEnvironment.ReadOnly.successors
    ?dependency
    (class_metadata_environment resolution)


let get_class_metadata ({ dependency; _ } as resolution) =
  ClassSuccessorMetadataEnvironment.ReadOnly.get_class_metadata
    ?dependency
    (class_metadata_environment resolution)


let is_class_typed_dictionary ({ dependency; _ } as resolution) =
  ClassSuccessorMetadataEnvironment.ReadOnly.is_class_typed_dictionary
    (class_metadata_environment resolution)
    ?dependency


let does_class_extend_enum ({ dependency; _ } as resolution) =
  ClassSuccessorMetadataEnvironment.ReadOnly.does_class_extend_enum
    (class_metadata_environment resolution)
    ?dependency


let full_order ({ dependency; _ } as resolution) =
  AttributeResolution.ReadOnly.full_order ?dependency (attribute_resolution resolution)


let parse_annotation ({ dependency; _ } as resolution) =
  AttributeResolution.ReadOnly.parse_annotation
    ?dependency
    (attribute_resolution resolution)
    ~scoped_type_variables:None


let parse_annotation_with_scoped_typed_variables
    ({ dependency; _ } as resolution)
    ~scoped_type_variables
  =
  AttributeResolution.ReadOnly.parse_annotation
    ?dependency
    (attribute_resolution resolution)
    ~scoped_type_variables


let global ({ dependency; _ } as resolution) =
  AttributeResolution.ReadOnly.global (attribute_resolution resolution) ?dependency


let attribute ({ dependency; _ } as resolution) =
  AttributeResolution.ReadOnly.attribute (attribute_resolution resolution) ?dependency


let get_typed_dictionary ({ dependency; _ } as resolution) =
  AttributeResolution.ReadOnly.get_typed_dictionary (attribute_resolution resolution) ?dependency


let constraints_solution_exists ({ dependency; _ } as resolution) =
  AttributeResolution.ReadOnly.constraints_solution_exists
    ?dependency
    (attribute_resolution resolution)


let variance_map resolution =
  AttributeResolution.ReadOnly.variance_map (attribute_resolution resolution)


let uninstantiated_attributes
    ({ dependency; _ } as resolution)
    ?(transitive = false)
    ?(accessed_through_class = false)
    ?(include_generated_attributes = true)
    name
  =
  AttributeResolution.ReadOnly.uninstantiated_attributes
    (attribute_resolution resolution)
    ~transitive
    ~accessed_through_class
    ~include_generated_attributes
    name
    ?dependency


let instantiate_attribute ({ dependency; _ } as resolution) ?type_for_lookup =
  AttributeResolution.ReadOnly.instantiate_attribute
    (attribute_resolution resolution)
    ?dependency
    ?type_for_lookup


let metaclass ({ dependency; _ } as resolution) =
  AttributeResolution.ReadOnly.metaclass ?dependency (attribute_resolution resolution)


let resolve_mutable_literals ({ dependency; _ } as resolution) =
  AttributeResolution.ReadOnly.resolve_mutable_literals
    ?dependency
    (attribute_resolution resolution)


let resolve_define ({ dependency; _ } as resolution) =
  AttributeResolution.ReadOnly.resolve_define ?dependency (attribute_resolution resolution)


let resolve_define_undecorated ({ dependency; _ } as resolution) =
  AttributeResolution.ReadOnly.resolve_define_undecorated
    ?dependency
    (attribute_resolution resolution)


let signature_select ({ dependency; _ } as resolution) =
  AttributeResolution.ReadOnly.signature_select ?dependency (attribute_resolution resolution)


let validate_and_sanitize_type_arguments ({ dependency; _ } as resolution) =
  AttributeResolution.ReadOnly.validate_and_sanitize_type_arguments
    (attribute_resolution resolution)
    ?dependency


let location_of_global ({ dependency; _ } as resolution) =
  AnnotatedGlobalEnvironment.ReadOnly.location_of_global
    (annotated_global_environment resolution)
    ?dependency


let immediate_parents resolution = ClassHierarchy.immediate_parents (class_hierarchy resolution)

let parse_reference ?(allow_untracked = false) resolution reference =
  let validation =
    if allow_untracked then AttributeResolution.NoValidation else ValidatePrimitives
  in
  Expression.from_reference ~location:Location.any ~create_origin:(fun _ -> None) reference
  |> parse_annotation resolution ~validation


let less_or_equal resolution = full_order resolution |> TypeOrder.always_less_or_equal

let join resolution = full_order resolution |> TypeOrder.join

let join_for_branch_merge resolution left right =
  match left, right with
  | Type.Literal (String AnyLiteral), Type.Literal (String _)
  | Type.Literal (String _), Type.Literal (String AnyLiteral) ->
      Type.Literal (String AnyLiteral)
  | Type.Literal _, Type.Literal _ -> Type.union [left; right]
  | _ -> TypeOrder.join (full_order resolution) left right


let meet resolution = full_order resolution |> TypeOrder.meet

let widen resolution = full_order resolution |> TypeOrder.widen

let is_invariance_mismatch resolution ~left ~right =
  match left, right with
  | ( Type.Parametric { name = left_name; arguments = left_arguments },
      Type.Parametric { name = right_name; arguments = right_arguments } )
    when Identifier.equal left_name right_name ->
      let zipped =
        let maybe_generic_parameters =
          ClassHierarchy.generic_parameters (class_hierarchy resolution) left_name
        in
        let generic_parameters =
          match maybe_generic_parameters with
          | Some parameters -> parameters
          | None -> []
        in
        let variances =
          maybe_generic_parameters
          (* TODO(T47346673): Do this check when list variadics have variance *)
          >>| List.filter_map ~f:(function
                  | Type.GenericParameter.GpTypeVar { name; _ } ->
                      let variance =
                        Map.find
                          (variance_map
                             resolution
                             ~parameters:generic_parameters
                             ~class_name:left_name)
                          name
                      in
                      variance
                  | _ -> None)
        in
        match variances with
        | Some variances -> (
            match List.zip left_arguments right_arguments with
            | Ok zipped -> (
                match List.zip zipped variances with
                | Ok zipped ->
                    List.map zipped ~f:(fun ((left, right), variance) -> variance, left, right)
                    |> Option.some
                | _ -> None)
            | _ -> None)
        | _ -> None
      in
      let due_to_invariant_variable (variance, left, right) =
        match variance, left, right with
        | Type.Record.Variance.Invariant, Type.Argument.Single left, Type.Argument.Single right ->
            less_or_equal resolution ~left ~right
        | _ -> false
      in
      zipped >>| List.exists ~f:due_to_invariant_variable |> Option.value ~default:false
  | _ -> false


let attribute_from_class_name
    resolution
    ?(transitive = false)
    ?(accessed_through_class = false)
    ?(accessed_through_readonly = false)
    ?(special_method = false)
    class_name
    ~name
    ~type_for_lookup
  =
  let access = function
    | Some attribute -> Some attribute
    | None -> (
        match get_class_summary resolution class_name with
        | Some _ ->
            AnnotatedAttribute.create_instantiated
              ~annotation:Type.Top
              ~original_annotation:Type.Top
              ~uninstantiated_annotation:(Some Type.Top)
              ~abstract:false
              ~async_property:false
              ~class_variable:false
              ~defined:false
              ~initialized:NotInitialized
              ~name
              ~parent:class_name
              ~visibility:ReadWrite
              ~property:false
              ~undecorated_signature:None
            |> Option.some
        | None -> None)
  in
  try
    attribute
      ~type_for_lookup
      ~transitive
      ~accessed_through_class
      ~accessed_through_readonly
      ~special_method
      ~include_generated_attributes:true
      resolution
      ~attribute_name:name
      class_name
    |> access
  with
  | ClassHierarchy.Untracked untracked_type ->
      Log.warning
        "Found untracked type `%s` when checking for attribute `%s` of `%s`."
        untracked_type
        name
        class_name;
      None


let attribute_from_annotation ?special_method resolution ~parent:annotation ~name =
  match Type.class_attribute_lookups_for_type annotation with
  | None -> None
  | Some [] -> None
  | Some [{ Type.type_for_lookup; accessed_through_class; class_name; accessed_through_readonly }]
    ->
      attribute_from_class_name
        resolution
        ~transitive:true
        ~type_for_lookup
        ~accessed_through_class
        ~accessed_through_readonly
        ~name
        ?special_method
        class_name
      >>= fun attribute -> Option.some_if (AnnotatedAttribute.defined attribute) attribute
  | Some (_ :: _) -> None


let is_typed_dictionary resolution annotation =
  Type.primitive_name annotation
  >>| is_class_typed_dictionary resolution
  |> Option.value ~default:false


let is_enum resolution annotation =
  (* The class either directly extends Enum, or has a metaclass that extends EnumMeta or EnumType *)
  let class_name = Type.primitive_name annotation in
  if class_name >>| does_class_extend_enum resolution |> Option.value ~default:false then
    true
  else
    match class_name >>| metaclass resolution |> Option.value ~default:None with
    | Some metaclass_type ->
        less_or_equal resolution ~left:metaclass_type ~right:(Primitive "enum.EnumMeta")
        || less_or_equal resolution ~left:metaclass_type ~right:(Primitive "enum.EnumType")
    | None -> false


let is_consistent_with resolution ~resolve left right ~expression =
  let comparator = constraints_solution_exists resolution in
  let left =
    WeakenMutableLiterals.weaken_mutable_literals
      ~resolve
      ~get_typed_dictionary:(get_typed_dictionary resolution)
      ~expression
      ~resolved:left
      ~expected:right
      ~comparator
    |> WeakenMutableLiterals.resolved_type
  in
  comparator ~get_typed_dictionary_override:(fun _ -> None) ~left ~right


module ConstraintsSet = struct
  include ConstraintsSet

  let add_and_simplify constraints ~new_constraint ~global_resolution =
    TypeOrder.OrderedConstraintsSet.add_and_simplify
      constraints
      ~new_constraint
      ~order:(full_order global_resolution)


  let solve constraints ~global_resolution =
    TypeOrder.OrderedConstraintsSet.solve constraints ~order:(full_order global_resolution)
end

(* Given a concrete subtype of a type like `Foo[T, U]` extract the type arguments for `T` and `U`.
   This should never be used except on hardcoded types known in advance to have all unary TypeVar
   parameters because it does not handle TypeVarTuple or ParamSpec. *)
let extract_unary_type_arguments__unsafe resolution ~source ~target =
  ClassHierarchy.generic_parameters_as_variables (class_hierarchy resolution) target
  >>= fun variables ->
  let namespace = Type.Variable.Namespace.create_fresh () in
  List.map variables ~f:(Type.Variable.namespace ~namespace)
  |> Type.Variable.all_unary
  >>= fun unaries ->
  let solve_against =
    List.map unaries ~f:(fun unary -> Type.Argument.Single (Type.Variable unary))
    |> Type.parametric target
  in
  TypeOrder.OrderedConstraintsSet.add_and_simplify
    ConstraintsSet.empty
    ~new_constraint:(LessOrEqual { left = source; right = solve_against })
    ~order:(full_order resolution)
  |> ConstraintsSet.solve ~global_resolution:resolution
  >>= fun solution ->
  List.map unaries ~f:(TypeConstraints.Solution.instantiate_single_type_var solution) |> Option.all


let type_of_iteration_value global_resolution maybe_iterator_type =
  match
    extract_unary_type_arguments__unsafe
      global_resolution
      ~target:"typing.Iterable"
      ~source:maybe_iterator_type
  with
  | Some [iteration_type] -> Some iteration_type
  | _ -> None


let type_of_awaited_value global_resolution maybe_awaitable_type =
  match
    extract_unary_type_arguments__unsafe
      global_resolution
      ~target:"typing.Awaitable"
      ~source:maybe_awaitable_type
  with
  | Some [awaited_type] -> Some awaited_type
  | _ -> None


let type_of_mapping_key_and_value global_resolution maybe_mapping_type =
  (* First match against Generator *)
  match
    extract_unary_type_arguments__unsafe
      global_resolution
      ~target:"typing.Mapping"
      ~source:maybe_mapping_type
  with
  | Some [key_type; value_type] -> Some (key_type, value_type)
  | _ -> None


(* Determine the appropriate type for `yield` expressions in a generator function, based on the
   return annotation. *)
let type_of_generator_send_and_return global_resolution maybe_generator_type =
  (* First match against Generator *)
  match
    extract_unary_type_arguments__unsafe
      global_resolution
      ~target:"typing.Generator"
      ~source:maybe_generator_type
  with
  | Some [_yield_type; send_type; return_type] -> send_type, return_type
  | _ -> (
      (* Fall back to match against AsyncGenerator. We fall back instead of using an explicit flag
         because, if the user mixes these types up we still ought to resolve their yield expressions
         to reasonable types *)
      match
        extract_unary_type_arguments__unsafe
          global_resolution
          ~target:"typing.AsyncGenerator"
          ~source:maybe_generator_type
      with
      | Some [_yield_type; send_type] -> send_type, Type.none
      | _ ->
          (* Fall back to Type.none because it's legal to use other annotations like `object` or
             `Iterator` on a generator function, but in those cases the send type is always
             NoneType *)
          Type.none, Type.none)


let annotation_parser resolution =
  {
    AnnotatedCallable.parse_annotation =
      parse_annotation ~validation:ValidatePrimitivesAndTypeParameters resolution;
    param_spec_from_vararg_annotations = param_spec_from_vararg_annotations resolution;
  }


(* Normally we validate not only that concrete types exist but also that all type variables are
   bound when converting expressions to types, and we refuse to return *any* type information if
   this validation fails. There are a few cases where producing good type errors requires us to
   allow type-checking code to work with a type that may have unbound variables, and this variation
   if the parser is used there. *)
let nonvalidating_annotation_parser resolution =
  {
    AnnotatedCallable.parse_annotation =
      parse_annotation ~validation:AttributeResolution.ValidatePrimitives resolution;
    param_spec_from_vararg_annotations = param_spec_from_vararg_annotations resolution;
  }


let overrides resolution class_name ~name =
  let find_override parent =
    attribute_from_class_name
      resolution
      ~transitive:false
      ~accessed_through_class:true
      ~name
      parent
      ~type_for_lookup:(Type.Primitive class_name)
    >>= fun attribute -> Option.some_if (AnnotatedAttribute.defined attribute) attribute
  in
  successors resolution class_name |> List.find_map ~f:find_override


let refine global_resolution annotation refined_type =
  let solve_less_or_equal ~left ~right =
    ConstraintsSet.add_and_simplify
      ConstraintsSet.empty
      ~new_constraint:(ConstraintsSet.LessOrEqual { left; right })
      ~global_resolution
    |> ConstraintsSet.solve ~global_resolution
    >>| fun solution -> TypeConstraints.Solution.instantiate solution left
  in
  let type_less_or_equal = less_or_equal global_resolution in
  TypeInfo.Unit.refine ~type_less_or_equal ~solve_less_or_equal ~refined_type annotation


module Testing = struct
  let constraints_for_instantiate ({ dependency; _ } as resolution) =
    AttributeResolution.ReadOnly.Testing.constraints_for_instantiate
      ?dependency
      (attribute_resolution resolution)
end
