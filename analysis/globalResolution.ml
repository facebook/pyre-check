(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Ast
open Statement

type global = Annotation.t Node.t [@@deriving eq, show, compare, sexp]

type t = {
  dependency: SharedMemoryKeys.dependency option;
  class_metadata_environment: ClassMetadataEnvironment.ReadOnly.t;
  global: Reference.t -> global option;
}

let create ?dependency ~class_metadata_environment ~global =
  { class_metadata_environment; dependency; global }


let class_hierarchy_environment { class_metadata_environment; _ } =
  ClassMetadataEnvironment.ReadOnly.class_hierarchy_environment class_metadata_environment


let alias_environment resolution =
  ClassHierarchyEnvironment.ReadOnly.alias_environment (class_hierarchy_environment resolution)


let empty_stub_environment resolution =
  alias_environment resolution |> AliasEnvironment.ReadOnly.empty_stub_environment


let unannotated_global_environment resolution =
  alias_environment resolution |> AliasEnvironment.ReadOnly.unannotated_global_environment


let class_definition ({ dependency; _ } as resolution) =
  UnannotatedGlobalEnvironment.ReadOnly.get_class_definition
    (unannotated_global_environment resolution)
    ?dependency


let ast_environment resolution =
  unannotated_global_environment resolution |> UnannotatedGlobalEnvironment.ReadOnly.ast_environment


let class_hierarchy ({ dependency; _ } as resolution) =
  let edges =
    ClassHierarchyEnvironment.ReadOnly.get_edges
      ?dependency
      (class_hierarchy_environment resolution)
  in
  ( module struct
    let edges = edges

    let contains key = class_definition resolution key |> Option.is_some
  end : ClassHierarchy.Handler )


let is_tracked resolution = ClassHierarchy.contains (class_hierarchy resolution)

let contains_untracked resolution annotation =
  List.exists
    ~f:(fun annotation -> not (is_tracked resolution annotation))
    (Type.elements annotation)


let is_protocol ({ dependency; _ } as resolution) annotation =
  UnannotatedGlobalEnvironment.ReadOnly.is_protocol
    (unannotated_global_environment resolution)
    ?dependency
    annotation


let primitive_name annotation =
  let primitive, _ = Type.split annotation in
  Type.primitive_name primitive


let class_definition ({ dependency; _ } as resolution) annotation =
  primitive_name annotation
  >>= UnannotatedGlobalEnvironment.ReadOnly.get_class_definition
        (unannotated_global_environment resolution)
        ?dependency


let define_body ({ dependency; _ } as resolution) =
  UnannotatedGlobalEnvironment.ReadOnly.get_define_body
    ?dependency
    (unannotated_global_environment resolution)


let class_metadata { dependency; class_metadata_environment; _ } annotation =
  primitive_name annotation
  >>= ClassMetadataEnvironment.ReadOnly.get_class_metadata ?dependency class_metadata_environment


let is_suppressed_module resolution reference =
  EmptyStubEnvironment.ReadOnly.from_empty_stub (empty_stub_environment resolution) reference


let undecorated_signature { dependency; class_metadata_environment; _ } =
  UndecoratedFunctionEnvironment.ReadOnly.get_undecorated_function
    ?dependency
    (ClassMetadataEnvironment.ReadOnly.undecorated_function_environment class_metadata_environment)


let aliases ({ dependency; _ } as resolution) =
  AliasEnvironment.ReadOnly.get_alias ?dependency (alias_environment resolution)


let module_definition ({ dependency; _ } as resolution) =
  AstEnvironment.ReadOnly.get_module_metadata ?dependency (ast_environment resolution)


module DefinitionsCache (Type : sig
  type t
end) =
struct
  let cache : Type.t Reference.Table.t = Reference.Table.create ()

  let enabled =
    (* Only enable this in nonincremental mode for now. *)
    ref false


  let enable () = enabled := true

  let set key value = Hashtbl.set cache ~key ~data:value

  let get key =
    if !enabled then
      Hashtbl.find cache key
    else
      None


  let invalidate () = Hashtbl.clear cache
end

module FunctionDefinitionsCache = DefinitionsCache (struct
  type t = Define.t Node.t list option
end)

module ClassDefinitionsCache = DefinitionsCache (struct
  type t = Class.t Node.t list option
end)

let containing_source resolution reference =
  let ast_environment = ast_environment resolution in
  let rec qualifier ~lead ~tail =
    match tail with
    | head :: (_ :: _ as tail) ->
        let new_lead = Reference.create ~prefix:lead head in
        if Option.is_none (module_definition resolution new_lead) then
          lead
        else
          qualifier ~lead:new_lead ~tail
    | _ -> lead
  in
  qualifier ~lead:Reference.empty ~tail:(Reference.as_list reference)
  |> AstEnvironment.ReadOnly.get_source ast_environment


let function_definitions resolution reference =
  match FunctionDefinitionsCache.get reference with
  | Some result -> result
  | None ->
      let result =
        containing_source resolution reference
        >>| Preprocessing.defines ~include_stubs:true ~include_nested:true
        >>| List.filter ~f:(fun { Node.value = { Define.signature = { name; _ }; _ }; _ } ->
                Reference.equal reference name)
      in
      FunctionDefinitionsCache.set reference result;
      result


let class_definitions resolution reference =
  match ClassDefinitionsCache.get reference with
  | Some result -> result
  | None ->
      let result =
        containing_source resolution reference
        >>| Preprocessing.classes
        >>| List.filter ~f:(fun { Node.value = { Class.name; _ }; _ } ->
                Reference.equal reference name)
        (* Prefer earlier definitions. *)
        >>| List.rev
      in
      ClassDefinitionsCache.set reference result;
      result


let full_order { dependency; class_metadata_environment; _ } =
  AttributeResolution.full_order ?dependency class_metadata_environment


let less_or_equal resolution = full_order resolution |> TypeOrder.always_less_or_equal

let is_compatible_with resolution = full_order resolution |> TypeOrder.is_compatible_with

let is_instantiated resolution = ClassHierarchy.is_instantiated (class_hierarchy resolution)

let parse_reference
    ?(allow_untracked = false)
    { dependency; class_metadata_environment; _ }
    reference
  =
  Expression.from_reference ~location:Location.Reference.any reference
  |> AttributeResolution.parse_annotation
       ?dependency
       ~allow_untracked
       ~allow_invalid_type_parameters:true
       ~class_metadata_environment


let parse_as_list_variadic ({ dependency; class_metadata_environment; _ } as resolution) name =
  let parsed_as_type_variable =
    AttributeResolution.parse_annotation
      ?dependency
      ~class_metadata_environment
      ~allow_untracked:true
      name
    |> Type.primitive_name
    >>= aliases resolution
  in
  match parsed_as_type_variable with
  | Some (VariableAlias (ListVariadic variable)) -> Some variable
  | _ -> None


let is_invariance_mismatch resolution ~left ~right =
  match left, right with
  | ( Type.Parametric { name = left_name; parameters = left_parameters },
      Type.Parametric { name = right_name; parameters = right_parameters } )
    when Identifier.equal left_name right_name ->
      let zipped =
        match
          ( ClassHierarchy.variables (class_hierarchy resolution) left_name,
            left_parameters,
            right_parameters )
        with
        | Some (Unaries variables), Concrete left_parameters, Concrete right_parameters -> (
            List.map3
              variables
              left_parameters
              right_parameters
              ~f:(fun { variance; _ } left right -> variance, left, right)
            |> function
            | List.Or_unequal_lengths.Ok list -> Some list
            | _ -> None )
        | Some (Concatenation _), _, _ ->
            (* TODO(T47346673): Do this check when list variadics have variance *)
            None
        | _ -> None
      in
      let due_to_invariant_variable (variance, left, right) =
        match variance with
        | Type.Variable.Invariant -> less_or_equal resolution ~left ~right
        | _ -> false
      in
      zipped >>| List.exists ~f:due_to_invariant_variable |> Option.value ~default:false
  | _ -> false


(* There isn't a great way of testing whether a file only contains tests in Python. Due to the
   difficulty of handling nested classes within test cases, etc., we use the heuristic that a class
   which inherits from unittest.TestCase indicates that the entire file is a test file. *)
let source_is_unit_test resolution ~source =
  let is_unittest { Node.value = { Class.name; _ }; _ } =
    let annotation = parse_reference resolution name in
    less_or_equal resolution ~left:annotation ~right:(Type.Primitive "unittest.case.TestCase")
  in
  List.exists (Preprocessing.classes source) ~f:is_unittest


let class_extends_placeholder_stub_class
    ({ dependency; class_metadata_environment; _ } as resolution)
    { ClassSummary.bases; _ }
  =
  let is_from_placeholder_stub { Expression.Call.Argument.value; _ } =
    let parsed =
      AttributeResolution.parse_annotation
        ~allow_untracked:true
        ~allow_invalid_type_parameters:true
        ~allow_primitives_from_empty_stubs:true
        ?dependency
        ~class_metadata_environment
        value
    in
    match parsed with
    | Type.Primitive primitive
    | Parametric { name = primitive; _ } ->
        Reference.create primitive
        |> fun reference ->
        EmptyStubEnvironment.ReadOnly.from_empty_stub (empty_stub_environment resolution) reference
    | _ -> false
  in
  List.exists bases ~f:is_from_placeholder_stub


let global { global; _ } reference =
  (* TODO (T41143153): We might want to properly support this by unifying attribute lookup logic for
     module and for class *)
  match Reference.last reference with
  | "__doc__"
  | "__file__"
  | "__name__" ->
      let annotation =
        Annotation.create_immutable ~global:true Type.string |> Node.create_with_default_location
      in
      Some annotation
  | "__dict__" ->
      let annotation =
        Type.dictionary ~key:Type.string ~value:Type.Any
        |> Annotation.create_immutable ~global:true
        |> Node.create_with_default_location
      in
      Some annotation
  | _ -> global reference


let c_attribute ~resolution:{ dependency; class_metadata_environment; _ } =
  AttributeResolution.attribute ?dependency ~class_metadata_environment


let attribute ({ dependency; class_metadata_environment; _ } as resolution) ~parent:annotation ~name
  =
  match
    UnannotatedGlobalEnvironment.ReadOnly.resolve_class
      ?dependency
      (unannotated_global_environment resolution)
      annotation
  with
  | None -> None
  | Some [] -> None
  | Some [{ instantiated; class_attributes; class_definition }] ->
      AttributeResolution.attribute
        class_definition
        ?dependency
        ~class_metadata_environment
        ~transitive:true
        ~instantiated
        ~class_attributes
        ~name
      |> fun attribute -> Option.some_if (AnnotatedAttribute.defined attribute) attribute
  | Some (_ :: _) -> None


let is_consistent_with { dependency; class_metadata_environment; _ } ~resolve left right ~expression
  =
  let comparator ~left ~right =
    AttributeResolution.constraints_solution_exists
      ?dependency
      ~class_metadata_environment
      ~left
      ~right
  in

  let left =
    AttributeResolution.weaken_mutable_literals
      resolve
      ~expression
      ~resolved:left
      ~expected:right
      ~comparator
  in
  comparator ~left ~right


let constructor ~resolution:{ dependency; class_metadata_environment; _ } =
  AttributeResolution.constructor ?dependency ~class_metadata_environment


let constraints ~resolution:{ dependency; class_metadata_environment; _ } =
  AttributeResolution.constraints ?dependency ~class_metadata_environment


let create_attribute ~resolution:{ dependency; class_metadata_environment; _ } =
  AttributeResolution.create_attribute ?dependency ~class_metadata_environment


let successors ~resolution:{ dependency; class_metadata_environment; _ } =
  ClassMetadataEnvironment.ReadOnly.successors ?dependency class_metadata_environment


let superclasses ~resolution:{ dependency; class_metadata_environment; _ } =
  ClassMetadataEnvironment.ReadOnly.superclasses ?dependency class_metadata_environment


let generics ~resolution:{ dependency; class_metadata_environment; _ } =
  AttributeResolution.generics ?dependency ~class_metadata_environment


let attribute_table ~resolution:{ dependency; class_metadata_environment; _ } =
  AttributeResolution.attribute_table ?dependency ~class_metadata_environment


let attributes ~resolution:{ dependency; class_metadata_environment; _ } =
  AttributeResolution.attributes ?dependency ~class_metadata_environment


let metaclass ~resolution:{ dependency; class_metadata_environment; _ } =
  AttributeResolution.metaclass ?dependency ~class_metadata_environment


let resolve_class ({ dependency; _ } as resolution) =
  UnannotatedGlobalEnvironment.ReadOnly.resolve_class
    (unannotated_global_environment resolution)
    ?dependency


let resolve_mutable_literals { dependency; class_metadata_environment; _ } =
  AttributeResolution.resolve_mutable_literals ?dependency ~class_metadata_environment


let apply_decorators ~resolution:{ dependency; class_metadata_environment; _ } =
  AttributeResolution.apply_decorators ?dependency ~class_metadata_environment


let create_callable ~resolution:{ dependency; class_metadata_environment; _ } =
  AttributeResolution.create_callable ?dependency ~class_metadata_environment


let signature_select ~global_resolution:{ dependency; class_metadata_environment; _ } =
  AttributeResolution.signature_select ?dependency ~class_metadata_environment


let resolve_exports ({ dependency; _ } as resolution) ~reference =
  AstEnvironment.ReadOnly.resolve_exports ?dependency (ast_environment resolution) reference


let widen resolution = full_order resolution |> TypeOrder.widen

let join resolution = full_order resolution |> TypeOrder.join

let meet resolution = full_order resolution |> TypeOrder.meet

let check_invalid_type_parameters { dependency; class_metadata_environment; _ } =
  AttributeResolution.check_invalid_type_parameters class_metadata_environment ?dependency


let variables ?default ({ dependency; _ } as resolution) =
  ClassHierarchyEnvironment.ReadOnly.variables
    ?default
    ?dependency
    (class_hierarchy_environment resolution)


let solve_less_or_equal resolution = full_order resolution |> TypeOrder.solve_less_or_equal

let constraints_solution_exists { dependency; class_metadata_environment; _ } =
  AttributeResolution.constraints_solution_exists ?dependency ~class_metadata_environment


let partial_solve_constraints resolution =
  TypeOrder.OrderedConstraints.extract_partial_solution ~order:(full_order resolution)


let solve_constraints resolution = TypeOrder.OrderedConstraints.solve ~order:(full_order resolution)

let solve_ordered_types_less_or_equal resolution =
  full_order resolution |> TypeOrder.solve_ordered_types_less_or_equal


let parse_annotation { dependency; class_metadata_environment; _ } =
  AttributeResolution.parse_annotation ?dependency ~class_metadata_environment


let resolve_literal { dependency; class_metadata_environment; _ } =
  AttributeResolution.resolve_literal ?dependency ~class_metadata_environment


let parse_as_concatenation ({ dependency; _ } as resolution) =
  AliasEnvironment.ReadOnly.parse_as_concatenation (alias_environment resolution) ?dependency


let parse_as_parameter_specification_instance_annotation ({ dependency; _ } as resolution) =
  AliasEnvironment.ReadOnly.parse_as_parameter_specification_instance_annotation
    (alias_environment resolution)
    ?dependency


let annotation_parser resolution =
  {
    AnnotatedCallable.parse_annotation = parse_annotation resolution;
    parse_as_concatenation = parse_as_concatenation resolution;
    parse_as_parameter_specification_instance_annotation =
      parse_as_parameter_specification_instance_annotation resolution;
  }
