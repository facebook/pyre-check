(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TypeAliasEnvironment: component of the environment stack
 * - upstream: EmptyStubEnvironment
 * - downstream: ClassHierarchyEnvironment
 * - key: type name (as an Identifier.t)
 * - value: a RawAlias.t option: either a type alias or typevar alias
 *
 * This layer is responsible for resolving type and type variable aliases -
 * that is, statements of the form `Name = SomeType` where `SomeType` is an
 * expression that is a valid type annotation or type variable.
 *
 * Note that general "aliases" (as in re-bindings of python globals to new
 * names) are handled by `resolve_exports` in UnannotatedGlobalEnvironments.
 * TypeAliasEnvironment is only used to resolve TypeAliases.
 *)

open Core
open Ast
open Pyre
open Expression
module PreviousEnvironment = EmptyStubEnvironment

module RawAlias = struct
  type t =
    | TypeAlias of Type.t
    | VariableAlias of Type.Variable.Declaration.t
  [@@deriving equal, compare, sexp, show, hash]
end

let empty_aliases ?replace_unbound_parameters_with_any:_ _ = None

module IncomingDataComputation = struct
  module Queries = struct
    type t = {
      class_exists: string -> bool;
      module_exists: Ast.Reference.t -> bool;
      get_unannotated_global: Ast.Reference.t -> Module.UnannotatedGlobal.t option;
      is_from_empty_stub: Ast.Reference.t -> bool;
    }
  end

  module UnresolvedAlias = struct
    type t = {
      value: Expression.t;
      target: Reference.t;
    }

    let unchecked_resolve ~unparsed ~target map =
      let aliases ?replace_unbound_parameters_with_any:_ name = Map.find map name in
      let resolved_aliases ?replace_unbound_parameters_with_any:_ name =
        match aliases name with
        | Some (RawAlias.TypeAlias t) -> Some t
        | _ -> None
      in
      let variable_aliases name =
        match aliases ?replace_unbound_parameters_with_any:(Some true) name with
        | Some (RawAlias.VariableAlias variable) -> Some variable
        | _ -> None
      in

      match Type.create ~variables:variable_aliases ~aliases:resolved_aliases unparsed with
      | Type.Variable variable -> Type.Variable { variable with name = Reference.show target }
      | annotation -> annotation


    type check_result =
      | Resolved of RawAlias.t
      | HasDependents of {
          unparsed: Expression.t;
          dependencies: string list;
        }

    let checked_resolve
        Queries.{ class_exists; module_exists; is_from_empty_stub; _ }
        { value; target }
      =
      let value_annotation = unchecked_resolve ~unparsed:value ~target String.Map.empty in
      let dependencies = String.Hash_set.create () in
      let module TrackedTransform = Type.VisitWithTransform.Make (struct
        type state = unit

        let visit_children_before _ = function
          | Type.NoneType -> false
          | _ -> true


        let visit_children_after = false

        let visit _ annotation =
          let new_state, transformed_annotation =
            match annotation with
            | Type.Parametric { name = primitive; _ }
            | Primitive primitive ->
                let reference =
                  match Node.value (Type.expression (Type.Primitive primitive)) with
                  | Expression.Name name when is_simple_name name -> name_to_reference_exn name
                  | _ -> Reference.create "typing.Any"
                in
                if is_from_empty_stub reference then
                  (), Type.Any
                else if
                  class_exists primitive
                  || module_exists (Reference.create primitive)
                  (* Don't consider "..." in `MyCallable[..., int]` to be a dependent alias. *)
                  || Identifier.equal primitive "..."
                then
                  (), annotation
                else
                  let _ = Hash_set.add dependencies primitive in
                  (), annotation
            | _ -> (), annotation
          in
          { Type.VisitWithTransform.transformed_annotation; new_state }
      end)
      in
      let _, annotation = TrackedTransform.visit () value_annotation in
      if Hash_set.is_empty dependencies then
        Resolved (RawAlias.TypeAlias annotation)
      else
        HasDependents { unparsed = value; dependencies = Hash_set.to_list dependencies }
  end

  type extracted =
    | VariableAlias of Type.Variable.Declaration.t
    | TypeAlias of UnresolvedAlias.t

  let extract_alias Queries.{ class_exists; get_unannotated_global; _ } name =
    let open Module.UnannotatedGlobal in
    let extract_alias = function
      | SimpleAssign { value = None; _ } -> None
      | SimpleAssign { explicit_annotation; value = Some value; _ } -> (
          let variable_aliases _ = None in
          let target_annotation =
            Type.create
              ~variables:variable_aliases
              ~aliases:Type.resolved_empty_aliases
              (from_reference ~location:Location.any name)
          in
          match Node.value value, explicit_annotation with
          | ( (BinaryOperator _ | Subscript _ | Call _ | Name _ | Constant (Constant.String _)),
              Some
                {
                  Node.value =
                    Name
                      (Name.Attribute
                        {
                          base =
                            {
                              Node.value = Name (Name.Identifier ("typing_extensions" | "typing"));
                              _;
                            };
                          attribute = "TypeAlias";
                          _;
                        });
                  _;
                } )
          | (BinaryOperator _ | Subscript _ | Call _ | Name _), None -> (
              match Type.Variable.Declaration.parse (delocalize value) ~target:name with
              | Some variable -> Some (VariableAlias variable)
              | _ ->
                  let variable_aliases _ = None in

                  let value = Type.preprocess_alias_value value |> delocalize in
                  let value_annotation =
                    Type.create
                      ~variables:variable_aliases
                      ~aliases:Type.resolved_empty_aliases
                      value
                  in
                  if
                    not
                      (Type.contains_unknown target_annotation
                      || Type.contains_unknown value_annotation
                      || Type.equal value_annotation target_annotation)
                  then
                    Some (TypeAlias { target = name; value })
                  else
                    None)
          | _ -> None)
      | Imported import -> (
          if class_exists (Reference.show name) then
            None
          else
            let original_name_of_alias =
              match import with
              | ImportModule { target; implicit_alias } ->
                  if implicit_alias then
                    Option.value_exn (Reference.head target)
                  else
                    target
              | ImportFrom { from; target; _ } -> (
                  match Reference.show from with
                  | "future.builtins"
                  | "builtins" ->
                      Reference.create target
                  | _ -> Reference.create target |> Reference.combine from)
            in
            match Reference.as_list name, Reference.as_list original_name_of_alias with
            | [single_identifier], [typing; identifier]
              when String.equal typing "typing" && String.equal single_identifier identifier ->
                (* builtins has a bare qualifier. Don't export bare aliases from typing. *)
                None
            | _ ->
                let value = from_reference ~location:Location.any original_name_of_alias in
                Some (TypeAlias { target = name; value }))
      | TupleAssign _
      | Class
      | Define _ ->
          None
    in
    get_unannotated_global name >>= extract_alias


  let produce_alias queries global_name =
    let maybe_convert_to_recursive_alias alias_reference = function
      | RawAlias.TypeAlias annotation
        when (not (Identifier.equal (Reference.show alias_reference) "typing_extensions"))
             && Type.RecursiveType.is_recursive_alias_reference
                  ~alias_name:(Reference.show alias_reference)
                  annotation ->
          let alias_name = Reference.show alias_reference in
          let is_directly_recursive =
            Type.class_data_for_attribute_lookup annotation
            >>| List.exists ~f:(fun { Type.class_name; _ } ->
                    Identifier.equal class_name alias_name)
            |> Option.value ~default:true
          in
          let is_generic = Type.contains_variable annotation in
          RawAlias.TypeAlias (Type.RecursiveType.create ~name:alias_name ~body:annotation)
          |> Option.some_if ((not is_directly_recursive) && not is_generic)
      | resolved_alias -> Some resolved_alias
    in
    (* TODO(T53786399): Optimize this function. Theres a lot of perf potentially to be gained here,
       currently biasing towards simplicity *)
    let rec get_aliased_type_for current ~visited =
      (* This means we're in a loop *)
      if Set.mem visited current then
        None
      else
        let visited = Set.add visited current in
        let resolve_after_resolving_dependencies = function
          | VariableAlias variable -> Some (RawAlias.VariableAlias variable)
          | TypeAlias unresolved -> (
              match UnresolvedAlias.checked_resolve queries unresolved with
              | Resolved alias -> Some alias
              | HasDependents { unparsed; dependencies } ->
                  let solve_pair dependency =
                    get_aliased_type_for (Reference.create dependency) ~visited
                    >>| fun solution -> dependency, solution
                  in
                  List.filter dependencies ~f:(fun dependency ->
                      not (Reference.equal (Reference.create dependency) current))
                  |> List.map ~f:solve_pair
                  |> Option.all
                  >>| String.Map.of_alist_exn
                  >>| UnresolvedAlias.unchecked_resolve ~target:current ~unparsed
                  >>| fun alias -> RawAlias.TypeAlias alias)
        in
        extract_alias queries current
        >>= resolve_after_resolving_dependencies
        >>= maybe_convert_to_recursive_alias current
    in
    get_aliased_type_for global_name ~visited:Reference.Set.empty
end

module OutgoingDataComputation = struct
  module Queries = struct
    type t = {
      class_exists: Type.Primitive.t -> bool;
      is_from_empty_stub: Ast.Reference.t -> bool;
      get_type_alias:
        ?replace_unbound_parameters_with_any:bool -> Type.Primitive.t -> RawAlias.t option;
    }
  end

  let type_contains_untracked_name Queries.{ class_exists; _ } annotation =
    List.exists ~f:(fun class_name -> not (class_exists class_name)) (Type.collect_names annotation)


  let parse_annotation_without_validating_type_parameters
      (Queries.{ is_from_empty_stub; get_type_alias; _ } as queries)
      ?modify_aliases
      ?(allow_untracked = false)
      expression
    =
    let modify_aliases =
      Option.value modify_aliases ~default:(fun ?replace_unbound_parameters_with_any:_ name -> name)
    in
    let parsed =
      let expression = Type.preprocess_alias_value expression |> delocalize in
      let aliases ?replace_unbound_parameters_with_any name =
        get_type_alias name >>| modify_aliases ?replace_unbound_parameters_with_any
      in
      let resolved_aliases ?replace_unbound_parameters_with_any name =
        match aliases ?replace_unbound_parameters_with_any name with
        | Some (RawAlias.TypeAlias t) -> Some t
        | _ -> None
      in
      let variable_aliases name =
        match aliases ?replace_unbound_parameters_with_any:(Some true) name with
        | Some (RawAlias.VariableAlias variable) -> Some variable
        | _ -> None
      in

      Type.create ~variables:variable_aliases ~aliases:resolved_aliases expression
    in
    let annotation =
      let type_map = function
        | Type.Primitive name ->
            let originates_is_from_empty_stub =
              let reference = Reference.create name in
              is_from_empty_stub reference
            in
            if originates_is_from_empty_stub then
              Some Type.Any
            else
              None
        | _ -> None
      in
      Type.apply_type_map parsed ~type_map
    in
    if type_contains_untracked_name queries annotation && not allow_untracked then
      Type.Top
    else
      annotation


  let param_spec_from_vararg_annotations
      Queries.{ get_type_alias; _ }
      ~args_annotation
      ~kwargs_annotation
      ()
    =
    let get_param_spec variable_name =
      match get_type_alias variable_name with
      | Some (RawAlias.VariableAlias (Type.Variable.Declaration.DParamSpec { name })) ->
          Some (Type.Variable.ParamSpec.create name)
      | _ -> None
    in
    Type.Variable.ParamSpec.of_component_annotations
      ~get_param_spec
      ~args_annotation:(delocalize args_annotation)
      ~kwargs_annotation:(delocalize kwargs_annotation)
end

module AliasValue = struct
  type t = RawAlias.t option

  let prefix = Hack_parallel.Std.Prefix.make ()

  let description = "Alias"

  let equal = Memory.equal_from_compare (Option.compare RawAlias.compare)
end

module Aliases = Environment.EnvironmentTable.NoCache (struct
  module PreviousEnvironment = PreviousEnvironment
  module Key = SharedMemoryKeys.StringKey
  module Value = AliasValue

  type trigger = Reference.t [@@deriving sexp, compare]

  let convert_trigger = Reference.show

  let key_to_trigger = Reference.create ?prefix:None

  module TriggerSet = Reference.Set

  let lazy_incremental = false

  let produce_value empty_stub_environment key ~dependency =
    let queries =
      let unannotated_global_environment =
        EmptyStubEnvironment.ReadOnly.unannotated_global_environment empty_stub_environment
      in
      IncomingDataComputation.Queries.
        {
          class_exists =
            UnannotatedGlobalEnvironment.ReadOnly.class_exists
              unannotated_global_environment
              ?dependency;
          module_exists =
            UnannotatedGlobalEnvironment.ReadOnly.module_exists
              unannotated_global_environment
              ?dependency;
          get_unannotated_global =
            UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global
              unannotated_global_environment
              ?dependency;
          is_from_empty_stub =
            EmptyStubEnvironment.ReadOnly.is_from_empty_stub empty_stub_environment ?dependency;
        }
    in
    IncomingDataComputation.produce_alias queries key


  let filter_upstream_dependency = function
    | SharedMemoryKeys.AliasRegister name -> Some name
    | _ -> None


  let trigger_to_dependency name = SharedMemoryKeys.AliasRegister name

  let show_key = Fn.id

  let overlay_owns_key source_code_overlay =
    SourceCodeIncrementalApi.Overlay.owns_identifier source_code_overlay


  let equal_value = Option.equal RawAlias.equal
end)

include Aliases

module ReadOnly = struct
  include Aliases.ReadOnly

  let get_type_alias environment ?dependency ?replace_unbound_parameters_with_any:_ name =
    get environment ?dependency name


  let empty_stub_environment = upstream_environment

  let unannotated_global_environment read_only =
    empty_stub_environment read_only |> EmptyStubEnvironment.ReadOnly.unannotated_global_environment


  let outgoing_queries ?dependency environment =
    OutgoingDataComputation.Queries.
      {
        class_exists =
          UnannotatedGlobalEnvironment.ReadOnly.class_exists
            (unannotated_global_environment environment)
            ?dependency;
        is_from_empty_stub =
          EmptyStubEnvironment.ReadOnly.is_from_empty_stub
            (empty_stub_environment environment)
            ?dependency;
        get_type_alias = get_type_alias environment ?dependency;
      }


  let parse_annotation_without_validating_type_parameters environment ?dependency =
    OutgoingDataComputation.parse_annotation_without_validating_type_parameters
      (outgoing_queries ?dependency environment)


  let param_spec_from_vararg_annotations environment ?dependency =
    OutgoingDataComputation.param_spec_from_vararg_annotations
      (outgoing_queries ?dependency environment)
end

module AliasReadOnly = ReadOnly
module UpdateResult = Aliases.UpdateResult
module Testing = Aliases.Testing
