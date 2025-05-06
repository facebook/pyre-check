(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TypeAliasEnvironment: component of the environment stack
 * - upstream: UnannotatedGlobalEnvironment
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
module PreviousEnvironment = UnannotatedGlobalEnvironment

module RawAlias = struct
  type t =
    | TypeAlias of Type.t
    | VariableDeclaration of Type.Variable.Declaration.t
  [@@deriving equal, compare, sexp, show, hash]
end

module IncomingDataComputation = struct
  module Queries = struct
    type t = {
      class_exists: string -> bool;
      module_exists: Ast.Reference.t -> bool;
      get_unannotated_global: Ast.Reference.t -> Module.UnannotatedGlobal.t option;
    }
  end

  module UnresolvedAlias = struct
    type t = {
      value: Expression.t;
      variables: (string -> Type.Variable.t option) option;
    }

    let unchecked_resolve ~unparsed ~variables ~aliases =
      let resolved_aliases ?replace_unbound_parameters_with_any:_ name =
        match aliases name with
        | Some (RawAlias.TypeAlias t) -> Some t
        | _ -> None
      in
      Type.create ~variables ~aliases:resolved_aliases unparsed


    type check_result =
      | Resolved of RawAlias.t
      | HasDependents of {
          unparsed: Expression.t;
          dependencies: string list;
        }

    let checked_resolve Queries.{ class_exists; module_exists; _ } { value; variables } =
      let variables =
        match variables with
        | Some variables -> variables
        | None -> Type.resolved_empty_variables
      in

      let aliases ?replace_unbound_parameters_with_any:_ _name = None in
      let value_annotation = unchecked_resolve ~unparsed:value ~variables ~aliases in
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
                if
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
    | ExtractedVariableDeclaration of Type.Variable.Declaration.t
    | ExtractedAlias of UnresolvedAlias.t

  let extract_alias Queries.{ class_exists; get_unannotated_global; _ } name =
    let open Module.UnannotatedGlobal in
    (* A helper function which we will use to extract type aliases from SimpleAssign and
       TypeStatement *)
    let process_aliases explicit_annotation value variables =
      let variable_aliases _ = None in
      let target_annotation =
        Type.create
          ~variables:variable_aliases
          ~aliases:Type.resolved_empty_aliases
          (from_reference ~location:Location.any ~create_origin:(fun _ -> None) name)
      in
      match Node.value value, explicit_annotation with
      | Expression.Call _, None -> (
          match
            Type.Variable.Declaration.parse
              (delocalize ~create_origin:(fun _ -> None) value)
              ~target:name
          with
          | Some variable -> Some (ExtractedVariableDeclaration variable)
          | None -> None)
      | ( (BinaryOperator _ | Subscript _ | Name _ | Constant (Constant.String _)),
          Some
            {
              Node.value =
                Expression.Name
                  (Name.Attribute
                    {
                      base =
                        { Node.value = Name (Name.Identifier ("typing_extensions" | "typing")); _ };
                      attribute = "TypeAlias";
                      _;
                    });
              _;
            } )
      | (BinaryOperator _ | Subscript _ | Name _), None ->
          let value =
            Type.preprocess_alias_value value |> delocalize ~create_origin:(fun _ -> None)
          in
          let value_annotation =
            (* before creating the value type, collect the local scope *)
            let local_variables x =
              match variables with
              | Some f -> f x
              | None -> None
            in
            Type.create ~variables:local_variables ~aliases:Type.resolved_empty_aliases value
          in
          if
            not
              (Type.contains_unknown target_annotation
              || Type.contains_unknown value_annotation
              || Type.equal value_annotation target_annotation)
          then
            Some (ExtractedAlias { value; variables })
          else
            None
      | _ -> None
    in

    let extract_alias = function
      | SimpleAssign { value = None; _ } -> None
      | SimpleAssign { explicit_annotation; value = Some value; _ } ->
          process_aliases explicit_annotation value None
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
                let value =
                  from_reference
                    ~location:Location.any
                    ~create_origin:(fun _ -> None)
                    original_name_of_alias
                in
                Some (ExtractedAlias { value; variables = None }))
      | TupleAssign _
      | Class
      | Define _ ->
          None
      | TypeStatement { value; type_params; _ } ->
          (* collect type parameters from type statement *)
          let variables =
            List.map
              ~f:
                (Type.Variable.of_ast_type_param
                   ~create_type:
                     (Type.create
                        ~aliases:Type.resolved_empty_aliases
                        ~variables:Type.resolved_empty_variables))
              type_params
          in
          (* create a mapping which looks up type variables based on their name *)
          let variables variable_name =
            match
              List.find variables ~f:(fun tv -> String.equal (Type.Variable.name tv) variable_name)
            with
            | Some tv -> Some tv
            | None -> None
          in
          (* pass the variable map to the TypeAlias *)
          process_aliases None value (Some variables)
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
            Type.class_attribute_lookups_for_type annotation
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
          | ExtractedVariableDeclaration variable -> Some (RawAlias.VariableDeclaration variable)
          | ExtractedAlias unresolved -> (
              match UnresolvedAlias.checked_resolve queries unresolved with
              | Resolved alias -> Some alias
              | HasDependents { unparsed; dependencies } -> (
                  let solve_pair dependency =
                    get_aliased_type_for (Reference.create dependency) ~visited
                    >>| fun solution -> dependency, solution
                  in
                  List.filter dependencies ~f:(fun dependency ->
                      not (Reference.equal (Reference.create dependency) current))
                  |> List.map ~f:solve_pair
                  |> Option.all
                  >>| String.Map.of_alist_exn
                  >>| fun map ->
                  let aliases ?replace_unbound_parameters_with_any:_ name = Map.find map name in

                  let resolved_aliases ?replace_unbound_parameters_with_any name =
                    match aliases ?replace_unbound_parameters_with_any name with
                    | Some (RawAlias.TypeAlias t) -> Some t
                    | _ -> None
                  in

                  let variable_aliases name =
                    match aliases name with
                    | Some (RawAlias.VariableDeclaration variable) ->
                        let type_variables =
                          Type.Variable.of_declaration
                            ~create_type:
                              (Type.create
                                 ~aliases:resolved_aliases
                                 ~variables:Type.resolved_empty_variables)
                            variable
                        in
                        Some type_variables
                    | _ -> None
                  in
                  match
                    UnresolvedAlias.unchecked_resolve ~unparsed ~variables:variable_aliases ~aliases
                  with
                  | Type.Variable { name; _ } as bare_variable -> (
                      (* Because the raw extract_alias code cannot resolve multiple import hops at
                         once, there's a case where we can have an `ExtractedAlias` that is
                         ulitmately just a chain of imports of a variable declaration. In order for
                         the rest of Pyre to predictably reason about the behavior of
                         `get_declaration` and `get_variable`, it is important that we catch that
                         case here rather than allowing the cache table to treat the import chain as
                         a TypeAlias resolving to a bare Type.Variable. *)
                      match aliases name with
                      | Some (RawAlias.VariableDeclaration _ as declaration) -> declaration
                      | Some non_variable_alias ->
                          (* This case may not be possible (we know the None case is happening), but
                             if we do encounter it we log an error and proceed. *)
                          Log.error
                            "TypeAliasEnvironment resolved type variable with name %s to an alias \
                             %a that is not a declaration when resolving the symbol %a. This \
                             should not occur and will cause downstream generic classes to be \
                             poorly understood by Pyre."
                            name
                            RawAlias.pp
                            non_variable_alias
                            Reference.pp
                            current;
                          non_variable_alias
                      | None ->
                          (* This case is known to be possible because it happens in production,
                             although we do not understand why yet. Log an error, which will help us
                             track this problem down, and proceed. *)
                          Log.warning
                            "Pyre could not resolve the type variable for symbol %s, which is \
                             probably an imported type variable %a. This could cause downstream \
                             generic classes to be poorly understood. You can fix this problem by \
                             avoiding importing the type variable."
                            name
                            Reference.pp
                            current;
                          RawAlias.TypeAlias bare_variable)
                  | resolved_alias -> RawAlias.TypeAlias resolved_alias))
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
      get_type_alias: Type.Primitive.t -> Type.t option;
      get_variable_declaration: Type.Primitive.t -> Type.Variable.Declaration.t option;
    }
  end

  let get_variable Queries.{ get_variable_declaration; get_type_alias; _ } name =
    let aliases ?replace_unbound_parameters_with_any:_ = get_type_alias in
    get_variable_declaration name
    >>| Type.Variable.of_declaration
          ~create_type:(Type.create ~aliases ~variables:Type.resolved_empty_variables)


  let type_contains_untracked_name Queries.{ class_exists; _ } annotation =
    List.exists ~f:(fun class_name -> not (class_exists class_name)) (Type.collect_names annotation)


  (* Convert a type expression of type `Expression.t` to a `Type.t`.
   *
   * In the process we perform two transforms
   * - Verify that all types map to valid classes (Pyre incorrectly assumes all
   *   types are classes; we use `missingFromStubs.ml` to fabricate classes for
   *   special forms that aren't really classes).
   * - Expand type aliases eagerly. NOTE: We use a parameterized macro expansion style
   *   which will detect arity problems, but ignores constraints and variance for
   *   type alias parameters; only generic classes validate these.
   *
   * This parse helper differs from the higher-level `parse_annotation` in that it does
   * not validate type arguments to generic classes; that requires analysis information
   * that is not available here.
   *)
  let parse_annotation_without_sanitizing_type_arguments
      (Queries.{ get_type_alias; _ } as queries)
      ?modify_aliases
      ~variables
      ?(allow_untracked = false)
      expression
    =
    let modify_aliases =
      Option.value modify_aliases ~default:(fun ?replace_unbound_parameters_with_any:_ name -> name)
    in
    let parsed =
      let expression =
        Type.preprocess_alias_value expression |> delocalize ~create_origin:(fun _ -> None)
      in
      let aliases ?replace_unbound_parameters_with_any name =
        get_type_alias name >>| modify_aliases ?replace_unbound_parameters_with_any
      in
      Type.create ~variables ~aliases expression
    in
    if (not allow_untracked) && type_contains_untracked_name queries parsed then
      Type.Top
    else
      parsed


  let param_spec_from_vararg_annotations queries ~args_annotation ~kwargs_annotation () =
    let get_param_spec variable_name =
      match get_variable queries variable_name with
      | Some (Type.Variable.ParamSpecVariable name) -> Some name
      | _ -> None
    in
    Type.Variable.ParamSpec.of_component_annotations
      ~get_param_spec
      ~args_annotation:(delocalize ~create_origin:(fun _ -> None) args_annotation)
      ~kwargs_annotation:(delocalize ~create_origin:(fun _ -> None) kwargs_annotation)
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

  let produce_value unannotated_global_environment key ~dependency =
    let queries =
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

  let get_type_alias environment ?dependency name =
    match get environment ?dependency name with
    | Some (RawAlias.TypeAlias t) -> Some t
    | _ -> None


  let get_variable_declaration environment ?dependency name =
    match get environment ?dependency name with
    | Some (RawAlias.VariableDeclaration t) -> Some t
    | _ -> None


  let unannotated_global_environment = upstream_environment

  let outgoing_queries ?dependency environment =
    OutgoingDataComputation.Queries.
      {
        class_exists =
          UnannotatedGlobalEnvironment.ReadOnly.class_exists
            (unannotated_global_environment environment)
            ?dependency;
        get_type_alias = get_type_alias environment ?dependency;
        get_variable_declaration = get_variable_declaration environment ?dependency;
      }


  let get_variable environment ?dependency =
    OutgoingDataComputation.get_variable (outgoing_queries ?dependency environment)


  let parse_annotation_without_sanitizing_type_arguments environment ?dependency =
    OutgoingDataComputation.parse_annotation_without_sanitizing_type_arguments
      (outgoing_queries ?dependency environment)


  let param_spec_from_vararg_annotations environment ?dependency =
    OutgoingDataComputation.param_spec_from_vararg_annotations
      (outgoing_queries ?dependency environment)
end

module AliasReadOnly = ReadOnly
module UpdateResult = Aliases.UpdateResult
module Testing = Aliases.Testing
