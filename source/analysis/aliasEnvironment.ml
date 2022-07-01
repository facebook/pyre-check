(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre
open Expression
module PreviousEnvironment = EmptyStubEnvironment

let preprocess_alias_value value =
  value
  |> Preprocessing.replace_union_shorthand_in_annotation_expression
  |> Preprocessing.expand_strings_in_annotation_expression


module AliasValue = struct
  type t = Type.alias option

  let prefix = Prefix.make ()

  let description = "Alias"

  let equal = Memory.equal_from_compare (Option.compare Type.compare_alias)
end

module UnresolvedAlias = struct
  type t = {
    value: Expression.t;
    target: Reference.t;
  }

  let unchecked_resolve ~unparsed ~target map =
    match
      Type.create
        ~aliases:(fun ?replace_unbound_parameters_with_any:_ name -> Map.find map name)
        unparsed
    with
    | Type.Variable variable ->
        if Type.Variable.Unary.contains_subvariable variable then
          Type.Any
        else
          Type.Variable { variable with variable = Reference.show target }
    | annotation -> annotation


  type check_result =
    | Resolved of Type.alias
    | HasDependents of {
        unparsed: Expression.t;
        dependencies: string list;
      }

  let checked_resolve empty_stub_environment { value; target } ~dependency () =
    let value_annotation = unchecked_resolve ~unparsed:value ~target String.Map.empty in
    let dependencies = String.Hash_set.create () in
    let module TrackedTransform = Type.Transform.Make (struct
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
              if
                EmptyStubEnvironment.ReadOnly.from_empty_stub
                  empty_stub_environment
                  ?dependency
                  reference
              then
                (), Type.Any
              else if
                UnannotatedGlobalEnvironment.ReadOnly.class_exists
                  ?dependency
                  (EmptyStubEnvironment.ReadOnly.unannotated_global_environment
                     empty_stub_environment)
                  primitive
                || UnannotatedGlobalEnvironment.ReadOnly.module_exists
                     (EmptyStubEnvironment.ReadOnly.unannotated_global_environment
                        empty_stub_environment)
                     ?dependency
                     (Reference.create primitive)
                (* Don't consider "..." in `MyCallable[..., int]` to be a dependent alias. *)
                || Identifier.equal primitive "..."
              then
                (), annotation
              else
                let _ = Hash_set.add dependencies primitive in
                (), annotation
          | _ -> (), annotation
        in
        { Type.Transform.transformed_annotation; new_state }
    end)
    in
    let _, annotation = TrackedTransform.visit () value_annotation in
    if Hash_set.is_empty dependencies then
      Resolved (Type.TypeAlias annotation)
    else
      HasDependents { unparsed = value; dependencies = Hash_set.to_list dependencies }
end

type extracted =
  | VariableAlias of Type.Variable.t
  | TypeAlias of UnresolvedAlias.t

let extract_alias unannotated_global_environment name ~dependency =
  let extract_alias = function
    | UnannotatedGlobal.SimpleAssign { explicit_annotation; value; _ } -> (
        let target_annotation =
          Type.create ~aliases:Type.empty_aliases (from_reference ~location:Location.any name)
        in
        match Node.value value, explicit_annotation with
        | ( _,
            Some
              {
                Node.value =
                  Call
                    {
                      callee =
                        {
                          Node.value =
                            Name
                              (Name.Attribute
                                {
                                  base =
                                    {
                                      Node.value =
                                        Name
                                          (Name.Attribute
                                            {
                                              base =
                                                { Node.value = Name (Name.Identifier "typing"); _ };
                                              attribute = "Type";
                                              _;
                                            });
                                      _;
                                    };
                                  attribute = "__getitem__";
                                  _;
                                });
                          _;
                        };
                      arguments =
                        [
                          {
                            Call.Argument.value =
                              {
                                Node.value =
                                  Call
                                    {
                                      callee =
                                        {
                                          Node.value =
                                            Name
                                              (Name.Attribute
                                                {
                                                  base =
                                                    {
                                                      Node.value =
                                                        Name
                                                          (Name.Attribute
                                                            {
                                                              base =
                                                                {
                                                                  Node.value =
                                                                    Name
                                                                      (Name.Identifier
                                                                        "mypy_extensions");
                                                                  _;
                                                                };
                                                              attribute = "TypedDict";
                                                              _;
                                                            });
                                                      _;
                                                    };
                                                  attribute = "__getitem__";
                                                  _;
                                                });
                                          _;
                                        };
                                      _;
                                    };
                                _;
                              };
                            _;
                          };
                        ];
                    };
                _;
              } ) ->
            if not (Type.is_top target_annotation) then
              let value = delocalize value in
              Some (TypeAlias { target = name; value })
            else
              None
        | ( (Call _ | Name _ | Constant (Constant.String _)),
            Some
              {
                Node.value =
                  Name
                    (Name.Attribute
                      {
                        base = { Node.value = Name (Name.Identifier "typing_extensions"); _ };
                        attribute = "TypeAlias";
                        _;
                      });
                _;
              } )
        | (Call _ | Name _), None -> (
            match Type.Variable.parse_declaration (delocalize value) ~target:name with
            | Some variable -> Some (VariableAlias variable)
            | _ ->
                let value = preprocess_alias_value value |> delocalize in
                let value_annotation = Type.create ~aliases:Type.empty_aliases value in
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
    | UnannotatedGlobal.Imported import -> (
        if
          UnannotatedGlobalEnvironment.ReadOnly.class_exists
            unannotated_global_environment
            (Reference.show name)
        then
          None
        else
          let original_name = UnannotatedGlobal.ImportEntry.deprecated_original_name import in
          match Reference.as_list name, Reference.as_list original_name with
          | [single_identifier], [typing; identifier]
            when String.equal typing "typing" && String.equal single_identifier identifier ->
              (* builtins has a bare qualifier. Don't export bare aliases from typing. *)
              None
          | _ ->
              let value = from_reference ~location:Location.any original_name in
              Some (TypeAlias { target = name; value }))
    | TupleAssign _
    | Class
    | Define _ ->
        None
  in
  UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global
    ?dependency
    unannotated_global_environment
    name
  >>= extract_alias


let produce_alias empty_stub_environment global_name ~dependency =
  let maybe_convert_to_recursive_alias alias_reference = function
    | Type.TypeAlias annotation
      when (not (Identifier.equal (Reference.show alias_reference) "typing_extensions"))
           && Type.RecursiveType.is_recursive_alias_reference
                ~alias_name:(Reference.show alias_reference)
                annotation ->
        let alias_name = Reference.show alias_reference in
        let is_directly_recursive =
          Type.resolve_class annotation
          >>| List.exists ~f:(fun { Type.class_name; _ } -> Identifier.equal class_name alias_name)
          |> Option.value ~default:true
        in
        let is_generic = Type.contains_variable annotation in
        Type.TypeAlias (Type.RecursiveType.create ~name:alias_name ~body:annotation)
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
        | VariableAlias variable -> Some (Type.VariableAlias variable)
        | TypeAlias unresolved -> (
            match
              UnresolvedAlias.checked_resolve empty_stub_environment unresolved ~dependency ()
            with
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
                >>| fun alias -> Type.TypeAlias alias)
      in
      extract_alias
        (EmptyStubEnvironment.ReadOnly.unannotated_global_environment empty_stub_environment)
        current
        ~dependency
      >>= resolve_after_resolving_dependencies
      >>= maybe_convert_to_recursive_alias current
  in
  if Reference.equal global_name (Reference.create "typing.NoReturn") then
    (* TODO(T76821797): We should fix upstream `typeshed` instead of doing special-case like this. *)
    None
  else
    get_aliased_type_for global_name ~visited:Reference.Set.empty


module Aliases = Environment.EnvironmentTable.NoCache (struct
  module PreviousEnvironment = PreviousEnvironment
  module Key = SharedMemoryKeys.StringKey
  module Value = AliasValue

  type trigger = Reference.t [@@deriving sexp, compare]

  let convert_trigger = Reference.show

  let key_to_trigger = Reference.create ?prefix:None

  module TriggerSet = Reference.Set

  let lazy_incremental = false

  let produce_value = produce_alias

  let filter_upstream_dependency = function
    | SharedMemoryKeys.AliasRegister name -> Some name
    | _ -> None


  let trigger_to_dependency name = SharedMemoryKeys.AliasRegister name

  let show_key = Fn.id

  let overlay_owns_key module_tracker_overlay =
    ModuleTracker.Overlay.owns_identifier module_tracker_overlay


  let equal_value = Option.equal Type.equal_alias
end)

include Aliases

module ReadOnly = struct
  include Aliases.ReadOnly

  let get_alias environment ?dependency ?replace_unbound_parameters_with_any:_ name =
    get environment ?dependency name


  let empty_stub_environment = upstream_environment

  let unannotated_global_environment read_only =
    empty_stub_environment read_only |> EmptyStubEnvironment.ReadOnly.unannotated_global_environment


  let parse_annotation_without_validating_type_parameters
      environment
      ?modify_aliases
      ?dependency
      ?(allow_untracked = false)
      expression
    =
    let modify_aliases =
      Option.value modify_aliases ~default:(fun ?replace_unbound_parameters_with_any:_ name -> name)
    in
    let parsed =
      let expression = preprocess_alias_value expression |> delocalize in
      let aliases ?replace_unbound_parameters_with_any name =
        get_alias environment ?dependency name
        >>| modify_aliases ?replace_unbound_parameters_with_any
      in
      Type.create ~aliases expression
    in
    let annotation =
      let constraints = function
        | Type.Primitive name ->
            let originates_from_empty_stub =
              let reference = Reference.create name in
              EmptyStubEnvironment.ReadOnly.from_empty_stub
                ?dependency
                (empty_stub_environment environment)
                reference
            in
            if originates_from_empty_stub then
              Some Type.Any
            else
              None
        | _ -> None
      in
      Type.instantiate parsed ~constraints
    in
    let contains_untracked =
      UnannotatedGlobalEnvironment.ReadOnly.contains_untracked
        (unannotated_global_environment environment)
        ?dependency
    in
    if contains_untracked annotation && not allow_untracked then
      Type.Top
    else
      annotation


  let parse_as_parameter_specification_instance_annotation
      environment
      ?dependency
      ~variable_parameter_annotation
      ~keywords_parameter_annotation
    =
    let variable_parameter_annotation, keywords_parameter_annotation =
      delocalize variable_parameter_annotation, delocalize keywords_parameter_annotation
    in
    Type.Variable.Variadic.Parameters.parse_instance_annotation
      ~create_type:Type.create
      ~aliases:(fun ?replace_unbound_parameters_with_any:_ name ->
        get_alias environment ?dependency name)
      ~variable_parameter_annotation
      ~keywords_parameter_annotation
end

module AliasReadOnly = ReadOnly
module UpdateResult = Aliases.UpdateResult
module Testing = Aliases.Testing
