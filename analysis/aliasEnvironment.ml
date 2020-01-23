(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre
open Expression
module PreviousEnvironment = EmptyStubEnvironment

module AliasValue = struct
  type t = Type.alias option

  let prefix = Prefix.make ()

  let description = "Alias"

  let unmarshall value = Marshal.from_string value 0

  let compare = Option.compare Type.compare_alias
end

module UnresolvedAlias = struct
  type t = {
    value: Expression.t;
    target: Reference.t;
  }

  let unchecked_resolve ~unparsed ~target map =
    match Type.create ~aliases:(Map.find map) unparsed with
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
        | Type.Optional Bottom -> false
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
                || Option.is_some
                     (AstEnvironment.ReadOnly.get_module_metadata
                        ( EmptyStubEnvironment.ReadOnly.unannotated_global_environment
                            empty_stub_environment
                        |> UnannotatedGlobalEnvironment.ReadOnly.ast_environment )
                        ?dependency
                        (Reference.create primitive))
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
    | UnannotatedGlobalEnvironment.SimpleAssign { explicit_annotation; value; _ } -> (
        let target_annotation =
          Type.create ~aliases:(fun _ -> None) (from_reference ~location:Location.any name)
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
        | ( Call _,
            Some
              {
                Node.value =
                  Name
                    (Name.Attribute
                      {
                        base = { Node.value = Name (Name.Identifier "typing"); _ };
                        attribute = "TypeAlias";
                        _;
                      });
                _;
              } )
        | ( Name _,
            Some
              {
                Node.value =
                  Name
                    (Name.Attribute
                      {
                        base = { Node.value = Name (Name.Identifier "typing"); _ };
                        attribute = "TypeAlias";
                        _;
                      });
                _;
              } )
        | Call _, None
        | Name _, None -> (
            let value = delocalize value in
            let value_annotation = Type.create ~aliases:(fun _ -> None) value in
            match Type.Variable.parse_declaration value ~target:name with
            | Some variable -> Some (VariableAlias variable)
            | _ ->
                if
                  not
                    ( Type.is_top target_annotation
                    || Type.is_top value_annotation
                    || Type.equal value_annotation target_annotation )
                then
                  Some (TypeAlias { target = name; value })
                else
                  None )
        | _ -> None )
    | UnannotatedGlobalEnvironment.Imported original_name -> (
        if
          UnannotatedGlobalEnvironment.ReadOnly.class_exists
            unannotated_global_environment
            (Reference.show name)
        then
          None
        else
          match Reference.as_list name, Reference.as_list original_name with
          | [single_identifier], [typing; identifier]
            when String.equal typing "typing" && String.equal single_identifier identifier ->
              (* builtins has a bare qualifier. Don't export bare aliases from typing. *)
              None
          | _ ->
              let value = from_reference ~location:Location.any original_name in
              Some (TypeAlias { target = name; value }) )
    | TupleAssign _
    | Define _ ->
        None
  in
  UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global
    ?dependency
    unannotated_global_environment
    name
  >>= extract_alias


let produce_alias empty_stub_environment global_name ~track_dependencies =
  (* TODO(T53786399): Optimize this function. Theres a lot of perf potentially to be gained here,
     currently biasing towards simplicity *)
  let dependency = Option.some_if track_dependencies (SharedMemoryKeys.AliasRegister global_name) in
  let rec get_aliased_type_for current ~visited =
    (* This means we're in a loop *)
    if Set.mem visited current then
      None
    else
      let visited = Set.add visited current in
      let handle_extracted = function
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
                List.map dependencies ~f:solve_pair
                |> Option.all
                >>| String.Map.of_alist_exn
                >>| UnresolvedAlias.unchecked_resolve ~target:current ~unparsed
                >>| fun alias -> Type.TypeAlias alias )
      in
      extract_alias
        (EmptyStubEnvironment.ReadOnly.unannotated_global_environment empty_stub_environment)
        current
        ~dependency
      >>= handle_extracted
  in
  get_aliased_type_for global_name ~visited:Reference.Set.empty


module Aliases = Environment.EnvironmentTable.NoCache (struct
  module PreviousEnvironment = PreviousEnvironment
  module Key = SharedMemoryKeys.StringKey
  module Value = AliasValue

  type trigger = Reference.t

  let convert_trigger = Reference.show

  let key_to_trigger = Reference.create ?prefix:None

  module TriggerSet = Reference.Set

  let lazy_incremental = false

  let produce_value = produce_alias

  let filter_upstream_dependency = function
    | SharedMemoryKeys.AliasRegister name -> Some name
    | _ -> None


  let legacy_invalidated_keys upstream_update =
    UnannotatedGlobalEnvironment.UpdateResult.previous_unannotated_globals upstream_update


  let all_keys unannotated_global_environment =
    UnannotatedGlobalEnvironment.ReadOnly.all_unannotated_globals unannotated_global_environment
    |> List.map ~f:Reference.show


  let serialize_value = function
    | Some alias -> Type.show_alias alias
    | None -> "None"


  let show_key = Fn.id

  let equal_value = Option.equal Type.equal_alias
end)

include Aliases

module ReadOnly = struct
  include Aliases.ReadOnly

  let get_alias = get

  let empty_stub_environment = upstream_environment

  let unannotated_global_environment read_only =
    empty_stub_environment read_only |> EmptyStubEnvironment.ReadOnly.unannotated_global_environment


  let parse_annotation_without_validating_type_parameters
      environment
      ?modify_aliases
      ?dependency
      ?(allow_untracked = false)
      ?(allow_primitives_from_empty_stubs = false)
      expression
    =
    let modify_aliases = Option.value modify_aliases ~default:Fn.id in
    let parsed =
      let expression = delocalize expression in
      let aliases name = get_alias environment ?dependency name >>| modify_aliases in
      Type.create ~aliases expression
    in
    let annotation =
      if allow_primitives_from_empty_stubs then
        parsed
      else
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


  let parse_as_concatenation environment ?dependency expression =
    delocalize expression
    |> Type.OrderedTypes.Concatenation.parse ~aliases:(get_alias environment ?dependency)


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
      ~aliases:(get_alias environment ?dependency)
      ~variable_parameter_annotation
      ~keywords_parameter_annotation
end

module AliasReadOnly = ReadOnly
module UpdateResult = Aliases.UpdateResult
