(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre
open Expression

type t = { unannotated_global_environment: UnannotatedGlobalEnvironment.ReadOnly.t }

let create unannotated_global_environment = { unannotated_global_environment }

module ReadOnly = struct
  type t = {
    get_alias: ?dependency:Reference.t -> Type.Primitive.t -> Type.alias option;
    unannotated_global_environment: UnannotatedGlobalEnvironment.ReadOnly.t;
  }

  let get_alias { get_alias; _ } = get_alias

  let unannotated_global_environment { unannotated_global_environment; _ } =
    unannotated_global_environment
end

module AliasValue = struct
  type t = Type.alias

  let prefix = Prefix.make ()

  let description = "Alias"

  let unmarshall value = Marshal.from_string value 0

  let compare = Type.compare_alias
end

module Aliases =
  Memory.DependencyTrackedTableNoCache
    (SharedMemoryKeys.StringKey)
    (SharedMemoryKeys.ReferenceDependencyKey)
    (AliasValue)

let ast_environment { unannotated_global_environment } =
  UnannotatedGlobalEnvironment.ReadOnly.ast_environment unannotated_global_environment


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

  let checked_resolve
      ({ unannotated_global_environment } as environment)
      { value; target }
      ~dependency
      ()
    =
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
                | Expression.Name name when Expression.is_simple_name name ->
                    Expression.name_to_reference_exn name
                | _ -> Reference.create "typing.Any"
              in
              let module_definition =
                AstEnvironment.ReadOnly.get_module_metadata (ast_environment environment)
              in
              if Module.from_empty_stub ~reference ~module_definition then
                (), Type.Any
              else if
                UnannotatedGlobalEnvironment.ReadOnly.class_exists
                  ?dependency
                  unannotated_global_environment
                  primitive
                || Option.is_some (module_definition (Reference.create primitive))
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

let extract_alias { unannotated_global_environment } name ~dependency =
  let extract_alias = function
    | UnannotatedGlobalEnvironment.SimpleAssign { explicit_annotation; value } -> (
        let target_annotation =
          Type.create
            ~aliases:(fun _ -> None)
            (Expression.from_reference ~location:Location.Reference.any name)
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
              let value = Expression.delocalize value in
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
            let value = Expression.delocalize value in
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
              let value =
                Expression.from_reference ~location:Location.Reference.any original_name
              in
              Some (TypeAlias { target = name; value }) )
  in
  UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global
    ?dependency
    unannotated_global_environment
    name
  >>= extract_alias


let register_aliases environment global_names ~track_dependencies =
  (* TODO(T53786399): Optimize this function. Theres a lot of perf potentially to be gained here,
     currently biasing towards simplicity *)
  (* We must do this in every worker because global state is not shared *)
  Type.Cache.disable ();
  let register global_name =
    let dependency =
      Option.some_if track_dependencies (UnannotatedGlobalEnvironment.AliasRegister global_name)
    in
    let rec get_aliased_type_for current ~visited =
      (* This means we're in a loop *)
      if Set.mem visited current then
        None
      else
        let visited = Set.add visited current in
        let handle_extracted = function
          | VariableAlias variable -> Some (Type.VariableAlias variable)
          | TypeAlias unresolved -> (
            match UnresolvedAlias.checked_resolve environment unresolved ~dependency () with
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
        extract_alias environment current ~dependency >>= handle_extracted
    in
    get_aliased_type_for global_name ~visited:Reference.Set.empty
    |> Option.iter ~f:(Aliases.add (Reference.show global_name))
  in
  List.iter global_names ~f:register;
  Type.Cache.enable ()


module UpdateResult = struct
  type t = {
    triggered_dependencies: SharedMemoryKeys.ReferenceDependencyKey.KeySet.t;
    upstream: UnannotatedGlobalEnvironment.UpdateResult.t;
  }

  let triggered_dependencies { triggered_dependencies; _ } = triggered_dependencies

  let upstream { upstream; _ } = upstream
end

let update environment ~scheduler ~configuration upstream_update =
  let update ~names_to_update ?(track_dependencies = true) () =
    Scheduler.iter
      scheduler
      ~configuration
      ~f:(register_aliases environment ~track_dependencies)
      ~inputs:(Set.to_list names_to_update);

    (* TODO(T53786398): make this no longer global state so we don't have to reset it per worker
       like this *)
    Type.Cache.enable ()
  in
  match configuration with
  | { incremental_style = FineGrained; _ } ->
      let dependencies =
        UnannotatedGlobalEnvironment.DependencyKey.KeySet.elements
          (UnannotatedGlobalEnvironment.UpdateResult.triggered_dependencies upstream_update)
        |> List.filter_map ~f:(function
               | UnannotatedGlobalEnvironment.AliasRegister name -> Some name
               | _ -> None)
      in
      let names_to_update =
        dependencies
        |> List.fold
             ~f:Set.add
             ~init:
               (UnannotatedGlobalEnvironment.UpdateResult.added_unannotated_globals upstream_update)
      in
      let keys_to_invalidate = List.map dependencies ~f:Reference.show |> Aliases.KeySet.of_list in
      let (), triggered_dependencies =
        SharedMemoryKeys.ReferenceDependencyKey.Transaction.empty
        |> Aliases.add_to_transaction ~keys:keys_to_invalidate
        |> SharedMemoryKeys.ReferenceDependencyKey.Transaction.execute
             ~update:(update ~names_to_update)
      in
      { UpdateResult.triggered_dependencies; upstream = upstream_update }
  | _ ->
      let current_and_previous =
        UnannotatedGlobalEnvironment.UpdateResult.current_and_previous_unannotated_globals
          upstream_update
      in
      let () =
        Reference.Set.to_list current_and_previous
        |> List.map ~f:Reference.show
        |> Aliases.KeySet.of_list
        |> Aliases.remove_batch
      in
      update ~names_to_update:current_and_previous ~track_dependencies:false ();
      {
        UpdateResult.triggered_dependencies = SharedMemoryKeys.ReferenceDependencyKey.KeySet.empty;
        upstream = upstream_update;
      }


let read_only { unannotated_global_environment } =
  { ReadOnly.unannotated_global_environment; get_alias = Aliases.get }
