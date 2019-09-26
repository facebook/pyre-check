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

type dependency =
  | TypeCheckSource of Reference.t
  | ClassConnect of Type.Primitive.t
  | RegisterClassMetadata of Type.Primitive.t
  | UndecoratedFunction of Reference.t
[@@deriving show, compare, sexp]

module DependencyKey = Memory.DependencyKey.Make (struct
  type nonrec t = dependency

  let to_string dependency = sexp_of_dependency dependency |> Sexp.to_string_mach

  let compare = compare_dependency

  type out = dependency

  let from_string string = Sexp.of_string string |> dependency_of_sexp
end)

module ReadOnly = struct
  type t = {
    get_alias: ?dependency:dependency -> Type.Primitive.t -> Type.alias option;
    unannotated_global_environment: UnannotatedGlobalEnvironment.ReadOnly.t;
  }

  let get_alias { get_alias; _ } = get_alias

  let unannotated_global_environment { unannotated_global_environment; _ } =
    unannotated_global_environment


  let parse_annotation_without_validating_type_parameters
      { get_alias; unannotated_global_environment; _ }
      ?(modify_aliases = Fn.id)
      ?dependency
      ?(allow_untracked = false)
      ?(allow_primitives_from_empty_stubs = false)
      expression
    =
    let parsed =
      let expression = Expression.delocalize expression in
      let aliases name = get_alias ?dependency name >>| modify_aliases in
      Type.create ~aliases expression
    in
    let annotation =
      if allow_primitives_from_empty_stubs then
        parsed
      else
        let constraints = function
          | Type.Primitive name ->
              let dependency =
                let translate = function
                  | TypeCheckSource source -> AstEnvironment.TypeCheckSource source
                  | ClassConnect class_name -> AstEnvironment.ClassConnect class_name
                  | RegisterClassMetadata class_name ->
                      AstEnvironment.RegisterClassMetadata class_name
                  | UndecoratedFunction function_name ->
                      AstEnvironment.UndecoratedFunction function_name
                in
                dependency >>| translate
              in
              let originates_from_empty_stub =
                let ast_environment =
                  UnannotatedGlobalEnvironment.ReadOnly.ast_environment
                    unannotated_global_environment
                in
                let reference = Reference.create name in
                AstEnvironment.ReadOnly.from_empty_stub ?dependency ast_environment reference
              in
              if originates_from_empty_stub then
                Some Type.Any
              else
                None
          | _ -> None
        in
        Type.instantiate parsed ~constraints
    in
    let contains_untracked annotation =
      let dependency =
        let translate = function
          | TypeCheckSource source -> UnannotatedGlobalEnvironment.TypeCheckSource source
          | ClassConnect class_name -> UnannotatedGlobalEnvironment.ClassConnect class_name
          | RegisterClassMetadata class_name ->
              UnannotatedGlobalEnvironment.RegisterClassMetadata class_name
          | UndecoratedFunction function_name ->
              UnannotatedGlobalEnvironment.UndecoratedFunction function_name
        in
        dependency >>| translate
      in
      let is_tracked =
        UnannotatedGlobalEnvironment.ReadOnly.class_exists
          unannotated_global_environment
          ?dependency
      in
      List.exists ~f:(fun annotation -> not (is_tracked annotation)) (Type.elements annotation)
    in
    if contains_untracked annotation && not allow_untracked then
      Type.Top
    else
      annotation


  let parse_as_concatenation { get_alias; _ } ?dependency expression =
    Expression.delocalize expression
    |> Type.OrderedTypes.Concatenation.parse ~aliases:(get_alias ?dependency)


  let parse_as_parameter_specification_instance_annotation
      { get_alias; _ }
      ?dependency
      ~variable_parameter_annotation
      ~keywords_parameter_annotation
    =
    let variable_parameter_annotation, keywords_parameter_annotation =
      ( Expression.delocalize variable_parameter_annotation,
        Expression.delocalize keywords_parameter_annotation )
    in
    Type.Variable.Variadic.Parameters.parse_instance_annotation
      ~aliases:(get_alias ?dependency)
      ~variable_parameter_annotation
      ~keywords_parameter_annotation
end

module AliasValue = struct
  type t = Type.alias

  let prefix = Prefix.make ()

  let description = "Alias"

  let unmarshall value = Marshal.from_string value 0

  let compare = Type.compare_alias
end

module Aliases =
  Memory.DependencyTrackedTableNoCache (SharedMemoryKeys.StringKey) (DependencyKey) (AliasValue)

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
    let unannotated_global_environment_dependency =
      dependency >>| fun dependency -> UnannotatedGlobalEnvironment.AliasRegister dependency
    in
    let ast_environment_dependency =
      dependency >>| fun dependency -> AstEnvironment.AliasRegister dependency
    in
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
              let ast_environment = ast_environment environment in
              if
                AstEnvironment.ReadOnly.from_empty_stub
                  ast_environment
                  ?dependency:ast_environment_dependency
                  reference
              then
                (), Type.Any
              else if
                UnannotatedGlobalEnvironment.ReadOnly.class_exists
                  ?dependency:unannotated_global_environment_dependency
                  unannotated_global_environment
                  primitive
                || Option.is_some
                     (AstEnvironment.ReadOnly.get_module_metadata
                        ast_environment
                        ?dependency:ast_environment_dependency
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

let extract_alias { unannotated_global_environment } name ~dependency =
  let extract_alias = function
    | UnannotatedGlobalEnvironment.SimpleAssign { explicit_annotation; value; _ } -> (
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
    | Define _ -> None
  in
  let unannotated_global_environment_dependency =
    dependency >>| fun dependency -> UnannotatedGlobalEnvironment.AliasRegister dependency
  in
  UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global
    ?dependency:unannotated_global_environment_dependency
    unannotated_global_environment
    name
  >>= extract_alias


let register_aliases environment global_names ~track_dependencies =
  (* TODO(T53786399): Optimize this function. Theres a lot of perf potentially to be gained here,
     currently biasing towards simplicity *)
  (* We must do this in every worker because global state is not shared *)
  Type.Cache.disable ();
  let register global_name =
    let dependency = Option.some_if track_dependencies global_name in
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
    triggered_dependencies: DependencyKey.KeySet.t;
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
      let global_environment_dependencies =
        UnannotatedGlobalEnvironment.DependencyKey.KeySet.elements
          (UnannotatedGlobalEnvironment.UpdateResult.triggered_dependencies upstream_update)
        |> List.filter_map ~f:(function
               | UnannotatedGlobalEnvironment.AliasRegister name -> Some name
               | _ -> None)
      in
      let ast_environment_dependencies =
        UnannotatedGlobalEnvironment.UpdateResult.upstream upstream_update
        |> AstEnvironment.UpdateResult.triggered_dependencies
        |> AstEnvironment.DependencyKey.KeySet.elements
        |> List.filter_map ~f:(function
               | AstEnvironment.AliasRegister name -> Some name
               | _ -> None)
      in
      let dependencies = global_environment_dependencies @ ast_environment_dependencies in
      let names_to_update =
        dependencies
        |> List.fold
             ~f:Set.add
             ~init:
               (UnannotatedGlobalEnvironment.UpdateResult.added_unannotated_globals upstream_update)
      in
      let keys_to_invalidate = List.map dependencies ~f:Reference.show |> Aliases.KeySet.of_list in
      let (), triggered_dependencies =
        DependencyKey.Transaction.empty
        |> Aliases.add_to_transaction ~keys:keys_to_invalidate
        |> DependencyKey.Transaction.execute ~update:(update ~names_to_update)
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
        UpdateResult.triggered_dependencies = DependencyKey.KeySet.empty;
        upstream = upstream_update;
      }


let read_only { unannotated_global_environment } =
  { ReadOnly.unannotated_global_environment; get_alias = Aliases.get }
