(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast
open Statement
open SharedMemoryKeys

module ResolvedReference = struct
  type export =
    | FromModuleGetattr
    | Exported of Module.Export.Name.t
  [@@deriving sexp, compare, hash]

  type t =
    | Module of Reference.t
    | ModuleAttribute of {
        from: Reference.t;
        name: Identifier.t;
        export: export;
        remaining: Identifier.t list;
      }
    | PlaceholderStub of {
        stub_module: Reference.t;
        remaining: Identifier.t list;
      }
  [@@deriving sexp, compare, hash]

  (* In type-checking code, we want to short-circuit recursion for Expression.Name resolution
     whenever the name points to a top-level module attribute. We do not want to short-circuit for
     nested attributes. This function facilitates making that decision. *)
  let as_module_toplevel_reference = function
    | Module qualifier -> Some qualifier
    | PlaceholderStub { stub_module; remaining } ->
        Some (Reference.combine stub_module (Reference.create_from_list remaining))
    | ModuleAttribute { from; name; remaining = []; _ } -> Some (Reference.create ~prefix:from name)
    | ModuleAttribute _ -> None
end

module ReadOnly = struct
  type t = {
    ast_environment: AstEnvironment.ReadOnly.t;
    class_exists: ?dependency:DependencyKey.registered -> string -> bool;
    all_classes: unit -> Type.Primitive.t list;
    all_indices: unit -> IndexTracker.t list;
    all_unannotated_globals: unit -> Reference.t list;
    get_define_names: ?dependency:DependencyKey.registered -> Reference.t -> Reference.t list;
    get_class_summary:
      ?dependency:DependencyKey.registered -> string -> ClassSummary.t Node.t option;
    get_unannotated_global:
      ?dependency:DependencyKey.registered -> Reference.t -> UnannotatedGlobal.t option;
    get_function_definition:
      ?dependency:DependencyKey.registered -> Reference.t -> FunctionDefinition.t option;
    get_module_metadata: ?dependency:DependencyKey.registered -> Reference.t -> Module.t option;
    module_exists: ?dependency:SharedMemoryKeys.DependencyKey.registered -> Reference.t -> bool;
  }

  let ast_environment { ast_environment; _ } = ast_environment

  let controls { ast_environment; _ } = AstEnvironment.ReadOnly.controls ast_environment

  let unannotated_global_environment = Fn.id

  let all_classes { all_classes; _ } = all_classes ()

  let all_indices { all_indices; _ } = all_indices ()

  let all_unannotated_globals { all_unannotated_globals; _ } = all_unannotated_globals ()

  let get_define_names { get_define_names; _ } = get_define_names

  let get_module_metadata { get_module_metadata; _ } = get_module_metadata

  let module_exists { module_exists; _ } = module_exists

  let get_class_summary { get_class_summary; _ } = get_class_summary

  let class_exists { class_exists; _ } = class_exists

  let get_function_definition { get_function_definition; _ } = get_function_definition

  let get_unannotated_global { get_unannotated_global; _ } = get_unannotated_global

  let get_define_body environment ?dependency name =
    get_function_definition environment ?dependency name
    >>= fun { FunctionDefinition.body; _ } -> body


  let primitive_name annotation =
    let primitive, _ = Type.split annotation in
    Type.primitive_name primitive


  let is_protocol { get_class_summary; _ } ?dependency annotation =
    primitive_name annotation
    >>= get_class_summary ?dependency
    >>| Node.value
    >>| ClassSummary.is_protocol
    |> Option.value ~default:false


  let contains_untracked read_only ?dependency annotation =
    let is_tracked = class_exists read_only ?dependency in
    List.exists ~f:(fun annotation -> not (is_tracked annotation)) (Type.elements annotation)


  let legacy_resolve_exports read_only ?dependency reference =
    (* Resolve exports. Fixpoint is necessary due to export/module name conflicts: P59503092 *)
    let widening_threshold = 25 in
    let rec resolve_exports_fixpoint ~reference ~visited ~count =
      if Set.mem visited reference || count > widening_threshold then
        reference
      else
        let rec resolve_exports ~lead ~tail =
          match tail with
          | head :: tail ->
              let incremented_lead = lead @ [head] in
              if
                Option.is_some
                  (get_module_metadata
                     ?dependency
                     read_only
                     (Reference.create_from_list incremented_lead))
              then
                resolve_exports ~lead:incremented_lead ~tail
              else
                get_module_metadata ?dependency read_only (Reference.create_from_list lead)
                >>| (fun definition ->
                      match Module.legacy_aliased_export definition (Reference.create head) with
                      | Some export -> Reference.combine export (Reference.create_from_list tail)
                      | _ -> resolve_exports ~lead:(lead @ [head]) ~tail)
                |> Option.value ~default:reference
          | _ -> reference
        in
        match Reference.as_list reference with
        | head :: tail ->
            let exported_reference = resolve_exports ~lead:[head] ~tail in
            if Reference.is_strict_prefix ~prefix:reference exported_reference then
              reference
            else
              resolve_exports_fixpoint
                ~reference:exported_reference
                ~visited:(Set.add visited reference)
                ~count:(count + 1)
        | _ -> reference
    in
    resolve_exports_fixpoint ~reference ~visited:Reference.Set.empty ~count:0


  module ResolveExportItem = struct
    module T = struct
      type t = {
        current_module: Reference.t;
        name: Identifier.t;
      }
      [@@deriving sexp, compare, hash]
    end

    include T
    include Hashable.Make (T)
  end

  let resolve_exports read_only ?dependency ?(from = Reference.empty) reference =
    let visited_set = ResolveExportItem.Hash_set.create () in
    let rec resolve_module_alias ~current_module ~names_to_resolve () =
      match get_module_metadata ?dependency read_only current_module with
      | None -> (
          (* If the module is not found:
           *  - first try to look up the more-specific module from including
           *    more of the reference we are looking up
           *    - this is necessary in order to do lazy module tracking which
           *      prevents us from fully supporting namespace modules.
           * - If that also fails, check for a placeholder stub.
           *)
          match names_to_resolve with
          | head :: tail ->
              resolve_module_alias
                ~current_module:(Reference.create ~prefix:current_module head)
                ~names_to_resolve:tail
                ()
          | [] ->
              let rec resolve_placeholder_stub sofar = function
                | [] -> None
                | name :: prefixes -> (
                    let checked_module = List.rev prefixes |> Reference.create_from_list in
                    let sofar = name :: sofar in
                    match get_module_metadata ?dependency read_only checked_module with
                    | Some module_metadata when Module.empty_stub module_metadata ->
                        Some
                          (ResolvedReference.PlaceholderStub
                             { stub_module = checked_module; remaining = sofar })
                    | _ -> resolve_placeholder_stub sofar prefixes)
              in
              (* Make sure none of the parent of `current_module` is placeholder stub *)
              resolve_placeholder_stub
                names_to_resolve
                (Reference.as_list current_module |> List.rev))
      | Some module_metadata -> (
          match Module.empty_stub module_metadata with
          | true ->
              Some
                (ResolvedReference.PlaceholderStub
                   { stub_module = current_module; remaining = names_to_resolve })
          | false -> (
              match names_to_resolve with
              | [] -> Some (ResolvedReference.Module current_module)
              | next_name :: rest_names -> (
                  let item = { ResolveExportItem.current_module; name = next_name } in
                  match Hash_set.strict_add visited_set item with
                  | Result.Error _ ->
                      (* Module alias cycle detected. Abort resolution. *)
                      None
                  | Result.Ok _ -> (
                      match Module.get_export module_metadata next_name with
                      | None -> (
                          match Module.get_export module_metadata "__getattr__" with
                          | Some Module.Export.(Name (Define { is_getattr_any = true })) ->
                              Some
                                (ResolvedReference.ModuleAttribute
                                   {
                                     from = current_module;
                                     name = next_name;
                                     export = ResolvedReference.FromModuleGetattr;
                                     remaining = rest_names;
                                   })
                          | _ ->
                              (* We could be hitting an implicit module, or we could be hitting an
                                 explicit module whose name is a prefix of another explicit module.
                                 Keep moving through the current reference chain to make sure we
                                 don't mis-handle those cases. *)
                              resolve_module_alias
                                ~current_module:
                                  (Reference.create next_name |> Reference.combine current_module)
                                ~names_to_resolve:rest_names
                                ())
                      | Some (Module.Export.NameAlias { from; name }) ->
                          if Reference.equal current_module from then
                            (* This could legitimately happen when an __init__ module trying to
                               import its sibling modules *)
                            resolve_module_alias
                              ~current_module:(Reference.create name |> Reference.combine from)
                              ~names_to_resolve:rest_names
                              ()
                          else
                            (* We don't know if `name` refers to a module or not. Move forward on
                               the alias chain. *)
                            resolve_module_alias
                              ~current_module:from
                              ~names_to_resolve:(name :: rest_names)
                              ()
                      | Some (Module.Export.Module name) ->
                          (* `name` is definitely a module. *)
                          resolve_module_alias ~current_module:name ~names_to_resolve:rest_names ()
                      | Some (Module.Export.Name export) ->
                          (* We find a non-module. *)
                          Some
                            (ResolvedReference.ModuleAttribute
                               {
                                 from = current_module;
                                 name = next_name;
                                 export = ResolvedReference.Exported export;
                                 remaining = rest_names;
                               })))))
    in
    resolve_module_alias ~current_module:from ~names_to_resolve:(Reference.as_list reference) ()


  let first_matching_class_decorator
      read_only
      ?dependency
      ~names
      { Node.value = { ClassSummary.decorators; _ }; _ }
    =
    let resolve_and_check_for_match decorator =
      match Decorator.from_expression decorator with
      | None -> None
      | Some ({ Ast.Statement.Decorator.name = { Node.value = name; location }; _ } as decorator) ->
          let resolved_name =
            match resolve_exports read_only ?dependency name with
            | Some (ResolvedReference.ModuleAttribute { from; name; remaining; _ }) ->
                Reference.create_from_list (name :: remaining) |> Reference.combine from
            | _ -> name
          in
          let with_matched_name_if_matches name_to_match =
            if String.equal (Reference.show resolved_name) name_to_match then
              Some { decorator with name = { Node.value = resolved_name; location } }
            else
              None
          in
          List.find_map names ~f:with_matched_name_if_matches
    in
    List.find_map decorators ~f:resolve_and_check_for_match


  let exists_matching_class_decorator read_only ?dependency ~names class_summary =
    first_matching_class_decorator read_only ?dependency ~names class_summary |> Option.is_some
end

module UpdateResult = struct
  type t = {
    triggered_dependencies: DependencyKey.RegisteredSet.t;
    upstream: AstEnvironment.UpdateResult.t;
  }

  let locally_triggered_dependencies { triggered_dependencies; _ } = triggered_dependencies

  let all_triggered_dependencies { triggered_dependencies; upstream; _ } =
    [triggered_dependencies; AstEnvironment.UpdateResult.triggered_dependencies upstream]


  let invalidated_modules { upstream; _ } = AstEnvironment.UpdateResult.invalidated_modules upstream

  let module_updates { upstream; _ } = AstEnvironment.UpdateResult.module_updates upstream

  let unannotated_global_environment_update_result = Fn.id
end

module FromReadOnlyUpstream = struct
  (* The key tracking is necessary because there is no empirical way to determine which classes
     exist for a given class. This "fan-out" necessitates internal tracking. However, this module
     need not be sealed to ensure write only-ness since we're not dependency tracking this, since
     it's only used internally for doing our dependency analysis, and for all_classes, which is only
     used for all-or-nothing operations like validating the class hierarchy and printing it for
     debugging purposes *)
  module KeyTracker = struct
    module ClassKeyValue = struct
      type t = Identifier.t list [@@deriving compare]

      let prefix = Prefix.make ()

      let description = "Class keys"
    end

    module UnannotatedGlobalKeyValue = struct
      type t = Reference.t list [@@deriving compare]

      let prefix = Prefix.make ()

      let description = "Class keys"
    end

    module ClassKeys =
      Memory.FirstClass.WithCache.Make (SharedMemoryKeys.ReferenceKey) (ClassKeyValue)
    module UnannotatedGlobalKeys =
      Memory.FirstClass.WithCache.Make (SharedMemoryKeys.ReferenceKey) (UnannotatedGlobalKeyValue)

    type t = {
      class_keys: ClassKeys.t;
      unannotated_global_keys: UnannotatedGlobalKeys.t;
    }

    let create () =
      {
        class_keys = ClassKeys.create ();
        unannotated_global_keys = UnannotatedGlobalKeys.create ();
      }


    let add_class_keys { class_keys; _ } = ClassKeys.add class_keys

    let add_unannotated_global_keys { unannotated_global_keys; _ } =
      UnannotatedGlobalKeys.add unannotated_global_keys


    let get_class_keys { class_keys; _ } qualifiers =
      ClassKeys.KeySet.of_list qualifiers
      |> ClassKeys.get_batch class_keys
      |> ClassKeys.KeyMap.values
      |> List.filter_map ~f:Fn.id
      |> List.concat


    let get_unannotated_global_keys { unannotated_global_keys; _ } qualifiers =
      UnannotatedGlobalKeys.KeySet.of_list qualifiers
      |> UnannotatedGlobalKeys.get_batch unannotated_global_keys
      |> UnannotatedGlobalKeys.KeyMap.values
      |> List.filter_map ~f:Fn.id
      |> List.concat


    module PreviousKeys = struct
      type t = {
        previous_classes_list: Type.Primitive.t list;
        previous_classes: Type.Primitive.Set.t;
        previous_unannotated_globals_list: Reference.t list;
        previous_unannotated_globals: Reference.Set.t;
      }
    end

    let get_previous_keys_and_clear
        ({ class_keys; unannotated_global_keys } as key_tracker)
        invalidated_modules
      =
      let previous_classes_list = get_class_keys key_tracker invalidated_modules in
      let previous_unannotated_globals_list =
        get_unannotated_global_keys key_tracker invalidated_modules
      in
      let previous_classes = Type.Primitive.Set.of_list previous_classes_list in
      let previous_unannotated_globals = Reference.Set.of_list previous_unannotated_globals_list in
      ClassKeys.KeySet.of_list invalidated_modules |> ClassKeys.remove_batch class_keys;
      UnannotatedGlobalKeys.KeySet.of_list invalidated_modules
      |> UnannotatedGlobalKeys.remove_batch unannotated_global_keys;
      PreviousKeys.
        {
          previous_classes_list;
          previous_classes;
          previous_unannotated_globals_list;
          previous_unannotated_globals;
        }
  end

  module ModuleValue = struct
    type t = Module.t

    let prefix = Prefix.make ()

    let description = "Module"

    let equal = Module.equal
  end

  module Modules = struct
    include
      DependencyTrackedMemory.DependencyTrackedTableWithCache
        (SharedMemoryKeys.ReferenceKey)
        (DependencyKey)
        (ModuleValue)

    let is_qualifier = true

    let key_to_reference = Fn.id
  end

  module DefineNamesValue = struct
    type t = Reference.t list [@@deriving equal]

    let prefix = Prefix.make ()

    let description = "DefineListing"
  end

  module DefineNames = struct
    include
      DependencyTrackedMemory.DependencyTrackedTableWithCache
        (SharedMemoryKeys.ReferenceKey)
        (DependencyKey)
        (DefineNamesValue)

    let is_qualifier = true

    let key_to_reference = Fn.id

    let get_define_names define_names qualifiers =
      get_batch define_names (KeySet.of_list qualifiers)
      |> KeyMap.values
      |> List.filter_opt
      |> List.concat
  end

  module ClassSummaryValue = struct
    type t = ClassSummary.t Node.t

    let prefix = Prefix.make ()

    let description = "ClassSummary"

    let equal = Memory.equal_from_compare (Node.compare ClassSummary.compare)
  end

  module ClassSummaries = struct
    include
      DependencyTrackedMemory.DependencyTrackedTableWithCache
        (SharedMemoryKeys.StringKey)
        (DependencyKey)
        (ClassSummaryValue)

    let is_qualifier = false

    let key_to_reference name = Reference.create name
  end

  module FunctionDefinitionValue = struct
    type t = FunctionDefinition.t

    let description = "FunctionDefinition"

    let prefix = Prefix.make ()

    let equal definition0 definition1 =
      Int.equal 0 (FunctionDefinition.compare definition0 definition1)
  end

  module FunctionDefinitions = struct
    include
      DependencyTrackedMemory.DependencyTrackedTableWithCache
        (SharedMemoryKeys.ReferenceKey)
        (DependencyKey)
        (FunctionDefinitionValue)

    let is_qualifier = false

    let key_to_reference = Fn.id
  end

  module UnannotatedGlobalValue = struct
    type t = UnannotatedGlobal.t

    let prefix = Prefix.make ()

    let description = "UnannotatedGlobal"

    let equal = Memory.equal_from_compare UnannotatedGlobal.compare
  end

  module UnannotatedGlobals = struct
    include
      DependencyTrackedMemory.DependencyTrackedTableNoCache
        (SharedMemoryKeys.ReferenceKey)
        (DependencyKey)
        (UnannotatedGlobalValue)

    let is_qualifier = false

    let key_to_reference = Fn.id
  end

  module ReadWrite = struct
    type t = {
      key_tracker: KeyTracker.t;
      modules: Modules.t;
      define_names: DefineNames.t;
      class_summaries: ClassSummaries.t;
      function_definitions: FunctionDefinitions.t;
      unannotated_globals: UnannotatedGlobals.t;
      ast_environment: AstEnvironment.ReadOnly.t;
    }

    let create ast_environment =
      {
        key_tracker = KeyTracker.create ();
        modules = Modules.create ();
        define_names = DefineNames.create ();
        class_summaries = ClassSummaries.create ();
        function_definitions = FunctionDefinitions.create ();
        unannotated_globals = UnannotatedGlobals.create ();
        ast_environment;
      }


    let controls { ast_environment; _ } = AstEnvironment.ReadOnly.controls ast_environment
  end

  include ReadWrite

  let set_module { modules; _ } ~qualifier module_ = Modules.add modules qualifier module_

  let set_class_summary { class_summaries; _ } ~name class_summary =
    ClassSummaries.write_around class_summaries name class_summary


  let set_function_definition { function_definitions; _ } ~name function_definition =
    FunctionDefinitions.write_around function_definitions name function_definition


  let set_unannotated_global { unannotated_globals; _ } ~name unannotated_global =
    UnannotatedGlobals.add unannotated_globals name unannotated_global


  let set_class_summaries
      ({ key_tracker; _ } as environment)
      ({ Source.module_path = { ModulePath.qualifier; _ }; _ } as source)
    =
    (* TODO (T57944324): Support checking classes that are nested inside function bodies *)
    let module ClassCollector = Visit.MakeStatementVisitor (struct
      type t = Class.t Node.t list

      let visit_children _ = true

      let statement _ sofar = function
        | { Node.location; value = Statement.Class definition } ->
            { Node.location; value = definition } :: sofar
        | _ -> sofar
    end)
    in
    let classes = ClassCollector.visit [] source in
    let classes =
      match Reference.as_list qualifier with
      | [] -> classes @ MissingFromStubs.missing_builtin_classes
      | ["typing"] -> classes @ MissingFromStubs.missing_typing_classes
      | ["typing_extensions"] -> classes @ MissingFromStubs.missing_typing_extensions_classes
      | _ -> classes
    in
    let register new_annotations { Node.location; value = { Class.name; _ } as definition } =
      let primitive = Reference.show name in
      let definition =
        match primitive with
        | "type" ->
            let value =
              Type.expression
                (Type.parametric "typing.Generic" [Single (Type.variable "typing._T")])
            in
            { definition with Class.base_arguments = [{ name = None; value }] }
        | _ -> definition
      in
      set_class_summary
        environment
        ~name:primitive
        { Node.location; value = ClassSummary.create ~qualifier definition };
      Set.add new_annotations primitive
    in
    List.fold classes ~init:Type.Primitive.Set.empty ~f:register
    |> Set.to_list
    |> KeyTracker.add_class_keys key_tracker qualifier


  let set_function_definitions
      ({ define_names; _ } as environment)
      ({ Source.module_path = { ModulePath.qualifier; is_external; _ }; _ } as source)
    =
    match is_external with
    | true ->
        (* Do not collect function bodies for external sources as they won't get type checked *)
        ()
    | false ->
        let function_definitions = FunctionDefinition.collect_defines source in
        let register (name, function_definition) =
          set_function_definition environment ~name function_definition;
          name
        in
        List.map function_definitions ~f:register
        |> List.sort ~compare:Reference.compare
        |> DefineNames.add define_names qualifier


  let set_unannotated_globals
      ({ key_tracker; _ } as environment)
      ({ Source.module_path = { ModulePath.qualifier; _ }; _ } as source)
    =
    let write { UnannotatedGlobal.Collector.Result.name; unannotated_global } =
      let name = Reference.create name |> Reference.combine qualifier in
      set_unannotated_global environment ~name unannotated_global;
      name
    in
    let merge_defines unannotated_globals_alist =
      let not_defines, defines =
        List.partition_map unannotated_globals_alist ~f:(function
            | { UnannotatedGlobal.Collector.Result.name; unannotated_global = Define defines } ->
                Either.Second (name, defines)
            | x -> Either.First x)
      in
      let add_to_map sofar (name, defines) =
        let merge_with_existing to_merge = function
          | None -> Some to_merge
          | Some existing -> Some (to_merge @ existing)
        in
        Map.change sofar name ~f:(merge_with_existing defines)
      in
      List.fold defines ~f:add_to_map ~init:Identifier.Map.empty
      |> Identifier.Map.to_alist
      |> List.map ~f:(fun (name, defines) ->
             {
               UnannotatedGlobal.Collector.Result.name;
               unannotated_global = Define (List.rev defines);
             })
      |> fun defines -> List.append defines not_defines
    in
    let drop_classes unannotated_globals =
      let is_not_class = function
        | { UnannotatedGlobal.Collector.Result.unannotated_global = Class; _ } -> false
        | _ -> true
      in
      List.filter unannotated_globals ~f:is_not_class
    in
    let globals = UnannotatedGlobal.Collector.from_source source |> merge_defines |> drop_classes in
    let globals =
      match Reference.as_list qualifier with
      | [] -> globals @ MissingFromStubs.missing_builtin_globals
      | _ -> globals
    in
    globals |> List.map ~f:write |> KeyTracker.add_unannotated_global_keys key_tracker qualifier


  let add_to_transaction
      { modules; class_summaries; function_definitions; unannotated_globals; define_names; _ }
      transaction
      ~previous_classes_list
      ~previous_unannotated_globals_list
      ~previous_defines_list
      ~invalidated_modules
    =
    let module_keys = Modules.KeySet.of_list invalidated_modules in
    let class_keys = ClassSummaries.KeySet.of_list previous_classes_list in
    let defines_keys = FunctionDefinitions.KeySet.of_list previous_defines_list in
    let unannotated_globals_keys =
      UnannotatedGlobals.KeySet.of_list previous_unannotated_globals_list
    in
    transaction
    |> Modules.add_to_transaction modules ~keys:module_keys
    |> DefineNames.add_to_transaction define_names ~keys:module_keys
    |> ClassSummaries.add_to_transaction class_summaries ~keys:class_keys
    |> FunctionDefinitions.add_to_transaction function_definitions ~keys:defines_keys
    |> UnannotatedGlobals.add_to_transaction unannotated_globals ~keys:unannotated_globals_keys


  let get_all_dependents ~class_additions ~unannotated_global_additions ~define_additions =
    let function_and_class_dependents =
      DependencyKey.RegisteredSet.union
        (ClassSummaries.KeySet.of_list class_additions |> ClassSummaries.get_all_dependents)
        (FunctionDefinitions.KeySet.of_list define_additions
        |> FunctionDefinitions.get_all_dependents)
    in
    DependencyKey.RegisteredSet.union
      function_and_class_dependents
      (UnannotatedGlobals.KeySet.of_list unannotated_global_additions
      |> UnannotatedGlobals.get_all_dependents)


  let set_module_data
      environment
      ({ Source.module_path = { ModulePath.qualifier; _ }; _ } as source)
    =
    set_class_summaries environment source;
    set_function_definitions environment source;
    set_unannotated_globals environment source;
    (* We must set this last, because lazy-loading uses Module.mem to determine whether the source
       has already been processed. So setting it earlier can lead to data races *)
    set_module environment ~qualifier (Module.create source)


  module LazyLoader = struct
    type t = {
      environment: ReadWrite.t;
      ast_environment: AstEnvironment.ReadOnly.t;
    }

    let load_module_if_tracked { environment; ast_environment } qualifier =
      if not (Modules.mem environment.modules qualifier) then
        if AstEnvironment.ReadOnly.is_module_tracked ast_environment qualifier then
          match
            AstEnvironment.ReadOnly.get_processed_source
              ~track_dependency:true
              ast_environment
              qualifier
          with
          | Some source -> set_module_data environment source
          | None -> ()


    let load_all_possible_modules loader ~is_qualifier reference =
      let load_module_if_tracked = load_module_if_tracked loader in
      let ancestors_descending = Reference.possible_qualifiers reference in
      List.iter ancestors_descending ~f:load_module_if_tracked;
      if is_qualifier then load_module_if_tracked reference;
      ()
  end

  module ReadOnlyTable = struct
    module type S = sig
      type key

      type value

      type table

      type t

      val create : loader:LazyLoader.t -> table -> t

      val mem : t -> ?dependency:DependencyKey.registered -> key -> bool

      val get : t -> ?dependency:DependencyKey.registered -> key -> value option
    end

    module type In = sig
      type key

      type value

      type t

      val is_qualifier : bool

      val key_to_reference : key -> Reference.t

      val mem : t -> ?dependency:DependencyKey.registered -> key -> bool

      val get : t -> ?dependency:DependencyKey.registered -> key -> value option
    end

    module Make (In : In) :
      S with type key := In.key and type value := In.value and type table := In.t = struct
      type t = {
        loader: LazyLoader.t;
        table: In.t;
      }

      let create ~loader table = { loader; table }

      let is_qualifier = In.is_qualifier

      let mem { loader; table } ?dependency key =
        (* First handle the case where it's already in the hash map *)
        match In.mem table ?dependency key with
        | true -> true
        | false ->
            (* If no precomputed value exists, we make sure all potential qualifiers are loaded *)
            LazyLoader.load_all_possible_modules loader ~is_qualifier (In.key_to_reference key);
            (* We try fetching again *)
            In.mem table ?dependency key


      let get { loader; table } ?dependency key =
        (* The first get finds precomputed values *)
        match In.get table ?dependency key with
        | Some _ as hit -> hit
        | None ->
            (* If no precomputed value exists, we make sure all potential qualifiers are loaded *)
            LazyLoader.load_all_possible_modules loader ~is_qualifier (In.key_to_reference key);
            (* We try fetching again *)
            In.get table ?dependency key
    end
  end

  let cold_start ({ ast_environment; _ } as environment) =
    (* Eagerly load `builtins.pyi` + the project sources but nothing else *)
    AstEnvironment.ReadOnly.get_processed_source
      ast_environment
      ~track_dependency:true
      Reference.empty
    >>| set_module_data environment
    |> Option.value ~default:()


  let read_only
      ({
         ast_environment;
         key_tracker;
         modules;
         define_names;
         class_summaries;
         function_definitions;
         unannotated_globals;
       } as environment)
    =
    let loader = LazyLoader.{ environment; ast_environment } in
    (* Mask the raw DependencyTrackedTables with lazy read-only views of each one *)
    let module Modules = ReadOnlyTable.Make (Modules) in
    let module DefineNames = ReadOnlyTable.Make (DefineNames) in
    let module ClassSummaries = ReadOnlyTable.Make (ClassSummaries) in
    let module FunctionDefinitions = ReadOnlyTable.Make (FunctionDefinitions) in
    let module UnannotatedGlobals = ReadOnlyTable.Make (UnannotatedGlobals) in
    let modules = Modules.create ~loader modules in
    let define_names = DefineNames.create ~loader define_names in
    let class_summaries = ClassSummaries.create ~loader class_summaries in
    let function_definitions = FunctionDefinitions.create ~loader function_definitions in
    let unannotated_globals = UnannotatedGlobals.create ~loader unannotated_globals in
    (* Define the basic getters and existence checks *)
    let get_module = Modules.get modules in
    let get_module_metadata ?dependency qualifier =
      let qualifier =
        match Reference.as_list qualifier with
        | ["future"; "builtins"]
        | ["builtins"] ->
            Reference.empty
        | _ -> qualifier
      in
      match get_module ?dependency qualifier with
      | Some _ as result -> result
      | None -> (
          match AstEnvironment.ReadOnly.is_module_tracked ast_environment qualifier with
          | true -> Some (Module.create_implicit ())
          | false -> None)
    in
    let module_exists ?dependency qualifier =
      let qualifier =
        match Reference.as_list qualifier with
        | ["future"; "builtins"]
        | ["builtins"] ->
            Reference.empty
        | _ -> qualifier
      in
      match Modules.mem modules ?dependency qualifier with
      | true -> true
      | false -> AstEnvironment.ReadOnly.is_module_tracked ast_environment qualifier
    in
    let get_class_summary = ClassSummaries.get class_summaries in
    let class_exists = ClassSummaries.mem class_summaries in
    let get_function_definition = FunctionDefinitions.get function_definitions in
    let get_unannotated_global = UnannotatedGlobals.get unannotated_globals in
    let get_define_names ?dependency qualifier =
      DefineNames.get define_names ?dependency qualifier |> Option.value ~default:[]
    in
    (* Define the bulk key reads - these tell us what's been loaded thus far *)
    let all_classes () =
      AstEnvironment.ReadOnly.all_explicit_modules ast_environment
      |> KeyTracker.get_class_keys key_tracker
    in
    let all_indices () =
      all_classes ()
      |> Type.Primitive.Set.of_list
      |> IndexTracker.indices
      |> IndexTracker.Set.to_list
    in
    let all_unannotated_globals () =
      AstEnvironment.ReadOnly.all_explicit_modules ast_environment
      |> KeyTracker.get_unannotated_global_keys key_tracker
    in
    {
      ReadOnly.ast_environment;
      get_module_metadata;
      module_exists;
      get_class_summary;
      class_exists;
      get_function_definition;
      get_unannotated_global;
      get_define_names;
      all_classes;
      all_indices;
      all_unannotated_globals;
    }


  let update ({ ast_environment; key_tracker; define_names; _ } as environment) ~scheduler upstream =
    let invalidated_modules = AstEnvironment.UpdateResult.invalidated_modules upstream in
    let map sources =
      let register qualifier =
        AstEnvironment.ReadOnly.get_processed_source
          ~track_dependency:true
          ast_environment
          qualifier
        >>| set_module_data environment
        |> Option.value ~default:()
      in
      List.iter sources ~f:register
    in
    let update () =
      SharedMemoryKeys.DependencyKey.Registry.collected_iter
        scheduler
        ~policy:
          (Scheduler.Policy.fixed_chunk_count
             ~minimum_chunks_per_worker:1
             ~minimum_chunk_size:100
             ~preferred_chunks_per_worker:5
             ())
        ~f:map
        ~inputs:invalidated_modules
    in
    let KeyTracker.PreviousKeys.
          {
            previous_classes_list;
            previous_classes;
            previous_unannotated_globals_list;
            previous_unannotated_globals;
          }
      =
      KeyTracker.get_previous_keys_and_clear key_tracker invalidated_modules
    in
    let previous_defines_list = DefineNames.get_define_names define_names invalidated_modules in
    let triggered_dependencies =
      Profiling.track_duration_and_shared_memory_with_dynamic_tags
        "TableUpdate(Unannotated globals)"
        ~f:(fun _ ->
          let (), mutation_triggers =
            DependencyKey.Transaction.empty ~scheduler
            |> add_to_transaction
                 environment
                 ~previous_classes_list
                 ~previous_unannotated_globals_list
                 ~previous_defines_list
                 ~invalidated_modules
            |> DependencyKey.Transaction.execute ~update
          in
          let current_classes =
            KeyTracker.get_class_keys key_tracker invalidated_modules |> Type.Primitive.Set.of_list
          in
          let current_defines =
            DefineNames.get_define_names define_names invalidated_modules |> Reference.Set.of_list
          in
          let current_unannotated_globals =
            KeyTracker.get_unannotated_global_keys key_tracker invalidated_modules
            |> Reference.Set.of_list
          in
          let class_additions = Type.Primitive.Set.diff current_classes previous_classes in
          let define_additions =
            Reference.Set.of_list previous_defines_list |> Reference.Set.diff current_defines
          in
          let unannotated_global_additions =
            Reference.Set.diff current_unannotated_globals previous_unannotated_globals
          in
          let addition_triggers =
            get_all_dependents
              ~class_additions:(Set.to_list class_additions)
              ~unannotated_global_additions:(Set.to_list unannotated_global_additions)
              ~define_additions:(Set.to_list define_additions)
          in
          let triggered_dependencies =
            DependencyKey.RegisteredSet.union addition_triggers mutation_triggers
          in
          let tags () =
            let triggered_dependencies_size =
              SharedMemoryKeys.DependencyKey.RegisteredSet.cardinal triggered_dependencies
              |> Format.sprintf "%d"
            in
            [
              "phase_name", "Global discovery";
              "number_of_triggered_dependencies", triggered_dependencies_size;
            ]
          in
          { Profiling.result = triggered_dependencies; tags })
    in
    { UpdateResult.triggered_dependencies; upstream }
end

module Base = struct
  type t = {
    ast_environment: AstEnvironment.t;
    from_read_only_upstream: FromReadOnlyUpstream.t;
  }

  let create controls =
    let ast_environment = AstEnvironment.create controls in
    let from_read_only_upstream =
      AstEnvironment.read_only ast_environment |> FromReadOnlyUpstream.create
    in
    FromReadOnlyUpstream.cold_start from_read_only_upstream;
    { ast_environment; from_read_only_upstream }


  let create_for_testing controls module_path_code_pairs =
    let ast_environment = AstEnvironment.create_for_testing controls module_path_code_pairs in
    let from_read_only_upstream =
      AstEnvironment.read_only ast_environment |> FromReadOnlyUpstream.create
    in
    FromReadOnlyUpstream.cold_start from_read_only_upstream;
    { ast_environment; from_read_only_upstream }


  let update_this_and_all_preceding_environments
      { ast_environment; from_read_only_upstream }
      ~scheduler
      artifact_paths
    =
    let update_result = AstEnvironment.update ~scheduler ast_environment artifact_paths in
    FromReadOnlyUpstream.update from_read_only_upstream ~scheduler update_result


  let read_only { from_read_only_upstream; _ } =
    FromReadOnlyUpstream.read_only from_read_only_upstream


  let controls { from_read_only_upstream; _ } =
    FromReadOnlyUpstream.controls from_read_only_upstream


  let ast_environment { ast_environment; _ } = ast_environment

  (* All SharedMemory tables are populated and stored in separate, imperative steps that must be run
     before loading / after storing. These functions only handle serializing and deserializing the
     non-SharedMemory data *)
  let load controls =
    let ast_environment = AstEnvironment.load controls in
    let from_read_only_upstream =
      AstEnvironment.read_only ast_environment |> FromReadOnlyUpstream.create
    in
    { ast_environment; from_read_only_upstream }


  let store { ast_environment; _ } = AstEnvironment.store ast_environment
end

module Overlay = struct
  type t = {
    parent: ReadOnly.t;
    ast_environment: AstEnvironment.Overlay.t;
    from_read_only_upstream: FromReadOnlyUpstream.t;
  }

  let create parent =
    let ast_environment = ReadOnly.ast_environment parent |> AstEnvironment.Overlay.create in
    let from_read_only_upstream =
      AstEnvironment.Overlay.read_only ast_environment |> FromReadOnlyUpstream.create
    in
    { parent; ast_environment; from_read_only_upstream }


  let module_tracker { ast_environment; _ } = AstEnvironment.Overlay.module_tracker ast_environment

  let consume_upstream_update ({ from_read_only_upstream; _ } as environment) update_result =
    let filtered_update_result =
      let filtered_invalidated_modules =
        AstEnvironment.UpdateResult.invalidated_modules update_result
        |> List.filter ~f:(module_tracker environment |> ModuleTracker.Overlay.owns_qualifier)
      in
      {
        update_result with
        AstEnvironment.UpdateResult.invalidated_modules = filtered_invalidated_modules;
      }
    in
    FromReadOnlyUpstream.update
      from_read_only_upstream
      ~scheduler:(Scheduler.create_sequential ())
      filtered_update_result


  let update_overlaid_code ({ ast_environment; _ } as environment) ~code_updates =
    AstEnvironment.Overlay.update_overlaid_code ast_environment ~code_updates
    |> consume_upstream_update environment


  let propagate_parent_update
      environment
      { UpdateResult.triggered_dependencies = parent_triggered_dependencies; upstream }
    =
    let { UpdateResult.triggered_dependencies; _ } = consume_upstream_update environment upstream in
    {
      UpdateResult.triggered_dependencies =
        SharedMemoryKeys.DependencyKey.RegisteredSet.union
          parent_triggered_dependencies
          triggered_dependencies;
      upstream;
    }


  let read_only ({ parent; from_read_only_upstream; _ } as environment) =
    let this_read_only = FromReadOnlyUpstream.read_only from_read_only_upstream in
    let { ReadOnly.all_classes; all_indices; all_unannotated_globals; _ } = parent in
    let { ReadOnly.ast_environment; _ } = this_read_only in
    let if_owns ~owns ~f ?dependency key =
      if owns key then
        f this_read_only ?dependency key
      else
        f parent ?dependency key
    in
    let owns_qualifier, owns_reference, owns_qualified_class_name =
      let module_tracker = module_tracker environment in
      ( ModuleTracker.Overlay.owns_qualifier module_tracker,
        ModuleTracker.Overlay.owns_reference module_tracker,
        ModuleTracker.Overlay.owns_identifier module_tracker )
    in
    {
      ReadOnly.ast_environment;
      module_exists = if_owns ~owns:owns_qualifier ~f:ReadOnly.module_exists;
      get_module_metadata = if_owns ~owns:owns_qualifier ~f:ReadOnly.get_module_metadata;
      get_define_names = if_owns ~owns:owns_qualifier ~f:ReadOnly.get_define_names;
      class_exists = if_owns ~owns:owns_qualified_class_name ~f:ReadOnly.class_exists;
      get_class_summary = if_owns ~owns:owns_qualified_class_name ~f:ReadOnly.get_class_summary;
      get_function_definition = if_owns ~owns:owns_reference ~f:ReadOnly.get_function_definition;
      get_unannotated_global = if_owns ~owns:owns_reference ~f:ReadOnly.get_unannotated_global;
      all_classes;
      all_indices;
      all_unannotated_globals;
    }
end

include Base
