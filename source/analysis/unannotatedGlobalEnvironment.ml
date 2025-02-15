(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* UnannotatedGlobalEnvironment(UGE): layer of the environment stack
 * - upstream: AstEnvironment
 * - downstream: TypeAliasEnvironment
 *
 * It is responsible for taking preprocessed Ast data, breaking it
 * into chunks, and storing it for use in later analysis stages.
 * Almost all downstream logic depends only on data in the UGE, rather
 * than on raw sources.
 *
 * It is also responsible for "resolving exports", which means dealing
 * with name aliases introduced by imports, for example that here:
 * ```
 * # my_module.py
 * from foo import bar as baz
 *
 * # other_module.py
 * from my_module import baz
 * ```
 * the name `baz` in `my_module` actually refers to `foo.bar`.
 *
 * This name alias handling of unannotated globals is not the same as
 * TypeAlias handling, which is what TypeAliasEnvironment does.
 *
 * This is one of the most complicated layers of the environment stack
 * from a data flow perspective because instead of being a single cache
 * table it consists of several separate tables:
 * - one for FunctionDefinitionTable, which are the preprocessed Asts of
 *   function bodies (including module and class toplevels)
 * - one for ClassSummaryTable, which are data structures that concisely
 *   represent some simple facts about a class that come from skimming
 *   the class body and constructor.
 * - one for UnannotatedGlobalTable, which represent the various global
 *   names (including global variables, functions, classes, and imports)
 *   exposed by a model.
 * - a set of KeyTrackers, which are needed because there's no API to
 *   list all shared memory keys so we have to store a one-to-many
 *   record of all the functions / classes / globals per module that
 *   we rely on to, e.g., run type check over all functions.
 *
 * The tables all have to be managed together because all of them
 * are populated from a single computation of a preprocessed Ast,
 * which is an expensive operation we want to do just once per module.
 *)

open Core
open Pyre
open Ast
open Statement

module IncomingDataComputation = struct
  module Queries = struct
    type t = {
      is_qualifier_tracked: Ast.Reference.t -> bool;
      source_of_qualifier: Ast.Reference.t -> Ast.Source.t option;
    }
  end

  let module_components Queries.{ is_qualifier_tracked; source_of_qualifier } qualifier =
    match source_of_qualifier qualifier with
    | Some source -> Some (Module.Components.of_source source)
    | None ->
        if is_qualifier_tracked qualifier then
          Some (Module.Components.implicit_module ())
        else
          None
end

module OutgoingDataComputation = struct
  module Queries = struct
    type t = { get_module_components: Reference.t -> Module.Components.t option }
  end

  let get_module_components { Queries.get_module_components; _ } = get_module_components

  let module_exists queries qualifier = get_module_components queries qualifier |> Option.is_some

  let get_module_metadata queries qualifier =
    get_module_components queries qualifier
    >>| fun { Module.Components.module_metadata; _ } -> module_metadata


  let search_possible_containing_modules ~f reference =
    (* String.equal (Reference.show reference) "str" in *)
    let ancestors_descending = Reference.possible_qualifiers_after_delocalize reference in
    List.find_map ~f ancestors_descending


  let get_class_summary queries class_name =
    let load_class_summary_if_in_module qualifier =
      get_module_components queries qualifier
      >>= fun { Module.Components.class_summaries; _ } ->
      Identifier.Map.Tree.find class_summaries class_name
    in
    search_possible_containing_modules
      ~f:load_class_summary_if_in_module
      (Reference.create class_name)


  let class_exists queries class_name = get_class_summary queries class_name |> Option.is_some

  let get_unannotated_global queries reference =
    Reference.prefix reference
    >>= get_module_components queries
    >>= fun { Module.Components.unannotated_globals; _ } ->
    let name = Reference.last reference in
    Identifier.Map.Tree.find unannotated_globals name


  let primitive_name annotation =
    let primitive, _ = Type.split annotation in
    Type.primitive_name primitive


  let is_protocol queries annotation =
    primitive_name annotation
    >>= get_class_summary queries
    >>| Node.value
    >>| ClassSummary.is_protocol
    |> Option.value ~default:false


  let is_special_form queries annotation =
    primitive_name annotation
    >>= get_class_summary queries
    >>| Node.value
    >>| ClassSummary.is_special_form
    |> Option.value ~default:false


  let resolve_exports queries ?(from = Reference.empty) reference =
    let module ResolveExportItem = struct
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
    in
    let visited_set = ResolveExportItem.Hash_set.create () in
    let rec resolve_module_alias ~current_module ~names_to_resolve () =
      match get_module_metadata queries current_module with
      | None -> (
          (* No module was found *)
          match names_to_resolve with
          | head :: tail ->
              (* If we were only using a prefix of the qualifier as the module name, try looking
                 deeper into the package hierarchy (e.g. if we were trying to find `foo.bar.baz` in
                 `foo`, now try `baz` in `foo.bar`). *)
              resolve_module_alias
                ~current_module:(Reference.create ~prefix:current_module head)
                ~names_to_resolve:tail
                ()
          | [] -> None)
      | Some module_metadata -> (
          match names_to_resolve with
          | [] ->
              (* If the module we just loaded is the entire reference we were looking for, then the
                 reference is a qualifier for this module *)
              Some (ResolvedReference.Module current_module)
          | next_name :: rest_names -> (
              (* Otherwise, we will look up globals of this module. First, we need to check that we
                 aren't stuck in a cycle *)
              let item = { ResolveExportItem.current_module; name = next_name } in
              match Hash_set.strict_add visited_set item with
              | Result.Error _ ->
                  (* We hit a cycle, give up. *)
                  None
              | Result.Ok _ -> (
                  (* There is no cycle, so look up the name in this module's globals. *)
                  match Module.Metadata.get_export module_metadata next_name with
                  | None -> (
                      (* We didn't find any explicit global symbol for this name. *)
                      match Module.Metadata.get_export module_metadata "__getattr__" with
                      | Some Module.Export.(Name (Define { is_getattr_any = true })) ->
                          (* If __getattr__ is defined, then all lookups resolve through it; this is
                             occasionally used for real code and is also a common pattern in
                             partially-typed stubs. *)
                          Some
                            (ResolvedReference.ModuleAttribute
                               {
                                 from = current_module;
                                 name = next_name;
                                 export = ResolvedReference.FromModuleGetattr;
                                 remaining = rest_names;
                               })
                      | _ ->
                          (* Otherwise, we have to give up resolving the name in this module. But we
                             can look for a more-deeply-nested module containing the name (e.g. if
                             the lookup of `foo.bar.baz` in `foo` failed, move on to looking in
                             `foo.bar` *)
                          resolve_module_alias
                            ~current_module:
                              (Reference.create next_name |> Reference.combine current_module)
                            ~names_to_resolve:rest_names
                            ())
                  | Some (Module.Export.NameAlias { from; name }) ->
                      if not (Reference.equal current_module from) then
                        (* We are resolving an import-from statement like `from x import y` (or
                           `from x import y as z; `name` and `next_name` can be different). In this
                           case we don't know what the name means - it might be an attribute `y`
                           defined in `x` or it might be a package `x.y`. But we can look for an an
                           attribute `y` in `x` *first* because if we fail to find it we'll search
                           the module `x.y` in the next recursive call. *)
                        resolve_module_alias
                          ~current_module:from
                          ~names_to_resolve:(name :: rest_names)
                          ()
                      else
                        (* Edge case: if we see `from . import y` or `from . import y as z`. In this
                           case we can skip straight to searching for a module `<current_module>.y`
                           (and we have to or we'll get stuck in an infinite loop). *)
                        resolve_module_alias
                          ~current_module:(Reference.create name |> Reference.combine from)
                          ~names_to_resolve:rest_names
                          ()
                  | Some (Module.Export.Module name) ->
                      (* The name is defined by a statement like `import name` or `import name as
                         next_name`; in that case it definitely refers to a module so we can jump
                         straight there in our next step. *)
                      resolve_module_alias ~current_module:name ~names_to_resolve:rest_names ()
                  | Some (Module.Export.Name export) ->
                      (* The name refers to a global (a global variable, function, class, etc)
                         actually defined in this module. We are finished. *)
                      Some
                        (ResolvedReference.ModuleAttribute
                           {
                             from = current_module;
                             name = next_name;
                             export = ResolvedReference.Exported export;
                             remaining = rest_names;
                           }))))
    in
    resolve_module_alias ~current_module:from ~names_to_resolve:(Reference.as_list reference) ()


  let first_matching_class_decorator
      queries
      ~names
      { Node.value = { ClassSummary.decorators; _ }; _ }
    =
    let resolve_and_check_for_match decorator =
      match Decorator.from_expression decorator with
      | None -> None
      | Some ({ Ast.Statement.Decorator.name = { Node.value = name; location }; _ } as decorator) ->
          let resolved_name =
            match resolve_exports queries name with
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


  let exists_matching_class_decorator queries ~names class_summary =
    first_matching_class_decorator queries ~names class_summary |> Option.is_some
end

module ModuleComponentsTable = Environment.EnvironmentTable.WithCache (struct
  module PreviousEnvironment = SourceCodeEnvironment
  module Key = SharedMemoryKeys.ReferenceKey

  module Value = struct
    type t = Module.Components.t option [@@deriving equal]

    let prefix = Hack_parallel.Std.Prefix.make ()

    let description = "Module.Components"
  end

  type trigger = Reference.t [@@deriving sexp, compare]

  let convert_trigger = Fn.id

  let key_to_trigger = Fn.id

  module TriggerSet = Reference.Set

  let lazy_incremental = false

  let show_key = Reference.show

  let overlay_owns_key source_code_overlay =
    SourceCodeIncrementalApi.Overlay.owns_qualifier source_code_overlay


  let equal_value = Option.equal Module.Components.equal

  (* NOTE: the `trigger_to_dependency` and `produce_value` logic are actually a misleading hack
     here. We allowed SourceCodeApi to skip dependency tracking (so that some implementations like
     the buck-calls-pyre one can just ban it), so as a result we rely on AstEnvironment modeling all
     our dependencies for us. *)
  let produce_value source_code_environment key ~dependency =
    let queries : IncomingDataComputation.Queries.t =
      let source_code_api =
        SourceCodeEnvironment.ReadOnly.source_code_read_only source_code_environment
        |>
        match dependency with
        | None -> SourceCodeIncrementalApi.ReadOnly.get_untracked_api
        | Some dependency -> SourceCodeIncrementalApi.ReadOnly.get_tracked_api ~dependency
      in
      let is_qualifier_tracked = SourceCodeApi.is_qualifier_tracked source_code_api in
      let source_of_qualifier = SourceCodeApi.source_of_qualifier source_code_api in
      { IncomingDataComputation.Queries.is_qualifier_tracked; source_of_qualifier }
    in
    IncomingDataComputation.module_components queries key


  let trigger_to_dependency qualifier = SharedMemoryKeys.ComputeModuleComponents qualifier

  let filter_upstream_dependency = function
    | SharedMemoryKeys.ComputeModuleComponents qualifier -> Some qualifier
    | _ -> None
end)

include ModuleComponentsTable

module ReadOnly = struct
  include ModuleComponentsTable.ReadOnly

  let get_module_components ?dependency read_only qualifier =
    let cannonical_qualifier =
      match Reference.as_list qualifier with
      | ["future"; "builtins"]
      | ["builtins"] ->
          Reference.empty
      | _not_builtins -> qualifier
    in
    get ?dependency read_only cannonical_qualifier


  let get_queries ?dependency read_only =
    {
      OutgoingDataComputation.Queries.get_module_components =
        get_module_components ?dependency read_only;
    }


  let source_code_read_only read_only =
    ModuleComponentsTable.ReadOnly.upstream_environment read_only
    |> SourceCodeEnvironment.ReadOnly.source_code_read_only


  let get_untracked_source_code_api environment =
    source_code_read_only environment |> SourceCodeIncrementalApi.ReadOnly.get_untracked_api


  let get_tracked_source_code_api environment =
    source_code_read_only environment |> SourceCodeIncrementalApi.ReadOnly.get_tracked_api


  let controls environment =
    source_code_read_only environment |> SourceCodeIncrementalApi.ReadOnly.controls


  let get_module_metadata read_only ?dependency =
    get_queries ?dependency read_only |> OutgoingDataComputation.get_module_metadata


  let module_exists read_only ?dependency =
    get_queries ?dependency read_only |> OutgoingDataComputation.module_exists


  let get_class_summary read_only ?dependency =
    get_queries ?dependency read_only |> OutgoingDataComputation.get_class_summary


  let class_exists read_only ?dependency =
    get_queries ?dependency read_only |> OutgoingDataComputation.class_exists


  let get_unannotated_global read_only ?dependency =
    get_queries ?dependency read_only |> OutgoingDataComputation.get_unannotated_global


  let is_protocol read_only ?dependency =
    get_queries ?dependency read_only |> OutgoingDataComputation.is_protocol


  let is_special_form read_only ?dependency =
    get_queries ?dependency read_only |> OutgoingDataComputation.is_special_form


  let resolve_exports read_only ?dependency =
    get_queries ?dependency read_only |> OutgoingDataComputation.resolve_exports


  let first_matching_class_decorator read_only ?dependency =
    get_queries ?dependency read_only |> OutgoingDataComputation.first_matching_class_decorator


  let exists_matching_class_decorator read_only ?dependency =
    get_queries ?dependency read_only |> OutgoingDataComputation.exists_matching_class_decorator


  (* These functions are not dependency tracked and should only be used:
   * - in bulk queries (e.g. to help with Pysa analysis)
   * - for debugging and testing
   *
   * They cannot be used in some contexts, e.g. a lazy environment for
   * powering IDEs.
   *)
  module GlobalApis = struct
    let all_classes read_only ~scheduler ~global_module_paths_api =
      let class_names_for_qualifier qualifier =
        get_module_components read_only qualifier
        >>| (fun { Module.Components.class_summaries; _ } ->
              Identifier.Map.Tree.keys class_summaries)
        |> Option.value ~default:[]
      in
      let qualifiers = GlobalModulePathsApi.explicit_qualifiers global_module_paths_api in
      let scheduler_policy =
        Scheduler.Policy.fixed_chunk_count
          ~minimum_chunks_per_worker:1
          ~minimum_chunk_size:1
          ~preferred_chunks_per_worker:1
          ()
      in
      let map = List.concat_map ~f:class_names_for_qualifier in
      Scheduler.map_reduce
        scheduler
        ~policy:scheduler_policy
        ~initial:[]
        ~map
        ~reduce:List.append
        ~inputs:qualifiers
        ()


    let all_unannotated_globals read_only ~scheduler ~global_module_paths_api =
      let unannotated_global_names_for_qualifier qualifier =
        get_module_components read_only qualifier
        >>| (fun { Module.Components.unannotated_globals; _ } ->
              Identifier.Map.Tree.keys unannotated_globals)
        |> Option.value ~default:[]
      in
      let qualified_unannotated_global_names qualifier =
        let local_name_to_fully_qualified_reference name =
          Reference.create name |> Reference.combine qualifier
        in
        unannotated_global_names_for_qualifier qualifier
        |> List.map ~f:local_name_to_fully_qualified_reference
      in
      let qualifiers = GlobalModulePathsApi.explicit_qualifiers global_module_paths_api in
      let scheduler_policy =
        Scheduler.Policy.fixed_chunk_count
          ~minimum_chunks_per_worker:1
          ~minimum_chunk_size:1
          ~preferred_chunks_per_worker:1
          ()
      in
      let map = List.concat_map ~f:qualified_unannotated_global_names in
      Scheduler.map_reduce
        scheduler
        ~policy:scheduler_policy
        ~initial:[]
        ~map
        ~reduce:List.append
        ~inputs:qualifiers
        ()
  end
end

let controls read_write =
  source_code_base read_write
  |> SourceCodeIncrementalApi.Base.read_only
  |> SourceCodeIncrementalApi.ReadOnly.controls


module AssumeGlobalModuleListing = struct
  let global_module_paths_api read_write =
    source_code_base read_write
    |> SourceCodeIncrementalApi.Base.AssumeGlobalModuleListing.global_module_paths_api
end

module AssumeAstEnvironment = struct
  include ModuleComponentsTable.AssumeAstEnvironment

  let ast_environment read_write =
    ModuleComponentsTable.AssumeDownstreamNeverNeedsUpdates.upstream read_write
    |> SourceCodeEnvironment.AssumeAstEnvironment.ast_environment
end
