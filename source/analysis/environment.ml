(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre

module type ReadOnly = sig
  type t

  val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.ReadOnly.t

  val controls : t -> EnvironmentControls.t
end

module UpdateResult = struct
  module type S = sig
    type t

    val locally_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.RegisteredSet.t

    val all_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.RegisteredSet.t list

    val unannotated_global_environment_update_result
      :  t ->
      UnannotatedGlobalEnvironment.UpdateResult.t

    val invalidated_modules : t -> Ast.Reference.t list
  end
end

module PreviousEnvironment = struct
  module type S = sig
    module ReadOnly : ReadOnly

    module UpdateResult : UpdateResult.S

    type t

    val create : EnvironmentControls.t -> t

    val create_for_testing : EnvironmentControls.t -> (Ast.ModulePath.t * string) list -> t

    val ast_environment : t -> AstEnvironment.t

    val read_only : t -> ReadOnly.t

    val update_this_and_all_preceding_environments
      :  t ->
      scheduler:Scheduler.t ->
      ArtifactPath.t list ->
      UpdateResult.t

    val store : t -> unit

    val load : EnvironmentControls.t -> t

    module Overlay : sig
      type t

      val create : ReadOnly.t -> t

      val module_tracker : t -> ModuleTracker.Overlay.t

      val update_overlaid_code
        :  t ->
        code_updates:(ArtifactPath.t * ModuleTracker.Overlay.CodeUpdate.t) list ->
        UpdateResult.t

      val propagate_parent_update : t -> UpdateResult.t -> UpdateResult.t

      val read_only : t -> ReadOnly.t
    end
  end
end

module type S = sig
  include PreviousEnvironment.S

  module PreviousEnvironment : PreviousEnvironment.S

  module Testing : sig
    module ReadOnly : sig
      val upstream : ReadOnly.t -> PreviousEnvironment.ReadOnly.t
    end

    module UpdateResult : sig
      val upstream : UpdateResult.t -> PreviousEnvironment.UpdateResult.t
    end
  end
end

module EnvironmentTable = struct
  module type In = sig
    module PreviousEnvironment : PreviousEnvironment.S

    module Key : Memory.KeyType

    module Value : Memory.ValueTypeWithEquivalence

    type trigger [@@deriving sexp, compare]

    val convert_trigger : trigger -> Key.t

    val key_to_trigger : Key.t -> trigger

    module TriggerSet : Set.S with type Elt.t = trigger

    val lazy_incremental : bool

    val filter_upstream_dependency : SharedMemoryKeys.dependency -> trigger option

    val trigger_to_dependency : trigger -> SharedMemoryKeys.dependency

    val produce_value
      :  PreviousEnvironment.ReadOnly.t ->
      trigger ->
      dependency:SharedMemoryKeys.DependencyKey.registered option ->
      Value.t

    val show_key : Key.t -> string

    val overlay_owns_key : ModuleTracker.Overlay.t -> Key.t -> bool

    val equal_value : Value.t -> Value.t -> bool
  end

  module type Table = sig
    include Memory.FirstClass.NoCache.S

    val add_to_transaction
      :  t ->
      SharedMemoryKeys.DependencyKey.Transaction.t ->
      keys:KeySet.t ->
      SharedMemoryKeys.DependencyKey.Transaction.t

    val add_pessimistic_transaction
      :  t ->
      SharedMemoryKeys.DependencyKey.Transaction.t ->
      keys:KeySet.t ->
      SharedMemoryKeys.DependencyKey.Transaction.t

    val get : t -> ?dependency:SharedMemoryKeys.DependencyKey.registered -> key -> value option

    val mem : t -> ?dependency:SharedMemoryKeys.DependencyKey.registered -> key -> bool
  end

  module type S = sig
    module In : In

    module ReadOnly : sig
      type t

      val get : t -> ?dependency:SharedMemoryKeys.DependencyKey.registered -> In.Key.t -> In.Value.t

      val upstream_environment : t -> In.PreviousEnvironment.ReadOnly.t

      val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.ReadOnly.t

      val controls : t -> EnvironmentControls.t
    end

    module UpdateResult : UpdateResult.S

    module Overlay : sig
      type t

      val create : ReadOnly.t -> t

      val module_tracker : t -> ModuleTracker.Overlay.t

      val update_overlaid_code
        :  t ->
        code_updates:(ArtifactPath.t * ModuleTracker.Overlay.CodeUpdate.t) list ->
        UpdateResult.t

      val propagate_parent_update : t -> UpdateResult.t -> UpdateResult.t

      val read_only : t -> ReadOnly.t
    end

    type t

    val create : EnvironmentControls.t -> t

    val create_for_testing : EnvironmentControls.t -> (Ast.ModulePath.t * string) list -> t

    val ast_environment : t -> AstEnvironment.t

    val read_only : t -> ReadOnly.t

    val update_this_and_all_preceding_environments
      :  t ->
      scheduler:Scheduler.t ->
      ArtifactPath.t list ->
      UpdateResult.t

    val store : t -> unit

    val load : EnvironmentControls.t -> t

    module Unsafe : sig
      val upstream : t -> In.PreviousEnvironment.t
    end

    module Testing : sig
      module ReadOnly : sig
        val upstream : ReadOnly.t -> In.PreviousEnvironment.ReadOnly.t
      end

      module UpdateResult : sig
        val upstream : UpdateResult.t -> In.PreviousEnvironment.UpdateResult.t
      end
    end
  end

  module Make (In : In) (Table : Table with type value = In.Value.t and type key = In.Key.t) =
  struct
    let _ = Table.mem

    module In = In

    module ReadOnly = struct
      type t = {
        get: ?dependency:SharedMemoryKeys.DependencyKey.registered -> In.Key.t -> In.Value.t;
        upstream_environment: In.PreviousEnvironment.ReadOnly.t;
      }

      let get { get; _ } = get

      let upstream_environment { upstream_environment; _ } = upstream_environment

      let unannotated_global_environment { upstream_environment; _ } =
        In.PreviousEnvironment.ReadOnly.unannotated_global_environment upstream_environment


      let controls { upstream_environment; _ } =
        In.PreviousEnvironment.ReadOnly.controls upstream_environment
    end

    module UpdateResult = struct
      type t = {
        upstream: In.PreviousEnvironment.UpdateResult.t;
        triggered_dependencies: SharedMemoryKeys.DependencyKey.RegisteredSet.t;
      }

      let locally_triggered_dependencies { triggered_dependencies; _ } = triggered_dependencies

      let all_triggered_dependencies { triggered_dependencies; upstream; _ } =
        triggered_dependencies
        :: In.PreviousEnvironment.UpdateResult.all_triggered_dependencies upstream


      let unannotated_global_environment_update_result { upstream; _ } =
        In.PreviousEnvironment.UpdateResult.unannotated_global_environment_update_result upstream


      let invalidated_modules previous =
        unannotated_global_environment_update_result previous
        |> UnannotatedGlobalEnvironment.UpdateResult.invalidated_modules


      let upstream { upstream; _ } = upstream
    end

    module FromReadOnlyUpstream = struct
      type t = {
        table: Table.t;
        upstream_environment: In.PreviousEnvironment.ReadOnly.t;
      }

      let create upstream_environment = { table = Table.create (); upstream_environment }

      let get { table; upstream_environment } ?dependency key =
        match Table.get table ?dependency key with
        | Some hit -> hit
        | None ->
            let trigger = In.key_to_trigger key in
            let dependency = In.trigger_to_dependency trigger in
            let dependency = Some (SharedMemoryKeys.DependencyKey.Registry.register dependency) in
            let value = In.produce_value upstream_environment trigger ~dependency in
            Table.add table key value;
            value


      let read_only ({ upstream_environment; _ } as this_environment) =
        { ReadOnly.get = get this_environment; upstream_environment }


      module TriggerMap = Map.Make (struct
        type t = In.trigger [@@deriving sexp, compare]
      end)

      let compute_trigger_map upstream_triggered_dependencies =
        List.fold
          upstream_triggered_dependencies
          ~init:TriggerMap.empty
          ~f:(fun triggers upstream_dependencies ->
            SharedMemoryKeys.DependencyKey.RegisteredSet.fold
              (fun dependency triggers ->
                match
                  In.filter_upstream_dependency (SharedMemoryKeys.DependencyKey.get_key dependency)
                with
                | Some trigger -> (
                    match TriggerMap.add triggers ~key:trigger ~data:dependency with
                    | `Duplicate -> triggers
                    | `Ok updated -> updated)
                | None -> triggers)
              upstream_dependencies
              triggers)


      let update_only_this_environment ~scheduler { table; upstream_environment } trigger_map =
        Log.log ~section:`Environment "Updating %s Environment" In.Value.description;
        let update ~names_to_update () =
          let register () =
            let set (name, dependency) =
              In.produce_value upstream_environment name ~dependency:(Some dependency)
              |> Table.add table (In.convert_trigger name)
            in
            List.iter ~f:set
          in
          let () =
            SharedMemoryKeys.DependencyKey.Registry.collected_map_reduce
              scheduler
              ~policy:
                (Scheduler.Policy.fixed_chunk_count
                   ~minimum_chunks_per_worker:1
                   ~minimum_chunk_size:100
                   ~preferred_chunks_per_worker:5
                   ())
              ~map:register
              ~reduce:(fun () () -> ())
              ~inputs:names_to_update
              ~initial:()
              ()
          in
          ()
        in
        let triggered_dependencies =
          let name = Format.sprintf "TableUpdate(%s)" In.Value.description in
          Profiling.track_duration_and_shared_memory_with_dynamic_tags name ~f:(fun _ ->
              let names_to_update = Map.to_alist trigger_map in
              let (), triggered_dependencies =
                let keys =
                  List.map names_to_update ~f:fst
                  |> List.map ~f:In.convert_trigger
                  |> Table.KeySet.of_list
                in
                let transaction = SharedMemoryKeys.DependencyKey.Transaction.empty ~scheduler in
                if In.lazy_incremental then
                  Table.add_pessimistic_transaction table ~keys transaction
                  |> SharedMemoryKeys.DependencyKey.Transaction.execute ~update:(fun () -> ())
                else
                  Table.add_to_transaction table ~keys transaction
                  |> SharedMemoryKeys.DependencyKey.Transaction.execute
                       ~update:(update ~names_to_update)
              in
              let tags () =
                let triggered_dependencies_size =
                  SharedMemoryKeys.DependencyKey.RegisteredSet.cardinal triggered_dependencies
                  |> Format.sprintf "%d"
                in
                [
                  "phase_name", In.Value.description;
                  "number_of_triggered_dependencies", triggered_dependencies_size;
                ]
              in
              { Profiling.result = triggered_dependencies; tags })
        in
        triggered_dependencies
    end

    module Base = struct
      type t = {
        upstream_environment: In.PreviousEnvironment.t;
        from_read_only_upstream: FromReadOnlyUpstream.t;
      }

      let from_upstream_environment upstream_environment =
        {
          upstream_environment;
          from_read_only_upstream =
            In.PreviousEnvironment.read_only upstream_environment |> FromReadOnlyUpstream.create;
        }


      let create controls = In.PreviousEnvironment.create controls |> from_upstream_environment

      let create_for_testing controls module_path_code_pairs =
        In.PreviousEnvironment.create_for_testing controls module_path_code_pairs
        |> from_upstream_environment


      let ast_environment { upstream_environment; _ } =
        In.PreviousEnvironment.ast_environment upstream_environment


      let read_only { from_read_only_upstream; _ } =
        FromReadOnlyUpstream.read_only from_read_only_upstream


      let update_this_and_all_preceding_environments
          { upstream_environment; from_read_only_upstream }
          ~scheduler
          artifact_paths
        =
        let upstream_update =
          In.PreviousEnvironment.update_this_and_all_preceding_environments
            upstream_environment
            ~scheduler
            artifact_paths
        in
        let triggered_dependencies =
          In.PreviousEnvironment.UpdateResult.all_triggered_dependencies upstream_update
          |> FromReadOnlyUpstream.compute_trigger_map
          |> FromReadOnlyUpstream.update_only_this_environment from_read_only_upstream ~scheduler
        in
        { UpdateResult.triggered_dependencies; upstream = upstream_update }


      (* All SharedMemory tables are populated and stored in separate, imperative steps that must be
         run after storing / before loading These functions only handle serializing and
         deserializing the non-SharedMemory data *)

      let store { upstream_environment; _ } = In.PreviousEnvironment.store upstream_environment

      let load controls = In.PreviousEnvironment.load controls |> from_upstream_environment
    end

    module Overlay = struct
      type t = {
        parent: ReadOnly.t;
        upstream_environment: In.PreviousEnvironment.Overlay.t;
        from_read_only_upstream: FromReadOnlyUpstream.t;
      }

      let create parent =
        let upstream_environment =
          ReadOnly.upstream_environment parent |> In.PreviousEnvironment.Overlay.create
        in
        let from_read_only_upstream =
          In.PreviousEnvironment.Overlay.read_only upstream_environment
          |> FromReadOnlyUpstream.create
        in
        { parent; upstream_environment; from_read_only_upstream }


      let module_tracker { upstream_environment; _ } =
        In.PreviousEnvironment.Overlay.module_tracker upstream_environment


      let overlay_owns_key environment = module_tracker environment |> In.overlay_owns_key

      let owns_trigger environment trigger =
        In.convert_trigger trigger |> overlay_owns_key environment


      let compute_owned_trigger_map environment upstream_triggered_dependencies =
        List.fold
          upstream_triggered_dependencies
          ~init:FromReadOnlyUpstream.TriggerMap.empty
          ~f:(fun triggers upstream_dependencies ->
            SharedMemoryKeys.DependencyKey.RegisteredSet.fold
              (fun dependency triggers ->
                match
                  In.filter_upstream_dependency (SharedMemoryKeys.DependencyKey.get_key dependency)
                  |> Option.filter ~f:(owns_trigger environment)
                with
                | None -> triggers
                | Some trigger -> (
                    match
                      FromReadOnlyUpstream.TriggerMap.add triggers ~key:trigger ~data:dependency
                    with
                    | `Ok updated -> updated
                    | `Duplicate -> triggers))
              upstream_dependencies
              triggers)


      let consume_upstream_update ({ from_read_only_upstream; _ } as environment) update_result =
        let triggered_dependencies =
          In.PreviousEnvironment.UpdateResult.all_triggered_dependencies update_result
          |> compute_owned_trigger_map environment
          |> FromReadOnlyUpstream.update_only_this_environment
               from_read_only_upstream
               ~scheduler:(Scheduler.create_sequential ())
        in
        { UpdateResult.triggered_dependencies; upstream = update_result }


      let update_overlaid_code ({ upstream_environment; _ } as environment) ~code_updates =
        In.PreviousEnvironment.Overlay.update_overlaid_code upstream_environment ~code_updates
        |> consume_upstream_update environment


      let propagate_parent_update ({ upstream_environment; _ } as environment) parent_update_result =
        let upstream =
          UpdateResult.upstream parent_update_result
          |> In.PreviousEnvironment.Overlay.propagate_parent_update upstream_environment
        in
        let direct_update_result = consume_upstream_update environment upstream in
        {
          UpdateResult.triggered_dependencies =
            SharedMemoryKeys.DependencyKey.RegisteredSet.union
              (UpdateResult.locally_triggered_dependencies parent_update_result)
              (UpdateResult.locally_triggered_dependencies direct_update_result);
          upstream;
        }


      let read_only ({ parent; upstream_environment; from_read_only_upstream } as environment) =
        let this_read_only = FromReadOnlyUpstream.read_only from_read_only_upstream in
        let get ?dependency key =
          if overlay_owns_key environment key then
            ReadOnly.get this_read_only ?dependency key
          else
            ReadOnly.get parent ?dependency key
        in
        {
          ReadOnly.get;
          upstream_environment = In.PreviousEnvironment.Overlay.read_only upstream_environment;
        }
    end

    include Base

    module Unsafe = struct
      let upstream { Base.upstream_environment; _ } = upstream_environment
    end

    module Testing = struct
      module ReadOnly = struct
        let upstream { ReadOnly.upstream_environment; _ } = upstream_environment
      end

      module UpdateResult = struct
        let upstream { UpdateResult.upstream; _ } = upstream
      end
    end
  end

  module WithCache (In : In) =
    Make
      (In)
      (DependencyTrackedMemory.DependencyTrackedTableWithCache
         (In.Key)
         (SharedMemoryKeys.DependencyKey)
         (In.Value))
  module NoCache (In : In) =
    Make
      (In)
      (DependencyTrackedMemory.DependencyTrackedTableNoCache
         (In.Key)
         (SharedMemoryKeys.DependencyKey)
         (In.Value))
end
