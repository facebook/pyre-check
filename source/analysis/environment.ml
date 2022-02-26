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
end

module type UpdateResultType = sig
  type t

  type read_only

  val locally_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.RegisteredSet.t

  val all_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.RegisteredSet.t list

  val read_only : t -> read_only

  val unannotated_global_environment_update_result
    :  t ->
    UnannotatedGlobalEnvironment.UpdateResult.t

  val ast_environment_update_result : t -> AstEnvironment.UpdateResult.t
end

module type PreviousEnvironment = sig
  module ReadOnly : ReadOnly

  module UpdateResult : UpdateResultType with type read_only := ReadOnly.t

  type t

  val create : AstEnvironment.t -> t

  val ast_environment : t -> AstEnvironment.t

  val read_only : t -> ReadOnly.t

  val update_this_and_all_preceding_environments
    :  t ->
    scheduler:Scheduler.t ->
    configuration:Configuration.Analysis.t ->
    AstEnvironment.trigger ->
    UpdateResult.t
end

module UpdateResult = struct
  module type S = UpdateResultType

  module Make (PreviousEnvironment : PreviousEnvironment) (ReadOnly : ReadOnly) = struct
    type read_only = ReadOnly.t

    type t = {
      upstream: PreviousEnvironment.UpdateResult.t;
      triggered_dependencies: SharedMemoryKeys.DependencyKey.RegisteredSet.t;
      read_only: ReadOnly.t;
    }

    let locally_triggered_dependencies { triggered_dependencies; _ } = triggered_dependencies

    let all_triggered_dependencies { triggered_dependencies; upstream; _ } =
      triggered_dependencies :: PreviousEnvironment.UpdateResult.all_triggered_dependencies upstream


    let read_only { read_only; _ } = read_only

    let unannotated_global_environment_update_result { upstream; _ } =
      PreviousEnvironment.UpdateResult.unannotated_global_environment_update_result upstream


    let ast_environment_update_result previous =
      unannotated_global_environment_update_result previous
      |> UnannotatedGlobalEnvironment.UpdateResult.upstream
  end
end

module type S = sig
  module ReadOnly : ReadOnly

  module PreviousEnvironment : PreviousEnvironment

  module UpdateResult : UpdateResult.S with type read_only = ReadOnly.t

  type t

  val create : AstEnvironment.t -> t

  val ast_environment : t -> AstEnvironment.t

  val read_only : t -> ReadOnly.t

  val update_this_and_all_preceding_environments
    :  t ->
    scheduler:Scheduler.t ->
    configuration:Configuration.Analysis.t ->
    AstEnvironment.trigger ->
    UpdateResult.t
end

module EnvironmentTable = struct
  module type In = sig
    module PreviousEnvironment : PreviousEnvironment

    module Key : Memory.KeyType

    module Value : Memory.ComparableValueType

    type trigger [@@deriving sexp, compare]

    val convert_trigger : trigger -> Key.t

    val key_to_trigger : Key.t -> trigger

    module TriggerSet : Set.S with type Elt.t = trigger

    val lazy_incremental : bool

    val filter_upstream_dependency : SharedMemoryKeys.dependency -> trigger option

    val trigger_to_dependency : trigger -> SharedMemoryKeys.dependency

    val legacy_invalidated_keys : UnannotatedGlobalEnvironment.UpdateResult.t -> TriggerSet.t

    val produce_value
      :  PreviousEnvironment.ReadOnly.t ->
      trigger ->
      dependency:SharedMemoryKeys.DependencyKey.registered option ->
      Value.t

    val all_keys : UnannotatedGlobalEnvironment.ReadOnly.t -> Key.t list

    val serialize_value : Value.t -> string

    val show_key : Key.out -> string

    val equal_value : Value.t -> Value.t -> bool
  end

  module type Table = sig
    include Memory.NoCache.S

    val add_to_transaction
      :  SharedMemoryKeys.DependencyKey.Transaction.t ->
      keys:KeySet.t ->
      SharedMemoryKeys.DependencyKey.Transaction.t

    val add_pessimistic_transaction
      :  SharedMemoryKeys.DependencyKey.Transaction.t ->
      keys:KeySet.t ->
      SharedMemoryKeys.DependencyKey.Transaction.t

    val get : ?dependency:SharedMemoryKeys.DependencyKey.registered -> key -> t option

    val mem : ?dependency:SharedMemoryKeys.DependencyKey.registered -> key -> bool
  end

  module type S = sig
    module In : In

    module ReadOnly : sig
      type t

      val get : t -> ?dependency:SharedMemoryKeys.DependencyKey.registered -> In.Key.t -> In.Value.t

      val upstream_environment : t -> In.PreviousEnvironment.ReadOnly.t

      val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.ReadOnly.t
    end

    module UpdateResult : UpdateResult.S with type read_only = ReadOnly.t

    type t

    val create : AstEnvironment.t -> t

    val ast_environment : t -> AstEnvironment.t

    val read_only : t -> ReadOnly.t

    val update_this_and_all_preceding_environments
      :  t ->
      scheduler:Scheduler.t ->
      configuration:Configuration.Analysis.t ->
      AstEnvironment.trigger ->
      UpdateResult.t
  end

  module Make
      (In : In)
      (Table : Table with type t = In.Value.t and type key = In.Key.t and type key_out = In.Key.out) =
  struct
    let _ = Table.mem

    module In = In

    module ReadOnly = struct
      type t = { upstream_environment: In.PreviousEnvironment.ReadOnly.t }

      let upstream_environment { upstream_environment } = upstream_environment

      let get { upstream_environment } ?dependency key =
        match Table.get ?dependency key with
        | Some hit -> hit
        | None ->
            let trigger = In.key_to_trigger key in
            let dependency = In.trigger_to_dependency trigger in
            let dependency = Some (SharedMemoryKeys.DependencyKey.Registry.register dependency) in
            let value = In.produce_value upstream_environment trigger ~dependency in
            Table.add key value;
            value


      let unannotated_global_environment { upstream_environment } =
        In.PreviousEnvironment.ReadOnly.unannotated_global_environment upstream_environment
    end

    module UpdateResult = UpdateResult.Make (In.PreviousEnvironment) (ReadOnly)

    let read_only previous_update_result =
      {
        ReadOnly.upstream_environment =
          In.PreviousEnvironment.UpdateResult.read_only previous_update_result;
      }


    module TriggerMap = Map.Make (struct
      type t = In.trigger [@@deriving sexp, compare]
    end)

    let update_only_this_environment ~scheduler ~configuration upstream_update =
      Log.log ~section:`Environment "Updating %s Environment" In.Value.description;
      let update ~names_to_update () =
        let register () =
          let set (name, dependency) =
            In.produce_value
              (In.PreviousEnvironment.UpdateResult.read_only upstream_update)
              name
              ~dependency:(Some dependency)
            |> Table.add (In.convert_trigger name)
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
      match configuration with
      | { Configuration.Analysis.incremental_style = FineGrained; _ } ->
          let triggered_dependencies =
            let name = Format.sprintf "TableUpdate(%s)" In.Value.description in
            Profiling.track_duration_and_shared_memory_with_dynamic_tags name ~f:(fun _ ->
                let names_to_update =
                  In.PreviousEnvironment.UpdateResult.all_triggered_dependencies upstream_update
                  |> List.fold ~init:TriggerMap.empty ~f:(fun triggers upstream_dependencies ->
                         SharedMemoryKeys.DependencyKey.RegisteredSet.fold
                           (fun dependency triggers ->
                             match
                               In.filter_upstream_dependency
                                 (SharedMemoryKeys.DependencyKey.get_key dependency)
                             with
                             | Some trigger -> (
                                 match TriggerMap.add triggers ~key:trigger ~data:dependency with
                                 | `Duplicate -> triggers
                                 | `Ok updated -> updated)
                             | None -> triggers)
                           upstream_dependencies
                           triggers)
                  |> Map.to_alist
                in
                let (), triggered_dependencies =
                  let keys =
                    List.map names_to_update ~f:fst
                    |> List.map ~f:In.convert_trigger
                    |> Table.KeySet.of_list
                  in
                  let transaction =
                    SharedMemoryKeys.DependencyKey.Transaction.empty ~scheduler ~configuration
                  in
                  if In.lazy_incremental then
                    Table.add_pessimistic_transaction ~keys transaction
                    |> SharedMemoryKeys.DependencyKey.Transaction.execute ~update:(fun () -> ())
                  else
                    Table.add_to_transaction ~keys transaction
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
          {
            UpdateResult.triggered_dependencies;
            upstream = upstream_update;
            read_only = read_only upstream_update;
          }
      | _ ->
          let _ =
            let name = Format.sprintf "LegacyTableUpdate(%s)" In.Value.description in
            Profiling.track_duration_and_shared_memory
              name
              ~tags:["phase_name", In.Value.description]
              ~f:(fun _ ->
                In.PreviousEnvironment.UpdateResult.unannotated_global_environment_update_result
                  upstream_update
                |> In.legacy_invalidated_keys
                |> Set.to_list
                |> List.map ~f:In.convert_trigger
                |> Table.KeySet.of_list
                |> Table.remove_batch)
          in
          {
            UpdateResult.triggered_dependencies = SharedMemoryKeys.DependencyKey.RegisteredSet.empty;
            upstream = upstream_update;
            read_only = read_only upstream_update;
          }


    type t = { upstream_environment: In.PreviousEnvironment.t }

    let create ast_environment =
      { upstream_environment = In.PreviousEnvironment.create ast_environment }


    let ast_environment { upstream_environment } =
      In.PreviousEnvironment.ast_environment upstream_environment


    let read_only { upstream_environment } =
      { ReadOnly.upstream_environment = In.PreviousEnvironment.read_only upstream_environment }


    let update_this_and_all_preceding_environments
        { upstream_environment }
        ~scheduler
        ~configuration
        ast_environment_trigger
      =
      In.PreviousEnvironment.update_this_and_all_preceding_environments
        upstream_environment
        ~scheduler
        ~configuration
        ast_environment_trigger
      |> update_only_this_environment ~scheduler ~configuration
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
