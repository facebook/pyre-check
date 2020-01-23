(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre

module type ReadOnly = sig
  type t

  val hash_to_key_map : t -> string String.Map.t

  val serialize_decoded : t -> Memory.decodable -> (string * string * string option) option

  val decoded_equal : t -> Memory.decodable -> Memory.decodable -> bool option

  val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.ReadOnly.t
end

module type PreviousUpdateResult = sig
  type t

  type read_only

  val locally_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.KeySet.t

  val all_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.KeySet.t list

  val read_only : t -> read_only

  val unannotated_global_environment_update_result
    :  t ->
    UnannotatedGlobalEnvironment.UpdateResult.t
end

module type PreviousEnvironment = sig
  module ReadOnly : ReadOnly

  module UpdateResult : PreviousUpdateResult with type read_only := ReadOnly.t

  val update_this_and_all_preceding_environments
    :  AstEnvironment.ReadOnly.t ->
    scheduler:Scheduler.t ->
    configuration:Configuration.Analysis.t ->
    ast_environment_update_result:AstEnvironment.UpdateResult.t ->
    Ast.Reference.Set.t ->
    UpdateResult.t
end

module UpdateResult = struct
  module type S = sig
    include PreviousUpdateResult

    type upstream

    val upstream : t -> upstream
  end

  module Make (PreviousEnvironment : PreviousEnvironment) (ReadOnly : ReadOnly) = struct
    type upstream = PreviousEnvironment.UpdateResult.t

    type read_only = ReadOnly.t

    type t = {
      upstream: upstream;
      triggered_dependencies: SharedMemoryKeys.DependencyKey.KeySet.t;
      read_only: ReadOnly.t;
    }

    let locally_triggered_dependencies { triggered_dependencies; _ } = triggered_dependencies

    let upstream { upstream; _ } = upstream

    let all_triggered_dependencies { triggered_dependencies; upstream; _ } =
      triggered_dependencies :: PreviousEnvironment.UpdateResult.all_triggered_dependencies upstream


    let read_only { read_only; _ } = read_only

    let unannotated_global_environment_update_result { upstream; _ } =
      PreviousEnvironment.UpdateResult.unannotated_global_environment_update_result upstream
  end
end

module type S = sig
  module ReadOnly : ReadOnly

  module PreviousEnvironment : PreviousEnvironment

  module UpdateResult :
    UpdateResult.S
      with type upstream = PreviousEnvironment.UpdateResult.t
       and type read_only = ReadOnly.t

  val update_this_and_all_preceding_environments
    :  AstEnvironment.ReadOnly.t ->
    scheduler:Scheduler.t ->
    configuration:Configuration.Analysis.t ->
    ast_environment_update_result:AstEnvironment.UpdateResult.t ->
    Ast.Reference.Set.t ->
    UpdateResult.t
end

module EnvironmentTable = struct
  module type In = sig
    module PreviousEnvironment : PreviousEnvironment

    module Key : Memory.KeyType

    module Value : Memory.ComparableValueType

    type trigger

    val convert_trigger : trigger -> Key.t

    val key_to_trigger : Key.t -> trigger

    module TriggerSet : Set.S with type Elt.t = trigger

    val lazy_incremental : bool

    val filter_upstream_dependency : SharedMemoryKeys.dependency -> trigger option

    val legacy_invalidated_keys : UnannotatedGlobalEnvironment.UpdateResult.t -> TriggerSet.t

    val produce_value
      :  PreviousEnvironment.ReadOnly.t ->
      trigger ->
      track_dependencies:bool ->
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

    val get : ?dependency:SharedMemoryKeys.DependencyKey.t -> key -> t option

    val add_dependency : key -> SharedMemoryKeys.DependencyKey.t -> unit
  end

  module type S = sig
    module In : In

    module ReadOnly : sig
      type t

      val get : t -> ?dependency:SharedMemoryKeys.dependency -> In.Key.t -> In.Value.t

      val upstream_environment : t -> In.PreviousEnvironment.ReadOnly.t

      val hash_to_key_map : t -> string String.Map.t

      val serialize_decoded : t -> Memory.decodable -> (string * string * string option) option

      val decoded_equal : t -> Memory.decodable -> Memory.decodable -> bool option

      val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.ReadOnly.t
    end

    module UpdateResult :
      UpdateResult.S
        with type upstream = In.PreviousEnvironment.UpdateResult.t
         and type read_only = ReadOnly.t

    val update_this_and_all_preceding_environments
      :  AstEnvironment.ReadOnly.t ->
      scheduler:Scheduler.t ->
      configuration:Configuration.Analysis.t ->
      ast_environment_update_result:AstEnvironment.UpdateResult.t ->
      Ast.Reference.Set.t ->
      UpdateResult.t
  end

  module Make
      (In : In)
      (Table : Table with type t = In.Value.t and type key = In.Key.t and type key_out = In.Key.out) =
  struct
    module In = In

    module ReadOnly = struct
      type t = { upstream_environment: In.PreviousEnvironment.ReadOnly.t }

      let upstream_environment { upstream_environment } = upstream_environment

      let get { upstream_environment } ?dependency key =
        match Table.get ?dependency key with
        | Some hit -> hit
        | None ->
            let value =
              In.produce_value upstream_environment (In.key_to_trigger key) ~track_dependencies:true
            in
            Table.add key value;
            Option.iter dependency ~f:(Table.add_dependency key);
            value


      let hash_to_key_map { upstream_environment } =
        let environment =
          In.PreviousEnvironment.ReadOnly.unannotated_global_environment upstream_environment
        in
        Table.compute_hashes_to_keys ~keys:(In.all_keys environment)


      let serialize_decoded _ decoded =
        match decoded with
        | Table.Decoded (key, value) ->
            let value = value >>| In.serialize_value in
            let key = In.show_key key in
            Some (In.Value.description, key, value)
        | _ -> None


      let decoded_equal _ first second =
        match first, second with
        | Table.Decoded (_, first), Table.Decoded (_, second) ->
            Some (Option.equal In.equal_value first second)
        | _ -> None


      let unannotated_global_environment { upstream_environment } =
        In.PreviousEnvironment.ReadOnly.unannotated_global_environment upstream_environment
    end

    module UpdateResult = UpdateResult.Make (In.PreviousEnvironment) (ReadOnly)

    let read_only previous_update_result =
      {
        ReadOnly.upstream_environment =
          In.PreviousEnvironment.UpdateResult.read_only previous_update_result;
      }


    let update_only_this_environment ~scheduler ~configuration upstream_update =
      Log.info "Updating %s Environment" In.Value.description;
      let update ~names_to_update ~track_dependencies () =
        let register =
          let set name =
            In.produce_value
              (In.PreviousEnvironment.UpdateResult.read_only upstream_update)
              name
              ~track_dependencies
            |> Table.add (In.convert_trigger name)
          in
          List.iter ~f:set
        in
        Scheduler.iter
          scheduler
          ~policy:
            (Scheduler.Policy.fixed_chunk_count
               ~minimum_chunks_per_worker:1
               ~minimum_chunk_size:100
               ~preferred_chunks_per_worker:5
               ())
          ~configuration
          ~f:register
          ~inputs:names_to_update
      in
      match configuration with
      | { incremental_style = FineGrained; _ } ->
          let triggered_dependencies =
            let name = Format.sprintf "TableUpdate(%s)" In.Value.description in
            Profiling.track_duration_and_shared_memory
              name
              ~tags:["phase_name", In.Value.description]
              ~f:(fun _ ->
                let names_to_update =
                  In.PreviousEnvironment.UpdateResult.all_triggered_dependencies upstream_update
                  |> List.fold ~init:In.TriggerSet.empty ~f:(fun triggers upstream_dependencies ->
                         SharedMemoryKeys.DependencyKey.KeySet.fold
                           (fun dependency triggers ->
                             match In.filter_upstream_dependency dependency with
                             | Some trigger -> In.TriggerSet.add triggers trigger
                             | None -> triggers)
                           upstream_dependencies
                           triggers)
                  |> Set.to_list
                in
                let (), triggered_dependencies =
                  let keys =
                    List.map names_to_update ~f:In.convert_trigger |> Table.KeySet.of_list
                  in
                  if In.lazy_incremental then
                    SharedMemoryKeys.DependencyKey.Transaction.empty
                    |> Table.add_pessimistic_transaction ~keys
                    |> SharedMemoryKeys.DependencyKey.Transaction.execute ~update:(fun () -> ())
                  else
                    SharedMemoryKeys.DependencyKey.Transaction.empty
                    |> Table.add_to_transaction ~keys
                    |> SharedMemoryKeys.DependencyKey.Transaction.execute
                         ~update:(update ~names_to_update ~track_dependencies:true)
                in
                triggered_dependencies)
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
            UpdateResult.triggered_dependencies = SharedMemoryKeys.DependencyKey.KeySet.empty;
            upstream = upstream_update;
            read_only = read_only upstream_update;
          }


    let update_this_and_all_preceding_environments
        ast_environment
        ~scheduler
        ~configuration
        ~ast_environment_update_result
        modified_qualifiers
      =
      In.PreviousEnvironment.update_this_and_all_preceding_environments
        ast_environment
        ~scheduler
        ~configuration
        ~ast_environment_update_result
        modified_qualifiers
      |> update_only_this_environment ~scheduler ~configuration
  end

  module WithCache (In : In) =
    Make
      (In)
      (Memory.DependencyTrackedTableWithCache (In.Key) (SharedMemoryKeys.DependencyKey) (In.Value))
  module NoCache (In : In) =
    Make
      (In)
      (Memory.DependencyTrackedTableNoCache (In.Key) (SharedMemoryKeys.DependencyKey) (In.Value))
end
