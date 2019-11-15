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
end

module type PreviousUpdateResult = sig
  type t

  type read_only

  val locally_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.KeySet.t

  val all_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.KeySet.t list

  val read_only : t -> read_only
end

module type PreviousEnvironment = sig
  module ReadOnly : ReadOnly

  module UpdateResult : PreviousUpdateResult with type read_only := ReadOnly.t
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


    let create ~triggered_dependencies ~upstream ~read_only =
      { triggered_dependencies; upstream; read_only }


    let read_only { read_only; _ } = read_only
  end

  module type Private = sig
    type upstream

    type t

    type read_only

    val create
      :  triggered_dependencies:SharedMemoryKeys.DependencyKey.KeySet.t ->
      upstream:upstream ->
      read_only:read_only ->
      t
  end
end

module type S = sig
  module ReadOnly : ReadOnly

  module PreviousEnvironment : PreviousEnvironment

  module UpdateResult :
    UpdateResult.S
      with type upstream = PreviousEnvironment.UpdateResult.t
       and type read_only = ReadOnly.t

  val update
    :  scheduler:Scheduler.t ->
    configuration:Configuration.Analysis.t ->
    PreviousEnvironment.UpdateResult.t ->
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

    val filter_upstream_dependency : SharedMemoryKeys.dependency -> trigger option

    val legacy_invalidated_keys : PreviousEnvironment.UpdateResult.t -> TriggerSet.t

    val produce_value
      :  PreviousEnvironment.ReadOnly.t ->
      trigger ->
      track_dependencies:bool ->
      Value.t

    val all_keys : PreviousEnvironment.ReadOnly.t -> Key.t list

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
    end

    module UpdateResult : sig
      include
        UpdateResult.S
          with type upstream = In.PreviousEnvironment.UpdateResult.t
           and type read_only = ReadOnly.t

      include
        UpdateResult.Private
          with type upstream := upstream
           and type t := t
           and type read_only := read_only
    end

    val update
      :  scheduler:Scheduler.t ->
      configuration:Configuration.Analysis.t ->
      In.PreviousEnvironment.UpdateResult.t ->
      UpdateResult.t
  end

  module Make
      (In : In)
      (Table : Table with type t = In.Value.t and type key = In.Key.t and type key_out = In.Key.out) =
  struct
    module In = In

    module ReadOnly = struct
      type t = {
        upstream_environment: In.PreviousEnvironment.ReadOnly.t;
        get: ?dependency:SharedMemoryKeys.dependency -> In.Key.t -> In.Value.t;
        hash_to_key_map: In.PreviousEnvironment.ReadOnly.t -> string String.Map.t;
        serialize_decoded: Memory.decodable -> (string * string * string option) option;
        decoded_equal: Memory.decodable -> Memory.decodable -> bool option;
      }

      let upstream_environment { upstream_environment; _ } = upstream_environment

      let get { get; _ } = get

      let hash_to_key_map { hash_to_key_map; upstream_environment; _ } =
        hash_to_key_map upstream_environment


      let serialize_decoded { serialize_decoded; _ } = serialize_decoded

      let decoded_equal { decoded_equal; _ } = decoded_equal
    end

    module UpdateResult = UpdateResult.Make (In.PreviousEnvironment) (ReadOnly)

    let read_only previous_update_result =
      let upstream_environment =
        In.PreviousEnvironment.UpdateResult.read_only previous_update_result
      in
      let get ?dependency key =
        match Table.get ?dependency key with
        | Some hit -> hit
        | None ->
            let value =
              In.produce_value upstream_environment (In.key_to_trigger key) ~track_dependencies:true
            in
            Table.add key value;
            Option.iter dependency ~f:(Table.add_dependency key);
            value
      in
      let hash_to_key_map environment =
        Table.compute_hashes_to_keys ~keys:(In.all_keys environment)
      in
      let serialize_decoded decoded =
        match decoded with
        | Table.Decoded (key, value) ->
            let value = value >>| In.serialize_value in
            let key = In.show_key key in
            Some (In.Value.description, key, value)
        | _ -> None
      in
      let decoded_equal first second =
        match first, second with
        | Table.Decoded (_, first), Table.Decoded (_, second) ->
            Some (Option.equal In.equal_value first second)
        | _ -> None
      in
      { ReadOnly.upstream_environment; get; hash_to_key_map; serialize_decoded; decoded_equal }


    let update ~scheduler ~configuration upstream_update =
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
        Scheduler.iter scheduler ~configuration ~f:register ~inputs:names_to_update
      in
      match configuration with
      | { incremental_style = FineGrained; _ } ->
          let triggered_dependencies =
            let name = Format.sprintf "TableUpdate(%s)" In.Value.description in
            Profiling.track_duration_and_shared_memory
              name
              ~tags:["phase_name", In.Value.description]
              ~f:(fun _ ->
                let dependencies =
                  let filter = List.filter_map ~f:In.filter_upstream_dependency in
                  In.PreviousEnvironment.UpdateResult.all_triggered_dependencies upstream_update
                  |> List.map ~f:SharedMemoryKeys.DependencyKey.KeySet.elements
                  |> List.concat_map ~f:filter
                in
                let (), triggered_dependencies =
                  let names_to_update =
                    dependencies |> List.fold ~f:Set.add ~init:In.TriggerSet.empty |> Set.to_list
                  in
                  let keys =
                    List.map names_to_update ~f:In.convert_trigger |> Table.KeySet.of_list
                  in
                  SharedMemoryKeys.DependencyKey.Transaction.empty
                  |> Table.add_to_transaction ~keys
                  |> SharedMemoryKeys.DependencyKey.Transaction.execute
                       ~update:(update ~names_to_update ~track_dependencies:true)
                in
                triggered_dependencies)
          in
          UpdateResult.create
            ~triggered_dependencies
            ~upstream:upstream_update
            ~read_only:(read_only upstream_update)
      | _ ->
          let _ =
            let name = Format.sprintf "LegacyTableUpdate(%s)" In.Value.description in
            Profiling.track_duration_and_shared_memory
              name
              ~tags:["phase_name", In.Value.description]
              ~f:(fun _ ->
                In.legacy_invalidated_keys upstream_update
                |> Set.to_list
                |> List.map ~f:In.convert_trigger
                |> Table.KeySet.of_list
                |> Table.remove_batch)
          in
          UpdateResult.create
            ~triggered_dependencies:SharedMemoryKeys.DependencyKey.KeySet.empty
            ~upstream:upstream_update
            ~read_only:(read_only upstream_update)
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
