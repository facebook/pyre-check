(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre

module type ReadOnly = sig
  type t
end

module type PreviousUpdateResult = sig
  type t

  val locally_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.KeySet.t

  val all_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.KeySet.t list
end

module type PreviousEnvironment = sig
  type t

  module ReadOnly : ReadOnly

  module UpdateResult : PreviousUpdateResult
end

module UpdateResult = struct
  module type S = sig
    include PreviousUpdateResult

    type upstream

    val upstream : t -> upstream
  end

  module Make (PreviousEnvironment : PreviousEnvironment) = struct
    type upstream = PreviousEnvironment.UpdateResult.t

    type t = {
      upstream: upstream;
      triggered_dependencies: SharedMemoryKeys.DependencyKey.KeySet.t;
    }

    let locally_triggered_dependencies { triggered_dependencies; _ } = triggered_dependencies

    let upstream { upstream; _ } = upstream

    let all_triggered_dependencies { triggered_dependencies; upstream } =
      triggered_dependencies
      :: PreviousEnvironment.UpdateResult.all_triggered_dependencies upstream


    let create ~triggered_dependencies ~upstream = { triggered_dependencies; upstream }
  end

  module type Private = sig
    type upstream

    type t

    val create
      :  triggered_dependencies:SharedMemoryKeys.DependencyKey.KeySet.t ->
      upstream:upstream ->
      t
  end
end

module type S = sig
  type t

  module ReadOnly : ReadOnly

  module PreviousEnvironment : PreviousEnvironment

  module UpdateResult : UpdateResult.S with type upstream = PreviousEnvironment.UpdateResult.t

  val create : PreviousEnvironment.ReadOnly.t -> t

  val update
    :  t ->
    scheduler:Scheduler.t ->
    configuration:Configuration.Analysis.t ->
    PreviousEnvironment.UpdateResult.t ->
    UpdateResult.t

  val read_only : t -> ReadOnly.t
end

module EnvironmentTable = struct
  module type In = sig
    module PreviousEnvironment : PreviousEnvironment

    module UpdateResult : sig
      include UpdateResult.S with type upstream = PreviousEnvironment.UpdateResult.t

      include UpdateResult.Private with type upstream := upstream and type t := t
    end

    module Key : Memory.KeyType

    module Value : Memory.ComparableValueType

    type t

    type trigger

    val convert_trigger : trigger -> Key.t

    module TriggerSet : Set.S with type Elt.t = trigger

    val filter_upstream_dependency : SharedMemoryKeys.dependency -> trigger option

    val added_keys : UpdateResult.upstream -> TriggerSet.t

    val current_and_previous_keys : UpdateResult.upstream -> TriggerSet.t

    val produce_value : t -> trigger -> track_dependencies:bool -> Value.t option

    val all_keys : t -> Key.t list

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
  end

  module type S = sig
    module In : In

    val update
      :  In.t ->
      scheduler:Scheduler.t ->
      configuration:Configuration.Analysis.t ->
      In.PreviousEnvironment.UpdateResult.t ->
      In.UpdateResult.t

    val get : ?dependency:SharedMemoryKeys.dependency -> In.Key.t -> In.Value.t sexp_option

    val hash_to_key_map : In.t -> string String.Map.t

    val serialize_decoded : Memory.decodable -> (string * string * string option) option

    val decoded_equal : Memory.decodable -> Memory.decodable -> bool option
  end

  module Make
      (In : In)
      (Table : Table with type t = In.Value.t and type key = In.Key.t and type key_out = In.Key.out) =
  struct
    module In = In

    let update environment ~scheduler ~configuration upstream_update =
      let update ~names_to_update ~track_dependencies () =
        let register =
          let set name =
            In.produce_value environment name ~track_dependencies
            |> Option.iter ~f:(Table.add (In.convert_trigger name))
          in
          List.iter ~f:set
        in
        Scheduler.iter scheduler ~configuration ~f:register ~inputs:names_to_update
      in
      match configuration with
      | { incremental_style = FineGrained; _ } ->
          let triggered_dependencies =
            let name = Format.sprintf "TableUpdate(%s)" In.Value.description in
            Profiling.track_duration_and_shared_memory name ~f:(fun _ ->
                let dependencies =
                  let filter = List.filter_map ~f:In.filter_upstream_dependency in
                  In.PreviousEnvironment.UpdateResult.all_triggered_dependencies upstream_update
                  |> List.map ~f:SharedMemoryKeys.DependencyKey.KeySet.elements
                  |> List.concat_map ~f:filter
                in
                let (), triggered_dependencies =
                  let names_to_update =
                    dependencies
                    |> List.fold ~f:Set.add ~init:(In.added_keys upstream_update)
                    |> Set.to_list
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
          In.UpdateResult.create ~triggered_dependencies ~upstream:upstream_update
      | _ ->
          let _ =
            let name = Format.sprintf "LegacyTableUpdate(%s)" In.Value.description in
            Profiling.track_duration_and_shared_memory name ~f:(fun _ ->
                let current_and_previous_keys =
                  In.current_and_previous_keys upstream_update |> Set.to_list
                in
                current_and_previous_keys
                |> List.map ~f:In.convert_trigger
                |> Table.KeySet.of_list
                |> Table.remove_batch;
                update ~names_to_update:current_and_previous_keys () ~track_dependencies:false)
          in
          In.UpdateResult.create
            ~triggered_dependencies:SharedMemoryKeys.DependencyKey.KeySet.empty
            ~upstream:upstream_update


    let get = Table.get

    let hash_to_key_map environment = Table.compute_hashes_to_keys ~keys:(In.all_keys environment)

    let serialize_decoded decoded =
      match decoded with
      | Table.Decoded (key, value) ->
          let value = value >>| In.serialize_value in
          let key = In.show_key key in
          Some (In.Value.description, key, value)
      | _ -> None


    let decoded_equal first second =
      match first, second with
      | Table.Decoded (_, first), Table.Decoded (_, second) ->
          Some (Option.equal In.equal_value first second)
      | _ -> None
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
