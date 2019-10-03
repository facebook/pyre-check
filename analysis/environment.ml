(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

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

module Updater = struct
  module type In = sig
    module Table : sig
      include Memory.NoCache.S

      val add_to_transaction
        :  SharedMemoryKeys.DependencyKey.Transaction.t ->
        keys:KeySet.t ->
        SharedMemoryKeys.DependencyKey.Transaction.t

      val get : ?dependency:SharedMemoryKeys.DependencyKey.t -> key -> t option
    end

    module PreviousEnvironment : PreviousEnvironment

    module UpdateResult : sig
      include UpdateResult.S with type upstream = PreviousEnvironment.UpdateResult.t

      include UpdateResult.Private with type upstream := upstream and type t := t
    end

    type t

    type trigger

    val convert_trigger : trigger -> Table.key

    module TriggerSet : Set.S with type Elt.t = trigger

    val filter_upstream_dependency : SharedMemoryKeys.dependency -> trigger option

    val added_keys : UpdateResult.upstream -> TriggerSet.t

    val current_and_previous_keys : UpdateResult.upstream -> TriggerSet.t

    val register : t -> trigger list -> track_dependencies:bool -> unit
  end

  module Make (In : In) = struct
    let update environment ~scheduler ~configuration upstream_update =
      let update ~names_to_update ~track_dependencies () =
        Scheduler.iter
          scheduler
          ~configuration
          ~f:(In.register environment ~track_dependencies)
          ~inputs:names_to_update
      in
      match configuration with
      | { incremental_style = FineGrained; _ } ->
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
            let keys = List.map dependencies ~f:In.convert_trigger |> In.Table.KeySet.of_list in
            SharedMemoryKeys.DependencyKey.Transaction.empty
            |> In.Table.add_to_transaction ~keys
            |> SharedMemoryKeys.DependencyKey.Transaction.execute
                 ~update:(update ~names_to_update ~track_dependencies:true)
          in
          In.UpdateResult.create ~triggered_dependencies ~upstream:upstream_update
      | _ ->
          let current_and_previous_keys =
            In.current_and_previous_keys upstream_update |> Set.to_list
          in
          current_and_previous_keys
          |> List.map ~f:In.convert_trigger
          |> In.Table.KeySet.of_list
          |> In.Table.remove_batch;
          update ~names_to_update:current_and_previous_keys () ~track_dependencies:false;

          In.UpdateResult.create
            ~triggered_dependencies:SharedMemoryKeys.DependencyKey.KeySet.empty
            ~upstream:upstream_update
  end
end
