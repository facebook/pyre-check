(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module type SexpableKeyType = sig
  type t [@@deriving sexp, compare]

  val to_string : t -> string

  type out

  val from_string : string -> out
end

module type In = sig
  module PreviousEnvironment : Environment.PreviousEnvironment

  module Key : SexpableKeyType

  module Value : Memory.ComparableValueType

  module KeySet : Set.S with type Elt.t = Key.t

  module HashableKey : Hashable with type t := Key.t

  val lazy_incremental : bool

  val produce_value
    :  PreviousEnvironment.ReadOnly.t ->
    Key.t ->
    dependency:SharedMemoryKeys.DependencyKey.registered option ->
    Value.t

  val filter_upstream_dependency : SharedMemoryKeys.dependency -> Key.t option

  val trigger_to_dependency : Key.t -> SharedMemoryKeys.dependency
end

module Make (In : In) = struct
  module UnmanagedCache = struct
    let enabled = ref false

    let cache = In.HashableKey.Table.create ()

    let clear () = In.HashableKey.Table.clear cache
  end

  (* For some reason this no longer matches our mli when I directly include this :/ *)
  module EnvironmentTable = Environment.EnvironmentTable.WithCache (struct
    include In

    (* I'd like to remove the distinction between triggers and values in general, but for now we can
       just make sure that any new managed caches don't rely on it *)
    type trigger = In.Key.t [@@deriving sexp, compare]

    let convert_trigger = Fn.id

    let key_to_trigger = Fn.id

    module TriggerSet = KeySet

    let lazy_incremental = In.lazy_incremental

    (* In legacy mode we're actually using process-local caches, so we never need to do a
       legacy-style invalidate in the way this interface is designed to do *)
    let legacy_invalidated_keys _ = KeySet.empty

    (* All of these functions are used for the shared memory debugging functionality. They all rely
       on the ability to exhaustively list your keys, which is doable for traditional environments,
       but the idea of a managed cache is using this for situations where the keys are not known
       ahead of time. For now we'll just ignore this debugging stuff, and maybe return to it later. *)
    let all_keys _ = []

    let serialize_value _ = "Not used"

    let show_key _ = "Not used"

    let equal_value _ _ = false
  end)

  include EnvironmentTable

  let update_this_and_all_preceding_environments ast_environment ~scheduler ~configuration =
    let () =
      match configuration with
      | { Configuration.Analysis.incremental_style = FineGrained; _ } ->
          UnmanagedCache.enabled := false;
          ()
      | _ ->
          UnmanagedCache.enabled := true;
          Scheduler.once_per_worker scheduler ~configuration ~f:UnmanagedCache.clear;
          UnmanagedCache.clear ()
    in
    (* In both cases we need to produce a UpdateResult, but in the legacy case this will be a
       basically a no-op because of all_keys = []. *)
    update_this_and_all_preceding_environments ast_environment ~scheduler ~configuration


  module ReadOnly = struct
    include ReadOnly

    let get read_only ?dependency key =
      match !UnmanagedCache.enabled with
      | false -> get read_only ?dependency key
      | true ->
          let default () = In.produce_value (upstream_environment read_only) key ~dependency:None in
          Hashtbl.find_or_add UnmanagedCache.cache key ~default
  end
end
