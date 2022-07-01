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

  val from_string : string -> t
end

module type In = sig
  module PreviousEnvironment : Environment.PreviousEnvironment.S

  module Key : SexpableKeyType

  module Value : Memory.ValueTypeWithEquivalence

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

  val overlay_owns_key : ModuleTracker.Overlay.t -> Key.t -> bool
end

module Make (In : In) = struct
  module UnmanagedCache = struct
    let enabled = ref false

    let cache = In.HashableKey.Table.create ()
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

    let show_key _ = "Not used"

    let equal_value _ _ = false
  end)

  include EnvironmentTable

  let create controls =
    let table = EnvironmentTable.create controls in
    let () = UnmanagedCache.enabled := not (EnvironmentControls.track_dependencies controls) in
    table


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
