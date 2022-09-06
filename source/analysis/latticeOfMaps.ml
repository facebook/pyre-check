(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast

module type MapSignature = sig
  type key

  type 'data t

  val empty : 'data t

  val mem : 'data t -> key -> bool

  val set : 'data t -> key:key -> data:'data -> 'data t

  val find : 'data t -> key -> 'data option

  val remove : 'data t -> key -> 'data t

  val fold : 'data t -> init:'a -> f:(key:key -> data:'data -> 'a -> 'a) -> 'a

  val fold2
    :  'data t ->
    'data t ->
    init:'a ->
    f:(key:key -> data:[ `Both of 'data * 'data | `Left of 'data | `Right of 'data ] -> 'a -> 'a) ->
    'a
end

module type LatticeFunctions = sig
  type key

  type 'data t

  val less_or_equal
    :  less_or_equal_one:(left:'data -> right:'data -> bool) ->
    left:'data t ->
    right:'data t ->
    bool

  val join : join_one:('data -> 'data -> 'data) -> 'data t -> 'data t -> 'data t

  val meet : meet_one:('data -> 'data -> 'data) -> 'data t -> 'data t -> 'data t

  val merge_with : merge_one:('data -> 'data -> 'data) -> 'data t -> 'data t -> 'data t

  val update_existing_entries : map_to_update:'data t -> new_map:'data t -> 'data t
end

module Make (Map : MapSignature) :
  LatticeFunctions with type key := Map.key and type 'data t := 'data Map.t = struct
  open Map

  let less_or_equal ~less_or_equal_one ~left ~right =
    let f ~key ~data:right_data sofar =
      sofar
      &&
      match find left key with
      | Some left_data -> less_or_equal_one ~left:left_data ~right:right_data
      | None -> false
    in
    fold right ~init:true ~f


  let join ~join_one left right =
    let f ~key ~data sofar =
      match data with
      | `Both (left, right) -> set sofar ~key ~data:(join_one left right)
      | `Left _
      | `Right _ ->
          (* If the key is present in only one of the maps, don't add it. That way, both `left` and
             `right` will be less-or-equal to the resulting map, making the resulting map an upper
             bound. *)
          sofar
    in
    fold2 left right ~init:empty ~f


  let meet ~meet_one left right =
    let f ~key ~data sofar =
      match data with
      | `Both (left, right) -> set sofar ~key ~data:(meet_one left right)
      | `Left data
      | `Right data ->
          (* If the key is present in only one of the maps, add it, so that the resulting map is
             compatible with both `left` and `right`. *)
          set sofar ~key ~data
    in
    fold2 left right ~init:empty ~f


  let merge_with ~merge_one left right =
    let f ~key ~data sofar =
      match data with
      | `Both (left, right) -> set sofar ~key ~data:(merge_one left right)
      | `Left data
      | `Right data ->
          set sofar ~key ~data
    in
    fold2 left right ~init:empty ~f


  let update_existing_entries ~map_to_update ~new_map =
    let update_key_if_it_exists ~key ~data map =
      if mem map key then
        set ~key ~data map
      else
        map
    in
    fold ~init:map_to_update ~f:update_key_if_it_exists new_map
end

module IdentifierMap = struct
  module T = struct
    include Identifier.Map.Tree

    type key = Identifier.t
  end

  include T
  include Make (T)
end

module ReferenceMap = struct
  module T = struct
    include Reference.Map

    type key = Reference.t
  end

  include T
  include Make (T)
end
