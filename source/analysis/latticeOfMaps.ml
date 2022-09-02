(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

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

  val of_alist : (string * 'a) list -> [ `Duplicate_key of string | `Ok of 'a t ]

  val of_alist_exn : (string * 'a) list -> 'a t
end

module Make (Map : MapSignature) = struct
  include Map

  let less_or_equal ~less_or_equal_one ~left ~right =
    let f ~key ~data:right_data sofar =
      sofar
      &&
      match find left key with
      | Some left_data -> less_or_equal_one ~left:left_data ~right:right_data
      | None -> false
    in
    fold right ~init:true ~f
end
