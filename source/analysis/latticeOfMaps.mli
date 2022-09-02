(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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

module Make (Map : MapSignature) : sig
  include MapSignature

  (** The keys of `right` have to be a subset of the keys of `left` for `left` to be less than or
      equal to `right`, since more keys = more restrictions = lower in the lattice *)
  val less_or_equal
    :  less_or_equal_one:(left:'data -> right:'data -> bool) ->
    left:'data t ->
    right:'data t ->
    bool
end
