(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

module type S = sig
  type t [@@deriving sexp, compare, hash]
end

module Make (Key : S) (Value : S) = struct
  module Item = struct
    type t = Key.t * Value.t [@@deriving sexp, compare, hash]
  end

  module ItemSet = struct
    type t = Hash_set.M(Item).t [@@deriving sexp]

    let to_list = Hash_set.to_list
  end

  module Difference = struct
    type t = {
      added: ItemSet.t;
      removed: ItemSet.t;
    }
    [@@deriving sexp]
  end

  module Unindexed = struct
    type t = ItemSet.t

    let of_list = Hash_set.of_list (module Item)

    let length = Hash_set.length

    let difference ~original current =
      {
        Difference.added = Hash_set.diff current original;
        removed = Hash_set.diff original current;
      }


    let lookup_key mapping key =
      let f sofar (item_key, item_value) =
        if Int.equal 0 (Key.compare item_key key) then
          item_value :: sofar
        else
          sofar
      in
      Hash_set.fold mapping ~init:[] ~f


    let lookup_value mapping value =
      let f sofar (item_key, item_value) =
        if Int.equal 0 (Value.compare item_value value) then
          item_key :: sofar
        else
          sofar
      in
      Hash_set.fold mapping ~init:[] ~f
  end
end
