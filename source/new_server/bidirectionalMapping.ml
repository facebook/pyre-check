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

  module Index = struct
    type t = {
      key_index: Value.t list Hashtbl.M(Key).t;
      value_index: Key.t list Hashtbl.M(Value).t;
    }

    let create unindexed =
      let size = Unindexed.length unindexed in
      let key_index = Hashtbl.create (module Key) ~size in
      let value_index = Hashtbl.create (module Value) ~size in
      let do_index (item_key, item_value) =
        Hashtbl.update key_index item_key ~f:(function
            | None -> [item_value]
            | Some values -> item_value :: values);
        Hashtbl.update value_index item_value ~f:(function
            | None -> [item_key]
            | Some keys -> item_key :: keys)
      in
      Hash_set.iter unindexed ~f:do_index;
      { key_index; value_index }


    let lookup_key { key_index; _ } key = Hashtbl.find key_index key |> Option.value ~default:[]

    let lookup_value { value_index; _ } value =
      Hashtbl.find value_index value |> Option.value ~default:[]
  end

  module Indexed = struct
    type t = {
      unindexed: Unindexed.t;
      index: Index.t;
    }

    let create unindexed = { unindexed; index = Index.create unindexed }

    let length { unindexed; _ } = Unindexed.length unindexed

    let difference
        ~original:{ unindexed = original_unindexed; _ }
        { unindexed = current_unindexed; _ }
      =
      Unindexed.difference ~original:original_unindexed current_unindexed


    let lookup_key { index; _ } key = Index.lookup_key index key

    let lookup_value { index; _ } value = Index.lookup_value index value
  end
end
