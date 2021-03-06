(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

(** This module implements a general bi-directional mapping data structure. The mapping permits
    many-to-many associations: there can be multiple keys mapped to the same value, and there can be
    multiple values mapped to the same key. Mappings are interpreted in an order-insensitive way: it
    does not matter which items are inserted before or after which. As long as two mappings hold the
    same items, they will be considered equivalent. *)

(** Under the hood, the data structure is implemented as a set of (key, value) pairs. To simplify
    the implementation, interfaces are designed in such a way that mappings are read-only after
    creation. Any in-place mutation of the structure is intentionally not supported. *)

module type S = sig
  type t [@@deriving sexp, compare, hash]
  (** We require both the key and the value type to be hashable, as lookups from both directions are
      supported. *)
end

module Make (Key : S) (Value : S) : sig
  (** Type of each element in the mapping. *)
  module Item : sig
    type t = Key.t * Value.t [@@deriving sexp, compare, hash]
  end

  (** A set of `Item.t`. *)
  module ItemSet : sig
    type t [@@deriving sexp]

    val to_list : t -> Item.t list
  end

  (** Provides the result type of the `difference` operation. *)
  module Difference : sig
    type t = {
      added: ItemSet.t;
      removed: ItemSet.t;
    }
    [@@deriving sexp]
  end

  (** An unindexed data structure which provides only the raw storage for the underlying mappings.
      It supports O(n) lookup operations on both the key and the value side. *)
  module Unindexed : sig
    type t

    val of_list : Item.t list -> t
    (** Create a bidirectional map from a list of (key, value) pairs. *)

    val length : t -> int
    (** Number of (key, value) pairs stored in the mapping. *)

    val difference : original:t -> t -> Difference.t
    (** [difference ~original current] computes the difference between the [original] mapping and
        the [current] mapping. The difference consists of (1) items that are included in [original]
        but not in [current], and (2) items that are included in [current] but not in [original].
        Time complexity of this operation is O(n + m), where n and m are the sizes of the given hash
        sets. *)

    val lookup_key : t -> Key.t -> Value.t list
    (** Lookup all values that corresponds to the given key in a mapping. If there is no such value,
        an empty list is returned. Note that the elements in the returned list are guaranteed to
        contain no duplicates but are not guaranteed to be put in any particular order. Time
        complexity of this operation is O(n), where n is the size of the mapping. *)

    val lookup_value : t -> Value.t -> Key.t list
    (** Lookup all keys that corresponds to the given value in a mapping. If there is no such key,
        an empty list is returned. Note that the elements in the returned list are guaranteed to
        contain no duplicates but are not guaranteed to be put in any particular order. Time
        complexity of this operation is O(n), where n is the size of the mapping.*)
  end
end
