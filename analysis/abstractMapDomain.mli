(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


module type KEY = sig
  include Core.Map.Key

  val show: t -> string

  (* If true, an absent key and a key mapping to bottom are equivalent. Map
     won't keep elements that map to bottom. Presence of keys is still
     observable via fold, transform, and partition. *)
  val absence_implicitly_maps_to_bottom: bool
end


module Make(Key: KEY)(Element : AbstractDomain.S) : sig
  include AbstractDomain.S

  type _ AbstractDomain.part +=
    | Key: Key.t AbstractDomain.part

  val set: t -> key: Key.t -> data: Element.t -> t
  val find: t -> Key.t -> Element.t option
  val to_alist:
    ?key_order:[ `Decreasing | `Increasing ]
    -> t
    -> (Key. t * Element.t) list

end
