(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the LICENSE file in the root
    directory of this source tree. *)

module type KEY = sig
  include Map.OrderedType

  val name : string

  val show : t -> string

  (* If true, an absent key and a key mapping to bottom are equivalent. Map won't keep elements that
     map to bottom. Presence of keys is still observable via fold, transform, and partition. *)
  val absence_implicitly_maps_to_bottom : bool
end

type 'a AbstractDomainCore.transform +=
  | (* Custom operation to expand one key/binding into multiple bindings *)
      Expand :
      ('a -> 'a list)
      -> 'a AbstractDomainCore.transform

module Make (Key : KEY) (Element : AbstractDomainCore.S) : sig
  include AbstractDomainCore.S

  type _ AbstractDomainCore.part +=
    | Key : Key.t AbstractDomainCore.part | KeyValue : (Key.t * Element.t) AbstractDomainCore.part

  (* Replace map at key with new element or set if missing. *)
  val set : t -> key:Key.t -> data:Element.t -> t

  (* Update map at key with result of ~f, passing possibly existing element. *)
  val update : t -> Key.t -> f:(Element.t option -> Element.t) -> t

  val get : Key.t -> t -> Element.t option

  val to_alist : t -> (Key.t * Element.t) list
end
