(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


(* Constructors of this type are used to select parts of composed abstract
   domains. E.g., a Set domain will add an Element: element constructor for the
   element type of the set, thereby allowing folding, partitioning, and
   transforming the abstract domains by Elements. Similarly, a Map domain will
   add a constructor for the Key part of the map and delegate all other parts to
   the range. *)
type _ part = ..


module type S = sig
  type t
  [@@deriving sexp]
  val bottom: t
  val is_bottom: t -> bool
  val join: t -> t -> t
  val less_or_equal: left:t -> right:t -> bool
  val show : t -> string
  val widen: iteration:int -> previous:t -> next:t -> t

  (* Access specific parts of composed abstract domains. *)
  val fold: 'a part -> f: ('b -> 'a -> 'b) -> init: 'b -> t -> 'b
  val partition: 'a part -> f: ('a -> 'b) -> t -> ('b, t) Core.Map.Poly.t
  val transform: 'a part -> f: ('a -> 'a) -> t -> t
end
