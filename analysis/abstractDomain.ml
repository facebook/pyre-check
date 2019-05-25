(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

(* Constructors of this type are used to select parts of composed abstract domains. E.g., a Set
   domain will add an Element: element constructor for the element type of the set, thereby
   allowing folding, partitioning, and transforming the abstract domains by Elements. Similarly, a
   Map domain will add a constructor for the Key part of the map and delegate all other parts to
   the range. *)
type _ part = ..

(* Packages a part and a corresponding value. See create function below. *)
type value_part = Part : 'a part * 'a -> value_part

module type S = sig
  type t [@@deriving show, sexp]

  val bottom : t

  val is_bottom : t -> bool

  val join : t -> t -> t

  val less_or_equal : left:t -> right:t -> bool

  val widen : iteration:int -> previous:t -> next:t -> t

  (* subtract a from = b, s.t. b <= from <= b |_| a *)
  val subtract : t -> from:t -> t

  (* Access specific parts of composed abstract domains. *)
  val fold : 'a part -> f:('b -> 'a -> 'b) -> init:'b -> t -> 'b

  val partition : 'a part -> f:('a -> 'b) -> t -> ('b, t) Core.Map.Poly.t

  val transform : 'a part -> f:('a -> 'a) -> t -> t

  (* Create an abstract value based on a part list. *)
  val create : value_part list -> t
end

(* First class abstract domain value. Used e.g., in the product domain. Should not be stored as
   part of abstract values. *)
type 'a abstract_domain = (module S with type t = 'a)

(* Equality witness used as result of comparing GADTs. *)
type ('a, 'b) equality_witness =
  | Equal : ('a, 'a) equality_witness
  | Distinct : ('a, 'b) equality_witness
