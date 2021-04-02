(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Constructors of this type are used to select parts of composed abstract domains. E.g., a Set
   domain will add an Element: element constructor for the element type of the set, thereby allowing
   folding, partitioning, and transforming the abstract domains by Elements. Similarly, a Map domain
   will add a constructor for the Key part of the map and delegate all other parts to the range. *)
type _ part = ..

(* Packages a part and a corresponding value. See create function below. *)
type value_part = Part : 'a part * 'a -> value_part

type _ introspect =
  (* Uses an object to pass a polymorphic function. *)
  | GetParts : < report : 'a. 'a part -> unit > -> unit introspect
  (* Get the human readable name of a particular part *)
  | Name : 'a part -> string introspect
  (* Get multi-line description of entire domain structure *)
  | Structure : string list introspect

type 'a transform =
  | Map of ('a -> 'a)
  | Add : 'a -> 'a transform
  | Filter : ('a -> bool) -> 'a transform
  | Expand : ('a -> 'a list) -> 'a transform
  | Multi : transformer list -> 'a transform

and transformer = T : 'a part * 'a transform -> transformer

module type S = sig
  type t [@@deriving show]

  type _ part += Self : t part

  val bottom : t

  val is_bottom : t -> bool

  val join : t -> t -> t

  val meet : t -> t -> t

  val less_or_equal : left:t -> right:t -> bool

  val widen : iteration:int -> prev:t -> next:t -> t

  (* subtract a from = b, s.t. b <= from and from <= b |_| a or b is bottom. The latter case (b =
     bottom) arises for strict domains. *)
  val subtract : t -> from:t -> t

  (* Access specific parts of composed abstract domains. *)
  val fold : 'a part -> f:('a -> 'b -> 'b) -> init:'b -> t -> 'b

  (* Partition the domain according to function. None partitions are dropped *)
  val partition : 'a part -> f:('a -> 'b option) -> t -> ('b, t) Core_kernel.Map.Poly.t

  val transform : 'a part -> 'a transform -> t -> t

  (* Return insights about the abstract domain structure *)
  val introspect : 'a introspect -> 'a

  (* Create an abstract value based on a part list. *)
  val create : value_part list -> t
end

(* First class abstract domain value. Used e.g., in the product domain. Should not be stored as part
   of abstract values. *)
type 'a abstract_domain = (module S with type t = 'a)

(* Equality witness used as result of comparing GADTs. *)
type ('a, 'b) equality_witness =
  | Equal : ('a, 'a) equality_witness
  | Distinct : ('a, 'b) equality_witness

let part_id (part : 'a part) : int =
  Obj.Extension_constructor.of_val part |> Obj.Extension_constructor.id


let part_name (part : 'a part) =
  Format.sprintf
    "%s:%d"
    (Obj.Extension_constructor.of_val part |> Obj.Extension_constructor.name)
    (part_id part)


let transform_name (t : 'a transform) =
  match t with
  | Map _ -> "Map"
  | Add _ -> "Add"
  | Filter _ -> "Filter"
  | Expand _ -> "Expand"
  | Multi _ -> "Multi"


module Common (D : sig
  type t

  val bottom : t

  val join : t -> t -> t

  val less_or_equal : left:t -> right:t -> bool
end) =
struct
  type _ part += Self : D.t part

  let unhandled_transform (p : 'a part) (t : 'a transform) =
    Format.sprintf "Unhandled part %s in transform %s" (part_name p) (transform_name t) |> failwith


  let unhandled_part (type a) (part : a part) what =
    let part_name = part_name part in
    Format.sprintf "Unknown part %s in %s" part_name what |> failwith


  let unhandled_introspect (type a) (op : a introspect) =
    match op with
    | GetParts _ -> Format.sprintf "Unhandled GetParts introspect" |> failwith
    | Name part ->
        let part_name = part_name part in
        Format.sprintf "Unhandled Name(%s) introspect" part_name |> failwith
    | Structure -> Format.sprintf "Unhandled Structure introspect" |> failwith


  let unhandled_fold part = unhandled_part part "fold"

  let unhandled_partition part = unhandled_part part "partition"

  let unhandled_create part = unhandled_part part "create"

  let fold (type a b) (part : a part) ~(f : a -> b -> b) ~(init : b) (d : D.t) : b =
    match part with
    | Self -> f d init
    | _ -> unhandled_fold part


  let transform
      (type a)
      (domain_transform : transformer -> D.t -> D.t)
      (part : a part)
      (t : a transform)
      (d : D.t)
      : D.t
    =
    match part, t with
    | Self, Map f -> f d
    | Self, Add a -> D.join d a
    | Self, Filter f ->
        if f d then
          d
        else
          D.bottom
    | _, Multi tl -> List.fold_left (fun d t -> domain_transform t d) d tl
    | _ -> unhandled_transform part t


  let partition (type a b) (part : a part) ~(f : a -> b option) (d : D.t) =
    match part with
    | Self -> (
        match f d with
        | None -> Core_kernel.Map.Poly.empty
        | Some key -> Core_kernel.Map.Poly.singleton key d )
    | _ -> unhandled_partition part


  let create (type a) (part : a part) (value : a) (sofar : D.t) : D.t =
    match part with
    | Self -> D.join sofar value
    | _ -> unhandled_create part


  let introspect (type a) (op : a introspect) : a = unhandled_introspect op

  (* default meet, returns the smaller argument if determinable, otherwise the first argument *)
  let meet a b = if D.less_or_equal ~left:b ~right:a then b else a
end
