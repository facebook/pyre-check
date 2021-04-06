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

type transform2 = T

type reduce = R

type partition = P

type ('k, 'a, _, 'd, _) operation =
  (* transforms *)
  | OpMap : (transform2, 'a, 'a -> 'a, 'd, 'd) operation
  | OpAdd : (transform2, 'a, 'a, 'd, 'd) operation
  | OpFilter : (transform2, 'a, 'a -> bool, 'd, 'd) operation
  (* expand a part into multiple parts. Typically, just join, but different for keys etc *)
  | OpExpand : (transform2, 'a, 'a -> 'a list, 'd, 'd) operation
  (* Perform the given transform before the second operation on the same part *)
  | OpSeq :
      (transform2, 'a, 'f, 'd, 'd) operation * ('k, 'a, 'g, 'd, 'b) operation
      -> ('k, 'a, 'f * 'g, 'd, 'b) operation
  (* context grabs an arbitrary outer part and passes it to an inner operation. 'c part must be on
     the path to 'a part in the structure. Only applies to non-leaf domain parts (like Map.Key,
     Tree.Path, ..., or Self) *)
  | OpContext : 'c part * ('k, 'a, 'f, 'd, 'b) operation -> ('k, 'a, 'c -> 'f, 'd, 'b) operation
  (* Used to reroot the traversal, in conjunction with OpSeq. The operation is applied not to 'a
     part but to 'c part, which must occur structurally under 'a part *)
  | OpNest : 'c part * ('k, 'c, 'f, 'd, 'b) operation -> ('k, 'a, 'f, 'd, 'b) operation
  (* reductions *)
  | OpAcc : (reduce, 'a, 'a -> 'b -> 'b, 'd, 'b) operation
  | OpExists : (reduce, 'a, 'a -> bool, 'd, bool) operation
  (* partitioning *)
  | OpBy : (partition, 'a, 'a -> 'b, 'd, 'b) operation
  | OpByFilter : (partition, 'a, 'a -> 'b option, 'd, 'b) operation

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

  (* Reduce specific parts of composed abstract domains with a given operation. *)
  val reduce : 'a part -> using:(reduce, 'a, 'f, t, 'b) operation -> f:'f -> init:'b -> t -> 'b

  (* Classic reduction using an accumulator. *)
  val fold : 'a part -> f:('a -> 'b -> 'b) -> init:'b -> t -> 'b

  (* Partition the domain according to function. None partitions are dropped *)
  val partition : 'a part -> f:('a -> 'b option) -> t -> ('b, t) Core_kernel.Map.Poly.t

  (* Partition the domain according to function. None partitions are dropped *)
  val partition_new
    :  'a part ->
    (partition, 'a, 'f, t, 'b) operation ->
    f:'f ->
    t ->
    ('b, t) Core_kernel.Map.Poly.t

  val transform : 'a part -> 'a transform -> t -> t

  val transform_new : 'a part -> (transform2, 'a, 'f, t, t) operation -> f:'f -> t -> t

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


let rec op_name : type k a f d b. (k, a, f, d, b) operation -> string =
 fun op ->
  match op with
  | OpMap -> "Map"
  | OpAdd -> "Add"
  | OpFilter -> "Filter"
  | OpExpand -> "Expand"
  (* compositions *)
  | OpContext (p, _) -> Format.sprintf "Context(%s, _)" (part_name p)
  | OpSeq (op1, op2) -> Format.sprintf "Seq(%s, %s)" (op_name op1) (op_name op2)
  | OpNest (p, op) -> Format.sprintf "Nest(%s, %s)" (part_name p) (op_name op)
  (* folds *)
  | OpAcc -> "Acc"
  | OpExists -> "Exists"
  (* Partitions *)
  | OpBy -> "By"
  | OpByFilter -> "ByFilter"


module type BASE = sig
  type t

  val freshen_transform
    :  (transform2, 'a, 'f, 'd, 'd) operation ->
    (transform2, 'a, 'f, 'e, 'e) operation

  val freshen_reduce : (reduce, 'a, 'f, 'd, 'b) operation -> (reduce, 'a, 'f, 'e, 'b) operation

  val freshen_partition
    :  (partition, 'a, 'f, 'd, 'b) operation ->
    (partition, 'a, 'f, 'e, 'b) operation

  val transform : 'a part -> (transform2, 'a, 'f, t, t) operation -> f:'f -> t -> t

  val reduce : 'a part -> using:(reduce, 'a, 'f, t, 'b) operation -> f:'f -> init:'b -> t -> 'b

  val fold : 'a part -> f:('a -> 'b -> 'b) -> init:'b -> t -> 'b

  val partition
    :  'a part ->
    (partition, 'a, 'f, t, 'b) operation ->
    f:'f ->
    t ->
    ('b, t) Core_kernel.Map.Poly.t

  val create : 'a part -> 'a -> t -> t

  val introspect : 'a introspect -> 'a

  val meet : t -> t -> t

  (* legacy api implemented with new code *)
  val legacy_transform : 'a part -> 'a transform -> t -> t

  val legacy_partition : 'a part -> f:('a -> 'b option) -> t -> ('b, t) Core_kernel.Map.Poly.t
end

module MakeBase (D : sig
  type t

  val bottom : t

  val join : t -> t -> t

  val less_or_equal : left:t -> right:t -> bool

  type _ part += Self : t part

  val transform_new : 'a part -> (transform2, 'a, 'f, t, t) operation -> f:'f -> t -> t

  val reduce : 'a part -> using:(reduce, 'a, 'f, t, 'b) operation -> f:'f -> init:'b -> t -> 'b

  val partition_new
    :  'a part ->
    (partition, 'a, 'f, t, 'b) operation ->
    f:'f ->
    t ->
    ('b, t) Core_kernel.Map.Poly.t

  val introspect : 'a introspect -> 'a
end) : BASE with type t := D.t = struct
  let unhandled_part (type a) (part : a part) what =
    let part_name = part_name part in
    Format.sprintf "In %s: unknown part %s in %s" (D.introspect (Name D.Self)) part_name what
    |> failwith


  let unhandled_context (type a) (part : a part) what =
    let part_name = part_name part in
    Format.sprintf "In %s: unknown context %s in %s" (D.introspect (Name D.Self)) part_name what
    |> failwith


  let unhandled_part_op (type a b f d k) (part : a part) (op : (k, a, f, d, b) operation) what =
    let part_name = part_name part in
    let op_name = op_name op in
    Format.sprintf
      "In %s: unknown part %s, op %s in %s"
      (D.introspect (Name D.Self))
      part_name
      op_name
      what
    |> failwith


  let rec freshen_transform
      : type a f d e. (transform2, a, f, d, d) operation -> (transform2, a, f, e, e) operation
    =
   fun op ->
    match op with
    | OpMap -> OpMap
    | OpAdd -> OpAdd
    | OpFilter -> OpFilter
    | OpExpand -> OpExpand
    | OpContext (part, op) -> OpContext (part, freshen_transform op)
    | OpSeq (op1, op2) -> OpSeq (freshen_transform op1, freshen_transform op2)
    | OpNest (p, op) -> OpNest (p, freshen_transform op)


  let rec freshen_reduce
      : type a b f d e. (reduce, a, f, d, b) operation -> (reduce, a, f, e, b) operation
    =
   fun op ->
    match op with
    | OpAcc -> OpAcc
    | OpExists -> OpExists
    | OpContext (part, op) -> OpContext (part, freshen_reduce op)
    | OpSeq (op1, op2) -> OpSeq (freshen_transform op1, freshen_reduce op2)
    | OpNest (p, op) -> OpNest (p, freshen_reduce op)


  let rec freshen_partition
      : type a b f d e. (partition, a, f, d, b) operation -> (partition, a, f, e, b) operation
    =
   fun op ->
    match op with
    | OpBy -> OpBy
    | OpByFilter -> OpByFilter
    | OpContext (part, op) -> OpContext (part, freshen_partition op)
    | OpSeq (op1, op2) -> OpSeq (freshen_transform op1, freshen_partition op2)
    | OpNest (p, op) -> OpNest (p, freshen_partition op)


  let transform : type a f. a part -> (transform2, a, f, D.t, D.t) operation -> f:f -> D.t -> D.t =
   fun part op ~f d ->
    match part, op with
    | D.Self, OpMap -> f d
    | D.Self, OpAdd -> D.join d f
    | D.Self, OpFilter -> if f d then d else D.bottom
    | D.Self, OpExpand -> f d |> Core_kernel.List.fold ~f:D.join ~init:D.bottom
    | _, OpSeq (op1, op2) ->
        let f1, f2 = f in
        let d = D.transform_new part op1 ~f:f1 d in
        D.transform_new part op2 ~f:f2 d
    | _, OpNest (nested_part, op) -> D.transform_new nested_part op ~f d
    | _, OpContext (D.Self, op) -> D.transform_new part op ~f:(f d) d
    | _, OpContext (p, _) -> unhandled_context p "transform"
    | _ -> unhandled_part_op part op "transform"


  let reduce
      : type a b f. a part -> using:(reduce, a, f, D.t, b) operation -> f:f -> init:b -> D.t -> b
    =
   fun part ~using:op ~f ~init d ->
    match part, op with
    | D.Self, OpAcc -> f d init
    | D.Self, OpExists -> init || f d
    | _, OpSeq (op1, op2) ->
        let f1, f2 = f in
        let d = D.transform_new part op1 ~f:f1 d in
        D.reduce part ~using:op2 ~f:f2 ~init d
    | _, OpNest (nested_part, op) -> D.reduce nested_part ~using:op ~f ~init d
    | _, OpContext (D.Self, op) -> D.reduce part ~using:op ~f:(f d) ~init d
    | _, OpContext (p, _) -> unhandled_context p "reduce"
    | _, op -> unhandled_part_op part op "reduce"


  let fold = D.reduce ~using:OpAcc

  let partition
      : type a b f.
        a part ->
        (partition, a, f, D.t, b) operation ->
        f:f ->
        D.t ->
        (b, D.t) Core_kernel.Map.Poly.t
    =
   fun part op ~f d ->
    match part, op with
    | D.Self, OpBy -> Core_kernel.Map.Poly.singleton (f d) d
    | D.Self, OpByFilter -> (
        match f d with
        | None -> Core_kernel.Map.Poly.empty
        | Some key -> Core_kernel.Map.Poly.singleton key d )
    | _, OpSeq (op1, op2) ->
        let f1, f2 = f in
        let d = D.transform_new part op1 ~f:f1 d in
        D.partition_new part (freshen_partition op2) ~f:f2 d
    | _, OpNest (nested_part, op) -> D.partition_new nested_part op ~f d
    | _, OpContext (D.Self, op) -> D.partition_new part op ~f:(f d) d
    | _, OpContext (p, _) -> unhandled_context p "partition"
    | _ -> unhandled_part_op part op "partition"


  let create (type a) (part : a part) (value : a) (sofar : D.t) : D.t =
    match part with
    | D.Self -> D.join sofar value
    | _ -> unhandled_part part "create"


  let unhandled_introspect (type a) (op : a introspect) =
    match op with
    | GetParts _ -> Format.sprintf "Unhandled GetParts introspect" |> failwith
    | Name part ->
        let part_name = part_name part in
        Format.sprintf "Unhandled Name(%s) introspect" part_name |> failwith
    | Structure -> Format.sprintf "Unhandled Structure introspect" |> failwith


  let introspect (type a) (op : a introspect) : a = unhandled_introspect op

  (* default meet, returns the smaller argument if determinable, otherwise the first argument *)
  let meet a b = if D.less_or_equal ~left:b ~right:a then b else a

  let rec adapt_transform
      : type f. (transform2, D.t, f, D.t, D.t) operation -> f:f -> transformer list -> D.t -> D.t
    =
   fun op ~f tl d ->
    match tl with
    | [] -> D.transform_new D.Self op ~f d
    | T (part, t) :: rest -> (
        match t with
        | Map g ->
            let op = OpSeq (op, OpNest (part, OpMap)) in
            let f = f, g in
            adapt_transform op ~f rest d
        | Add a ->
            let op = OpSeq (op, OpNest (part, OpAdd)) in
            let f = f, a in
            adapt_transform op ~f rest d
        | Filter g ->
            let op = OpSeq (op, OpNest (part, OpFilter)) in
            let f = f, g in
            adapt_transform op ~f rest d
        | Expand g ->
            let op = OpSeq (op, OpNest (part, OpExpand)) in
            let f = f, g in
            adapt_transform op ~f rest d
        | Multi tl -> adapt_transform op ~f (tl @ rest) d )


  let legacy_transform part t d = adapt_transform OpAdd ~f:D.bottom [T (part, t)] d

  let legacy_partition part ~f d = D.partition_new part OpByFilter ~f d
end
