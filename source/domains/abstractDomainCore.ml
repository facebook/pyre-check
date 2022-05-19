(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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

type ('k, 'a, _, _) operation =
  (* transforms *)
  | Map : ([< `Transform ], 'a, 'a -> 'a, _) operation
  | Add : ([< `Transform ], 'a, 'a, _) operation
  | Filter : ([ `Transform ], 'a, 'a -> bool, _) operation
  | FilterMap : ([ `Transform ], 'a, 'a -> 'a option, _) operation
  (* expand a part into multiple parts. Typically, just join, but different for keys etc *)
  | Expand : ([ `Transform ], 'a, 'a -> 'a list, _) operation
  (* Perform two operations of the same type (Reduce or Transform) in sequence. *)
  | Seq :
      (([< `Transform | `Reduce ] as 'k), 'a, 'f, 'r) operation * ('k, 'a, 'g, 'r) operation
      -> ('k, 'a, 'f * 'g, 'r) operation
  (* Perform two operations in sequence when the first is Reduce and the second is Transform. Result
     of Reduce will be passed to Transform operation. *)
  | ReduceTransform :
      ((([ `Reduce ] as 'k1), 'a, 'f, 'r) operation * 'r)
      * (([ `Transform ] as 'k2), 'a, 'g, 'r) operation
      -> ('k2, 'a, 'f * ('r -> 'g), 'r) operation
  (* Perform two operations in sequence when th first is Transform and the second is Reduce. *)
  | TransformReduce :
      (([ `Transform ] as 'k1), 'a, 'f, 'r) operation * (([ `Reduce ] as 'k2), 'a, 'g, 'r) operation
      -> ('k2, 'a, 'f * 'g, 'r) operation
  (* context grabs an arbitrary outer part and passes it to an inner operation. 'c part must be on
     the path to 'a part in the structure. Only applies to non-leaf domain parts (like Map.Key,
     Tree.Path, ..., or Self) *)
  | Context : 'c part * ('k, 'a, 'f, 'r) operation -> ('k, 'a, 'c -> 'f, 'r) operation
  (* Used to reroot the traversal, in conjunction with OpSeq. The operation is applied not to 'a
     part but to 'c part, which must occur structurally under 'a part *)
  | Nest : 'c part * ('k, 'c, 'f, 'r) operation -> ('k, 'a, 'f, 'r) operation
  (* reductions *)
  | Acc : ([< `Reduce ], 'a, 'a -> 'b -> 'b, 'b) operation
  | Exists : ([< `Reduce ], 'a, 'a -> bool, bool) operation
  (* partitioning *)
  | By : ([ `Partition ], 'a, 'a -> 'b, 'b) operation
  | ByFilter : ([ `Partition ], 'a, 'a -> 'b option, 'b) operation

(* datatype to encapsulate a transform operation *)
type transform =
  | T : {
      part: 'a part;
      op: ([ `Transform ], 'a, 'f, _) operation;
      f: 'f;
    }
      -> transform
  | TSeq of transform list
  | TNone : transform

module type S = sig
  type t [@@deriving show]

  type _ part += Self : t part

  val bottom : t

  val is_bottom : t -> bool

  val join : t -> t -> t

  (* meet a b is an over-approximation of the intersection of 'a' and 'b' *)
  val meet : t -> t -> t

  val less_or_equal : left:t -> right:t -> bool

  val widen : iteration:int -> prev:t -> next:t -> t

  (* subtract a from = b, s.t. b <= from and from <= b |_| a *)
  val subtract : t -> from:t -> t

  (* Reduce specific parts of composed abstract domains with a given operation. *)
  val reduce : 'a part -> using:([ `Reduce ], 'a, 'f, 'b) operation -> f:'f -> init:'b -> t -> 'b

  (* Classic reduction using an accumulator. *)
  val fold : 'a part -> f:('a -> 'b -> 'b) -> init:'b -> t -> 'b

  (* Partition the domain according to function. None partitions are dropped *)
  val partition
    :  'a part ->
    ([ `Partition ], 'a, 'f, 'b) operation ->
    f:'f ->
    t ->
    ('b, t) Core_kernel.Map.Poly.t

  val transform : 'a part -> ([ `Transform ], 'a, 'f, _) operation -> f:'f -> t -> t

  (* Return insights about the abstract domain structure *)
  val introspect : 'a introspect -> 'a

  (* Create an abstract value based on a part list. *)
  val create : value_part list -> t

  (* A way to apply transforms as data *)
  val apply : transform -> t -> t
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


let rec operation_name : type k a f b. (k, a, f, b) operation -> string =
 fun op ->
  match op with
  | Map -> "Map"
  | Add -> "Add"
  | Filter -> "Filter"
  | FilterMap -> "FilterMap"
  | Expand -> "Expand"
  (* compositions *)
  | Context (p, _) -> Format.sprintf "Context(%s, _)" (part_name p)
  | Seq (op1, op2) -> Format.sprintf "Seq(%s, %s)" (operation_name op1) (operation_name op2)
  | ReduceTransform ((op1, _), op2) ->
      Format.sprintf "ReduceTransform(%s, %s)" (operation_name op1) (operation_name op2)
  | TransformReduce (op1, op2) ->
      Format.sprintf "TransformReduce(%s, %s)" (operation_name op1) (operation_name op2)
  | Nest (p, op) -> Format.sprintf "Nest(%s, %s)" (part_name p) (operation_name op)
  (* folds *)
  | Acc -> "Acc"
  | Exists -> "Exists"
  (* Partitions *)
  | By -> "By"
  | ByFilter -> "ByFilter"


module type BASE = sig
  type t

  val transform : 'a part -> ([ `Transform ], 'a, 'f, _) operation -> f:'f -> t -> t

  val reduce : 'a part -> using:([ `Reduce ], 'a, 'f, 'b) operation -> f:'f -> init:'b -> t -> 'b

  val fold : 'a part -> f:('a -> 'b -> 'b) -> init:'b -> t -> 'b

  val partition
    :  'a part ->
    ([ `Partition ], 'a, 'f, 'b) operation ->
    f:'f ->
    t ->
    ('b, t) Core_kernel.Map.Poly.t

  val create : 'a part -> 'a -> t -> t

  val introspect : 'a introspect -> 'a

  val meet : t -> t -> t

  val apply : transform -> t -> t
end

module MakeBase (D : sig
  type t

  val bottom : t

  val join : t -> t -> t

  val less_or_equal : left:t -> right:t -> bool

  type _ part += Self : t part

  val transform : 'a part -> ([ `Transform ], 'a, 'f, _) operation -> f:'f -> t -> t

  val reduce : 'a part -> using:([ `Reduce ], 'a, 'f, 'b) operation -> f:'f -> init:'b -> t -> 'b

  val partition
    :  'a part ->
    ([ `Partition ], 'a, 'f, 'b) operation ->
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


  let unhandled_part_op (type a b f k) (part : a part) (op : (k, a, f, b) operation) what =
    let part_name = part_name part in
    let op_name = operation_name op in
    Format.sprintf
      "In %s: unknown part %s, op %s in %s"
      (D.introspect (Name D.Self))
      part_name
      op_name
      what
    |> failwith


  let transform : type a b f. a part -> ([ `Transform ], a, f, b) operation -> f:f -> D.t -> D.t =
   fun part op ~f d ->
    match part, op with
    | D.Self, Map -> f d
    | D.Self, Add -> D.join d f
    | D.Self, Filter -> if f d then d else D.bottom
    | D.Self, FilterMap -> f d |> Option.value ~default:D.bottom
    | D.Self, Expand -> f d |> Core_kernel.List.fold ~f:D.join ~init:D.bottom
    | _, Seq (op1, op2) ->
        let f1, f2 = f in
        let d = D.transform part op1 ~f:f1 d in
        D.transform part op2 ~f:f2 d
    | _, ReduceTransform ((op1, init), op2) ->
        let f1, f2 = f in
        let r = D.reduce part ~using:op1 ~f:f1 ~init d in
        D.transform part op2 ~f:(f2 r) d
    | _, Nest (nested_part, op) -> D.transform nested_part op ~f d
    | _, Context (D.Self, op) -> D.transform part op ~f:(f d) d
    | _, Context (p, _) -> unhandled_context p "transform"
    | _ -> unhandled_part_op part op "transform"


  let reduce
      : type a b f. a part -> using:([ `Reduce ], a, f, b) operation -> f:f -> init:b -> D.t -> b
    =
   fun part ~using:op ~f ~init d ->
    match part, op with
    | D.Self, Acc -> f d init
    | D.Self, Exists -> init || f d
    | _, Seq (op1, op2) ->
        let f1, f2 = f in
        let init = D.reduce part ~using:op1 ~f:f1 ~init d in
        D.reduce part ~using:op2 ~f:f2 ~init d
    | _, TransformReduce (op1, op2) ->
        let f1, f2 = f in
        let d = D.transform part op1 ~f:f1 d in
        D.reduce part ~using:op2 ~f:f2 ~init d
    | _, Nest (nested_part, op) -> D.reduce nested_part ~using:op ~f ~init d
    | _, Context (D.Self, op) -> D.reduce part ~using:op ~f:(f d) ~init d
    | _, Context (p, _) -> unhandled_context p "reduce"
    | _, op -> unhandled_part_op part op "reduce"


  let fold = D.reduce ~using:Acc

  let partition
      : type a b f.
        a part ->
        ([ `Partition ], a, f, b) operation ->
        f:f ->
        D.t ->
        (b, D.t) Core_kernel.Map.Poly.t
    =
   fun part op ~f d ->
    match part, op with
    | D.Self, By -> Core_kernel.Map.Poly.singleton (f d) d
    | D.Self, ByFilter -> (
        match f d with
        | None -> Core_kernel.Map.Poly.empty
        | Some key -> Core_kernel.Map.Poly.singleton key d)
    | _, Nest (nested_part, op) -> D.partition nested_part op ~f d
    | _, Context (D.Self, op) -> D.partition part op ~f:(f d) d
    | _, Context (p, _) -> unhandled_context p "partition"
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

  let rec apply t v =
    match t with
    | TNone -> v
    | T { part; op; f } -> D.transform D.Self (Nest (part, op)) ~f v
    | TSeq ts -> List.fold_left (fun v t -> apply t v) v ts
end
