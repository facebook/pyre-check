(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open AbstractDomain


module type CONFIG = sig
  val route: 'a part -> [ `Left | `Right | `Both ]
end


module Make
    (Config : CONFIG)
    (Left : AbstractDomain.S)
    (Right : AbstractDomain.S) =
struct
  type t = Left.t * Right.t
  [@@deriving sexp]

  let bottom = Left.bottom, Right.bottom
  let is_bottom (l, r) =
    Left.is_bottom l && Right.is_bottom r

  let join (l1, r1) (l2, r2) =
    Left.join l1 l2, Right.join r1 r2

  let widen ~iteration ~previous:(prev_l, prev_r) ~next:(next_l, next_r) =
    Left.widen ~iteration ~previous:prev_l ~next:next_l,
    Right.widen ~iteration ~previous:prev_r ~next:next_r

  let less_or_equal ~left:(l1, r1) ~right:(l2, r2) =
    Left.less_or_equal ~left:l1 ~right:l2 &&
    Right.less_or_equal ~left:r1 ~right:r2

  let show (left, right) =
    Format.sprintf "(%s, %s)" (Left.show left) (Right.show right)

  let make l r =
    (l, r)

  let get pair =
    pair

  let fold (type a b) (part: a part) ~(f: b -> a -> b) ~(init: b) ((left, right): t) : b =
    match Config.route part with
    | `Left ->
        Left.fold part ~f ~init left
    | `Right ->
        Right.fold part ~f ~init right
    | `Both ->
        let left_fold = Left.fold part ~f ~init left in
        Right.fold part ~f ~init:left_fold right

  let transform (type a) (part: a part) ~(f: a -> a) ((left, right): t) : t =
    match Config.route part with
    | `Left ->
        make (Left.transform part ~f left) right
    | `Right ->
        make left (Right.transform part ~f right)
    | `Both ->
        make (Left.transform part ~f left) (Right.transform part ~f right)

  let partition (type a b) (part: a part) ~(f: a -> b) ((left, right): t)
    : (b, t) Map.Poly.t =
    match Config.route part with
    | `Left ->
        Left.partition part ~f left
        |> Map.Poly.map ~f:(fun left -> make left right)
    | `Right ->
        Right.partition part ~f right
        |> Map.Poly.map ~f:(fun right -> make left right)
    | `Both ->
        let left_partition = Left.partition part ~f left in
        let right_partition = Right.partition part ~f right in
        let combine ~key:_ = function
          | `Both (left, right) -> Some (make left right)
          | `Left left -> Some (make left Right.bottom)
          | `Right right -> Some (make Left.bottom right)
        in
        Map.Poly.merge left_partition right_partition ~f:combine
end
