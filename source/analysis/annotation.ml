(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module Mutability = struct
  type immutable = {
    original: Type.t;
    final: bool;
  }
  [@@deriving compare, eq, hash, sexp]

  type t =
    | Mutable
    | Immutable of immutable
  [@@deriving compare, eq, hash, sexp]

  let pp format = function
    | Mutable -> Format.fprintf format "m"
    | Immutable { original; final } ->
        let final =
          match final with
          | true -> " (final)"
          | _ -> ""
        in
        Format.fprintf format " (%a)%s" Type.pp original final


  let transform_types ~f = function
    | Mutable -> Mutable
    | Immutable { original; final } -> Immutable { original = f original; final }


  let less_or_equal ~left ~right =
    match left, right with
    | Mutable, _
    (* we don't have to look at original or final because they will be the same *)
    | Immutable _, Immutable _ ->
        true
    | Immutable _, Mutable -> false


  let join ~type_join left right =
    match left, right with
    | Immutable left, Immutable right ->
        Immutable
          { original = type_join left.original right.original; final = left.final || right.final }
    | (Immutable _ as immutable), Mutable
    | Mutable, (Immutable _ as immutable) ->
        immutable
    | Mutable, Mutable -> Mutable


  let meet ~type_meet left right =
    match left, right with
    | Mutable, _
    | _, Mutable ->
        Mutable
    | Immutable left, Immutable right ->
        Immutable
          { original = type_meet left.original right.original; final = left.final && right.final }
end

type t = {
  annotation: Type.t;
  mutability: Mutability.t;
}
[@@deriving compare, eq, hash, sexp]

let pp format { annotation; mutability } =
  Format.fprintf format "(%a: %a)" Type.pp annotation Mutability.pp mutability


let show = Format.asprintf "%a" pp

let display_as_revealed_type { annotation; mutability } =
  match mutability with
  | Mutable -> Format.asprintf "`%a`" Type.pp annotation
  | Immutable { original; final; _ } ->
      let if_final display = if final then display else "" in
      if Type.contains_unknown original then
        Format.asprintf "`%a`%s" Type.pp annotation (if_final " (final)")
      else if Type.equal annotation original then
        Format.asprintf "`%a`%s" Type.pp original (if_final " (final)")
      else
        Format.asprintf
          "`%a` (inferred: `%a`%s)"
          Type.pp
          original
          Type.pp
          annotation
          (if_final ", final")


let create_mutable annotation = { annotation; mutability = Mutable }

let create_immutable ?(original = None) ?(final = false) annotation =
  let original = Option.value ~default:annotation original in
  { annotation; mutability = Immutable { original; final } }


let annotation { annotation; _ } = annotation

let original { annotation; mutability } =
  match mutability with
  | Immutable { original; _ } -> original
  | Mutable -> annotation


let is_immutable { mutability; _ } = not (Mutability.equal mutability Mutable)

let is_final { mutability; _ } =
  match mutability with
  | Immutable { final; _ } -> final
  | Mutable -> false


let transform_types ~f { annotation; mutability } =
  { annotation = f annotation; mutability = Mutability.transform_types ~f mutability }


let instantiate annotation ~constraints =
  let instantiate = Type.instantiate ~constraints in
  transform_types ~f:instantiate annotation


let dequalify dequalify_map annotation =
  let dequalify = Type.dequalify dequalify_map in
  transform_types ~f:dequalify annotation


let less_or_equal ~type_less_or_equal ~left ~right =
  Mutability.less_or_equal ~left:left.mutability ~right:right.mutability
  && type_less_or_equal ~left:left.annotation ~right:right.annotation


let join ~type_join left right =
  {
    annotation = type_join left.annotation right.annotation;
    mutability = Mutability.join ~type_join left.mutability right.mutability;
  }


let meet ~type_meet left right =
  {
    annotation = type_meet left.annotation right.annotation;
    mutability = Mutability.meet ~type_meet left.mutability right.mutability;
  }


let refine ~type_less_or_equal ~solve_less_or_equal ~refined_type { annotation; mutability } =
  let accept_refinement_of_immutable original =
    (not (Type.is_unbound refined_type)) && type_less_or_equal ~left:refined_type ~right:original
  in
  match mutability with
  | Mutable -> { annotation = refined_type; mutability }
  | Immutable { original; _ } -> (
      match refined_type with
      | Type.Top -> { annotation = Type.Top; mutability }
      | Type.Bottom -> { annotation; mutability }
      | _ when accept_refinement_of_immutable original -> { annotation = refined_type; mutability }
      | _ ->
          {
            annotation =
              solve_less_or_equal ~left:refined_type ~right:original
              |> Option.value ~default:annotation;
            mutability;
          })
