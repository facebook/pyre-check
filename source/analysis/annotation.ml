(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type immutable = {
  original: Type.t;
  final: bool;
}
[@@deriving compare, eq, hash, sexp]

type mutability =
  | Mutable
  | Immutable of immutable
[@@deriving compare, eq, hash, sexp]

let pp_mutability format = function
  | Mutable -> Format.fprintf format "m"
  | Immutable { original; final } ->
      let final =
        match final with
        | true -> " (final)"
        | _ -> ""
      in
      Format.fprintf format " (%a)%s" Type.pp original final


type t = {
  annotation: Type.t;
  mutability: mutability;
}
[@@deriving compare, eq, hash, sexp]

let pp format { annotation; mutability } =
  Format.fprintf format "(%a: %a)" Type.pp annotation pp_mutability mutability


let show = Format.asprintf "%a" pp

let create ?(mutability = Mutable) annotation = { annotation; mutability }

let create_immutable ?(original = None) ?(final = false) annotation =
  let original = Option.value ~default:annotation original in
  { annotation; mutability = Immutable { original; final } }


let annotation { annotation; _ } = annotation

let mutability { mutability; _ } = mutability

let original { annotation; mutability } =
  match mutability with
  | Immutable { original; _ } -> original
  | Mutable -> annotation


let is_immutable { mutability; _ } = not (equal_mutability mutability Mutable)

let is_final { mutability; _ } =
  match mutability with
  | Immutable { final; _ } -> final
  | Mutable -> false


let instantiate { annotation; mutability } ~constraints =
  let instantiate = Type.instantiate ~constraints in
  let mutability =
    match mutability with
    | Mutable -> Mutable
    | Immutable { original; final } -> Immutable { original = instantiate original; final }
  in
  { annotation = instantiate annotation; mutability }


let dequalify dequalify_map { annotation; mutability } =
  let mutability =
    match mutability with
    | Mutable -> Mutable
    | Immutable { original; final } ->
        Immutable { original = Type.dequalify dequalify_map original; final }
  in
  { annotation = Type.dequalify dequalify_map annotation; mutability }
