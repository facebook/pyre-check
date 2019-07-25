(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

type scope =
  | Local
  | Global

and immutable = {
  scope: scope;
  original: Type.t;
  final: bool;
}

and mutability =
  | Mutable
  | Immutable of immutable

and t = {
  annotation: Type.t;
  mutability: mutability;
}
[@@deriving compare, eq, show, hash, sexp]

let pp format { annotation; mutability } =
  let mutability =
    match mutability with
    | Mutable -> "m"
    | Immutable { scope; original; final } ->
        let scope =
          match scope with
          | Local -> "l"
          | Global -> "g"
        in
        let final =
          match final with
          | true -> " (final)"
          | _ -> ""
        in
        Format.asprintf "%s (%a)%s" scope Type.pp original final
  in
  Format.fprintf format "(%a: %s)" Type.pp annotation mutability


let _ = show (* Ignore unused generated show *)

let show = Format.asprintf "%a" pp

let create ?(mutability = Mutable) annotation = { annotation; mutability }

let create_immutable ~global ?(original = None) ?(final = false) annotation =
  let scope = if global then Global else Local in
  let original = Option.value ~default:annotation original in
  { annotation; mutability = Immutable { scope; original; final } }


let annotation { annotation; _ } = annotation

let mutability { mutability; _ } = mutability

let scope { mutability; _ } =
  match mutability with
  | Immutable { scope; _ } -> Some scope
  | _ -> None


let original { annotation; mutability } =
  match mutability with
  | Immutable { original; _ } -> original
  | Mutable -> annotation


let is_global annotation =
  match scope annotation with
  | Some Global -> true
  | _ -> false


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
    | Immutable { scope; original; _ } ->
        Immutable { scope; original = instantiate original; final = false }
  in
  { annotation = instantiate annotation; mutability }


let dequalify dequalify_map { annotation; mutability } =
  let mutability =
    match mutability with
    | Mutable -> Mutable
    | Immutable { scope; original; _ } ->
        Immutable { scope; original = Type.dequalify dequalify_map original; final = false }
  in
  { annotation = Type.dequalify dequalify_map annotation; mutability }
