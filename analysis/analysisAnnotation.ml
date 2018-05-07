(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

module Type = AnalysisType

type scope = | Local | Global


and immutable = {
  scope: scope;
  original: Type.t;
}


and mutability =
  | Mutable
  | Immutable of immutable


and t = {
  annotation: Type.t;
  mutability: mutability;
}
[@@deriving eq, show, hash]


let pp format { annotation; mutability } =
  let mutability =
    match mutability with
    | Mutable -> "m"
    | Immutable { scope; original } ->
        let scope =
          match scope with
          | Local -> "l"
          | Global -> "g"
        in
        Format.asprintf "%s (%a)" scope Type.pp original
  in
  Format.fprintf format "(%a: %s)" Type.pp annotation mutability


let create ?(mutability = Mutable) annotation =
  { annotation; mutability }


let create_immutable ~global ?(original = None) annotation =
  let scope = if global then Global else Local in
  let original = Option.value ~default:annotation original in
  { annotation; mutability = Immutable { scope; original }}


let annotation { annotation; _ } =
  annotation


let mutability { mutability; _ } =
  mutability


let original { annotation; mutability; _ } =
  match mutability with
  | Immutable { original; _ } -> original
  | Mutable -> annotation


let is_immutable { mutability; _ } =
  mutability <> Mutable


let instantiate { annotation; mutability } ~constraints =
  let instantiate = Type.instantiate ~constraints:(Map.find constraints) in
  let mutability =
    match mutability with
    | Mutable -> Mutable
    | Immutable { scope; original } -> Immutable { scope; original = instantiate original }
  in
  { annotation = instantiate annotation; mutability }
