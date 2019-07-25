(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Sexplib.Std

type 'expression parameter = {
  name: Identifier.t;
  value: 'expression option;
  annotation: 'expression option;
}
[@@deriving compare, eq, sexp, show, hash, to_yojson]

type 'expression t = 'expression parameter Node.t
[@@deriving compare, eq, sexp, show, hash, to_yojson]

let create ~location ?value ?annotation ~name () =
  { Node.location; value = { name; value; annotation } }


let name { Node.value = { name; _ }; _ } = name
