(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

type 'expression parameter = {
  name: Identifier.t;
  value: 'expression option;
  annotation: 'expression option;
}
[@@deriving compare, eq, sexp, show, hash, to_yojson]

type 'expression t = 'expression parameter Node.t
[@@deriving compare, eq, sexp, show, hash, to_yojson]

val create
  :  location:Location.t ->
  ?value:'expression ->
  ?annotation:'expression ->
  name:Identifier.t ->
  unit ->
  'expression t

val name : 'expression t -> Identifier.t
