(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

type t = private
  | TopLevel
  | Function of {
      parent: t;
      name: string;
    }
  | Class of {
      parent: t;
      name: string;
    }
[@@deriving sexp, compare, equal, hash, show, to_yojson]

val create_toplevel : unit -> t

val create_function : parent:t -> string -> t

val create_class : parent:t -> string -> t

val is_toplevel : t -> bool

val is_function : t -> bool

val is_class : t -> bool

val to_qualifier : module_name:Reference.t -> t -> Reference.t
