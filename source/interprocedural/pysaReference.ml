(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* PysaReference defines a type alias for string that lets us include custom
 * logic, such as serializable maps, for cases where Pysa uses strings as
 * identifiers of entities in code. *)
open Core

module T = struct
  type t = string [@@deriving compare, sexp, hash, to_yojson]
end

include T
module Map = Map.Make (T)
