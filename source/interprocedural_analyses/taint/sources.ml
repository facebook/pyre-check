(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module T = struct
  type t =
    | Attach
    | NamedSource of string
    | ParametricSource of {
        source_name: string;
        subkind: string;
      }
  [@@deriving compare, eq, sexp, hash]
end

include T

let pp formatter = function
  | Attach -> Format.fprintf formatter "Attach"
  | NamedSource name -> Format.fprintf formatter "%s" name
  | ParametricSource { source_name; subkind } ->
      Format.fprintf formatter "%s[%s]" source_name subkind


let show = Format.asprintf "%a" pp

let ignore_leaf_at_call = function
  | Attach -> true
  | _ -> false


module Set = Set.Make (struct
  include T
end)

let name = "source"
