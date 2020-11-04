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
  [@@deriving compare, eq, sexp, show, hash]
end

include T

let _ = show (* unused *)

let show = function
  | Attach -> "Attach"
  | NamedSource name -> name
  | ParametricSource { source_name; subkind } -> Format.sprintf "%s[%s]" source_name subkind


let create = function
  | name -> failwith (Format.sprintf "Unsupported taint source `%s`" name)


let ignore_leaf_at_call = function
  | Attach -> true
  | _ -> false


let parse ~allowed ?subkind name =
  if List.mem allowed name ~equal:String.equal then
    match subkind with
    | Some subkind -> ParametricSource { source_name = name; subkind }
    | None -> NamedSource name
  else
    create name


module Set = Set.Make (struct
  include T
end)

let name = "source"
