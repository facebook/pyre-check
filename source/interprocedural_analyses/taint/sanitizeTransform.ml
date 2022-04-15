(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module T = struct
  type t =
    | NamedSource of string
    | NamedSink of string
  [@@deriving compare, eq, hash, sexp]

  let pp formatter = function
    | NamedSource source -> Format.fprintf formatter "NotSource[%s]" source
    | NamedSink sink -> Format.fprintf formatter "NotSink[%s]" sink


  let show = Format.asprintf "%a" pp
end

include T

module Set = struct
  include Data_structures.SerializableSet.Make (T)

  let filter_sources =
    filter (function
        | NamedSource _ -> true
        | _ -> false)


  let filter_sinks =
    filter (function
        | NamedSink _ -> true
        | _ -> false)
end
