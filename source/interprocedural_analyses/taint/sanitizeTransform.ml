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
  include Stdlib.Set.Make (T)

  let t_of_sexp set = [%of_sexp: T.t list] set |> of_list

  let sexp_of_t set = set |> elements |> [%sexp_of: T.t list]

  let hash_fold_t state set = set |> elements |> [%hash_fold: T.t list] state

  let hash set = set |> elements |> [%hash: T.t list]

  let show set =
    set |> elements |> List.map ~f:T.show |> String.concat ~sep:", " |> Format.asprintf "{%s}"


  let pp format set = Format.fprintf format "%s" (show set)

  let filter_sources =
    filter (function
        | NamedSource _ -> true
        | _ -> false)


  let filter_sinks =
    filter (function
        | NamedSink _ -> true
        | _ -> false)
end
