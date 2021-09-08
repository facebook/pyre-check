(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module T = struct
  type partial_sink = {
    kind: string;
    label: string;
  }
  [@@deriving compare, eq, sexp, show, hash]

  type t =
    | Attach
    | PartialSink of partial_sink
    | TriggeredPartialSink of partial_sink
    | LocalReturn (* Special marker to describe function in-out behavior *)
    | NamedSink of string
    | ParametricSink of {
        sink_name: string;
        subkind: string;
      }
    | ParameterUpdate of int (* Special marker to describe side effect in-out behavior *)
    | AddFeatureToArgument
  (* Special marker to designate modifying the state the parameter passed in. *)
  [@@deriving compare, eq, sexp, hash]

  let pp formatter = function
    | Attach -> Format.fprintf formatter "Attach"
    | PartialSink { kind; label } -> Format.fprintf formatter "PartialSink[%s[%s]]" kind label
    | TriggeredPartialSink { kind; label } ->
        Format.fprintf formatter "TriggeredPartialSink[%s[%s]]" kind label
    | LocalReturn -> Format.fprintf formatter "LocalReturn"
    | NamedSink name -> Format.fprintf formatter "%s" name
    | ParametricSink { sink_name; subkind } -> Format.fprintf formatter "%s[%s]" sink_name subkind
    | ParameterUpdate index -> Format.fprintf formatter "ParameterUpdate%d" index
    | AddFeatureToArgument -> Format.fprintf formatter "AddFeatureToArgument"


  let show = Format.asprintf "%a" pp
end

include T

let ignore_kind_at_call = function
  | Attach -> true
  | _ -> false


module Set = struct
  include Stdlib.Set.Make (struct
    include T
  end)

  let show set =
    set |> elements |> List.map ~f:T.show |> String.concat ~sep:", " |> Format.asprintf "[%s]"


  let pp format set = Format.fprintf format "%s" (show set)
end

let name = "sink"
