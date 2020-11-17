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
  [@@deriving compare, eq, sexp, show, hash]
end

include T

let _ = show (* unused but derived *)

let show = function
  | Attach -> "Attach"
  | PartialSink { kind; label } -> Format.sprintf "PartialSink[%s[%s]]" kind label
  | TriggeredPartialSink { kind; label } -> Format.sprintf "TriggeredPartialSink[%s[%s]]" kind label
  | LocalReturn -> "LocalReturn"
  | NamedSink name -> name
  | ParametricSink { sink_name; subkind } -> Format.sprintf "%s[%s]" sink_name subkind
  | ParameterUpdate index -> Format.sprintf "ParameterUpdate%d" index
  | AddFeatureToArgument -> "AddFeatureToArgument"


let ignore_leaf_at_call = function
  | Attach -> true
  | _ -> false


module Set = Set.Make (struct
  include T
end)

let name = "sink"
