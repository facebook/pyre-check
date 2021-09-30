(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type t =
  | SanitizeNamedSource of string
  | SanitizeNamedSink of string
[@@deriving compare, eq, sexp, hash]

let pp formatter = function
  | SanitizeNamedSource source -> Format.fprintf formatter "NotSource[%s]" source
  | SanitizeNamedSink sink -> Format.fprintf formatter "NotSink[%s]" sink


let show = Format.asprintf "%a" pp

let show_transforms transforms = List.map ~f:show transforms |> String.concat ~sep:":"

let pp_transforms formatter transforms = Format.fprintf formatter "%s" (show_transforms transforms)

let pp_kind ~formatter ~pp_base ~local ~global ~base =
  match local, global with
  | [], _ -> Format.fprintf formatter "%a:%a" pp_transforms global pp_base base
  | _, [] -> Format.fprintf formatter "%a@@%a" pp_transforms local pp_base base
  | _ -> Format.fprintf formatter "%a@@%a:%a" pp_transforms local pp_transforms global pp_base base


let filter_sanitized_sources =
  List.filter ~f:(function
      | SanitizeNamedSource _ -> true
      | _ -> false)


let filter_sanitized_sinks =
  List.filter ~f:(function
      | SanitizeNamedSink _ -> true
      | _ -> false)
