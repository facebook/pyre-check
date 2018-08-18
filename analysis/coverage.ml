(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast


type t = {
  full: int;
  partial: int;
  untyped: int;
  ignore: int;
  crashes: int;
}
[@@deriving show]


let create ?(full = 0) ?(partial = 0) ?(untyped = 0) ?(ignore = 0) ?(crashes = 0) () =
  { full; partial; untyped; ignore; crashes }


let full { full; _ } =
  full


let partial { partial; _ } =
  partial


let untyped { untyped; _ } =
  untyped


let ignore { ignore; _ } =
  ignore


let crashes { crashes; _ } =
  crashes


let sum left right =
  {
    full = full left + full right;
    partial = partial left + partial right;
    untyped = untyped left + untyped right;
    ignore = ignore left + ignore right;
    crashes = crashes left + crashes right;
  }


let aggregate annotations =
  let aggregate
      ({ full; partial; untyped; _ } as coverage)
      { Annotation.annotation; _ } =
    if Type.is_untyped annotation then
      { coverage with untyped = untyped + 1 }
    else if Type.is_partially_typed annotation then
      { coverage with partial = partial + 1 }
    else
      { coverage with full = full + 1 }
  in
  List.fold ~init:(create ()) ~f:aggregate annotations


let aggregate_over_source ~source coverages =
  List.fold
    ~init:(create ~ignore:(List.length (Source.ignore_lines source)) ())
    ~f:sum
    coverages


let log { full; partial; untyped; ignore; crashes } ~total_errors ~path =
  Statistics.coverage
    ~coverage:[
      "full_type_coverage", full;
      "partial_type_coverage", partial;
      "no_type_coverage", untyped;
      "ignore_coverage", ignore;
      "total_errors", total_errors;
      "crashes", crashes;
    ]
    ~normals:["file_name", path]
    ();
