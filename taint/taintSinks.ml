(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


type t =
  | LocalReturn  (* Special marker to infer function in-out behavior *)
  | RemoteCodeExecution
  | TestSink
[@@deriving compare, sexp, show, hash]


let to_string = function
  | LocalReturn -> "LocalReturn"
  | RemoteCodeExecution -> "RCE"
  | TestSink -> "test sink"
