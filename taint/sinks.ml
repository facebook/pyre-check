(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


type t =
  | LocalReturn  (* Special marker to infer function in-out behavior *)
  | RemoteCodeExecution
  | Test
[@@deriving compare, eq, sexp, show, hash]


let show = function
  | LocalReturn -> "LocalReturn"
  | RemoteCodeExecution -> "RemoteCodeExecution"
  | Test -> "Test"


let create = function
  | "LocalReturn" -> LocalReturn
  | "RemoteCodeExecution" -> RemoteCodeExecution
  | "Test" -> Test
  | name -> failwith (Format.sprintf "Unsupported taint sink %s" name)
