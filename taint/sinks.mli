(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


type t =
  | Demo
  | GetAttr
  | IdentityCreation
  | LocalReturn  (* Special marker to infer function in-out behavior *)
  | Logging
  | ODS
  | RemoteCodeExecution
  | RequestSend
  | SQL
  | Test
  | Thrift
  | XMLParser
  | XSS
  | FileSystem
[@@deriving compare, eq, sexp, show, hash]

val create: string -> t
