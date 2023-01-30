(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core

module ServerStatus = struct
  type t =
    | Rebuilding
    | Rechecking
    (* We were previously using the existence of a Response.TypeErrors response to indicate that the
       server is done processing an incremental request.

       However, there are cases in which a TypeErrors response cannot be handled by the client (i.e.
       shadow mode), which requires us to invent a new type called Ready to indicate the server is
       done processing an incremental request, and is ready to handle new requests.. *)
    | Ready
  [@@deriving sexp, compare, to_yojson]
end

module IncrementalTelemetry = struct
  type t = { overall_duration_ms: int } [@@deriving sexp, compare, to_yojson]
end

type t =
  | Ok
  | Error of string
  | Info of {
      (* All fields are required to implement `pyre servers` *)
      version: string;
      pid: int;
      socket: string;
      global_root: string;
      relative_local_root: string option;
    }
  | StatusUpdate of ServerStatus.t
  | TypeErrors of Analysis.AnalysisError.Instantiated.t list
  | Query of Query.Response.t
  | IncrementalTelemetry of IncrementalTelemetry.t
[@@deriving sexp, compare, to_yojson]
