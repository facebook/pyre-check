(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Base

module Module = struct
  (** A helper type that help specifying a Python module. *)
  type t =
    | OfPath of string
        (** Specify a module at the given file path. The path is expected to be absolute. Symlinks
            will not be followed.*)
    | OfName of string  (** Specify a module with a given dot-qualified name directly. *)
  [@@deriving sexp, compare, yojson { strict = false }]
end

module FileUpdateEvent = struct
  module Kind = struct
    type t =
      | CreatedOrChanged
      | Deleted
    [@@deriving sexp, compare, yojson { strict = false }]
  end

  type t = {
    kind: Kind.t;
    path: string;
  }
  [@@deriving sexp, compare, yojson { strict = false }]
end

module Command = struct
  type t =
    | Stop
    | LocalUpdate of {
        module_: Module.t; [@key "module"]
        content: string option;
        overlay_id: string;
      }
    | FileUpdate of FileUpdateEvent.t list
  [@@deriving sexp, compare, yojson { strict = false }]
end

module Query = struct
  type t =
    | GetTypeErrors of {
        module_: Module.t; [@key "module"]
        overlay_id: string option;
      }
    | Hover of {
        module_: Module.t; [@key "module"]
        position: Ast.Location.position;
        overlay_id: string option;
      }
    | LocationOfDefinition of {
        module_: Module.t; [@key "module"]
        position: Ast.Location.position;
        overlay_id: string option;
      }
    | GetInfo (* Poll the server's state. *)
  [@@deriving sexp, compare, yojson { strict = false }]
end

module Subscription = struct
  type t = Subscribe [@@deriving sexp, compare, yojson { strict = false }]
end

type t =
  | Query of Query.t
  | Command of Command.t
  | Subscription of Subscription.t
[@@deriving sexp, compare, yojson { strict = false }]
