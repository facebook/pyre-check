(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Base

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

module ClassExpression = struct
  type t = {
    module_: string; [@key "module"]
    qualified_name: string;
  }
  [@@deriving sexp, compare, yojson { strict = false }]
end

module Command = struct
  type t =
    | Stop
    | RegisterClient of { client_id: string }
    | DisposeClient of { client_id: string }
    | FileOpened of {
        path: string;
        content: string option;
        client_id: string;
      }
    | FileClosed of {
        path: string;
        client_id: string;
      }
    | LocalUpdate of {
        path: string;
        content: string option;
        client_id: string;
      }
    | FileUpdate of FileUpdateEvent.t list
  [@@deriving sexp, compare, yojson { strict = false }]
end

module Query = struct
  type t =
    | GetTypeErrors of {
        path: string;
        client_id: string;
      }
    | Hover of {
        path: string;
        position: Ast.Location.position;
        client_id: string;
      }
    | LocationOfDefinition of {
        path: string;
        position: Ast.Location.position;
        client_id: string;
      }
    | Completion of {
        path: string;
        position: Ast.Location.position;
        client_id: string;
      }
    | GetInfo (* Poll the server's state. *)
    | Superclasses of { class_: ClassExpression.t [@key "class"] }
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
