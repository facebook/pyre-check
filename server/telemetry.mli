(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

module Message : sig
  type message =
    | Base of {
        root: string;
        hash: string;
      }
    | Update of {
        path: string;
        content: string;
      }

  type t = {
    uuid: string;
    message: message;
  }
end

val reset_budget : ?value:int -> unit -> unit

val send_telemetry : f:(unit -> Message.t) -> unit -> unit
