(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

module ServerInternal = struct
  type t = {
    properties: Server.ServerProperties.t;
    state: State.t Server.ExclusiveLock.t;
  }
end

let handle_request ~server:_ = function
  | Request.Stop -> Server.Stop.stop_waiting_server ()


let handle_raw_request ~server raw_request =
  match Request.of_string raw_request with
  | Result.Error message -> Lwt.return (Response.Error message)
  | Result.Ok request -> handle_request ~server request
