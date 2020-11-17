(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Completed = struct
  type t = {
    stdout: string;
    stderr: string;
    status: Unix.process_status;
  }

  let create_from_process process =
    let open Lwt.Infix in
    (* Wait for it to finish *)
    process#status
    >>= fun status ->
    Lwt_io.read process#stdout
    >>= fun stdout ->
    Lwt_io.read process#stderr >>= fun stderr -> Lwt.return { stdout; stderr; status }
end

let run ~arguments executable =
  let lwt_command = executable, Array.of_list (executable :: arguments) in
  Lwt_process.with_process_full lwt_command Completed.create_from_process
