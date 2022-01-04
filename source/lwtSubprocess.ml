(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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

  let create_from_process_and_consumers ~consume_stdout ~consume_stderr process =
    let open Lwt.Infix in
    Lwt.both
      process#status
      (Lwt.both (consume_stdout process#stdout) (consume_stderr process#stderr))
    >>= fun (status, (stdout, stderr)) -> Lwt.return { stdout; stderr; status }
end

let run
    ?(consume_stdout = fun input_channel -> Lwt_io.read input_channel)
    ?(consume_stderr = fun input_channel -> Lwt_io.read input_channel)
    ~arguments
    executable
  =
  let lwt_command = executable, Array.of_list (executable :: arguments) in
  Lwt_process.with_process_full
    lwt_command
    (Completed.create_from_process_and_consumers ~consume_stdout ~consume_stderr)
