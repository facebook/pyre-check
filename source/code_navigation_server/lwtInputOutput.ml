(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

let write_line_ignoring_errors ~output_channel message =
  let send () =
    let%lwt () = Lwt_io.write_line output_channel message in
    Lwt_io.flush output_channel
  in
  let on_io_exception exn =
    Log.log
      ~section:`Server
      "Exception occurred while writing to output channel: %s"
      (Exn.to_string exn);
    Lwt.return_unit
  in
  Lwt.catch send on_io_exception
