(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  start_message: string;
  end_message: string;
  timer: Timer.t;
}

let start ~start_message ~end_message () =
  let timer = Timer.start () in
  Log.info "%s..." start_message;
  { start_message; end_message; timer }


let finish ?(integers = []) step_logger =
  let time_in_seconds = Timer.stop_in_sec step_logger.timer in
  Log.info "%s: %.3fs" step_logger.end_message time_in_seconds;
  Statistics.performance
    ~name:step_logger.end_message
    ~phase_name:step_logger.start_message
    ~timer:step_logger.timer
    ~integers
    ()
