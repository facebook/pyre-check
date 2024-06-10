open Core

module StepLogger = struct
  type t = {
    start_message: string;
    end_message: string;
    timer: Timer.t;
  }

  let start ~start_message ~end_message =
    let timer = Timer.start () in
    Log.info "%s" start_message;
    { start_message; end_message; timer }

  let finish ~step_logger ~section ~integers () =
    let time_in_seconds = Timer.stop_in_sec step_logger.timer in
    Log.log ~section "%s: %.3fs" step_logger.end_message time_in_seconds;
    Statistics.performance
      ~name:step_logger.end_message
      ~phase_name:step_logger.start_message
      ~timer:step_logger.timer
      ~integers
      ()
end
