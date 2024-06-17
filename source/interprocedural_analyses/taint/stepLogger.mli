type t

val start : start_message:string -> end_message:string -> t

val finish : step_logger:t -> integers:(string * int) list -> unit -> unit
