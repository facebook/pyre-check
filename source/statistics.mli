(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module GlobalState : sig
  type t

  val initialize
    :  ?logger:string ->
    ?log_identifier:string ->
    ?project_name:string ->
    ?project_root:string ->
    unit ->
    unit

  val get : unit -> t

  val restore : t -> unit
end

val disable : unit -> unit

val sample
  :  ?integers:(string * int) list ->
  ?normals:(string * string) list ->
  ?metadata:bool ->
  unit ->
  string

val flush : unit -> unit

val performance
  :  ?flush:bool ->
  ?randomly_log_every:int ->
  ?always_log_time_threshold:float ->
  ?section:Log.section ->
  name:string ->
  timer:Timer.t ->
  ?phase_name:string ->
  ?integers:(string * int) list ->
  ?normals:(string * string) list ->
  unit ->
  unit

val event
  :  ?flush:bool ->
  ?randomly_log_every:int ->
  ?section:Log.section ->
  name:string ->
  ?integers:(string * int) list ->
  ?normals:(string * string) list ->
  unit ->
  unit

val log_model_query_outputs
  :  ?flush:bool ->
  ?section:Log.section ->
  model_query_name:string ->
  generated_models_count:int ->
  unit ->
  unit

val log_exception : exn -> fatal:bool -> origin:string -> unit

val buck_event
  :  ?flush:bool ->
  ?integers:(string * int) list ->
  ?normals:(string * string) list ->
  unit ->
  unit

val log_worker_exception : pid:int -> origin:string -> Unix.process_status -> unit
