(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

module Completed : sig
  type t = {
    stdout: string;
    stderr: string;
    status: Unix.process_status;
  }
end

val run : arguments:string list -> string -> Completed.t Lwt.t
