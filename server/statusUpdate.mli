(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

val information : message:string -> short_message:string option -> state:State.t -> unit

val warning : message:string -> short_message:string option -> state:State.t -> unit

val error : message:string -> short_message:string option -> state:State.t -> unit
