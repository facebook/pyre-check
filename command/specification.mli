(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

val base_command_line_arguments
  : ( bool ->
      string option ->
      string list ->
      bool ->
      bool ->
      bool ->
      bool ->
      bool ->
      string list ->
      bool ->
      string option ->
      string option ->
      int ->
      string ->
      string option ->
      string option ->
      string ->
      string list ->
      string list ->
      string list ->
      string list ->
      string ->
      unit ->
      unit,
      unit -> unit )
    Core.Command.Spec.t
