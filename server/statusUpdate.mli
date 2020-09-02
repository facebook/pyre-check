(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val write
  :  connections:State.connections ->
  message:string ->
  message_type:LanguageServer.Types.ShowMessageParameters.messageType ->
  unit

val information : message:string -> state:State.t -> unit

val warning : message:string -> state:State.t -> unit

val error : message:string -> state:State.t -> unit
