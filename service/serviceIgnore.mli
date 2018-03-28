(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Analysis

module Scheduler = ServiceScheduler

val register: configuration: Configuration.t -> Scheduler.t -> File.Handle.t list -> unit

val filter_by_mode
  :  configuration: Configuration.t
  -> Error.t list
  -> Error.t list

val postprocess: File.Handle.t list -> Error.t list -> Error.t list
