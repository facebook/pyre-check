(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Analysis


val analyze_sources
  :  Scheduler.t
  -> Configuration.t
  -> (module Environment.Handler)
  -> File.Handle.t list
  -> Error.t list * TypeCheck.Coverage.t
