(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis

module Error = PyreError


val analyze_sources
  : Service.t
  -> ?repopulate_handles: (File.Handle.t list)
  -> Configuration.t
  -> (module Environment.Handler)
  -> File.Handle.t list
  -> Error.t list * (Lookup.t String.Map.t) * TypeCheck.Coverage.t
