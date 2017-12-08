(** Copyright 2016-present Facebook. All rights reserved. **)

open Core

open Analysis

module Error = PyreError


val analyze_sources
  : Service.t
  -> ?repopulate_handles: (File.Handle.t list)
  -> Configuration.t
  -> (module Environment.Reader)
  -> File.Handle.t list
  -> Error.t list * (Lookup.t String.Map.t)
