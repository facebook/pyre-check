(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


val find_sources: ?filter: (string -> bool) -> Configuration.t -> Pyre.Path.t list

val parse_stubs: ServiceScheduler.t -> configuration: Configuration.t -> File.Handle.t list

val parse_sources
  :  ?filter: (string -> bool)
  -> ServiceScheduler.t
  -> configuration: Configuration.t
  -> File.Handle.t list

val parse_sources_list
  :  configuration: Configuration.t
  -> scheduler: ServiceScheduler.t
  -> files: File.t list
  -> File.Handle.t list

type result = {
  stubs: File.Handle.t list;
  sources: File.Handle.t list;
}

val parse_all: ServiceScheduler.t -> configuration: Configuration.t -> result
