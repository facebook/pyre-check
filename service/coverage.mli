(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


type t = {
  strict_coverage: int;
  declare_coverage: int;
  default_coverage: int;
  source_files: int;
}

val coverage: number_of_files: int -> sources: File.Handle.t list -> t
