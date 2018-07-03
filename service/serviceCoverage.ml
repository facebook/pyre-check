(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open Core

open Ast


type t = {
  strict_coverage: int;
  declare_coverage: int;
  default_coverage: int;
  source_files: int;
}


let coverage ~number_of_files ~sources =
  let strict_coverage, declare_coverage =
    List.fold
      ~init:(0, 0)
      ~f:(fun (prev_strict, prev_declare) handle ->
          match AstSharedMemory.get_source handle with
          | Some { Source.metadata = { Source.Metadata.local_mode; _ }; _ } ->
              (
                prev_strict + (if local_mode = Source.Strict then 1 else 0),
                prev_declare + (if local_mode = Source.Declare then 1 else 0)
              )
          | None -> (prev_strict, prev_declare)
        )
      sources
  in
  {
    strict_coverage;
    declare_coverage;
    default_coverage = number_of_files - strict_coverage - declare_coverage;
    source_files = number_of_files;
  }
