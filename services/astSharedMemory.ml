(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast


module HandleKey = struct
  type t = File.Handle.t
  let to_string = File.Handle.show
  let compare = File.Handle.compare
end


include SharedMem.NoCache
    (HandleKey)
    (struct
      type t = Source.t
      let prefix = Prefix.make ()
      let description = "AST"
    end)


let get_source path =
  get path


(* The sources must be removed by remove_paths beforehand. *)
let add_source path source =
  add path source


(* The way hack_parallel works, only the master thread is allowed to remove items from shared
   memory. *)
let remove_paths paths =
  let paths = List.filter ~f:mem paths in
  remove_batch (KeySet.of_list paths)
