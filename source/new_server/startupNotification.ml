(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Pyre

let file_name = "startup_notification.txt"

let produce ~log_path content =
  let path = Path.create_relative ~root:log_path ~relative:file_name in
  File.create ~content path |> File.write


let consume ~log_path () =
  let path = Path.create_relative ~root:log_path ~relative:file_name in
  match File.create path |> File.content with
  | None -> None
  | Some content ->
      Path.remove path;
      Some content
