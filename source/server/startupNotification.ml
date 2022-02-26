(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let file_name = "startup_notification.txt"

let produce ~log_path content =
  let path = PyrePath.create_relative ~root:log_path ~relative:file_name in
  File.create ~content path |> File.write


let consume ~log_path () =
  let path = PyrePath.create_relative ~root:log_path ~relative:file_name in
  match File.create path |> File.content with
  | None -> None
  | Some content ->
      PyrePath.remove path;
      Some content
