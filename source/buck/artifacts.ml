(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
module Path = Pyre.Path

let ensure_parent_directory_exists path =
  (* The directory creation part is intentionally using a sequential API, since if we call
     `Lwt_unix.mkdir` instead it would lead to race conditions among the prefix `mkdir` invocations. *)
  Path.ensure_parent_directory_exists path |> Lwt.return


let create_symlink ~target link =
  let open Lwt.Infix in
  Lwt.catch
    (fun () ->
      Lwt_unix.symlink (Path.absolute target) (Path.absolute link)
      >>= fun () -> Lwt.return (Result.Ok ()))
    (function
      | Unix.Unix_error (error, function_name, _) ->
          let message =
            Format.sprintf "Error when invoking `%s`: %s" function_name (Unix.error_message error)
          in
          Lwt.return (Result.Error message)
      | _ as exn ->
          (* Any exception other than `Unix_error` would be unexpected. *)
          Lwt.fail exn)


let populate ~source_root ~artifact_root build_map =
  if not (Path.is_directory source_root) then
    let message = Format.asprintf "Source root is not a directory: %a" Path.pp source_root in
    Lwt.return (Result.Error message)
  else if not (Path.is_directory artifact_root) then
    let message = Format.asprintf "Artifact root is not a directory: %a" Path.pp artifact_root in
    Lwt.return (Result.Error message)
  else
    let open Lwt.Infix in
    let create_symlink (artifact, source) =
      let source_path = Path.create_relative ~root:source_root ~relative:source in
      let artifact_path = Path.create_relative ~root:artifact_root ~relative:artifact in
      ensure_parent_directory_exists artifact_path
      >>= function
      | Result.Error _ as error -> Lwt.return error
      | Result.Ok () -> create_symlink ~target:source_path artifact_path
    in
    BuildMap.to_alist build_map
    |> List.map ~f:create_symlink
    |> Lwt.all
    >>= fun results -> Lwt.return (Result.all_unit results)
