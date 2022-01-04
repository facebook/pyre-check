(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

let ensure_parent_directory_exists path =
  (* The directory creation part is intentionally using a sequential API, since if we call
     `Lwt_unix.mkdir` instead it would lead to race conditions among the prefix `mkdir` invocations. *)
  PyrePath.ensure_parent_directory_exists path |> Lwt.return


let catch_unix_error f =
  let open Lwt.Infix in
  Lwt.catch
    (fun () -> f () >>= fun () -> Lwt.return (Result.Ok ()))
    (function
      | Unix.Unix_error (error, function_name, _) ->
          let message =
            Format.sprintf "Error when invoking `%s`: %s" function_name (Unix.error_message error)
          in
          Lwt.return (Result.Error message)
      | _ as exn ->
          (* Any exception other than `Unix_error` would be unexpected. *)
          Lwt.fail exn)


let create_symlink ~target link =
  catch_unix_error (fun () -> Lwt_unix.symlink (PyrePath.absolute target) (PyrePath.absolute link))


let remove_if_exists path =
  catch_unix_error (fun () ->
      Lwt.catch
        (fun () -> Lwt_unix.unlink (PyrePath.absolute path))
        (function
          | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_unit
          | _ as exn -> Lwt.fail exn))


let create_parent_directory_and_symlink ~target link =
  let open Lwt.Infix in
  ensure_parent_directory_exists link
  >>= function
  | Result.Error _ as error -> Lwt.return error
  | Result.Ok () -> create_symlink ~target link


let populate ~source_root ~artifact_root build_map =
  if not (PyrePath.is_directory source_root) then
    let message = Format.asprintf "Source root is not a directory: %a" PyrePath.pp source_root in
    Lwt.return (Result.Error message)
  else if not (PyrePath.is_directory artifact_root) then
    let message =
      Format.asprintf "Artifact root is not a directory: %a" PyrePath.pp artifact_root
    in
    Lwt.return (Result.Error message)
  else
    let create_symlink (artifact, source) =
      let source_path = PyrePath.create_relative ~root:source_root ~relative:source in
      let artifact_path = PyrePath.create_relative ~root:artifact_root ~relative:artifact in
      create_parent_directory_and_symlink ~target:source_path artifact_path
    in
    let open Lwt.Infix in
    BuildMap.to_alist build_map
    |> List.map ~f:create_symlink
    |> Lwt.all
    >>= fun results -> Lwt.return (Result.all_unit results)


let update ~source_root ~artifact_root difference =
  if not (PyrePath.is_directory source_root) then
    let message = Format.asprintf "Source root is not a directory: %a" PyrePath.pp source_root in
    Lwt.return (Result.Error message)
  else if not (PyrePath.is_directory artifact_root) then
    let message =
      Format.asprintf "Artifact root is not a directory: %a" PyrePath.pp artifact_root
    in
    Lwt.return (Result.Error message)
  else
    let open Lwt.Infix in
    let process_update (artifact, kind) =
      let artifact_path = PyrePath.create_relative ~root:artifact_root ~relative:artifact in
      match kind with
      | BuildMap.Difference.Kind.Deleted -> remove_if_exists artifact_path
      | New source
      | Changed source -> (
          remove_if_exists artifact_path
          >>= function
          | Result.Error _ as error -> Lwt.return error
          | Result.Ok () ->
              let source_path = PyrePath.create_relative ~root:source_root ~relative:source in
              create_parent_directory_and_symlink ~target:source_path artifact_path)
    in
    BuildMap.Difference.to_alist difference
    |> List.map ~f:process_update
    |> Lwt.all
    >>= fun results -> Lwt.return (Result.all_unit results)
