(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)
open Base

let source_path_of_string path = PyrePath.create_absolute path |> SourcePath.create

let string_of_source_path source_path = SourcePath.raw source_path |> PyrePath.absolute

module Client = struct
  module OverlayId = struct
    type t = {
      client_id: string;
      source_path: SourcePath.t;
    }

    let to_string { client_id; source_path } =
      Format.sprintf "%s:%s" client_id (string_of_source_path source_path)
  end

  module Single = struct
    type t = { opened_files: Set.M(String).t }

    let empty = { opened_files = Set.empty (module String) }

    let open_file { opened_files } source_path =
      let opened_files = string_of_source_path source_path |> Set.add opened_files in
      { opened_files }


    (* Return [None] if [source_path] has not been opened. *)
    let close_file { opened_files } source_path =
      let path = string_of_source_path source_path in
      match Set.mem opened_files path with
      | false -> None
      | true ->
          let opened_files = Set.remove opened_files path in
          Some { opened_files }


    let mem { opened_files } source_path = string_of_source_path source_path |> Set.mem opened_files

    let opened_files { opened_files } = opened_files
  end

  type t = Single.t Hashtbl.M(String).t

  let create () = Hashtbl.create (module String)

  let register client_states client_id =
    let data = Single.empty in
    match Hashtbl.add client_states ~key:client_id ~data with
    | `Duplicate -> false
    | `Ok -> true


  let dispose client_states client_id =
    let success = Hashtbl.mem client_states client_id in
    if success then
      Hashtbl.remove client_states client_id;
    success


  module WorkingSet = struct
    let add ~client_id ~source_path client_states =
      match Hashtbl.find client_states client_id with
      | None -> `ClientNotRegistered
      | Some single ->
          let new_client = Single.open_file single source_path in
          Hashtbl.set client_states ~key:client_id ~data:new_client;
          `Ok { OverlayId.client_id; source_path }


    let remove ~client_id ~source_path client_states =
      match Hashtbl.find client_states client_id with
      | None -> `ClientNotRegistered
      | Some single -> (
          match Single.close_file single source_path with
          | None -> `FileNotAdded
          | Some new_client ->
              Hashtbl.set client_states ~key:client_id ~data:new_client;
              `Ok { OverlayId.client_id; source_path })


    let lookup ~client_id ~source_path client_states =
      match Hashtbl.find client_states client_id with
      | None -> `ClientNotRegistered
      | Some single -> (
          match Single.mem single source_path with
          | false -> `FileNotAdded
          | true -> `Ok { OverlayId.client_id; source_path })


    let to_list client_states =
      let init = Set.empty (module String) in
      Hashtbl.fold client_states ~init ~f:(fun ~key:_ ~data sofar ->
          Set.union sofar (Single.opened_files data))
      |> Set.to_list
      |> List.map ~f:source_path_of_string
  end
end

type t = {
  environment: Analysis.OverlaidEnvironment.t;
  build_system: BuildSystem.t;
  client_states: Client.t;
}
