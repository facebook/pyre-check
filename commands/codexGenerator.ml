(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Pyre

let to_json ~root handles =
  let get_sources =
    List.fold
      ~init:[]
      ~f:(fun sources path ->
          match AstSharedMemory.get_source path with
          | Some source -> source::sources
          | None -> sources)
  in
  let sources = get_sources handles in
  `Assoc (List.map sources ~f:(Codex.source_to_json root))

let run is_parallel project_root () =
  if Sys.is_directory project_root <> `Yes then
    raise (Invalid_argument (Format.asprintf "`%s` is not a directory" project_root));

  let service = Service.create ~is_parallel () in
  let root = Path.create_absolute project_root in

  Log.info "Parsing...";
  let source_handles = ParseService.parse_sources service ~root in

  Log.info "Generating JSON for Codex...";
  to_json ~root:(Path.absolute root) source_handles
  |> Yojson.Safe.to_string
  |> Log.print "%s"


let command =
  Command.basic
    ~summary:"Generates JSON for Codex without a server"
    Command.Spec.(
      empty
      +> flag "-parallel" no_arg ~doc:"Runs Pyre processing in parallel."
      +> anon (maybe_with_default "." ("project-root" %: string)))
    run
