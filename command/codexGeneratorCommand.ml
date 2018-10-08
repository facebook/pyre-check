(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Pyre

let to_json ~configuration handles =
  let get_sources =
    List.fold
      ~init:[]
      ~f:(fun sources path ->
          match Ast.SharedMemory.Sources.get path with
          | Some source -> source::sources
          | None -> sources)
  in
  let sources = get_sources handles in
  `Assoc (List.map sources ~f:(Codex.source_to_json ~configuration))

let run is_parallel local_root () =
  if Sys.is_directory local_root <> `Yes then
    raise (Invalid_argument (Format.asprintf "`%s` is not a directory" local_root));

  let configuration =
    Configuration.Analysis.create
      ~parallel:is_parallel
      ~local_root:(Path.create_absolute local_root)
      ()
  in
  let scheduler = Scheduler.create ~configuration () in

  Log.info "Parsing...";
  let { Service.Parser.sources; _ } = Service.Parser.parse_all scheduler ~configuration in

  Log.info "Generating JSON for Codex...";
  to_json ~configuration sources
  |> Yojson.Safe.to_string
  |> Log.print "%s"


let command =
  Command.basic_spec
    ~summary:"Generates JSON for Codex without a server"
    Command.Spec.(
      empty
      +> flag "-parallel" no_arg ~doc:"Runs Pyre processing in parallel."
      +> anon (maybe_with_default "." ("source-root" %: string)))
    run
