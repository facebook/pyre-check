(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Analysis
open Ast
open Test

let ( !+ ) descriptor = Unix.File_descr.of_int descriptor

let connections persistent_clients =
  let persistent_clients =
    List.map persistent_clients ~f:(fun (descriptor, failures) -> !+descriptor, failures)
    |> Network.Socket.Map.of_alist_exn
  in
  {
    Server.State.lock = Mutex.create ();
    connections =
      ref
        {
          Server.State.socket = Unix.openfile ~mode:[Unix.O_RDONLY] "/dev/null";
          json_socket = Unix.openfile ~mode:[Unix.O_RDONLY] "/dev/null";
          adapter_socket = Unix.openfile ~mode:[Unix.O_RDONLY] "/dev/null";
          persistent_clients;
          json_sockets = [];
          sockets_to_close = [];
          adapter_sockets = [];
        };
  }


let initialize_server ?incremental_style ~context ~initial_sources =
  let ({ ScratchProject.module_tracker; _ } as project) =
    let internal_sources, external_sources =
      let fold_source (internal_sources, external_sources) (is_external, source) =
        if is_external then
          internal_sources, source :: external_sources
        else
          source :: internal_sources, external_sources
      in
      List.fold initial_sources ~init:([], []) ~f:fold_source
    in
    ScratchProject.setup ?incremental_style ~context ~external_sources internal_sources
  in
  let ( { ScratchProject.BuiltTypeEnvironment.ast_environment; type_environment = environment; _ },
        type_errors )
    =
    ScratchProject.build_type_environment_and_postprocess project
  in
  let errors = Reference.Table.create () in
  List.iter type_errors ~f:(fun error ->
      let key = AnalysisError.path error in
      Hashtbl.add_multi errors ~key ~data:error);
  let state =
    {
      Server.State.module_tracker;
      connections = connections [];
      ast_environment;
      environment;
      lookups = String.Table.create ();
      symlink_targets_to_sources = String.Table.create ();
      last_integrity_check = 0.0;
      last_request_time = 0.0;
      open_documents = Reference.Table.create ();
      errors;
      scheduler = Test.mock_scheduler ();
    }
  in
  project, state
