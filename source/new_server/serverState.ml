(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast
open Analysis

module Subscriptions = struct
  type t = Subscription.t String.Table.t

  let create () = String.Table.create ()

  let add ~name ~subscription subscriptions = Hashtbl.set subscriptions ~key:name ~data:subscription

  let get ~name subscriptions = Hashtbl.find subscriptions name

  let remove ~name subscriptions = Hashtbl.remove subscriptions name

  let all subscriptions = Hashtbl.data subscriptions
end

type t = {
  start_time: Timer.t;
  socket_path: Path.t;
  configuration: Configuration.Analysis.t;
  critical_files: CriticalFile.t list;
  build_system: BuildSystem.t;
  type_environment: TypeEnvironment.t;
  error_table: AnalysisError.t list Reference.Table.t;
  subscriptions: Subscriptions.t;
}

let create
    ?start_time
    ?error_table
    ?subscriptions
    ~socket_path
    ~configuration
    ~critical_files
    ~build_system
    ~type_environment
    ()
  =
  {
    start_time = Option.value start_time ~default:(Timer.start ());
    socket_path;
    critical_files;
    configuration;
    build_system;
    type_environment;
    error_table = Option.value error_table ~default:(Reference.Table.create ());
    subscriptions = Option.value subscriptions ~default:(Subscriptions.create ());
  }


let load ~socket_path ~configuration ~critical_files ~build_system () =
  let module_tracker = Analysis.ModuleTracker.SharedMemory.load () in
  let ast_environment = Analysis.AstEnvironment.load module_tracker in
  let type_environment =
    Analysis.AnnotatedGlobalEnvironment.create ast_environment |> Analysis.TypeEnvironment.create
  in
  Analysis.SharedMemoryKeys.DependencyKey.Registry.load ();
  let error_table = Server.SavedState.ServerErrors.load () in
  create ~socket_path ~critical_files ~configuration ~build_system ~type_environment ~error_table ()


let store ~path { configuration; type_environment; error_table; build_system; _ } =
  Memory.SharedMemory.collect `aggressive;
  Analysis.TypeEnvironment.module_tracker type_environment
  |> Analysis.ModuleTracker.SharedMemory.store;
  Analysis.TypeEnvironment.ast_environment type_environment |> Analysis.AstEnvironment.store;
  Server.SavedState.StoredConfiguration.store configuration;
  Server.SavedState.ServerErrors.store error_table;
  BuildSystem.store build_system;
  Analysis.SharedMemoryKeys.DependencyKey.Registry.store ();
  Memory.save_shared_memory ~path:(Path.absolute path) ~configuration
