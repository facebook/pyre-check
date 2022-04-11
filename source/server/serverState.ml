(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
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
  build_system: BuildSystem.t;
  type_environment: TypeEnvironment.t;
  error_table: AnalysisError.t list Reference.Table.t;
  subscriptions: Subscriptions.t;
}

let create ?error_table ?subscriptions ~build_system ~type_environment () =
  {
    build_system;
    type_environment;
    error_table = Option.value error_table ~default:(Reference.Table.create ());
    subscriptions = Option.value subscriptions ~default:(Subscriptions.create ());
  }


module StoredConfiguration = Memory.Serializer (struct
  type t = Configuration.Analysis.t

  module Serialized = struct
    type t = Configuration.Analysis.t

    let prefix = Prefix.make ()

    let description = "Configuration"
  end

  let serialize = Fn.id

  let deserialize = Fn.id
end)

module ServerErrors = Memory.Serializer (struct
  type t = Analysis.AnalysisError.t list Reference.Table.t

  module Serialized = struct
    type t = (Reference.t * Analysis.AnalysisError.t list) list

    let prefix = Prefix.make ()

    let description = "All errors"
  end

  let serialize = Hashtbl.to_alist

  let deserialize data = Reference.Table.of_alist_exn data
end)

let load_stored_configuration = StoredConfiguration.load

let load ~configuration ~build_system () =
  let module_tracker = Analysis.ModuleTracker.Serializer.from_stored_layouts ~configuration () in
  let ast_environment = Analysis.AstEnvironment.load module_tracker in
  let type_environment =
    Analysis.AnnotatedGlobalEnvironment.create ast_environment |> Analysis.TypeEnvironment.create
  in
  Analysis.SharedMemoryKeys.DependencyKey.Registry.load ();
  let error_table = ServerErrors.load () in
  create ~build_system ~type_environment ~error_table ()


let store ~path ~configuration { type_environment; error_table; build_system; _ } =
  Memory.SharedMemory.collect `aggressive;
  Analysis.TypeEnvironment.module_tracker type_environment
  |> Analysis.ModuleTracker.Serializer.store_layouts;
  Analysis.TypeEnvironment.ast_environment type_environment |> Analysis.AstEnvironment.store;
  StoredConfiguration.store configuration;
  ServerErrors.store error_table;
  BuildSystem.store build_system;
  Analysis.SharedMemoryKeys.DependencyKey.Registry.store ();
  Memory.save_shared_memory ~path:(PyrePath.absolute path) ~configuration
