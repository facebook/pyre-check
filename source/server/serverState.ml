(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* ServerState defines the elements that make up a running Pyre Server, which is an environment but
   also several other components such as a Builder and a Scheduler needed to run updates. *)

open Core
open Analysis

module Subscriptions = struct
  type t = Subscription.t String.Table.t

  let create () = String.Table.create ()

  let add ~subscription subscriptions =
    Hashtbl.set subscriptions ~key:(Subscription.name_of subscription) ~data:subscription


  let get ~name subscriptions = Hashtbl.find subscriptions name

  let remove ~name subscriptions = Hashtbl.remove subscriptions name

  let all subscriptions = Hashtbl.data subscriptions
end

module BuildFailure = struct
  type t = {
    (* Holds the error message of last build failure that occurred. *)
    mutable message: string option;
    (* Holds temporarily stashed update events that lead to the build failure. Intended to be
       processed again at a later point when the build is fixed. *)
    mutable deferred_events: SourcePath.Event.t list;
  }

  let create () = { message = None; deferred_events = [] }

  let update ~events ~error_message build_failure =
    build_failure.message <- Some error_message;
    build_failure.deferred_events <- List.rev_append events build_failure.deferred_events


  let get_last_error_message { message; _ } = message

  let get_deferred_events { deferred_events; _ } = List.rev deferred_events

  let clear build_failure =
    build_failure.message <- None;
    build_failure.deferred_events <- []
end

type t = {
  scheduler: Scheduler.t;
  build_system: BuildSystem.t;
  overlaid_environment: OverlaidEnvironment.t;
  subscriptions: Subscriptions.t;
  build_failure: BuildFailure.t;
}

let create ?subscriptions ?build_failure ~scheduler ~build_system ~overlaid_environment () =
  {
    scheduler;
    build_system;
    overlaid_environment;
    subscriptions = Option.value subscriptions ~default:(Subscriptions.create ());
    build_failure = Option.value build_failure ~default:(BuildFailure.create ());
  }


module StoredConfiguration = Memory.Serializer (struct
  type t = Configuration.Analysis.t

  module Serialized = struct
    type t = Configuration.Analysis.t

    let prefix = Hack_parallel.Std.Prefix.make ()

    let description = "Configuration"
  end

  let serialize = Fn.id

  let deserialize = Fn.id
end)

let load_stored_configuration = StoredConfiguration.load

let load ~environment_controls ~scheduler ~build_system () =
  let overlaid_environment =
    Analysis.ErrorsEnvironment.AssumeAstEnvironment.load environment_controls
    |> OverlaidEnvironment.create
  in
  create ~scheduler ~build_system ~overlaid_environment ()


let store ~path ~configuration { overlaid_environment; build_system; _ } =
  Memory.SharedMemory.collect `aggressive;
  Analysis.OverlaidEnvironment.store overlaid_environment;
  StoredConfiguration.store configuration;
  BuildSystem.store build_system;
  Memory.save_shared_memory ~path:(PyrePath.absolute path) ~configuration
