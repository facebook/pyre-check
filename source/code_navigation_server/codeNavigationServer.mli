(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The library provides logic for starting a code navigation Pyre server.

    Code navigation server is different from a traditional Pyre server in the following
    perspectives:

    - Code navigation server is lazier. It attempts to skip as much upfront computation as possible,
      and perform the least amount of necessary building and code analysis only when relevant
      information is queried by the user.
    - Code navigation server is not expected to interact with end users directly. Its primary focus
      is to serve downstream language toolings like linters and IDEs. *)

(** {1 Server Start} *)

(** This module contains all information needed to start a code navigation server. *)
module StartOptions : sig
  type t = {
    environment_controls: Analysis.EnvironmentControls.t;
        (** Configurations which control type checking behaviors. *)
    source_paths: Configuration.SourcePaths.t;
        (** Paths to the source files needed to be type checked. *)
    socket_path: PyrePath.t;
        (** The code navigation server uses Unix-domain socket, and this option controls where the
            socket should be placed on the filesystem. *)
    watchman: Server.StartOptions.Watchman.t option;
        (** Watchman setting for the code navigation server. [None] means watchman is disabled. *)
    critical_files: Server.CriticalFile.t list;
        (** A list of files whose changes would immediately bring down the server.*)
  }
end

(** {1 Server State}*)

(** This module contains APIs that are relevant to the internal state of the code navigation server. *)
module State : sig
  (** A type that represent the internal state of the server. *)
  type t
end

(** This module contains APIs that are relevant to start a code navigation server. *)
module Start : sig
  (** [start_server ~on_started ~on_exception start_options] starts a code navigation server, whose
      behavior is controlled by the given [start_options]. After the server is started, [on_started]
      will be invoked and waited. Once the promise returned by [on_started] gets resolved or
      rejected, the server will be automatically shutdown.

      If the server fails to start, or if an exception is raised from [on_started], [on_exception]
      will be invoked on the raised exception.

      Other than [on_started], the started server will also monitor signals received by the process.
      When an [SIGINT] is received, a {!Server.Start.ServerStopped} exception will be raised. Other
      fatal signals like [SIGTERM], [SIGSEGV], etc. will result in a
      {!Server.Start.ServerInterrupted} exception instead.*)
  val start_server
    :  on_started:(Server.ServerProperties.t -> State.t Server.ExclusiveLock.t -> 'a Lwt.t) ->
    on_exception:(exn -> 'a Lwt.t) ->
    StartOptions.t ->
    'a Lwt.t
end

(** {1 Testing} *)

(** This module contains code that are useful for testing purpose only. They are considered internal
    implementation details and it is strongly recommended to avoid relying on them in production
    code! *)
module Testing : sig
  module Request : sig
    type t = Stop [@@deriving sexp, compare, yojson { strict = false }]
  end

  module Response : sig
    type t = Error of string [@@deriving sexp, compare, yojson { strict = false }]
  end
end
