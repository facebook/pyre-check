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
    module Module : sig
      (** A helper type that help specifying a Python module. *)
      type t =
        | OfPath of string
            (** Specify a module at the given file path. The path is expected to be absolute.
                Symlinks will not be followed.*)
        | OfName of string  (** Specify a module with a given dot-qualified name directly. *)
      [@@deriving sexp, compare, yojson { strict = false }]
    end

    (** A type representing requests sent from the clients to the server.

        The code navigation server supports a primitive form of isolation between different clients.
        Many kinds of requests that query server state can optionally specify an [overlay_id], and
        the server will attempt to guarantee that type checking states for different [overlay_id]s
        will not interfere with each other. Overlays are implicitly created the first time a
        [LocalUpdate] request is sent to the server. *)
    type t =
      | Stop
          (** A request that asks the server to stop. The server will shut itself down immediately
              when this request gets processed. No response will be sent back to the client. *)
      | GetTypeErrors of {
          module_: Module.t;
          overlay_id: string option;
        }
          (** A request that asks the server to type check a given module. The server will send back
              a {!Response.TypeErrors} response when the type checking completes.

              If the provided module is not covered by the code navigation server, the server will
              respond with a {!Response.ErrorKind.ModuleNotTracked} error. If the server cannot find
              the overlay with the given ID, it will respond with a
              {!Response.ErrorKind.OverlayNotFound} error. *)
      | LocalUpdate of {
          module_: Module.t;
          content: string;
          overlay_id: string;
        }
          (** A request that asks the server to update a given module locally for an overlay. The
              server will send back a {!Response.Ok} response when the update succeeds. If the
              overlay with the given ID does not exist yet, a new overlay with that ID will be
              created.

              If the provided module is not covered by the code navigation server, the server will
              respond with a {!Response.ErrorKind.ModuleNotTracked} error. *)
    [@@deriving sexp, compare, yojson { strict = false }]
  end

  module Response : sig
    module ErrorKind : sig
      (** A type storing details of the error the server runs into *)
      type t =
        | InvalidRequest of string
            (** This error occurs when the client has sent a request which the server cannot parse. *)
        | ModuleNotTracked of { module_: Request.Module.t }
            (** This error occurs when the client has requested info on a module which the server
                cannot find. *)
        | OverlayNotFound of { overlay_id: string }
            (** This error occurs when the client has requested info from an overlay whose id does
                not exist within the server. *)
      [@@deriving sexp, compare, yojson { strict = false }]
    end

    (** A type representing responses sent from the server to its clients *)
    type t =
      | Ok  (** This response will be used for acknowledging successful processing of a request. *)
      | Error of ErrorKind.t
          (** This response will be sent when the server runs into errors when processing a request. *)
      | TypeErrors of Analysis.AnalysisError.Instantiated.t list
          (** Response for {!Request.GetTypeErrors}. *)
    [@@deriving sexp, compare, yojson { strict = false }]
  end
end
