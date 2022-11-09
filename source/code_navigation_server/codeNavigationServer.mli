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

      Other than [on_started], the started server will also monitor signals received by the process.
      When an [SIGINT] is received, a {!Server.Start.ServerStopped} exception will be raised. Other
      fatal signals like [SIGTERM], [SIGSEGV], etc. will result in a
      {!Server.Start.ServerInterrupted} exception instead.*)
  val start_server
    :  on_started:(Server.ServerProperties.t -> State.t Server.ExclusiveLock.t -> 'a Lwt.t) ->
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

    module FileUpdateEvent : sig
      module Kind : sig
        (** A helper type that help specifying the change associated with the event. *)
        type t =
          | CreatedOrChanged
          | Deleted
        [@@deriving sexp, compare, yojson { strict = false }]
      end

      (** A helper type that help specifying a file change event. *)
      type t = {
        kind: Kind.t;  (** The change type. *)
        path: string;
            (** The changed path. We currently do not support specifying the path by module name due
                to how caching was done in {!Analysis.ModuleTracker} *)
      }
      [@@deriving sexp, compare, yojson { strict = false }]
    end

    module Command : sig
      (** A type representing actionable commands sent from the clients to the server. Processing
          these commands may alter the internal state of the code navigation server.

          The server will send back a response (usually {!Response.Ok} or {!Response.Error}) and
          close its connection with the client once the command gets processed. *)
      type t =
        | Stop
            (** A command that asks the server to stop. The server will shut itself down immediately
                when this request gets processed. No response will be sent back to the client. *)
        | LocalUpdate of {
            module_: Module.t;
            content: string option;
            overlay_id: string;
          }
            (** A command that asks the server to update a given module locally for an overlay.
                [content] specifies the content of the source file corresponds to the module.
                [content] being [None] indicates that contents of the source file should match what
                was stored on the filesystem.

                The server will send back a {!Response.Ok} response when the update succeeds. If the
                overlay with the given ID does not exist yet, a new overlay with that ID will be
                created.

                If the provided module is not covered by the code navigation server, the server will
                respond with a {!Response.ErrorKind.ModuleNotTracked} error. *)
        | FileUpdate of FileUpdateEvent.t list
            (** A command that notify the server that a file has changed on disk, so the server
                needs to incrementally adjust its internal state accordingly. Events will get
                processed in-order. An on-disk change may potentially affect existing overlays when
                those overlays have dependency to the file being updated.

                The server will send back a {!Response.Ok} response when the sever is done updating
                its internal state. In particular, no errors will be returned if any of the provided
                modules is not covered by the code navigation server. *)
      [@@deriving sexp, compare, yojson { strict = false }]
    end

    module Query : sig
      (** A type representing queries sent from the clients to the server.

          The code navigation server supports a primitive form of isolation between different
          clients. Many kinds of requests that query server state can optionally specify an
          [overlay_id], and the server will attempt to guarantee that type checking states for
          different [overlay_id]s will not interfere with each other. Overlays are implicitly
          created the first time a {!Command.LocalUpdate} command is sent to the server.

          The server will send back a query response and close its connection with the client once
          the query gets processed. *)
      type t =
        | GetTypeErrors of {
            module_: Module.t;
            overlay_id: string option;
          }
            (** A query that asks the server to type check a given module. The server will send back
                a {!Response.TypeErrors} response when the type checking completes.

                If the provided module is not covered by the code navigation server, the server will
                respond with a {!Response.ErrorKind.ModuleNotTracked} error. If the server cannot
                find the overlay with the given ID, it will respond with a
                {!Response.ErrorKind.OverlayNotFound} error. *)
        | Hover of {
            module_: Module.t;
            position: Ast.Location.position;
            overlay_id: string option;
          }
            (** A query that asks the server to return hover information at a given location in a
                given module. The server will send back a {!Response.Hover} response as result. The
                response will contain an empty list if the server do not have any hover text to show
                at the location.

                If the provided module is not covered by the code navigation server, the server will
                respond with a {!Response.ErrorKind.ModuleNotTracked} error. If the server cannot
                find the overlay with the given ID, it will respond with a
                {!Response.ErrorKind.OverlayNotFound} error. *)
        | LocationOfDefinition of {
            module_: Module.t;
            position: Ast.Location.position;
            overlay_id: string option;
          }
            (** A query that asks the server to return the location of definitions for a given
                cursor point in a given module. The server will send back a
                {!Response.LocationOfDefinition} response as result. The response will contain an
                empty list if a definition cannot be found.

                If the provided module is not covered by the code navigation server, the server will
                respond with a {!Response.ErrorKind.ModuleNotTracked} error. If the server cannot
                find the overlay with the given ID, it will respond with a
                {!Response.ErrorKind.OverlayNotFound} error. *)
        | GetInfo
            (** A query that asks for server metadata, intended to be consumed by the `pyre servers`
                command. *)
      [@@deriving sexp, compare, yojson { strict = false }]
    end

    (** Subscription is a mechanism with which the client can estabilsh a persistent connection to
        the code navigation server. The mechanism is useful when the client wants to continuously
        keep track of certain internal status change (liveness, availability, etc.). *)
    module Subscription : sig
      (** A type representing subscription requests sent from the clients to the server.

          Unlike {!Command.t} and {!Query.t}, the code navigation server will not proactively close
          the underlying socket connection when receiving a {!Subscription.t}. Instead, it will send
          back a {!Response.Ok} to acknowledge the subscription first, then leave the connection
          open, and unilaterally push interesting internal status changes to the client via that
          connection. Only when the client terminates the connection on its side will the server
          stop sending the updates. *)
      type t = Subscribe [@@deriving sexp, compare, yojson { strict = false }]
    end

    (** A type representing requests sent from the clients to the server. *)
    type t =
      | Query of Query.t  (** Ephemeral connection. Read-only access to server state. *)
      | Command of Command.t  (** Ephemeral connection. Read-write access to server state. *)
      | Subscription of Subscription.t  (** Persistent connection. *)
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

    module HoverContent : sig
      module Kind : sig
        (** TODO(T103574623): Support Markup. *)
        type t = PlainText [@@deriving sexp, compare, yojson { strict = false }]
      end

      (** A type representing hovering text element. Roughly corresponds to LSP's [MarkupContent]
          structure. *)
      type t = {
        kind: Kind.t;
        value: string;
      }
      [@@deriving sexp, compare, yojson { strict = false }]
    end

    module DefinitionLocation : sig
      (** A type representing location of a definition.

          TODO: Support LSP [LocationLink] to enable the functionality of "peek definition". *)
      type t = {
        path: string;
        range: Ast.Location.t;
      }
      [@@deriving sexp, compare, yojson { strict = false }]
    end

    module Status : sig
      (** A type representing current status of the server. It usually gets sent from the server to
          its clients via the [ServerStatus] response. *)
      type t =
        | Idle
            (** This response is sent when the code navigation server is done processing a
                outstanding incremental update request in the background. *)
        | BusyChecking of { overlay_id: string option }
            (** This response is sent when the code navigation server is about to start performing
                an incremental update request.

                [overlay_id] will be [None] if the incremental update is on the whole project (i.e.
                the server is handling a {!Testing.Request.FileUpdateEvent}), and will bet set if
                the incremental update is on a given overlay (i.e. the server is handling a
                {!Testing.Request.LocalUpdate}). *)
        | Stop of { message: string }
            (** This response is sent when the code navigation server is about to terminate itself.
                [message] field will contain message that explains why the server wants to go down. *)
      [@@deriving sexp, compare, yojson { strict = false }]
    end

    (** A type representing responses sent from the server to its clients.

        If a client establishes a connection with the code navigation server and sends a
        {!Request.t}, the server will process the request, send back a {!Response.t}, and close the
        connection immediately. *)
    type t =
      | Ok  (** This response will be used for acknowledging successful processing of a request. *)
      | Error of ErrorKind.t
          (** This response will be sent when the server runs into errors when processing a request. *)
      | TypeErrors of Analysis.AnalysisError.Instantiated.t list
          (** Response for {!Request.GetTypeErrors}. *)
      | Hover of { contents: HoverContent.t list }
          (** Response for {!Request.Hover}. [contents] contains a list of items that will be shown
              to the user (there can be many because build system may map the same file to multiple
              modules). TODO: Add an optional [range] field used to visualize a hover. *)
      | LocationOfDefinition of { definitions: DefinitionLocation.t list }
          (** Response for {!Request.LocationOfDefinition}. The associated value is a list since
              there can be many potential definitions for a given item, either because build system
              may map the same file to multiple modules, or because the same name may get redefined
              multiple times.*)
      | ServerStatus of Status.t
          (** Response the server may push if the client choose to establish a subscription on
              server status. *)
      | Info of {
          version: string;
          pid: int;
          socket: string;
          global_root: string;
          relative_local_root: string option;
        }
          (** The information provides in response to GetInfo queries. All fields must be present
              for the `pyre servers` command. *)
    [@@deriving sexp, compare, yojson { strict = false }]
  end

  (** A utility module that helps the code navigation to keep track of established subscriptions in
      its internal state. *)
  module Subscriptions : sig
    module Identifier : sig
      (** An opaque type used to differentiate one subscription from another.

          Identifiers are expected to be created and maintained exclusively by {!Subscriptions.t}. *)
      type t [@@deriving sexp, equal]
    end

    (** A type representing a collection of subscriptions. *)
    type t

    (** Create an empty collection of subscriptions. *)
    val create : unit -> t

    (** Return how many subscriptions are currently registered in the collection. *)
    val count : t -> int

    (** [register ~output_channel subscriptions] add a new subscription [output_channel] to the
        collection [subscriptions].

        The collection itself does not perform any checks for duplicate subscriptions. Each call to
        the [register] function is treated as setting up a different subscription, regardless of
        whether we pass the same [output_channel] to the function or not.

        When a subscription is registered, the [register] function will hand back an {!Identifier.t}
        to its caller. The returned identifier is guaranteed to be unique from all
        previously-registered subscriptions in the collection, and can be used to unregister a
        subscription later. *)
    val register : output_channel:Lwt_io.output_channel -> t -> Identifier.t

    (** [unregister ~identifier subscriptions] removes a subscription with the given [identifier]
        from the collection [subscriptions]. *)
    val unregister : identifier:Identifier.t -> t -> unit

    (** [broadcast_raw ~message subscriptions] sends a [message] string (a terminating '\n'
        character will be appended as a message separator) to every subscription channel in
        [subscriptions].

        The message being sent is constructed by forcing [message]. The message is constructed
        lazily to avoid the cost of the construction when [subscriptions] is empty *)
    val broadcast_raw : message:string Lazy.t -> t -> unit Lwt.t

    (** [broadcast ~response subscriptions] sends a [response] to every subscription channel in
        [subscriptions]. It is a convenient wrapper around serlializing [response] and then invoking
        [broadcast_raw].

        The message being sent is constructed by forcing [message]. The message is constructed
        lazily to avoid the cost of the construction when [subscriptions] is empty *)
    val broadcast : response:Response.t Lazy.t -> t -> unit Lwt.t
  end
end
