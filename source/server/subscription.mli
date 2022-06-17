(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ServerResponse = Response

module Request : sig
  type t = SubscribeToTypeErrors of string [@@deriving sexp, compare, yojson]
end

module Response : sig
  type t = {
    name: string;
    body: Response.t;
  }
  [@@deriving sexp, compare, to_yojson]
end

type t

val create : name:string -> output_channel:Lwt_io.output_channel -> unit -> t

val name_of : t -> string

val send : response:ServerResponse.t -> t -> unit Lwt.t

val batch_send : response:ServerResponse.t Lazy.t -> t list -> unit Lwt.t
(** [batch_send ~response subscriptions] sends a response to every subscription channel in
    [subscriptions].

    The response being sent is constructed by forcing [reponse]. The reason why the response is
    constructed lazily is because we want to avoid the cost of the construction when [subscriptions]
    is an empty list. *)
