(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Statement

module type Kind = sig
  type t [@@deriving compare, eq, show, sexp, hash]

  val code : t -> int

  val name : t -> string

  val messages
    :  concise:bool ->
    signature:Define.Signature.t Node.t ->
    Location.WithPath.t ->
    t ->
    string list

  val inference_information : signature:Define.Signature.t Node.t -> t -> Yojson.Safe.json
end

module type Error = sig
  type kind

  type t = {
    location: Location.WithModule.t;
    kind: kind;
    signature: Define.Signature.t Node.t;
  }
  [@@deriving compare, eq, show, sexp, hash]

  module Instantiated : sig
    type t [@@deriving sexp, compare, eq, show, hash, yojson { strict = false }]

    val location : t -> Location.WithPath.t

    val path : t -> string

    val code : t -> int

    val description : t -> string

    val long_description : t -> string

    val concise_description : t -> string
  end

  include Hashable with type t := t

  val create : location:Location.WithModule.t -> kind:kind -> define:Define.t Node.t -> t

  val path : t -> Reference.t

  val key : t -> Location.WithModule.t

  val code : t -> int

  val instantiate
    :  show_error_traces:bool ->
    lookup:(Reference.t -> string option) ->
    t ->
    Instantiated.t

  val async_instantiate
    :  show_error_traces:bool ->
    lookup:(Reference.t -> string option Lwt.t) ->
    t ->
    Instantiated.t Lwt.t
end

module Make (Kind : Kind) = struct
  module T = struct
    type t = {
      location: Location.WithModule.t;
      kind: Kind.t;
      signature: Define.Signature.t Node.t;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end

  include T
  include Hashable.Make (T)

  let create ~location ~kind ~define =
    let { Node.value = { Define.signature; _ }; location = define_location } = define in
    { location; kind; signature = { Node.value = signature; location = define_location } }


  let path { location = { Location.WithModule.path; _ }; _ } = path

  let key { location = { Location.WithModule.start = { Location.line; _ }; path; _ }; _ } =
    let start = { Location.line; column = -1 } in
    { Location.WithModule.start; stop = start; path }


  let code { kind; _ } = Kind.code kind

  let _ = show (* shadowed below *)

  let show error = Format.asprintf "%a" pp error

  module Instantiated = struct
    type t = {
      line: int;
      column: int;
      stop_line: int;
      stop_column: int;
      path: string;
      code: int;
      name: string;
      description: string;
      long_description: string;
      concise_description: string;
      (* TODO (T70359404): This field does not belong here. *)
      inference: (Yojson.Safe.t[@sexp.opaque] [@compare.ignore]);
      define: string;
    }
    [@@deriving sexp, compare, show, hash, yojson { strict = false }]

    let equal = [%compare.equal: t]

    let location { line; column; stop_line; stop_column; path; _ } =
      { Location.start = { line; column }; stop = { line = stop_line; column = stop_column } }
      |> Location.with_path ~path


    let path { path; _ } = path

    let code { code; _ } = code

    let description { description; _ } = description

    let long_description { long_description; _ } = long_description

    let concise_description { concise_description; _ } = concise_description

    let create
        ~location:
          ( {
              Location.WithPath.path;
              start = { Location.line = start_line; column = start_column };
              stop = { Location.line = stop_line; column = stop_column };
            } as location )
        ~kind
        ~signature:({ Node.value = signature; _ } as signature_node)
        ~show_error_traces
        ()
      =
      let description ~concise ~separator ~show_error_traces =
        let messages = Kind.messages ~concise ~signature:signature_node location kind in
        Format.asprintf
          "%s [%d]: %s"
          (Kind.name kind)
          (Kind.code kind)
          ( if show_error_traces then
              String.concat ~sep:separator messages
          else
            List.nth_exn messages 0 )
      in
      {
        line = start_line;
        column = start_column;
        stop_line;
        stop_column;
        path;
        code = Kind.code kind;
        name = Kind.name kind;
        description = description ~show_error_traces ~concise:false ~separator:" ";
        long_description = description ~show_error_traces:true ~concise:false ~separator:"\n";
        concise_description = description ~show_error_traces ~concise:true ~separator:"\n";
        inference = Kind.inference_information ~signature:signature_node kind;
        define = Reference.show_sanitized (Reference.delocalize (Node.value signature.name));
      }
  end

  let instantiate ~show_error_traces ~lookup { location; kind; signature } =
    Instantiated.create
      ~location:(Location.WithModule.instantiate ~lookup location)
      ~kind
      ~signature
      ~show_error_traces
      ()


  let async_instantiate ~show_error_traces ~lookup { location; kind; signature } =
    let open Lwt.Infix in
    Location.WithModule.async_instantiate ~lookup location
    >>= fun location ->
    Lwt.return (Instantiated.create ~location ~kind ~signature ~show_error_traces ())
end
