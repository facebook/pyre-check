(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Statement

module type Kind = sig
  type t [@@deriving compare, eq, show, sexp, hash]

  val code : t -> int

  val name : t -> string

  val messages
    :  concise:bool ->
    signature:Define.signature Node.t ->
    Location.Instantiated.t ->
    t ->
    string list

  val inference_information : signature:Define.signature Node.t -> t -> Yojson.Safe.json
end

module type Error = sig
  type kind

  type t = {
    location: Location.t;
    kind: kind;
    signature: Define.signature Node.t;
  }
  [@@deriving compare, eq, show, sexp, hash]

  module Instantiated : sig
    type t [@@deriving sexp, compare, eq, show, hash]

    val location : t -> Location.Instantiated.t

    val path : t -> string

    val kind : t -> kind

    val code : t -> int

    val description : ?separator:string -> ?concise:bool -> t -> show_error_traces:bool -> string

    val to_json : show_error_traces:bool -> t -> Yojson.Safe.json
  end

  include Hashable with type t := t

  val create : location:Location.t -> kind:kind -> define:Define.t Node.t -> t

  val path : t -> Reference.t

  val key : t -> Location.t

  val code : t -> int

  val instantiate : lookup:(Reference.t -> string option) -> t -> Instantiated.t
end

module Make (Kind : Kind) = struct
  module T = struct
    type t = {
      location: Location.t;
      kind: Kind.t;
      signature: Define.signature Node.t;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end

  include T
  include Hashable.Make (T)

  let create ~location ~kind ~define =
    let { Node.value = { Define.signature; _ }; location = define_location } = define in
    { location; kind; signature = { Node.value = signature; location = define_location } }


  let path { location = { Location.path; _ }; _ } = path

  let key { location = { Location.start = { Location.line; _ }; path; _ }; _ } =
    let start = { Location.line; column = -1 } in
    { Location.start; stop = start; path }


  let code { kind; _ } = Kind.code kind

  let _ = show (* shadowed below *)

  let show error = Format.asprintf "%a" pp error

  module Instantiated = struct
    type t = {
      location: Location.Instantiated.t;
      kind: Kind.t;
      signature: Define.signature Node.t;
    }
    [@@deriving sexp, compare, show, hash]

    let equal = [%compare.equal: t]

    let location { location; _ } = location

    let path { location = { Location.path; _ }; _ } = path

    let kind { kind; _ } = kind

    let code { kind; _ } = Kind.code kind

    let description
        ?(separator = " ")
        ?(concise = false)
        { kind; location; signature; _ }
        ~show_error_traces
      =
      let messages = Kind.messages ~concise ~signature location kind in
      Format.asprintf
        "%s [%d]: %s"
        (Kind.name kind)
        (Kind.code kind)
        ( if show_error_traces then
            String.concat ~sep:separator messages
        else
          List.nth_exn messages 0 )


    let to_json
        ~show_error_traces
        ( {
            location =
              { Location.path; start = { Location.line = start_line; column = start_column }; _ };
            kind;
            signature = { Node.value = signature; _ } as signature_node;
            _;
          } as error )
      =
      `Assoc
        [ "line", `Int start_line;
          "column", `Int start_column;
          "path", `String path;
          "code", `Int (Kind.code kind);
          "name", `String (Kind.name kind);
          "description", `String (description error ~show_error_traces);
          "long_description", `String (description error ~show_error_traces:true ~separator:"\n");
          ( "concise_description",
            `String (description error ~show_error_traces ~concise:true ~separator:"\n") );
          "inference", Kind.inference_information ~signature:signature_node kind;
          "define", `String (Reference.show_sanitized signature.name) ]
  end

  let instantiate ~lookup { location; kind; signature } =
    { Instantiated.location = Location.instantiate ~lookup location; kind; signature }
end
