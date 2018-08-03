(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Statement


module type KIND = sig

  type t
  [@@deriving compare, eq, show, sexp, hash]

  val code: t -> int
  val name: t -> string
  val messages:
    detailed: bool
    -> define: Define.t Node.t
    -> Location.Instantiated.t
    -> t
    -> string list
  val inference_information:
    define: Define.t Node.t
    -> t
    -> Yojson.Safe.json
end


module type ERROR = sig
  type kind

  type t = {
    location: Location.Instantiated.t;
    kind: kind;
    define: Statement.Define.t Node.t;
  }
  [@@deriving compare, eq, show, hash]

  include Hashable with type t := t

  val create: location: Location.t -> kind: kind -> define: Statement.Define.t Node.t -> t

  val path: t -> string
  val location: t -> Location.Instantiated.t
  val key: t -> Location.t
  val code: t -> int
  val description: t -> detailed:bool -> string

  val to_json: detailed: bool -> t -> Yojson.Safe.json
end


module Make(Kind : KIND) = struct

  type kind = Kind.t

  type t = {
    location: Location.Instantiated.t;
    kind: Kind.t;
    define: Define.t Node.t;
  }
  [@@deriving compare, eq, show, sexp, hash]


  include Hashable.Make(struct
      type nonrec t = t
      let compare = compare
      let hash = hash
      let hash_fold_t = hash_fold_t
      let sexp_of_t = sexp_of_t
      let t_of_sexp = t_of_sexp
    end)


  let create ~location ~kind ~define  =
    {
      location = Location.instantiate ~lookup:(fun hash -> AstSharedMemory.get_path ~hash) location;
      kind;
      define
    }


  let path { location = { Location.path; _ }; _ } =
    path


  let location { location; _ } =
    location


  let key { location = { Location.start = { Location.line; _ }; path; _ }; _ } =
    let start = { Location.line; column = -1 } in
    { Location.start; stop = start; path }
    |> Location.reference


  let description
      ({
        kind;
        location;
        define;
        _;
      })
      ~detailed =
    let messages = Kind.messages ~detailed ~define location kind in
    Format.asprintf
      "%s [%d]: %s"
      (Kind.name kind)
      (Kind.code kind)
      (
        if detailed then
          String.concat ~sep:" " messages
        else
          List.nth_exn messages 0
      )


  let show error =
    Format.asprintf "%a" pp error


  let to_json
      ~detailed
      ({
        location =
          { Location.path; start = { Location.line = start_line; column = start_column }; _ };
        kind;
        define = ({ Node.value = define; _ } as define_node);
        _;
      } as error) =
    `Assoc ([
        "line", `Int (start_line);
        "column", `Int (start_column);
        "path", `String path;
        "code", `Int (Kind.code kind);
        "name", `String (Kind.name kind);
        "description", `String (description error ~detailed);
        "inference", (Kind.inference_information ~define:define_node kind);
        "define", `String (Access.show_sanitized define.Define.name);
      ])


  let code { kind; _ } =
    Kind.code kind

end
