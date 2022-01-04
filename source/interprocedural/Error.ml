(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast

type kind = {
  code: int;
  name: string;
  messages: string list;
}
[@@deriving sexp, compare]

type t = {
  location: Location.WithModule.t;
  kind: kind;
  define_name: Reference.t;
}
[@@deriving sexp, compare]

let create ~location ~kind ~define =
  let {
    Node.value =
      { Statement.Define.signature = { Statement.Define.Signature.name = define_name; _ }; _ };
    _;
  }
    =
    define
  in
  { location; kind; define_name }


let code { kind = { code; _ }; _ } = code

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
    define: string;
  }
  [@@deriving sexp, compare, yojson { strict = false }]

  let location { line; column; stop_line; stop_column; path; _ } =
    { Location.start = { line; column }; stop = { line = stop_line; column = stop_column } }
    |> Location.with_path ~path


  let code { code; _ } = code

  let description { description; _ } = description

  let create
      ~location:
        {
          Location.WithPath.path;
          start = { Location.line = start_line; column = start_column };
          stop = { Location.line = stop_line; column = stop_column };
        }
      ~kind:{ code; name; messages }
      ~define_name
      ~show_error_traces
      ()
    =
    let description ~show_error_traces =
      Format.asprintf
        "%s [%d]: %s"
        name
        code
        (if show_error_traces then
           String.concat ~sep:" " messages
        else
          List.nth_exn messages 0)
    in
    {
      line = start_line;
      column = start_column;
      stop_line;
      stop_column;
      path;
      code;
      name;
      description = description ~show_error_traces;
      define = Reference.show_sanitized (Reference.delocalize define_name);
    }
end

let instantiate ~show_error_traces ~lookup { location; kind; define_name } =
  Instantiated.create
    ~location:(Location.WithModule.instantiate ~lookup location)
    ~kind
    ~define_name
    ~show_error_traces
    ()
