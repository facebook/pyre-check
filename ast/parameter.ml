(** Copyright 2016-present Facebook. All rights reserved. **)

open Core
open Sexplib.Std


type 'expression parameter = {
  name: Identifier.t;
  value: 'expression option;
  annotation: 'expression option;
}
[@@deriving compare, eq, sexp, show]


type 'expression t = 'expression parameter Node.t
[@@deriving compare, eq, sexp, show]


let create ?(location = Location.any) ?value ?annotation ~name () =
  {
    Node.location;
    value = {
      name;
      value;
      annotation;
    };
  }


let name { Node.value = { name; _ }; _ } = name
