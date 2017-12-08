(** Copyright 2016-present Facebook. All rights reserved. **)

open Core

exception ParserError of string

module Log = Log
module Path = PyrePath


let (>>|) =
  Option.(>>|)


let (>>=) =
  Option.(>>=)
