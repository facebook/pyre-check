(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The Caml module binds everything that is available in the standard
   environment so that we can easily refer to standard things even if they've
   been rebound.
*)

module Arg = Arg
module Array = Array
module ArrayLabels = ArrayLabels
module Buffer = Buffer
module Callback = Callback
module Char = Char
module Complex = Complex
module Digest = Digest
module Filename = Filename
module Format = Format
module Gc = Gc
module Hashtbl = Hashtbl
module Int32 = Int32
module Int64 = Int64
module Lazy = Lazy
module Lexing = Lexing
module List = List
module ListLabels = ListLabels
module Map = Map
module Marshal = Marshal
module MoreLabels = MoreLabels
module Nativeint = Nativeint
module Oo = Oo
module Parsing = Parsing
module Stdlib = Stdlib
module Printexc = Printexc
module Printf = Printf
module Queue = Queue
module Random = Random
module Scanf = Scanf
module Set = Set
module Stack = Stack
module StdLabels = StdLabels
module String = String
module StringLabels = StringLabels
module Sys = Sys
module Weak = Weak
