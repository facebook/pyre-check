(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast

module NamedTuples = PluginNamedTuples


val apply: File.Handle.t List.t -> unit

val apply_to_ast: Source.t -> Source.t
