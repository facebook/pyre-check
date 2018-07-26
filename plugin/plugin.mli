(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Analysis
open Environment

module NamedTuples = PluginNamedTuples
module NewType = PluginNewType
module DataClass = PluginDataClass
module Filter = PluginFilter

val apply_to_ast: Source.t -> Source.t

val apply_to_environment: (module Handler) -> Source.t -> unit
