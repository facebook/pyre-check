(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


module Callable = InterproceduralCallable
module Fixpoint = InterproceduralFixpoint


val one_analysis_pass: Fixpoint.step -> analyses:string list -> schedule:Callable.t list -> unit

val summaries: Callable.t -> Yojson.Safe.json list
