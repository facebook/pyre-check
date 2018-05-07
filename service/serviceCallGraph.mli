(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Analysis


val in_process_handler: unit -> (module CallGraph.Handler)

val shared_memory_handler: unit -> (module CallGraph.Handler)
