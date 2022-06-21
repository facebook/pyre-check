(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val create : ?populate_call_graph:bool -> Configuration.Analysis.t -> t

val create_for_overlay : t -> t

val configuration : t -> Configuration.Analysis.t

val track_dependencies : t -> bool

val debug : t -> bool

val python_version_info : t -> int * int * int

val populate_call_graph : t -> bool
