(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = { configuration: Configuration.Analysis.t }

val create : Configuration.Analysis.t -> t

val configuration : t -> Configuration.Analysis.t

val track_dependencies : t -> bool

val debug : t -> bool

val python_version_info : t -> int * int * int
