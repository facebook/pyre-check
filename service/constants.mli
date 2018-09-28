(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Pyre


module Persistent: sig
  val log_path: Configuration.Analysis.t -> Path.t
end

module Server: sig
  val root: Configuration.Analysis.t -> Path.t

  val log_path: Configuration.Analysis.t -> Path.t

  val saved_state_path: Configuration.Analysis.t -> Path.t
end

module Watchman: sig
  val watchman_root: Configuration.Analysis.t -> Path.t

  val lock_path: Configuration.Analysis.t -> Path.t

  val pid_path: Configuration.Analysis.t -> Path.t

  val log_path: Configuration.Analysis.t -> Path.t
end
