(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Pyre


let analyze ~scheduler:_ ~configuration:_ ~environment:_ ~call_graph:_ ~handles:_ =
  Log.print "Analysis";
