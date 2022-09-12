(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The purpose of the no-daemon-query is to be able to run a query without needing to start the
    Pyre server.

    For more information about querying Pyre in general, please consult the following docs:
    https://www.internalfb.com/intern/staticdocs/pyre/docs/querying-pyre/ **)

open Core

val command : Command.t
