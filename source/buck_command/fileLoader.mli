(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = { load: string -> (string, string) result }

val create : ?load:(string -> (string, string) result) -> unit -> t

val create_for_testing : (string * string) list -> t

val create_from_sourcedb_lookup : root:PyrePath.t -> Sourcedb.Lookup.t -> t
