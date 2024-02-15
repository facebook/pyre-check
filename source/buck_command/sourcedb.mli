(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Lookup : sig
  type t = {
    get_source: string -> string option;
    get_dependency: string -> string option;
  }

  val create
    :  ?get_source:(string -> string option) ->
    ?get_dependency:(string -> string option) ->
    unit ->
    t

  val create_for_testing
    :  ?sources:(string * string) list ->
    ?dependencies:(string * string) list ->
    unit ->
    t
end

module Listing : sig
  type t = {
    all_sources: unit -> string list;
    all_dependencies: unit -> string list;
  }

  val create
    :  ?all_sources:(unit -> string list) ->
    ?all_dependencies:(unit -> string list) ->
    unit ->
    t

  val create_for_testing : ?sources:string list -> ?dependencies:string list -> unit -> t
end

type t = {
  lookup: Lookup.t;
  listing: Listing.t;
}

val create : ?lookup:Lookup.t -> ?listing:Listing.t -> unit -> t

val create_from_alists
  :  source_alists:(string * string) list list ->
  dependency_alists:(string * string) list list ->
  typeshed_alists:(string * string) list list ->
  unit ->
  t

val create_from_manifests
  :  source_manifests:Manifest.t list ->
  dependency_manifests:Manifest.t list ->
  typeshed_manifests:Manifest.t list ->
  unit ->
  t
