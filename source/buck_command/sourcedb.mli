(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  lookup_source: string -> string option;
  lookup_dependency: string -> string option;
  all_sources: unit -> string list;
  all_dependencies: unit -> string list;
}

val create_for_testing
  :  ?lookup_source:(string -> string option) ->
  ?lookup_dependency:(string -> string option) ->
  ?all_sources:(unit -> string list) ->
  ?all_dependencies:(unit -> string list) ->
  unit ->
  t

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
