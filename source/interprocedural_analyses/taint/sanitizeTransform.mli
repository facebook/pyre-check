(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | NamedSource of string
  | NamedSink of string
[@@deriving compare, eq]

module Set : sig
  include Stdlib.Set.S with type elt = t

  val pp : Format.formatter -> t -> unit

  val show : t -> string

  val filter_sources : t -> t

  val filter_sinks : t -> t
end

val pp_kind
  :  formatter:Format.formatter ->
  pp_base:(Format.formatter -> 'a -> unit) ->
  local:Set.t ->
  global:Set.t ->
  base:'a ->
  unit
