(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

type 'analysis kind = ..

(* A 1:1 kind that can be stored in the shared heap. *)
type 'a storable_kind

val cast : 'a kind -> 'a storable_kind

(* An existentially abstracted kind for uniform lists. *)
type abstract

val abstract : 'a storable_kind -> abstract

val show : abstract -> string

module Map : Caml.Map.S with type key = abstract

type ('a, 'b) equality_witness =
  | Equal : ('a, 'a) equality_witness
  | Distinct : ('a, 'b) equality_witness

val are_equal : 'a storable_kind -> 'b storable_kind -> ('a, 'b) equality_witness

val register : 'a kind -> name:string -> abstract

val analysis_by_name : string -> abstract option
