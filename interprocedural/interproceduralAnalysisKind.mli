(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


type 'analysis kind = ..

type 'a internal_kind
val cast: 'a kind -> 'a internal_kind

type abstract
val abstract: 'a internal_kind -> abstract
val show: abstract -> string

module Map : Caml.Map.S with type key = abstract

type ('a,'b) equality_witness =
  | Equal: ('a, 'a) equality_witness
  | Distinct: ('a, 'b) equality_witness

val are_equal: 'a internal_kind -> 'b internal_kind -> ('a, 'b) equality_witness

val register: 'a kind -> name:string -> abstract
val analysis_by_name: string -> abstract option
