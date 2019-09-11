(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast

module TypeAnnotationsValue : sig
  type t = LocalAnnotationMap.t

  val prefix : Prefix.t

  val description : string

  val unmarshall : string -> t
end

include
  Memory.WithCache.S
    with type t = TypeAnnotationsValue.t
     and type key = SharedMemoryKeys.ReferenceKey.t
     and type key_out = SharedMemoryKeys.ReferenceKey.out
     and module KeySet = Caml.Set.Make(SharedMemoryKeys.ReferenceKey)
     and module KeyMap = MyMap.Make(SharedMemoryKeys.ReferenceKey)

module AnnotationsKeyValue : sig
  type t = Reference.t list

  val prefix : Prefix.t

  val description : string

  val unmarshall : string -> t
end

module Keys :
  Memory.NoCache.S
    with type t = AnnotationsKeyValue.t
     and type key = SharedMemoryKeys.ReferenceKey.t
     and type key_out = SharedMemoryKeys.ReferenceKey.out
     and module KeySet = Caml.Set.Make(SharedMemoryKeys.ReferenceKey)
     and module KeyMap = MyMap.Make(SharedMemoryKeys.ReferenceKey)

val add : qualifier:Reference.t -> Reference.t -> LocalAnnotationMap.t -> unit

val remove : Reference.t list -> unit

val get : Reference.t -> LocalAnnotationMap.t option

val get_keys : qualifiers:Reference.t list -> Reference.t list
