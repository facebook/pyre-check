(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

val add : Reference.t -> (Reference.t * LocalAnnotationMap.t) list -> unit

val remove : Reference.t list -> unit

val get : Reference.t -> (Reference.t * LocalAnnotationMap.t) list option

val get_local_annotation_map : qualifier:Reference.t -> Reference.t -> LocalAnnotationMap.t option
