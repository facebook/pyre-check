(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Annotation

val refine : resolution:GlobalResolution.t -> t -> Type.t -> t

val less_or_equal : resolution:GlobalResolution.t -> t -> t -> bool

val join : resolution:GlobalResolution.t -> t -> t -> t

val meet : resolution:GlobalResolution.t -> t -> t -> t

val widen
  :  resolution:GlobalResolution.t ->
  widening_threshold:int ->
  previous:t ->
  next:t ->
  iteration:int ->
  t
