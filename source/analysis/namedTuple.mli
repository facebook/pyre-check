(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val field_annotations : global_resolution:GlobalResolution.t -> Type.t -> Type.t list option

val field_names : global_resolution:GlobalResolution.t -> Type.t -> string list option

val is_named_tuple : global_resolution:GlobalResolution.t -> annotation:Type.t -> bool
