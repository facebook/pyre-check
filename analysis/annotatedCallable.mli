(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Statement


val is_generator: Define.t -> bool

val return_annotation: define: Define.t -> resolution: Resolution.t -> Type.t

val create_overload: resolution: Resolution.t -> define: Define.t -> Type.t Type.Callable.overload
val create
  :  parent: Type.t option
  -> name: Identifier.t
  -> (bool * Type.t Type.Callable.overload) list
  -> Type.Callable.t

val apply_decorators: define: Define.t -> resolution: Resolution.t -> Type.t Type.Callable.overload
