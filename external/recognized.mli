(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


val classmethod_decorators: String.Set.t

val property_decorators: String.Set.t

val classproperty_decorators: String.Set.t

val enumeration_classes: String.Set.t

(* Names of functions that implement an 'assert'. *)
val assert_functions: String.Set.t
