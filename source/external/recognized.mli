(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

val asyncio_contextmanager_decorators : String.Set.t

val classmethod_decorators : String.Set.t

val property_decorators : String.Set.t

val classproperty_decorators : String.Set.t

val enumeration_classes : String.Set.t

(* Names of functions that implement an 'assert'. *)
val assert_functions : String.Set.t

val lazy_import_functions : String.Set.t

val allowlisted_callable_class_decorators : String.Set.t
