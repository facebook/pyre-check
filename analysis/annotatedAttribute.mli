(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

type read_only =
  | Refinable of { overridable: bool }
  | Unrefinable
[@@deriving eq, show, compare, sexp]

type visibility =
  | ReadOnly of read_only
  | ReadWrite
[@@deriving eq, show, compare, sexp]

type 'a t [@@deriving eq, show, compare, sexp]

type instantiated_annotation

type instantiated = instantiated_annotation t [@@deriving eq, show, compare, sexp]

val create
  :  abstract:bool ->
  annotation:Type.t ->
  original_annotation:Type.t ->
  async:bool ->
  class_attribute:bool ->
  defined:bool ->
  initialized:bool ->
  name:Identifier.t ->
  parent:Type.Primitive.t ->
  visibility:visibility ->
  property:bool ->
  static:bool ->
  value:Expression.t ->
  location:Location.t ->
  instantiated

val create_uninstantiated
  :  abstract:bool ->
  uninstantiated_annotation:'a ->
  async:bool ->
  class_attribute:bool ->
  defined:bool ->
  initialized:bool ->
  name:Identifier.t ->
  parent:Type.Primitive.t ->
  visibility:visibility ->
  property:bool ->
  static:bool ->
  value:Expression.t ->
  location:Location.t ->
  'a t

val annotation : instantiated -> Annotation.t

val uninstantiated_annotation : 'a t -> 'a

val name : 'a t -> Identifier.t

val abstract : 'a t -> bool

val async : 'a t -> bool

val parent : 'a t -> Type.Primitive.t

val value : 'a t -> Expression.t

val initialized : 'a t -> bool

val location : 'a t -> Location.t

val defined : 'a t -> bool

val class_attribute : 'a t -> bool

val static : 'a t -> bool

val property : 'a t -> bool

val visibility : 'a t -> visibility

val with_value : 'a t -> value:Expression.t -> 'a t

val with_location : 'a t -> location:Location.t -> 'a t

val instantiate : 'a t -> annotation:Type.t -> original_annotation:Type.t -> instantiated

(* For testing *)
val ignore_callable_define_locations : instantiated -> instantiated
