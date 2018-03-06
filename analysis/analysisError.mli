(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression

module Annotated = AnalysisAnnotated
module Environment = AnalysisEnvironment
module Resolution = AnalysisResolution
module Type = AnalysisType

type undefined_function = {
  annotation: Type.t option;
  call: Annotated.Call.t;
}
[@@deriving compare, eq, show, hash]

type undefined_attribute = {
  annotation: Type.t;
  attribute: Access.t;
  class_attribute: bool;
}
[@@deriving compare, eq, show, hash]

type mismatch = {
  actual: Type.t;
  expected: Type.t;
}
[@@deriving compare, eq, show, hash]

type missing_parameter = {
  name: Identifier.t;
  annotation: Type.t;
  due_to_any: bool;
}
[@@deriving compare, eq, show, hash]

type parameter_mismatch = {
  name: Identifier.t;
  position: int;
  callee: Statement.Define.t;
  mismatch: mismatch;
}
[@@deriving compare, eq, show, hash]

type missing_annotation = {
  name: Access.t;
  annotation: Type.t;
  evidence_locations: Location.t list;
  due_to_any: bool;
}
[@@deriving compare, eq, sexp, hash]

type missing_attribute_annotation = {
  parent: Annotated.Class.t;
  missing_annotation: missing_annotation;
}
[@@deriving compare, eq, sexp, hash]

type incompatible_type = {
  name: Access.t;
  mismatch: mismatch;
  declare_location: Location.t;
}
[@@deriving compare, eq, show, sexp, hash]

type incompatible_attribute_type = {
  parent: Annotated.Class.t;
  incompatible_type: incompatible_type;
}
[@@deriving compare, eq, show, sexp, hash]

type override =
  | StrengthenedPrecondition
  | WeakenedPostcondition
[@@deriving compare, eq, show, sexp, hash]

type inconsistent_override = {
  overridden_method: Annotated.Method.t;
  override: override;
  mismatch: mismatch;
}
[@@deriving compare, eq, show, sexp, hash]

type missing_return = {
  annotation: Type.t;
  evidence_locations: int list;
  due_to_any: bool;
}
[@@deriving compare, eq, sexp, hash]

type initialization_mismatch = {
  name: Access.t;
  parent: Annotated.Class.t;
  mismatch: mismatch;
}
[@@deriving compare, eq, show, sexp, hash]

type kind =
  | IncompatibleAwaitableType of Type.t
  | IncompatibleParameterType of parameter_mismatch
  | IncompatibleReturnType of mismatch
  | IncompatibleAttributeType of incompatible_attribute_type
  | IncompatibleVariableType of incompatible_type
  | InconsistentOverride of inconsistent_override
  | MissingAttributeAnnotation of missing_attribute_annotation
  | MissingGlobalAnnotation of missing_annotation
  | MissingParameterAnnotation of missing_parameter
  | MissingReturnAnnotation of missing_return
  | Top
  | UndefinedAttribute of undefined_attribute
  | UndefinedFunction of undefined_function
  | UndefinedType of Type.t
  | UninitializedAttribute of initialization_mismatch
  | UnusedIgnore of int list
[@@deriving compare, eq, show, hash]

type t = {
  location: Location.t;
  kind: kind;
  define: Statement.Define.t Node.t;
}
[@@deriving compare, eq, show, hash]

include Hashable with type t := t

val location: t -> Location.t
val key: t -> Location.t
val code: t -> int
val description: t -> detailed:bool -> string

val due_to_analysis_limitations: t -> bool
val due_to_mismatch_with_any: t -> bool

val less_or_equal: resolution:Resolution.t -> t -> t -> bool
val join: resolution:Resolution.t -> t -> t -> t
val meet: resolution:Resolution.t -> t -> t -> t
val widen
  :  resolution:Resolution.t
  -> previous:t
  -> next:t
  -> iteration:int
  -> t

val join_at_define: resolution: Resolution.t -> location: Location.t -> t list -> t list
val join_at_source: resolution:Resolution.t -> t list -> t list

val process_ignores: (module Environment.Handler) -> File.Handle.t list -> t list -> t list

val dequalify: Access.t Access.Map.t -> (module Environment.Handler) -> t -> t

val to_json: detailed:bool -> t -> Yojson.Safe.json
