(** Copyright 2016-present Facebook. All rights reserved. **)

open Core

open Ast
open Expression

type undefined_method = {
  annotation: Type.t;
  call: Annotated.Call.t;
}
[@@deriving compare, eq, show]

type mismatch = {
  actual: Type.t;
  expected: Type.t;
}
[@@deriving compare, eq, show]

type missing_parameter = {
  name: Identifier.t;
  annotation: Type.t;
  due_to_any: bool;
}
[@@deriving compare, eq, show]

type parameter_mismatch = {
  name: Identifier.t;
  position: int;
  callee: Statement.define;
  mismatch: mismatch;
}
[@@deriving compare, eq, show]

type missing_immutable = {
  name: Expression.access;
  annotation: Type.t;
  parent: Annotated.Class.t option;
  due_to_any: bool;
}
[@@deriving compare, eq, show]

type immutable_mismatch = {
  name: Expression.access;
  parent: Annotated.Class.t option;
  mismatch: mismatch;
  declare_location: Location.t;
}
[@@deriving compare, eq, show, sexp]

type override =
  | StrengthenedPrecondition
  | WeakenedPostcondition
[@@deriving compare, eq, show, sexp]

type inconsistent_override = {
  overridden_method: Annotated.Method.t;
  override: override;
  mismatch: mismatch;
}
[@@deriving compare, eq, show, sexp]

type missing_return = {
  type_annotation: Type.t;
  return_locations: Int.Set.t;
  due_to_any: bool;
}
[@@deriving compare, eq, sexp]

type initialization_mismatch = {
  name: Expression.access;
  parent: Annotated.Class.t;
  mismatch: mismatch;
}
[@@deriving compare, eq, show, sexp]

type kind =
  | IncompatibleAwaitableType of Type.t
  | IncompatibleParameterType of parameter_mismatch
  | IncompatibleReturnType of mismatch
  | IncompatibleType of immutable_mismatch
  | InconsistentOverride of inconsistent_override
  | MissingAnnotation of missing_immutable
  | MissingParameterAnnotation of missing_parameter
  | MissingReturnAnnotation of missing_return
  | Top
  | UndefinedMethod of undefined_method
  | UndefinedType of Type.t
  | UninitializedField of initialization_mismatch
[@@deriving compare, eq, show]

type t = {
  location: Location.t;
  kind: kind;
  define: Statement.define Node.t;
}
[@@deriving compare, eq, show]

include Hashable with type t := t

val location: t -> Location.t
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

val dequalify: access Instantiated.Access.Map.t -> (module Environment.Reader) -> t -> t

val to_json: detailed:bool -> t -> Yojson.Safe.json
