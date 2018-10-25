(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression


type class_origin = {
  annotation: Type.t;
  class_attribute: bool;
}
[@@deriving compare, eq, show, hash]

type origin =
  | Class of class_origin
  | Module of Access.t
[@@deriving compare, eq, show, hash]

type undefined_attribute = {
  attribute: Access.t;
  origin: origin;
}
[@@deriving compare, eq, show, hash]

type mismatch = {
  actual: Type.t;
  expected: Type.t;
}
[@@deriving compare, eq, show, hash]

type return_mismatch = {
  mismatch: mismatch;
  is_implicit: bool;
}
[@@deriving compare, eq, show, hash]

type missing_parameter = {
  name: Access.t;
  annotation: Type.t;
  due_to_any: bool;
}
[@@deriving compare, eq, show, hash]

type parameter_mismatch = {
  name: Access.t option;
  position: int;
  callee: Access.t option;
  mismatch: mismatch;
}
[@@deriving compare, eq, show, hash]

type missing_annotation = {
  name: Access.t;
  annotation: Type.t;
  evidence_locations: Location.Instantiated.t list;
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
  declare_location: Location.Instantiated.t;
}
[@@deriving compare, eq, show, sexp, hash]

type incompatible_attribute_type = {
  parent: Annotated.Class.t;
  incompatible_type: incompatible_type;
}
[@@deriving compare, eq, show, sexp, hash]

type precondition_mismatch =
  | Found of mismatch
  | NotFound of Access.t
[@@deriving compare, eq, show, sexp, hash]

type override =
  | StrengthenedPrecondition of precondition_mismatch
  | WeakenedPostcondition of mismatch
[@@deriving compare, eq, show, sexp, hash]

type inconsistent_override = {
  overridden_method: Access.t;
  parent: Access.t;
  override: override;
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

type too_many_arguments = {
  callee: Access.t option;
  expected: int;
  provided: int;
}
[@@deriving compare, eq, sexp, show, hash]

type missing_argument = {
  callee: Access.t option;
  name: Access.t
}
[@@deriving compare, eq, sexp, show, hash]

type revealed_type = {
  expression: Expression.t;
  annotation: Type.t;
}
[@@deriving compare, eq, sexp, show, hash]


type unpack_problem =
  | UnacceptableType of Type.t
  | CountMismatch of int
[@@deriving compare, eq, sexp, show, hash]


type unpack = {
  expected_count: int;
  unpack_problem: unpack_problem;
}
[@@deriving compare, eq, sexp, show, hash]


type missing_type_parameters = {
  annotation: Type.t;
  number_of_parameters: int;
}
[@@deriving compare, eq, sexp, show, hash]


type impossible_isinstance = {
  expression: Expression.t;
  mismatch: mismatch;
  negation: bool;
}
[@@deriving compare, eq, sexp, show, hash]


type kind =
  | ImpossibleIsinstance of impossible_isinstance
  | IncompatibleAwaitableType of Type.t
  | IncompatibleParameterType of parameter_mismatch
  | IncompatibleConstructorAnnotation of Type.t
  | IncompatibleReturnType of return_mismatch
  | IncompatibleAttributeType of incompatible_attribute_type
  | IncompatibleVariableType of incompatible_type
  | InconsistentOverride of inconsistent_override
  | MissingArgument of missing_argument
  | MissingAttributeAnnotation of missing_attribute_annotation
  | MissingGlobalAnnotation of missing_annotation
  | MissingParameterAnnotation of missing_parameter
  | MissingReturnAnnotation of missing_return
  | MissingTypeParameters of missing_type_parameters
  | RedundantCast of Type.t
  | RevealedType of revealed_type
  | TooManyArguments of too_many_arguments
  | Unpack of unpack
  | Top
  | UndefinedAttribute of undefined_attribute
  | UndefinedImport of Access.t
  | UndefinedName of Access.t
  | UndefinedType of Type.t
  | UninitializedAttribute of initialization_mismatch
  | UnusedIgnore of int list

  (* Additionals errors. *)
  | UnawaitedAwaitable of Access.t
[@@deriving compare, eq, show, hash]

include BaseError.ERROR with type kind := kind

val due_to_analysis_limitations: t -> bool
val due_to_mismatch_with_any: t -> bool

val less_or_equal: resolution: Resolution.t -> t -> t -> bool
val join: resolution: Resolution.t -> t -> t -> t
val meet: resolution: Resolution.t -> t -> t -> t
val widen: resolution: Resolution.t -> previous: t -> next: t -> iteration: int -> t

val join_at_define
  :  resolution: Resolution.t
  -> location: Location.Instantiated.t
  -> t list
  -> t list

val join_at_source: resolution: Resolution.t -> t list -> t list

val filter: configuration: Configuration.Analysis.t -> resolution: Resolution.t -> t list -> t list
val suppress: mode: Source.mode -> t -> bool

val dequalify: Access.t Access.Map.t -> (module Environment.Handler) -> t -> t
