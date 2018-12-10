(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression


type origin =
  | Class of { annotation: Type.t; class_attribute: bool }
  | Module of Access.t
[@@deriving compare, eq, show, hash]

type mismatch = {
  actual: Type.t;
  expected: Type.t;
  due_to_invariance: bool;
}
[@@deriving compare, eq, show, hash]

type missing_annotation = {
  name: Access.t;
  annotation: Type.t option;
  evidence_locations: Location.Instantiated.t list;
  due_to_any: bool;
}
[@@deriving compare, eq, sexp, hash]

type incompatible_type = {
  name: Access.t;
  mismatch: mismatch;
  declare_location: Location.Instantiated.t;
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

type unpack_problem =
  | UnacceptableType of Type.t
  | CountMismatch of int
[@@deriving compare, eq, sexp, show, hash]

type kind =
  | AnalysisFailure of Type.t
  | ImpossibleIsinstance of { expression: Expression.t; mismatch: mismatch }
  | IncompatibleAwaitableType of Type.t
  | IncompatibleParameterType of {
      name: Access.t option;
      position: int;
      callee: Access.t option;
      mismatch: mismatch;
    }
  | IncompatibleConstructorAnnotation of Type.t
  | IncompatibleReturnType of { mismatch: mismatch; is_implicit: bool }
  | IncompatibleAttributeType of { parent: Type.t; incompatible_type: incompatible_type }
  | IncompatibleVariableType of incompatible_type
  | InconsistentOverride of { overridden_method: Access.t; parent: Access.t; override: override }
  | MissingArgument of { callee: Access.t option; name: Access.t }
  | MissingAttributeAnnotation of {
      parent: Type.t;
      missing_annotation: missing_annotation;
    }
  | MissingGlobalAnnotation of missing_annotation
  | MissingParameterAnnotation of { name: Access.t; annotation: Type.t; due_to_any: bool }
  | MissingReturnAnnotation of {
      annotation: Type.t;
      evidence_locations: int list;
      due_to_any: bool;
    }
  | MissingTypeParameters of { annotation: Type.t; number_of_parameters: int }
  | NotCallable of Type.t
  | RedundantCast of Type.t
  | RevealedType of { expression: Expression.t; annotation: Type.t }
  | TooManyArguments of { callee: Access.t option; expected: int; provided: int }
  | Unpack of { expected_count: int; unpack_problem: unpack_problem }
  | Top
  | UndefinedAttribute of { attribute: Access.t; origin: origin }
  | UndefinedImport of Access.t
  | UndefinedName of Access.t
  | UndefinedType of Type.t
  | UnexpectedKeyword of { name: Identifier.t; callee: Access.t option }
  | UninitializedAttribute of { name: Access.t; parent: Type.t; mismatch: mismatch }
  | UnusedIgnore of int list

  (* Additionals errors. *)
  | UnawaitedAwaitable of Access.t
  | TypedDictionaryAccessWithNonLiteral of string list
  | TypedDictionaryKeyNotFound of { typed_dictionary_name: Identifier.t; missing_key: string }
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

val dequalify: Access.t Access.Map.t -> resolution: Resolution.t -> t -> t

val create_mismatch
  :  resolution: Resolution.t
  -> actual: Type.t
  -> expected: Type.t
  -> covariant: bool
  -> mismatch
