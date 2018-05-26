(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open Statement

module Environment = AnalysisEnvironment
module Resolution = AnalysisResolution
module Type = AnalysisType
module TypeOrder = AnalysisTypeOrder


type class_origin = {
  annotation: Type.t;
  class_attribute: bool;
}
[@@deriving compare, eq, show, sexp, hash]


type origin =
  | Class of class_origin
  | Module of Access.t
[@@deriving compare, eq, show, sexp, hash]


type undefined_attribute = {
  attribute: Access.t;
  origin: origin;
}
[@@deriving compare, eq, show, sexp, hash]


type mismatch = {
  actual: Type.t;
  expected: Type.t;
}
[@@deriving compare, eq, show, sexp, hash]


type missing_parameter = {
  name: Access.t;
  annotation: Type.t;
  due_to_any: bool;
}
[@@deriving compare, eq, show, sexp, hash]


type parameter_mismatch = {
  name: Access.t option;
  position: int;
  callee: Access.t option;
  mismatch: mismatch;
}
[@@deriving compare, eq, show, sexp, hash]


type missing_annotation = {
  name: Access.t;
  annotation: Type.t;
  evidence_locations: Location.t list;
  due_to_any: bool;
}
[@@deriving compare, eq, sexp, show, hash]


type missing_attribute_annotation = {
  parent: Annotated.Class.t;
  missing_annotation: missing_annotation;
}
[@@deriving compare, eq, sexp, show, hash]


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


type initialization_mismatch = {
  name: Access.t;
  parent: Annotated.Class.t;
  mismatch: mismatch;
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
  overridden_method: Annotated.Method.t;
  override: override;
}
[@@deriving compare, eq, show, sexp, hash]


type missing_return = {
  annotation: Type.t;
  evidence_locations: int list;
  due_to_any: bool;
}
[@@deriving compare, eq, sexp, show, hash]


type too_many_arguments = {
  callee: Access.t option;
  expected: int;
  provided: int;
}
[@@deriving compare, eq, sexp, show, hash]


type missing_argument = {
  callee: Access.t option;
  name: Access.t;
}
[@@deriving compare, eq, sexp, show, hash]


type kind =
  | IncompatibleAwaitableType of Type.t
  | IncompatibleParameterType of parameter_mismatch
  | IncompatibleConstructorAnnotation of Type.t
  | IncompatibleReturnType of mismatch
  | IncompatibleAttributeType of incompatible_attribute_type
  | IncompatibleVariableType of incompatible_type
  | InconsistentOverride of inconsistent_override
  | MissingArgument of missing_argument
  | MissingAttributeAnnotation of missing_attribute_annotation
  | MissingGlobalAnnotation of missing_annotation
  | MissingParameterAnnotation of missing_parameter
  | MissingReturnAnnotation of missing_return
  | RedundantCast of Type.t
  | RevealedType of Type.t * Ast.Expression.t
  | TooManyArguments of too_many_arguments
  | Top
  | UndefinedAttribute of undefined_attribute
  | UndefinedImport of Access.t
  | UndefinedName of Access.t
  | UndefinedType of Type.t
  | UninitializedAttribute of initialization_mismatch
  | UnusedIgnore of int list
[@@deriving compare, eq, show, sexp, hash]


type t = {
  location: Location.t;
  kind: kind;
  define: Define.t Node.t;
}
[@@deriving compare, eq, show, sexp, hash]


include Hashable.Make(struct
    type nonrec t = t
    let compare = compare
    let hash = hash
    let hash_fold_t = hash_fold_t
    let sexp_of_t = sexp_of_t
    let t_of_sexp = t_of_sexp
  end)


let location { location; _ } =
  location


let key { location; _ } =
  let start = { Location.line = (Location.line location); column = -1 } in
  { location with Location.start; stop = start }


let code { kind; _ } =
  match kind with
  | RevealedType _ -> -1
  | UnusedIgnore _ -> 0
  | Top -> 1
  | MissingParameterAnnotation _ -> 2
  | MissingReturnAnnotation _ -> 3
  | MissingAttributeAnnotation _ -> 4
  | MissingGlobalAnnotation _ -> 5
  | IncompatibleParameterType _ -> 6
  | IncompatibleReturnType _ -> 7
  | IncompatibleAttributeType _ -> 8
  | IncompatibleVariableType _ -> 9
  | UndefinedType _ -> 11
  | IncompatibleAwaitableType _ -> 12
  | UninitializedAttribute _ -> 13
  | InconsistentOverride { override; _ } ->
      begin
        match override with
        | StrengthenedPrecondition _ -> 14
        | WeakenedPostcondition _ -> 15
      end
  | UndefinedAttribute _ -> 16
  | IncompatibleConstructorAnnotation _ -> 17
  | UndefinedName _ -> 18
  | TooManyArguments _ -> 19
  | MissingArgument _ -> 20
  | UndefinedImport _ -> 21
  | RedundantCast _ -> 22


let name { kind; _ } =
  match kind with
  | IncompatibleAwaitableType _ -> "Incompatible awaitable type"
  | IncompatibleParameterType _ -> "Incompatible parameter type"
  | IncompatibleConstructorAnnotation _ -> "Incompatible constructor annotation"
  | IncompatibleReturnType _ -> "Incompatible return type"
  | IncompatibleAttributeType _ -> "Incompatible attribute type"
  | IncompatibleVariableType _ -> "Incompatible variable type"
  | InconsistentOverride _ -> "Inconsistent override"
  | MissingArgument _ -> "Missing argument"
  | MissingAttributeAnnotation _ -> "Missing attribute annotation"
  | MissingGlobalAnnotation _ -> "Missing global annotation"
  | MissingParameterAnnotation _ -> "Missing parameter annotation"
  | MissingReturnAnnotation _ -> "Missing return annotation"
  | RevealedType _ -> "Revealed type"
  | RedundantCast _ -> "Redundant cast"
  | TooManyArguments _ -> "Too many arguments"
  | Top -> "Undefined error"
  | UndefinedAttribute _ -> "Undefined attribute"
  | UndefinedName _ -> "Undefined name"
  | UndefinedType _ -> "Undefined type"
  | UndefinedImport _ -> "Undefined import"
  | UninitializedAttribute _ -> "Uninitialized attribute"
  | UnusedIgnore _ -> "Unused ignore"


let description
    ({
      kind;
      location;
      define = { Node.value = { Define.name = define_name; _ }; _ };
    } as error)
    ~detailed =
  let ordinal number =
    let suffix =
      if (number % 10 = 1) && (number % 100 <> 11) then "st"
      else if (number % 10 = 2) && (number % 100 <> 12) then "nd"
      else if (number % 10 = 3) && (number % 100 <> 13) then "rd"
      else "th"
    in
    (string_of_int number) ^ suffix
  in
  let message kind =
    match kind with
    | IncompatibleAwaitableType actual ->
        [
          (Format.asprintf
             "Expected an awaitable but got %a."
             Type.pp actual
          );
        ]
    | Top -> [ "Problem with analysis." ]
    | MissingParameterAnnotation { name; annotation; due_to_any = false } ->
        [
          Format.asprintf
            "Parameter `%s` has type %a but no type is specified."
            (Access.show_sanitized name)
            Type.pp annotation
        ]
    | MissingParameterAnnotation { name; annotation; due_to_any = true } ->
        [
          Format.asprintf
            "Parameter `%s` has type %a but type `Any` is specified."
            (Access.show_sanitized name)
            Type.pp annotation
        ]
    | MissingReturnAnnotation { annotation; evidence_locations; due_to_any } ->
        begin
          match due_to_any with
          | false ->
              [
                (Format.asprintf
                   "Returning %a but no return type is specified."
                   Type.pp annotation);
                (Format.asprintf
                   "Type %a was returned on %s %s, return type should be specified on line %d."
                   Type.pp annotation
                   (if (List.length evidence_locations) > 1 then "lines" else "line")
                   (evidence_locations
                    |> List.map
                      ~f:Int.to_string
                    |> String.concat ~sep:", ")
                   (Location.line location))
              ]
          | true ->
              [
                (Format.asprintf
                   "Returning %a but type `Any` is specified."
                   Type.pp annotation);
                (Format.asprintf
                   "Type %a was returned on %s %s, return type should be specified on line %d."
                   Type.pp annotation
                   (if (List.length evidence_locations) > 1 then "lines" else "line")
                   (evidence_locations
                    |> List.map
                      ~f:Int.to_string
                    |> String.concat ~sep:", ")
                   (Location.line location))
              ]
        end
    | MissingAttributeAnnotation {
        parent;
        missing_annotation = {
          name;
          annotation;
          evidence_locations;
          due_to_any;
        };
      } ->
        begin
          let evidence_string =
            evidence_locations
            |> List.map
              ~f:(Format.asprintf "%a" Location.pp_start)
            |> String.concat ~sep:", "
          in
          if due_to_any then
            [
              Format.asprintf
                "Attribute `%a` of class `%a` has type %a but type `Any` is specified."
                Access.pp name
                Access.pp (Annotated.Class.name parent)
                Type.pp annotation;
              Format.asprintf
                "Attribute `%a` declared on line %d, type %a deduced from %s."
                Access.pp name
                (Location.line location)
                Type.pp annotation
                evidence_string
            ]
          else
            [
              Format.asprintf
                "Attribute `%a` of class `%a` has type %a but no type is specified."
                Access.pp name
                Access.pp (Annotated.Class.name parent)
                Type.pp annotation;
              Format.asprintf
                "Attribute `%a` declared on line %d, type %a deduced from %s."
                Access.pp name
                (Location.line location)
                Type.pp annotation
                evidence_string
            ]
        end
    | MissingGlobalAnnotation {
        name;
        annotation;
        evidence_locations;
        due_to_any;
      } ->
        begin
          let evidence_string =
            evidence_locations
            |> List.map
              ~f:(Format.asprintf "%a" Location.pp_start)
            |> String.concat ~sep:", "
          in
          if due_to_any then
            [
              Format.asprintf
                "Globally accessible variable `%a` has type %a but type `Any` is specified."
                Access.pp name
                Type.pp annotation;
              Format.asprintf
                "Global variable `%a` declared on line %d, type %a deduced from %s."
                Access.pp name
                (Location.line location)
                Type.pp annotation
                evidence_string
            ]
          else
            [
              Format.asprintf
                "Globally accessible variable `%a` has type %a but no type is specified."
                Access.pp name
                Type.pp annotation;
              Format.asprintf
                "Global variable `%a` declared on line %d, type %a deduced from %s."
                Access.pp name
                (Location.line location)
                Type.pp annotation
                evidence_string
            ]
        end
    | IncompatibleParameterType { name; position; callee; mismatch = { actual; expected } } ->
        let evidence =
          let parameter =
            match name with
            | Some name ->
                Access.show_sanitized name
                |> Format.asprintf "parameter `%s`"
            | _ ->
                "anonymous parameter"
          in
          let callee =
            match callee with
            | Some callee ->
                Format.asprintf "call `%a`." Access.pp callee
            | _ ->
                "anoynmous call"
          in
          Format.asprintf "%s %s to %s" (ordinal position) parameter callee
        in
        [Format.asprintf "Expected %a but got %a." Type.pp expected Type.pp actual; evidence]
    | IncompatibleConstructorAnnotation annotation ->
        [
          Format.asprintf
            "`__init__` is annotated as returning %a, but it should return `None`."
            Type.pp
            annotation;
        ]
    | IncompatibleReturnType { actual; expected } ->
        [
          (Format.asprintf
             "Expected %a but got %a."
             Type.pp expected
             Type.pp actual);
          (Format.asprintf
             "Type %a expected on line %d, specified on line %d."
             Type.pp expected
             error.location.Location.stop.Location.line
             error.define.Node.location.Location.start.Location.line)
        ]
    | IncompatibleAttributeType {
        parent;
        incompatible_type = {
          name;
          mismatch = { actual; expected };
          declare_location;
        };
      } ->
        [
          (Format.asprintf
             "Attribute `%a` declared in class `%a` has type %a but is used as type %a."
             Access.pp name
             Access.pp (Annotated.Class.name parent)
             Type.pp expected
             Type.pp actual);
          (Format.asprintf
             "Attribute `%a` declared on line %d, incorrectly used on line %d."
             Access.pp name
             declare_location.Location.start.Location.line
             (Location.line location))
        ]
    | IncompatibleVariableType {
        name;
        mismatch = { actual; expected };
        _;
      } ->
        let message =
          if Type.is_tuple expected && not (Type.is_tuple actual) then
            Format.asprintf "Unable to unpack %a, expected a `Tuple`." Type.pp actual
          else
            Format.asprintf
              "%s is declared to have type %a but is used as type %a."
              (Access.show_sanitized name)
              Type.pp expected
              Type.pp actual

        in
        [
          message;
          (Format.asprintf
             "%s incorrectly used on line %d."
             (Access.show_sanitized name)
             (Location.line location))
        ]
    | InconsistentOverride { overridden_method; override } ->
        let detail =
          match override with
          | WeakenedPostcondition { actual; expected } ->
              Format.asprintf
                "Returned type %a is not a subtype of the overridden return %a."
                Type.pp actual
                Type.pp expected
          | StrengthenedPrecondition (Found { actual; expected }) ->
              Format.asprintf
                "Parameter of type %a is not a supertype of the overridden parameter %a."
                Type.pp actual
                Type.pp expected
          | StrengthenedPrecondition (NotFound name) ->
              Format.asprintf
                "Could not find parameter `%s` in overriding signature."
                (Access.show_sanitized name)
        in
        [
          Format.asprintf
            "`%a` overloads method defined in `%a` inconsistently. %s"
            Access.pp define_name
            Access.pp
            (Annotated.Method.parent overridden_method |> Annotated.Class.name)
            detail
        ]
    | MissingArgument { callee; name } ->
        let callee =
          match callee with
          | Some name ->
              Format.asprintf "Call `%a`" Access.pp name
          | _ ->
              "Anonymous call"
        in
        [Format.asprintf "%s expects argument `%s`." callee (Access.show_sanitized name)]
    | TooManyArguments { callee; expected; provided } ->
        let callee =
          match callee with
          | Some name ->
              Format.asprintf "Call `%a`" Access.pp name
          | _ ->
              "Anonymous call"
        in
        [
          Format.asprintf "%s expects %d positional argument%s, %d %s provided."
            callee
            expected
            (if expected <> 1 then "s" else "")
            provided
            (if provided > 1 then "were" else "was");
        ]
    | RedundantCast annotation ->
        [
          Format.asprintf
            "The value being cast is already of type %a."
            Type.pp annotation;
        ]
    | RevealedType (annotation, expression) ->
        let show_sanitized { Node.location; value } =
          match value with
          | Access access ->
              Access.show_sanitized access
          | _ ->
              Expression.show { Node.location; value }
        in
        [
          Format.asprintf
            "Revealed type for `%s` is %s."
            (show_sanitized expression)
            (Type.show annotation);
        ]
    | UndefinedAttribute { attribute; origin } ->
        let target =
          match origin with
          | Class { annotation; _ } ->
              let name =
                if Type.is_optional_primitive annotation then
                  "Optional type"
                else
                  Type.show annotation
              in
              name
          | Module access ->
              (Format.asprintf "Module `%a`" Access.pp access)
        in
        let detail =
          match origin with
          | Class { class_attribute; _ } when class_attribute ->
              [
                "This attribute is accessed as a class variable; did you mean to declare it with " ^
                "`typing.ClassVar`?";
              ]
          | _ ->
              []
        in
        [Format.asprintf "%s has no attribute `%s`." target (Access.show_sanitized attribute)]
        @ detail
    | UndefinedName access ->
        [Format.asprintf "Global name `%s` is undefined." (Access.show_sanitized access)]
    | UndefinedImport access ->
        [
          Format.asprintf
            "Could not find a module corresponding to import `%s`."
            (Access.show_sanitized access);
        ]
    | UndefinedType annotation ->
        [
          Format.asprintf
            "Type %a is not defined."
            Type.pp annotation
        ]
    | UninitializedAttribute {
        name;
        parent;
        mismatch = { actual; expected };
      } ->
        [
          (Format.asprintf
             "Attribute `%a` is declared in class `%a` to have non-optional type %a but is never \
              initialized."
             Access.pp name
             Access.pp (Annotated.Class.name parent)
             Type.pp expected);
          (Format.asprintf
             "Attribute `%a` is declared on line %d, never initialized and therefore must be %a."
             Access.pp name
             (Location.line location)
             Type.pp actual)
        ]
    | UnusedIgnore codes ->
        let string_from_codes codes =
          if List.length codes > 0 then
            List.map ~f:Int.to_string codes
            |> String.concat ~sep:", "
            |> Format.asprintf "[%s] "
          else
            ""
        in
        let plural = List.length codes > 1 in
        [
          Format.asprintf
            "Pyre ignore%s %s%s extraneous."
            (if plural then "s" else "")
            (string_from_codes codes)
            (if plural then "are" else "is")
        ]
  in
  let messages = message kind in
  Format.asprintf
    "%s [%d]: %s"
    (name error)
    (code error)
    (
      if detailed then
        String.concat ~sep:" " messages
      else
        List.nth_exn messages 0
    )


let show error =
  Format.asprintf "%a" pp error


let due_to_analysis_limitations { kind; _ } =
  match kind with
  | IncompatibleAwaitableType actual
  | IncompatibleParameterType { mismatch = { actual; _ }; _ }
  | IncompatibleReturnType { actual; _ }
  | IncompatibleAttributeType { incompatible_type = { mismatch = { actual; _ }; _ }; _ }
  | IncompatibleVariableType { mismatch = { actual; _ }; _ }
  | InconsistentOverride { override = StrengthenedPrecondition (Found { actual; _ }); _ }
  | InconsistentOverride { override = WeakenedPostcondition { actual; _ }; _ }
  | MissingAttributeAnnotation { missing_annotation = { annotation = actual; _ }; _ }
  | MissingGlobalAnnotation { annotation = actual; _ }
  | MissingParameterAnnotation { annotation = actual; _ }
  | MissingReturnAnnotation { annotation = actual; _ }
  | RedundantCast actual
  | UninitializedAttribute { mismatch = {actual; _ }; _ }->
      Type.is_unknown actual
  | Top -> true
  | UndefinedAttribute { origin = Class { annotation; _ }; _ }
  | UndefinedType annotation ->
      Type.is_unknown annotation
  | IncompatibleConstructorAnnotation _
  | InconsistentOverride { override = StrengthenedPrecondition (NotFound _); _ }
  | MissingArgument _
  | TooManyArguments _
  | RevealedType _
  | UndefinedAttribute _
  | UndefinedName _
  | UndefinedImport _
  | UnusedIgnore _ ->
      false


let due_to_unsupported_calls { kind; _ } =
  match kind with
  | MissingArgument { callee = Some name; _ }
  | TooManyArguments { callee = Some name; _ } ->
      List.last name
      >>| (fun name -> Expression.Access.show [name])
      (* TODO(T28686494): unbreak `dict.update`call resolution. *)
      >>| List.mem ~equal:String.equal ["__init__"; "__str__"; "update"]
      |> Option.value ~default:false
  | _ ->
      false


let due_to_mismatch_with_any { kind; _ } =
  match kind with
  | UndefinedAttribute { origin = Class { annotation = actual; _ }; _ }
  | IncompatibleAwaitableType actual ->
      Type.equal actual Type.Object
  | InconsistentOverride { override = StrengthenedPrecondition (Found { actual; expected }); _ }
  | InconsistentOverride { override = WeakenedPostcondition { actual; expected }; _ }
  | IncompatibleParameterType { mismatch = { actual; expected }; _ }
  | IncompatibleReturnType { actual; expected }
  | IncompatibleAttributeType { incompatible_type = { mismatch = { actual; expected }; _ }; _ }
  | IncompatibleVariableType { mismatch = { actual; expected }; _ }
  | UninitializedAttribute { mismatch = { actual; expected }; _ } ->
      Type.mismatch_with_any actual expected
  | TooManyArguments _
  | MissingAttributeAnnotation _
  | MissingGlobalAnnotation _
  | MissingParameterAnnotation _
  | MissingReturnAnnotation _
  | RedundantCast _
  | RevealedType _
  | IncompatibleConstructorAnnotation _
  | InconsistentOverride _
  | Top
  | UndefinedType _
  | MissingArgument _
  | UndefinedAttribute _
  | UndefinedName _
  | UndefinedImport _
  | UnusedIgnore _ ->
      false


let less_or_equal ~resolution left right =
  let less_or_equal_mismatch left right =
    Resolution.less_or_equal resolution ~left:left.actual ~right:right.actual &&
    Resolution.less_or_equal resolution ~left:left.expected ~right:right.expected
  in
  Location.equal left.location right.location &&
  begin
    match left.kind, right.kind with
    | IncompatibleAwaitableType left, IncompatibleAwaitableType right ->
        Resolution.less_or_equal resolution ~left ~right
    | MissingArgument left, MissingArgument right ->
        equal_missing_argument left right
    | MissingParameterAnnotation left, MissingParameterAnnotation right
      when left.name = right.name ->
        Resolution.less_or_equal resolution ~left:left.annotation ~right:right.annotation
    | MissingReturnAnnotation left, MissingReturnAnnotation right ->
        Resolution.less_or_equal resolution ~left: left.annotation ~right: right.annotation
    | MissingAttributeAnnotation { missing_annotation = left; _ },
      MissingAttributeAnnotation { missing_annotation = right; _ }
    | MissingGlobalAnnotation left, MissingGlobalAnnotation right
      when left.name = right.name && left.due_to_any = right.due_to_any ->
        Resolution.less_or_equal resolution ~left:left.annotation ~right:right.annotation
    | RedundantCast left, RedundantCast right ->
        Resolution.less_or_equal resolution ~left ~right
    | RevealedType (left, left_expression), RevealedType (right, right_expression) ->
        Expression.equal left_expression right_expression &&
        Resolution.less_or_equal resolution ~left ~right
    | IncompatibleParameterType left, IncompatibleParameterType right
      when Option.equal Access.equal left.name right.name ->
        less_or_equal_mismatch left.mismatch right.mismatch
    | IncompatibleConstructorAnnotation left, IncompatibleConstructorAnnotation right ->
        Resolution.less_or_equal resolution ~left ~right
    | IncompatibleReturnType left, IncompatibleReturnType right ->
        less_or_equal_mismatch left right
    | IncompatibleAttributeType left, IncompatibleAttributeType right
      when Annotated.Class.name_equal left.parent right.parent &&
           left.incompatible_type.name = right.incompatible_type.name ->
        less_or_equal_mismatch left.incompatible_type.mismatch right.incompatible_type.mismatch
    | IncompatibleVariableType left, IncompatibleVariableType right when left.name = right.name ->
        less_or_equal_mismatch left.mismatch right.mismatch
    | InconsistentOverride left, InconsistentOverride right ->
        begin
          match left.override, right.override with
          | StrengthenedPrecondition (NotFound left_access),
            StrengthenedPrecondition (NotFound right_access) ->
              Access.equal left_access right_access
          | StrengthenedPrecondition (Found left_mismatch),
            StrengthenedPrecondition (Found right_mismatch)
          | WeakenedPostcondition left_mismatch, WeakenedPostcondition right_mismatch ->
              less_or_equal_mismatch left_mismatch right_mismatch
          | _ ->
              false
        end
    | TooManyArguments left, TooManyArguments right ->
        equal_too_many_arguments left right
    | UninitializedAttribute left, UninitializedAttribute right when left.name = right.name ->
        less_or_equal_mismatch left.mismatch right.mismatch
    | UndefinedAttribute left, UndefinedAttribute right
      when Access.equal left.attribute right.attribute ->
        begin
          match left.origin, right.origin with
          | Class left, Class right when left.class_attribute = right.class_attribute ->
              Resolution.less_or_equal resolution ~left:left.annotation ~right:right.annotation
          | Module left, Module right ->
              Access.equal left right
          | _ ->
              false
        end
    | UndefinedName left, UndefinedName right when Access.equal left right ->
        true
    | UndefinedType left, UndefinedType right ->
        Resolution.less_or_equal resolution ~left ~right
    | UndefinedImport left, UndefinedImport right ->
        Access.equal left right
    | UnusedIgnore left, UnusedIgnore right ->
        Set.is_subset (Int.Set.of_list left) ~of_:(Int.Set.of_list right)
    | _, Top -> true
    | _ ->
        false
  end


let join ~resolution left right =
  let join_mismatch left right =
    {
      expected = Resolution.join resolution left.expected right.expected;
      actual = Resolution.join resolution left.actual right.actual;
    }
  in
  let join_missing_annotation
      (left: missing_annotation)  (* Ohcaml... *)
      (right: missing_annotation): missing_annotation =
    {
      left with
      annotation = Resolution.join resolution left.annotation right.annotation;
      evidence_locations =
        Location.Set.of_list (left.evidence_locations @ right.evidence_locations)
        |> Set.to_list;
      due_to_any = left.due_to_any && right.due_to_any;
    }
  in
  let kind =
    match left.kind, right.kind with
    | IncompatibleAwaitableType left, IncompatibleAwaitableType right ->
        IncompatibleAwaitableType (Resolution.join resolution left right)
    | MissingArgument left, MissingArgument right
      when equal_missing_argument left right ->
        MissingArgument left
    | MissingParameterAnnotation left, MissingParameterAnnotation right
      when left.name = right.name ->
        let annotation = Resolution.join resolution left.annotation right.annotation in
        MissingParameterAnnotation { left with annotation }
    | MissingReturnAnnotation left, MissingReturnAnnotation right ->
        MissingReturnAnnotation {
          annotation = Resolution.join resolution left.annotation right.annotation;
          evidence_locations =
            Int.Set.of_list (left.evidence_locations @ right.evidence_locations)
            |> Set.to_list;
          due_to_any = left.due_to_any && right.due_to_any;
        }
    | MissingAttributeAnnotation left, MissingAttributeAnnotation right
      when left.missing_annotation.name = right.missing_annotation.name &&
           Annotated.Class.name_equal left.parent right.parent ->
        MissingAttributeAnnotation {
          parent = left.parent;
          missing_annotation =
            join_missing_annotation
              left.missing_annotation
              right.missing_annotation;
        }
    | MissingGlobalAnnotation left, MissingGlobalAnnotation right when left.name = right.name ->
        MissingGlobalAnnotation (join_missing_annotation left right)
    | RedundantCast left, RedundantCast right ->
        RedundantCast (Resolution.join resolution left right)
    | RevealedType (left_annotation, left_expression),
      RevealedType (right_annotation, right_expression)
      when Expression.equal left_expression right_expression ->
        RevealedType (Resolution.join resolution left_annotation right_annotation, left_expression)
    | IncompatibleParameterType left, IncompatibleParameterType right
      when Option.equal Access.equal left.name right.name &&
           left.position = right.position &&
           Option.equal Access.equal left.callee right.callee ->
        IncompatibleParameterType {
          left with mismatch = join_mismatch left.mismatch right.mismatch
        }
    | IncompatibleConstructorAnnotation left, IncompatibleConstructorAnnotation right ->
        IncompatibleConstructorAnnotation (Resolution.join resolution left right)
    | IncompatibleReturnType left, IncompatibleReturnType right ->
        IncompatibleReturnType (join_mismatch left right)
    | IncompatibleAttributeType left, IncompatibleAttributeType right
      when Annotated.Class.name_equal left.parent right.parent &&
           left.incompatible_type.name = right.incompatible_type.name ->
        IncompatibleAttributeType {
          parent = left.parent;
          incompatible_type = {
            left.incompatible_type with
            mismatch =
              join_mismatch
                left.incompatible_type.mismatch
                right.incompatible_type.mismatch;
          };
        }
    | IncompatibleVariableType left, IncompatibleVariableType right when left.name = right.name ->
        IncompatibleVariableType { left with mismatch = join_mismatch left.mismatch right.mismatch }
    | InconsistentOverride ({ override = StrengthenedPrecondition left_issue; _ } as left),
      InconsistentOverride ({ override = StrengthenedPrecondition right_issue; _ } as right) ->
        begin
          match left_issue, right_issue with
          | Found left_mismatch, Found right_mismatch ->
              InconsistentOverride {
                left with
                override =
                  StrengthenedPrecondition (Found (join_mismatch left_mismatch right_mismatch))
              }
          | NotFound _, _ ->
              InconsistentOverride left
          | _, NotFound _ ->
              InconsistentOverride right
        end
    | InconsistentOverride ({ override = WeakenedPostcondition left_mismatch; _ } as left),
      InconsistentOverride { override = WeakenedPostcondition right_mismatch; _ } ->
        InconsistentOverride {
          left with override = WeakenedPostcondition (join_mismatch left_mismatch right_mismatch);
        }
    | TooManyArguments left, TooManyArguments right when equal_too_many_arguments left right ->
        TooManyArguments left
    | UninitializedAttribute left, UninitializedAttribute right
      when left.name = right.name && Annotated.Class.name_equal left.parent right.parent ->
        UninitializedAttribute { left with mismatch = join_mismatch left.mismatch right.mismatch }
    | UndefinedAttribute left, UndefinedAttribute right
      when Access.equal left.attribute right.attribute ->
        let origin: origin option =
          match left.origin, right.origin with
          | Class left, Class right when left.class_attribute = right.class_attribute ->
              let annotation = Resolution.join resolution left.annotation right.annotation in
              Some (Class { left with annotation })
          | Module left, Module right when Access.equal left right ->
              Some (Module left)
          | _ ->
              None
        in
        origin
        >>| (fun origin -> UndefinedAttribute { left with origin })
        |> Option.value ~default:Top
    | UndefinedName left, UndefinedName right when Access.equal left right ->
        UndefinedName left
    | UndefinedType left, UndefinedType right ->
        UndefinedType (Resolution.join resolution left right)
    | UndefinedImport left, UndefinedImport right when Access.equal left right ->
        UndefinedImport left

    (* Join UndefinedImport/Name pairs into an undefined import, as the missing name is due to us
       being unable to resolve the import. *)
    | UndefinedImport left, UndefinedName right when Access.equal left right ->
        UndefinedImport left
    | UndefinedName left, UndefinedImport right when Access.equal left right ->
        UndefinedImport right

    | UnusedIgnore left, UnusedIgnore right ->
        UnusedIgnore (Set.to_list (Set.union (Int.Set.of_list left) (Int.Set.of_list right)))
    | _ ->
        Log.debug
          "Incompatible type in error join at %a: %a %a"
          Location.pp left.location
          pp_kind left.kind
          pp_kind right.kind;
        Top
  in
  { location = left.location; kind; define = left.define }


let meet ~resolution:_ left _ =
  (* We do not yet care about meeting errors. *)
  left


let widen ~resolution ~previous ~next ~iteration:_ =
  join ~resolution previous next


let join_at_define ~resolution ~location errors =
  let return_errors, other_errors =
    List.partition_tf
      ~f:(function | { kind = MissingReturnAnnotation _; _ } -> true | _ -> false)
      errors
  in
  match return_errors with
  | [] ->
      other_errors
  | error :: errors ->
      List.fold ~init:error ~f:(join ~resolution) errors
      |> fun return_error -> { return_error with location } :: other_errors


let join_at_source ~resolution errors =
  let key error =
    match error with
    | { kind = MissingAttributeAnnotation { parent; missing_annotation = { name; _ }; _ }; _ } ->
        Annotated.Class.show parent ^ Access.show name
    | { kind = MissingGlobalAnnotation { name; _ }; _ } ->
        Access.show name
    | { kind = UndefinedImport name; _ }
    | { kind = UndefinedName name; _ } ->
        Format.asprintf "Unknown[%a]" Access.pp name
    | _ ->
        show error
  in
  let joined_missing_annotations =
    let filter errors error =
      let filtered ~key =
        try
          match Map.find errors key with
          | Some existing_error ->
              let joined_error = join ~resolution error existing_error in
              if joined_error.kind <> Top then
                Map.change ~f:(fun _ -> Some joined_error) errors key
              else
                Map.set ~key ~data:error errors
          | _ ->
              Map.set ~key ~data:error errors
        with TypeOrder.Untracked _ ->
          errors
      in
      match error with
      | { kind = MissingAttributeAnnotation _; _ }
      | { kind = MissingGlobalAnnotation _; _ }
      | { kind = UndefinedImport _; _ }
      | { kind = UndefinedName _; _ } ->
          filtered ~key:(key error)
      | _ ->
          errors
    in
    List.fold ~init:String.Map.empty ~f:filter errors
  in
  let add_joins errors error =
    let joined ~key =
      match Map.find joined_missing_annotations key with
      | Some { kind; _ } ->
          begin
            match error.kind, kind with
            | UndefinedName _, UndefinedImport _ ->
                (* Swallow up UndefinedName errors when the Import error already exists. *)
                errors
            | _ ->
                let new_error = { error with kind } in
                Map.set ~key:(show new_error) ~data:new_error errors
          end
      | _ -> Map.set ~key:(show error) ~data:error errors
    in
    match error with
    | { kind = MissingAttributeAnnotation _; _ }
    | { kind = MissingGlobalAnnotation _; _ }
    | { kind = UndefinedImport _; _ }
    | { kind = UndefinedName _; _ }
      when not (due_to_analysis_limitations error) ->
        joined ~key:(key error)
    | _ ->
        Map.set ~key:(show error) ~data:error errors
  in
  List.fold ~init:String.Map.empty ~f:add_joins errors
  |> Map.data


let filter ~configuration ~resolution errors =
  let should_filter error =
    let is_mock_error { kind; _ } =
      match kind with
      | IncompatibleAttributeType { incompatible_type = { mismatch = { actual; _ }; _ }; _ }
      | IncompatibleAwaitableType actual
      | IncompatibleParameterType { mismatch = { actual; _ }; _ }
      | IncompatibleReturnType { actual; _ }
      | IncompatibleVariableType { mismatch = { actual; _ }; _ }
      | UndefinedAttribute { origin = Class { annotation = actual; _ }; _ } ->
          let is_subclass_of_mock annotation =
            (not (Type.equal annotation Type.Bottom)) &&
            ((Resolution.less_or_equal
                resolution
                ~left:annotation
                ~right:(Type.Primitive (Identifier.create "unittest.mock.Base"))) ||
             (* Special-case mypy's workaround for mocks. *)
             (Resolution.less_or_equal
                resolution
                ~left:annotation
                ~right:(Type.Primitive (Identifier.create "unittest.mock.NonCallableMock"))))
          in
          Type.exists actual ~predicate:is_subclass_of_mock
      | _ ->
          false
    in
    let is_callable_error { kind;_ } =
      match kind with
      | IncompatibleAttributeType {
          incompatible_type = { mismatch = { actual; expected }; _ }; _ }
      | IncompatibleParameterType { mismatch = { actual; expected }; _ }
      | IncompatibleReturnType { actual; expected }
      | IncompatibleVariableType { mismatch = { actual; expected }; _ } ->
          Type.contains_callable actual || Type.contains_callable expected
      | IncompatibleAwaitableType actual ->
          Type.contains_callable actual
      | _ ->
          false
    in

    let is_unimplemented_return_error error =
      match error with
      | { kind = IncompatibleReturnType _; define = { Node.value = { Define.body; _ }; _ }; _ } ->
          let rec check_statements = function
            | [{ Node.value = Statement.Pass; _ }; { Node.value = Statement.Return None; _ }] ->
                true
            | {
              Node.value = Statement.Expression { Node.value = Expression.String _; _ };
              _;
            } :: tail ->
                check_statements tail
            | _ ->
                false
          in
          check_statements body
      | _ ->
          false
    in

    (* Ignore naming mismatches on parameters of dunder methods due to unofficial typeshed naming *)
    let is_override_on_dunder_method { kind; _ } =
      let get_name overridden = overridden |> Annotated.Class.Method.name |> Access.show in
      match kind with
      | InconsistentOverride { overridden_method; override }
        when String.is_prefix ~prefix:"__" (get_name overridden_method) &&
             String.is_suffix ~suffix:"__" (get_name overridden_method) ->
          begin
            match override with
            | StrengthenedPrecondition (NotFound _) -> true
            | _ -> false
          end
      | _ -> false
    in

    is_mock_error error ||
    is_callable_error error ||
    is_unimplemented_return_error error ||
    is_override_on_dunder_method error
  in
  match configuration with
  | { Configuration.debug = true; _ } -> errors
  | _ -> List.filter ~f:(fun error -> not (should_filter error)) errors


let suppress ~mode error =
  let suppress_in_strict ({ kind; _ } as error) =
    if due_to_analysis_limitations error then
      match kind with
      | TooManyArguments _
      | IncompatibleParameterType _
      | IncompatibleReturnType _
      | IncompatibleConstructorAnnotation _
      | MissingParameterAnnotation _
      | MissingReturnAnnotation _
      | UndefinedAttribute _
      | UndefinedName _
      | UndefinedImport _
      | RedundantCast _
      | RevealedType _
      | MissingArgument _ ->
          false
      | IncompatibleAwaitableType _
      | IncompatibleAttributeType _
      | IncompatibleVariableType _
      | InconsistentOverride _
      | MissingAttributeAnnotation _
      | MissingGlobalAnnotation _
      | Top
      | UndefinedType _
      | UninitializedAttribute _
      | UnusedIgnore _ ->
          true
    else
      match kind with
      | MissingParameterAnnotation { due_to_any; _ } ->
          due_to_any
      | _ ->
          false
  in

  let suppress_in_default ({ kind; define = { Node.value = define; _ }; _ } as error) =
    match kind with
    | MissingReturnAnnotation _
    | MissingParameterAnnotation _
    | MissingAttributeAnnotation _
    | MissingGlobalAnnotation _
    | UndefinedType _ ->
        true
    | UndefinedImport _ ->
        false
    | UndefinedName name when Access.show name = "reveal_type" ->
        true
    | RevealedType _ ->
        false
    | _ ->
        due_to_analysis_limitations error ||
        due_to_mismatch_with_any error ||
        due_to_unsupported_calls error ||
        Define.is_untyped define
  in

  let suppress_in_infer ({ kind; _ } as error) =
    match kind with
    | MissingReturnAnnotation { annotation = actual; _ }
    | MissingParameterAnnotation { annotation = actual; _ }
    | MissingAttributeAnnotation { missing_annotation = { annotation = actual; _ }; _ }
    | MissingGlobalAnnotation { annotation = actual; _ } ->
        due_to_analysis_limitations error ||
        Type.equal actual Type.Object
    | _ ->
        true
  in

  match mode with
  | Source.Infer -> suppress_in_infer error
  | Source.Strict -> suppress_in_strict error
  | Source.Declare -> true
  | _ -> suppress_in_default error


let dequalify
    dequalify_map
    environment
    ({kind; define = { Node.location; value = define }; _ } as error) =
  let resolution = Environment.resolution environment () in
  let dequalify = Type.dequalify dequalify_map in
  let kind =
    match kind with
    | IncompatibleAwaitableType actual  ->
        IncompatibleAwaitableType (dequalify actual)
    | IncompatibleConstructorAnnotation annotation ->
        IncompatibleConstructorAnnotation (dequalify annotation)
    | TooManyArguments extra_argument ->
        TooManyArguments extra_argument
    | Top ->
        Top
    | MissingParameterAnnotation { name; annotation; due_to_any; } ->
        MissingParameterAnnotation { annotation = dequalify annotation; name; due_to_any; }
    | MissingReturnAnnotation ({ annotation; _ } as missing_return) ->
        MissingReturnAnnotation { missing_return with annotation = dequalify annotation; }
    | MissingAttributeAnnotation {
        parent;
        missing_annotation = { annotation; _ } as missing_annotation;
      } ->
        MissingAttributeAnnotation {
          parent;
          missing_annotation = { missing_annotation with annotation = dequalify annotation };
        }
    | MissingGlobalAnnotation ({ annotation; _ } as immutable_type) ->
        MissingGlobalAnnotation {
          immutable_type with  annotation = dequalify annotation;
        }
    | RedundantCast annotation ->
        RedundantCast (dequalify annotation)
    | RevealedType (annotation, expression) ->
        RevealedType (dequalify annotation, expression)
    | IncompatibleParameterType ({ mismatch = { actual; expected }; _ } as parameter) ->
        IncompatibleParameterType {
          parameter with
          mismatch = { actual = dequalify actual; expected = dequalify expected };
        }
    | IncompatibleReturnType { actual; expected }  ->
        IncompatibleReturnType { actual = dequalify actual; expected = dequalify expected }
    | IncompatibleAttributeType {
        parent;
        incompatible_type = { mismatch = { actual; expected }; _ } as incompatible_type;
      } ->
        IncompatibleAttributeType {
          parent;
          incompatible_type = {
            incompatible_type with
            mismatch = { actual = dequalify actual; expected = dequalify expected };
          };
        }
    | IncompatibleVariableType ({ mismatch = { actual; expected }; _ } as incompatible_type) ->
        IncompatibleVariableType {
          incompatible_type with
          mismatch = { actual = dequalify actual; expected = dequalify expected };
        }
    | InconsistentOverride
        ({
          override = StrengthenedPrecondition (Found { actual; expected });
          _;
        } as inconsistent_override) ->
        InconsistentOverride {
          inconsistent_override with
          override = StrengthenedPrecondition (Found {
              actual = dequalify actual;
              expected = dequalify expected;
            });
        }
    | InconsistentOverride (
        { override = StrengthenedPrecondition (NotFound access); _ } as inconsistent_override
      ) ->
        InconsistentOverride {
          inconsistent_override with override = StrengthenedPrecondition (NotFound access);
        }
    | InconsistentOverride
        ({
          override = WeakenedPostcondition { actual; expected };
          _;
        } as inconsistent_override) ->
        InconsistentOverride {
          inconsistent_override with
          override = WeakenedPostcondition {
              actual = dequalify actual;
              expected = dequalify expected
            };
        }
    | UninitializedAttribute ({ mismatch = { actual; expected }; _ } as inconsistent_usage) ->
        UninitializedAttribute {
          inconsistent_usage with
          mismatch = { actual = dequalify actual; expected = dequalify expected };
        }
    | UndefinedAttribute { attribute; origin } ->
        let origin: origin =
          match origin with
          | Class { annotation; class_attribute } ->
              let annotation =
                (* Don't dequalify optionals because we special case their display. *)
                if Type.is_optional_primitive annotation then
                  annotation
                else
                  dequalify annotation
              in
              Class { annotation; class_attribute }
          | _ ->
              origin
        in
        UndefinedAttribute { attribute; origin }
    | UndefinedName access ->
        UndefinedName access
    | UndefinedType annotation ->
        UndefinedType (dequalify annotation)
    | UndefinedImport access ->
        UndefinedImport access
    | MissingArgument missing_argument ->
        MissingArgument missing_argument
    | UnusedIgnore codes ->
        UnusedIgnore codes
  in
  let define =
    let dequalify_parameter ({ Node.value; _ } as parameter) =
      value.Parameter.annotation
      >>| Resolution.parse_annotation resolution
      >>| dequalify
      >>| Type.expression
      |> fun annotation ->
      { parameter with Node.value = { value with Parameter.annotation }}
    in
    let parameters = List.map ~f:dequalify_parameter define.Define.parameters in
    let return_annotation =
      define.Define.return_annotation
      >>| Resolution.parse_annotation resolution
      >>| dequalify
      >>| Type.expression
    in
    { define with Define.parameters; return_annotation }
  in
  { error with kind; define = { Node.location; value = define} }


let to_json ~detailed ({ kind; define = { Node.value = define; _ }; location; _ } as error) =
  let function_name = Access.show define.Define.name in
  let print_annotation annotation =
    Format.asprintf "%a" Type.pp annotation
    |> String.strip ~drop:((=) '`')
  in
  let parameters =
    let to_json { Node.value = { Parameter.name; annotation; value }; _ } =
      let annotation =
        match kind with
        | MissingParameterAnnotation { name = parameter_name; annotation = parameter_annotation; _ }
          when Access.create_from_identifiers [name] = parameter_name ->
            parameter_annotation
            |> print_annotation
            |> (fun string -> `String string)
        | _ ->
            annotation
            >>| Expression.show
            >>| (fun string -> `String string)
            |> Option.value ~default:`Null
      in
      let value =
        value
        >>| Expression.show
        >>| (fun string -> `String string)
        |> Option.value ~default:`Null
      in
      `Assoc [ "name", `String (Identifier.show name); "type", annotation ; "value", value ]
    in
    List.map ~f:to_json define.Define.parameters
  in
  let decorators =
    let decorator_to_json decorator = `String (Expression.show decorator) in
    List.map ~f:decorator_to_json define.Define.decorators
  in
  let print_parent parent =
    parent
    >>| Access.show
    >>| (fun string -> `String string)
    |> Option.value ~default:`Null
  in
  let inference_information =
    match kind with
    | MissingReturnAnnotation { annotation; _ } ->
        [
          "annotation", `String (print_annotation annotation);
          "parent", print_parent define.Define.parent;
          "function_name", `String function_name;
          "parameters", `List parameters;
          "decorators", `List decorators;
          "async", `Bool define.Define.async;
        ]
    | MissingParameterAnnotation _ ->
        let return_annotation =
          define.Define.return_annotation
          >>| Format.asprintf "%a" Expression.pp
          >>| (fun string -> `String string)
          |> Option.value ~default:`Null
        in
        [
          "annotation", return_annotation;
          "parent", print_parent define.Define.parent;
          "function_name", `String function_name;
          "parameters", `List parameters;
          "decorators", `List decorators;
          "async", `Bool define.Define.async;
        ]
    | MissingAttributeAnnotation { parent; missing_annotation = { name; annotation; _ } } ->
        [
          "annotation", `String (print_annotation annotation);
          "parent", `String (Annotated.Class.name parent |> Access.show);
          "attribute_name", `String (Access.show name);
        ]
    | MissingGlobalAnnotation { name; annotation; _ } ->
        [
          "annotation", `String (print_annotation annotation);
          "parent", `Null;
          "attribute_name", `String (Access.show name);
        ]
    | _ -> []
  in
  `Assoc ([
      "line", `Int (Location.line location);
      "column", `Int (Location.column location);
      "path", `String (Location.path location);
      "code", `Int (code error);
      "name", `String (name error);
      "description", `String (description error ~detailed);
      "inference", `Assoc inference_information;
    ])
