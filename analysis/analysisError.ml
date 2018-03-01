(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open Statement

module Annotated = AnalysisAnnotated
module Environment = AnalysisEnvironment
module Resolution = AnalysisResolution
module Type = AnalysisType
module TypeOrder = AnalysisTypeOrder

module AstSharedMemory = ServiceAstSharedMemory


type undefined_method = {
  annotation: Type.t;
  call: Annotated.Call.t;
}
[@@deriving compare, eq, show, sexp, hash]


type undefined_attribute = {
  annotation: Type.t;
  attribute: Access.t;
  class_attribute: bool;
}
[@@deriving compare, eq, show, sexp, hash]


type mismatch = {
  actual: Type.t;
  expected: Type.t;
}
[@@deriving compare, eq, show, sexp, hash]


type missing_parameter = {
  name: Identifier.t;
  annotation: Type.t;
  due_to_any: bool;
}
[@@deriving compare, eq, show, sexp, hash]


type parameter_mismatch = {
  name: Identifier.t;
  position: int;
  callee: Define.t;
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
[@@deriving compare, eq, sexp, show, hash]


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
  | UndefinedMethod of undefined_method
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
  | IncompatibleAwaitableType _ -> 12
  | IncompatibleParameterType _ -> 6
  | IncompatibleReturnType _ -> 7
  | IncompatibleAttributeType _ -> 8
  | IncompatibleVariableType _ -> 9
  | InconsistentOverride { override; _ } ->
      begin
        match override with
        | StrengthenedPrecondition -> 14
        | WeakenedPostcondition -> 15
      end
  | MissingParameterAnnotation _ -> 2
  | MissingReturnAnnotation _ -> 3
  | MissingAttributeAnnotation _ -> 4
  | MissingGlobalAnnotation _ -> 5
  | Top -> 1
  | UndefinedAttribute _ -> 16
  | UndefinedMethod _ -> 10
  | UndefinedType _ -> 11
  | UninitializedAttribute _ -> 13
  | UnusedIgnore _ -> 0


let name { kind; _ } =
  match kind with
  | IncompatibleAwaitableType _ -> "Incompatible awaitable type"
  | IncompatibleParameterType _ -> "Incompatible parameter type"
  | IncompatibleReturnType _ -> "Incompatible return type"
  | IncompatibleAttributeType _ -> "Incompatible attribute type"
  | IncompatibleVariableType _ -> "Incompatible variable type"
  | InconsistentOverride _ -> "Inconsistent override"
  | MissingAttributeAnnotation _ -> "Missing attribute annotation"
  | MissingGlobalAnnotation _ -> "Missing global annotation"
  | MissingParameterAnnotation _ -> "Missing parameter annotation"
  | MissingReturnAnnotation _ -> "Missing return annotation"
  | Top -> "Undefined error"
  | UndefinedAttribute _ -> "Undefined attribute"
  | UndefinedMethod _ -> "Undefined method"
  | UndefinedType _ -> "Undefined type"
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
            "Parameter %a has type %a but no type is specified."
            Identifier.pp name
            Type.pp annotation
        ]
    | MissingParameterAnnotation { name; annotation; due_to_any = true } ->
        [
          Format.asprintf
            "Parameter %a has type %a but type `Any` is specified."
            Identifier.pp name
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
    | IncompatibleParameterType {
        name;
        position;
        callee = { Define.name = callee_name; parent; _ };
        mismatch = { actual; expected };
      } ->
        let parent =
          match parent with
          | Some parent -> Format.asprintf "%a." Access.pp parent
          | _ -> ""
        in
        let name =
          let replaced = Str.global_replace (Str.regexp "^\\$.*_") "" (Identifier.show name) in
          "`" ^ replaced ^ "`"
        in
        [
          Format.asprintf
            "Expected %a but got %a."
            Type.pp expected
            Type.pp actual;
          Format.asprintf
            "%s parameter %s to call `%s%a`."
            (ordinal position)
            name
            parent
            Access.pp callee_name
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
        [
          (Format.asprintf
             "%a is declared to have type %a but is used as type %a."
             Access.pp name
             Type.pp expected
             Type.pp actual);
          (Format.asprintf
             "%a incorrectly used on line %d."
             Access.pp name
             (Location.line location))
        ]
    | InconsistentOverride { overridden_method; override; mismatch = { actual; expected } } ->
        let detail =
          match override with
          | WeakenedPostcondition ->
              Format.asprintf
                "Returned type %a is not a subtype of the overridden return %a."
                Type.pp actual
                Type.pp expected
          | StrengthenedPrecondition ->
              if not (Type.equal actual Type.none) then
                Format.asprintf
                  "Parameter of type %a is not a supertype of the overridden parameter %a."
                  Type.pp actual
                  Type.pp expected
              else
                "Not all parameters are provided."
        in
        [
          Format.asprintf
            "`%a` overrides method defined in `%a` inconsistently."
            Access.pp define_name
            Access.pp
            (Annotated.Method.parent overridden_method |> Annotated.Class.name);
          detail;
        ]
    | UndefinedAttribute { annotation; attribute; class_attribute } ->
        let name =
          if Type.is_optional_primitive annotation then
            "Optional type"
          else
            Type.show annotation
        in
        let class_attribute_detail =
          [
            "This attribute is accessed as a class variable; did you mean to declare it with \
             `typing.ClassVar`?"
          ]
        in
        let message =
          [Format.asprintf "%s has no attribute `%a`." name Access.pp attribute]
        in
        if class_attribute then
          List.append message class_attribute_detail
        else
          message
    | UndefinedMethod { annotation; call } ->
        let name =
          match Annotated.Call.name call with
          | { Node.value = Access access; _ } ->
              Access.show access
          | name ->
              Expression.show name
        in
        [
          Format.asprintf
            "Could not resolve call `%s` on %a."
            name
            Type.pp annotation
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
  | InconsistentOverride { mismatch = { actual; _ }; _ }
  | MissingAttributeAnnotation { missing_annotation = { annotation = actual; _ }; _ }
  | MissingGlobalAnnotation { annotation = actual; _ }
  | MissingParameterAnnotation { annotation = actual; _ }
  | MissingReturnAnnotation { annotation = actual; _ }
  | UninitializedAttribute { mismatch = {actual; _ }; _ }->
      Type.is_unknown actual
  | Top -> true
  | UndefinedAttribute { annotation; _ }
  | UndefinedMethod { annotation; _ }
  | UndefinedType annotation ->
      Type.is_unknown annotation
  | UnusedIgnore _ -> false


let due_to_mismatch_with_any { kind; _ } =
  match kind with
  | MissingAttributeAnnotation _
  | MissingGlobalAnnotation _
  | MissingParameterAnnotation _
  | MissingReturnAnnotation _
  | Top
  | UndefinedMethod _
  | UndefinedType _
  | UnusedIgnore _ ->
      false
  | UndefinedAttribute { annotation = actual; _ }
  | IncompatibleAwaitableType actual ->
      Type.equal actual Type.Object
  | InconsistentOverride { mismatch = { actual; expected }; _ }
  | IncompatibleParameterType { mismatch = { actual; expected }; _ }
  | IncompatibleReturnType { actual; expected }
  | IncompatibleAttributeType { incompatible_type = { mismatch = { actual; expected }; _ }; _ }
  | IncompatibleVariableType { mismatch = { actual; expected }; _ }
  | UninitializedAttribute { mismatch = { actual; expected }; _ }->
      Type.mismatch_with_any actual expected


let less_or_equal ~resolution left right =
  let order = Resolution.order resolution in

  let less_or_equal_mismatch left right =
    TypeOrder.less_or_equal order ~left:left.actual ~right:right.actual &&
    TypeOrder.less_or_equal order ~left:left.expected ~right:right.expected
  in
  Location.equal left.location right.location &&
  begin
    match left.kind, right.kind with
    | IncompatibleAwaitableType left, IncompatibleAwaitableType right ->
        TypeOrder.less_or_equal order ~left ~right
    | MissingParameterAnnotation left, MissingParameterAnnotation right
      when left.name = right.name ->
        TypeOrder.less_or_equal
          order
          ~left:left.annotation
          ~right:right.annotation
    | MissingReturnAnnotation left, MissingReturnAnnotation right ->
        TypeOrder.less_or_equal
          order
          ~left: left.annotation
          ~right: right.annotation
    | MissingAttributeAnnotation { missing_annotation = left; _ },
      MissingAttributeAnnotation { missing_annotation = right; _ }
    | MissingGlobalAnnotation left, MissingGlobalAnnotation right
      when left.name = right.name && left.due_to_any = right.due_to_any ->
        TypeOrder.less_or_equal
          order
          ~left:left.annotation
          ~right:right.annotation
    | IncompatibleParameterType left, IncompatibleParameterType right when left.name = right.name ->
        less_or_equal_mismatch left.mismatch right.mismatch
    | IncompatibleReturnType left, IncompatibleReturnType right ->
        less_or_equal_mismatch left right
    | IncompatibleAttributeType left, IncompatibleAttributeType right
      when Annotated.Class.name_equal left.parent right.parent &&
           left.incompatible_type.name = right.incompatible_type.name ->
        less_or_equal_mismatch left.incompatible_type.mismatch right.incompatible_type.mismatch
    | IncompatibleVariableType left, IncompatibleVariableType right when left.name = right.name ->
        less_or_equal_mismatch left.mismatch right.mismatch
    | InconsistentOverride left, InconsistentOverride right ->
        less_or_equal_mismatch left.mismatch right.mismatch
    | UninitializedAttribute left, UninitializedAttribute right when left.name = right.name ->
        less_or_equal_mismatch left.mismatch right.mismatch
    | UndefinedAttribute left, UndefinedAttribute right
      when Access.equal left.attribute right.attribute ->
        TypeOrder.less_or_equal
          order
          ~left:left.annotation
          ~right:right.annotation
    | UndefinedMethod left, UndefinedMethod right ->
        TypeOrder.less_or_equal
          order
          ~left:left.annotation
          ~right:right.annotation
    | UndefinedType left, UndefinedType right ->
        TypeOrder.less_or_equal order ~left ~right
    | UnusedIgnore left, UnusedIgnore right ->
        Set.is_subset (Int.Set.of_list left) ~of_:(Int.Set.of_list right)
    | _, Top -> true
    | _ ->
        false
  end


let join ~resolution left right =
  let order = Resolution.order resolution in
  let join_mismatch left right =
    {
      expected = TypeOrder.join order left.expected right.expected;
      actual = TypeOrder.join order left.actual right.actual;
    }
  in
  let join_missing_annotation
      (left: missing_annotation)  (* Ohcaml... *)
      (right: missing_annotation): missing_annotation =
    {
      left with
      annotation = TypeOrder.join order left.annotation right.annotation;
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
    | MissingParameterAnnotation left, MissingParameterAnnotation right
      when left.name = right.name ->
        let annotation =
          TypeOrder.join order left.annotation right.annotation
        in
        MissingParameterAnnotation { left with annotation }
    | MissingReturnAnnotation left, MissingReturnAnnotation right ->
        MissingReturnAnnotation {
          annotation = TypeOrder.join order left.annotation right.annotation;
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
    | IncompatibleParameterType left, IncompatibleParameterType right
      when left.name = right.name &&
           left.position = right.position && Define.equal left.callee right.callee ->
        IncompatibleParameterType {
          left with mismatch = join_mismatch left.mismatch right.mismatch
        }
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
    | InconsistentOverride left, InconsistentOverride right ->
        InconsistentOverride { left with mismatch = join_mismatch left.mismatch right.mismatch }
    | UninitializedAttribute left, UninitializedAttribute right
      when left.name = right.name && Annotated.Class.name_equal left.parent right.parent ->
        UninitializedAttribute { left with mismatch = join_mismatch left.mismatch right.mismatch }
    | UndefinedAttribute left, UndefinedAttribute right
      when Access.equal left.attribute right.attribute ->
        UndefinedAttribute {
          left with
          annotation = TypeOrder.join order left.annotation right.annotation;
        }
    | UndefinedMethod left, UndefinedMethod right ->
        UndefinedMethod {
          annotation =
            TypeOrder.join order left.annotation right.annotation;
          call = left.call;
        }
    | UndefinedType left, UndefinedType right ->
        UndefinedType (TypeOrder.join order left right)
    | UnusedIgnore left, UnusedIgnore right ->
        UnusedIgnore (Set.to_list (Set.union (Int.Set.of_list left) (Int.Set.of_list right)))
    | _ ->
        Format.asprintf
          "Incompatible type in error join at %a."
          Location.pp left.location
        |> Log.debug "%s";
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
        with TypeOrder.Undefined _ ->
          errors
      in
      match error with
      | { kind = MissingAttributeAnnotation { parent; missing_annotation = { name; _ }; _ }; _ } ->
          filtered ~key:((Annotated.Class.show parent) ^ (Access.show name))
      | { kind = MissingGlobalAnnotation { name; _ }; _ } ->
          filtered ~key:(Access.show name)
      | _ ->
          errors
    in
    List.fold ~init:String.Map.empty ~f:filter errors
  in
  let add_joins errors error =
    let joined ~key =
      match Map.find joined_missing_annotations key with
      | Some { kind; _ } ->
          let new_error = { error with kind } in
          Map.set ~key:(show new_error) ~data:new_error errors
      | _ -> Map.set ~key:(show error) ~data:error errors
    in
    match error with
    | { kind = MissingAttributeAnnotation { parent; missing_annotation = { name; _ } }; _ }
      when not (due_to_analysis_limitations error) ->
        joined ~key:((Annotated.Class.show parent) ^ (Access.show name))
    | { kind = MissingGlobalAnnotation { name; _ }; _ }
      when not (due_to_analysis_limitations error) ->
        joined ~key:(Access.show name)
    | _ ->
        Map.set ~key:(show error) ~data:error errors
  in
  List.fold ~init:String.Map.empty ~f:add_joins errors
  |> Map.data


let process_ignores environment handles errors =
  let module Reader = (val environment : Environment.Handler) in
  let error_lookup = Location.Table.create () in
  let errors_with_ignore_suppression =
    let add_to_lookup ~key ~code =
      match Hashtbl.find error_lookup key with
      | Some codes -> Hashtbl.set ~key ~data:(code :: codes) error_lookup
      | _ -> Hashtbl.set ~key ~data:[code] error_lookup
    in
    let not_ignored error =
      add_to_lookup
        ~key:(key error)
        ~code:(code error);
      Reader.ignore_lines (key error)
      >>| (fun ignore_instance ->
          not (List.is_empty (Ignore.codes ignore_instance) ||
               List.mem ~equal:(=) (Ignore.codes ignore_instance) (code error)))
      |> Option.value ~default:true
    in
    List.filter ~f:not_ignored errors
  in
  let unused_ignores =
    let paths_from_handles =
      let get_path paths handle =
        AstSharedMemory.get_source handle
        >>| (fun { Source.path; _ } -> path :: paths)
        |> Option.value ~default:paths
      in
      List.fold ~init:[] ~f:get_path
    in
    let get_unused_ignores sofar path =
      let ignores =
        let key_to_ignores sofar key =
          Reader.ignore_lines key
          >>| (fun ignore -> ignore :: sofar)
          |> Option.value ~default:sofar
        in
        List.fold ~init:[] ~f:key_to_ignores (Reader.DependencyHandler.get_ignore_keys ~path)
      in
      let unused_ignores =
        let filter_active_ignores sofar ignore =
          match Ignore.kind ignore with
          | Ignore.TypeIgnore -> sofar
          | _ ->
              begin
                match Hashtbl.find error_lookup (Ignore.key ignore) with
                | Some codes ->
                    let unused_codes =
                      let find_unused sofar code =
                        if List.mem ~equal:(=) codes code then sofar else code :: sofar
                      in
                      List.fold ~init:[] ~f:find_unused (Ignore.codes ignore)
                    in
                    if List.is_empty (Ignore.codes ignore) || List.is_empty unused_codes then
                      sofar
                    else
                      { ignore with Ignore.codes = unused_codes } :: sofar
                | _ -> ignore :: sofar
              end
        in
        List.fold ~init:[] ~f:filter_active_ignores ignores
      in
      sofar @ unused_ignores
    in
    List.fold ~init:[] ~f:get_unused_ignores (paths_from_handles handles)
  in
  let create_unused_ignore_error errors unused_ignore =
    let error =
      {
        location = Ignore.location unused_ignore;
        kind = UnusedIgnore (Ignore.codes unused_ignore);
        define = {
          Node.location = Ignore.location unused_ignore;
          value = Statement.Define.create_toplevel []
        };
      }
    in
    error :: errors
  in
  List.fold ~init:errors_with_ignore_suppression ~f:create_unused_ignore_error unused_ignores


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
    | Top -> Top
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
    | InconsistentOverride ({ mismatch = { actual; expected }; _ } as inconsistent_override) ->
        InconsistentOverride {
          inconsistent_override with
          mismatch = { actual = dequalify actual; expected = dequalify expected };
        }
    | UninitializedAttribute ({ mismatch = { actual; expected }; _ } as inconsistent_usage) ->
        UninitializedAttribute {
          inconsistent_usage with
          mismatch = { actual = dequalify actual; expected = dequalify expected };
        }
    | UndefinedAttribute { annotation; attribute; class_attribute } ->
        let annotation =
          (* Don't dequalify optionals because we special case their display. *)
          if Type.is_optional_primitive annotation then
            annotation
          else
            dequalify annotation
        in
        UndefinedAttribute { annotation; attribute; class_attribute }

    | UndefinedMethod { annotation; call } ->
        UndefinedMethod { annotation = dequalify annotation; call }
    | UndefinedType annotation -> UndefinedType (dequalify annotation)
    | UnusedIgnore codes -> UnusedIgnore codes
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
          when name = parameter_name ->
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
