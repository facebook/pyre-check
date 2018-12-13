(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open Statement


type origin =
  | Class of { annotation: Type.t; class_attribute: bool }
  | Module of Access.t
[@@deriving compare, eq, show, sexp, hash]


type mismatch = {
  actual: Type.t;
  expected: Type.t;
  due_to_invariance: bool;
}
[@@deriving compare, eq, show, sexp, hash]


type missing_annotation = {
  name: Access.t;
  annotation: Type.t option;
  evidence_locations: Location.Instantiated.t list;
  due_to_any: bool;
}
[@@deriving compare, eq, sexp, show, hash]


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
  | UninitializedAttribute of {
      name: Access.t;
      parent: Type.t;
      mismatch: mismatch;
    }
  | UnusedIgnore of int list

  (* Additionals errors. *)
  | UnawaitedAwaitable of Access.t
  | TypedDictionaryAccessWithNonLiteral of string list
  | TypedDictionaryKeyNotFound of { typed_dictionary_name: Identifier.t; missing_key: string }
[@@deriving compare, eq, show, sexp, hash]


let code = function
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
  | Unpack _ -> 23
  | MissingTypeParameters _ -> 24
  | ImpossibleIsinstance _ -> 25
  | TypedDictionaryAccessWithNonLiteral _ -> 26
  | TypedDictionaryKeyNotFound _ -> 27
  | UnexpectedKeyword _ -> 28
  | NotCallable _ -> 29
  | AnalysisFailure _ -> 30

  (* Additional errors. *)
  | UnawaitedAwaitable _ -> 101


let name = function
  | AnalysisFailure _ -> "Analysis failure"
  | ImpossibleIsinstance _ -> "Impossible isinstance check"
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
  | MissingTypeParameters _ -> "Missing type parameters"
  | NotCallable _ -> "Call error"
  | RevealedType _ -> "Revealed type"
  | RedundantCast _ -> "Redundant cast"
  | TooManyArguments _ -> "Too many arguments"
  | Top -> "Undefined error"
  | TypedDictionaryAccessWithNonLiteral _ -> "TypedDict accessed with a non-literal"
  | TypedDictionaryKeyNotFound _ -> "TypedDict accessed with a missing key"
  | UnawaitedAwaitable _ -> "Unawaited awaitable"
  | UndefinedAttribute _ -> "Undefined attribute"
  | UndefinedName _ -> "Undefined name"
  | UndefinedType _ -> "Undefined type"
  | UnexpectedKeyword _ -> "Unexpected keyword"
  | UndefinedImport _ -> "Undefined import"
  | UninitializedAttribute _ -> "Uninitialized attribute"
  | UnusedIgnore _ -> "Unused ignore"
  | Unpack _ -> "Unable to unpack"


let messages ~detailed:_ ~define location kind =
  let {
    Location.start = { Location.line = start_line; _ };
    Location.stop = { Location.line = stop_line; _ };
    _;
  } = location
  in
  let { Node.value = { Define.name = define_name; _ }; _ } = define in
  let ordinal number =
    let suffix =
      if (number % 10 = 1) && (number % 100 <> 11) then "st"
      else if (number % 10 = 2) && (number % 100 <> 12) then "nd"
      else if (number % 10 = 3) && (number % 100 <> 13) then "rd"
      else "th"
    in
    (string_of_int number) ^ suffix
  in
  let invariance_message =
    " See https://pyre-check.org/docs/error-types.html#list-and-dictionary-mismatches" ^
    "-with-subclassing for mutable container errors."
  in
  match kind with
  | AnalysisFailure annotation ->
      [
        Format.asprintf
          "Terminating analysis because type `%a` is not defined."
          Type.pp annotation
      ]
  | ImpossibleIsinstance { expression; mismatch = { actual; expected; _ } } ->
      let expression_string = Expression.show expression in
      [
        Format.asprintf
          "`%s` has type `%a`, checking if `%s` not isinstance `%a` will always fail."
          expression_string
          Type.pp actual
          expression_string
          Type.pp expected
      ]
  | IncompatibleAwaitableType actual ->
      [
        (Format.asprintf
           "Expected an awaitable but got `%a`."
           Type.pp actual
        );
      ]
  | Top -> [ "Problem with analysis." ]
  | MissingAttributeAnnotation { parent; missing_annotation } ->
      begin
        match missing_annotation with
        | { name; annotation = Some annotation; due_to_any; _ }
          when Type.equal annotation Type.Bottom ||
               Type.equal annotation Type.Object ||
               Type.is_unknown annotation ->
            begin
              if due_to_any then
                [
                  Format.asprintf
                    "Attribute `%a` of class `%a` must have a type other than `Any`."
                    Access.pp name
                    Type.pp parent;
                ]
              else
                [
                  Format.asprintf
                    "Attribute `%a` of class `%a` has no type specified."
                    Access.pp name
                    Type.pp parent;
                ]
            end
        | { name; annotation = Some annotation; evidence_locations; due_to_any } ->
            let detail =
              let evidence_string =
                evidence_locations
                |> List.map ~f:(Format.asprintf "%a" Location.Instantiated.pp_start)
                |> String.concat ~sep:", "
              in
              Format.asprintf
                "Attribute `%a` declared on line %d, type `%a` deduced from %s."
                Access.pp name
                start_line
                Type.pp annotation
                evidence_string
            in
            begin
              if due_to_any then
                [
                  Format.asprintf
                    "Attribute `%a` of class `%a` has type `%a` but type `Any` is specified."
                    Access.pp name
                    Type.pp parent
                    Type.pp annotation;
                  detail;
                ]
              else
                [
                  Format.asprintf
                    "Attribute `%a` of class `%a` has type `%a` but no type is specified."
                    Access.pp name
                    Type.pp parent
                    Type.pp annotation;
                  detail;
                ]
            end
        | { name; annotation = None; _ } ->
            [
              Format.asprintf "Attribute `%a` of class `%a` has no type specified."
                Access.pp name
                Type.pp parent;
            ]
      end
  | MissingParameterAnnotation { name; annotation; due_to_any }
    when Type.equal annotation Type.Bottom ||
         Type.equal annotation Type.Object ||
         Type.is_unknown annotation ->
      begin
        if due_to_any then
          [
            Format.asprintf
              "Parameter `%s` must have a type other than `Any`."
              (Access.show_sanitized name)
          ]
        else
          [
            Format.asprintf
              "Parameter `%s` has no type specified."
              (Access.show_sanitized name)
          ]
      end
  | MissingParameterAnnotation { name; annotation; due_to_any } ->
      begin
        if due_to_any then
          [
            Format.asprintf
              "Parameter `%s` has type `%a` but type `Any` is specified."
              (Access.show_sanitized name)
              Type.pp annotation
          ]
        else
          [
            Format.asprintf
              "Parameter `%s` has type `%a` but no type is specified."
              (Access.show_sanitized name)
              Type.pp annotation
          ]
      end
  | MissingReturnAnnotation { annotation; due_to_any; _ }
    when Type.equal annotation Type.Bottom ||
         Type.equal annotation Type.Object ||
         Type.is_unknown annotation ->
      begin
        if due_to_any then
          ["Return type must be specified as type other than `Any`."]
        else
          ["Return type is not specified."]
      end
  | MissingReturnAnnotation { annotation; evidence_locations; due_to_any } ->
      let detail =
        Format.asprintf
          "Type `%a` was returned on %s %s, return type should be specified on line %d."
          Type.pp annotation
          (if (List.length evidence_locations) > 1 then "lines" else "line")
          (evidence_locations
           |> List.map
             ~f:Int.to_string
           |> String.concat ~sep:", ")
          start_line
      in
      begin
        if due_to_any then
          [
            (Format.asprintf
               "Returning `%a` but type `Any` is specified."
               Type.pp annotation);
            detail;
          ]
        else
          [
            (Format.asprintf
               "Returning `%a` but no return type is specified."
               Type.pp annotation);
            detail;
          ]
      end
  | MissingGlobalAnnotation {
      name;
      annotation = Some annotation;
      evidence_locations;
      due_to_any;
    } ->
      begin
        let evidence_string =
          evidence_locations
          |> List.map ~f:(Format.asprintf "%a" Location.Instantiated.pp_start)
          |> String.concat ~sep:", "
        in
        if due_to_any then
          [
            Format.asprintf
              "Globally accessible variable `%s` has type `%a` but type `Any` is specified."
              (Access.show_sanitized name)
              Type.pp annotation;
            Format.asprintf
              "Global variable `%s` declared on line %d, type `%a` deduced from %s."
              (Access.show_sanitized name)
              start_line
              Type.pp annotation
              evidence_string
          ]
        else
          [
            Format.asprintf
              "Globally accessible variable `%s` has type `%a` but no type is specified."
              (Access.show_sanitized name)
              Type.pp annotation;
            Format.asprintf
              "Global variable `%s` declared on line %d, type `%a` deduced from %s."
              (Access.show_sanitized name)
              start_line
              Type.pp annotation
              evidence_string
          ]
      end
  | MissingGlobalAnnotation {
      name;
      annotation = None;
      _;
    } ->
      [
        Format.asprintf
          "Globally accessible variable `%s` has no type specified."
          (Access.show name);
      ]
  | MissingTypeParameters { annotation; number_of_parameters } ->
      [
        Format.asprintf
          "Generic type `%a` expects %d type parameters."
          Type.pp annotation
          number_of_parameters;
      ]
  | IncompatibleParameterType {
      name;
      position;
      callee;
      mismatch = { actual; expected; due_to_invariance };
    } ->
      let target =
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
              Format.asprintf "call `%a`" Access.pp callee
          | _ ->
              "anoynmous call"
        in
        Format.asprintf "%s %s to %s" (ordinal position) parameter callee
      in
      let invariance_message = if due_to_invariance then [invariance_message] else [] in
      Format.asprintf
        "Expected `%a` for %s but got `%a`."
        Type.pp expected
        target
        Type.pp actual
      :: invariance_message;


  | IncompatibleConstructorAnnotation annotation ->
      [
        Format.asprintf
          "`__init__` is annotated as returning `%a`, but it should return `None`."
          Type.pp
          annotation;
      ]
  | IncompatibleReturnType { mismatch = { actual; expected; due_to_invariance }; is_implicit } ->
      let detail =
        (Format.asprintf
           "Type `%a` expected on line %d, specified on line %d.%s"
           Type.pp expected
           stop_line
           define.Node.location.Location.start.Location.line
           (if due_to_invariance then " " ^ invariance_message else ""))
      in
      let message =
        if is_implicit then
          Format.asprintf
            "Expected `%a` but got implicit return value of `None`."
            Type.pp expected
        else
          Format.asprintf
            "Expected `%a` but got `%a`."
            Type.pp expected
            Type.pp actual
      in
      [message; detail]
  | IncompatibleAttributeType {
      parent;
      incompatible_type = {
        name;
        mismatch = { actual; expected; due_to_invariance };
        declare_location;
      };
    } ->
      let detail =
        if due_to_invariance then
          invariance_message
        else
          Format.asprintf
            "Attribute `%a` declared on line %d, incorrectly used on line %d."
            Access.pp name
            declare_location.Location.start.Location.line
            start_line
      in
      [
        (Format.asprintf
           "Attribute `%a` declared in class `%a` has type `%a` but is used as type `%a`."
           Access.pp name
           Type.pp parent
           Type.pp expected
           Type.pp actual);
        detail;
      ]
  | IncompatibleVariableType {
      name;
      mismatch = { actual; expected; due_to_invariance };
      _;
    } ->
      let message =
        if Type.is_tuple expected && not (Type.is_tuple actual) then
          Format.asprintf "Unable to unpack `%a`, expected a `Tuple`." Type.pp actual
        else
          Format.asprintf
            "%s is declared to have type `%a` but is used as type `%a`."
            (Access.show_sanitized name)
            Type.pp expected
            Type.pp actual
      in
      let detail =
        Format.asprintf
          "Redeclare `%s` on line %d if you wish to override the previously declared type.%s"
          (Access.show_sanitized name)
          start_line
          (if due_to_invariance then " " ^ invariance_message else "")
      in
      [message; detail]
  | InconsistentOverride { parent; override; _ } ->
      let detail =
        match override with
        | WeakenedPostcondition { actual; expected; due_to_invariance } ->
            if Type.equal actual Type.Top then
              Format.asprintf
                "The overriding method is not annotated but should return a subtype of `%a`."
                Type.pp expected
            else
            if due_to_invariance then
              invariance_message
            else
              Format.asprintf
                "Returned type `%a` is not a subtype of the overridden return `%a`."
                Type.pp actual
                Type.pp expected
        | StrengthenedPrecondition (Found { actual; expected; due_to_invariance }) ->
            let extra_detail =
              if due_to_invariance then " " ^ invariance_message else ""
            in
            Format.asprintf
              "Parameter of type `%a` is not a supertype of the overridden parameter `%a`.%s"
              Type.pp actual
              Type.pp expected
              extra_detail
        | StrengthenedPrecondition (NotFound name) ->
            Format.asprintf
              "Could not find parameter `%s` in overriding signature."
              (Access.show_sanitized name)
      in
      [
        Format.asprintf
          "`%a` overrides method defined in `%a` inconsistently. %s"
          Access.pp define_name
          Access.pp parent
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
  | NotCallable annotation ->
      [ Format.asprintf "`%a` is not a function." Type.pp annotation ]
  | TooManyArguments { callee; expected; provided } ->
      let callee =
        match callee with
        | Some name ->
            Format.asprintf "Call `%a`" Access.pp name
        | _ ->
            "Anonymous call"
      in
      [
        Format.asprintf "%s expects %d argument%s, %d %s provided."
          callee
          expected
          (if expected <> 1 then "s" else "")
          provided
          (if provided > 1 then "were" else "was");
      ]
  | TypedDictionaryAccessWithNonLiteral acceptable_keys ->
      let acceptable_keys =
        List.map acceptable_keys ~f:(Format.sprintf "'%s'")
        |> String.concat ~sep:", "
      in
      [
        Format.asprintf
          "TypedDict key must be a string literal; expected one of (%s)."
          acceptable_keys
      ]
  | TypedDictionaryKeyNotFound { typed_dictionary_name; missing_key } ->
      if Identifier.show typed_dictionary_name = "$anonymous" then
        [ Format.asprintf "TypedDict has no key `%s`." missing_key ]
      else
        [
          Format.asprintf
            "TypedDict `%a` has no key `%s`."
            Identifier.pp typed_dictionary_name
            missing_key
        ]
  | Unpack { expected_count; unpack_problem } ->
      begin
        match unpack_problem with
        | UnacceptableType bad_type ->
            [Format.asprintf
               "Unable to unpack `%a` into %d values."
               Type.pp
               bad_type
               expected_count]
        | CountMismatch actual_count ->
            let value_message =
              if actual_count = 1 then
                "single value"
              else
                Format.sprintf "%d values" actual_count
            in
            [Format.sprintf "Unable to unpack %s, %d were expected." value_message expected_count]
      end
  | RedundantCast annotation ->
      [
        Format.asprintf
          "The value being cast is already of type `%a`."
          Type.pp annotation;
      ]
  | RevealedType { expression; annotation } ->
      let show_sanitized { Node.location; value } =
        match value with
        | Access access ->
            Access.show_sanitized access
        | _ ->
            Expression.show { Node.location; value }
      in
      [
        Format.asprintf
          "Revealed type for `%s` is `%s`."
          (show_sanitized expression)
          (Type.show annotation);
      ]
  | UnawaitedAwaitable name ->
      [Format.asprintf "`%a` is never awaited." Access.pp name]
  | UndefinedAttribute { attribute; origin } ->
      let target =
        match origin with
        | Class { annotation; _ } ->
            let name =
              if Type.is_optional_primitive annotation then
                "Optional type"
              else
                Format.asprintf "`%a`" Type.pp annotation
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
          "Type `%a` is not defined."
          Type.pp annotation
      ]
  | UnexpectedKeyword { name; callee } ->
      let callee =
        match callee with
        | Some name ->
            Format.asprintf "call `%a`" Access.pp name
        | _ ->
            "anonymous call"
      in
      [
        Format.asprintf
          "Unexpected keyword argument `%s` to %s."
          (Identifier.show_sanitized name)
          callee
      ]
  | UninitializedAttribute {
      name;
      parent;
      mismatch = { actual; expected; _ };
    } ->
      [
        (Format.asprintf
           "Attribute `%a` is declared in class `%a` to have non-optional type `%a` but is never \
            initialized."
           Access.pp name
           Type.pp parent
           Type.pp expected);
        (Format.asprintf
           "Attribute `%a` is declared on line %d, never initialized and therefore must be `%a`."
           Access.pp name
           start_line
           Type.pp actual)
      ]
  | UnusedIgnore codes ->
      let string_from_codes codes =
        if List.length codes > 0 then
          List.map codes ~f:Int.to_string
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


let inference_information
    ~define:
    {
      Node.value = {
        Define.name;
        parameters;
        return_annotation;
        decorators;
        parent;
        async;
        _ };
      _;
    }
    kind =
  let print_annotation annotation =
    Format.asprintf "`%a`" Type.pp annotation
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
      `Assoc [
        "name", `String (Identifier.show_sanitized name);
        "type", annotation;
        "value", value
      ]
    in
    List.map parameters ~f:to_json
  in
  let decorators =
    let decorator_to_json decorator = `String (Expression.show decorator) in
    List.map decorators ~f:decorator_to_json
  in
  let print_parent parent =
    parent
    >>| Access.show_sanitized
    >>| (fun string -> `String string)
    |> Option.value ~default:`Null
  in
  let function_name = Access.show_sanitized name in
  match kind with
  | MissingReturnAnnotation { annotation; _ } ->
      `Assoc [
        "annotation", `String (print_annotation annotation);
        "parent", print_parent parent;
        "function_name", `String function_name;
        "parameters", `List parameters;
        "decorators", `List decorators;
        "async", `Bool async;
      ]
  | MissingParameterAnnotation _ ->
      let return_annotation =
        return_annotation
        >>| Format.asprintf "%a" Expression.pp
        >>| (fun string -> `String string)
        |> Option.value ~default:`Null
      in
      `Assoc [
        "annotation", return_annotation;
        "parent", print_parent parent;
        "function_name", `String function_name;
        "parameters", `List parameters;
        "decorators", `List decorators;
        "async", `Bool async;
      ]
  | MissingAttributeAnnotation { parent; missing_annotation = { name; annotation; _ } } ->
      let attributes =
        [
          "parent", `String (Type.show parent);
          "attribute_name", `String (Access.show_sanitized name);
        ]
      in
      begin
        match annotation with
        | Some annotation ->
            `Assoc (("annotation", `String (print_annotation annotation)) :: attributes)
        | None ->
            `Assoc attributes
      end
  | MissingGlobalAnnotation { name; annotation; _ } ->
      let attributes =
        [
          "parent", `Null;
          "attribute_name", `String (Access.show_sanitized name);
        ]
      in
      begin
        match annotation with
        | Some annotation ->
            `Assoc (("annotation", `String (print_annotation annotation)) :: attributes)
        | None ->
            `Assoc attributes
      end
  | _ -> `Assoc []


include BaseError.Make(struct
    type t = kind
    let compare = compare_kind
    let hash = hash_kind
    let show = show_kind
    let hash_fold_t = hash_fold_kind
    let sexp_of_t = sexp_of_kind
    let t_of_sexp = kind_of_sexp
    let pp = pp_kind
    let equal = equal_kind

    let code = code
    let name = name
    let messages = messages
    let inference_information = inference_information
  end)


let due_to_analysis_limitations { kind; _ } =
  match kind with
  | ImpossibleIsinstance { mismatch = { actual; _ }; _ }
  | IncompatibleAwaitableType actual
  | IncompatibleParameterType { mismatch = { actual; _ }; _ }
  | IncompatibleReturnType { mismatch = { actual; _ }; _ }
  | IncompatibleAttributeType { incompatible_type = { mismatch = { actual; _ }; _ }; _ }
  | IncompatibleVariableType { mismatch = { actual; _ }; _ }
  | InconsistentOverride { override = StrengthenedPrecondition (Found { actual; _ }); _ }
  | InconsistentOverride { override = WeakenedPostcondition { actual; _ }; _ }
  | MissingAttributeAnnotation { missing_annotation = { annotation = Some actual; _ }; _ }
  | MissingGlobalAnnotation { annotation = Some actual; _ }
  | MissingParameterAnnotation { annotation = actual; _ }
  | MissingReturnAnnotation { annotation = actual; _ }
  | MissingTypeParameters { annotation = actual; _ }
  | NotCallable actual
  | RedundantCast actual
  | UninitializedAttribute { mismatch = {actual; _ }; _ }->
      Type.is_unknown actual ||
      Type.is_type_alias actual
  | Top -> true
  | UndefinedAttribute { origin = Class { annotation; _ }; _ } ->
      Type.is_unknown annotation
  | AnalysisFailure _
  | IncompatibleConstructorAnnotation _
  | InconsistentOverride { override = StrengthenedPrecondition (NotFound _); _ }
  | MissingArgument _
  | MissingAttributeAnnotation _
  | MissingGlobalAnnotation _
  | TooManyArguments _
  | TypedDictionaryAccessWithNonLiteral _
  | TypedDictionaryKeyNotFound _
  | Unpack _
  | RevealedType _
  | UnawaitedAwaitable _
  | UndefinedAttribute _
  | UndefinedName _
  | UndefinedImport _
  | UndefinedType _
  | UnexpectedKeyword _
  | UnusedIgnore _ ->
      false


let due_to_unsupported_calls { kind; _ } =
  match kind with
  | MissingArgument { callee = Some name; _ }
  | TooManyArguments { callee = Some name; _ } ->
      List.last name
      >>| (fun name -> Access.show [name])
      >>| List.mem ~equal:String.equal ["__init__"; "__str__"]
      |> Option.value ~default:false
  | _ ->
      false


let due_to_builtin_import { kind; _ } =
  match kind with
  | UndefinedImport import ->
      Access.show import = "builtins"
  | _ ->
      false


let due_to_mismatch_with_any { kind; _ } =
  match kind with
  | IncompatibleAwaitableType actual
  | NotCallable actual
  | UndefinedAttribute { origin = Class { annotation = actual; _ }; _ } ->
      Type.equal actual Type.Object
  | ImpossibleIsinstance { mismatch = { actual; expected; _ }; _ }
  | InconsistentOverride { override = StrengthenedPrecondition (Found { actual; expected; _ }); _ }
  | InconsistentOverride { override = WeakenedPostcondition { actual; expected; _ }; _ }
  | IncompatibleParameterType { mismatch = { actual; expected; _ }; _ }
  | IncompatibleReturnType { mismatch = { actual; expected; _ }; _ }
  | IncompatibleAttributeType { incompatible_type = { mismatch = { actual; expected; _ }; _ }; _ }
  | IncompatibleVariableType { mismatch = { actual; expected; _ }; _ }
  | UninitializedAttribute { mismatch = { actual; expected; _ }; _ } ->
      Type.mismatch_with_any actual expected
  | AnalysisFailure _
  | TooManyArguments _
  | Unpack _
  | MissingAttributeAnnotation _
  | MissingGlobalAnnotation _
  | MissingParameterAnnotation _
  | MissingReturnAnnotation _
  | MissingTypeParameters _
  | RedundantCast _
  | RevealedType _
  | IncompatibleConstructorAnnotation _
  | InconsistentOverride _
  | Top
  | TypedDictionaryAccessWithNonLiteral _
  | TypedDictionaryKeyNotFound _
  | UndefinedType _
  | UnexpectedKeyword _
  | MissingArgument _
  | UnawaitedAwaitable _
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
  Location.Instantiated.equal left.location right.location &&
  begin
    match left.kind, right.kind with
    | AnalysisFailure left, AnalysisFailure right ->
        Type.equal left right
    | ImpossibleIsinstance left, ImpossibleIsinstance right
      when Expression.equal left.expression right.expression ->
        less_or_equal_mismatch left.mismatch right.mismatch
    | IncompatibleAwaitableType left, IncompatibleAwaitableType right ->
        Resolution.less_or_equal resolution ~left ~right
    | MissingTypeParameters { annotation = left; number_of_parameters = left_parameters },
      MissingTypeParameters { annotation = right; number_of_parameters = right_parameters }
      when left_parameters = right_parameters ->
        Resolution.less_or_equal resolution ~left ~right
    | MissingArgument left, MissingArgument right ->
        Option.equal Access.equal left.callee right.callee &&
        Access.equal left.name right.name
    | MissingParameterAnnotation left, MissingParameterAnnotation right
      when left.name = right.name ->
        Resolution.less_or_equal resolution ~left:left.annotation ~right:right.annotation
    | MissingReturnAnnotation left, MissingReturnAnnotation right ->
        Resolution.less_or_equal resolution ~left: left.annotation ~right: right.annotation
    | MissingAttributeAnnotation { missing_annotation = left; _ },
      MissingAttributeAnnotation { missing_annotation = right; _ }
    | MissingGlobalAnnotation left, MissingGlobalAnnotation right
      when (Access.equal left.name right.name) && left.due_to_any = right.due_to_any ->
        begin
          match left.annotation, right.annotation with
          | Some left, Some right ->
              Resolution.less_or_equal resolution ~left ~right
          | None, None ->
              true
          | _ ->
              false
        end
    | NotCallable left, NotCallable right ->
        Resolution.less_or_equal resolution ~left ~right
    | RedundantCast left, RedundantCast right ->
        Resolution.less_or_equal resolution ~left ~right
    | RevealedType left, RevealedType right ->
        Expression.equal left.expression right.expression &&
        Resolution.less_or_equal resolution ~left:left.annotation ~right:right.annotation
    | IncompatibleParameterType left, IncompatibleParameterType right
      when Option.equal Access.equal left.name right.name ->
        less_or_equal_mismatch left.mismatch right.mismatch
    | IncompatibleConstructorAnnotation left, IncompatibleConstructorAnnotation right ->
        Resolution.less_or_equal resolution ~left ~right
    | IncompatibleReturnType left, IncompatibleReturnType right ->
        less_or_equal_mismatch left.mismatch right.mismatch
    | IncompatibleAttributeType left, IncompatibleAttributeType right
      when Type.equal left.parent right.parent &&
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
        Option.equal Access.equal left.callee right.callee &&
        left.expected = right.expected &&
        left.provided = right.provided
    | UninitializedAttribute left, UninitializedAttribute right when left.name = right.name ->
        less_or_equal_mismatch left.mismatch right.mismatch
    | UnawaitedAwaitable left, UnawaitedAwaitable right ->
        Access.equal left right
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
        Type.equal left right
    | UnexpectedKeyword left, UnexpectedKeyword right ->
        Option.equal Access.equal left.callee right.callee &&
        Identifier.equal left.name right.name
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
      due_to_invariance = left.due_to_invariance || right.due_to_invariance;
    }
  in
  let join_missing_annotation
      (left: missing_annotation)  (* Ohcaml... *)
      (right: missing_annotation): missing_annotation =
    let annotation =
      match left.annotation, right.annotation with
      | Some left, Some right ->
          Some (Resolution.join resolution left right)
      | None, Some annotation
      | Some annotation, None ->
          Some annotation
      | None, None ->
          None
    in
    {
      left with
      annotation;
      evidence_locations =
        List.dedup_and_sort
          ~compare:Location.Instantiated.compare
          (left.evidence_locations @ right.evidence_locations);
      due_to_any = left.due_to_any && right.due_to_any;
    }
  in
  let kind =
    match left.kind, right.kind with
    | AnalysisFailure left, AnalysisFailure right ->
        AnalysisFailure (Type.union [left; right])
    | IncompatibleAwaitableType left, IncompatibleAwaitableType right ->
        IncompatibleAwaitableType (Resolution.join resolution left right)
    | MissingArgument left, MissingArgument right
      when Option.equal Access.equal left.callee right.callee &&
           Access.equal left.name right.name ->
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
      when (Access.equal left.missing_annotation.name right.missing_annotation.name) &&
           Type.equal left.parent right.parent ->
        MissingAttributeAnnotation {
          parent = left.parent;
          missing_annotation =
            join_missing_annotation
              left.missing_annotation
              right.missing_annotation;
        }
    | MissingGlobalAnnotation left, MissingGlobalAnnotation right
      when Access.equal left.name right.name ->
        MissingGlobalAnnotation (join_missing_annotation left right)
    | MissingTypeParameters { annotation = left; number_of_parameters = left_parameters },
      MissingTypeParameters { annotation = right; number_of_parameters = right_parameters }
      when left_parameters = right_parameters ->
        MissingTypeParameters {
          annotation = Resolution.join resolution left right;
          number_of_parameters = left_parameters;
        }
    | NotCallable left, NotCallable right ->
        NotCallable (Resolution.join resolution left right)
    | RedundantCast left, RedundantCast right ->
        RedundantCast (Resolution.join resolution left right)
    | RevealedType left, RevealedType right
      when Expression.equal left.expression right.expression ->
        RevealedType {
          left with
          annotation = Resolution.join resolution left.annotation right.annotation;
        }
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
        IncompatibleReturnType {
          mismatch = join_mismatch left.mismatch right.mismatch;
          is_implicit = left.is_implicit && right.is_implicit;
        }
    | IncompatibleAttributeType left, IncompatibleAttributeType right
      when Type.equal left.parent right.parent &&
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
    | TooManyArguments left, TooManyArguments right ->
        if Option.equal Access.equal left.callee right.callee &&
           left.expected = right.expected &&
           left.provided = right.provided then
          TooManyArguments left
        else
          Top
    | UninitializedAttribute left, UninitializedAttribute right
      when left.name = right.name && Type.equal left.parent right.parent ->
        UninitializedAttribute { left with mismatch = join_mismatch left.mismatch right.mismatch }
    | UnawaitedAwaitable left, UnawaitedAwaitable right when Access.equal left right ->
        UnawaitedAwaitable left
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
    | UndefinedType left, UndefinedType right when Type.equal left right ->
        UndefinedType left
    | UnexpectedKeyword left, UnexpectedKeyword right ->
        if Option.equal Access.equal left.callee right.callee &&
           Identifier.equal left.name right.name then
          UnexpectedKeyword left
        else
          Top
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

    | TypedDictionaryKeyNotFound left, TypedDictionaryKeyNotFound right
      when Identifier.equal left.typed_dictionary_name right.typed_dictionary_name &&
           left.missing_key = right.missing_key ->
        TypedDictionaryKeyNotFound left
    | TypedDictionaryAccessWithNonLiteral left, TypedDictionaryAccessWithNonLiteral right
      when left = right ->
        TypedDictionaryAccessWithNonLiteral left
    | _ ->
        Log.debug
          "Incompatible type in error join at %a: %a %a"
          Location.Instantiated.pp (location left)
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
        Type.show parent ^ Access.show name
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
      | IncompatibleReturnType { mismatch = { actual; _ }; _ }
      | IncompatibleVariableType { mismatch = { actual; _ }; _ }
      | UndefinedAttribute { origin = Class { annotation = actual; _ }; _ } ->
          let is_subclass_of_mock annotation =
            try
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
            with
            | TypeOrder.Untracked _ ->
                false
          in
          Type.exists actual ~predicate:is_subclass_of_mock
      | UnexpectedKeyword { callee = Some callee; _ } ->
          String.is_prefix ~prefix:"unittest.mock" (Access.show callee)
      | _ ->
          false
    in
    let is_unimplemented_return_error error =
      match error with
      | { kind = IncompatibleReturnType _; define = { Node.value = { Define.body; _ }; _ }; _ } ->
          let rec check_statements = function
            | [{ Node.value = Statement.Pass; _ };
               { Node.value = Statement.Return { Return.expression = None; _ }; _ }] ->
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
    let is_builtin_import_error = function
      | { kind = UndefinedImport builtins; _ } when Access.show builtins = "builtins" ->
          true
      | _ ->
          false
    in
    let is_stub_error error =
      String.is_suffix ~suffix:".pyi" (path error)
    in
    (* Ignore naming mismatches on parameters of dunder methods due to unofficial typeshed naming *)
    let is_override_on_dunder_method { kind; _ } =
      match kind with
      | InconsistentOverride { overridden_method; override; _ }
        when String.is_prefix ~prefix:"__" (Access.show overridden_method) &&
             String.is_suffix ~suffix:"__" (Access.show overridden_method) ->
          begin
            match override with
            | StrengthenedPrecondition (NotFound _) -> true
            | _ -> false
          end
      | _ -> false
    in

    is_stub_error error ||
    is_mock_error error ||
    is_unimplemented_return_error error ||
    is_builtin_import_error error ||
    is_override_on_dunder_method error
  in
  match configuration with
  | { Configuration.Analysis.debug = true; _ } -> errors
  | _ -> List.filter ~f:(fun error -> not (should_filter error)) errors


let suppress ~mode error =
  let suppress_in_strict ({ kind; _ } as error) =
    if due_to_analysis_limitations error then
      match kind with
      | AnalysisFailure _
      | TooManyArguments _
      | Unpack _
      | IncompatibleParameterType _
      | IncompatibleReturnType _
      | IncompatibleConstructorAnnotation _
      | MissingParameterAnnotation _
      | MissingReturnAnnotation _
      | MissingTypeParameters _
      | NotCallable _
      | TypedDictionaryAccessWithNonLiteral _
      | TypedDictionaryKeyNotFound _
      | UnawaitedAwaitable _
      | UndefinedAttribute _
      | UndefinedName _
      | UndefinedImport _
      | UndefinedType _
      | UnexpectedKeyword _
      | RedundantCast _
      | RevealedType _
      | MissingArgument _ ->
          false
      | ImpossibleIsinstance _
      | IncompatibleAwaitableType _
      | IncompatibleAttributeType _
      | IncompatibleVariableType _
      | InconsistentOverride _
      | MissingAttributeAnnotation _
      | MissingGlobalAnnotation _
      | Top
      | UninitializedAttribute _
      | UnusedIgnore _ ->
          true
    else
      match kind with
      | UndefinedImport _ ->
          due_to_builtin_import error
      | _ ->
          due_to_mismatch_with_any error
  in

  let suppress_in_default ({ kind; define = { Node.value = define; _ }; _ } as error) =
    match kind with
    | IncompatibleVariableType _ ->
        due_to_analysis_limitations error ||
        (Define.is_untyped define && not (Define.is_toplevel define))
    | InconsistentOverride { override = WeakenedPostcondition { actual = Type.Top; _ }; _ } ->
        false
    | InconsistentOverride {
        override = StrengthenedPrecondition (Found { expected = Type.Variable _; _ });
        _;
      } ->
        true
    | MissingReturnAnnotation _
    | MissingParameterAnnotation _
    | MissingAttributeAnnotation _
    | MissingGlobalAnnotation _
    | MissingTypeParameters _
    | Unpack { unpack_problem = UnacceptableType Type.Object; _ }
    | Unpack { unpack_problem = UnacceptableType Type.Top; _ } ->
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
        (Define.is_untyped define && not (Define.is_toplevel define))
  in

  let suppress_in_infer ({ kind; _ } as error) =
    match kind with
    | MissingReturnAnnotation { annotation = actual; _ }
    | MissingParameterAnnotation { annotation = actual; _ }
    | MissingAttributeAnnotation { missing_annotation = { annotation = Some actual; _ }; _ }
    | MissingGlobalAnnotation { annotation = Some actual; _ } ->
        due_to_analysis_limitations error ||
        Type.equal actual Type.Object ||
        Type.equal actual Type.Bottom
    | _ ->
        true
  in
  if Location.Instantiated.equal (Location.Instantiated.synthetic) (location error) then
    true
  else
    match mode with
    | Source.Infer ->
        suppress_in_infer error
    | Source.Strict ->
        suppress_in_strict error
    | Source.Declare ->
        true
    | Source.DefaultButDontCheck suppressed_codes
      when List.exists suppressed_codes ~f:((=) (code error)) ->
        true
    | _ ->
        suppress_in_default error


let dequalify
    dequalify_map
    ~resolution
    ({
      kind;
      define = { Node.location; value = ({ Define.parameters; return_annotation; _ } as define) };
      _;
    } as error) =
  let dequalify = Type.dequalify dequalify_map in
  let kind =
    match kind with
    | AnalysisFailure annotation ->
        AnalysisFailure (dequalify annotation)
    | ImpossibleIsinstance ({
        mismatch = { actual; expected; due_to_invariance };
        _;
      } as isinstance) ->
        ImpossibleIsinstance {
          isinstance with
          mismatch = {
            actual = dequalify actual;
            expected = dequalify expected;
            due_to_invariance;
          };
        }
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
          missing_annotation = { missing_annotation with annotation = annotation >>| dequalify };
        }
    | MissingGlobalAnnotation ({ annotation; _ } as immutable_type) ->
        MissingGlobalAnnotation {
          immutable_type with  annotation = annotation >>| dequalify;
        }
    | MissingTypeParameters { annotation; number_of_parameters } ->
        MissingTypeParameters { annotation = dequalify annotation; number_of_parameters }
    | NotCallable annotation ->
        NotCallable (dequalify annotation)
    | RedundantCast annotation ->
        RedundantCast (dequalify annotation)
    | RevealedType { expression; annotation } ->
        RevealedType { expression; annotation = dequalify annotation }
    | IncompatibleParameterType ({
        mismatch = { actual; expected; due_to_invariance };
        _;
      } as parameter) ->
        IncompatibleParameterType {
          parameter with
          mismatch = {
            actual = dequalify actual;
            expected = dequalify expected;
            due_to_invariance;
          };
        }
    | IncompatibleReturnType ({
        mismatch = { actual; expected; due_to_invariance };
        _;
      } as return) ->
        IncompatibleReturnType {
          return with
          mismatch = { actual = dequalify actual; expected = dequalify expected; due_to_invariance }
        }
    | IncompatibleAttributeType {
        parent;
        incompatible_type = {
          mismatch = { actual; expected; due_to_invariance };
          _;
        } as incompatible_type;
      } ->
        IncompatibleAttributeType {
          parent;
          incompatible_type = {
            incompatible_type with
            mismatch = {
              actual = dequalify actual;
              expected = dequalify expected;
              due_to_invariance;
            };
          };
        }
    | IncompatibleVariableType ({
        mismatch = { actual; expected; due_to_invariance };
        _;
      } as incompatible_type) ->
        IncompatibleVariableType {
          incompatible_type with
          mismatch = {
            actual = dequalify actual;
            expected = dequalify expected;
            due_to_invariance;
          };
        }
    | InconsistentOverride
        ({
          override = StrengthenedPrecondition (Found { actual; expected; due_to_invariance });
          _;
        } as inconsistent_override) ->
        InconsistentOverride {
          inconsistent_override with
          override = StrengthenedPrecondition (Found {
              actual = dequalify actual;
              expected = dequalify expected;
              due_to_invariance;
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
          override = WeakenedPostcondition { actual; expected; due_to_invariance };
          _;
        } as inconsistent_override) ->
        InconsistentOverride {
          inconsistent_override with
          override = WeakenedPostcondition {
              actual = dequalify actual;
              expected = dequalify expected;
              due_to_invariance;
            };
        }
    | TypedDictionaryAccessWithNonLiteral expression ->
        TypedDictionaryAccessWithNonLiteral expression
    | TypedDictionaryKeyNotFound key ->
        TypedDictionaryKeyNotFound key
    | UninitializedAttribute ({
        mismatch = { actual; expected; due_to_invariance };
        _;
      } as inconsistent_usage) ->
        UninitializedAttribute {
          inconsistent_usage with
          mismatch = {
            actual = dequalify actual;
            expected = dequalify expected;
            due_to_invariance;
          };
        }
    | UnawaitedAwaitable left ->
        UnawaitedAwaitable left
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
    | UnexpectedKeyword access ->
        UnexpectedKeyword access
    | MissingArgument missing_argument ->
        MissingArgument missing_argument
    | UnusedIgnore codes ->
        UnusedIgnore codes
    | Unpack unpack ->
        Unpack unpack
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
    let parameters = List.map parameters ~f:dequalify_parameter in
    let return_annotation =
      return_annotation
      >>| Resolution.parse_annotation resolution
      >>| dequalify
      >>| Type.expression
    in
    { define with Define.parameters; return_annotation }
  in
  { error with kind; define = { Node.location; value = define} }


let create_mismatch ~resolution ~actual ~expected ~covariant =
  let left, right =
    if covariant then
      actual, expected
    else
      expected, actual
  in
  {
    expected;
    actual;
    due_to_invariance = Resolution.is_invariance_mismatch resolution ~left ~right;
  }
