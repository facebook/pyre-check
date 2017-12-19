(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open Statement


type undefined_method = {
  annotation: Type.t;
  call: Annotated.Call.t;
}
[@@deriving compare, eq, show, sexp]


type mismatch = {
  actual: Type.t;
  expected: Type.t;
}
[@@deriving compare, eq, show, sexp]

type missing_parameter = {
  name: Identifier.t;
  annotation: Type.t;
  due_to_any: bool;
}
[@@deriving compare, eq, show, sexp]

type parameter_mismatch = {
  name: Identifier.t;
  position: int;
  callee: Statement.define;
  mismatch: mismatch;
}
[@@deriving compare, eq, show, sexp]


type missing_immutable = {
  name: Expression.access;
  annotation: Type.t;
  parent: Annotated.Class.t option;
  due_to_any: bool;
}
[@@deriving compare, eq, show, sexp]


type immutable_mismatch = {
  name: Expression.access;
  parent: Annotated.Class.t option;
  mismatch: mismatch;
  declare_location: Location.t;
}
[@@deriving compare, eq, show, sexp]

type initialization_mismatch = {
  name: Expression.access;
  parent: Annotated.Class.t;
  mismatch: mismatch;
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
  return_locations: int list;
  due_to_any: bool;
}
[@@deriving compare, eq, sexp]


let pp_missing_return format missing_return =
  Format.fprintf
    format
    "Annotation: %s, Return locations: %a"
    (Type.show missing_return.type_annotation)
    Sexp.pp (sexp_of_list sexp_of_int missing_return.return_locations)


let show_missing_return missing_return =
  Format.asprintf "%a" pp missing_return


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
[@@deriving compare, eq, show, sexp]


type t = {
  location: Location.t;
  kind: kind;
  define: Statement.define Node.t;
}
[@@deriving compare, eq, show, sexp]


include Hashable.Make(struct
    type nonrec t = t
    let compare = compare
    let hash = Hashtbl.hash
    let sexp_of_t = sexp_of_t
    let t_of_sexp = t_of_sexp
  end)


let location { location; _ } =
  location


let code { kind; _ } =
  match kind with
  | IncompatibleAwaitableType _ -> 12
  | IncompatibleParameterType _ -> 6
  | IncompatibleReturnType _ -> 7
  | IncompatibleType { parent; _ } ->
      (match parent with
       | Some _ -> 8
       | None -> 9)
  | InconsistentOverride { override; _ } ->
      (match override with
       | StrengthenedPrecondition -> 14
       | WeakenedPostcondition -> 15)
  | MissingParameterAnnotation _ -> 2
  | MissingReturnAnnotation _ -> 3
  | MissingAnnotation { parent; _ } ->
      (match parent with
       | Some _ -> 4
       | None -> 5)
  | Top -> 1
  | UndefinedMethod _ -> 10
  | UndefinedType _ -> 11
  | UninitializedField _ -> 13


let name { kind; _ } =
  match kind with
  | IncompatibleAwaitableType _ -> "Incompatible awaitable type"
  | IncompatibleParameterType _ -> "Incompatible parameter type"
  | IncompatibleReturnType _ -> "Incompatible return type"
  | IncompatibleType _ -> "Incompatible type"
  | InconsistentOverride _ -> "Inconsistent override"
  | MissingAnnotation _ -> "Missing annotation"
  | MissingParameterAnnotation _ -> "Missing parameter annotation"
  | MissingReturnAnnotation _ -> "Missing return annotation"
  | Top -> "Undefined error"
  | UndefinedMethod _ -> "Undefined method"
  | UndefinedType _ -> "Undefined type"
  | UninitializedField _ -> "Uninitialized field"


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
             "expected an awaitable but got %a."
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
    | MissingReturnAnnotation { type_annotation; return_locations; due_to_any = false } ->
        [
          (Format.asprintf
             "Returning %a but no return type is specified."
             Type.pp type_annotation);
          (Format.asprintf
             "Type %a was returned on %s %s, return type should be specified on line %d."
             Type.pp type_annotation
             (if (List.length return_locations) > 1 then "lines" else "line")
             (return_locations
              |> List.map
                ~f:Int.to_string
              |> String.concat ~sep:", ")
             (Location.line location))
        ]
    | MissingReturnAnnotation { type_annotation; return_locations; due_to_any = true } ->
        [
          (Format.asprintf
             "Returning %a but type `Any` is specified."
             Type.pp type_annotation);
          (Format.asprintf
             "Type %a was returned on %s %s, return type should be specified on line %d."
             Type.pp type_annotation
             (if (List.length return_locations) > 1 then "lines" else "line")
             (return_locations
              |> List.map
                ~f:Int.to_string
              |> String.concat ~sep:", ")
             (Location.line location))
        ]
    | MissingAnnotation { name; annotation; parent = Some parent; due_to_any = false } ->
        [
          Format.asprintf
            "Field %a of class %a has type %a but no type is specified."
            Instantiated.Access.pp name
            Instantiated.Access.pp (Annotated.Class.name parent)
            Type.pp annotation
        ]
    | MissingAnnotation { name; annotation; parent = Some parent; due_to_any = true } ->
        [
          Format.asprintf
            "Field %a of class %a has type %a but type `Any` is specified."
            Instantiated.Access.pp name
            Instantiated.Access.pp (Annotated.Class.name parent)
            Type.pp annotation
        ]
    | MissingAnnotation { name; annotation; parent = None; due_to_any = false } ->
        [
          Format.asprintf
            "Globally accessible field %a has type %a but no type is specified."
            Instantiated.Access.pp name
            Type.pp annotation
        ]
    | MissingAnnotation { name; annotation; parent = None; due_to_any = true } ->
        [
          Format.asprintf
            "Globally accessible field %a has type %a but type `Any` is specified."
            Instantiated.Access.pp name
            Type.pp annotation
        ]
    | IncompatibleParameterType {
        name;
        position;
        callee = { Define.name = callee_name; parent; _ };
        mismatch = { actual; expected };
      } ->
        let parent =
          match parent with
          | Some parent -> Format.asprintf "%a." Instantiated.Access.pp parent
          | _ -> ""
        in
        [
          Format.asprintf
            "%s parameter %a to call `%s%a` expected %a but got %a."
            (ordinal position)
            Identifier.pp name
            parent
            Instantiated.Access.pp callee_name
            Type.pp expected
            Type.pp actual
        ]
    | IncompatibleReturnType { actual; expected } ->
        [
          (Format.asprintf
             "expected %a but got %a."
             Type.pp expected
             Type.pp actual);
          (Format.asprintf
             "Type %a expected on line %d, specified on line %d."
             Type.pp expected
             error.location.Location.stop.Location.line
             error.define.Node.location.Location.start.Location.line)
        ]
    | IncompatibleType {
        name;
        parent = Some parent;
        mismatch = { actual; expected };
        declare_location;
      } ->
        [
          (Format.asprintf
             "field %a declared in class %a has type %a but is used as type %a."
             Instantiated.Access.pp name
             Instantiated.Access.pp (Annotated.Class.name parent)
             Type.pp expected
             Type.pp actual);
          (Format.asprintf
             "Field %a declared on line %d, incorrectly used on line %d."
             Instantiated.Access.pp name
             declare_location.Location.start.Location.line
             (Location.line location))
        ]
    | IncompatibleType {
        name;
        parent = None;
        mismatch = { actual; expected };
        _;
      } ->
        [
          (Format.asprintf
             "%a is declared to have type %a but is used as type %a."
             Instantiated.Access.pp name
             Type.pp expected
             Type.pp actual);
          (Format.asprintf
             "%a incorrectly used on line %d."
             Instantiated.Access.pp name
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
              if not (Type.equal actual Type.void) then
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
            Instantiated.Access.pp define_name
            Instantiated.Access.pp
            (Annotated.Method.parent overridden_method |> Annotated.Class.name);
          detail;
        ]
    | UndefinedMethod { annotation; call } ->
        let name =
          match Annotated.Call.name call with
          | { Node.value = Access access; _ } ->
              Instantiated.Access.show access
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
    | UninitializedField {
        name;
        parent;
        mismatch = { actual; expected };
      } ->
        [
          (Format.asprintf
             "field %a is declared in class %a to have non-optional type %a but is never \
              initialized."
             Instantiated.Access.pp name
             Instantiated.Access.pp (Annotated.Class.name parent)
             Type.pp expected);
          (Format.asprintf
             "Field %a is declared on line %d, never initialized and therefore must be %a."
             Instantiated.Access.pp name
             (Location.line location)
             Type.pp actual)
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
  | IncompatibleType { mismatch = { actual; _ }; _ }
  | InconsistentOverride { mismatch = { actual; _ }; _ }
  | MissingAnnotation { annotation = actual; _ }
  | MissingParameterAnnotation { annotation = actual; _ }
  | MissingReturnAnnotation { type_annotation = actual; _ }
  | UninitializedField { mismatch = {actual; _ }; _ }->
      Type.is_unknown actual
  | Top -> true
  | UndefinedMethod { annotation; _ }
  | UndefinedType annotation ->
      Type.is_unknown annotation


let due_to_mismatch_with_any { kind; _ } =
  match kind with
  | InconsistentOverride _
  | MissingAnnotation _
  | MissingParameterAnnotation _
  | MissingReturnAnnotation _
  | Top
  | UndefinedMethod _
  | UndefinedType _ ->
      false
  | IncompatibleAwaitableType actual ->
      Type.equal actual Type.Object
  | IncompatibleParameterType { mismatch = { actual; expected }; _ }
  | IncompatibleReturnType { actual; expected }
  | IncompatibleType { mismatch = { actual; expected }; _ }
  | UninitializedField { mismatch = { actual; expected }; _ }->
      Type.mismatch_with_any actual expected


let less_or_equal ~resolution left right =
  let order = Resolution.order resolution in

  let less_or_equal_mismatch left right =
    TypeOrder.less_or_equal order ~left:left.actual ~right:right.actual &&
    TypeOrder.less_or_equal order ~left:left.expected ~right:right.expected
  in
  Location.equal left.location right.location &&
  (match left.kind, right.kind with
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
         ~left: left.type_annotation
         ~right: right.type_annotation
   | MissingAnnotation left, MissingAnnotation right
     when left.name = right.name && left.due_to_any = right.due_to_any ->
       TypeOrder.less_or_equal
         order
         ~left:left.annotation
         ~right:right.annotation
   | IncompatibleParameterType left, IncompatibleParameterType right when left.name = right.name ->
       less_or_equal_mismatch left.mismatch right.mismatch
   | IncompatibleReturnType left, IncompatibleReturnType right ->
       less_or_equal_mismatch left right
   | IncompatibleType left, IncompatibleType right when left.name = right.name ->
       less_or_equal_mismatch left.mismatch right.mismatch
   | InconsistentOverride left, InconsistentOverride right ->
       less_or_equal_mismatch left.mismatch right.mismatch
   | UninitializedField left, UninitializedField right when left.name = right.name ->
       less_or_equal_mismatch left.mismatch right.mismatch
   | UndefinedMethod left, UndefinedMethod right ->
       TypeOrder.less_or_equal
         order
         ~left:left.annotation
         ~right:right.annotation
   | UndefinedType left, UndefinedType right ->
       TypeOrder.less_or_equal order ~left ~right
   | _, Top -> true
   | _ ->
       false)


let join ~resolution left right =
  let order = Resolution.order resolution in
  let join_mismatch left right =
    {
      expected = TypeOrder.join order left.expected right.expected;
      actual = TypeOrder.join order left.actual right.actual;
    }
  in
  let class_equal left right =
    Instantiated.Access.equal
      (Annotated.Class.name left)
      (Annotated.Class.name right)
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
          type_annotation = TypeOrder.join order left.type_annotation right.type_annotation;
          return_locations =
            Int.Set.of_list (left.return_locations @ right.return_locations)
            |> Set.to_list;
          due_to_any = left.due_to_any && right.due_to_any;
        }
    | MissingAnnotation left, MissingAnnotation right
      when left.name = right.name && Option.equal class_equal left.parent right.parent ->
        let annotation =
          TypeOrder.join order left.annotation right.annotation
        in
        MissingAnnotation { left with annotation }
    | IncompatibleParameterType left, IncompatibleParameterType right
      when left.name = right.name &&
           left.position = right.position &&
           (Define.equal Statement.equal) left.callee right.callee ->
        IncompatibleParameterType {
          left with mismatch = join_mismatch left.mismatch right.mismatch
        }
    | IncompatibleReturnType left, IncompatibleReturnType right ->
        IncompatibleReturnType (join_mismatch left right)
    | IncompatibleType left, IncompatibleType right
      when left.name = right.name && Option.equal class_equal left.parent right.parent ->
        IncompatibleType { left with mismatch = join_mismatch left.mismatch right.mismatch }
    | InconsistentOverride left, InconsistentOverride right ->
        InconsistentOverride { left with mismatch = join_mismatch left.mismatch right.mismatch }
    | UninitializedField left, UninitializedField right
      when left.name = right.name && class_equal left.parent right.parent ->
        UninitializedField { left with mismatch = join_mismatch left.mismatch right.mismatch }
    | UndefinedMethod left, UndefinedMethod right ->
        UndefinedMethod {
          annotation =
            TypeOrder.join order left.annotation right.annotation;
          call = left.call;
        }
    | UndefinedType left, UndefinedType right ->
        UndefinedType (TypeOrder.join order left right)
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
  let immutable_type_map, other_errors =
    let filter (error_map, errors) error =
      match error with
      | { kind = MissingAnnotation { name; parent; _ }; _ } ->
          (try
             let key =
               parent
               >>| Annotated.Class.show
               |> Option.value ~default:""
               |> (fun parent_string -> parent_string ^ (Instantiated.Access.show name))
             in
             let new_map =
               match Map.find error_map key with
               | Some existing_error ->
                   let joined_error = join ~resolution error existing_error in
                   if joined_error.kind <> Top then
                     Map.change ~f:(fun _ -> Some joined_error) error_map key
                   else
                     Map.add ~key ~data:error error_map
               | _ ->
                   Map.add ~key ~data:error error_map
             in
             new_map, errors
           with
           | TypeOrder.Undefined _ ->
               error_map, error :: errors)
      | _ -> error_map, error :: errors
    in
    List.fold ~init:(String.Map.empty, []) ~f:filter errors
  in
  let merge ~key:_ ~data:error errors =
    if due_to_analysis_limitations error then
      errors
    else
      error :: errors
  in
  Map.fold ~init:other_errors ~f:merge immutable_type_map


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
    | MissingReturnAnnotation ({ type_annotation; _ } as missing_return) ->
        MissingReturnAnnotation { missing_return with type_annotation = dequalify type_annotation; }
    | MissingAnnotation ({ annotation; _ } as immutable_type) ->
        MissingAnnotation {
          immutable_type with  annotation = dequalify annotation;
        }
    | IncompatibleParameterType ({ mismatch = { actual; expected }; _ } as parameter) ->
        IncompatibleParameterType {
          parameter with
          mismatch = { actual = dequalify actual; expected = dequalify expected };
        }
    | IncompatibleReturnType { actual; expected }  ->
        IncompatibleReturnType { actual = dequalify actual; expected = dequalify expected }
    | IncompatibleType ({ mismatch = { actual; expected }; _ } as immutable_type) ->
        IncompatibleType {
          immutable_type with
          mismatch = { actual = dequalify actual; expected = dequalify expected };
        }
    | InconsistentOverride ({ mismatch = { actual; expected }; _ } as inconsistent_override) ->
        InconsistentOverride {
          inconsistent_override with
          mismatch = { actual = dequalify actual; expected = dequalify expected };
        }
    | UninitializedField ({ mismatch = { actual; expected }; _ } as inconsistent_usage) ->
        UninitializedField {
          inconsistent_usage with
          mismatch = { actual = dequalify actual; expected = dequalify expected };
        }
    | UndefinedMethod { annotation; call } ->
        UndefinedMethod { annotation = dequalify annotation; call }
    | UndefinedType annotation -> UndefinedType (dequalify annotation)
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
    { define with Define.parameters; Define.return_annotation }
  in
  { error with kind; define = { Node.location; value = define} }



let to_json ~detailed ({ kind; define = { Node.value = define; _ }; location; _ } as error) =
  let function_name = Instantiated.Access.show define.Define.name in
  let print_annotation annotation =
    Format.asprintf "%a" Type.pp annotation
    |> String.strip ~drop:((=) '`')
    |> String.substr_replace_all ~pattern:" (void)" ~with_:""
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
    >>| Instantiated.Access.show
    >>| (fun string -> `String string)
    |> Option.value ~default:`Null
  in
  let inference_information =
    match kind with
    | MissingReturnAnnotation { type_annotation; _ } ->
        [
          "annotation", `String (print_annotation type_annotation);
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
    | MissingAnnotation { name; annotation; parent; _ } ->
        [
          "annotation", `String (print_annotation annotation);
          "parent", print_parent (parent >>| Annotated.Class.name);
          "field_name", `String (Instantiated.Access.show name);
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
