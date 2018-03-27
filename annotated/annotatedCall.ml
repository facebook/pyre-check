(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open Statement

module Annotation = AnalysisAnnotation
module Resolution = AnalysisResolution
module Type = AnalysisType
module Signature = AnalysisSignature

module Assign = AnnotatedAssign
module Class = AnnotatedClass
module Attribute = Class.Attribute
module Method = Class.Method
module Define = AnnotatedDefine


type kind =
  | Function
  | Method
[@@deriving compare, eq, sexp, show, hash]


type t = {
  call: Call.t;
  kind: kind;
}
[@@deriving compare, eq, sexp, show, hash]


let name_equal { call = { Call.name = left; _ }; _ } { call = { Call.name = right; _ }; _ } =
  Expression.equal left right


let create ~kind call =
  { call; kind }


let call { call; _ } =
  call


let name { call = { Call.name; _ }; _ } =
  name


let arguments { call = { Call.arguments; _ }; _ } =
  arguments


let with_arguments { call; kind } arguments =
  { call = { call with Call.arguments }; kind }


let insert_implicit_arguments { call; kind } ~callee ~location =
  let call =
    callee
    >>| (fun { Signature.instantiated = callee; _ } ->
        let prepend_self call =
          let self =
            {
              Argument.name = None;
              value = Node.create ~location (Access (Access.create "self"));
            }
          in
          { call with Call.arguments = self :: call.Call.arguments }
        in
        match kind with
        | Method ->
            if Statement.Define.is_static_method callee then
              call
            else
              prepend_self call
        | Function ->
            if Statement.Define.is_class_method callee ||
               (Statement.Define.is_constructor callee &&
                not (Call.is_explicit_constructor_call call)) then
              prepend_self call
            else
              call)
    |> Option.value ~default:call
  in
  { call; kind }


type redirect = {
  access: Access.t;
  call: Access.t;
}


let redirect { call = { Call.name; arguments }; kind = _ } =
  match name, arguments with
  | { Node.location; value = Access [Access.Identifier name]; _ },
    [{
      Argument.value = { Node.value = Access access; _ };
      _;
    } as argument] ->
      begin
        match Identifier.show name with
        | "abs" -> Some "__abs__"
        | "repr" -> Some "__repr__"
        | "str" -> Some "__str__"
        | _ -> None
      end
      >>| (fun name ->
          let call =
            [Access.Call {
                Node.location;
                value = {
                  Call.name = {
                    Node.location;
                    value = Access (Access.create name);
                  };
                  arguments = [argument];
                };
              }]
          in
          { access; call })

  | _ -> None


let backup { call = { Call.name; arguments }; kind } =
  match name with
  | { Node.location; value = Access [Access.Identifier name]; _ } ->
      (* cf. https://docs.python.org/3/reference/datamodel.html#object.__radd__ *)
      begin
        match Identifier.show name with
        | "__add__" -> Some "__radd__"
        | "__sub__" -> Some "__rsub__"
        | "__mul__" -> Some "__rmul__"
        | "__matmul__" -> Some "__rmatmul__"
        | "__truediv__" -> Some "__rtruediv__"
        | "__floordiv__" -> Some "__rfloordiv__"
        | "__mod__" -> Some "__rmod__"
        | "__divmod__" -> Some "__rdivmod__"
        | "__pow__" -> Some "__rpow__"
        | "__lshift__" -> Some "__rlshift__"
        | "__rshift__" -> Some "__rrshift__"
        | "__and__" -> Some "__rand__"
        | "__xor__" -> Some "__rxor__"
        | "__or__" -> Some "__ror__"
        | _ -> None
      end
      >>| (fun name ->
          {
            call = {
              Call.name = {
                Node.location;
                value = Access (Access.create name);
              };
              arguments;
            };
            kind;
          })
  | _ -> None


let argument_annotations { call = { Call.arguments; _ }; kind = _ } ~resolution =
  let extract_argument { Argument.value; _ } =
    match value with
    | { Node.location; Node.value = Starred (Starred.Once expression) } ->
        {
          Node.location;
          value = Signature.Starred (Resolution.resolve resolution expression);
        }
    | { Node.location; _ } ->
        {
          Node.location;
          value = Signature.Normal {
              Signature.annotation = Resolution.resolve resolution value;
              value;
            }
        }
  in
  List.map ~f:extract_argument arguments


let check_parameters
    ~resolution
    ~check_parameter
    ~add_error
    ~init
    call
    { Signature.instantiated = callee; _ } =
  let parameter_ok
      ~position
      ~offset
      { Node.location; value } =
    (* Get the argument's name. *)
    List.nth (arguments call) position
    >>= fun argument ->
    Define.infer_argument_name
      (Define.create callee)
      ~index:(position + offset)
      ~argument
    >>= fun name ->
    (* Get the type of the argument. *)
    let parameter_map = Define.parameter_annotations (Define.create callee) ~resolution in
    begin
      match Map.find parameter_map name with
      | Some annotation when not (Type.is_meta annotation) -> Some annotation
      | _ -> None
    end
    >>= fun expected ->

    (* Compare to the actual type. *)
    begin
      match value with
      | Signature.Normal { Signature.annotation = actual; _ }
      | Signature.Starred (Type.Parametric { Type.parameters = [actual]; _ }) ->
          Some actual
      | _ ->
          None
    end
    >>= fun actual ->
    check_parameter ~argument ~position ~offset ~location ~name ~actual ~expected
  in
  let accumulate_errors position (offset, errors) = function
    | { Node.value = Signature.Normal _; _ } as argument ->
        begin
          match parameter_ok ~position ~offset argument with
          | Some error -> offset, add_error errors error
          | _ -> offset, errors
        end
    | { Node.value = Signature.Starred _; _ } as argument ->
        (* Angelic assumption: if we get a type error with a starred argument we move on
         * to the next argument, otherwise we keep consuming parameters. The offset tries
         * to match the next argument to one left of where it would be normally matched.
        *)
        begin
          match parameter_ok ~position ~offset argument with
          | Some _ -> offset - 1, errors
          | _ -> offset, errors
        end
  in
  argument_annotations ~resolution call
  |> List.foldi ~init:(0, init) ~f:accumulate_errors
  |> snd


let overload call ~resolution ~overloads =
  (* Assuming calls have the following format:
     `[argument, ]* [\*variable,]* [keyword=value,]* [\*\*keywords]*` *)
  ignore call; ignore resolution;
  let open Type.Callable in
  let overload ({ parameters; _ } as overload) =
    match parameters with
    | Defined parameters ->
        let arguments = arguments call in

        let compatible { Argument.value; _ } parameter =
          let actual = Resolution.resolve resolution value in
          let expected =
            match parameter with
            | Parameter.Anonymous annotation
            | Parameter.Named { Parameter.annotation; _ } ->
                annotation
            | _ ->
                Type.Top
          in
          Resolution.less_or_equal resolution ~left:actual ~right:expected
        in

        let rec consume_anonymous (arguments, parameters) =
          match arguments, parameters with
          | ({ Argument.name = None; _ } as argument) :: arguments,
            ((Parameter.Anonymous _) as parameter) :: parameters
          | ({ Argument.name = None; _ } as argument) :: arguments,
            ((Parameter.Named _) as parameter) :: parameters
            when compatible argument parameter ->
              consume_anonymous (arguments, parameters)
          | arguments, parameters ->
              arguments, parameters
        in

        let rec consume_variable (arguments, parameters) =
          match arguments, parameters with
          | { Argument.name = None; _ } :: arguments, (Parameter.Variable _) :: _ ->
              consume_variable (arguments, parameters)
          | arguments, (Parameter.Variable _) :: parameters ->
              consume_variable (arguments, parameters)

          | { Argument.value = { Node.value = Starred (Starred.Once _); _ }; _ } :: _,
            (Parameter.Anonymous _) :: parameters
          | { Argument.value = { Node.value = Starred (Starred.Once _); _ }; _ } :: _,
            (Parameter.Named _) :: parameters ->
              consume_variable (arguments, parameters)
          | { Argument.value = { Node.value = Starred (Starred.Once _); _ }; _ } :: arguments,
            parameters ->
              consume_variable (arguments, parameters)

          | arguments, parameters ->
              arguments, parameters
        in

        let consume_named (arguments, parameters) =
          let named_arguments =
            let argument map { Argument.name; value } =
              Map.set map ~key:(Option.value_exn name) ~data:(Resolution.resolve resolution value)
            in
            List.take_while ~f:(fun { Argument.name; _ } -> Option.is_some name) arguments
            |> List.fold ~init:Identifier.Map.empty ~f:argument
          in

          let named_parameters =
            let parameter map = function
              | Parameter.Named { Parameter.name; annotation } ->
                  Map.set map ~key:(Access.show name |> Identifier.create) ~data:annotation
              | _ ->
                  map
            in
            List.take_while ~f:(function | Parameter.Named _ -> true | _ -> false) parameters
            |> List.fold ~init:Identifier.Map.empty ~f:parameter
          in

          let consumed =
            let argument ~key ~data consumed =
              match Map.find named_parameters key with
              | Some annotation ->
                  if Resolution.less_or_equal resolution ~left:data ~right:annotation then
                    Set.add consumed key
                  else
                    consumed
              | _ ->
                  consumed
            in
            Map.fold ~init:Identifier.Set.empty ~f:argument named_arguments
          in

          let arguments =
            let argument_consumed { Argument.name; _ } =
              name
              >>| Set.mem consumed
              |> Option.value ~default:false
            in
            List.drop_while ~f:argument_consumed arguments
          in
          let parameters =
            let parameter_consumed = function
              | Parameter.Named { Parameter.name; _ } ->
                  Set.mem consumed (Access.show name |> Identifier.create)
              | _ ->
                  false
            in
            List.drop_while ~f:parameter_consumed parameters
          in

          arguments, parameters
        in

        let rec consume_keywords (arguments, parameters) =
          match arguments, parameters with
          | { Argument.name = Some _; _ } :: arguments, (Parameter.Keywords _) :: _ ->
              consume_keywords (arguments, parameters)
          | arguments, (Parameter.Keywords _) :: parameters ->
              consume_keywords (arguments, parameters)

          | { Argument.value = { Node.value = Starred (Starred.Twice _); _ }; _ } :: _,
            (Parameter.Named _) :: parameters ->
              consume_keywords (arguments, parameters)
          | { Argument.value = { Node.value = Starred (Starred.Twice _); _ }; _ } :: arguments,
            parameters ->
              consume_keywords (arguments, parameters)

          | arguments, parameters ->
              arguments, parameters
        in

        let arguments, parameters =
          (arguments, parameters)
          |> consume_anonymous
          |> consume_variable
          |> consume_named
          |> consume_keywords
        in

        if List.is_empty arguments && List.is_empty parameters then
          Some overload
        else
          None
    | Undefined ->
        Some overload
  in
  List.find_map ~f:overload overloads
