(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Statement
open Pyre

module Resolution = AnalysisResolution
module Type = AnalysisType

module Call = AnnotatedCall
module Class = AnnotatedClass


type mismatch = {
  actual: Type.t;
  expected: Type.t;
  name: Identifier.t option;
  position: int;
}
[@@deriving eq, show]


type reason =
  | Mismatch of mismatch Node.t
[@@deriving eq, show]


type closest = {
  rank: int;
  callable: Type.Callable.t;
  reason: reason option;
}
[@@deriving eq, show]


let equal_closest left right =
  (* Ignore rank. *)
  Type.Callable.equal left.callable right.callable &&
  Option.equal equal_reason left.reason right.reason


type t =
  | Found of Type.Callable.t
  | NotFound of closest
[@@deriving eq, show]


type resolution_state = {
  arguments: (Expression.t Argument.t) list;
  parameters: (Type.t Type.Callable.Parameter.t) list;
  constraints: Type.t Type.Map.t;
  reason: reason option;
}


let select call ~resolution ~callable:({ Type.Callable.overloads; _ } as callable) =
  (* Assuming calls have the following format:
     `[argument,]* [\*variable,]* [keyword=value,]* [\*\*keywords]*` *)
  let open Type.Callable in
  let ranked ({ Type.Callable.parameters; _ } as overload) =
    let callable = { callable with Type.Callable.overloads = [overload] } in
    match parameters with
    | Defined parameters ->
        let arguments = Call.arguments call in

        let check_parameter
            ~constraints
            ~reason
            ~argument:{ Argument.name; value = { Node.location; _ } as expression }
            ~parameter
            ~remaining_arguments =
          let sequence_parameter annotation =
            let sequence = Type.parametric "typing.Sequence" [Type.Object] in
            if Resolution.less_or_equal resolution ~left:annotation ~right:sequence then
              (* Try to extract first parameter. *)
              Type.parameters annotation
              |> List.hd
              |> Option.value ~default:Type.Top
            else
              annotation
          in
          let mapping_value_parameter annotation =
            let mapping = Type.parametric "typing.Mapping" [Type.string; Type.Object] in
            if Resolution.less_or_equal resolution ~left:annotation ~right:mapping then
              (* Try to extract second parameter. *)
              Type.parameters annotation
              |> (fun parameters -> List.nth parameters 1)
              |> Option.value ~default:Type.Top
            else
              annotation
          in

          let expected =
            match parameter with
            | Parameter.Anonymous annotation
            | Parameter.Named { Parameter.annotation; _ } ->
                annotation
            | Parameter.Variable { Parameter.annotation; _ } ->
                sequence_parameter annotation
            | Parameter.Keywords { Parameter.annotation; _ } ->
                mapping_value_parameter annotation
          in
          let actual =
            match Node.value expression with
            | Starred (Starred.Once expression) ->
                Resolution.resolve resolution expression
                |> sequence_parameter
            | Starred (Starred.Twice expression) ->
                Resolution.resolve resolution expression
                |> mapping_value_parameter
            | _ ->
                let actual = Resolution.resolve resolution expression in
                if Type.is_meta expected && Type.equal actual Type.Top then
                  Resolution.parse_annotation resolution expression
                  |> Type.meta
                else
                  actual
          in
          let mismatch =
            let position = List.length arguments - remaining_arguments in
            { actual; expected; name; position }
            |> Node.create ~location
            |> fun mismatch -> Some (Mismatch mismatch)
          in

          let parameters_to_infer = Type.variables expected |> List.length in
          if parameters_to_infer > 0 then
            let updated_constraints =
              let update_constraints ~constraints ~variable ~resolved =
                let resolved =
                  Map.find constraints variable
                  >>| (fun existing -> Resolution.join resolution existing resolved)
                  |> Option.value ~default:resolved
                in
                let in_constraints =
                  match variable with
                  | Type.Variable { Type.constraints; _ } when not (List.is_empty constraints) ->
                      let in_constraint bound =
                        Resolution.less_or_equal resolution ~left:resolved ~right:bound
                      in
                      List.exists ~f:in_constraint constraints
                  | _ ->
                      true
                in
                if in_constraints then
                  Some (Map.set ~key:variable ~data:resolved constraints)
                else
                  None
              in
              match expected with
              | Type.Variable _ as variable ->
                  update_constraints ~constraints ~variable ~resolved:actual
              | Type.Parametric _ ->
                  let primitive, parameters = Type.split expected in
                  Resolution.class_definition resolution primitive
                  >>| Class.create
                  >>= fun target ->
                  let primitive, _ = Type.split actual in
                  Resolution.class_definition resolution primitive
                  >>| Class.create
                  >>| Class.constraints ~target ~parameters ~instantiated:actual ~resolution
                  >>= fun inferred ->
                  if Map.length inferred < parameters_to_infer then
                    None
                  else
                    let update_constraints ~key ~data constraints =
                      constraints
                      >>= fun constraints ->
                      update_constraints ~constraints ~variable:key ~resolved:data
                    in
                    Map.fold ~init:(Some constraints) ~f:update_constraints inferred
              | _ ->
                  Some constraints
            in
            updated_constraints
            >>| (fun constraints -> constraints, reason)
            |> Option.value ~default:(constraints, mismatch)
          else if Resolution.less_or_equal resolution ~left:actual ~right:expected then
            constraints, reason
          else
            constraints, mismatch
        in

        let rec consume_anonymous ({ arguments; parameters; constraints; reason } as state) =
          let starred_twice { Argument.value = { Node.value; _ }; _ } =
            match value with
            | Starred (Starred.Twice _) -> true
            | _ -> false
          in
          match arguments, parameters with
          | ({ Argument.name = None; _ } as argument) :: arguments,
            ((Parameter.Anonymous _) as parameter) :: parameters
          | ({ Argument.name = None; _ } as argument) :: arguments,
            ((Parameter.Named _) as parameter) :: parameters
            when not (starred_twice argument)->
              begin
                let constraints, reason =
                  check_parameter
                    ~constraints
                    ~reason
                    ~argument
                    ~parameter
                    ~remaining_arguments:(List.length arguments)
                in
                match reason with
                | None -> consume_anonymous { state with arguments; parameters; constraints }
                | _ -> { state with reason }
              end

          (* Eagerly consume arguments when types of variable parameters match. *)
          | ({
              Argument.value = { Node.value = Starred (Starred.Once _); _ };
              _;
            } as argument) :: arguments,
            ((Parameter.Variable _) as parameter) :: _
          | ({ Argument.name = None; _ } as argument) :: arguments,
            ((Parameter.Variable _) as parameter) :: _ ->
              begin
                let constraints, reason =
                  check_parameter
                    ~constraints
                    ~reason
                    ~argument
                    ~parameter
                    ~remaining_arguments:(List.length arguments)
                in
                match reason with
                | None -> consume_anonymous { state with arguments; parameters; constraints }
                | _ -> { state with reason }
              end

          (* Cleanup variable arguments. *)
          | _, (Parameter.Variable _) :: parameters ->
              consume_anonymous { state with parameters; reason }
          | { Argument.value = { Node.value = Starred (Starred.Once _); _ }; _ } :: arguments,
            parameters ->
              consume_anonymous { state with arguments; parameters; reason }

          | _ ->
              state
        in

        let rec consume_named ({ arguments; parameters; constraints; reason } as state) =
          match arguments, parameters with
          | ({ Argument.name = Some argument_name; _ } as argument) :: arguments, _ ->
              begin
                let parameter, parameters =
                  let matching_parameter = function
                    | Parameter.Named { Parameter.name = [Access.Identifier parameter_name]; _ }
                      when Identifier.equal argument_name parameter_name -> true
                    | _ -> false
                  in
                  List.find ~f:matching_parameter parameters,
                  List.filter ~f:(fun parameter -> not (matching_parameter parameter)) parameters
                in
                match parameter with
                | Some parameter ->
                    begin
                      let constraints, reason =
                        check_parameter
                          ~constraints
                          ~reason
                          ~argument
                          ~parameter
                          ~remaining_arguments:(List.length arguments)
                      in
                      match reason with
                      | None -> consume_named { state with arguments; parameters; constraints }
                      | _ -> { state with reason }
                    end
                | None ->
                    (* Extraneous arguments. Not yet handled. *)
                    state
              end

          | ({
              Argument.value = { Node.value = Starred (Starred.Twice _); _ };
              _;
            } as argument) :: _,
            ((Parameter.Named _) as parameter) :: parameters ->
              begin
                let constraints, reason =
                  check_parameter
                    ~constraints
                    ~reason
                    ~argument
                    ~parameter
                    ~remaining_arguments:(List.length arguments)
                in
                match reason with
                | None -> consume_named { state with arguments; parameters; constraints }
                | _ -> { state with reason }
              end
          | { Argument.value = { Node.value = Starred (Starred.Twice _); _ }; _ } :: arguments,
            parameters ->
              consume_named { state with arguments; parameters; reason }

          | _ ->
              state
        in

        let consume_named_with_defaults ({ arguments = _; parameters; _ } as state) =
          let parameters =
            let has_default = function
              | Parameter.Named { Parameter.default; _ } -> default
              | _ -> false
            in
            List.filter ~f:(fun parameter -> not (has_default parameter)) parameters
          in
          { state with parameters }
        in


        let rec consume_keywords ({ arguments; parameters; constraints; reason } as state) =
          match arguments, parameters with
          | ({ Argument.name = Some _; _ } as argument) :: arguments,
            ((Parameter.Keywords _) as parameter) :: _ ->
              begin
                let constraints, reason =
                  check_parameter
                    ~constraints
                    ~reason
                    ~argument
                    ~parameter
                    ~remaining_arguments:(List.length arguments)
                in
                match reason with
                | None -> consume_keywords { state with arguments; parameters; constraints }
                | _ -> { state with reason }
              end
          | _, (Parameter.Keywords _) :: parameters ->
              consume_keywords { state with parameters; reason }

          | _ ->
              state
        in

        let { arguments; parameters; constraints; reason } =
          {
            arguments;
            parameters;
            constraints = Type.Map.empty;
            reason = None;
          }
          |> consume_anonymous
          |> consume_named
          |> consume_named_with_defaults
          |> consume_keywords
        in

        let rank = (List.length parameters) + (List.length arguments) in

        (* Map unresolved constraints to `Bottom`. *)
        let constraints =
          let variables =
            Type.Callable {
              Type.Callable.kind = Anonymous;
              overloads = [overload];
              implicit_argument = false;
            }
            |> Type.variables
          in
          let remaining_to_bottom constraints variable =
            let update = function
              | None -> Some Type.Bottom
              | value -> value
            in
            Map.change constraints variable ~f:update
          in
          List.fold ~f:remaining_to_bottom ~init:constraints variables
        in

        if List.is_empty arguments && List.is_empty parameters then
          begin
            match reason with
            | None ->
                Type.Callable {
                  Type.Callable.kind = Anonymous;
                  overloads = [overload];
                  implicit_argument = false;
                }
                |> Type.instantiate ~widen:false ~constraints:(Map.find constraints)
                |> (function
                    | Type.Callable { overloads = [instantiated]; _  } ->
                        Found { callable with Type.Callable.overloads = [instantiated] }
                    | _ ->
                        failwith "Instantiate did not return a callable")
            | _ ->
                NotFound { rank = 1; callable; reason }
          end
        else
          NotFound { rank; callable; reason }
    | Undefined ->
        Found callable
  in

  let rec find ~overloads ~closest =
    match overloads with
    | overload :: overloads ->
        begin
          match ranked overload with
          | Found callable ->
              Found callable
          | NotFound candidate ->
              let closest = if candidate.rank < closest.rank then candidate else closest in
              find ~overloads ~closest
        end
    | [] ->
        NotFound closest
  in
  let closest =
    assert (List.length overloads > 0);
    {
      rank = Int.max_value;
      callable = { callable with Type.Callable.overloads = [List.hd_exn overloads] };
      reason = None;
    }
  in
  find ~overloads ~closest
