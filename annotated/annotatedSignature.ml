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
              Type.Top
          in
          let expected =
            match parameter with
            | Parameter.Anonymous annotation
            | Parameter.Named { Parameter.annotation; _ } ->
                annotation
            | Parameter.Variable { Parameter.annotation; _ } ->
                sequence_parameter annotation
            | Parameter.Keywords { Parameter.annotation; _ } ->
                let mapping = Type.parametric "typing.Mapping" [Type.string; Type.Object] in
                if Resolution.less_or_equal resolution ~left:annotation ~right:mapping then
                  (* Try to extract second parameter. *)
                  Type.parameters annotation
                  |> (fun parameters -> List.nth parameters 1)
                  |> Option.value ~default:Type.Top
                else
                  Type.Top
          in
          let actual =
            match Node.value expression with
            | Starred (Starred.Once expression) ->
                Resolution.resolve resolution expression
                |> sequence_parameter
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
          match arguments, parameters with
          | ({ Argument.name = None; _ } as argument) :: arguments,
            ((Parameter.Anonymous _) as parameter) :: parameters
          | ({ Argument.name = None; _ } as argument) :: arguments,
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
                | None -> consume_anonymous { state with arguments; parameters; constraints }
                | _ -> { state with reason }
              end

          (* Eagerly consume parameters when types of starred arguments match. *)
          | ({ Argument.value = { Node.value = Starred (Starred.Once _); _ }; _ } as argument) :: _,
            ((Parameter.Anonymous _) as parameter) :: parameters
          | ({ Argument.value = { Node.value = Starred (Starred.Once _); _ }; _ } as argument) :: _,
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

        let consume_named { arguments; parameters; constraints; reason } =
          let named_arguments =
            let argument map ({ Argument.name; _ } as argument) =
              Map.set map ~key:(Option.value_exn name) ~data:argument
            in
            List.take_while ~f:(fun { Argument.name; _ } -> Option.is_some name) arguments
            |> List.fold ~init:Identifier.Map.empty ~f:argument
          in
          let named_parameters =
            let parameter map = function
              | (Parameter.Named { Parameter.name; _ }) as parameter ->
                  Map.set map ~key:(Access.show name |> Identifier.create) ~data:parameter
              | _ ->
                  map
            in
            List.take_while ~f:(function | Parameter.Named _ -> true | _ -> false) parameters
            |> List.fold ~init:Identifier.Map.empty ~f:parameter
          in

          let consumed, constraints, reason =
            let argument ~key ~data (consumed, constraints, reason) =
              match Map.find named_parameters key with
              | Some parameter ->
                  begin
                    let constraints, reason =
                      check_parameter
                        ~constraints
                        ~reason
                        ~argument:data
                        ~parameter
                        ~remaining_arguments:(List.length arguments)
                    in
                    let consumed =
                      match reason with
                      | None -> Set.add consumed key
                      | _ -> consumed
                    in
                    consumed, constraints, reason
                  end
              | _ ->
                  consumed, constraints, reason
            in
            Map.fold ~init:(Identifier.Set.empty, constraints, reason) ~f:argument named_arguments
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
            let has_default = function
              | Parameter.Named { Parameter.default; _ } ->
                  default
              | _ ->
                  false
            in
            List.drop_while ~f:parameter_consumed parameters
            |> List.filter ~f:(fun parameter -> not (has_default parameter))
          in

          { arguments; parameters; constraints; reason }
        in

        let rec consume_keywords ({ arguments; parameters; constraints; _ } as state) =
          let reason = None in
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

          | { Argument.value = { Node.value = Starred (Starred.Twice _); _ }; _ } :: _,
            (Parameter.Named _) :: parameters ->
              consume_keywords { state with parameters; reason }
          | { Argument.value = { Node.value = Starred (Starred.Twice _); _ }; _ } :: arguments,
            parameters ->
              consume_keywords { state with arguments; parameters; reason }

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
