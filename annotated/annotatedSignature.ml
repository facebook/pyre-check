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
            ~argument:{ Argument.name; value = { Node.location; _ } as value }
            ~parameter
            ~remaining_arguments =
          let actual = Resolution.resolve resolution value in
          let expected =
            match parameter with
            | Parameter.Anonymous annotation
            | Parameter.Named { Parameter.annotation; _ } ->
                annotation
            | _ ->
                Type.Top
          in
          let mismatch =
            let position = List.length arguments - remaining_arguments in
            { actual; expected; name; position }
            |> Node.create ~location
            |> fun mismatch -> Some (Mismatch mismatch)
          in

          let parameters_to_infer = Type.variables expected |> List.length in
          if parameters_to_infer > 0 then
            match expected with
            | Type.Variable _ as variable ->
                let resolved =
                  Map.find constraints variable
                  >>| (fun resolved -> Resolution.join resolution actual resolved)
                  |> Option.value ~default:actual
                in
                Map.set constraints ~key:variable ~data:resolved, reason
            | Type.Parametric _ ->
                let primitive, parameters = Type.split expected in
                (Resolution.class_definition resolution primitive
                 >>| Class.create
                 >>= fun target ->
                 let primitive, _ = Type.split actual in
                 Resolution.class_definition resolution primitive
                 >>| Class.create
                 >>| Class.constraints ~target ~instantiated:actual ~resolution
                 >>| fun inferred ->
                 let inferred =
                   (* Translate type variables, e.g. a class might have a generic variable
                      `_T` that is referred to with a differnet variable `_S` in the
                      callable instantiation. *)
                   let generics = Class.generics target ~resolution in
                   if List.length generics = List.length parameters then
                     let translation =
                       let translation map generic parameter =
                         match generic, parameter with
                         | Type.Variable _, Type.Variable _ ->
                             Map.set map ~key:generic ~data:parameter
                         | _ ->
                             map
                       in
                       List.fold2_exn ~init:Type.Map.empty ~f:translation generics parameters
                     in
                     let translate ~key ~data inferred =
                       let key = Map.find translation key |> Option.value ~default:key in
                       Map.set inferred ~key ~data
                     in
                     Map.fold ~init:Type.Map.empty ~f:translate inferred
                   else
                     Type.Map.empty
                 in
                 if Map.length inferred < parameters_to_infer then
                   constraints, mismatch
                 else
                   let merge ~key:_ = function
                     | `Both (left, right) -> Some (Resolution.join resolution left right)
                     | `Left left -> Some left
                     | `Right right -> Some right
                   in
                   Map.merge ~f:merge constraints inferred, reason)
                |> Option.value ~default:(constraints, mismatch)
            | _ ->
                constraints, reason
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
          | _ ->
              state
        in

        let rec consume_variable ({ arguments; parameters; _ } as state) =
          let reason = None in
          match arguments, parameters with
          | { Argument.name = None; _ } :: arguments, (Parameter.Variable _) :: _ ->
              consume_variable { state with arguments }
          | _, (Parameter.Variable _) :: parameters ->
              consume_variable { state with parameters; reason }

          | { Argument.value = { Node.value = Starred (Starred.Once _); _ }; _ } :: _,
            (Parameter.Anonymous _) :: parameters
          | { Argument.value = { Node.value = Starred (Starred.Once _); _ }; _ } :: _,
            (Parameter.Named _) :: parameters ->
              consume_variable { state with parameters; reason }

          | { Argument.value = { Node.value = Starred (Starred.Once _); _ }; _ } :: arguments,
            parameters ->
              consume_variable { state with arguments; parameters; reason }

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

        let rec consume_keywords ({ arguments; parameters; _ } as state) =
          let reason = None in
          match arguments, parameters with
          | { Argument.name = Some _; _ } :: arguments, (Parameter.Keywords _) :: _ ->
              consume_keywords { state with arguments; reason }
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
          |> consume_variable
          |> consume_named
          |> consume_keywords
        in

        let rank = (List.length parameters) + (List.length arguments) in

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
