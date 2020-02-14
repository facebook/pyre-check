(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Ast
open Statement

type annotation_parser = {
  parse_annotation: Expression.expression Node.t -> Type.t;
  parse_as_concatenation:
    Expression.t ->
    (Type.t Type.OrderedTypes.Concatenation.Middle.t, Type.t) Type.OrderedTypes.Concatenation.t
    option;
  parse_as_parameter_specification_instance_annotation:
    variable_parameter_annotation:Expression.t ->
    keywords_parameter_annotation:Expression.t ->
    Type.Variable.Variadic.Parameters.t option;
}

let return_annotation_without_applying_decorators
    ~signature:({ Define.Signature.return_annotation; async; generator; _ } as signature)
    ~parser:{ parse_annotation; _ }
  =
  let annotation = Option.value_map return_annotation ~f:parse_annotation ~default:Type.Any in
  if async && not generator then
    Type.coroutine [Single Type.Any; Single Type.Any; Single annotation]
  else if Define.Signature.is_coroutine signature then
    match annotation with
    | Type.Parametric { name = "typing.Generator"; parameters = [_; _; Single return_annotation] }
      ->
        Type.awaitable return_annotation
    | _ -> Type.Top
  else
    annotation


let create_overload_without_applying_decorators
    ~parser:
      ( {
          parse_annotation;
          parse_as_concatenation;
          parse_as_parameter_specification_instance_annotation;
          _;
        } as parser )
    ({ Define.Signature.parameters; _ } as signature)
  =
  let open Type.Callable in
  let parameters =
    let parameter { Node.value = { Expression.Parameter.name; annotation; value }; _ } =
      let default = Option.is_some value in
      { Parameter.name; annotation; default }
    in
    let parse_as_annotation annotation =
      annotation >>| parse_annotation |> Option.value ~default:Type.Top
    in
    let parse_parameters parameters =
      let parse = function
        | Type.Callable.Parameter.PositionalOnly ({ annotation; _ } as anonymous) ->
            Type.Callable.Parameter.PositionalOnly
              { anonymous with annotation = parse_as_annotation annotation }
        | Named ({ annotation; _ } as named) ->
            Named { named with annotation = parse_as_annotation annotation }
        | KeywordOnly ({ annotation; _ } as named) ->
            KeywordOnly { named with annotation = parse_as_annotation annotation }
        | Variable (Concatenation _) -> failwith "impossible"
        | Variable (Concrete annotation) -> (
            let parsed_as_concatentation () = annotation >>= parse_as_concatenation in
            match parsed_as_concatentation () with
            | Some concatenation -> Parameter.Variable (Concatenation concatenation)
            | None -> Parameter.Variable (Concrete (parse_as_annotation annotation)) )
        | Keywords annotation -> Keywords (parse_as_annotation annotation)
      in
      match List.rev parameters with
      | Type.Callable.Parameter.Keywords (Some keywords_parameter_annotation)
        :: Type.Callable.Parameter.Variable (Concrete (Some variable_parameter_annotation))
           :: reversed_head -> (
          let default () = Defined (List.map parameters ~f:parse) in
          match
            parse_as_parameter_specification_instance_annotation
              ~variable_parameter_annotation
              ~keywords_parameter_annotation
          with
          | Some variable -> (
              let parsed_head =
                let extract_positional_only = function
                  | Type.Callable.Parameter.PositionalOnly { annotation; _ } -> Some annotation
                  | _ -> None
                in
                List.rev reversed_head
                |> List.map ~f:parse
                |> List.map ~f:extract_positional_only
                |> Option.all
              in
              match parsed_head with
              | Some head -> ParameterVariadicTypeVariable { head; variable }
              | None -> default () )
          | None -> default () )
      | _ -> Defined (List.map parameters ~f:parse)
    in
    List.map parameters ~f:parameter |> Parameter.create |> parse_parameters
  in
  { annotation = return_annotation_without_applying_decorators ~signature ~parser; parameters }
