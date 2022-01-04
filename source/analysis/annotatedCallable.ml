(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast
open Statement

type annotation_parser = {
  parse_annotation: Expression.expression Node.t -> Type.t;
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
      ({ parse_annotation; parse_as_parameter_specification_instance_annotation; _ } as parser)
    ~variables
    ({ Define.Signature.parameters; parent; _ } as signature)
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
        | Variable (Concrete annotation) ->
            annotation
            >>= Type.OrderedTypes.concatenation_from_unpack_expression
                  ~parse_annotation:(fun expression -> parse_as_annotation (Some expression))
            >>| (fun concatenation ->
                  Type.Callable.Parameter.Variable (Concatenation concatenation))
            |> Option.value
                 ~default:
                   (Type.Callable.Parameter.Variable (Concrete (parse_as_annotation annotation)))
        | Variable (Concatenation _) ->
            (* We are guaranteed that `Type.Callable.Parameter.create expression` will not convert
               `*args: <anything>` to `Variable (Concatenation ...)`. *)
            failwith "impossible"
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
                  | Type.Callable.Parameter.PositionalOnly { annotation; _ }
                  | Named { annotation; _ } ->
                      Some annotation
                  | _ -> None
                in
                List.rev reversed_head
                |> List.map ~f:parse
                |> List.map ~f:extract_positional_only
                |> Option.all
              in
              match parsed_head with
              | Some head -> ParameterVariadicTypeVariable { head; variable }
              | None -> default ())
          | None -> default ())
      | _ -> Defined (List.map parameters ~f:parse)
    in
    List.map parameters ~f:parameter |> Parameter.create |> parse_parameters
  in
  let parameters =
    match parameters, parent with
    | ( Type.Callable.Defined
          (Named { Type.Callable.Parameter.name; annotation = Type.Top; default } :: tail),
        Some parent ) ->
        let replacement ~meta =
          let parent_type =
            let class_annotation = Reference.show parent in
            variables class_annotation
            >>| List.map ~f:Type.Variable.to_parameter
            >>| Type.parametric class_annotation
            |> Option.value ~default:(Type.Primitive class_annotation)
          in
          let annotation = if meta then Type.meta parent_type else parent_type in
          Type.Callable.Defined
            (Named { Type.Callable.Parameter.name; annotation; default } :: tail)
        in
        if String.equal (Define.Signature.unqualified_name signature) "__new__" then
          replacement ~meta:true
        else if Define.Signature.is_static_method signature then
          parameters
        else if
          Define.Signature.is_class_method signature || Define.Signature.is_class_property signature
        then
          replacement ~meta:true
        else
          replacement ~meta:false
    | _ -> parameters
  in
  { annotation = return_annotation_without_applying_decorators ~signature ~parser; parameters }
