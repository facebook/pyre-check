(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Pyre
open Ast
open Statement

type annotation_parser = {
  parse_annotation: Expression.expression Node.t -> Type.t;
  param_spec_from_vararg_annotations:
    args_annotation:Expression.t ->
    kwargs_annotation:Expression.t ->
    Type.Variable.ParamSpec.t option;
}

let return_annotation_without_applying_decorators
    ~signature:({ Define.Signature.return_annotation; async; generator; _ } as signature)
    ~parser:{ parse_annotation; _ }
  =
  let annotation = Option.value_map return_annotation ~f:parse_annotation ~default:Type.Any in
  let needs_coroutine_return_type =
    async
    && (not generator)
    (* If the function is a stub, its signature will have `generator=false` (since there is no
       `yield`, etc. in the body). However, in the case of an `AsyncIterator`, we need to keep the
       return type as `AsyncIterator` instead of wrapping it in a `Coroutine`.

       TODO(T128109170): The long-term fix here is to replace the misleading use of `async && (not
       generator)` above with a proper `is_generator` check that accounts for the return type,
       instead of just going by whether the body has a `yield`. That way, we won't need to
       special-case `AsyncIterator` here. *)
    && not (Type.is_async_iterator annotation)
  in
  if needs_coroutine_return_type then
    Type.coroutine [Single Type.Any; Single Type.Any; Single annotation]
  else if Define.Signature.is_coroutine signature then
    match annotation with
    | Type.Parametric { name = "typing.Generator"; arguments = [_; _; Single return_annotation] } ->
        Type.awaitable return_annotation
    | _ -> Type.Top
  else
    annotation


let create_overload_without_applying_decorators
    ~parser:({ parse_annotation; param_spec_from_vararg_annotations; _ } as parser)
    ~generic_parameters_as_variables
    ({ Define.Signature.parameters; legacy_parent; _ } as signature)
  =
  let open Type.Callable in
  let parameters =
    let parameter { Node.value = { Expression.Parameter.name; annotation; value }; _ } =
      let default = Option.is_some value in
      { CallableParamType.name; annotation; default }
    in
    let parse_as_annotation annotation =
      annotation >>| parse_annotation |> Option.value ~default:Type.Top
    in
    let parse_parameters parameters =
      let parse = function
        | CallableParamType.PositionalOnly ({ annotation; _ } as anonymous) ->
            CallableParamType.PositionalOnly
              { anonymous with annotation = parse_as_annotation annotation }
        | CallableParamType.Named ({ annotation; _ } as named) ->
            Named { named with annotation = parse_as_annotation annotation }
        | CallableParamType.KeywordOnly ({ annotation; _ } as named) ->
            KeywordOnly { named with annotation = parse_as_annotation annotation }
        | CallableParamType.Variable (Concrete annotation) ->
            annotation
            >>= Type.OrderedTypes.concatenation_from_unpack_expression
                  ~parse_annotation:(fun expression -> parse_as_annotation (Some expression))
            >>| (fun concatenation -> CallableParamType.Variable (Concatenation concatenation))
            |> Option.value
                 ~default:(CallableParamType.Variable (Concrete (parse_as_annotation annotation)))
        | Variable (Concatenation _) ->
            (* We are guaranteed that `Type.Callable.CallableParamType.create expression` will not
               convert `*args: <anything>` to `Variable (Concatenation ...)`. *)
            failwith "impossible"
        | Keywords annotation -> Keywords (parse_as_annotation annotation)
      in
      match List.rev parameters with
      | CallableParamType.Keywords (Some kwargs_annotation)
        :: CallableParamType.Variable (Concrete (Some args_annotation))
        :: reversed_head -> (
          let default () = Defined (List.map parameters ~f:parse) in
          match param_spec_from_vararg_annotations ~args_annotation ~kwargs_annotation with
          | Some variable -> (
              let parsed_head =
                let extract_positional_only = function
                  | CallableParamType.PositionalOnly { annotation; _ }
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
              | Some head -> FromParamSpec { head; variable }
              | None -> default ())
          | None -> default ())
      | _ -> Defined (List.map parameters ~f:parse)
    in
    List.map parameters ~f:parameter |> CallableParamType.create |> parse_parameters
  in
  let parameters =
    let replace_self_or_cls_annotation parent_class parameter_factory parameters_tail =
      let replacement ~meta =
        let parent_type =
          let class_annotation = Reference.show parent_class in
          generic_parameters_as_variables class_annotation
          >>| List.map ~f:Type.Variable.to_argument
          >>| Type.parametric class_annotation
          |> Option.value ~default:(Type.Primitive class_annotation)
        in
        let annotation = if meta then Type.class_type parent_type else parent_type in
        Type.Callable.Defined (parameter_factory annotation :: parameters_tail)
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
    in
    match parameters, legacy_parent with
    | ( Type.Callable.Defined
          (Named { CallableParamType.name; annotation = Type.Top; default } :: tail),
        Some parent ) ->
        let factory annotation = CallableParamType.Named { name; annotation; default } in
        replace_self_or_cls_annotation parent factory tail
    | ( Type.Callable.Defined (PositionalOnly { index; annotation = Type.Top; default } :: tail),
        Some parent ) ->
        let factory annotation = CallableParamType.PositionalOnly { index; annotation; default } in
        replace_self_or_cls_annotation parent factory tail
    | _ -> parameters
  in
  { annotation = return_annotation_without_applying_decorators ~signature ~parser; parameters }
