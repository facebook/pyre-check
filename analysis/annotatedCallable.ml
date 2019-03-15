(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre

open Ast
open Statement


let is_generator { Define.body; _ } =
  let module YieldVisit = Visit.Make(struct
      type t = bool

      let expression result expression =
        match result, expression with
        | true, _ -> true
        | false, { Node.value = Expression.Yield _; _ } -> true
        | false, _ -> false

      let statement result statement =
        match result, statement with
        | true, _ -> true
        | false, { Node.value = Statement.Yield _; _ } -> true
        | false, { Node.value = Statement.YieldFrom _; _ } -> true
        | false, _ -> false
    end)
  in
  YieldVisit.visit false (Source.create body)


let return_annotation ~define:({ Define.return_annotation; async; _ } as define) ~resolution =
  let annotation =
    Option.value_map
      return_annotation
      ~f:(Resolution.parse_annotation resolution)
      ~default:Type.Top
  in
  if async && not (is_generator define) then
    Type.coroutine [Type.Any; Type.Any; annotation]
  else if Define.is_coroutine define then
    begin
      match annotation with
      | Type.Parametric { name = "typing.Generator"; parameters = [_; _; return_annotation] } ->
          Type.awaitable return_annotation
      | _ ->
          Type.Top
    end
  else
    annotation


let apply_decorators ~define ~resolution =
  let return_annotation = return_annotation ~define ~resolution in
  if Define.has_decorator define "contextlib.contextmanager" then
    let joined =
      try
        Resolution.join resolution return_annotation (Type.iterator Type.Bottom)
      with
        TypeOrder.Untracked _ ->
          (* Apply_decorators gets called when building the environment,
             which is unsound and can raise. *)
          Type.Any
    in
    if Type.is_iterator joined then
      {
        define with
        Define.return_annotation =
          Type.parametric "contextlib.GeneratorContextManager" [Type.single_parameter joined]
          |> Type.expression
          |> Option.some
      }
    else
      define
  else if Define.has_decorator define "contextlib.asynccontextmanager" then
    let joined =
      try
        Resolution.join resolution return_annotation (Type.async_iterator Type.Bottom)
      with
        TypeOrder.Untracked _ ->
          (* Apply_decorators gets called when building the environment,
             which is unsound and can raise. *)
          Type.Any
    in
    if Type.is_async_iterator joined then
      {
        define with
        Define.return_annotation =
          Type.parametric "typing.AsyncContextManager" [Type.single_parameter joined]
          |> Type.expression
          |> Option.some
      }
    else
      define
  else if Define.has_decorator ~match_prefix:true define "click.command" ||
          Define.has_decorator ~match_prefix:true define "click.group" ||
          Define.has_decorator define "click.pass_context" ||
          Define.has_decorator define "click.pass_obj" then
    (* Suppress caller/callee parameter matching by altering the click entry
       point to have a generic parameter list. *)
    {
      define with
      parameters = [
        Parameter.create ~name:"*args" ();
        Parameter.create ~name:"**kwargs" ();
      ];
    }
  else
    Decorators.apply ~define ~resolution


let create ~parent ~resolution defines =
  let open Type.Callable in
  let { Define.name; _ } = List.hd_exn defines in
  let parameter { Node.value = { Ast.Parameter.name; annotation; value }; _ } =
    let bare_name = String.lstrip ~drop:(function | '*' -> true | _ -> false) name in
    let annotation =
      annotation
      >>| Resolution.parse_annotation resolution
      |> Option.value ~default:Type.Top
    in
    if String.is_prefix ~prefix:"**" name then
      Parameter.Keywords { Parameter.name = bare_name; annotation; default = false }
    else if String.is_prefix ~prefix:"*" name then
      Parameter.Variable { Parameter.name = bare_name; annotation; default = false }
    else
      Parameter.Named { Parameter.name = bare_name; annotation; default = Option.is_some value }
  in
  let implementation, overloads =
    let to_signature (implementation, overloads) ({ Define.parameters; _ } as define) =
      let signature =
        {
          annotation = return_annotation ~define ~resolution;
          parameters = Defined (List.map parameters ~f:parameter);
        }
      in
      if Define.is_overloaded_method define then
        implementation, signature :: overloads
      else
        signature, overloads
    in
    List.fold
      ~init:({ annotation = Type.Top; parameters = Type.Callable.Undefined }, [])
      ~f:to_signature
      defines
  in
  let callable =
    {
      kind = Named name;
      implementation;
      overloads;
      implicit = None;
    }
  in
  match parent with
  | Some parent ->
      let { Type.Callable.kind; implementation; overloads; implicit } =
        match implementation with
        | { parameters = Defined (self_parameter :: _); _ } ->
            let callable =
              let implicit = {
                implicit_annotation = parent;
                name = Type.Callable.Parameter.name self_parameter;
              }
              in
              { callable with implicit = Some implicit }
            in
            let constraints =
              TypeOrder.diff_variables
                Type.Map.empty
                (Parameter.annotation self_parameter)
                parent
            in
            let instantiated =
              Type.instantiate (Type.Callable callable) ~constraints:(Map.find constraints)
            in
            begin
              match instantiated with
              | Type.Callable callable -> callable
              | _ -> callable
            end
        | _ ->
            callable
      in
      let drop_self { Type.Callable.annotation; parameters } =
        let parameters =
          match parameters with
          | Type.Callable.Defined (_ :: parameters) ->
              Type.Callable.Defined parameters
          | _ ->
              parameters
        in
        { Type.Callable.annotation; parameters }
      in
      {
        Type.Callable.kind;
        implementation = drop_self implementation;
        overloads = List.map overloads ~f:drop_self;
        implicit;
      }
  | None ->
      callable
