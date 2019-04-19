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


let return_annotation
    ~define:({ Define.signature = { Define.return_annotation; async; _ }; _ } as define)
    ~resolution =
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


let apply_decorators
    ~define:({ Define.signature = { Define.decorators; _ }; _ } as define)
    ~resolution =
  let apply_decorator define { Node.value = decorator; _ } =
    let name decorator =
      (* A decorator is either a call or a list of identifiers. *)
      match Expression.Access.name_and_arguments ~call:decorator with
      | Some { Expression.Access.callee; _ } ->
          callee
      | None ->
          Expression.Access.show decorator
    in
    let resolve_decorators = function
      | "contextlib.contextmanager" ->
          let return_annotation = return_annotation ~define ~resolution in
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
            let signature =
              {
                define.signature with
                return_annotation =
                  Type.parametric
                    "contextlib.GeneratorContextManager"
                    [Type.single_parameter joined]
                  |> Type.expression
                  |> Option.some
              }
            in
            { define with signature }
          else
            define
      | "click.command"
      | "click.group"
      | "click.pass_context"
      | "click.pass_obj" ->
          (* Suppress caller/callee parameter matching by altering the click entry
             point to have a generic parameter list. *)
          let signature =
            {
              define.signature with
              parameters = [
                Parameter.create ~name:"*args" ();
                Parameter.create ~name:"**kwargs" ();
              ]
            }
          in
          { define with signature }
      | name when Set.mem Recognized.asyncio_contextmanager_decorators name ->
          let return_annotation = return_annotation ~define ~resolution in
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
            let signature =
              {
                define.signature with
                return_annotation =
                  Type.parametric "typing.AsyncContextManager" [Type.single_parameter joined]
                  |> Type.expression
                  |> Option.some
              }
            in
            { define with signature }
          else
            define
      | name ->
          Decorators.apply ~define ~resolution ~name
    in
    match decorator with
    | Expression.Access (Expression.Access.SimpleAccess call) ->
        resolve_decorators (name call)
    | Expression.Call { callee = { Node.value = Expression.Name name; _ }; _ }
    | Expression.Name name ->
        Expression.name_to_identifiers name
        >>| String.concat ~sep:"."
        >>| resolve_decorators
        |> Option.value ~default:define
    | _ ->
        define
  in
  List.fold decorators ~init:define ~f:apply_decorator


let create ~parent ~resolution defines =
  let open Type.Callable in
  let { Define.signature = { name; _ }; _ } = List.hd_exn defines in
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
    let to_signature
        (implementation, overloads)
        ({ Define.signature = { parameters; _ }; _ } as define) =
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
