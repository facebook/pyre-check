(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Ast
open Statement

let is_generator { Define.body; _ } =
  let module YieldVisit = Visit.Make (struct
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
    ~resolution
  =
  let annotation =
    Option.value_map
      return_annotation
      ~f:(Resolution.parse_annotation resolution)
      ~default:Type.Top
  in
  if async && not (is_generator define) then
    Type.coroutine [Type.Any; Type.Any; annotation]
  else if Define.is_coroutine define then
    match annotation with
    | Type.Parametric { name = "typing.Generator"; parameters = [_; _; return_annotation] } ->
        Type.awaitable return_annotation
    | _ -> Type.Top
  else
    annotation


let create_overload ~resolution
                    ~define:({ Define.signature = { parameters; _ }; _ } as define) =
  let open Type.Callable in
  let parameter { Node.value = { Ast.Parameter.name; annotation; value }; _ } =
    let annotation =
      annotation >>| Resolution.parse_annotation resolution |> Option.value ~default:Type.Top
    in
    let default = Option.is_some value in
    name, annotation, default
  in
  { annotation = return_annotation ~define ~resolution;
    parameters = Defined (List.map parameters ~f:parameter |> Parameter.create)
  }


let create ~parent ~name overloads =
  let open Type.Callable in
  let implementation, overloads =
    let to_signature (implementation, overloads) (is_overload, signature) =
      if is_overload then
        implementation, signature :: overloads
      else
        signature, overloads
    in
    List.fold
      ~init:({ annotation = Type.Top; parameters = Type.Callable.Undefined }, [])
      ~f:to_signature
      overloads
  in
  let callable =
    { kind = Named (Reference.create name); implementation; overloads; implicit = None }
  in
  match parent with
  | Some parent ->
      let { Type.Callable.kind; implementation; overloads; implicit } =
        match implementation with
        | { parameters = Defined (Named { name; annotation; _ } :: _); _ } -> (
            let callable =
              let implicit = { implicit_annotation = parent; name } in
              { callable with implicit = Some implicit }
            in
            let constraints = TypeOrder.diff_variables Type.Map.empty annotation parent in
            let instantiated =
              Type.instantiate (Type.Callable callable) ~constraints:(Map.find constraints)
            in
            match instantiated with
            | Type.Callable callable -> callable
            | _ -> callable )
        | _ -> callable
      in
      let drop_self { Type.Callable.annotation; parameters } =
        let parameters =
          match parameters with
          | Type.Callable.Defined (_ :: parameters) -> Type.Callable.Defined parameters
          | _ -> parameters
        in
        { Type.Callable.annotation; parameters }
      in
      { Type.Callable.kind;
        implementation = drop_self implementation;
        overloads = List.map overloads ~f:drop_self;
        implicit
      }
  | None -> callable


let apply_decorators
    ~define:({ Define.signature = { Define.decorators; _ }; _ } as define)
    ~resolution
  =
  let apply_decorator
      ({ Type.Callable.annotation; parameters } as overload)
      { Node.value = decorator; _ }
    =
    let name decorator =
      (* A decorator is either a call or a list of identifiers. *)
      match Expression.Access.name_and_arguments ~call:decorator with
      | Some { Expression.Access.callee; _ } -> callee
      | None -> Expression.Access.show decorator
    in
    let resolve_decorators name =
      match name with
      | "contextlib.contextmanager" ->
          let joined =
            try Resolution.join resolution annotation (Type.iterator Type.Bottom) with
            | TypeOrder.Untracked _ ->
                (* Apply_decorators gets called when building the environment, which is unsound and
                   can raise. *)
                Type.Any
          in
          if Type.is_iterator joined then
            { overload with
              Type.Callable.annotation =
                Type.parametric "contextlib.GeneratorContextManager" [Type.single_parameter joined]
            }
          else
            overload
      | "click.command"
      | "click.group"
      | "click.pass_context"
      | "click.pass_obj" ->
          (* Suppress caller/callee parameter matching by altering the click entry point to have a
             generic parameter list. *)
          let parameters =
            Type.Callable.Defined
              [ Type.Callable.Parameter.Variable
                  { name = "args"; annotation = Type.Any; default = false };
                Type.Callable.Parameter.Keywords
                  { name = "kwargs"; annotation = Type.Any; default = false } ]
          in
          { overload with Type.Callable.parameters }
      | name when Set.mem Recognized.asyncio_contextmanager_decorators name ->
          let joined =
            try Resolution.join resolution annotation (Type.async_iterator Type.Bottom) with
            | TypeOrder.Untracked _ ->
                (* Apply_decorators gets called when building the environment, which is unsound and
                   can raise. *)
                Type.Any
          in
          if Type.is_async_iterator joined then
            { overload with
              Type.Callable.annotation =
                Type.parametric "typing.AsyncContextManager" [Type.single_parameter joined]
            }
          else
            overload
      | name when Set.mem Decorators.special_decorators name ->
          Decorators.apply ~overload ~resolution ~name
      | name -> (
        match Resolution.undecorated_signature resolution (Reference.create name) with
        | Some
            { Type.Callable.annotation = return_annotation;
              parameters =
                Type.Callable.Defined
                  [ Type.Callable.Parameter.Named
                      { Type.Callable.Parameter.annotation = parameter_annotation; _ } ]
            } -> (
            let decorated_annotation =
              Resolution.solve_less_or_equal
                resolution
                ~constraints:TypeConstraints.empty
                ~left:(Type.Callable.create ~parameters ~annotation ())
                ~right:parameter_annotation
              |> List.filter_map ~f:(Resolution.solve_constraints resolution)
              |> List.hd
              >>| (fun solution -> TypeConstraints.Solution.instantiate solution return_annotation)
              (* If we failed, just default to the old annotation. *)
              |> Option.value ~default:annotation
            in
            match decorated_annotation with
            (* Note that @property decorators can't properly be handled in this fashion. The
               problem stems from the need to use `apply_decorators` to individual overloaded
               defines - if an overloaded define could become Not An Overload, it's not clear what
               we should do. Defer the problem by now by only inferring a limited set of
               decorators. *)
            | Type.Callable
                { Type.Callable.implementation =
                    { Type.Callable.parameters = decorated_parameters;
                      annotation = decorated_annotation
                    }
                ; _
                } -> (
              (* Typeshed currently exhibits the common behavior of decorating with `Callable[...,
                 T] -> Modified[T]` when the parameters are meant to be left alone. Support this by
                 hard coding :( *)
              match decorated_parameters with
              | Undefined -> { overload with Type.Callable.annotation = decorated_annotation }
              | _ ->
                  { Type.Callable.annotation = decorated_annotation;
                    parameters = decorated_parameters
                  } )
            | _ -> overload )
        | _ -> overload )
    in
    match decorator with
    | Expression.Access (Expression.Access.SimpleAccess call) -> resolve_decorators (name call)
    | Expression.Call { callee = { Node.value = Expression.Name name; _ }; _ }
    | Expression.Name name ->
        Expression.name_to_identifiers name
        >>| String.concat ~sep:"."
        >>| resolve_decorators
        |> Option.value ~default:overload
    | _ -> overload
  in
  decorators
  |> List.rev
  |> List.fold ~init:(create_overload ~define ~resolution) ~f:apply_decorator
