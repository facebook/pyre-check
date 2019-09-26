(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre
open Statement

let apply_decorators
    ~resolution
    ( { Node.value = { Define.signature = { Define.Signature.decorators; _ }; _ }; _ } as
    define_node )
  =
  let apply_decorator
      ({ Type.Callable.annotation; parameters; _ } as overload)
      { Node.value = decorator; _ }
    =
    let resolve_decorators name =
      match name with
      | "click.command"
      | "click.group"
      | "click.pass_context"
      | "click.pass_obj" ->
          (* Suppress caller/callee parameter matching by altering the click entry point to have a
             generic parameter list. *)
          let parameters =
            Type.Callable.Defined
              [
                Type.Callable.Parameter.Variable (Concrete Type.Any);
                Type.Callable.Parameter.Keywords Type.Any;
              ]
          in
          { overload with Type.Callable.parameters }
      | name when Set.mem Recognized.asyncio_contextmanager_decorators name ->
          let joined =
            try GlobalResolution.join resolution annotation (Type.async_iterator Type.Bottom) with
            | ClassHierarchy.Untracked _ ->
                (* Apply_decorators gets called when building the environment, which is unsound and
                   can raise. *)
                Type.Any
          in
          if Type.is_async_iterator joined then
            {
              overload with
              Type.Callable.annotation =
                Type.parametric
                  "typing.AsyncContextManager"
                  (Concrete [Type.single_parameter joined]);
            }
          else
            overload
      | name when Set.mem Decorators.special_decorators name ->
          Decorators.apply ~overload ~resolution ~name
      | name -> (
        match GlobalResolution.undecorated_signature resolution (Reference.create name) with
        | Some
            {
              Type.Callable.annotation = return_annotation;
              parameters =
                Type.Callable.Defined
                  [Type.Callable.Parameter.Named { annotation = parameter_annotation; _ }];
              _;
            } -> (
            let decorated_annotation =
              GlobalResolution.solve_less_or_equal
                resolution
                ~constraints:TypeConstraints.empty
                ~left:(Type.Callable.create ~parameters ~annotation ())
                ~right:parameter_annotation
              |> List.filter_map ~f:(GlobalResolution.solve_constraints resolution)
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
                {
                  Type.Callable.implementation =
                    {
                      Type.Callable.parameters = decorated_parameters;
                      annotation = decorated_annotation;
                      define_location;
                    };
                  _;
                } -> (
              (* Typeshed currently exhibits the common behavior of decorating with `Callable[...,
                 T] -> Modified[T]` when the parameters are meant to be left alone. Support this by
                 hard coding :( *)
              match decorated_parameters with
              | Undefined -> { overload with Type.Callable.annotation = decorated_annotation }
              | _ ->
                  {
                    Type.Callable.annotation = decorated_annotation;
                    parameters = decorated_parameters;
                    define_location;
                  } )
            | _ -> overload )
        | _ -> overload )
    in
    match decorator with
    | Expression.Call { callee = { Node.value = Expression.Name name; _ }; _ }
    | Expression.Name name ->
        Expression.name_to_identifiers name
        >>| String.concat ~sep:"."
        >>| resolve_decorators
        |> Option.value ~default:overload
    | _ -> overload
  in
  let parser = GlobalResolution.annotation_parser resolution in
  decorators
  |> List.rev
  |> List.fold ~init:(AnnotatedCallable.create_overload define_node ~parser) ~f:apply_decorator


let create_callable ~resolution ~parent ~name overloads =
  let open Type.Callable in
  let implementation, overloads =
    let to_signature (implementation, overloads) (is_overload, signature) =
      if is_overload then
        implementation, signature :: overloads
      else
        signature, overloads
    in
    List.fold
      ~init:
        ( { annotation = Type.Top; parameters = Type.Callable.Undefined; define_location = None },
          [] )
      ~f:to_signature
      overloads
  in
  let callable =
    { kind = Named (Reference.create name); implementation; overloads; implicit = None }
  in
  match parent with
  | Some parent ->
      let { Type.Callable.kind; implementation; overloads; implicit } =
        match implementation, overloads with
        | { parameters = Defined (Named { name; annotation; _ } :: _); _ }, _ -> (
            let callable =
              let implicit = { implicit_annotation = parent; name } in
              { callable with implicit = Some implicit }
            in
            let solution =
              try
                GlobalResolution.solve_less_or_equal
                  resolution
                  ~any_is_bottom:true
                  ~left:parent
                  ~right:annotation
                  ~constraints:TypeConstraints.empty
                |> List.filter_map
                     ~f:(GlobalResolution.solve_constraints ~any_is_bottom:true resolution)
                |> List.hd
                |> Option.value ~default:TypeConstraints.Solution.empty
              with
              | ClassHierarchy.Untracked _ -> TypeConstraints.Solution.empty
            in
            let instantiated =
              TypeConstraints.Solution.instantiate solution (Type.Callable callable)
            in
            match instantiated with
            | Type.Callable callable -> callable
            | _ -> callable )
        (* We also need to set the implicit up correctly for overload-only methods. *)
        | _, { parameters = Defined (Named { name; _ } :: _); _ } :: _ ->
            let implicit = { implicit_annotation = parent; name } in
            { callable with implicit = Some implicit }
        | _ -> callable
      in
      let drop_self { Type.Callable.annotation; parameters; define_location } =
        let parameters =
          match parameters with
          | Type.Callable.Defined (_ :: parameters) -> Type.Callable.Defined parameters
          | _ -> parameters
        in
        { Type.Callable.annotation; parameters; define_location }
      in
      {
        Type.Callable.kind;
        implementation = drop_self implementation;
        overloads = List.map overloads ~f:drop_self;
        implicit;
      }
  | None -> callable
