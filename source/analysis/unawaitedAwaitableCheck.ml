(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Statement
open Pyre
module Error = AnalysisError

(** This module allows us to statically catch `RuntimeWarning`s that "coroutine `foo` was never
    awaited".

    Usual Pyre type errors are for catching runtime `TypeError`s. Pyre is considered "sound" iff
    Pyre emitting no type errors implies that there are no runtime `TypeError`s.

    This module extends the same notion to coroutine `RuntimeWarning`s as well.

    In an *ideal* world, if Pyre does not emit any unawaited-awaitable errors, then there should not
    be any `RuntimeWarning`s about coroutines never being awaited. However, to be practical, we may
    relax our checks in some cases to avoid noisy false positives. For example, in classes that
    inherit from `Awaitable`, we don't emit an error when assigning an awaitable to an attribute,
    because that led to many false positives in widely-used frameworks. So, the above notion of
    soundness is an ideal we are tending towards, but by no means a hard guarantee.

    Strategy: Reaching-definitions analysis + Alias analysis.

    1. Reaching-definitions analysis: We want to find out which awaitables are not awaited by the
    time the function returns (at least along some code path).

    - If an awaitable is awaited in some expression, then the awaitable doesn't reach the return
      point of the function. We can say that the `await` "kills" the "reach" of the awaitable.

    - If the awaitable is never awaited along some code path, then it reaches the return statement
      and is thus an unawaited awaitable.

    We can think of the original awaitable as a "definition". Getting all definitions that reach a
    certain point without being "killed" in between is called reaching-definition analysis.

    In our case, a "definition" is an awaitable expression (more precisely, its location in the
    code). "Killing" an awaitable means using `await` on it. We want all definitions (awaitables)
    that reach the return point of the function without being killed (awaited).

    Reaching-definitions analysis means we need a forward analysis. For more details, see
    https://en.wikipedia.org/wiki/Reaching_definition.

    2. Alias analysis: Awaitables can be stored in variables. If we await any variable that it has
    been assigned to (or any variable to which those variables have been assigned), then we should
    mark the original awaitable as awaited. So, whenever we detect that a variable is being assigned
    some other variable, we copy over all the awaitables that are pointed to by the other variable. *)

let is_awaitable ~global_resolution annotation =
  (not (Type.equal annotation Type.Any))
  && GlobalResolution.less_or_equal
       global_resolution
       ~left:annotation
       ~right:(Type.awaitable Type.Top)


module Awaitable : sig
  type t [@@deriving show, sexp, compare]

  module Map : Map.S with type Key.t = t

  module Set : Set.S with type Elt.t = t

  val create : Location.t -> t

  val to_location : t -> Location.t
end = struct
  module T = struct
    (* We represent an awaitable by the location of the expression where it was introduced.

       For example, if we have `x = awaitable()`, then we store the location of the right-hand side
       expression. If any variable pointing to it (x or any other alias) is awaited, then we need to
       mark it as awaited. *)
    type t = Location.t [@@deriving show, sexp, compare]
  end

  include T
  module Map = Map.Make (T)
  module Set = Set.Make (T)

  let create = Fn.id

  let to_location = Fn.id
end

module type Context = sig
  val qualifier : Reference.t

  val define : Define.t Node.t

  val global_resolution : GlobalResolution.t

  val local_annotations : LocalAnnotationMap.ReadOnly.t option
end

module State (Context : Context) = struct
  type awaitable_state =
    | Unawaited of Expression.t
    | Awaited
  [@@deriving show]

  let _ = show_awaitable_state (* unused *)

  type alias_for_awaitable =
    (* This is a variable that refers to an awaitable. *)
    | NamedAlias of Reference.t
    (* This represents an expression that contains nested aliases.

       For example, if `foo` is an instance of a class that derives from `Awaitable`, then we may
       have `foo.set_x(1).set_y(2)`. Awaiting the whole expression needs to mark `foo`'s awaitable
       as awaited. *)
    | ExpressionWithNestedAliases of { expression_location: Location.t }
  [@@deriving compare, sexp, show]

  module AliasMap = Map.Make (struct
    type t = alias_for_awaitable [@@deriving sexp, compare]
  end)

  type t = {
    (* For every location where we encounter an awaitable, we maintain whether that awaitable's
       state, i.e., has it been awaited or not? *)
    unawaited: awaitable_state Awaitable.Map.t;
    (* For an alias, what awaitable locations could it point to? *)
    awaitables_for_alias: Awaitable.Set.t AliasMap.t;
    need_to_await: bool;
  }

  let show { unawaited; awaitables_for_alias; need_to_await } =
    let unawaited =
      Map.to_alist unawaited
      |> List.map ~f:(fun (location, awaitable_state) ->
             Format.asprintf "%a -> %a" Awaitable.pp location pp_awaitable_state awaitable_state)
      |> String.concat ~sep:", "
    in
    let awaitables_for_alias =
      let show_awaitables awaitables =
        Set.to_list awaitables |> List.map ~f:Awaitable.show |> String.concat ~sep:", "
      in
      Map.to_alist awaitables_for_alias
      |> List.map ~f:(fun (alias_for_awaitable, locations) ->
             Format.asprintf
               "%a -> {%s}"
               pp_alias_for_awaitable
               alias_for_awaitable
               (show_awaitables locations))
      |> String.concat ~sep:", "
    in
    Format.sprintf
      "Unawaited expressions: %s\nAwaitables for aliases: %s\nNeed to await: %b"
      unawaited
      awaitables_for_alias
      need_to_await


  let pp format state = Format.fprintf format "%s" (show state)

  let bottom =
    {
      unawaited = Awaitable.Map.empty;
      awaitables_for_alias = AliasMap.empty;
      need_to_await = false;
    }


  let initial ~global_resolution { Define.signature = { Define.Signature.parameters; _ }; _ } =
    let state =
      {
        unawaited = Awaitable.Map.empty;
        awaitables_for_alias = AliasMap.empty;
        need_to_await = true;
      }
    in
    let forward_parameter
        ({ unawaited; awaitables_for_alias; need_to_await } as state)
        { Node.value = { Expression.Parameter.name; annotation; _ }; location }
      =
      let is_awaitable =
        match annotation with
        | Some annotation ->
            let annotation = GlobalResolution.parse_annotation global_resolution annotation in
            is_awaitable ~global_resolution annotation
        | None -> false
      in
      if is_awaitable then
        let name =
          if String.is_prefix ~prefix:"**" name then
            String.drop_prefix name 2
          else if String.is_prefix ~prefix:"*" name then
            String.drop_prefix name 1
          else
            name
        in
        {
          unawaited =
            Map.set
              unawaited
              ~key:(Awaitable.create location)
              ~data:(Unawaited { Node.value = Name (Expression.Name.Identifier name); location });
          awaitables_for_alias =
            Map.set
              awaitables_for_alias
              ~key:(NamedAlias (Reference.create name))
              ~data:(Awaitable.create location |> Awaitable.Set.singleton);
          need_to_await;
        }
      else
        state
    in
    List.fold ~init:state ~f:forward_parameter parameters


  let errors { unawaited; awaitables_for_alias; _ } =
    let errors =
      let keep_unawaited = function
        | Unawaited expression -> Some { Error.references = []; expression }
        | Awaited -> None
      in
      Map.filter_map unawaited ~f:keep_unawaited
    in
    let add_reference ~key ~data errors =
      let awaitables = data in
      let alias = key in
      match alias with
      | NamedAlias name ->
          let add_reference errors awaitable =
            match Map.find errors awaitable with
            | Some { Error.references; expression } ->
                Map.set errors ~key:awaitable ~data:{ references = name :: references; expression }
            | None -> errors
          in
          Awaitable.Set.fold awaitables ~init:errors ~f:add_reference
      | ExpressionWithNestedAliases _ -> errors
    in
    let error (awaitable, unawaited_awaitable) =
      Error.create
        ~location:
          (Awaitable.to_location awaitable
          |> Location.with_module ~module_reference:Context.qualifier)
        ~kind:(Error.UnawaitedAwaitable unawaited_awaitable)
        ~define:Context.define
    in
    Map.fold awaitables_for_alias ~init:errors ~f:add_reference |> Map.to_alist |> List.map ~f:error


  let less_or_equal ~left ~right =
    let less_or_equal_unawaited (reference, awaitable_state) =
      match awaitable_state, Map.find right.unawaited reference with
      | Unawaited _, Some _ -> true
      | Awaited, Some Awaited -> true
      | _ -> false
    in
    let less_or_equal_awaitables_for_alias (reference, locations) =
      match Map.find right.awaitables_for_alias reference with
      | Some other_locations -> Set.is_subset locations ~of_:other_locations
      | None -> false
    in
    Map.to_alist left.unawaited |> List.for_all ~f:less_or_equal_unawaited
    && Map.to_alist left.awaitables_for_alias |> List.for_all ~f:less_or_equal_awaitables_for_alias


  (* TODO(T79853064): If an awaitable is unawaited in one branch, we should consider it as
     unawaited. *)
  let join left right =
    let merge_unawaited ~key:_ left right =
      match left, right with
      | Awaited, _
      | _, Awaited ->
          Awaited
      | unawaited, _ -> unawaited
    in
    let merge_awaitables ~key:_ left right = Set.union left right in
    {
      unawaited = Map.merge_skewed left.unawaited right.unawaited ~combine:merge_unawaited;
      awaitables_for_alias =
        Map.merge_skewed
          left.awaitables_for_alias
          right.awaitables_for_alias
          ~combine:merge_awaitables;
      need_to_await = left.need_to_await || right.need_to_await;
    }


  let widen ~previous ~next ~iteration:_ = join previous next

  let mark_name_as_awaited { unawaited; awaitables_for_alias; need_to_await } ~name =
    if Expression.is_simple_name name then
      let unawaited =
        let await_location unawaited location = Map.set unawaited ~key:location ~data:Awaited in
        Map.find awaitables_for_alias (NamedAlias (Expression.name_to_reference_exn name))
        >>| (fun locations -> Set.fold locations ~init:unawaited ~f:await_location)
        |> Option.value ~default:unawaited
      in
      { unawaited; awaitables_for_alias; need_to_await }
    else (* Non-simple names cannot store awaitables. *)
      { unawaited; awaitables_for_alias; need_to_await }


  let mark_location_as_awaited { unawaited; awaitables_for_alias; need_to_await } ~location =
    if Map.mem unawaited (Awaitable.create location) then
      {
        unawaited = Map.set unawaited ~key:(Awaitable.create location) ~data:Awaited;
        awaitables_for_alias;
        need_to_await;
      }
    else
      match
        Map.find
          awaitables_for_alias
          (ExpressionWithNestedAliases { expression_location = location })
      with
      | Some awaitables ->
          let unawaited =
            Set.fold awaitables ~init:unawaited ~f:(fun unawaited awaitable ->
                Map.set unawaited ~key:awaitable ~data:Awaited)
          in
          { unawaited; awaitables_for_alias; need_to_await }
      | None -> { unawaited; awaitables_for_alias; need_to_await }


  let ( |>> ) (new_awaitables, state) awaitables = List.rev_append new_awaitables awaitables, state

  let await_all_subexpressions ~state ~expression =
    let names =
      Visit.collect_names
        { Node.location = Node.location expression; value = Expression expression }
    in
    let mark state name = mark_name_as_awaited state ~name:(Node.value name) in
    List.fold names ~init:state ~f:mark


  let rec forward_generator
      ~resolution
      (awaitables, state)
      { Expression.Comprehension.Generator.target = _; iterator; conditions; async = _ }
    =
    let awaitables, state =
      List.fold
        conditions
        ~f:(fun (awaitables, state) expression ->
          forward_expression ~resolution ~state ~expression |>> awaitables)
        ~init:(awaitables, state)
    in
    forward_expression ~resolution ~state ~expression:iterator |>> awaitables


  and forward_expression
      ~resolution
      ~(state : t)
      ~expression:({ Node.value; location } as expression)
    =
    let open Expression in
    match value with
    | Await ({ Node.value = Name name; _ } as expression) when is_simple_name name ->
        let awaitables, state = forward_expression ~resolution ~state ~expression in
        awaitables, mark_name_as_awaited state ~name
    | Await ({ Node.location; _ } as expression) ->
        let awaitables, state = forward_expression ~resolution ~state ~expression in
        awaitables, mark_location_as_awaited state ~location
    | BooleanOperator { BooleanOperator.left; right; _ } ->
        let awaitables, state = forward_expression ~resolution ~state ~expression:left in
        let new_awaitables, state = forward_expression ~resolution ~state ~expression:right in
        List.rev_append awaitables new_awaitables, state
    | Call { Call.callee; arguments } -> (
        let awaitables, state = forward_expression ~resolution ~state ~expression:callee in
        let is_special_function =
          match Node.value callee with
          | Name (Name.Attribute { special = true; _ }) -> true
          | _ -> false
        in
        let forward_argument (awaitables, state) { Call.Argument.value; _ } =
          (* For special methods such as `+`, ensure that we still require you to await the
             expressions. *)
          if is_special_function then
            forward_expression ~resolution ~state ~expression:value |>> awaitables
          else
            let state = await_all_subexpressions ~state ~expression:value in
            awaitables, state
        in
        let awaitables, state =
          match Node.value callee, arguments with
          (* a["b"] = c gets converted to a.__setitem__("b", c). *)
          | ( Name (Name.Attribute { attribute = "__setitem__"; base; _ }),
              [_; { Call.Argument.value; _ }] ) -> (
              let new_awaitable_expressions, state =
                forward_expression ~resolution ~state ~expression:value
              in
              match Node.value base with
              | Name name when is_simple_name name && not (List.is_empty new_awaitable_expressions)
                ->
                  let { awaitables_for_alias; _ } = state in
                  let name = name_to_reference_exn name in
                  let new_awaitables =
                    new_awaitable_expressions
                    |> List.map ~f:Node.location
                    |> List.map ~f:Awaitable.create
                    |> Awaitable.Set.of_list
                  in
                  let awaitables_for_alias =
                    match Map.find awaitables_for_alias (NamedAlias name) with
                    | Some awaitables ->
                        Map.set
                          awaitables_for_alias
                          ~key:(NamedAlias name)
                          ~data:(Set.union awaitables new_awaitables)
                    | None ->
                        Map.set awaitables_for_alias ~key:(NamedAlias name) ~data:new_awaitables
                  in
                  ( List.rev_append new_awaitable_expressions awaitables,
                    { state with awaitables_for_alias } )
              | _ -> awaitables, state)
          | _ ->
              let need_to_await = state.need_to_await in
              (* Don't introduce awaitables for the arguments of a call, as they will be consumed by
                 the call anyway. *)
              let awaitables, state =
                List.fold
                  arguments
                  ~init:(awaitables, { state with need_to_await = is_special_function })
                  ~f:forward_argument
              in
              let state = { state with need_to_await } in
              awaitables, state
        in
        let annotation = Resolution.resolve_expression_to_type resolution expression in
        let { unawaited; awaitables_for_alias; need_to_await } = state in
        let find_aliases { Node.value; location } =
          let awaitable = Awaitable.create location in
          if Map.mem unawaited awaitable then
            Some (Awaitable.Set.singleton awaitable)
          else
            match value with
            | Expression.Name name when is_simple_name name ->
                Map.find awaitables_for_alias (NamedAlias (name_to_reference_exn name))
            | _ ->
                Map.find
                  awaitables_for_alias
                  (ExpressionWithNestedAliases { expression_location = location })
        in
        if
          need_to_await
          && is_awaitable ~global_resolution:(Resolution.global_resolution resolution) annotation
        then
          (* If the callee is a method on an awaitable, make the assumption that the returned value
             is the same awaitable. *)
          let awaitables = expression :: awaitables in
          let awaitable = Awaitable.create location in
          match Node.value callee with
          | Name (Name.Attribute { base; _ }) -> (
              match find_aliases base with
              | Some locations ->
                  ( awaitables,
                    {
                      unawaited;
                      awaitables_for_alias =
                        Map.set
                          awaitables_for_alias
                          ~key:(ExpressionWithNestedAliases { expression_location = location })
                          ~data:locations;
                      need_to_await;
                    } )
              | None ->
                  ( awaitables,
                    {
                      unawaited = Map.set unawaited ~key:awaitable ~data:(Unawaited expression);
                      awaitables_for_alias;
                      need_to_await;
                    } ))
          | _ ->
              ( awaitables,
                {
                  unawaited = Map.set unawaited ~key:awaitable ~data:(Unawaited expression);
                  awaitables_for_alias;
                  need_to_await;
                } )
        else
          match Node.value callee with
          | Name (Name.Attribute { base; _ }) -> (
              (* If we can't resolve the type of the method as being an awaitable, be unsound and
                 assume the method awaits the `base` awaitable. Otherwise, we will complain that the
                 awaitable pointed to by `base` was never awaited.

                 Do this by pretending that the entire expression actually refers to the same
                 awaitable(s) pointed to by `base`. *)
              match find_aliases base with
              | Some locations ->
                  ( awaitables,
                    {
                      unawaited;
                      awaitables_for_alias =
                        Map.set
                          awaitables_for_alias
                          ~key:(ExpressionWithNestedAliases { expression_location = location })
                          ~data:locations;
                      need_to_await;
                    } )
              | None -> awaitables, state)
          | _ -> awaitables, state)
    | ComparisonOperator { ComparisonOperator.left; right; _ } ->
        let awaitables, state = forward_expression ~resolution ~state ~expression:left in
        forward_expression ~resolution ~state ~expression:right |>> awaitables
    | Dictionary { Dictionary.entries; keywords } ->
        let forward_entry (awaitables, state) { Dictionary.Entry.key; value } =
          let awaitables, state =
            forward_expression ~resolution ~state ~expression:key |>> awaitables
          in
          forward_expression ~resolution ~state ~expression:value |>> awaitables
        in
        let awaitables, state = List.fold entries ~init:([], state) ~f:forward_entry in
        let forward_keywords (awaitables, state) expression =
          forward_expression ~resolution ~state ~expression |>> awaitables
        in
        List.fold keywords ~init:(awaitables, state) ~f:forward_keywords
    | Lambda { Lambda.body; _ } -> forward_expression ~resolution ~state ~expression:body
    | Starred (Starred.Once expression)
    | Starred (Starred.Twice expression) ->
        forward_expression ~resolution ~state ~expression
    | Ternary { Ternary.target; test; alternative } ->
        let awaitables, state = forward_expression ~resolution ~state ~expression:target in
        let awaitables, state =
          forward_expression ~resolution ~state ~expression:test |>> awaitables
        in
        forward_expression ~resolution ~state ~expression:alternative |>> awaitables
    | List items
    | Set items
    | Tuple items ->
        List.fold items ~init:([], state) ~f:(fun (awaitables, state) expression ->
            forward_expression ~resolution ~state ~expression |>> awaitables)
    | UnaryOperator { UnaryOperator.operand; _ } ->
        forward_expression ~resolution ~state ~expression:operand
    | WalrusOperator { target; value; _ } ->
        let awaitables, state = forward_expression ~resolution ~state ~expression:target in
        forward_expression ~resolution ~state ~expression:value |>> awaitables
    | Yield (Some expression)
    | YieldFrom expression ->
        let expressions, state = forward_expression ~resolution ~state ~expression in
        let state = await_all_subexpressions ~state ~expression in
        expressions, state
    | Yield None -> [], state
    | Generator { Comprehension.element; generators }
    | ListComprehension { Comprehension.element; generators }
    | SetComprehension { Comprehension.element; generators } ->
        let awaitables, state =
          List.fold generators ~init:([], state) ~f:(forward_generator ~resolution)
        in
        forward_expression ~resolution ~state ~expression:element |>> awaitables
    | DictionaryComprehension
        { Comprehension.element = { Dictionary.Entry.key; value }; generators } ->
        let awaitables, state =
          List.fold generators ~init:([], state) ~f:(forward_generator ~resolution)
        in
        let awaitables, state =
          forward_expression ~resolution ~state ~expression:key |>> awaitables
        in
        forward_expression ~resolution ~state ~expression:value |>> awaitables
    | Name (Name.Attribute { base; _ }) -> forward_expression ~resolution ~state ~expression:base
    | Name name when is_simple_name name ->
        let awaitables =
          match Map.find state.awaitables_for_alias (NamedAlias (name_to_reference_exn name)) with
          | Some aliases ->
              let add_unawaited unawaited location =
                match Map.find state.unawaited location with
                | Some (Unawaited expression) -> expression :: unawaited
                | _ -> unawaited
              in
              Set.fold aliases ~init:[] ~f:add_unawaited
          | None -> []
        in
        awaitables, state
    | FormatString substrings ->
        List.fold substrings ~init:([], state) ~f:(fun (awaitables, state) substring ->
            match substring with
            | Substring.Format expression -> forward_expression ~resolution ~state ~expression
            | Substring.Literal _ -> awaitables, state)
    (* Base cases. *)
    | Constant _
    | Name _ ->
        [], state


  and forward_assign
      ~resolution
      ~state:({ unawaited; awaitables_for_alias; need_to_await } as state)
      ~annotation
      ~expression
      ~awaitable_expressions
      ~target
    =
    let open Expression in
    let is_nonuniform_sequence ~minimum_length annotation =
      match annotation with
      | Type.Tuple (Concrete parameters) when minimum_length <= List.length parameters -> true
      | _ -> false
    in
    let nonuniform_sequence_parameters annotation =
      match annotation with
      | Type.Tuple (Concrete parameters) -> parameters
      | _ -> []
    in
    match Node.value target with
    | Expression.Name target when is_simple_name target -> (
        match expression with
        | { Node.value = Expression.Name value; _ } when is_simple_name value ->
            (* Aliasing. *)
            let awaitables_for_alias =
              Map.find awaitables_for_alias (NamedAlias (name_to_reference_exn value))
              >>| (fun locations ->
                    Map.set
                      awaitables_for_alias
                      ~key:(NamedAlias (name_to_reference_exn target))
                      ~data:locations)
              |> Option.value ~default:awaitables_for_alias
            in
            { unawaited; awaitables_for_alias; need_to_await }
        | _ ->
            (* The expression must be analyzed before we call `forward_assign` on it, as that's
               where unawaitables are introduced. *)
            let awaitables_for_alias =
              let awaitable = Node.location expression |> Awaitable.create in
              let key = NamedAlias (name_to_reference_exn target) in
              if not (List.is_empty awaitable_expressions) then
                let awaitables =
                  List.map awaitable_expressions ~f:Node.location
                  |> List.map ~f:Awaitable.create
                  |> Awaitable.Set.of_list
                in
                Map.set awaitables_for_alias ~key ~data:awaitables
              else if Map.mem unawaited awaitable then
                Map.set awaitables_for_alias ~key ~data:(Awaitable.Set.singleton awaitable)
              else
                awaitables_for_alias
            in
            { unawaited; awaitables_for_alias; need_to_await })
    | List elements
    | Tuple elements
      when is_nonuniform_sequence ~minimum_length:(List.length elements) annotation -> (
        let left, starred, right =
          let is_starred { Node.value; _ } =
            match value with
            | Expression.Starred (Starred.Once _) -> true
            | _ -> false
          in
          let left, tail = List.split_while elements ~f:(fun element -> not (is_starred element)) in
          let starred, right =
            let starred, right = List.split_while tail ~f:is_starred in
            let starred =
              match starred with
              | [{ Node.value = Starred (Starred.Once starred); _ }] -> [starred]
              | _ -> []
            in
            starred, right
          in
          left, starred, right
        in
        match Node.value expression with
        | Tuple items ->
            let tuple_values =
              let annotations = List.zip_exn (nonuniform_sequence_parameters annotation) items in
              let left, tail = List.split_n annotations (List.length left) in
              let starred, right = List.split_n tail (List.length tail - List.length right) in
              let starred =
                if not (List.is_empty starred) then
                  let annotation =
                    List.fold starred ~init:Type.Bottom ~f:(fun joined (annotation, _) ->
                        GlobalResolution.join resolution joined annotation)
                    |> Type.list
                  in
                  [
                    ( annotation,
                      Node.create_with_default_location (Expression.List (List.map starred ~f:snd))
                    );
                  ]
                else
                  []
              in
              left @ starred @ right
            in
            List.zip_exn (left @ starred @ right) tuple_values
            |> List.fold ~init:state ~f:(fun state (target, (annotation, expression)) ->
                   (* Don't propagate awaitables for tuple assignments for soundness. *)
                   forward_assign
                     ~resolution
                     ~state
                     ~target
                     ~annotation
                     ~awaitable_expressions:[]
                     ~expression)
        | _ ->
            (* Right now, if we don't have a concrete tuple to break down, we won't introduce new
               unawaited awaitables. *)
            state)
    | _ -> state


  let forward ~statement_key state ~statement:{ Node.value; _ } =
    let { Node.value = { Define.signature = { Define.Signature.parent; _ }; _ }; _ } =
      Context.define
    in
    let resolution =
      TypeCheck.resolution_with_key
        ~global_resolution:Context.global_resolution
        ~local_annotations:Context.local_annotations
        ~parent
        ~statement_key
        (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
        (module TypeCheck.DummyContext)
    in
    let global_resolution = Resolution.global_resolution resolution in
    match value with
    | Statement.Assert { Assert.test; _ } ->
        forward_expression ~resolution ~state ~expression:test |> snd
    | Assign { value; target; _ } ->
        let awaitable_expressions, state =
          forward_expression ~resolution ~state ~expression:value
        in
        let annotation = Resolution.resolve_expression_to_type resolution value in
        forward_assign
          ~state
          ~resolution:global_resolution
          ~annotation
          ~expression:value
          ~awaitable_expressions
          ~target
    | Delete expressions ->
        let f state expression = forward_expression ~resolution ~state ~expression |> snd in
        List.fold expressions ~init:state ~f
    | Expression expression -> forward_expression ~resolution ~state ~expression |> snd
    | Raise { Raise.expression = None; _ } -> state
    | Raise { Raise.expression = Some expression; _ } ->
        forward_expression ~resolution ~state ~expression |> snd
    | Return { expression = Some expression; _ } ->
        let need_to_await = state.need_to_await in
        let _, state =
          forward_expression ~resolution ~state:{ state with need_to_await = false } ~expression
        in
        let state = { state with need_to_await } in
        await_all_subexpressions ~state ~expression
    | Return { expression = None; _ } -> state
    (* Control flow and nested functions/classes doesn't need to be analyzed explicitly. *)
    | If _
    | Class _
    | Define _
    | For _
    | Match _
    | While _
    | With _
    | Try _ ->
        state
    (* Trivial cases. *)
    | Break
    | Continue
    | Global _
    | Import _
    | Nonlocal _
    | Pass ->
        state


  let backward ~statement_key:_ _ ~statement:_ = failwith "Not implemented"
end

let unawaited_awaitable_errors ~type_environment ~qualifier define =
  let module Context = struct
    let qualifier = qualifier

    let define = define

    let global_resolution = TypeEnvironment.ReadOnly.global_resolution type_environment

    let local_annotations =
      TypeEnvironment.TypeEnvironmentReadOnly.get_or_recompute_local_annotations
        type_environment
        (Node.value define |> Define.name)
  end
  in
  let module State = State (Context) in
  let module Fixpoint = Fixpoint.Make (State) in
  let cfg = Cfg.create (Node.value define) in
  let global_resolution =
    TypeEnvironment.ReadOnly.global_environment type_environment |> GlobalResolution.create
  in
  Fixpoint.forward ~cfg ~initial:(State.initial ~global_resolution (Node.value define))
  |> Fixpoint.exit
  >>| State.errors
  |> Option.value ~default:[]


(** Avoid emitting "unawaited awaitable" errors for classes that inherit from `Awaitable`. They may,
    for example, store attributes that are unawaited. *)
let should_run_analysis
    ~type_environment
    { Node.value = { Define.signature = { parent; _ }; _ }; _ }
  =
  let global_resolution =
    TypeEnvironment.ReadOnly.global_environment type_environment |> GlobalResolution.create
  in
  parent
  >>| (fun parent -> Type.Primitive (Reference.show parent))
  >>| is_awaitable ~global_resolution
  >>| not
  |> Option.value ~default:true


let check_define ~type_environment ~qualifier define =
  if should_run_analysis ~type_environment define then
    unawaited_awaitable_errors ~type_environment ~qualifier define
  else
    []


let check_module_TESTING_ONLY
    ~type_environment
    ({ Source.module_path = { ModulePath.qualifier; _ }; _ } as source)
  =
  source
  |> Preprocessing.defines ~include_toplevels:true
  |> List.map ~f:(check_define ~type_environment ~qualifier)
  |> List.concat
