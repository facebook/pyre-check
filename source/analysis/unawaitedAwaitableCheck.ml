(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

[@@@warning "-27"]

open Core
open Ast
open Statement
open Pyre
open Expression
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

(* Return true if the expression is of an awaitable type that will lead to a `RuntimeWarning` that
   `coroutine ... was never awaited`.

   Note that this is more restrictive than the general question of "does this type satisfy the
   Awaitable protocol?", which just needs to check for the `__await__` method. However, this matches
   our goal of catching potential RuntimeWarnings about unawaited coroutines. Otherwise, we would
   emit an error for entire class hierarchies where the base class happens to have a `__await__`
   method, since that means every class in the hierarchy will satisfy the `Awaitable` protocol. That
   has caused a lot of noise in the past for basic functions such as:

   def set_foo(builder: BuilderClass, foo: Foo) -> BuilderClass: ...

   which is seen as a function returning an awaitable, since `BuilderClass` satisfies `Awaitable`.
   Given that there is no RuntimeWarning for classes with hand-rolled `__await__`, we can leave them
   out of our analysis. *)
let can_lead_to_runtime_warning_if_unawaited ~global_resolution = function
  | Type.Parametric { name = "typing.Coroutine" | "typing.Awaitable"; _ } -> true
  | _ -> false


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

  val local_annotations : LocalAnnotationMap.ReadOnly.t option

  val resolution : Resolution.t
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
    awaitable_to_awaited_state: awaitable_state Awaitable.Map.t;
    (* For an alias, what awaitable locations could it point to? *)
    awaitables_for_alias: Awaitable.Set.t AliasMap.t;
    (* HACK: This flag represents whether an expression should be expected to await.

       This is used to indicate that expressions that are returned or passed to a callee should not
       cause unawaited-errors even if they were not awaited. This is because the job of awaiting
       them is up to the caller or the callee, respectively. Note that if the function returns or
       passes an expression of the wrong type, that will be a type error in the normal fixpoint. So,
       we don't need to worry about that here. *)
    expect_expressions_to_be_awaited: bool;
  }

  (* The result of `forward_expression`. *)
  type forward_expression_result = {
    state: t;
    (* The nested awaitable expressions that would be awaited by awaiting/handling the overall
       expression.

       For example, if the expression is a list containing awaitables (maybe nested), this will have
       the individual awaitable expressions. That way, if someone passes the overall expression into
       `asyncio.gather` or some other function, they will all be marked as awaited (or
       not-unawaited). In short, we won't have to emit unawaited errors for them. *)
    nested_awaitable_expressions: Expression.t list;
  }

  let result_state { state; nested_awaitable_expressions = _ } = state

  let show { awaitable_to_awaited_state; awaitables_for_alias; expect_expressions_to_be_awaited } =
    let awaitable_to_awaited_state =
      Map.to_alist awaitable_to_awaited_state
      |> List.map ~f:(fun (awaitable, awaitable_state) ->
             Format.asprintf "%a -> %a" Awaitable.pp awaitable pp_awaitable_state awaitable_state)
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
      awaitable_to_awaited_state
      awaitables_for_alias
      expect_expressions_to_be_awaited


  let pp format state = Format.fprintf format "%s" (show state)

  let bottom =
    {
      awaitable_to_awaited_state = Awaitable.Map.empty;
      awaitables_for_alias = AliasMap.empty;
      expect_expressions_to_be_awaited = false;
    }


  let initial =
    {
      awaitable_to_awaited_state = Awaitable.Map.empty;
      awaitables_for_alias = AliasMap.empty;
      expect_expressions_to_be_awaited = true;
    }


  let errors { awaitable_to_awaited_state; awaitables_for_alias; _ } =
    let errors =
      let keep_unawaited = function
        | Unawaited expression -> Some { Error.references = []; expression }
        | Awaited -> None
      in
      Map.filter_map awaitable_to_awaited_state ~f:keep_unawaited
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
      match awaitable_state, Map.find right.awaitable_to_awaited_state reference with
      | Unawaited _, Some _ -> true
      | Awaited, Some Awaited -> true
      | _ -> false
    in
    let less_or_equal_awaitables_for_alias (reference, locations) =
      match Map.find right.awaitables_for_alias reference with
      | Some other_locations -> Set.is_subset locations ~of_:other_locations
      | None -> false
    in
    Map.to_alist left.awaitable_to_awaited_state |> List.for_all ~f:less_or_equal_unawaited
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
      awaitable_to_awaited_state =
        Map.merge_skewed
          left.awaitable_to_awaited_state
          right.awaitable_to_awaited_state
          ~combine:merge_unawaited;
      awaitables_for_alias =
        Map.merge_skewed
          left.awaitables_for_alias
          right.awaitables_for_alias
          ~combine:merge_awaitables;
      expect_expressions_to_be_awaited =
        left.expect_expressions_to_be_awaited || right.expect_expressions_to_be_awaited;
    }


  let widen ~previous ~next ~iteration:_ = join previous next

  let mark_name_as_awaited
      { awaitable_to_awaited_state; awaitables_for_alias; expect_expressions_to_be_awaited }
      ~name
    =
    if is_simple_name name then
      let awaitable_to_awaited_state =
        let await_location awaitable_to_awaited_state awaitable =
          Map.set awaitable_to_awaited_state ~key:awaitable ~data:Awaited
        in
        Map.find awaitables_for_alias (NamedAlias (name_to_reference_exn name))
        >>| (fun locations -> Set.fold locations ~init:awaitable_to_awaited_state ~f:await_location)
        |> Option.value ~default:awaitable_to_awaited_state
      in
      { awaitable_to_awaited_state; awaitables_for_alias; expect_expressions_to_be_awaited }
    else (* Non-simple names cannot store awaitables. *)
      { awaitable_to_awaited_state; awaitables_for_alias; expect_expressions_to_be_awaited }


  let mark_awaitable_as_awaited
      { awaitable_to_awaited_state; awaitables_for_alias; expect_expressions_to_be_awaited }
      ~awaitable
    =
    if Map.mem awaitable_to_awaited_state awaitable then
      {
        awaitable_to_awaited_state = Map.set awaitable_to_awaited_state ~key:awaitable ~data:Awaited;
        awaitables_for_alias;
        expect_expressions_to_be_awaited;
      }
    else
      match
        Map.find
          awaitables_for_alias
          (ExpressionWithNestedAliases { expression_location = Awaitable.to_location awaitable })
      with
      | Some awaitables ->
          let awaitable_to_awaited_state =
            Set.fold
              awaitables
              ~init:awaitable_to_awaited_state
              ~f:(fun awaitable_to_awaited_state awaitable ->
                Map.set awaitable_to_awaited_state ~key:awaitable ~data:Awaited)
          in
          { awaitable_to_awaited_state; awaitables_for_alias; expect_expressions_to_be_awaited }
      | None ->
          { awaitable_to_awaited_state; awaitables_for_alias; expect_expressions_to_be_awaited }


  let ( |>> ) { state; nested_awaitable_expressions } existing_awaitables =
    {
      state;
      nested_awaitable_expressions =
        List.rev_append nested_awaitable_expressions existing_awaitables;
    }


  let await_all_subexpressions ~state ~expression =
    let names =
      Visit.collect_names
        { Node.location = Node.location expression; value = Expression expression }
    in
    let mark state name = mark_name_as_awaited state ~name:(Node.value name) in
    List.fold names ~init:state ~f:mark


  let rec forward_generator
      ~resolution
      { state; nested_awaitable_expressions }
      { Comprehension.Generator.target = _; iterator; conditions; async = _ }
    =
    let { state; nested_awaitable_expressions } =
      List.fold
        conditions
        ~f:(fun { state; nested_awaitable_expressions } expression ->
          forward_expression ~resolution ~state ~expression |>> nested_awaitable_expressions)
        ~init:{ state; nested_awaitable_expressions }
    in
    forward_expression ~resolution ~state ~expression:iterator |>> nested_awaitable_expressions


  and forward_call ~resolution ~state ~location ({ Call.callee; arguments } as call) =
    let is_special_function callee =
      match Node.value callee with
      | Expression.Name (Name.Attribute { special = true; _ }) -> true
      | _ -> false
    in
    let forward_arguments { state; nested_awaitable_expressions } =
      let forward_argument { state; nested_awaitable_expressions } { Call.Argument.value; _ } =
        (* For special methods such as `+`, ensure that we still require you to await the
           expressions. *)
        if is_special_function callee then
          forward_expression ~resolution ~state ~expression:value |>> nested_awaitable_expressions
        else
          let state = await_all_subexpressions ~state ~expression:value in
          { state; nested_awaitable_expressions }
      in
      match Node.value callee, arguments with
      (* a["b"] = c gets converted to a.__setitem__("b", c). *)
      | ( Name (Name.Attribute { attribute = "__setitem__"; base; _ }),
          [_; { Call.Argument.value; _ }] ) -> (
          let { state; nested_awaitable_expressions = new_awaitable_expressions } =
            forward_expression ~resolution ~state ~expression:value
          in
          match Node.value base with
          | Name name when is_simple_name name && not (List.is_empty new_awaitable_expressions) ->
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
                | Some nested_awaitable_expressions ->
                    Map.set
                      awaitables_for_alias
                      ~key:(NamedAlias name)
                      ~data:(Set.union nested_awaitable_expressions new_awaitables)
                | None -> Map.set awaitables_for_alias ~key:(NamedAlias name) ~data:new_awaitables
              in
              {
                state = { state with awaitables_for_alias };
                nested_awaitable_expressions =
                  List.rev_append new_awaitable_expressions nested_awaitable_expressions;
              }
          | _ -> { state; nested_awaitable_expressions })
      | _ ->
          let expect_expressions_to_be_awaited = state.expect_expressions_to_be_awaited in
          (* Don't introduce awaitables for the arguments of a call, as they will be consumed by the
             call anyway. *)
          let { state; nested_awaitable_expressions } =
            List.fold
              arguments
              ~init:
                {
                  state =
                    { state with expect_expressions_to_be_awaited = is_special_function callee };
                  nested_awaitable_expressions;
                }
              ~f:forward_argument
          in
          let state = { state with expect_expressions_to_be_awaited } in
          { state; nested_awaitable_expressions }
    in
    let find_aliases
        ~state:{ awaitable_to_awaited_state; awaitables_for_alias; _ }
        { Node.value; location }
      =
      let awaitable = Awaitable.create location in
      if Map.mem awaitable_to_awaited_state awaitable then
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
    let expression = { Node.value = Expression.Call call; location } in
    let {
      state =
        { awaitable_to_awaited_state; awaitables_for_alias; expect_expressions_to_be_awaited } as
        state;
      nested_awaitable_expressions;
    }
      =
      forward_expression ~resolution ~state ~expression:callee |> forward_arguments
    in
    let annotation = Resolution.resolve_expression_to_type resolution expression in
    if
      expect_expressions_to_be_awaited
      && can_lead_to_runtime_warning_if_unawaited
           ~global_resolution:(Resolution.global_resolution resolution)
           annotation
    then
      (* If the callee is a method on an awaitable, make the assumption that the returned value is
         the same awaitable. *)
      let nested_awaitable_expressions = expression :: nested_awaitable_expressions in
      let awaitable = Awaitable.create location in
      match Node.value callee with
      | Name (Name.Attribute { base; _ }) -> (
          match find_aliases ~state base with
          | Some locations ->
              {
                state =
                  {
                    state with
                    awaitables_for_alias =
                      Map.set
                        awaitables_for_alias
                        ~key:(ExpressionWithNestedAliases { expression_location = location })
                        ~data:locations;
                  };
                nested_awaitable_expressions;
              }
          | None ->
              {
                state =
                  {
                    state with
                    awaitable_to_awaited_state =
                      Map.set awaitable_to_awaited_state ~key:awaitable ~data:(Unawaited expression);
                  };
                nested_awaitable_expressions;
              })
      | _ ->
          {
            state =
              {
                state with
                awaitable_to_awaited_state =
                  Map.set awaitable_to_awaited_state ~key:awaitable ~data:(Unawaited expression);
              };
            nested_awaitable_expressions;
          }
    else
      match Node.value callee with
      | Name (Name.Attribute { base; _ }) -> (
          (* If we can't resolve the type of the method as being an awaitable, be unsound and assume
             the method awaits the `base` awaitable. Otherwise, we will complain that the awaitable
             pointed to by `base` was never awaited.

             Do this by pretending that the entire expression actually refers to the same
             awaitable(s) pointed to by `base`. *)
          match find_aliases ~state base with
          | Some locations ->
              {
                state =
                  {
                    state with
                    awaitables_for_alias =
                      Map.set
                        awaitables_for_alias
                        ~key:(ExpressionWithNestedAliases { expression_location = location })
                        ~data:locations;
                  };
                nested_awaitable_expressions;
              }
          | None -> { state; nested_awaitable_expressions })
      | _ -> { state; nested_awaitable_expressions }


  (** Add any awaitables introduced in the expression to the tracked awaitables in state. If any
      existing awaitables are awaited, mark them as awaited in the state.

      Return the new state along with nested awaitable expressions. *)
  and forward_expression ~resolution ~(state : t) ~expression:{ Node.value; location } =
    match value with
    | Await ({ Node.value = Name name; _ } as expression) when is_simple_name name ->
        let { state; nested_awaitable_expressions } =
          forward_expression ~resolution ~state ~expression
        in
        { state = mark_name_as_awaited state ~name; nested_awaitable_expressions }
    | Await ({ Node.location; _ } as expression) ->
        let { state; nested_awaitable_expressions } =
          forward_expression ~resolution ~state ~expression
        in
        let state =
          List.fold nested_awaitable_expressions ~init:state ~f:(fun state { Node.location; _ } ->
              mark_awaitable_as_awaited ~awaitable:(Awaitable.create location) state)
        in
        {
          state = mark_awaitable_as_awaited state ~awaitable:(Awaitable.create location);
          nested_awaitable_expressions;
        }
    | BooleanOperator { BooleanOperator.left; right; _ } ->
        let { state; nested_awaitable_expressions = left_awaitable_expressions } =
          forward_expression ~resolution ~state ~expression:left
        in
        let { state; nested_awaitable_expressions = right_awaitable_expressions } =
          forward_expression ~resolution ~state ~expression:right
        in
        {
          state;
          nested_awaitable_expressions =
            List.rev_append left_awaitable_expressions right_awaitable_expressions;
        }
    | Call call -> forward_call ~resolution ~state ~location call
    | ComparisonOperator { ComparisonOperator.left; right; _ } ->
        let { state; nested_awaitable_expressions } =
          forward_expression ~resolution ~state ~expression:left
        in
        forward_expression ~resolution ~state ~expression:right |>> nested_awaitable_expressions
    | Dictionary { Dictionary.entries; keywords } ->
        let forward_entry { state; nested_awaitable_expressions } { Dictionary.Entry.key; value } =
          let { state; nested_awaitable_expressions } =
            forward_expression ~resolution ~state ~expression:key |>> nested_awaitable_expressions
          in
          forward_expression ~resolution ~state ~expression:value |>> nested_awaitable_expressions
        in
        let { state; nested_awaitable_expressions } =
          List.fold entries ~init:{ state; nested_awaitable_expressions = [] } ~f:forward_entry
        in
        let forward_keywords { state; nested_awaitable_expressions } expression =
          forward_expression ~resolution ~state ~expression |>> nested_awaitable_expressions
        in
        List.fold keywords ~init:{ state; nested_awaitable_expressions } ~f:forward_keywords
    | Lambda { Lambda.body; _ } -> forward_expression ~resolution ~state ~expression:body
    | Starred (Starred.Once expression)
    | Starred (Starred.Twice expression) ->
        forward_expression ~resolution ~state ~expression
    | Ternary { Ternary.target; test; alternative } ->
        let { state; nested_awaitable_expressions } =
          forward_expression ~resolution ~state ~expression:target
        in
        let { state; nested_awaitable_expressions } =
          forward_expression ~resolution ~state ~expression:test |>> nested_awaitable_expressions
        in
        forward_expression ~resolution ~state ~expression:alternative
        |>> nested_awaitable_expressions
    | List items
    | Set items
    | Tuple items ->
        List.fold
          items
          ~init:{ state; nested_awaitable_expressions = [] }
          ~f:(fun { state; nested_awaitable_expressions } expression ->
            forward_expression ~resolution ~state ~expression |>> nested_awaitable_expressions)
    | UnaryOperator { UnaryOperator.operand; _ } ->
        forward_expression ~resolution ~state ~expression:operand
    | WalrusOperator { target; value; _ } ->
        let { state; nested_awaitable_expressions } =
          forward_expression ~resolution ~state ~expression:target
        in
        forward_expression ~resolution ~state ~expression:value |>> nested_awaitable_expressions
    | Yield (Some expression)
    | YieldFrom expression ->
        let { state; nested_awaitable_expressions } =
          forward_expression ~resolution ~state ~expression
        in
        let state = await_all_subexpressions ~state ~expression in
        { state; nested_awaitable_expressions }
    | Yield None -> { state; nested_awaitable_expressions = [] }
    | Generator { Comprehension.element; generators }
    | ListComprehension { Comprehension.element; generators }
    | SetComprehension { Comprehension.element; generators } ->
        let { state; nested_awaitable_expressions } =
          List.fold
            generators
            ~init:{ state; nested_awaitable_expressions = [] }
            ~f:(forward_generator ~resolution)
        in
        forward_expression ~resolution ~state ~expression:element |>> nested_awaitable_expressions
    | DictionaryComprehension
        { Comprehension.element = { Dictionary.Entry.key; value }; generators } ->
        let { state; nested_awaitable_expressions } =
          List.fold
            generators
            ~init:{ state; nested_awaitable_expressions = [] }
            ~f:(forward_generator ~resolution)
        in
        let { state; nested_awaitable_expressions } =
          forward_expression ~resolution ~state ~expression:key |>> nested_awaitable_expressions
        in
        forward_expression ~resolution ~state ~expression:value |>> nested_awaitable_expressions
    | Name (Name.Attribute { base; _ }) -> forward_expression ~resolution ~state ~expression:base
    | Name name when is_simple_name name ->
        let nested_awaitable_expressions =
          match Map.find state.awaitables_for_alias (NamedAlias (name_to_reference_exn name)) with
          | Some aliases ->
              let add_unawaited unawaited location =
                match Map.find state.awaitable_to_awaited_state location with
                | Some (Unawaited expression) -> expression :: unawaited
                | _ -> unawaited
              in
              Set.fold aliases ~init:[] ~f:add_unawaited
          | None -> []
        in
        { state; nested_awaitable_expressions }
    | FormatString substrings ->
        List.fold
          substrings
          ~init:{ state; nested_awaitable_expressions = [] }
          ~f:(fun { state; nested_awaitable_expressions } substring ->
            match substring with
            | Substring.Format expression -> forward_expression ~resolution ~state ~expression
            | Substring.Literal _ -> { state; nested_awaitable_expressions })
    (* Base cases. *)
    | Constant _
    | Name _ ->
        { state; nested_awaitable_expressions = [] }


  and forward_assign
      ~resolution
      ~state:({ awaitable_to_awaited_state; awaitables_for_alias; _ } as state)
      ~annotation
      ~expression
      ~awaitable_expressions_in_value
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
    | Expression.Name (Identifier target_name) -> (
        match expression with
        | { Node.value = Expression.Name value; _ } when is_simple_name value ->
            (* Aliasing: If `target` is awaited, then the awaitables pointed to by `value` will have
               been awaited. So, make `target` also point to them. *)
            let awaitables_for_alias =
              Map.find awaitables_for_alias (NamedAlias (name_to_reference_exn value))
              >>| (fun locations ->
                    Map.set
                      awaitables_for_alias
                      ~key:(NamedAlias (Reference.create target_name))
                      ~data:locations)
              |> Option.value ~default:awaitables_for_alias
            in
            { state with awaitables_for_alias }
        | _ ->
            let value_awaitable = Node.location expression |> Awaitable.create in
            let key = NamedAlias (Reference.create target_name) in
            let awaitables_for_alias =
              if not (List.is_empty awaitable_expressions_in_value) then
                (* If there are awaitables inside the value, then make `target` point to them.

                   For example, it is a list expression, `target = [awaitable(), awaitable2()]`,
                   `target` should point to the awaitables. *)
                let awaitables_in_value =
                  List.map awaitable_expressions_in_value ~f:Node.location
                  |> List.map ~f:Awaitable.create
                  |> Awaitable.Set.of_list
                in
                Map.set awaitables_for_alias ~key ~data:awaitables_in_value
              else if Map.mem awaitable_to_awaited_state value_awaitable then
                (* If the entire value being assigned is known to be an awaitable, make `target`
                   point to it. *)
                Map.set awaitables_for_alias ~key ~data:(Awaitable.Set.singleton value_awaitable)
              else
                awaitables_for_alias
            in
            { state with awaitables_for_alias })
    | Name (Attribute _) ->
        (* Unsoundly assume that any awaitable assigned to an attribute is being awaited somewhere.
           Otherwise, we risk getting many false positives for attribute assignments. *)
        let awaitable_to_awaited_state =
          Node.location expression
          |> Awaitable.create
          |> Map.change awaitable_to_awaited_state ~f:(function
                 | Some _ -> Some Awaited
                 | None -> None)
        in
        { state with awaitable_to_awaited_state }
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
                     ~awaitable_expressions_in_value:[]
                     ~expression)
        | _ ->
            (* Right now, if we don't have a concrete tuple to break down, we won't introduce new
               unawaited awaitables. *)
            state)
    | _ -> state


  let resolution_for_statement ~local_annotations ~parent ~statement_key resolution =
    let annotation_store =
      local_annotations
      >>= LocalAnnotationMap.ReadOnly.get_precondition ~statement_key
      |> Option.value ~default:Refinement.Store.empty
    in
    resolution
    |> Resolution.with_annotation_store ~annotation_store
    |> Resolution.with_parent ~parent


  let forward ~statement_key state ~statement:{ Node.value; _ } =
    let { Node.value = { Define.signature = { Define.Signature.parent; _ }; _ }; _ } =
      Context.define
    in
    let resolution =
      resolution_for_statement
        ~local_annotations:Context.local_annotations
        ~parent
        ~statement_key
        Context.resolution
    in
    match value with
    | Statement.Assert { Assert.test; _ } ->
        forward_expression ~resolution ~state ~expression:test |> result_state
    | Assign { value; target; _ } ->
        let { state; nested_awaitable_expressions } =
          forward_expression ~resolution ~state ~expression:value
        in
        let annotation = Resolution.resolve_expression_to_type resolution value in
        forward_assign
          ~state
          ~resolution:(Resolution.global_resolution resolution)
          ~annotation
          ~expression:value
          ~awaitable_expressions_in_value:nested_awaitable_expressions
          ~target
    | Delete expressions ->
        let f state expression =
          forward_expression ~resolution ~state ~expression |> result_state
        in
        List.fold expressions ~init:state ~f
    | Expression expression -> forward_expression ~resolution ~state ~expression |> result_state
    | Raise { Raise.expression = None; _ } -> state
    | Raise { Raise.expression = Some expression; _ } ->
        forward_expression ~resolution ~state ~expression |> result_state
    | Return { expression = Some expression; _ } ->
        let expect_expressions_to_be_awaited = state.expect_expressions_to_be_awaited in
        let { state; _ } =
          forward_expression
            ~resolution
            ~state:{ state with expect_expressions_to_be_awaited = false }
            ~expression
        in
        let state = { state with expect_expressions_to_be_awaited } in
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

let unawaited_awaitable_errors ~resolution ~local_annotations ~qualifier define =
  let module Context = struct
    let qualifier = qualifier

    let define = define

    let local_annotations = local_annotations

    let resolution = resolution
  end
  in
  let module State = State (Context) in
  let module Fixpoint = Fixpoint.Make (State) in
  let cfg = Cfg.create (Node.value define) in
  Fixpoint.forward ~cfg ~initial:State.initial
  |> Fixpoint.exit
  >>| State.errors
  |> Option.value ~default:[]


let should_check_define { Node.value = define; _ } = Define.is_async define

let check_define ~resolution ~local_annotations ~qualifier define =
  if not (should_check_define define) then
    []
  else
    let timer = Timer.start () in
    let errors = unawaited_awaitable_errors ~resolution ~local_annotations ~qualifier define in
    let () =
      Statistics.performance
        ~flush:false
        ~randomly_log_every:1000
        ~always_log_time_threshold:0.5 (* Seconds *)
        ~name:"UnawaitedAwaitableCheck"
        ~timer
        ~normals:
          [
            (* We want the time taken for each function, so send the function name as the name of
               the event. *)
            "name", define |> Node.value |> Define.name |> Reference.show;
            "request kind", "UnawaitedAwaitableCheck";
          ]
        ()
    in
    errors


let check_module_TESTING_ONLY
    ~resolution
    ~local_annotations_for_define
    ({ Source.module_path = { ModulePath.qualifier; _ }; _ } as source)
  =
  source
  |> Preprocessing.defines ~include_toplevels:true
  |> List.map ~f:(fun define ->
         check_define
           ~resolution
           ~local_annotations:(local_annotations_for_define (Node.value define |> Define.name))
           ~qualifier
           define)
  |> List.concat
