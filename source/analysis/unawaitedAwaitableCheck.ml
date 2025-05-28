(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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

    Strategy: Reaching-definitions analysis + ownership analysis

    1. Reaching-definitions analysis: We want to find out which awaitables are not awaited by the
    time the function returns (at least along some code path).

    - If an awaitable is awaited in some expression, then the awaitable doesn't reach the return
      point of the function. We can say that the `await` "kills" the "reach" of the awaitable.

    - If the awaitable is never awaited along some code path, then it reaches the return statement
      and is thus an unawaited awaitable.

    We can think of the original awaitable as a "definition". Getting all definitions that reach a
    certain point without being "killed" in between is called reaching-definition analysis.

    In our case, a "definition" is an awaitable expression (more precisely, its location in the
    code). "Killing" or consuming an awaitable means doing one of the following:

    - using `await` on it, which consumes it directly.
    - passing it in a function call, which transfers ownership (the callee should consume it).
    - yeilding or returning it, which transfers ownership to a calling / consuming scope.

    We want all definitions (awaitables) that reach the return point of the function without being
    killed (awaited or otherwise consumed).

    Reaching-definitions analysis means we need a forward analysis. For more details, see
    https://en.wikipedia.org/wiki/Reaching_definition.

    2. Ownership (specifically *shared* ownership, or aliasability) analysis: Awaitables can be
    stored in variables. If we await any variable that it has been assigned to (or any variable to
    which those variables have been assigned), then we should mark the original awaitable as
    awaited. So, whenever we detect that a variable is being assigned some other variable, we copy
    over all the awaitables that are pointed to by the other variable. *)

open Core
open Ast
open Statement
open Pyre
open Expression
module Error = AnalysisError

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
let can_lead_to_runtime_warning_if_unawaited ~global_resolution:_ = function
  | Type.Parametric
      {
        name =
          "typing.Coroutine" | "typing.Awaitable" | "asyncio.futures.Future" | "_asyncio.Future";
        _;
      } ->
      true
  | _ -> false


(* In order to do reachability and aliasability analysis at the same time, it is helpful to track
   the state (reachability) of an awaitable separately from the ownership (aliasability), and only
   move between them when we are ready to consume (or "kill" in reachability vocab) all the
   awaitables tracked by some owner.

   This requires using two separate maps to track them, and we need a persistent identifier for
   them. So we key awaitables based on the location of the *originator* expression where they were
   first introduced. This originator location remains the same even as ownership can bubble up (and
   be aggregated, i.e. one owner can track many separate originators) into larger expressions or
   into variables by way of assignment.

   For example:

   - in `await asyncio.gather(awaitable())` we will store the location of the `awaitable()` call as
   the `Awaitable.t` originator, even as ownership bubbles up to the `asyncio.gather` expression
   (and awaitability gets "consumed" when that owner is awaited)

   - in `x = awaitable()` we will store the location of the `awaitable()` call as the `Awaitable.t`
   originator, even though ownership will be transferred to the name `x` (and awaitability can
   eventually be consumed if we consume x by awaiting it, returning it, or passing it as an argument
   in a call). *)
module Awaitable : sig
  type t [@@deriving show, sexp, compare]

  module Map : Map.S with type Key.t = t

  module Set : Set.S with type Elt.t = t

  val of_location : Location.t -> t

  val to_location : t -> Location.t
end = struct
  module T = struct
    type t = Location.t [@@deriving show, sexp, compare]
  end

  include T
  module Map = Map.Make (T)
  module Set = Set.Make (T)

  let of_location = Fn.id

  let to_location = Fn.id
end

module type Context = sig
  val qualifier : Reference.t

  val define : Define.t Node.t

  val local_annotations : TypeInfo.ForFunctionBody.ReadOnly.t option

  val resolution : Resolution.t
end

module State (Context : Context) = struct
  type awaitable_state =
    | Unawaited of Expression.t
    | Awaited
  [@@deriving show]

  let _ = show_awaitable_state (* unused *)

  (* In order to analyze awaitables statically, we have to handle the propagation of
   * (shared) ownership of awaitables. This can happen in two ways:
   * - ownership can bubble outward through complex expressions that contain awaitable
   *   sub-expressions.
   *   - in most cases we don't currently need ownership to model this, we just
   *     track it using the `nested_awaitable_subexpressions` data in the
   *     `forward_expression_result` type.
   *   - but the current way handle interactions between `forward_call` and the
   *     consuming logic in `forward_expression` requires us to invent anonymous
   *     owners for chained method calls such as `create_awaitable().set_x(x)`.
   *     We use these anonymous owners so that we can mark the awaitable originating
   *     at `create_awaitable()` as awaited if the entire chained-method-call
   *     expression is awaited. The `AnonymousExpression` variant handles this.
   * - assignments (including subscript assignments) can propagate awaitables into
   *   variables, which unlike expressions can be awaited much later (i.e. in later
   *   statements); in addition, knowing the owning variable name can help make error
   *   messages more informative. This is handled by the `NamedOwner` variant.
   *
   * This notion of ownership is similar to Rust's but is more lax because we are
   * only trying to ensure that awaitables are *plausibly* waited *at least* once rather
   * than *definitely* waited *exactly* once.
   *
   * As a result, we allow the same awaitable to have multiple owners. For example:
   * ```
   * x = awaitable0()
   * d["x"] = x
   * ```
   * leads to both `x` and `d` tracking the awaitable originating at the expression
   * `awaitable()`, and consuming either `x` *or* `d` will lead to us marking it as
   * plausibly consumed.
   *
   * And since our analysis only cares about consuming *at least* once, we won't
   * complain if code tries to consume both `x` and `d`, for example by
   * awaiting both. This is an intentional choice - consuming an awaitable
   * twice typically leads to a pretty clear-cut runtime error, and our priority
   * here is attempting to statically catch the silent failures that happen when
   * an awaitable is never consumed.)
   *)
  type owner_for_awaitable =
    | AnonymousExpression of { location: Location.t }
    | NamedOwner of Reference.t
  [@@deriving compare, sexp, show]

  module OwnerMap = Map.Make (struct
    type t = owner_for_awaitable [@@deriving sexp, compare]
  end)

  type t = {
    (* For each awaitable, has it been awaited yet? *)
    awaitable_to_awaited_state: awaitable_state Awaitable.Map.t;
    (* For each owner, what are all the awaitables it could point to? *)
    owner_to_awaitables: Awaitable.Set.t OwnerMap.t;
    (* HACK: This flag represents whether an expression should be expected to await.

       This is used to indicate that expressions that are returned or passed to a callee should not
       cause unawaited-errors even if they were not awaited. This is because the job of awaiting
       them is up to the caller or the callee, respectively. Note that if the function returns or
       passes an expression of the wrong type, that will be a type error. So, we don't need to worry
       about that here. *)
    expect_expressions_to_be_awaited: bool;
  }

  let show { awaitable_to_awaited_state; owner_to_awaitables; expect_expressions_to_be_awaited } =
    let awaitable_to_awaited_state =
      Map.to_alist awaitable_to_awaited_state
      |> List.map ~f:(fun (awaitable, awaitable_state) ->
             Format.asprintf "%a -> %a" Awaitable.pp awaitable pp_awaitable_state awaitable_state)
      |> String.concat ~sep:", "
    in
    let owner_to_awaitables =
      let show_awaitables awaitables =
        Set.to_list awaitables |> List.map ~f:Awaitable.show |> String.concat ~sep:", "
      in
      Map.to_alist owner_to_awaitables
      |> List.map ~f:(fun (owner_for_awaitable, awaitables) ->
             Format.asprintf
               "%a -> {%s}"
               pp_owner_for_awaitable
               owner_for_awaitable
               (show_awaitables awaitables))
      |> String.concat ~sep:", "
    in
    Format.sprintf
      "Unawaited expressions: %s\nAwaitables for aliases: %s\nNeed to await: %b"
      awaitable_to_awaited_state
      owner_to_awaitables
      expect_expressions_to_be_awaited


  let pp format state = Format.fprintf format "%s" (show state)

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
  [@@deriving show]

  (* Dummy use to avoid unused-declaration error. *)
  let _ = show_forward_expression_result

  let result_state { state; nested_awaitable_expressions = _ } = state

  let bottom =
    {
      awaitable_to_awaited_state = Awaitable.Map.empty;
      owner_to_awaitables = OwnerMap.empty;
      expect_expressions_to_be_awaited = false;
    }


  let initial =
    {
      awaitable_to_awaited_state = Awaitable.Map.empty;
      owner_to_awaitables = OwnerMap.empty;
      expect_expressions_to_be_awaited = true;
    }


  let errors { awaitable_to_awaited_state; owner_to_awaitables; _ } =
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
      | NamedOwner name ->
          let add_reference errors awaitable =
            match Map.find errors awaitable with
            | Some { Error.references; expression } ->
                Map.set errors ~key:awaitable ~data:{ references = name :: references; expression }
            | None -> errors
          in
          Set.fold awaitables ~init:errors ~f:add_reference
      | AnonymousExpression _ -> errors
    in
    let error (awaitable, unawaited_awaitable) =
      Error.create
        ~location:
          (Awaitable.to_location awaitable
          |> Location.with_module ~module_reference:Context.qualifier)
        ~kind:(Error.UnawaitedAwaitable unawaited_awaitable)
        ~define:Context.define
    in
    Map.fold owner_to_awaitables ~init:errors ~f:add_reference |> Map.to_alist |> List.map ~f:error


  let less_or_equal ~left ~right =
    let less_or_equal_unawaited (reference, awaitable_state) =
      match awaitable_state, Map.find right.awaitable_to_awaited_state reference with
      | Unawaited _, Some _ -> true
      | Awaited, Some Awaited -> true
      | _ -> false
    in
    let less_or_equal_owner_to_awaitables (reference, awaitables) =
      match Map.find right.owner_to_awaitables reference with
      | Some other_awaitables -> Set.is_subset awaitables ~of_:other_awaitables
      | None -> false
    in
    Map.to_alist left.awaitable_to_awaited_state |> List.for_all ~f:less_or_equal_unawaited
    && Map.to_alist left.owner_to_awaitables |> List.for_all ~f:less_or_equal_owner_to_awaitables


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
      owner_to_awaitables =
        Map.merge_skewed
          left.owner_to_awaitables
          right.owner_to_awaitables
          ~combine:merge_awaitables;
      expect_expressions_to_be_awaited =
        left.expect_expressions_to_be_awaited || right.expect_expressions_to_be_awaited;
    }


  let widen ~previous ~next ~iteration:_ = join previous next

  let await_awaitable awaitable_to_awaited_state awaitable =
    Map.set awaitable_to_awaited_state ~key:awaitable ~data:Awaited


  let mark_name_as_awaited
      { awaitable_to_awaited_state; owner_to_awaitables; expect_expressions_to_be_awaited }
      ~name
    =
    if is_simple_name name then
      let awaitable_to_awaited_state =
        Map.find owner_to_awaitables (NamedOwner (name_to_reference_exn name))
        >>| (fun awaitables ->
              Set.fold awaitables ~init:awaitable_to_awaited_state ~f:await_awaitable)
        |> Option.value ~default:awaitable_to_awaited_state
      in
      { awaitable_to_awaited_state; owner_to_awaitables; expect_expressions_to_be_awaited }
    else (* Non-simple names cannot store awaitables. *)
      { awaitable_to_awaited_state; owner_to_awaitables; expect_expressions_to_be_awaited }


  (* Mark any awaitables covered by an expression as awaited, using the location of the expression.
     There are two ways we could find awaitables:

     - we could get a "direct" hit looking up this location as an originator key for an awaitable

     - or we could get an "indirect" hit by finding an AnonymousExpression owner which might track
     many awaitables. *)
  let mark_expression_at_location_as_awaited
      ({ awaitable_to_awaited_state; owner_to_awaitables; _ } as state)
      ~location
    =
    let location_as_awaitable, location_as_anonymous_owner =
      Awaitable.of_location location, AnonymousExpression { location }
    in
    if Map.mem awaitable_to_awaited_state location_as_awaitable then
      {
        state with
        awaitable_to_awaited_state =
          Map.set awaitable_to_awaited_state ~key:location_as_awaitable ~data:Awaited;
      }
    else
      match Map.find owner_to_awaitables location_as_anonymous_owner with
      | Some awaitables ->
          let awaitable_to_awaited_state =
            Set.fold awaitables ~init:awaitable_to_awaited_state ~f:await_awaitable
          in
          { state with awaitable_to_awaited_state }
      | None -> state


  (* Mark awaitable originating at an expression as awaited, but do not mark any owned awaitables
     originating elsewhere as awaited. *)
  let mark_awaitable_originating_at_expression_as_awaited
      ({ awaitable_to_awaited_state; _ } as state)
      ~expression:{ Ast.Node.location; _ }
    =
    let awaitable_to_awaited_state =
      Awaitable.of_location location
      |> Map.change awaitable_to_awaited_state ~f:(function
             | Some _ -> Some Awaited
             | None -> None)
    in
    { state with awaitable_to_awaited_state }


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


  let awaitables_pointed_to_by_expression
      ~state:{ awaitable_to_awaited_state; owner_to_awaitables; _ }
      { Node.value; location }
    =
    let awaitable = Awaitable.of_location location in
    if Map.mem awaitable_to_awaited_state awaitable then
      Some (Awaitable.Set.singleton awaitable)
    else
      match value with
      | Expression.Name name when is_simple_name name ->
          Map.find owner_to_awaitables (NamedOwner (name_to_reference_exn name))
      | _ -> Map.find owner_to_awaitables (AnonymousExpression { location })


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


  (* Handle the special semantics of subscript assignments / `__setitem__` calls. We assume that
     `__setitem__` behaves "normally":

     - it assigns some internal data on `base` using the key and value, making `base` an owner of
     any awaitable data coming from the key and value

     - the base, while it *can contain awaitables, will not itself *be* an awaitable (or if it does,
     that will typically be a type error so we can ignore it) be) returns None (so we don't need any
     special handling of the return value)

     - the base, while it cantain awaitable expressions, is not itself awaitable *)
  and forward_setitem ~resolution ~state ~base ~index ~value_expression =
    let { state; nested_awaitable_expressions = awaitable_expressions_from_base } =
      forward_expression ~resolution ~state ~expression:base
    in
    let { state; nested_awaitable_expressions = awaitable_expressions_from_index } =
      forward_expression ~resolution ~state ~expression:index
    in
    let { state; nested_awaitable_expressions = awaitable_expressions_from_value } =
      forward_expression ~resolution ~state ~expression:value_expression
    in
    let new_awaitable_expressions_referrable_from_base =
      awaitable_expressions_from_base
      @ awaitable_expressions_from_index
      @ awaitable_expressions_from_value
    in
    match Node.value base with
    | Expression.Name name
      when is_simple_name name && not (List.is_empty new_awaitable_expressions_referrable_from_base)
      ->
        let { owner_to_awaitables; _ } = state in
        let name = name_to_reference_exn name in
        let new_awaitables =
          new_awaitable_expressions_referrable_from_base
          |> List.map ~f:Node.location
          |> List.map ~f:Awaitable.of_location
          |> Awaitable.Set.of_list
        in
        let owner_to_awaitables =
          match Map.find owner_to_awaitables (NamedOwner name) with
          | Some nested_awaitable_expressions ->
              Map.set
                owner_to_awaitables
                ~key:(NamedOwner name)
                ~data:(Set.union nested_awaitable_expressions new_awaitables)
          | None -> Map.set owner_to_awaitables ~key:(NamedOwner name) ~data:new_awaitables
        in
        { state with owner_to_awaitables }
    | _ -> state


  and forward_call ~resolution ~state ~location ({ Call.callee; arguments; origin = _ } as call) =
    let forward_arguments
        { state; nested_awaitable_expressions = nested_awaitable_expressions_from_callee }
      =
      match Node.value callee, arguments with
      | _ ->
          let is_special_function =
            match Node.value callee with
            | Expression.Name (Name.Attribute { origin = Some origin; _ }) ->
                Origin.is_dunder_method origin
            | _ -> false
          in
          let forward_argument
              { state; nested_awaitable_expressions = nested_awaitable_expressions_so_far }
              { Call.Argument.value; _ }
            =
            (* For special methods such as `+`, ensure that we still require you to await the
               expressions. *)
            if is_special_function then
              forward_expression ~resolution ~state ~expression:value
              |>> nested_awaitable_expressions_so_far
            else
              let state = await_all_subexpressions ~state ~expression:value in
              { state; nested_awaitable_expressions = nested_awaitable_expressions_so_far }
          in
          let expect_expressions_to_be_awaited = state.expect_expressions_to_be_awaited in
          (* Don't introduce awaitables for the arguments of a call, as they will be consumed by the
             call anyway. *)
          let { state; nested_awaitable_expressions } =
            List.fold
              arguments
              ~init:
                {
                  state = { state with expect_expressions_to_be_awaited = is_special_function };
                  nested_awaitable_expressions = nested_awaitable_expressions_from_callee;
                }
              ~f:forward_argument
          in
          let state = { state with expect_expressions_to_be_awaited } in
          { state; nested_awaitable_expressions }
    in
    let {
      state =
        { awaitable_to_awaited_state; owner_to_awaitables; expect_expressions_to_be_awaited } as
        state;
      nested_awaitable_expressions = nested_awaitable_expressions_in_callee_and_arguments;
    }
      =
      forward_expression ~resolution ~state ~expression:callee |> forward_arguments
    in
    let call_expression = { Node.value = Expression.Call call; location } in
    let annotation = Resolution.resolve_expression_to_type resolution call_expression in
    if
      expect_expressions_to_be_awaited
      && can_lead_to_runtime_warning_if_unawaited
           ~global_resolution:(Resolution.global_resolution resolution)
           annotation
    then
      let nested_awaitable_expressions =
        call_expression :: nested_awaitable_expressions_in_callee_and_arguments
      in
      let awaitable_for_call = Awaitable.of_location location in
      match Node.value callee with
      | Name (Name.Attribute { base; _ }) -> (
          match
            ( awaitables_pointed_to_by_expression ~state base,
              Map.find awaitable_to_awaited_state awaitable_for_call )
          with
          | Some awaitables, Some (Unawaited _) ->
              (* The callee is an awaitable method on an unawaited `base`, so assume that the
                 returned value is the same awaitable. *)
              {
                state =
                  {
                    state with
                    owner_to_awaitables =
                      Map.set
                        owner_to_awaitables
                        ~key:(AnonymousExpression { location })
                        ~data:awaitables;
                  };
                nested_awaitable_expressions;
              }
          | _ ->
              {
                state =
                  {
                    state with
                    awaitable_to_awaited_state =
                      Map.set
                        awaitable_to_awaited_state
                        ~key:awaitable_for_call
                        ~data:(Unawaited call_expression);
                  };
                nested_awaitable_expressions;
              })
      | _ ->
          {
            state =
              {
                state with
                awaitable_to_awaited_state =
                  Map.set
                    awaitable_to_awaited_state
                    ~key:awaitable_for_call
                    ~data:(Unawaited call_expression);
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
          match awaitables_pointed_to_by_expression ~state base with
          | Some awaitables ->
              {
                state =
                  {
                    state with
                    owner_to_awaitables =
                      Map.set
                        owner_to_awaitables
                        ~key:(AnonymousExpression { location })
                        ~data:awaitables;
                  };
                nested_awaitable_expressions = nested_awaitable_expressions_in_callee_and_arguments;
              }
          | None ->
              {
                state;
                nested_awaitable_expressions = nested_awaitable_expressions_in_callee_and_arguments;
              })
      | _ ->
          {
            state;
            nested_awaitable_expressions = nested_awaitable_expressions_in_callee_and_arguments;
          }


  (** Add any awaitables introduced in the expression to the tracked awaitables in state. If any
      existing awaitables are awaited, mark them as awaited in the state.

      Return the new state along with nested awaitable expressions. *)
  and forward_expression ~resolution ~(state : t) ~expression:{ Node.value; location } =
    match value with
    | Await { Await.operand = { Node.value = Name name; _ } as expression; origin = _ }
      when is_simple_name name ->
        let { state; nested_awaitable_expressions } =
          forward_expression ~resolution ~state ~expression
        in
        { state = mark_name_as_awaited state ~name; nested_awaitable_expressions }
    | Await { Await.operand = { Node.location; _ } as expression; origin = _ } ->
        let { state; nested_awaitable_expressions } =
          forward_expression ~resolution ~state ~expression
        in
        let state =
          List.fold nested_awaitable_expressions ~init:state ~f:(fun state { Node.location; _ } ->
              mark_expression_at_location_as_awaited ~location state)
        in
        {
          state = mark_expression_at_location_as_awaited state ~location;
          nested_awaitable_expressions;
        }
    | BinaryOperator { BinaryOperator.left; right; _ }
    | BooleanOperator { BooleanOperator.left; right; _ }
    | ComparisonOperator { ComparisonOperator.left; right; _ } ->
        let { state; nested_awaitable_expressions } =
          forward_expression ~resolution ~state ~expression:left
        in
        forward_expression ~resolution ~state ~expression:right |>> nested_awaitable_expressions
    | Call
        {
          callee = { Node.value = Name (Name.Attribute { attribute = "__setitem__"; base; _ }); _ };
          arguments =
            [{ Call.Argument.value = index; _ }; { Call.Argument.value = value_expression; _ }];
          origin = _;
        } ->
        {
          state = forward_setitem ~resolution ~state ~base ~index ~value_expression;
          nested_awaitable_expressions = [];
        }
    | Slice slice ->
        let lowered = Slice.lower_to_expression ~location slice in
        forward_expression ~resolution ~state ~expression:lowered
    | Subscript { Subscript.base; index; origin = _ } ->
        let { state; nested_awaitable_expressions } =
          forward_expression ~resolution ~state ~expression:base
        in
        forward_expression ~resolution ~state ~expression:index |>> nested_awaitable_expressions
    | Call call -> forward_call ~resolution ~state ~location call
    | Dictionary entries ->
        let forward_entry { state; nested_awaitable_expressions } entry =
          let open Dictionary.Entry in
          match entry with
          | KeyValue { key; value } ->
              let { state; nested_awaitable_expressions } =
                forward_expression ~resolution ~state ~expression:key
                |>> nested_awaitable_expressions
              in
              forward_expression ~resolution ~state ~expression:value
              |>> nested_awaitable_expressions
          | Splat s ->
              forward_expression ~resolution ~state ~expression:s |>> nested_awaitable_expressions
        in
        List.fold entries ~init:{ state; nested_awaitable_expressions = [] } ~f:forward_entry
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
    | DictionaryComprehension { Comprehension.element = entry; generators } ->
        let { state; nested_awaitable_expressions } =
          List.fold
            generators
            ~init:{ state; nested_awaitable_expressions = [] }
            ~f:(forward_generator ~resolution)
        in
        let forward_entry
            { state; nested_awaitable_expressions }
            Dictionary.Entry.KeyValue.{ key; value }
          =
          let { state; nested_awaitable_expressions } =
            forward_expression ~resolution ~state ~expression:key |>> nested_awaitable_expressions
          in
          forward_expression ~resolution ~state ~expression:value |>> nested_awaitable_expressions
        in
        forward_entry { state; nested_awaitable_expressions } entry
    | Name (Name.Attribute { base; _ }) -> forward_expression ~resolution ~state ~expression:base
    | Name name when is_simple_name name ->
        let nested_awaitable_expressions =
          match Map.find state.owner_to_awaitables (NamedOwner (name_to_reference_exn name)) with
          | Some aliases ->
              let add_unawaited unawaited awaitable =
                match Map.find state.awaitable_to_awaited_state awaitable with
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
            | Substring.Format format -> begin
                let { state; nested_awaitable_expressions } =
                  forward_expression ~resolution ~state ~expression:format.value
                  |>> nested_awaitable_expressions
                in
                match format.format_spec with
                | Some f ->
                    forward_expression ~resolution ~state ~expression:f
                    |>> nested_awaitable_expressions
                | None -> { state; nested_awaitable_expressions }
              end
            | Substring.Literal _ -> { state; nested_awaitable_expressions })
    (* Base cases. *)
    | Constant _
    | Name _ ->
        { state; nested_awaitable_expressions = [] }


  and forward_assign
      ~resolution
      ~state:({ awaitable_to_awaited_state; owner_to_awaitables; _ } as state)
      ~annotation
      ~expression
      ~awaitable_expressions_in_value
      ~target
    =
    let open Expression in
    let is_nonuniform_sequence ~minimum_length annotation =
      match annotation with
      | Type.Tuple (Concrete arguments) when minimum_length <= List.length arguments -> true
      | _ -> false
    in
    let nonuniform_sequence_arguments annotation =
      match annotation with
      | Type.Tuple (Concrete arguments) -> arguments
      | _ -> []
    in
    match Node.value target with
    | Expression.Name (Identifier target_name) -> (
        match expression with
        | { Node.value = Expression.Name value; _ } when is_simple_name value ->
            (* Ownership: If `target` is awaited, then the awaitables pointed to by `value` will
               have been awaited. So, make `target` also point to them. *)
            let owner_to_awaitables =
              Map.find owner_to_awaitables (NamedOwner (name_to_reference_exn value))
              >>| (fun awaitables ->
                    Map.set
                      owner_to_awaitables
                      ~key:(NamedOwner (Reference.create target_name))
                      ~data:awaitables)
              |> Option.value ~default:owner_to_awaitables
            in
            { state with owner_to_awaitables }
        | _ ->
            let value_awaitable = Node.location expression |> Awaitable.of_location in
            let key = NamedOwner (Reference.create target_name) in
            let owner_to_awaitables =
              if not (List.is_empty awaitable_expressions_in_value) then
                (* If there are awaitables inside the value, then make `target` point to them.

                   For example, it is a list expression, `target = [awaitable(), awaitable2()]`,
                   `target` should point to the awaitables. *)
                let awaitables_in_value =
                  List.map awaitable_expressions_in_value ~f:Node.location
                  |> List.map ~f:Awaitable.of_location
                  |> Awaitable.Set.of_list
                in
                Map.set owner_to_awaitables ~key ~data:awaitables_in_value
              else if Map.mem awaitable_to_awaited_state value_awaitable then
                (* If the entire value being assigned is known to be an awaitable, make `target`
                   point to it. *)
                Map.set owner_to_awaitables ~key ~data:(Awaitable.Set.singleton value_awaitable)
              else
                owner_to_awaitables
            in
            { state with owner_to_awaitables })
    | Name (Attribute _) ->
        (* Unsoundly assume that any awaitable assigned to an attribute is being awaited somewhere.
           Otherwise, we risk getting many false positives for attribute assignments. *)
        mark_awaitable_originating_at_expression_as_awaited ~expression state
    | Subscript { Subscript.base; index; origin = _ } ->
        forward_setitem ~resolution ~state ~base ~index ~value_expression:expression
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
        | Tuple items -> (
            match List.zip (nonuniform_sequence_arguments annotation) items with
            | Ok annotations ->
                let tuple_values =
                  let left, tail = List.split_n annotations (List.length left) in
                  let starred, right = List.split_n tail (List.length tail - List.length right) in
                  let starred =
                    if not (List.is_empty starred) then
                      let annotation =
                        List.fold starred ~init:Type.Bottom ~f:(fun joined (annotation, _) ->
                            GlobalResolution.join
                              (Resolution.global_resolution resolution)
                              joined
                              annotation)
                        |> Type.list
                      in
                      [
                        ( annotation,
                          Node.create_with_default_location
                            (Expression.List (List.map starred ~f:snd)) );
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
            | Unequal_lengths ->
                (* TODO(T197284307) Model this case more carefully. Sometimes it is generated by
                   invalid code, but it can also occur when there is a Starred expression on the RHS
                   of a multi-target assignment. *)
                state)
        | _ ->
            (* Right now, if we don't have a concrete tuple to break down, we won't introduce new
               unawaited awaitables. *)
            state)
    | _ -> state


  let resolution_for_statement ~local_annotations ~legacy_parent ~statement_key resolution =
    let type_info_store =
      local_annotations
      >>= TypeInfo.ForFunctionBody.ReadOnly.get_precondition ~statement_key
      |> Option.value ~default:TypeInfo.Store.empty
    in
    resolution
    |> Resolution.with_type_info_store ~type_info_store
    |> Resolution.with_parent ~parent:legacy_parent


  let forward ~statement_key state ~statement:{ Node.value; location } =
    let { Node.value = { Define.signature = { Define.Signature.legacy_parent; _ }; _ }; _ } =
      Context.define
    in
    let resolution =
      resolution_for_statement
        ~local_annotations:Context.local_annotations
        ~legacy_parent
        ~statement_key
        Context.resolution
    in
    match value with
    | Statement.Assert { Assert.test; _ } ->
        forward_expression ~resolution ~state ~expression:test |> result_state
    | AugmentedAssign ({ target; _ } as augmented_assignment) ->
        let lowered_call =
          AugmentedAssign.lower_to_expression
            ~location
            ~callee_location:location
            augmented_assignment
        in
        let { state; nested_awaitable_expressions } =
          forward_expression ~resolution ~state ~expression:lowered_call
        in
        let annotation = Resolution.resolve_expression_to_type resolution lowered_call in
        forward_assign
          ~state
          ~resolution
          ~annotation
          ~expression:lowered_call
          ~awaitable_expressions_in_value:nested_awaitable_expressions
          ~target
    | Assign { value = Some value; target; _ } ->
        let { state; nested_awaitable_expressions } =
          forward_expression ~resolution ~state ~expression:value
        in
        let annotation = Resolution.resolve_expression_to_type resolution value in
        forward_assign
          ~state
          ~resolution
          ~annotation
          ~expression:value
          ~awaitable_expressions_in_value:nested_awaitable_expressions
          ~target
    | Assign { value = None; _ } -> state
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
    | TypeAlias _
    | Pass ->
        state


  let backward ~statement_key:_ _ ~statement:_ = failwith "Not implemented"
end

let check_define ~resolution ~local_annotations ~qualifier define =
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
           ~local_annotations:
             (local_annotations_for_define
                (Node.value define |> Define.name)
                (Node.location define))
           ~qualifier
           define)
  |> List.concat
