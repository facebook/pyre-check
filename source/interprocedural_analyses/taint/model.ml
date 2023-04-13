(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Model: represents the model of a given callable.
 *
 * A model contains all the information the global fixpoint needs about a given
 * callable, which is:
 * - The set of sources returned by the callable;
 * - The set of sinks reached by the parameters of the callable;
 * - Whether a parameter propagates its taint to the return value, which we call
 * taint-in-taint-out (tito).
 *
 * For instance, for the following callable `foo`:
 * ```
 * def foo(x, y, cond):
 *   if cond:
 *     x = user_controlled()
 *   sql(str(y))
 *   return x
 * ```
 *
 * The model of `foo` would be:
 * - sources returned: UserControlled
 * - sinks: y -> SQL
 * - tito: x -> LocalReturn
 *)

open Core
open Pyre
open Analysis
open Interprocedural
open Domains

let json_to_string ~indent json =
  let lines = Yojson.Safe.to_string json |> Yojson.Safe.prettify |> String.split ~on:'\n' in
  match lines with
  | [line] -> line
  | lines ->
      lines
      |> List.map ~f:(fun line -> indent ^ line)
      |> String.concat ~sep:"\n"
      |> fun content -> "\n" ^ content


module Forward = struct
  type t = { source_taint: ForwardState.t }

  let pp formatter { source_taint } =
    Format.fprintf
      formatter
      "  Sources: %s"
      (json_to_string
         ~indent:"    "
         (ForwardState.to_json
            ~expand_overrides:None
            ~is_valid_callee:(fun ~port:_ ~path:_ ~callee:_ -> true)
            ~filename_lookup:None
            ~export_leaf_names:ExportLeafNames.Always
            source_taint))


  let show = Format.asprintf "%a" pp

  let empty = { source_taint = ForwardState.empty }

  let is_empty { source_taint } = ForwardState.is_empty source_taint

  let obscure = empty

  let join { source_taint = left } { source_taint = right } =
    { source_taint = ForwardState.join left right }


  let widen ~iteration ~previous:{ source_taint = prev } ~next:{ source_taint = next } =
    { source_taint = ForwardState.widen ~iteration ~prev ~next }


  let less_or_equal ~left:{ source_taint = left } ~right:{ source_taint = right } =
    ForwardState.less_or_equal ~left ~right
end

module Backward = struct
  type t = {
    taint_in_taint_out: BackwardState.t;
    sink_taint: BackwardState.t;
  }

  let pp formatter { taint_in_taint_out; sink_taint } =
    Format.fprintf
      formatter
      "  Taint-in-taint-out: %s\n  Sinks: %s"
      (json_to_string
         ~indent:"    "
         (BackwardState.to_json
            ~expand_overrides:None
            ~is_valid_callee:(fun ~port:_ ~path:_ ~callee:_ -> true)
            ~filename_lookup:None
            ~export_leaf_names:ExportLeafNames.Always
            taint_in_taint_out))
      (json_to_string
         ~indent:"    "
         (BackwardState.to_json
            ~expand_overrides:None
            ~is_valid_callee:(fun ~port:_ ~path:_ ~callee:_ -> true)
            ~filename_lookup:None
            ~export_leaf_names:ExportLeafNames.Always
            sink_taint))


  let show = Format.asprintf "%a" pp

  let empty = { sink_taint = BackwardState.empty; taint_in_taint_out = BackwardState.empty }

  let is_empty { sink_taint; taint_in_taint_out } =
    BackwardState.is_empty sink_taint && BackwardState.is_empty taint_in_taint_out


  let obscure = empty

  let join
      { sink_taint = sink_taint_left; taint_in_taint_out = tito_left }
      { sink_taint = sink_taint_right; taint_in_taint_out = tito_right }
    =
    {
      sink_taint = BackwardState.join sink_taint_left sink_taint_right;
      taint_in_taint_out = BackwardState.join tito_left tito_right;
    }


  let widen
      ~iteration
      ~previous:{ sink_taint = sink_taint_previous; taint_in_taint_out = tito_previous }
      ~next:{ sink_taint = sink_taint_next; taint_in_taint_out = tito_next }
    =
    let sink_taint =
      BackwardState.widen ~iteration ~prev:sink_taint_previous ~next:sink_taint_next
    in
    let taint_in_taint_out = BackwardState.widen ~iteration ~prev:tito_previous ~next:tito_next in
    { sink_taint; taint_in_taint_out }


  let less_or_equal
      ~left:{ sink_taint = sink_taint_left; taint_in_taint_out = tito_left }
      ~right:{ sink_taint = sink_taint_right; taint_in_taint_out = tito_right }
    =
    BackwardState.less_or_equal ~left:sink_taint_left ~right:sink_taint_right
    && BackwardState.less_or_equal ~left:tito_left ~right:tito_right
end

module Sanitizers = struct
  type t = {
    (* `global` applies to all parameters and the return value.
     * For callables:
     * - sources are only sanitized in the forward trace;
     * - sinks are only sanitized in the backward trace;
     * - titos are sanitized in both traces (using sanitize taint transforms).
     * For attribute models, sanitizers are applied on attribute accesses, in both traces.
     *)
    global: Sanitize.t;
    (* Sanitizers applying to all parameters, in both traces. *)
    parameters: Sanitize.t;
    (* Map from parameter or return value to sanitizers applying in both traces. *)
    roots: Sanitize.RootMap.t;
  }

  let pp formatter { global; parameters; roots } =
    Format.fprintf
      formatter
      "  Global Sanitizer: %s\n  Parameters Sanitizer: %s\n  Sanitizers: %s"
      (json_to_string ~indent:"    " (Sanitize.to_json global))
      (json_to_string ~indent:"    " (Sanitize.to_json parameters))
      (json_to_string ~indent:"    " (Sanitize.RootMap.to_json roots))


  let show = Format.asprintf "%a" pp

  let empty =
    { global = Sanitize.empty; parameters = Sanitize.empty; roots = Sanitize.RootMap.bottom }


  let is_empty { global; parameters; roots } =
    Sanitize.is_empty global && Sanitize.is_empty parameters && Sanitize.RootMap.is_bottom roots


  let join
      { global = global_left; parameters = parameters_left; roots = roots_left }
      { global = global_right; parameters = parameters_right; roots = roots_right }
    =
    {
      global = Sanitize.join global_left global_right;
      parameters = Sanitize.join parameters_left parameters_right;
      roots = Sanitize.RootMap.join roots_left roots_right;
    }


  let widen ~iteration:_ ~previous ~next = join previous next

  let less_or_equal
      ~left:{ global = global_left; parameters = parameters_left; roots = roots_left }
      ~right:{ global = global_right; parameters = parameters_right; roots = roots_right }
    =
    Sanitize.less_or_equal ~left:global_left ~right:global_right
    && Sanitize.less_or_equal ~left:parameters_left ~right:parameters_right
    && Sanitize.RootMap.less_or_equal ~left:roots_left ~right:roots_right
end

module Mode = struct
  let name = "modes"

  type t =
    | Obscure
    | SkipAnalysis (* Don't analyze at all *)
    | SkipDecoratorWhenInlining
    | SkipOverrides
    | Entrypoint
    | IgnoreDecorator
  [@@deriving compare, equal]

  let pp formatter = function
    | Obscure -> Format.fprintf formatter "Obscure"
    | SkipAnalysis -> Format.fprintf formatter "SkipAnalysis"
    | SkipDecoratorWhenInlining -> Format.fprintf formatter "SkipDecoratorWhenInlining"
    | SkipOverrides -> Format.fprintf formatter "SkipOverrides"
    | Entrypoint -> Format.fprintf formatter "Entrypoint"
    | IgnoreDecorator -> Format.fprintf formatter "IgnoreDecorator"


  let show = Format.asprintf "%a" pp

  let to_json mode = `String (show mode)

  let from_string = function
    | "Obscure" -> Some Obscure
    | "SkipAnalysis" -> Some SkipAnalysis
    | "SkipDecoratorWhenInlining" -> Some SkipDecoratorWhenInlining
    | "SkipOverrides" -> Some SkipOverrides
    | "Entrypoint" -> Some Entrypoint
    | "IgnoreDecorator" -> Some IgnoreDecorator
    | _ -> None
end

module ModeSet = struct
  module T = Abstract.SetDomain.Make (Mode)
  include T

  let empty = T.bottom

  let is_empty = T.is_bottom

  let equal left right = T.less_or_equal ~left ~right && T.less_or_equal ~left:right ~right:left

  let to_json modes = `List (modes |> T.elements |> List.map ~f:Mode.to_json)

  let pp formatter modes =
    Format.fprintf formatter "  Modes: %s" (json_to_string ~indent:"    " (to_json modes))
end

type t = {
  forward: Forward.t;
  backward: Backward.t;
  sanitizers: Sanitizers.t;
  modes: ModeSet.t;
}

let pp formatter { forward; backward; sanitizers; modes } =
  Format.fprintf
    formatter
    "%a\n%a\n%a\n%a"
    Forward.pp
    forward
    Backward.pp
    backward
    Sanitizers.pp
    sanitizers
    ModeSet.pp
    modes


let show = Format.asprintf "%a" pp

let is_empty ~with_modes { forward; backward; sanitizers; modes } =
  Forward.is_empty forward
  && Backward.is_empty backward
  && Sanitizers.is_empty sanitizers
  && ModeSet.equal with_modes modes


let empty_model =
  {
    forward = Forward.empty;
    backward = Backward.empty;
    sanitizers = Sanitizers.empty;
    modes = ModeSet.empty;
  }


let empty_skip_model =
  {
    forward = Forward.empty;
    backward = Backward.empty;
    sanitizers = Sanitizers.empty;
    modes = ModeSet.singleton SkipAnalysis;
  }


let obscure_model =
  {
    forward = Forward.obscure;
    backward = Backward.obscure;
    sanitizers = Sanitizers.empty;
    modes = ModeSet.singleton Obscure;
  }


let is_obscure { modes; _ } = ModeSet.contains Obscure modes

let remove_obscureness ({ modes; _ } as model) = { model with modes = ModeSet.remove Obscure modes }

let remove_sinks model =
  { model with backward = { model.backward with sink_taint = BackwardState.empty } }


let add_obscure_sink ~resolution ~call_target model =
  let real_target =
    match call_target with
    | Target.Function _ -> Some call_target
    | Target.Method _ -> Some call_target
    | Target.Override method_name -> Some (Target.Method method_name)
    | Target.Object _ -> None
  in
  match real_target with
  | None -> model
  | Some real_target -> (
      match
        Target.get_module_and_definition
          ~resolution:(Resolution.global_resolution resolution)
          real_target
      with
      | None ->
          let () = Log.warning "Found no definition for %a" Target.pp_pretty real_target in
          model
      | Some (_, { value = { signature = { parameters; _ }; _ }; _ }) ->
          let open Domains in
          let sink =
            BackwardTaint.singleton CallInfo.declaration (Sinks.NamedSink "Obscure") Frame.initial
            |> BackwardState.Tree.create_leaf
          in
          let parameters = AccessPath.Root.normalize_parameters parameters in
          let add_parameter_sink sink_taint (root, _, _) =
            BackwardState.assign ~root ~path:[] sink sink_taint
          in
          let sink_taint =
            List.fold_left ~init:model.backward.sink_taint ~f:add_parameter_sink parameters
          in
          { model with backward = { model.backward with sink_taint } })


let join left right =
  {
    forward = Forward.join left.forward right.forward;
    backward = Backward.join left.backward right.backward;
    sanitizers = Sanitizers.join left.sanitizers right.sanitizers;
    modes = ModeSet.join left.modes right.modes;
  }


let widen ~iteration ~previous ~next =
  {
    forward = Forward.widen ~iteration ~previous:previous.forward ~next:next.forward;
    backward = Backward.widen ~iteration ~previous:previous.backward ~next:next.backward;
    sanitizers = Sanitizers.widen ~iteration ~previous:previous.sanitizers ~next:next.sanitizers;
    modes = ModeSet.widen ~iteration ~prev:previous.modes ~next:next.modes;
  }


let less_or_equal ~left ~right =
  Forward.less_or_equal ~left:left.forward ~right:right.forward
  && Backward.less_or_equal ~left:left.backward ~right:right.backward
  && Sanitizers.less_or_equal ~left:left.sanitizers ~right:right.sanitizers
  && ModeSet.less_or_equal ~left:left.modes ~right:right.modes


let strip_for_callsite
    { forward = { source_taint }; backward = { sink_taint; taint_in_taint_out }; sanitizers; modes }
  =
  (* Remove positions and other info that are not needed at call site *)
  let source_taint =
    source_taint
    |> ForwardState.transform Features.TitoPositionSet.Self Map ~f:(fun _ ->
           Features.TitoPositionSet.bottom)
    |> ForwardState.transform ForwardTaint.call_info Map ~f:CallInfo.strip_for_callsite
  in
  let sink_taint =
    sink_taint
    |> BackwardState.transform Features.TitoPositionSet.Self Map ~f:(fun _ ->
           Features.TitoPositionSet.bottom)
    |> BackwardState.transform BackwardTaint.call_info Map ~f:CallInfo.strip_for_callsite
  in
  let taint_in_taint_out =
    taint_in_taint_out
    |> BackwardState.transform Features.TitoPositionSet.Self Map ~f:(fun _ ->
           Features.TitoPositionSet.bottom)
    |> BackwardState.transform BackwardTaint.call_info Map ~f:CallInfo.strip_for_callsite
  in
  { forward = { source_taint }; backward = { sink_taint; taint_in_taint_out }; sanitizers; modes }


let apply_sanitizers
    ~taint_configuration
    {
      forward = { source_taint };
      backward = { taint_in_taint_out; sink_taint };
      sanitizers = { global; parameters; roots } as sanitizers;
      modes;
    }
  =
  (* Apply the global sanitizer. *)
  (* Here, we are applying the legacy behavior of sanitizers, where we only
   * sanitize the forward trace or the backward trace. *)
  let source_taint =
    (* @SanitizeSingleTrace(TaintSource[...]) *)
    ForwardState.apply_sanitizers
      ~taint_configuration
      ~sanitize_source:true
      ~sanitizer:global
      source_taint
  in
  let taint_in_taint_out =
    (* @SanitizeSingleTrace(TaintInTaintOut[...]) *)
    BackwardState.apply_sanitizers
      ~taint_configuration
      ~sanitize_tito:true
      ~insert_location:TaintTransformOperation.InsertLocation.Back
      ~sanitizer:global
      taint_in_taint_out
  in
  let sink_taint =
    (* @SanitizeSingleTrace(TaintSink[...]) *)
    BackwardState.apply_sanitizers
      ~taint_configuration
      ~sanitize_sink:true
      ~sanitizer:global
      sink_taint
  in

  (* Apply the parameters sanitizer. *)
  (* Here, we apply sanitizers both in the forward and backward trace. *)
  (* Note that by design, sanitizing a specific source or sink also sanitizes
   * taint-in-taint-out for that source/sink. *)
  let sink_taint =
    (* Sanitize(Parameters[TaintSource[...]]) *)
    BackwardState.apply_sanitizers
      ~taint_configuration
      ~sanitize_source:true
      ~ignore_if_sanitize_all:true
      ~sanitizer:parameters
      sink_taint
  in
  let taint_in_taint_out =
    (* Sanitize(Parameters[TaintSource[...]]) *)
    BackwardState.apply_sanitizers
      ~taint_configuration
      ~sanitize_source:true
      ~ignore_if_sanitize_all:true
      ~sanitizer:parameters
      taint_in_taint_out
  in
  let taint_in_taint_out =
    (* Sanitize(Parameters[TaintInTaintOut[...]]) *)
    BackwardState.apply_sanitizers
      ~taint_configuration
      ~sanitize_tito:true
      ~insert_location:TaintTransformOperation.InsertLocation.Back
      ~sanitizer:parameters
      taint_in_taint_out
  in
  let sink_taint =
    (* Sanitize(Parameters[TaintSink[...]]) *)
    BackwardState.apply_sanitizers
      ~taint_configuration
      ~sanitize_sink:true
      ~sanitizer:parameters
      sink_taint
  in
  let taint_in_taint_out =
    (* Sanitize(Parameters[TaintSink[...]]) *)
    BackwardState.apply_sanitizers
      ~taint_configuration
      ~sanitize_sink:true
      ~ignore_if_sanitize_all:true
      ~sanitizer:parameters
      taint_in_taint_out
  in

  (* Apply the return sanitizer. *)
  let sanitize_return sanitizer (source_taint, taint_in_taint_out, sink_taint) =
    let source_taint =
      (* def foo() -> Sanitize[TaintSource[...]] *)
      ForwardState.apply_sanitizers
        ~taint_configuration
        ~sanitize_source:true
        ~sanitizer
        source_taint
    in
    let taint_in_taint_out =
      (* def foo() -> Sanitize[TaintSource[...]] *)
      BackwardState.apply_sanitizers
        ~taint_configuration
        ~sanitize_source:true
        ~ignore_if_sanitize_all:true
        ~sanitizer
        taint_in_taint_out
    in
    let taint_in_taint_out =
      (* def foo() -> Sanitize[TaintInTaintOut[...]] *)
      BackwardState.apply_sanitizers
        ~taint_configuration
        ~sanitize_tito:true
        ~insert_location:TaintTransformOperation.InsertLocation.Back
        ~sanitizer
        taint_in_taint_out
    in
    let source_taint =
      (* def foo() -> Sanitize[TaintSink[...]] *)
      ForwardState.apply_sanitizers
        ~taint_configuration
        ~sanitize_sink:true
        ~ignore_if_sanitize_all:true
        ~sanitizer
        source_taint
    in
    let taint_in_taint_out =
      (* def foo() -> Sanitize[TaintSink[...]] *)
      BackwardState.apply_sanitizers
        ~taint_configuration
        ~sanitize_sink:true
        ~ignore_if_sanitize_all:true
        ~sanitizer
        taint_in_taint_out
    in
    source_taint, taint_in_taint_out, sink_taint
  in

  (* Apply the parameter-specific sanitizers. *)
  let sanitize_parameter (parameter, sanitizer) (source_taint, taint_in_taint_out, sink_taint) =
    let sink_taint =
      (* def foo(x: Sanitize[TaintSource[...]]): ... *)
      BackwardState.apply_sanitizers
        ~taint_configuration
        ~sanitize_source:true
        ~ignore_if_sanitize_all:true
        ~parameter
        ~sanitizer
        sink_taint
    in
    let taint_in_taint_out =
      (* def foo(x: Sanitize[TaintSource[...]]): ... *)
      BackwardState.apply_sanitizers
        ~taint_configuration
        ~sanitize_source:true
        ~ignore_if_sanitize_all:true
        ~parameter
        ~sanitizer
        taint_in_taint_out
    in
    let taint_in_taint_out =
      (* def foo(x: Sanitize[TaintInTaintOut[...]]): ... *)
      BackwardState.apply_sanitizers
        ~taint_configuration
        ~sanitize_tito:true
        ~insert_location:TaintTransformOperation.InsertLocation.Back
        ~parameter
        ~sanitizer
        taint_in_taint_out
    in
    let sink_taint =
      (* def foo(x: Sanitize[TaintSink[...]]): ... *)
      BackwardState.apply_sanitizers
        ~taint_configuration
        ~sanitize_sink:true
        ~parameter
        ~sanitizer
        sink_taint
    in
    let taint_in_taint_out =
      (* def foo(x: Sanitize[TaintSink[...]]): ... *)
      BackwardState.apply_sanitizers
        ~taint_configuration
        ~sanitize_sink:true
        ~ignore_if_sanitize_all:true
        ~parameter
        ~sanitizer
        taint_in_taint_out
    in
    source_taint, taint_in_taint_out, sink_taint
  in

  let sanitize_root (root, sanitizer) (source_taint, taint_in_taint_out, sink_taint) =
    match root with
    | AccessPath.Root.LocalResult ->
        sanitize_return sanitizer (source_taint, taint_in_taint_out, sink_taint)
    | PositionalParameter _
    | NamedParameter _
    | StarParameter _
    | StarStarParameter _ ->
        sanitize_parameter (root, sanitizer) (source_taint, taint_in_taint_out, sink_taint)
    | Variable _ -> failwith "unexpected"
  in
  let source_taint, taint_in_taint_out, sink_taint =
    Sanitize.RootMap.fold
      Sanitize.RootMap.KeyValue
      ~f:sanitize_root
      ~init:(source_taint, taint_in_taint_out, sink_taint)
      roots
  in
  { forward = { source_taint }; backward = { sink_taint; taint_in_taint_out }; sanitizers; modes }


let should_externalize { forward; backward; sanitizers; _ } =
  (not (Forward.is_empty forward))
  || (not (Backward.is_empty backward))
  || not (Sanitizers.is_empty sanitizers)


(* For every frame, convert the may breadcrumbs into must breadcrumbs. *)
let may_breadcrumbs_to_must
    { forward = { source_taint }; backward = { taint_in_taint_out; sink_taint }; sanitizers; modes }
  =
  let source_taint = ForwardState.may_breadcrumbs_to_must source_taint in
  let taint_in_taint_out = BackwardState.may_breadcrumbs_to_must taint_in_taint_out in
  let sink_taint = BackwardState.may_breadcrumbs_to_must sink_taint in
  { forward = { source_taint }; backward = { taint_in_taint_out; sink_taint }; sanitizers; modes }


(* Within every local taint, join every frame with the frame in the same local taint of the `Attach`
   kind. *)
let join_every_frame_with_attach
    { forward = { source_taint }; backward = { taint_in_taint_out; sink_taint }; sanitizers; modes }
  =
  let source_taint = ForwardState.join_every_frame_with source_taint ~frame_kind:Sources.Attach in
  let taint_in_taint_out =
    BackwardState.join_every_frame_with taint_in_taint_out ~frame_kind:Sinks.Attach
  in
  let sink_taint = BackwardState.join_every_frame_with sink_taint ~frame_kind:Sinks.Attach in
  { forward = { source_taint }; backward = { taint_in_taint_out; sink_taint }; sanitizers; modes }


(* A special case of join, only used for user-provided models. *)
let join_user_models ({ modes = left_modes; _ } as left) ({ modes = right_modes; _ } as right) =
  let update_obscure_mode ({ modes; _ } as model) =
    (* If one model has @SkipObscure and the other does not, we expect the joined model to also have
       @SkipObscure *)
    if (not (ModeSet.contains Obscure left_modes)) || not (ModeSet.contains Obscure right_modes)
    then
      { model with modes = ModeSet.subtract (ModeSet.singleton Obscure) ~from:modes }
    else
      model
  in
  join left right |> update_obscure_mode |> join_every_frame_with_attach |> may_breadcrumbs_to_must


let to_json
    ~expand_overrides
    ~is_valid_callee
    ~filename_lookup
    ~export_leaf_names
    callable
    {
      forward = { source_taint };
      backward = { sink_taint; taint_in_taint_out };
      sanitizers =
        { global = global_sanitizer; parameters = parameters_sanitizer; roots = root_sanitizers };
      modes;
    }
  =
  let callable_name = Target.external_name callable in
  let model_json = ["callable", `String callable_name] in
  let model_json =
    if not (ForwardState.is_empty source_taint) then
      model_json
      @ [
          ( "sources",
            ForwardState.to_json
              ~expand_overrides
              ~is_valid_callee
              ~filename_lookup
              ~export_leaf_names
              source_taint );
        ]
    else
      model_json
  in
  let model_json =
    if not (BackwardState.is_empty sink_taint) then
      model_json
      @ [
          ( "sinks",
            BackwardState.to_json
              ~expand_overrides
              ~is_valid_callee
              ~filename_lookup
              ~export_leaf_names
              sink_taint );
        ]
    else
      model_json
  in
  let model_json =
    if not (BackwardState.is_empty taint_in_taint_out) then
      model_json
      @ [
          ( "tito",
            BackwardState.to_json
              ~expand_overrides
              ~is_valid_callee
              ~filename_lookup
              ~export_leaf_names
              taint_in_taint_out );
        ]
    else
      model_json
  in
  let model_json =
    if not (Sanitize.is_empty global_sanitizer) then
      model_json @ ["global_sanitizer", Sanitize.to_json global_sanitizer]
    else
      model_json
  in
  let model_json =
    if not (Sanitize.is_empty parameters_sanitizer) then
      model_json @ ["parameters_sanitizer", Sanitize.to_json parameters_sanitizer]
    else
      model_json
  in
  let model_json =
    if not (Sanitize.RootMap.is_bottom root_sanitizers) then
      model_json @ ["sanitizers", Sanitize.RootMap.to_json root_sanitizers]
    else
      model_json
  in
  let model_json =
    if not (ModeSet.is_empty modes) then
      model_json @ ["modes", ModeSet.to_json modes]
    else
      model_json
  in
  `Assoc ["kind", `String "model"; "data", `Assoc model_json]


module WithTarget = struct
  type nonrec t = {
    model: t;
    target: Target.t;
  }
end

module WithCallTarget = struct
  type nonrec t = {
    model: t;
    call_target: CallGraph.CallTarget.t;
  }
end
