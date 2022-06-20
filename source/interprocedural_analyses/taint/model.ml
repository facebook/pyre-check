(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
            source_taint))


  let show = Format.asprintf "%a" pp

  let empty = { source_taint = ForwardState.empty }

  let is_empty { source_taint } = ForwardState.is_empty source_taint

  let obscure = empty

  let join { source_taint = left } { source_taint = right } =
    { source_taint = ForwardState.join left right }


  let widen ~iteration ~previous:{ source_taint = prev } ~next:{ source_taint = next } =
    { source_taint = ForwardState.widen ~iteration ~prev ~next }


  let reached_fixpoint
      ~iteration:_
      ~previous:{ source_taint = previous }
      ~next:{ source_taint = next }
    =
    ForwardState.less_or_equal ~left:next ~right:previous
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
            taint_in_taint_out))
      (json_to_string
         ~indent:"    "
         (BackwardState.to_json
            ~expand_overrides:None
            ~is_valid_callee:(fun ~port:_ ~path:_ ~callee:_ -> true)
            ~filename_lookup:None
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


  let reached_fixpoint
      ~iteration:_
      ~previous:{ sink_taint = sink_taint_previous; taint_in_taint_out = tito_previous }
      ~next:{ sink_taint = sink_taint_next; taint_in_taint_out = tito_next }
    =
    BackwardState.less_or_equal ~left:sink_taint_next ~right:sink_taint_previous
    && BackwardState.less_or_equal ~left:tito_next ~right:tito_previous
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
    roots: SanitizeRootMap.t;
  }

  let pp formatter { global; parameters; roots } =
    Format.fprintf
      formatter
      "  Global Sanitizer: %s\n  Parameters Sanitizer: %s\n  Sanitizers: %s"
      (json_to_string ~indent:"    " (Sanitize.to_json global))
      (json_to_string ~indent:"    " (Sanitize.to_json parameters))
      (json_to_string ~indent:"    " (SanitizeRootMap.to_json roots))


  let show = Format.asprintf "%a" pp

  let empty =
    { global = Sanitize.empty; parameters = Sanitize.empty; roots = SanitizeRootMap.bottom }


  let is_empty { global; parameters; roots } =
    Sanitize.is_empty global && Sanitize.is_empty parameters && SanitizeRootMap.is_bottom roots


  let join
      { global = global_left; parameters = parameters_left; roots = roots_left }
      { global = global_right; parameters = parameters_right; roots = roots_right }
    =
    {
      global = Sanitize.join global_left global_right;
      parameters = Sanitize.join parameters_left parameters_right;
      roots = SanitizeRootMap.join roots_left roots_right;
    }


  let widen ~iteration:_ ~previous ~next = join previous next

  let reached_fixpoint
      ~iteration:_
      ~previous:
        { global = global_previous; parameters = parameters_previous; roots = roots_previous }
      ~next:{ global = global_next; parameters = parameters_next; roots = roots_next }
    =
    Sanitize.less_or_equal ~left:global_next ~right:global_previous
    && Sanitize.less_or_equal ~left:parameters_next ~right:parameters_previous
    && SanitizeRootMap.less_or_equal ~left:roots_next ~right:roots_previous
end

module Mode = struct
  let name = "modes"

  type t =
    | Obscure
    | SkipAnalysis (* Don't analyze at all *)
    | SkipDecoratorWhenInlining
    | SkipOverrides
  [@@deriving compare]

  let pp formatter = function
    | Obscure -> Format.fprintf formatter "Obscure"
    | SkipAnalysis -> Format.fprintf formatter "SkipAnalysis"
    | SkipDecoratorWhenInlining -> Format.fprintf formatter "SkipDecoratorWhenInlining"
    | SkipOverrides -> Format.fprintf formatter "SkipOverrides"


  let show = Format.asprintf "%a" pp

  let to_json mode = `String (show mode)
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
            BackwardTaint.singleton (Sinks.NamedSink "Obscure") Frame.initial
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


let reached_fixpoint ~iteration ~previous ~next =
  Forward.reached_fixpoint ~iteration ~previous:previous.forward ~next:next.forward
  && Backward.reached_fixpoint ~iteration ~previous:previous.backward ~next:next.backward
  && Sanitizers.reached_fixpoint ~iteration ~previous:previous.sanitizers ~next:next.sanitizers
  && ModeSet.less_or_equal ~left:next.modes ~right:previous.modes


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
    {
      forward = { source_taint };
      backward = { taint_in_taint_out; sink_taint };
      sanitizers = { global; parameters; roots } as sanitizers;
      modes;
    }
  =
  let kinds_to_sanitize_transforms ~sources ~sinks =
    let sources = Sources.Set.to_sanitize_transforms_exn sources in
    let sinks = Sinks.Set.to_sanitize_transforms_exn sinks in
    { SanitizeTransformSet.sources; sinks }
  in
  let sanitize_tito ?(sources = Sources.Set.empty) ?(sinks = Sinks.Set.empty) taint_in_taint_out =
    let transforms = kinds_to_sanitize_transforms ~sources ~sinks in
    BackwardState.apply_sanitize_transforms transforms taint_in_taint_out
  in
  let sanitize_tito_parameter
      parameter
      ?(sources = Sources.Set.empty)
      ?(sinks = Sinks.Set.empty)
      taint_in_taint_out
    =
    let sanitize_tito_taint_tree = function
      | None -> BackwardState.Tree.bottom
      | Some taint_tree ->
          let transforms = kinds_to_sanitize_transforms ~sources ~sinks in
          BackwardState.Tree.apply_sanitize_transforms transforms taint_tree
    in
    BackwardState.update taint_in_taint_out parameter ~f:sanitize_tito_taint_tree
  in

  (* Apply the global sanitizer. *)
  (* Here, we are applying the legacy behavior of sanitizers, where we only
   * sanitize the forward trace or the backward trace. *)
  let source_taint =
    (* @Sanitize(TaintSource[...]) *)
    match global.sources with
    | Some All -> ForwardState.empty
    | Some (Specific sanitized_sources) -> ForwardState.sanitize sanitized_sources source_taint
    | None -> source_taint
  in
  let taint_in_taint_out =
    (* @Sanitize(TaintInTaintOut[...]) *)
    match global.tito with
    | Some All -> BackwardState.empty
    | Some (Specific { sanitized_tito_sources; sanitized_tito_sinks }) ->
        sanitize_tito ~sources:sanitized_tito_sources ~sinks:sanitized_tito_sinks taint_in_taint_out
    | None -> taint_in_taint_out
  in
  let sink_taint =
    (* @Sanitize(TaintSink[...]) *)
    match global.sinks with
    | Some All -> BackwardState.empty
    | Some (Specific sanitized_sinks) -> BackwardState.sanitize sanitized_sinks sink_taint
    | None -> sink_taint
  in

  (* Apply the parameters sanitizer. *)
  (* Here, we apply sanitizers both in the forward and backward trace. *)
  (* Note that by design, sanitizing a specific source or sink also sanitizes
   * taint-in-taint-out for that source/sink. *)
  let sink_taint, taint_in_taint_out =
    (* Sanitize(Parameters[TaintSource[...]]) *)
    match parameters.sources with
    | Some All -> sink_taint, taint_in_taint_out
    | Some (Specific sanitized_sources) ->
        let sanitized_sources_transforms =
          Sources.Set.to_sanitize_transforms_exn sanitized_sources
          |> SanitizeTransformSet.from_sources
        in
        let sink_taint =
          sink_taint
          |> BackwardState.apply_sanitize_transforms sanitized_sources_transforms
          |> BackwardState.transform BackwardTaint.kind Filter ~f:Issue.sink_can_match_rule
        in
        let taint_in_taint_out = sanitize_tito ~sources:sanitized_sources taint_in_taint_out in
        sink_taint, taint_in_taint_out
    | None -> sink_taint, taint_in_taint_out
  in
  let taint_in_taint_out =
    (* Sanitize(Parameters[TaintInTaintOut[...]]) *)
    match parameters.tito with
    | Some All -> BackwardState.empty
    | Some (Specific { sanitized_tito_sources; sanitized_tito_sinks }) ->
        sanitize_tito ~sources:sanitized_tito_sources ~sinks:sanitized_tito_sinks taint_in_taint_out
    | _ -> taint_in_taint_out
  in
  let sink_taint, taint_in_taint_out =
    (* Sanitize(Parameters[TaintSink[...]]) *)
    match parameters.sinks with
    | Some All ->
        let sink_taint = BackwardState.empty in
        sink_taint, taint_in_taint_out
    | Some (Specific sanitized_sinks) ->
        let sink_taint = BackwardState.sanitize sanitized_sinks sink_taint in
        let taint_in_taint_out = sanitize_tito ~sinks:sanitized_sinks taint_in_taint_out in
        sink_taint, taint_in_taint_out
    | None -> sink_taint, taint_in_taint_out
  in

  (* Apply the return sanitizer. *)
  let sanitize_return sanitize (source_taint, taint_in_taint_out, sink_taint) =
    let root = AccessPath.Root.LocalResult in
    let source_taint, taint_in_taint_out =
      (* def foo() -> Sanitize[TaintSource[...]] *)
      match sanitize.Sanitize.sources with
      | Some All ->
          let source_taint = ForwardState.remove root source_taint in
          source_taint, taint_in_taint_out
      | Some (Specific sanitized_sources) ->
          let filter_sources = function
            | None -> ForwardState.Tree.bottom
            | Some taint_tree -> ForwardState.Tree.sanitize sanitized_sources taint_tree
          in
          let source_taint = ForwardState.update source_taint root ~f:filter_sources in
          let taint_in_taint_out = sanitize_tito ~sources:sanitized_sources taint_in_taint_out in
          source_taint, taint_in_taint_out
      | None -> source_taint, taint_in_taint_out
    in
    let taint_in_taint_out =
      (* def foo() -> Sanitize[TaintInTaintOut[...]] *)
      match sanitize.Sanitize.tito with
      | Some All -> BackwardState.remove root taint_in_taint_out
      | Some (Specific { sanitized_tito_sources; sanitized_tito_sinks }) ->
          sanitize_tito
            ~sources:sanitized_tito_sources
            ~sinks:sanitized_tito_sinks
            taint_in_taint_out
      | _ -> taint_in_taint_out
    in
    let source_taint, taint_in_taint_out =
      (* def foo() -> Sanitize[TaintSink[...]] *)
      match sanitize.Sanitize.sinks with
      | Some All -> source_taint, taint_in_taint_out
      | Some (Specific sanitized_sinks) ->
          let sanitized_sinks_transforms =
            Sinks.Set.to_sanitize_transforms_exn sanitized_sinks |> SanitizeTransformSet.from_sinks
          in
          let source_taint =
            source_taint
            |> ForwardState.apply_sanitize_transforms sanitized_sinks_transforms
            |> ForwardState.transform ForwardTaint.kind Filter ~f:Issue.source_can_match_rule
          in
          let taint_in_taint_out = sanitize_tito ~sinks:sanitized_sinks taint_in_taint_out in
          source_taint, taint_in_taint_out
      | None -> source_taint, taint_in_taint_out
    in
    source_taint, taint_in_taint_out, sink_taint
  in

  (* Apply the parameter-specific sanitizers. *)
  let sanitize_parameter (parameter, sanitize) (source_taint, taint_in_taint_out, sink_taint) =
    let sink_taint, taint_in_taint_out =
      (* def foo(x: Sanitize[TaintSource[...]]): ... *)
      match sanitize.Sanitize.sources with
      | Some All -> sink_taint, taint_in_taint_out
      | Some (Specific sanitized_sources) ->
          let apply_taint_transforms = function
            | None -> BackwardState.Tree.bottom
            | Some taint_tree ->
                let sanitized_sources_transforms =
                  Sources.Set.to_sanitize_transforms_exn sanitized_sources
                  |> SanitizeTransformSet.from_sources
                in
                taint_tree
                |> BackwardState.Tree.apply_sanitize_transforms sanitized_sources_transforms
                |> BackwardState.Tree.transform
                     BackwardTaint.kind
                     Filter
                     ~f:Issue.sink_can_match_rule
          in
          let sink_taint = BackwardState.update sink_taint parameter ~f:apply_taint_transforms in
          let taint_in_taint_out =
            sanitize_tito_parameter parameter ~sources:sanitized_sources taint_in_taint_out
          in
          sink_taint, taint_in_taint_out
      | None -> sink_taint, taint_in_taint_out
    in
    let taint_in_taint_out =
      (* def foo(x: Sanitize[TaintInTaintOut[...]]): ... *)
      match sanitize.Sanitize.tito with
      | Some All -> BackwardState.remove parameter taint_in_taint_out
      | Some (Specific { sanitized_tito_sources; sanitized_tito_sinks }) ->
          sanitize_tito_parameter
            parameter
            ~sources:sanitized_tito_sources
            ~sinks:sanitized_tito_sinks
            taint_in_taint_out
      | None -> taint_in_taint_out
    in
    let sink_taint, taint_in_taint_out =
      (* def foo(x: Sanitize[TaintSink[...]]): ... *)
      match sanitize.Sanitize.sinks with
      | Some All ->
          let sink_taint = BackwardState.remove parameter sink_taint in
          sink_taint, taint_in_taint_out
      | Some (Specific sanitized_sinks) ->
          let filter_sinks = function
            | None -> BackwardState.Tree.bottom
            | Some taint_tree -> BackwardState.Tree.sanitize sanitized_sinks taint_tree
          in
          let sink_taint = BackwardState.update sink_taint parameter ~f:filter_sinks in
          let taint_in_taint_out =
            sanitize_tito_parameter parameter ~sinks:sanitized_sinks taint_in_taint_out
          in
          sink_taint, taint_in_taint_out
      | None -> sink_taint, taint_in_taint_out
    in
    source_taint, taint_in_taint_out, sink_taint
  in

  let sanitize_root (root, sanitize) (source_taint, taint_in_taint_out, sink_taint) =
    match root with
    | AccessPath.Root.LocalResult ->
        sanitize_return sanitize (source_taint, taint_in_taint_out, sink_taint)
    | PositionalParameter _
    | NamedParameter _
    | StarParameter _
    | StarStarParameter _ ->
        sanitize_parameter (root, sanitize) (source_taint, taint_in_taint_out, sink_taint)
    | Variable _ -> failwith "unexpected"
  in
  let source_taint, taint_in_taint_out, sink_taint =
    SanitizeRootMap.fold
      SanitizeRootMap.KeyValue
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


let join_user_models left right =
  join left right |> join_every_frame_with_attach |> may_breadcrumbs_to_must


let to_json
    ~expand_overrides
    ~is_valid_callee
    ~filename_lookup
    callable
    {
      forward = { source_taint };
      backward = { sink_taint; taint_in_taint_out };
      sanitizers =
        { global = global_sanitizer; parameters = parameters_sanitizer; roots = root_sanitizers };
      modes;
    }
  =
  let callable_name = Interprocedural.Target.external_name callable in
  let model_json = ["callable", `String callable_name] in
  let model_json =
    if not (ForwardState.is_empty source_taint) then
      model_json
      @ [
          ( "sources",
            ForwardState.to_json ~expand_overrides ~is_valid_callee ~filename_lookup source_taint );
        ]
    else
      model_json
  in
  let model_json =
    if not (BackwardState.is_empty sink_taint) then
      model_json
      @ [
          ( "sinks",
            BackwardState.to_json ~expand_overrides ~is_valid_callee ~filename_lookup sink_taint );
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
    if not (SanitizeRootMap.is_bottom root_sanitizers) then
      model_json @ ["sanitizers", SanitizeRootMap.to_json root_sanitizers]
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
