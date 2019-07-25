(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Domains
open Pyre

let json_to_string json =
  Yojson.Safe.to_string json
  |> Yojson.Safe.prettify
  |> String.split ~on:'\n'
  |> List.map ~f:(fun line -> "      " ^ line)
  |> String.concat ~sep:"\n"


module Forward = struct
  type model = { source_taint: ForwardState.t } [@@deriving sexp]

  let pp_model formatter { source_taint } =
    Format.fprintf
      formatter
      "    Sources:\n%s"
      (json_to_string (ForwardState.to_json source_taint))


  let show_model = Format.asprintf "%a" pp_model

  let empty = { source_taint = ForwardState.empty }

  let is_empty { source_taint } = ForwardState.is_empty source_taint

  let obscure = empty

  let join { source_taint = left } { source_taint = right } =
    { source_taint = ForwardState.join left right }


  let widen ~iteration ~previous:{ source_taint = previous } ~next:{ source_taint = next } =
    { source_taint = ForwardState.widen ~iteration ~previous ~next }


  let reached_fixpoint
      ~iteration:_
      ~previous:{ source_taint = previous }
      ~next:{ source_taint = next }
    =
    ForwardState.less_or_equal ~left:next ~right:previous


  let to_json { source_taint } = ForwardState.to_json source_taint
end

module Backward = struct
  type model = {
    taint_in_taint_out: BackwardState.t;
    sink_taint: BackwardState.t;
  }
  [@@deriving sexp]

  let pp_model formatter { taint_in_taint_out; sink_taint } =
    Format.fprintf
      formatter
      "    Taint-in-taint-out:\n%s\n    Sinks:\n%s"
      (json_to_string (BackwardState.to_json taint_in_taint_out))
      (json_to_string (BackwardState.to_json sink_taint))


  let show_model = Format.asprintf "%a" pp_model

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
      BackwardState.widen ~iteration ~previous:sink_taint_previous ~next:sink_taint_next
    in
    let taint_in_taint_out =
      BackwardState.widen ~iteration ~previous:tito_previous ~next:tito_next
    in
    { sink_taint; taint_in_taint_out }


  let reached_fixpoint
      ~iteration:_
      ~previous:{ sink_taint = sink_taint_previous; taint_in_taint_out = tito_previous }
      ~next:{ sink_taint = sink_taint_next; taint_in_taint_out = tito_next }
    =
    BackwardState.less_or_equal ~left:sink_taint_next ~right:sink_taint_previous
    && BackwardState.less_or_equal ~left:tito_next ~right:tito_previous


  let to_json_sinks { sink_taint; _ } = BackwardState.to_json sink_taint

  let to_json_tito { taint_in_taint_out; _ } = BackwardState.to_json taint_in_taint_out
end

type mode =
  | SkipAnalysis (* Don't analyze at all *)
  | Sanitize (* Analyze, but throw away inferred model *)
  | Normal
[@@deriving sexp, show]

type call_model = {
  forward: Forward.model;
  backward: Backward.model;
  mode: mode;
}
[@@deriving sexp]

let pp_call_model formatter { forward; backward; mode } =
  Format.fprintf
    formatter
    "  Forward:\n%a\n  Backward:\n%a\n  Mode:%a"
    Forward.pp_model
    forward
    Backward.pp_model
    backward
    pp_mode
    mode


let show_call_model = Format.asprintf "%a" pp_call_model

type result = Flow.issue list

let empty_skip_model = { forward = Forward.empty; backward = Backward.empty; mode = SkipAnalysis }

module ResultArgument = struct
  let name = "taint"

  type nonrec result = result

  type nonrec call_model = call_model

  let show_call_model = show_call_model

  let pp_call_model = pp_call_model

  let obscure_model = { forward = Forward.obscure; backward = Backward.obscure; mode = Normal }

  let empty_model = { forward = Forward.empty; backward = Backward.empty; mode = Normal }

  let is_empty { forward; backward; _ } = Forward.is_empty forward && Backward.is_empty backward

  let join_modes left right =
    match left, right with
    | SkipAnalysis, _ -> SkipAnalysis
    | _, SkipAnalysis -> SkipAnalysis
    | Sanitize, _ -> Sanitize
    | _, Sanitize -> Sanitize
    | Normal, Normal -> Normal


  let join ~iteration:_ left right =
    {
      forward = Forward.join left.forward right.forward;
      backward = Backward.join left.backward right.backward;
      mode = join_modes left.mode right.mode;
    }


  let widen ~iteration ~previous ~next =
    {
      forward = Forward.widen ~iteration ~previous:previous.forward ~next:next.forward;
      backward = Backward.widen ~iteration ~previous:previous.backward ~next:next.backward;
      mode = join_modes previous.mode next.mode;
    }


  let get_errors result = List.map ~f:Flow.generate_error result

  let issues_to_json callable result =
    match result with
    | None -> []
    | Some issues ->
        let issue_to_json issue =
          let json = Flow.to_json callable issue in
          `Assoc ["kind", `String "issue"; "data", json]
        in
        List.map ~f:issue_to_json issues


  let model_to_json callable model =
    let callable_name = Interprocedural.Callable.external_target_name callable in
    let model_json =
      `Assoc
        [ "callable", `String callable_name;
          "sources", Forward.to_json model.forward;
          "sinks", Backward.to_json_sinks model.backward;
          "tito", Backward.to_json_tito model.backward ]
    in
    `Assoc ["kind", `String "model"; "data", model_json]


  let reached_fixpoint ~iteration ~previous ~next =
    Forward.reached_fixpoint ~iteration ~previous:previous.forward ~next:next.forward
    && Backward.reached_fixpoint ~iteration ~previous:previous.backward ~next:next.backward


  (* Emit both issues and models for external processing *)
  let externalize callable result model =
    let issues = issues_to_json callable result in
    if is_empty model then
      issues
    else
      model_to_json callable model :: issues


  let metadata () =
    let codes = Flow.code_metadata () in
    `Assoc ["codes", codes]
end

include Interprocedural.Result.Make (ResultArgument)

(* Patch the forward reference to access the final summaries in trace info generation. *)
let has_significant_summary root path target =
  let model =
    Interprocedural.Fixpoint.get_model target >>= Interprocedural.Result.get_model kind
  in
  match model with
  | None -> false
  | Some { forward; backward; _ } -> (
    match root with
    | AccessPath.Root.LocalResult ->
        let tree = ForwardState.read ~root ~path forward.source_taint in
        not (ForwardState.Tree.is_empty tree)
    | _ ->
        let tree = BackwardState.read ~root ~path backward.sink_taint in
        not (BackwardState.Tree.is_empty tree) )


let () = TraceInfo.has_significant_summary := has_significant_summary
