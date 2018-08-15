(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Or_error

open Ast
open Expression
open PyreParser
open Interprocedural
open Statement
open Domains
open TaintResult


type t = {
  call_target: Callable.t;
  model: TaintResult.call_model;
}
[@@deriving show, sexp]


let introduce_sink_taint
    ~root
    ~taint_sink_kind
    ({ TaintResult.backward = { sink_taint; taint_in_taint_out }; _ } as taint) =
  let backward =
    let assign_backward_taint taint =
      BackwardState.assign
        ~root
        ~path:[]
        (BackwardTaint.singleton taint_sink_kind
         |> BackwardState.make_leaf)
        taint
    in
    match taint_sink_kind with
    | Sinks.LocalReturn ->
        let taint_in_taint_out = assign_backward_taint taint_in_taint_out  in
        { taint.backward with taint_in_taint_out }
    | _ ->
        let sink_taint = assign_backward_taint sink_taint in
        { taint.backward with sink_taint }
  in
  { taint with backward }


let introduce_source_taint taint_source_kind =
  let source_taint =
    ForwardState.assign
      ~root:AccessPath.Root.LocalResult
      ~path:[]
      (ForwardTaint.singleton taint_source_kind
       |> ForwardState.make_leaf)
      ForwardState.empty
  in
  TaintResult.Forward.{ source_taint }


let taint_annotation = function
  | Some {
      Node.value =
        Expression.Access
          (Identifier taint_direction
           :: _
           :: Call {
             value = {
               Argument.value = {
                 value = Access (Identifier taint_kind :: _);
                 _;
               };
               _;
             }
               :: _;
             _ }
           :: _);
      _ } ->
      Some (Identifier.show taint_direction, Identifier.show taint_kind)
  | _ ->
      None


let taint_parameter position model expression =
  model >>= fun model ->
  match taint_annotation expression with
  | Some (taint_direction, taint_kind)
    when taint_direction = "TaintSink" || taint_direction = "TaintInTaintOut" ->
      let taint_sink_kind = Sinks.create taint_kind in
      let root = AccessPath.Root.Parameter { position } in
      introduce_sink_taint ~root ~taint_sink_kind model
      |> Or_error.return
  | Some (taint_direction, _) ->
      Or_error.errorf "Unrecognized taint direction in parameter annotation %s" taint_direction
  | _ ->
      Or_error.return model


let taint_return model expression =
  match taint_annotation expression with
  | Some (taint_direction, taint_kind) when taint_direction = "TaintSource" ->
      let taint_source_kind = Sources.create taint_kind in
      Or_error.return { model with forward = introduce_source_taint taint_source_kind }
  | Some (taint_direction, _) ->
      Or_error.errorf "Unrecognized taint direction in return annotation: %s" taint_direction
  | _ ->
      Or_error.return model


let create ~model_source =
  let defines =
    let filter_define = function
      | { Node.value = Define define; _ } -> Some define
      | _ -> None
    in
    (* TODO(T32372231) *)
    String.split ~on:'\n' model_source
    |> Parser.parse
    |> List.filter_map ~f:filter_define
  in
  let create_model { Define.name; parameters; return_annotation; _ } =
    let call_target = Callable.make_real name in
    List.map parameters ~f:(fun { Node.value; _ } -> value.annotation)
    |> List.foldi ~init:(Ok TaintResult.empty_model) ~f:taint_parameter
    >>= Fn.flip taint_return return_annotation
    >>= (fun model -> Ok { model; call_target })
  in
  match List.map defines ~f:create_model with
  | models -> Or_error.combine_errors models
  | exception Parser.Error message -> Or_error.errorf "Could not parse taint model: %s." message
  | exception Failure message -> Or_error.error_string message
