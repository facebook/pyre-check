(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open PyreParser
open Interprocedural
open Statement
open TaintDomains


type t = {
  call_target: Callable.t;
  model: TaintResult.call_model;
}


let introduce_taint
    ~root
    ~taint_sink_kind
    ({ TaintResult.backward = { sink_taint; _ }; _ } as taint) =
  let sink_taint =
    BackwardState.assign
      ~root
      ~path:[]
      (BackwardTaint.singleton taint_sink_kind
       |> BackwardState.make_leaf)
      sink_taint
  in
  { taint with backward = { taint.backward with sink_taint } }


let taint_parameter position model = function
  | { Parameter.annotation =
        Some {
          Node.value =
            Expression.Access
              (Identifier taint_direction
               :: _
               :: Call {
                 value = {
                   Argument.value = {
                     value = Access (Identifier taint_sink_kind :: _);
                     _;
                   };
                   _;
                 }
                   :: _;
                 _ }
               :: _);
          _ };
      _ } ->
      let taint_sink_kind = TaintSinks.create (Identifier.show taint_sink_kind) in
      let root = TaintAccessPath.Root.Parameter { position } in
      introduce_taint ~root ~taint_sink_kind model
  | _ -> model


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
  let create_model { Define.name; parameters; _ } =
    let call_target = Callable.make_real name in
    List.map parameters ~f:(fun { Node.value; _ } -> value)
    |> List.foldi ~init:TaintResult.empty_model ~f:taint_parameter
    |> (fun model -> { model; call_target })
  in
  match List.map defines ~f:create_model with
  | models -> Ok models
  | exception Parser.Error message -> Or_error.errorf "Could not parse taint model: %s." message
  | exception Failure message -> Or_error.error_string message
