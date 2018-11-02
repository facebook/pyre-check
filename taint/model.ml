(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Or_error

open Ast
open Analysis
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
         |> BackwardState.create_leaf)
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
       |> ForwardState.create_leaf)
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


let taint_parameter model (root, _name, annotation) =
  model >>= fun model ->
  match taint_annotation annotation with
  | Some (taint_direction, taint_kind)
    when taint_direction = "TaintSink" || taint_direction = "TaintInTaintOut" ->
      let taint_sink_kind = Sinks.create taint_kind in
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


let create ~resolution ~model_source =
  let defines =
    let filter_define = function
      | { Node.value = Define define; _ } ->
          Some define
      | { Node.value = Assign { Assign.target; annotation = Some annotation; _ }; _ }
        when Expression.show annotation |> String.is_prefix ~prefix:"TaintSource[" ->
          Some {
            Define.name = Expression.access target;
            parameters = [];
            body = [];
            decorators = [];
            docstring = None;
            return_annotation = Some annotation;
            async = false;
            generated = false;
            parent = None;
          }
      | _ ->
          None
    in
    String.split ~on:'\n' model_source
    |> Parser.parse
    |> List.filter_map ~f:filter_define
  in
  let create_model { Define.name; parameters; return_annotation; _ } =
    (* Make sure we know about what we model. *)
    let annotation = Resolution.resolve resolution (Access.expression name) in
    if Type.equal annotation Type.Top then
      Format.asprintf "Modeled entity `%a` is not part of the environment!" Access.pp name
      |> failwith;

    let call_target = Callable.create_real name in
    let normalized_parameters = AccessPath.Root.normalize_parameters parameters in
    List.fold ~init:(Ok TaintResult.empty_model) ~f:taint_parameter normalized_parameters
    >>= Fn.flip taint_return return_annotation
    >>= (fun model -> Ok { model; call_target })
  in
  match List.map defines ~f:create_model with
  | models -> Or_error.combine_errors models
  | exception Parser.Error message -> Or_error.errorf "Could not parse taint model: %s." message
  | exception Failure message -> Or_error.error_string message


let subprocess_calls =
  String.Set.of_list [
    "subprocess.run";
    "subprocess.call";
    "subprocess.check_call";
    "subprocess.check_output";
  ]


let get_callsite_model ~resolution ~call_target ~arguments =
  let open Pyre in
  let subprocess_model =
    let shell_set_to_true ~arguments =
      let shell_set_to_true argument =
        match argument with
        | {
          Argument.name = Some { Node.value = shell; _ };
          value = { Node.value = True; _ };
        } when Identifier.show shell = "$parameter$shell" -> true
        | _ -> false
      in
      List.exists arguments ~f:shell_set_to_true
    in
    let called_with_list =
      let is_list_argument { Argument.name; value } =
        let annotation = Resolution.resolve resolution value in
        Option.is_none name &&
        not (Type.equal annotation Type.Bottom) &&
        not (Type.equal annotation Type.string) &&
        Resolution.less_or_equal
          resolution
          ~left:annotation
          ~right:(Type.list Type.string)

      in
      List.hd arguments
      >>| is_list_argument
      |> (Option.value ~default:true)
    in
    let target = Callable.external_target_name call_target in
    if (not called_with_list) &&
       String.Set.mem subprocess_calls target &&
       shell_set_to_true ~arguments then
      let { model; _ } =
        let model_source =
          Format.asprintf
            "def %s(command: TaintSink[RemoteCodeExecution], shell): ..."
            target
        in
        create ~resolution ~model_source
        |> Or_error.ok_exn
        |> List.hd_exn
      in
      Result.empty_model
      |> Result.with_model TaintResult.kind model
      |> Option.some
    else
      None
  in

  if Option.is_some subprocess_model then
    subprocess_model
  else
    Interprocedural.Fixpoint.get_model call_target
