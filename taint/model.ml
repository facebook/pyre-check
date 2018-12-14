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
  is_obscure: bool;
  call_target: Callable.t;
  model: TaintResult.call_model;
}
[@@deriving show, sexp]


let introduce_sink_taint
    ~root
    ~taint_sink_kind
    ({ TaintResult.backward = { sink_taint; taint_in_taint_out }; _ } as taint) =
  let backward =
    let assign_backward_taint environment taint =
      BackwardState.assign
        ~root
        ~path:[]
        taint
        environment
    in
    match taint_sink_kind with
    | Sinks.LocalReturn ->
        let return_taint = Domains.local_return_taint |> BackwardState.Tree.create_leaf in
        let taint_in_taint_out = assign_backward_taint taint_in_taint_out return_taint in
        { taint.backward with taint_in_taint_out }
    | _ ->
        let leaf_taint =
          BackwardTaint.singleton taint_sink_kind
          |> BackwardState.Tree.create_leaf
        in
        let sink_taint = assign_backward_taint sink_taint leaf_taint in
        { taint.backward with sink_taint }
  in
  { taint with backward }


let introduce_source_taint taint_source_kind =
  let source_taint =
    ForwardState.assign
      ~root:AccessPath.Root.LocalResult
      ~path:[]
      (ForwardTaint.singleton taint_source_kind
       |> ForwardState.Tree.create_leaf)
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


let create ~resolution ?(verify = true) ~model_source () =
  let defines =
    let filter_define node =
      match node with
      | { Node.value = Define define; _ } ->
          let class_name = Access.prefix define.name in
          let class_candidate =
            Access.expression class_name
            |> Resolution.parse_annotation resolution
            |> Resolution.class_definition resolution
          in
          let call_target =
            match class_candidate with
            | Some _ -> Callable.create_method define.name
            | None -> Callable.create_function define.name
          in
          Some (define, call_target)
      | { Node.value = Assign { Assign.target; annotation = Some annotation; _ }; _ }
        when Expression.show annotation |> String.is_prefix ~prefix:"TaintSource[" ->
          let define = {
            Define.name = Expression.access target;
            parameters = [];
            body = [];
            decorators = [];
            docstring = None;
            return_annotation = Some annotation;
            async = false;
            parent = None;
          }
          in
          Some (define, Callable.create_object define.name)
      | _ ->
          None
    in
    String.split ~on:'\n' model_source
    |> Parser.parse
    |> List.filter_map ~f:filter_define
  in
  let create_model ({ Define.name; parameters; return_annotation; _ }, call_target) =
    (* Make sure we know about what we model. *)
    let call_target = (call_target :> Callable.t) in
    let annotation = Resolution.resolve resolution (Access.expression name) in
    if Type.equal annotation Type.Top then
      Format.asprintf "Modeled entity `%a` is not part of the environment!" Access.pp name
      |> failwith;

    (* Check model matches callables primary signature. *)
    begin
      match verify, annotation with
      | true,
        (Type.Callable {
            Type.Callable.implementation = {
              Type.Callable.parameters = Type.Callable.Defined implementation_parameters;
              _;
            };
            implicit;
            _;
          } as callable) ->
          let self_length = if Option.is_some implicit then 1 else 0 in
          if List.length parameters <> self_length + List.length implementation_parameters then
            let message =
              Format.asprintf
                "Model signature parameters for `%a` do not match implementation `%a`"
                Access.pp name
                Type.pp callable
            in
            Log.error "%s" message;
            failwith message
      | _ ->
          ()
    end;
    let normalized_parameters = AccessPath.Root.normalize_parameters parameters in
    List.fold ~init:(Ok TaintResult.empty_model) ~f:taint_parameter normalized_parameters
    >>= Fn.flip taint_return return_annotation
    >>= (fun model -> Ok { model; call_target; is_obscure = false })
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


let model_cache =
  String.Table.create ()


let get_callsite_model ~resolution ~call_target ~arguments =
  let open Pyre in
  let call_target = (call_target :> Callable.t) in
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
      match Hashtbl.find model_cache target with
      | Some model ->
          Some model
      | None ->
          let { model; _ } =
            let model_source =
              Format.asprintf
                "def %s(command: TaintSink[RemoteCodeExecution], shell): ..."
                target
            in
            create ~verify:false ~resolution ~model_source ()
            |> Or_error.ok_exn
            |> List.hd_exn
          in
          let result = {
            call_target;
            model;
            is_obscure = false;
          }
          in
          Hashtbl.set model_cache ~key:target ~data:result;
          Some result
    else
      None
  in

  match subprocess_model with
  | Some model ->
      model
  | None ->
      match Interprocedural.Fixpoint.get_model call_target with
      | None ->
          { is_obscure = true; call_target; model = TaintResult.empty_model }
      | Some model ->
          let strip_for_call_site model =
            model
          in
          let taint_model =
            Interprocedural.Result.get_model TaintResult.kind model
            |> Option.value ~default:TaintResult.empty_model
            |> strip_for_call_site
          in
          { is_obscure = model.is_obscure; call_target; model = taint_model }
