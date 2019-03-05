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


type breadcrumbs = Breadcrumb.t list
[@@deriving show, sexp]


type taint_annotation =
  | Sink of { sink: Sinks.t; breadcrumbs: breadcrumbs }
  | Source of { source: Sources.t; breadcrumbs: breadcrumbs }
  | Tito of { tito: Sinks.t; breadcrumbs: breadcrumbs }
  | SkipAnalysis  (* Don't analyze methods with SkipAnalysis *)
  | Sanitize      (* Don't propagate inferred model of methods with Sanitize *)
[@@deriving show, sexp]


let add_breadcrumbs breadcrumbs init =
  List.fold
    breadcrumbs
    ~f:(fun set breadcrumb -> SimpleFeatures.Breadcrumb breadcrumb :: set)
    ~init


let introduce_sink_taint
    ~root
    ({ TaintResult.backward = { sink_taint; _ }; _ } as taint)
    taint_sink_kind
    breadcrumbs =
  let backward =
    let assign_backward_taint environment taint =
      BackwardState.assign
        ~weak:true
        ~root
        ~path:[]
        taint
        environment
    in
    match taint_sink_kind with
    | Sinks.LocalReturn ->
        Or_error.errorf "Invalid TaintSink annotation `LocalReturn`"
    | _ ->
        let leaf_taint =
          BackwardTaint.singleton taint_sink_kind
          |> BackwardTaint.transform
            BackwardTaint.simple_feature_set
            ~f:(add_breadcrumbs breadcrumbs)
          |> BackwardState.Tree.create_leaf
        in
        let sink_taint = assign_backward_taint sink_taint leaf_taint in
        { taint.backward with sink_taint }
        |> Or_error.return
  in
  backward
  |> Or_error.map ~f:(fun backward -> { taint with backward })


let introduce_taint_in_taint_out
    ~root
    ({ TaintResult.backward = { taint_in_taint_out; _ }; _ } as taint)
    taint_sink_kind
    breadcrumbs =
  let backward =
    let assign_backward_taint environment taint =
      BackwardState.assign
        ~weak:true
        ~root
        ~path:[]
        taint
        environment
    in
    match taint_sink_kind with
    | Sinks.LocalReturn ->
        let return_taint =
          Domains.local_return_taint
          |> BackwardTaint.transform
            BackwardTaint.simple_feature_set
            ~f:(add_breadcrumbs breadcrumbs)
          |> BackwardState.Tree.create_leaf in
        let taint_in_taint_out = assign_backward_taint taint_in_taint_out return_taint in
        { taint.backward with taint_in_taint_out }
        |> Or_error.return
    | _ ->
        Or_error.errorf "Invalid TaintInTaintOut annotation `%s`" (Sinks.show taint_sink_kind)
  in
  backward
  |> Or_error.map ~f:(fun backward -> { taint with backward })


let introduce_source_taint
    ~root
    ({ TaintResult.forward = { source_taint }; _ } as taint)
    taint_source_kind
    breadcrumbs =
  let source_taint =
    let leaf_taint =
      ForwardTaint.singleton taint_source_kind
      |> ForwardTaint.transform ForwardTaint.simple_feature_set ~f:(add_breadcrumbs breadcrumbs)
      |> ForwardState.Tree.create_leaf
    in
    ForwardState.assign
      ~weak:true
      ~root
      ~path:[]
      leaf_taint
      source_taint
  in
  { taint with forward = { source_taint } }


let extract_identifier = function
  | Access.Identifier name -> Some name
  | _ -> None


type leaf_kind =
  | Leaf of string
  | Breadcrumbs of breadcrumbs


let rec extract_breadcrumbs expression =
  match expression.Node.value with
  | Access (SimpleAccess [Identifier breadcrumb]) ->
      [Breadcrumb.simple_via breadcrumb]
  | Tuple expressions ->
      List.concat_map ~f:extract_breadcrumbs expressions
  | _ ->
      []


let rec extract_kinds expression =
  match expression.Node.value with
  | Access (SimpleAccess [Identifier taint_kind]) ->
      [Leaf taint_kind]
  | Access (SimpleAccess (
      (Identifier "Via"
       :: _
       :: Call {
         value = { Argument.value = expression; _; } :: _; _ }
       :: _))) ->
      [Breadcrumbs (extract_breadcrumbs expression)]
  | Tuple expressions ->
      List.concat_map ~f:extract_kinds expressions
  | _ ->
      []


let extract_leafs expression =
  let kinds, breadcrumbs =
    extract_kinds expression
    |> List.partition_map ~f:(function Leaf l -> `Fst l | Breadcrumbs b -> `Snd b)
  in
  kinds, List.concat breadcrumbs


let get_source_kinds expression =
  let kinds, breadcrumbs = extract_leafs expression in
  List.map kinds ~f:(fun kind -> Source { source = Sources.create kind; breadcrumbs })


let get_sink_kinds expression =
  let kinds, breadcrumbs = extract_leafs expression in
  List.map kinds ~f:(fun kind -> Sink { sink = Sinks.create kind; breadcrumbs })


let get_taint_in_taint_out expression =
  let kinds, breadcrumbs = extract_leafs expression in
  match kinds with
  | [] ->
      [Tito { tito = Sinks.LocalReturn; breadcrumbs }]
  | _ ->
      List.map kinds ~f:(fun kind -> Tito { tito = Sinks.create kind; breadcrumbs })


let rec parse_annotations = function
  | Some {
      Node.value =
        Expression.Access
          (SimpleAccess access); _ } ->
      begin
        match access with
        | (Identifier "Union"
           :: _
           :: Call {
             value = { Argument.value = { value = Tuple expressions; _ }; _; } :: _; _ }
           :: _) ->
            List.map ~f:(fun expression -> parse_annotations (Some expression)) expressions
            |> Or_error.combine_errors
            |> Or_error.map ~f:List.concat
        | (Identifier "TaintSink"
           :: _
           :: Call {
             value = { Argument.value = expression; _; } :: _; _ }
           :: _) ->
            get_sink_kinds expression
            |> Or_error.return
        | (Identifier "TaintSource"
           :: _
           :: Call {
             value = { Argument.value = expression; _; } :: _; _ }
           :: _) ->
            get_source_kinds expression
            |> Or_error.return
        | [Identifier "TaintInTaintOut"] ->
            [Tito { tito = Sinks.LocalReturn; breadcrumbs = [] }]
            |> Or_error.return
        | (Identifier "TaintInTaintOut"
           :: _
           :: Call {
             value = { Argument.value = expression; _; } :: _; _ }
           :: _) ->
            get_taint_in_taint_out expression
            |> Or_error.return
        | [Identifier "SkipAnalysis"] ->
            [SkipAnalysis]
            |> Or_error.return
        | [Identifier "Sanitize"] ->
            [Sanitize]
            |> Or_error.return
        | [Identifier "Any"] ->  (* Ignore Any annotations *)
            []
            |> Or_error.return
        | _ ->
            Or_error.errorf "Unrecognized taint annotation `%s`" (Expression.Access.show access)
      end
  | None ->
      Or_error.return []
  | Some value ->
      Or_error.errorf "Unrecognized taint annotation `%s`" (Expression.show value)


let taint_parameter model (root, _name, annotation) =
  let add_to_model model annotation =
    model >>= fun model ->
    match annotation with
    | Sink { sink; breadcrumbs } ->
        introduce_sink_taint ~root model sink breadcrumbs
    | Source { source; breadcrumbs } ->
        introduce_source_taint ~root model source breadcrumbs
        |> Or_error.return
    | Tito { tito; breadcrumbs } ->
        introduce_taint_in_taint_out ~root model tito breadcrumbs
    | SkipAnalysis ->
        Or_error.error_string "SkipAnalysis annotation must be in return position"
    | Sanitize ->
        Or_error.error_string "Sanitize annotation must be in return position"
  in
  parse_annotations annotation
  |> Or_error.map ~f:(List.fold ~init:model ~f:add_to_model)
  |> Or_error.join


let taint_return model expression =
  let add_to_model model annotation =
    model >>= fun model ->
    let root = AccessPath.Root.LocalResult in
    match annotation with
    | Sink { sink;  breadcrumbs } ->
        introduce_sink_taint ~root model sink breadcrumbs
    | Source { source; breadcrumbs } ->
        introduce_source_taint ~root model source breadcrumbs
        |> Or_error.return
    | Tito _ ->
        Or_error.error_string "Invalid return annotation: TaintInTaintOut"
    | SkipAnalysis ->
        { model with mode = TaintResult.SkipAnalysis; }
        |> Or_error.return
    | Sanitize ->
        { model with mode = TaintResult.Sanitize; }
        |> Or_error.return
  in
  parse_annotations expression
  |> Or_error.map ~f:(List.fold ~init:model ~f:add_to_model)
  |> Or_error.join


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
          let name =
            match Node.value target with
            | Access (SimpleAccess access) ->
                access
            | _ ->
                failwith "Non-access name for define."
          in
          let define = {
            Define.name;
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
    |> Fn.flip taint_return return_annotation
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
        } when shell = "$parameter$shell" -> true
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
