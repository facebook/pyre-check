(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Ast
open Pyre
open Statement
open Taint
open Domains

open Test
open Interprocedural


type source_expectation = {
  define_name: string;
  returns: Sources.t list;
}


let create_model define resolution =
  let taint_annotation = "TaintSource" in
  let introduce_taint taint_source_kind =
    let source_taint =
      ForwardState.assign
        ~root:Taint.AccessPath.Root.LocalResult
        ~path:[]
        (ForwardTaint.singleton taint_source_kind
         |> ForwardState.make_leaf)
        ForwardState.empty
    in
    Taint.Result.Forward.{ source_taint }
  in
  Annotated.Callable.create [define] ~resolution
  |> (fun callable -> Type.Callable callable)
  |> function
  | Type.Callable { kind = Named define_name ; overloads; implicit } ->
      begin match overloads with
        | {
          annotation = Type.Parametric { name; parameters = (Primitive primitive) :: _ }
        } :: _ when (Identifier.show name = taint_annotation) ->
            let taint_source_kind = Taint.Sources.create (Identifier.show primitive) in
            {
              Taint.Result.empty_model with
              forward = introduce_taint taint_source_kind;
            }
        | _ ->
            failwith "Cannot create taint model: no annotation"
      end
  | _ ->
      failwith "Cannot create taint model: not a callable"


(** Populates shared memory with existing models *)
let add_model stub =
  let source = parse ~qualifier:(Access.create "taint_model") stub in
  let configuration = Configuration.create () in
  let environment =
    let environment = Environment.Builder.create () in
    Service.Environment.populate
      (Environment.handler ~configuration environment)
      [source];
    Environment.handler ~configuration environment
  in
  let resolution = Environment.resolution environment () in
  let defines =
    source
    |> Preprocessing.defines ~include_stubs:true
    |> List.map ~f:Node.value
  in
  let add_model_to_memory { Define.name = define; _ } model =
    let call_target = `RealTarget define in
    Result.empty_model
    |> Result.with_model Taint.Result.kind model
    |> Fixpoint.add_predefined call_target
  in
  let models = List.map defines ~f:(fun define -> create_model define resolution) in
  List.iter2_exn defines models ~f:add_model_to_memory


let assert_sources ~source ~expect:{ define_name; returns; _ } =
  let { Node.value = define; _ } =
    parse source
    |> Preprocessing.preprocess
    |> Preprocessing.defines
    |> List.hd_exn
  in
  let call_target = `RealTarget (Access.create define_name) in
  let forward_model = ForwardAnalysis.run define in
  let taint_model = { Taint.Result.empty_model with forward = forward_model; } in
  let () =
    Result.empty_model
    |> Result.with_model Taint.Result.kind taint_model
    |> Fixpoint.add_predefined call_target
  in
  match Fixpoint.get_model call_target >>= Result.get_model Taint.Result.kind with
  | None -> assert_failure ("no model for " ^ define_name)
  | Some { forward = { source_taint; } ; _ } ->
      let returned_sources =
        ForwardState.read AccessPath.Root.LocalResult source_taint
        |> ForwardState.collapse
        |> ForwardTaint.leaves
        |> List.map ~f:Sources.show
        |> String.Set.of_list
      in
      let expected_sources =
        List.map ~f:Sources.show returns
        |> String.Set.of_list
      in
      assert_equal
        ~cmp:String.Set.equal
        ~printer:(fun set -> Sexp.to_string [%message (set: String.Set.t)])
        expected_sources
        returned_sources


let assert_model expect_source_taint stub =
  let () = add_model stub in
  let call_target = `RealTarget (Access.create "taint") in
  let taint_model = Fixpoint.get_model call_target >>= Result.get_model Taint.Result.kind in
  assert_equal
    ~printer:Taint.Result.show_call_model
    (Option.value_exn taint_model)
    {
      Taint.Result.empty_model with
      forward = { source_taint = expect_source_taint }
    }


let test_models _ =
  let expect_source_taint root =
    ForwardState.assign
      ~root
      ~path:[]
      (ForwardTaint.singleton TestSource
       |> ForwardState.make_leaf)
      ForwardState.empty
  in

  let stub = "def taint() -> TaintSource[TestSource]: ..." in
  let return_taint = expect_source_taint Taint.AccessPath.Root.LocalResult in
  assert_model return_taint stub


let test_simple_source _ =
  let stub = "def taint() -> TaintSource[TestSource]: ..." in
  add_model stub;
  assert_sources
    ~source:
      {|
      def simple_source():
        return taint()
      |}
    ~expect:
      {
        define_name = "simple_source";
        returns = [Sources.TestSource];
      }


let test_local_copy _ =
  let stub = "def taint() -> TaintSource[TestSource]: ..." in
  add_model stub;
  assert_sources
    ~source:
      {|
      def copy_source():
        var = taint()
        return var
      |}
    ~expect:
      {
        define_name = "copy_source";
        returns = [Sources.TestSource];
      }


let () =
  "taint">:::[
    "models">::test_models;
    "simple">::test_simple_source;
    "copy">::test_local_copy;
  ]
  |> run_test_tt_main
