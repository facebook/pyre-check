(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Ast
open Statement
open Taint
open Domains

open Test


type source_expectation = {
  define_name: string;
  returns: Sources.t list;
}


let create_model define resolution =
  let taint_annotation = "TaintSource" in
  let introduce_taint taint_source_kind =
    ForwardState.assign
      ~root:Taint.AccessPath.Root.LocalResult
      ~path:[]
      (ForwardTaint.singleton taint_source_kind
       |> ForwardState.make_leaf)
      ForwardState.empty
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
              Taint.Model.forward = introduce_taint taint_source_kind;
              backward = BackwardState.empty;
              taint_in_taint_out = BackwardState.empty
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
    |> List.tl
    |> Option.value ~default:[]
    |> List.map ~f:Node.value
  in
  let add_model_to_memory { Define.name = define; _ } model =
    Taint.SharedMemory.add_model ~define model
  in
  let models = List.map defines ~f:(fun define -> create_model define resolution) in
  List.iter2_exn defines models ~f:add_model_to_memory


let assert_sources ~source ~expect:{ define_name; returns; _ } =
  let { Node.value = define; _ } =
    parse source
    |> Preprocessing.preprocess
    |> Preprocessing.defines
    |> List.tl_exn
    |> List.hd_exn
  in
  let { ForwardAnalysis.source_taint; _ } = Option.value_exn (ForwardAnalysis.run define) in
  Taint.SharedMemory.add_model
    ~define:(Access.create define_name)
    {
      forward = source_taint;
      backward = BackwardState.empty;
      taint_in_taint_out = BackwardState.empty
    };
  match Taint.SharedMemory.get_model ~define:(Access.create define_name) with
  | None -> assert_failure ("no model for " ^ define_name)
  | Some { forward; _ } ->
      let returned_sources =
        ForwardState.read AccessPath.Root.LocalResult forward
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
        ~printer:(Fn.compose Sexp.to_string String.Set.sexp_of_t)
        expected_sources
        returned_sources


let test_model _ =
  let expect_source_taint =
    ForwardState.assign
      ~root:AccessPath.Root.LocalResult
      ~path:[]
      (ForwardTaint.singleton TestSource
       |> ForwardState.make_leaf)
      ForwardState.empty
  in
  let stub = "def taint() -> TaintSource[TestSource]: ..." in
  add_model stub;
  assert_equal
    ~printer:Model.show
    (Option.value_exn (Taint.SharedMemory.get_model ~define:(Access.create "taint")))
    {
      forward = expect_source_taint;
      backward = BackwardState.empty;
      taint_in_taint_out = BackwardState.empty;
    }


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
    "model">::test_model;
    "simple">::test_simple_source;
    "copy">::test_local_copy;
  ]
  |> run_test_tt_main
