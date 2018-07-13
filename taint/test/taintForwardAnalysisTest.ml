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

module Model = ForwardAnalysis.Model


type source_expectation = {
  define_name: string;
  returns: Sources.t list;
}


let models =
  let stub = "def taint() -> TaintSource[TestSource]: ..." in
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
  [ForwardAnalysis.Model.create defines resolution]


let get_model models name =
  List.find models ~f:(fun { Model.define_name; _ } -> Access.show define_name = name)


let assert_sources ~source ~models ~expect:{ define_name; returns; _ } =
  let { Node.value = define; _ } =
    parse source
    |> Preprocessing.preprocess
    |> Preprocessing.defines
    |> List.hd_exn
  in
  match ForwardAnalysis.run ~models (Cfg.create define) with
  | None -> assert_failure "produced no result"
  | Some { ForwardAnalysis.FixpointState.models; _ } ->
      match get_model models define_name with
      | None -> assert_failure ("no model for " ^ define_name)
      | Some model ->
          let returned_sources =
            ForwardState.read AccessPath.Root.LocalResult model.source_taint
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


let test_simple_source _ =
  assert_sources
    ~models
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
  assert_sources
    ~models
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


let test_model _ =
  let expect_source_taint =
    ForwardState.assign
      ~root:AccessPath.Root.LocalResult
      ~path:[]
      (ForwardTaint.singleton TestSource
       |> ForwardState.make_leaf)
      ForwardState.empty
  in
  assert_equal
    ~printer:Model.show
    (List.hd_exn models)
    {
      Model.define_name = Access.create "taint";
      source_taint = expect_source_taint;
    }


let () =
  "taint">:::[
    "simple">::test_simple_source;
    "copy">::test_local_copy;
    "model">::test_model;
  ]
  |> run_test_tt_main
