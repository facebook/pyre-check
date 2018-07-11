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


let parse_source source =
  parse source
  |> Preprocessing.preprocess


type source_expectation = {
  define_name: string;
  returns: Sources.t list;
}


let get_model models name =
  List.find models ~f:(fun { Model.define_name; _ } -> Access.show define_name = name)


let assert_sources ~source ~expect:{ define_name; returns; _ } =
  let { Node.value = define; _ } =
    parse_source source
    |> Preprocessing.defines
    |> List.hd_exn
  in
  match ForwardAnalysis.run (Cfg.create define) with
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
    ~source:
      {|
      def simple_source():
        return __testSource()
      |}
    ~expect:
      {
        define_name = "simple_source";
        returns = [Sources.TestSource];
      }


let test_local_copy _ =
  assert_sources
    ~source:
      {|
      def copy_source():
        var = __testSource()
        return var
      |}
    ~expect:
      {
        define_name = "copy_source";
        returns = [Sources.TestSource];
      }


let test_model _ =
  let assert_model ~qualifier ~taint_annotation ~source ~expect =
    let source = parse ~qualifier:(Access.create qualifier) source in
    let configuration = Configuration.create () in
    let environment = Environment.Builder.create () in
    Service.Environment.populate
      (Environment.handler ~configuration environment)
      [source];
    let environment = Environment.handler ~configuration environment in
    let resolution = Environment.resolution environment () in
    let defines =
      source
      |> Preprocessing.defines ~include_stubs:true
      |> List.tl
      |> Option.value ~default:[]
      |> List.map ~f:Node.value
    in
    let model = Model.create defines resolution in
    assert_equal
      ~printer:ident
      (Model.show model)
      (Model.show expect)
  in
  let expect_source_taint =
    ForwardState.assign
      ~root:AccessPath.Root.LocalResult
      ~path:[]
      (ForwardTaint.singleton TestSource
       |> ForwardState.make_leaf)
      ForwardState.empty
  in
  assert_model
    ~qualifier:"taint_model"
    ~taint_annotation:"TaintSource"
    ~source:
      {|
        def taint() -> TaintSource[TestSource]: ...
      |}
    ~expect:
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
