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


let parse_source ?(qualifier=[]) source =
  parse ~qualifier source
  |> Preprocessing.preprocess


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
          annotation = Type.Parametric { name; parameters = (Primitive primitive) :: _ }; _
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
let add_model ~stub =
  let source = parse ~qualifier:[] stub in
  let configuration = Test.configuration in
  let environment = Test.environment ~configuration () in
  Service.Environment.populate environment [source];
  let resolution = Test.resolution () in
  let defines =
    source
    |> Preprocessing.preprocess
    |> Preprocessing.defines ~include_stubs:true
  in
  let add_model_to_memory define model =
    let call_target = Callable.make define in
    Result.empty_model
    |> Result.with_model Taint.Result.kind model
    |> Fixpoint.add_predefined call_target
  in
  let models = List.map defines ~f:(fun define -> create_model define.value resolution) in
  List.iter2_exn defines models ~f:add_model_to_memory


let assert_sources ?qualifier ~source ~expect =
  let qualifier = Option.map qualifier ~f:Access.create in
  let source =
    parse ?qualifier source
    |> Preprocessing.preprocess
  in
  let configuration = Test.configuration in
  let environment = Test.environment ~configuration () in
  Service.Environment.populate environment [source];
  TypeCheck.check configuration environment source |> ignore;
  let defines =
    source
    |> Preprocessing.defines
    |> List.rev
  in
  let () =
    List.map ~f:Callable.make defines
    |> Fixpoint.KeySet.of_list
    |> Fixpoint.remove_new
  in
  let analyze_and_store_in_order define =
    let call_target = Callable.make define in
    let forward = ForwardAnalysis.run define.Node.value in
    let model = { Taint.Result.empty_model with forward } in
    Result.empty_model
    |> Result.with_model Taint.Result.kind model
    |> Fixpoint.add_predefined call_target;
  in
  let () = List.iter ~f:analyze_and_store_in_order defines in
  let check_expectation { define_name; returns } =
    let open Taint.Result in
    let expected_call_target = Callable.make_real (Access.create define_name) in
    let model =
      Fixpoint.get_model expected_call_target
      >>= Result.get_model Taint.Result.kind
    in
    match model with
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
  in
  List.iter ~f:check_expectation expect


let assert_model ~stub ~call_target ~expect_taint =
  let () = add_model ~stub in
  let call_target = Callable.make_real (Access.create call_target) in
  let taint_model = Fixpoint.get_model call_target >>= Result.get_model Taint.Result.kind in
  assert_equal
    ~printer:Taint.Result.show_call_model
    (Option.value_exn taint_model)
    {
      Taint.Result.empty_model with
      forward = { source_taint = expect_taint }
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

  let return_taint = expect_source_taint Taint.AccessPath.Root.LocalResult in
  assert_model
    ~stub:"def taint() -> TaintSource[TestSource]: ..."
    ~call_target:"taint"
    ~expect_taint:return_taint


let test_no_model _ =
  let assert_no_model _ =
    assert_sources
      ?qualifier:None
      ~source:
        {|
        def copy_source():
          pass
        |}
      ~expect:[
        {
          define_name = "does_not_exist";
          returns = [];
        }
      ]
  in
  assert_raises
    (OUnitTest.OUnit_failure "no model for does_not_exist")
    assert_no_model


let test_simple_source _ =
  add_model ~stub:"def taint() -> TaintSource[TestSource]: ...";
  assert_sources
    ?qualifier:None
    ~source:
      {|
      def simple_source():
        return taint()
      |}
    ~expect:[
      {
        define_name = "simple_source";
        returns = [Sources.TestSource];
      }
    ]


let test_local_copy _ =
  add_model ~stub:"def taint() -> TaintSource[TestSource]: ...";
  assert_sources
    ?qualifier:None
    ~source:
      {|
      def copy_source():
        var = taint()
        return var
      |}
    ~expect:[
      {
        define_name = "copy_source";
        returns = [Sources.TestSource];
      }
    ]


let test_class_model _ =
  add_model ~stub:"def taint() -> TaintSource[TestSource]: ...";
  assert_sources
    ~qualifier:"test"
    ~source:
      {|
        class Foo:
          def bar():
            return taint()
      |}
    ~expect:[
      {
        define_name = "test.Foo.bar";
        returns = [Sources.TestSource];
      }
    ]


let test_apply_method_model_at_call_site _ =
  add_model ~stub:"def taint() -> TaintSource[TestSource]: ...";
  assert_sources
    ~qualifier:"test"
    ~source:
      {|
        class Foo:
          def qux():
            return taint()

        class Bar:
          def qux():
            return not_tainted()

        def taint_across_methods():
          f = Foo()
          return f.qux()
      |}
    ~expect:[
      {
        define_name = "test.taint_across_methods";
        returns = [Sources.TestSource];
      }
    ];

  assert_sources
    ~qualifier:"test"
    ~source:
      {|
        class Foo:
          def qux():
            return taint()

        class Bar:
          def qux():
            return not_tainted()

        def taint_across_methods():
          f = Bar()
          return f.qux()
      |}
    ~expect:[
      {
        define_name = "test.taint_across_methods";
        returns = [];
      }
    ];

  assert_sources
    ~qualifier:"test"
    ~source:
      {|
        class Foo:
          def qux():
            return taint()

        class Bar:
          def qux():
            return not_tainted()

        def taint_across_methods(f: Foo):
          return f.qux()
      |}
    ~expect:[
      {
        define_name = "test.taint_across_methods";
        returns = [Sources.TestSource];
      }
    ];

  assert_sources
    ~qualifier:"test"
    ~source:
      {|
        class Foo:
          def qux():
            return taint()

        class Bar:
          def qux():
            return not_tainted()

        def taint_across_methods(f: Bar):
          return f.qux()
      |}
    ~expect:[
      {
        define_name = "test.taint_across_methods";
        returns = [];
      }
    ]


let () =
  "taint">:::[
    "no_model">::test_no_model;
    "models">::test_models;
    "simple">::test_simple_source;
    "copy">::test_local_copy;
    "class_model">::test_class_model;
    "test_apply_method_model_at_call_site">::test_apply_method_model_at_call_site;
  ]
  |> run_test_tt_main
