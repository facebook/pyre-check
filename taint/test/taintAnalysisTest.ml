(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Ast
open Expression
open Pyre
open Taint

open Interprocedural
open TestHelper


type expect_fixpoint = {
  expect: expectation list;
  iterations: int;
}


let create_call_graph ?(test_file = "test_file") source =
  let handle = File.Handle.create test_file in
  let source =
    Test.parse source
    |> Preprocessing.preprocess
  in
  let () =
    Preprocessing.defines source
    |> List.map ~f:Callable.create
    |> Fixpoint.KeySet.of_list
    |> Fixpoint.remove_new
  in
  let () = Ast.SharedMemory.Sources.remove ~handles:[handle] in
  let () = Ast.SharedMemory.Sources.add handle source in
  let environment = Test.environment () in
  Service.Environment.populate environment [source];
  let configuration = Configuration.Analysis.create () in
  TypeCheck.check configuration environment source |> ignore;
  let call_graph =
    Service.StaticAnalysis.record_and_merge_call_graph
      ~environment
      ~call_graph:CallGraph.empty
      ~path:handle
      ~source
  in
  let () =
    Service.StaticAnalysis.record_overrides ~environment ~source in
  let callables =
    Service.StaticAnalysis.record_path_of_definitions ~path:handle ~source
    |> List.map ~f:Callable.create
  in
  call_graph, callables


let assert_fixpoint ~source ~expect:{ iterations = expect_iterations; expect } =
  let scheduler = Scheduler.mock () in
  let call_graph, all_callables = create_call_graph source in
  let caller_map = CallGraph.reverse call_graph in
  let analyses = [Taint.Analysis.abstract_kind] in
  let configuration = Configuration.Analysis.create () in
  let environment =
    let source =
      Test.parse source
      |> Preprocessing.preprocess
    in
    let environment = Test.environment () in
    Service.Environment.populate environment [source];
    environment
  in
  let iterations =
    Analysis.compute_fixpoint
      ~configuration
      ~scheduler
      ~environment
      ~analyses
      ~caller_map
      ~all_callables
      Fixpoint.Epoch.initial
  in
  let get_model call_target =
    Fixpoint.get_model call_target
    >>= Result.get_model Taint.Result.kind
  in
  let read_analysis_result { define_name; _ } =
    let call_target = Callable.create_real (Access.create define_name) in
    Fixpoint.get_result call_target
    |> Result.get_result Taint.Result.kind
    >>| List.map ~f:Flow.generate_error
    >>| (fun result -> define_name, result)
  in
  let assert_error define { code; pattern } error =
    if code <> Error.code error then
      Format.sprintf "Expected error code %d for %s, but got %d"
        code
        define
        (Error.code error)
      |> assert_failure;
    let error_string = Error.description ~detailed:true error in
    let regexp = Str.regexp pattern in
    if not (Str.string_match regexp error_string 0) then
      Format.sprintf "Expected error for %s to match %s, but got %s"
        define
        pattern
        error_string
      |> assert_failure
  in
  let assert_errors (define1, error_patterns) (define2, errors) =
    if define1 <> define2 then
      Format.sprintf "Expected errors for %s, but found %s"
        define1
        define2
      |> assert_failure;
    assert_equal
      (List.length error_patterns)
      (List.length errors)
      ~msg:(Format.sprintf "Number of errors for %s" define1)
      ~printer:Int.to_string;
    List.iter2_exn ~f:(assert_error define1) error_patterns errors
  in
  let results = List.filter_map expect ~f:read_analysis_result in
  let expect_results =
    let create_result_patterns { define_name; errors; _ } = define_name, errors in
    List.map expect ~f:create_result_patterns
  in
  assert_bool "Callgraph is empty!" (Access.Map.length call_graph > 0);
  assert_equal expect_iterations iterations ~printer:Int.to_string;
  List.iter ~f:(check_expectation ~get_model) expect;
  List.iter2_exn expect_results results ~f:assert_errors


let test_fixpoint _ =
  assert_fixpoint
    ~source:
      {|
      def bar():
        return __testSource()

      def qux(arg):
        __testSink(arg)

      def bad(arg):
        qux(arg)

      def some_source():
        return bar()

      def match_flows():
        x = some_source()
        bad(x)

      def match_flows_multiple(arg):
        if arg:
          x = some_source()
        else:
          x = some_source()
        bad(x)

      def rce_problem():
        x = __userControlled()
        __eval(x)

      class TestMethods:
        def method_source(self):
          return some_source()

        def method_sink(self, tainted):
          bad(tainted)

        def receiver_sink(self, not_tainted):
          bad(self.taint)

      def match_via_methods():
        x = TestMethods()
        taint = x.method_source()
        x.method_sink(taint)

      def no_match_via_methods():
        x = TestMethods()
        taint = x.method_source()
        x.taint = taint
        x.method_sink(5)

      def match_via_receiver():
        x = TestMethods()
        taint = x.method_source()
        x.taint = taint
        x.receiver_sink(5)

      def list_sink(list):
        __testSink(list[1])

      def list_match():
        x = [5, __testSource()]
        list_sink(x)

      def no_list_match():
        x = [__testSource(), 5]
        list_sink(x)
      |}
    ~expect:{
      iterations = 4;
      expect = [
        {
          define_name = "rce_problem";
          returns = [];
          sink_parameters = [];
          tito_parameters = [];
          errors = [
            {
              code = 5001;
              pattern = ".*Possible shell injection.*Data from \\[UserControlled\\].*\\[RemoteCodeExecution\\].*";
            };
          ]
        };
        {
          define_name = "match_flows";
          returns = [];
          sink_parameters = [];
          tito_parameters = [];
          errors = [
            {
              code = 5002;
              pattern = ".*Test flow.*Data from \\[Test\\] source(s).* \\[Test\\] sink(s).*";
            }
          ];
        };
        {
          define_name = "match_flows_multiple";
          returns = [];
          sink_parameters = [];
          tito_parameters = [];
          errors = [
            {
              code = 5002;
              pattern = ".*Test flow.*Data from \\[Test\\] source(s).* \\[Test\\] sink(s).*";
            }
          ];
        };
        {
          define_name = "match_via_methods";
          returns = [];
          sink_parameters = [];
          tito_parameters = [];
          errors = [
            {
              code = 5002;
              pattern = ".*Test flow.*Data from \\[Test\\] source(s).* \\[Test\\] sink(s).*";
            };
          ]
        };
        {
          define_name = "no_match_via_methods";
          returns = [];
          sink_parameters = [];
          tito_parameters = [];
          errors = [];
        };
        {
          define_name = "match_via_receiver";
          returns = [];
          sink_parameters = [];
          tito_parameters = [];
          errors = [
            {
              code = 5002;
              pattern = ".*Test flow.*Data from \\[Test\\] source(s).* \\[Test\\] sink(s).*";
            };
          ]
        };
        {
          define_name = "qux";
          returns = [];
          sink_parameters = [
            { name = "arg"; sinks = [Taint.Sinks.Test] }
          ];
          tito_parameters = [];
          errors = [];
        };
        {
          define_name = "bad";
          returns = [];
          sink_parameters = [
            { name = "arg"; sinks = [Taint.Sinks.Test] }
          ];
          tito_parameters = [];
          errors = [];
        };
        {
          define_name = "bar";
          returns = [Sources.Test];
          sink_parameters = [];
          tito_parameters = [];
          errors = [];
        };
        {
          define_name = "some_source";
          returns = [Sources.Test];
          sink_parameters = [];
          tito_parameters = [];
          errors = [];
        };
        {
          define_name = "list_sink";
          returns = [];
          sink_parameters = [
            { name = "list"; sinks = [Taint.Sinks.Test] }
          ];
          tito_parameters = [];
          errors = [];
        };
        {
          define_name = "no_list_match";
          returns = [];
          sink_parameters = [];
          tito_parameters = [];
          errors = [];
        };
        {
          define_name = "list_match";
          returns = [];
          sink_parameters = [];
          tito_parameters = [];
          errors = [
            {
              code = 5002;
              pattern = ".*Test flow.*Data from \\[Test\\] source(s).* \\[Test\\] sink(s).*";
            };
          ]
        };
      ]
    }


let test_integration _ =
  TaintIntegrationTest.Files.dummy_dependency |> ignore;

  let test_paths =
    (* Shameful things happen here... *)
    Path.current_working_directory ()
    |> Path.show
    |> String.chop_suffix_exn ~suffix:"_build/default/taint/test"
    |> (fun root -> Path.create_absolute root)
    |> (fun root -> Path.create_relative ~root ~relative:"taint/test/integration/")
    |> (fun root -> Path.list ~filter:(String.is_suffix ~suffix:".py") ~root)
  in
  let run_test path =
    let serialized_models =
      let source =
        File.create path
        |> File.content
        |> (fun content -> Option.value_exn content)
      in
      let call_graph, all_callables = create_call_graph source in
      let environment = Test.environment () in
      Analysis.compute_fixpoint
        ~configuration:Test.mock_configuration
        ~scheduler:(Scheduler.mock ())
        ~environment
        ~analyses:[Taint.Analysis.abstract_kind]
        ~caller_map:(CallGraph.reverse call_graph)
        ~all_callables
        Fixpoint.Epoch.initial
      |> ignore;
      let serialized_model callable: string =
        let externalization =
          Interprocedural.Analysis.externalize callable
          |> List.map ~f:(fun json -> Yojson.Safe.pretty_to_string ~std:true json ^ "\n")
          |> String.concat ~sep:""
        in
        externalization
      in
      List.map all_callables ~f:serialized_model
      |> List.sort ~compare:String.compare
      |> String.concat ~sep:""
    in
    let expected =
      try
        Path.show path
        |> (fun path -> path ^ ".expect")
        |> Path.create_absolute
        |> File.create
        |> File.content
        |> (fun content -> Option.value_exn content)
      with Unix.Unix_error _ ->
        failwith (Format.asprintf "Could not read `.expect` file for %a" Path.pp path)
    in
    let write_output () =
      try
        Path.show path
        |> (fun path -> path ^ ".output")
        |> Path.create_absolute ~follow_symbolic_links:false
        |> File.create ~content:serialized_models
        |> File.write
      with Unix.Unix_error _ ->
        failwith (Format.asprintf "Could not write `.output` file for %a" Path.pp path)
    in
    let remove_old_output () =
      try
        Path.show path
        |> (fun path -> path ^ ".output")
        |> Sys.remove
      with Sys_error _ ->
        (* be silent *)
        ()
    in
    if String.equal expected serialized_models then
      remove_old_output ()
    else begin
      write_output ();
      Printf.printf "Expectations differ for %s" (Path.show path);
      assert_equal
        ~printer:ident
        ~cmp:String.equal
        ~pp_diff:(Test.diff ~print:String.pp)
        expected
        serialized_models
    end
  in
  List.iter test_paths ~f:run_test


let () =
  "taint">:::[
    "fixpoint">::test_fixpoint;
    "integration">::test_integration;
  ]
  |> TestHelper.run_with_taint_models
