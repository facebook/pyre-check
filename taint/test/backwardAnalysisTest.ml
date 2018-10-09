(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Ast
open Pyre
open Expression
open Taint

open Test
open Interprocedural

type parameter_taint = {
  position: int;
  sinks: Taint.Sinks.t list;
}


type taint_in_taint_out_expectation = {
  define_name: string;
  taint_sink_parameters: parameter_taint list;
  tito_parameters: int list;
}


let assert_taint ?(qualifier = Access.create "qualifier") ~source ~expected () =
  let source =
    parse ~qualifier source
    |> Preprocessing.preprocess
  in
  let configuration = Test.mock_configuration in
  let environment = Test.environment ~configuration () in
  Service.Environment.populate environment [source];
  TypeCheck.check configuration environment source |> ignore;
  let defines =
    source
    |> Preprocessing.defines
    |> List.rev
  in
  let () =
    List.map ~f:Callable.create defines
    |> Fixpoint.KeySet.of_list
    |> Fixpoint.remove_new
  in
  let analyze_and_store_in_order define =
    let call_target = Callable.create define in
    let () =
      Log.log
        ~section:`Taint
        "Analyzing %s"
        (Interprocedural.Callable.show call_target)
    in
    let backward = BackwardAnalysis.run define in
    let model = { Taint.Result.empty_model with backward } in
    Result.empty_model
    |> Result.with_model Taint.Result.kind model
    |> Fixpoint.add_predefined call_target
  in
  let () = List.iter ~f:analyze_and_store_in_order defines in
  let check_expectation { define_name; taint_sink_parameters; tito_parameters; } =
    let open Taint.Result in
    let extract_sinks_by_parameter_position root sink_tree sink_map =
      match root with
      | AccessPath.Root.Parameter { position; _ } ->
          let sinks =
            Domains.BackwardState.collapse sink_tree
            |> Domains.BackwardTaint.leaves
          in
          let sinks =
            Int.Map.find sink_map position
            |> Option.value ~default:[]
            |> List.rev_append sinks
            |> List.dedup_and_sort ~compare:Taint.Sinks.compare
          in
          Int.Map.set sink_map ~key:position ~data:sinks
      | _ ->
          sink_map
    in
    let backward =
      let model =
        Fixpoint.get_model (Callable.create_real (Access.create define_name))
        >>= Result.get_model Taint.Result.kind
      in
      match model with
      | None -> Format.sprintf "model not found for %s" define_name |> assert_failure
      | Some { backward; _ } -> backward
    in
    let taint_map =
      Domains.BackwardState.fold
        backward.sink_taint
        ~f:extract_sinks_by_parameter_position
        ~init:Int.Map.empty
    in
    let extract_tito_parameter_position root _ positions =
      match root with
      | AccessPath.Root.Parameter { position; _ } -> Int.Set.add positions position
      | _ -> positions
    in
    let taint_in_taint_out_positions =
      Domains.BackwardState.fold
        backward.taint_in_taint_out
        ~f:extract_tito_parameter_position
        ~init:Int.Set.empty
    in
    let check_each_sink_position ~key:position ~data =
      match data with
      | `Both (expected, actual) ->
          assert_equal
            ~cmp:(List.equal ~equal:Taint.Sinks.equal)
            ~printer:(fun list -> Sexp.to_string [%message (list: Taint.Sinks.t list)])
            ~msg:(Format.sprintf "Define %s Position %d" define_name position)
            expected
            actual
      | `Left expected ->
          assert_equal
            ~cmp:(List.equal ~equal:Taint.Sinks.equal)
            ~printer:(fun list -> Sexp.to_string [%message (list: Taint.Sinks.t list)])
            ~msg:(Format.sprintf "Define %s Position %d" define_name position)
            expected
            []
      | `Right _ ->
          (* Okay, we may have outcomes we don't care about *)
          ()
    in
    let expected_sinks =
      List.map ~f:(fun { position; sinks; } -> position, sinks) taint_sink_parameters
      |> Int.Map.of_alist_exn
    in
    let () = Int.Map.iter2 ~f:check_each_sink_position expected_sinks taint_map in
    let expected_tito = Int.Set.of_list tito_parameters in
    assert_equal
      ~cmp:Int.Set.equal
      ~printer:(fun set -> Sexp.to_string [%message (set: Int.Set.t)])
      ~msg:(Format.sprintf "Define %s Tito positions" define_name)
      expected_tito
      taint_in_taint_out_positions
  in
  List.iter ~f:check_expectation expected


let test_plus_taint_in_taint_out _ =
  assert_taint
    ~source:
      {|
      def test_plus_taint_in_taint_out(tainted_parameter1, parameter2):
        tainted_value = tainted_parameter1 + 5
        return tainted_value
      |}
    ~expected:[
      {
        define_name = "qualifier.test_plus_taint_in_taint_out";
        taint_sink_parameters = [];
        tito_parameters = [0];
      };
    ]
    ()

let test_concatenate_taint_in_taint_out _ =
  assert_taint
    ~source:
      {|
      def test_concatenate_taint_in_taint_out(parameter0, tainted_parameter1):
        unused_parameter = parameter0
        command_unsafe = 'echo' + tainted_parameter1 + ' >> /dev/null'
        return command_unsafe
      |}
    ~expected:[
      {
        define_name = "qualifier.test_concatenate_taint_in_taint_out";
        taint_sink_parameters = [];
        tito_parameters = [1];
      };
    ]
    ()

let test_call_taint_in_taint_out _ =
  assert_taint
    ~source:
      {|
      def test_base_tito(parameter0, tainted_parameter1):
        return tainted_parameter1

      def test_called_tito(tainted_parameter0, parameter1):
        return test_base_tito(parameter1, tainted_parameter0)
    |}
    ~expected:[
      {
        define_name = "qualifier.test_base_tito";
        taint_sink_parameters = [];
        tito_parameters = [1];
      };
      {
        define_name = "qualifier.test_called_tito";
        taint_sink_parameters = [];
        tito_parameters = [0];
      };
    ]
    ()

let test_sink _ =
  assert_taint
    ~source:
      {|
      def test_sink(parameter0, tainted_parameter1):
        unused_parameter = parameter0
        command_unsafe = 'echo' + tainted_parameter1 + ' >> /dev/null'
        __testSink(command_unsafe)
      |}
    ~expected:[
      {
        define_name = "qualifier.test_sink";
        taint_sink_parameters = [
          { position = 1; sinks = [Taint.Sinks.Test]; };
        ];
        tito_parameters = [];
      };
    ]
    ()


let test_rce_sink _ =
  assert_taint
    ~source:
      {|
      def test_rce_sink(parameter0, tainted_parameter1):
        unused_parameter = parameter0
        command_unsafe = 'echo' + tainted_parameter1 + ' >> /dev/null'
        __eval(command_unsafe)
      |}
    ~expected:[
      {
        define_name = "qualifier.test_rce_sink";
        taint_sink_parameters = [
          { position = 1; sinks = [Taint.Sinks.RemoteCodeExecution]; }
        ];
        tito_parameters = [];
      };
    ]
    ()


let test_rce_and_test_sink _ =
  assert_taint
    ~source:
      {|
      def test_rce_and_test_sink(test_only, rce_only, both):
        __testSink(test_only)
        __eval(rce_only)
        if True:
          __testSink(both)
        else:
          __eval(both)
      |}
    ~expected:[
      {
        define_name = "qualifier.test_rce_and_test_sink";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test]; };
          { position = 1; sinks = [Taint.Sinks.RemoteCodeExecution]; };
          { position = 2; sinks = [Taint.Sinks.RemoteCodeExecution; Taint.Sinks.Test]; };
        ];
        tito_parameters = [];
      }
    ]
    ()


let test_tito_sink _ =
  assert_taint
    ~source:
      {|
      def test_base_tito(parameter0, tainted_parameter1):
        return tainted_parameter1

      def test_called_tito(tainted_parameter0, parameter1):
        return test_base_tito(parameter1, tainted_parameter0)

      def test_tito_sink(parameter0, tainted_parameter1):
        tainted = test_called_tito(tainted_parameter1, parameter0)
        __testSink(tainted)
      |}
    ~expected:[
      {
        define_name = "qualifier.test_tito_sink";
        taint_sink_parameters = [
          { position = 0; sinks = []; };
          { position = 1; sinks = [Taint.Sinks.Test]; };
        ];
        tito_parameters = [];
      };
    ]
    ()


let test_apply_method_model_at_call_site _ =
  assert_taint
    ~source:
      {|
        class Foo:
          def qux(self, tainted_parameter):
            command_unsafe = tainted_parameter
            __testSink(command_unsafe)

        class Bar:
          def qux(self, not_tainted_parameter):
            pass

        def taint_across_methods(tainted_parameter):
          f = Foo()
          return f.qux(tainted_parameter)
      |}
    ~expected:[
      {
        define_name = "qualifier.taint_across_methods";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
      };
    ]
    ();

  assert_taint
    ~source:
      {|
        class Foo:
          def qux(self, tainted_parameter):
            command_unsafe = tainted_parameter
            __testSink(command_unsafe)

        class Bar:
          def qux(self, not_tainted_parameter):
            pass

        def taint_across_methods(not_tainted_parameter):
          f = Bar()
          return f.qux(not_tainted_parameter)
      |}
    ~expected:[
      {
        define_name = "qualifier.taint_across_methods";
        taint_sink_parameters = [];
        tito_parameters = [];
      };
    ]
    ();

  assert_taint
    ~source:
      {|
        class Foo:
          def qux(self, tainted_parameter):
            command_unsafe = tainted_parameter
            __testSink(command_unsafe)

        class Bar:
          def qux(self, not_tainted_parameter):
            pass

        def taint_across_methods(f: Foo, tainted_parameter):
          return f.qux(tainted_parameter)
      |}
    ~expected:[
      {
        define_name = "qualifier.taint_across_methods";
        taint_sink_parameters = [
          { position = 0; sinks = [] };
          { position = 1; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
      };
    ]
    ();

  assert_taint
    ~source:
      {|
        class Foo:
          def qux(self, tainted_parameter):
            command_unsafe = tainted_parameter
            __testSink(command_unsafe)

        class Bar:
          def qux(self, not_tainted_parameter):
            pass

        def taint_across_methods(f: Bar, not_tainted_parameter):
          return f.qux(not_tainted_parameter)
      |}
    ~expected:[
      {
        define_name = "qualifier.taint_across_methods";
        taint_sink_parameters = [];
        tito_parameters = [];
      };
    ]
    ();

  assert_taint
    ~source:
      {|
        class Foo:
          def qux(self, tainted_parameter):
            command_unsafe = tainted_parameter
            __testSink(command_unsafe)

        class Bar:
          def qux(self, not_tainted_parameter):
            pass

        def taint_across_union_receiver_types(condition, tainted_parameter):
          if condition:
            f = Foo()
          else:
            f = Bar()

          return f.qux(tainted_parameter)
      |}
    ~expected:[
      {
        define_name = "qualifier.taint_across_union_receiver_types";
        taint_sink_parameters = [
          { position = 1; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
      };
    ]
    ();

  assert_taint
    ~source:
      {|
        class Foo:
          def qux(self, not_tainted_parameter):
            pass

        class Bar:
          def qux(self, not_tainted_parameter):
            pass

        class Baz:
          def qux(self, tainted_parameter):
            command_unsafe = tainted_parameter
            __testSink(command_unsafe)

        def taint_across_union_receiver_types(condition, tainted_parameter):
          if condition:
            f = Foo()
          elif condition > 1:
            f = Bar()
          else:
            f = Baz()

          return f.qux(tainted_parameter)
      |}
    ~expected:[
      {
        define_name = "qualifier.Foo.qux";
        taint_sink_parameters = [];
        tito_parameters = [];
      };
      {
        define_name = "qualifier.Baz.qux";
        taint_sink_parameters = [
          { position = 1; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
      };
      {
        define_name = "qualifier.taint_across_union_receiver_types";
        taint_sink_parameters = [
          { position = 1; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
      };
    ]
    ()


let test_tito_via_receiver _ =
  assert_taint
    ~source:
      {|
        class Foo:
          def tito(self, argument1):
              return self.f

        def tito_via_receiver(parameter):
          x = Foo()
          x.f = parameter
          return f.tito
      |}
    ~expected:[
      {
        define_name = "qualifier.tito_via_receiver";
        taint_sink_parameters = [];
        tito_parameters = [0];
      };
      {
        define_name = "qualifier.Foo.tito";
        taint_sink_parameters = [];
        tito_parameters = [0];
      };
    ]
    ()


let test_nested_call_path _ =
  assert_taint
    ~source:
      {|
        class Foo:
          def tito(self, argument1) -> Foo:
              return self

          def sink(self, argument1) -> Foo:
              __testSink(argument1)
              return self

        def nested(parameter0, parameter1, parameter2):
          x = Foo()
          x.sink(parameter0).tito(parameter1).sink(parameter2)
      |}
    ~expected:[
      {
        define_name = "qualifier.Foo.sink";
        taint_sink_parameters = [
          { position = 1; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [0];
      };
      {
        define_name = "qualifier.nested";
        taint_sink_parameters = [
          (*
          { position = 0; sinks = [Taint.Sinks.Test] };
          { position = 2; sinks = [Taint.Sinks.Test] };
          *)
        ];
        tito_parameters = [];
        };
    ]
    ()


let () =
  "taint">:::[
    "plus_taint_in_taint_out">::test_plus_taint_in_taint_out;
    "concatenate_taint_in_taint_out">::test_concatenate_taint_in_taint_out;
    "rce_sink">::test_rce_sink;
    "test_sink">::test_sink;
    "rce_and_test_sink">::test_rce_and_test_sink;
    "test_call_tito">::test_call_taint_in_taint_out;
    "test_tito_sink">::test_tito_sink;
    "test_apply_method_model_at_call_site">::test_apply_method_model_at_call_site;
    "test_nested_call_path">::test_nested_call_path;
  ]
  |> Test.run_with_taint_models
