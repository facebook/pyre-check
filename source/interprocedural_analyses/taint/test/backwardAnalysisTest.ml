(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open Pyre
open Taint
open Interprocedural
open TestHelper

let assert_taint ?(skip_for_pyrefly = false) ~context source expected =
  let handle = "qualifier.py" in
  let qualifier = Ast.Reference.create "qualifier" in
  let project =
    Test.ScratchPyrePysaProject.setup
      ~context
      ~force_pyre1:skip_for_pyrefly
      ~requires_type_of_expressions:true
      [handle, source]
  in
  let configuration = Test.ScratchPyrePysaProject.configuration_of project in
  let static_analysis_configuration =
    Configuration.StaticAnalysis.create
      ~maximum_target_depth:Configuration.StaticAnalysis.default_maximum_target_depth
      configuration
      ()
  in
  let pyre_api = Test.ScratchPyrePysaProject.read_only_api project in
  let initial_models = TestHelper.get_initial_models ~pyre_api in
  let initial_callables = FetchCallables.from_qualifier ~configuration ~pyre_api ~qualifier in
  let scheduler = Test.mock_scheduler () in
  let scheduler_policy = Scheduler.Policy.legacy_fixed_chunk_count () in
  let definitions_and_stubs =
    Interprocedural.FetchCallables.get initial_callables ~definitions:true ~stubs:true
  in
  let callables_to_definitions_map =
    Interprocedural.CallablesSharedMemory.ReadWrite.from_callables
      ~scheduler
      ~scheduler_policy
      ~pyre_api
      definitions_and_stubs
  in
  let type_of_expression_shared_memory =
    Interprocedural.TypeOfExpressionSharedMemory.create
      ~pyre_api
      ~callables_to_definitions_map:
        (Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
      ()
  in
  let analyze_and_store_in_order models (callable, define) =
    let () = Log.log ~section:`Taint "Analyzing %a" Target.pp callable in
    let call_graph_of_define =
      TestHelper.call_graph_of_callable
        ~static_analysis_configuration
        ~pyre_api
        ~override_graph:
          (Some (OverrideGraph.SharedMemory.create () |> OverrideGraph.SharedMemory.read_only))
        ~object_targets:(initial_models |> Registry.object_targets |> Target.Set.elements)
        ~callables_to_definitions_map:
          (Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
        ~callables_to_decorators_map:
          (Interprocedural.CallableToDecoratorsMap.SharedMemory.create_empty
             ~is_pyrefly:(PyrePysaApi.ReadOnly.is_pyrefly pyre_api)
             ()
          |> Interprocedural.CallableToDecoratorsMap.SharedMemory.read_only)
        ~type_of_expression_shared_memory
        ~check_invariants:true
        ~normalize_to_pyre1:false
        ~module_name:qualifier
        ~callable
    in
    let cfg = Cfg.create (Ast.Node.value define) in
    let taint_configuration = TaintConfiguration.Heap.default in
    let backward =
      BackwardAnalysis.run
        ?profiler:None
        ~taint_configuration
        ~string_combine_partial_sink_tree:
          (Taint.CallModel.StringFormatCall.declared_partial_sink_tree taint_configuration)
        ~pyre_api
        ~type_of_expression_shared_memory
        ~class_interval_graph:(ClassIntervalSetGraph.SharedMemory.create ())
        ~global_constants:
          (GlobalConstants.SharedMemory.create () |> GlobalConstants.SharedMemory.read_only)
        ~qualifier
        ~callable
        ~define
        ~cfg
        ~call_graph_of_define
        ~get_callee_model:(Registry.get models)
        ~existing_model:Model.empty_model
        ~triggered_sinks:(Issue.TriggeredSinkForBackward.create ())
        ()
    in
    let model = { Model.empty_model with backward } in
    Registry.set models ~target:callable ~model
  in
  let callable_and_defines =
    let add_define callable =
      let { Interprocedural.CallablesSharedMemory.DefineAndQualifier.define; _ } =
        Interprocedural.CallablesSharedMemory.ReadOnly.get_define
          (Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
          callable
        |> PyrePysaApi.AstResult.value_exn ~message:"missing ast"
      in
      callable, define
    in
    Interprocedural.FetchCallables.get_definitions initial_callables
    |> List.map ~f:add_define
    |> List.sort
         ~compare:(fun (_, { Ast.Node.location = left; _ }) (_, { Ast.Node.location = right; _ }) ->
           Ast.Location.compare left right)
  in
  let models = List.fold ~f:analyze_and_store_in_order ~init:initial_models callable_and_defines in
  let get_model = Registry.get models in
  let get_errors _ = [] in
  Interprocedural.CallablesSharedMemory.ReadWrite.cleanup callables_to_definitions_map;
  List.iter
    ~f:
      (check_expectation
         ~pyre_api
         ~taint_configuration:TaintConfiguration.Heap.default
         ~get_model
         ~get_errors)
    expected


let test_plus_taint_in_taint_out context =
  assert_taint
    ~context
    {|
    def test_plus_taint_in_taint_out(tainted_parameter1, parameter2):
      tainted_value = tainted_parameter1 + 5
      return tainted_value
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "tainted_parameter1"; titos = [Sinks.LocalReturn] }]
        "qualifier.test_plus_taint_in_taint_out";
    ]


let test_concatenate_taint_in_taint_out context =
  assert_taint
    ~context
    {|
      def test_concatenate_taint_in_taint_out(parameter0, tainted_parameter1):
        unused_parameter = parameter0
        command_unsafe = 'echo' + tainted_parameter1 + ' >> /dev/null'
        return command_unsafe
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "tainted_parameter1"; titos = [Sinks.LocalReturn] }]
        "qualifier.test_concatenate_taint_in_taint_out";
    ]


let test_call_taint_in_taint_out context =
  assert_taint
    ~context
    {|
      def test_base_tito(parameter0, tainted_parameter1):
        return tainted_parameter1

      def test_called_tito(tainted_parameter0, parameter1):
        return test_base_tito(parameter1, tainted_parameter0)
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "tainted_parameter1"; titos = [Sinks.LocalReturn] }]
        "qualifier.test_base_tito";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "tainted_parameter0"; titos = [Sinks.LocalReturn] }]
        "qualifier.test_called_tito";
    ]


let test_sink context =
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      def test_sink(parameter0, tainted_parameter1):
        unused_parameter = parameter0
        command_unsafe = 'echo' + tainted_parameter1 + ' >> /dev/null'
        _test_sink(command_unsafe)
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "tainted_parameter1"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.test_sink";
    ]


let test_rce_sink context =
  assert_taint
    ~context
    {|
      def test_rce_sink(parameter0, tainted_parameter1):
        unused_parameter = parameter0
        command_unsafe = 'echo' + tainted_parameter1 + ' >> /dev/null'
        eval(command_unsafe)
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:
          [{ name = "tainted_parameter1"; sinks = [Sinks.NamedSink "RemoteCodeExecution"] }]
        "qualifier.test_rce_sink";
    ]


let test_rce_and_test_sink context =
  assert_taint
    ~context
    ~skip_for_pyrefly:true (* TODO(T225700656): False negative for pyrefly *)
    {|
      from pysa import _test_sink

      def test_rce_and_test_sink(test_only, rce_only, both):
        _test_sink(test_only)
        eval(rce_only)
        if 1 > 2:
          _test_sink(both)
        else:
          eval(both)
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:
          [
            { name = "test_only"; sinks = [Sinks.NamedSink "Test"] };
            { name = "rce_only"; sinks = [Sinks.NamedSink "RemoteCodeExecution"] };
            {
              name = "both";
              sinks = [Sinks.NamedSink "RemoteCodeExecution"; Sinks.NamedSink "Test"];
            };
          ]
        "qualifier.test_rce_and_test_sink";
    ]


let test_tito_sink context =
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      def test_base_tito(parameter0, tainted_parameter1):
        return tainted_parameter1

      def test_called_tito(tainted_parameter0, parameter1):
        return test_base_tito(parameter1, tainted_parameter0)

      def test_tito_sink(parameter0, tainted_parameter1):
        tainted = test_called_tito(tainted_parameter1, parameter0)
        _test_sink(tainted)
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "tainted_parameter1"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.test_tito_sink";
    ]


let test_asyncio_gather context =
  assert_taint
    ~context
    {|
      import asyncio
      def sink_through_gather(arg):
          (_, result, _) = await asyncio.gather(1, arg, "foo")
          return result
      def benign_through_gather(arg):
          (_, _, result) = await asyncio.gather(1, arg, "foo")
          return result
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "arg"; titos = [Sinks.LocalReturn] }]
        "qualifier.sink_through_gather";
      outcome ~kind:`Function ~parameter_titos:[] "qualifier.benign_through_gather";
    ]


let test_apply_method_model_at_call_site context =
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      class Foo:
        def qux(self, tainted_parameter):
          command_unsafe = tainted_parameter
          _test_sink(command_unsafe)

      class Bar:
        def qux(self, not_tainted_parameter):
          pass

      def taint_across_methods(tainted_parameter):
        f = Foo()
        return f.qux(tainted_parameter)
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "tainted_parameter"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.taint_across_methods";
    ];
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      class Foo:
        def qux(self, tainted_parameter):
          command_unsafe = tainted_parameter
          _test_sink(command_unsafe)

      class Bar:
        def qux(self, not_tainted_parameter):
          pass

      def taint_across_methods(not_tainted_parameter):
        f = Bar()
        return f.qux(not_tainted_parameter)
    |}
    [outcome ~kind:`Function ~parameter_sinks:[] "qualifier.taint_across_methods"];
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      class Foo:
        def qux(self, tainted_parameter):
          command_unsafe = tainted_parameter
          _test_sink(command_unsafe)

      class Bar:
        def qux(self, not_tainted_parameter):
          pass

      def taint_across_methods(f: Foo, tainted_parameter):
        return f.qux(tainted_parameter)
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "tainted_parameter"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.taint_across_methods";
    ];
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      class Foo:
        def qux(self, tainted_parameter):
          command_unsafe = tainted_parameter
          _test_sink(command_unsafe)

      class Bar:
        def qux(self, not_tainted_parameter):
          pass

      def taint_across_methods(f: Bar, not_tainted_parameter):
        return f.qux(not_tainted_parameter)
    |}
    [outcome ~kind:`Function ~parameter_sinks:[] "qualifier.taint_across_methods"];
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      class Foo:
        def qux(self, tainted_parameter):
          command_unsafe = tainted_parameter
          _test_sink(command_unsafe)

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
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "tainted_parameter"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.taint_across_union_receiver_types";
    ];
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      class Foo:
        def qux(self, not_tainted_parameter):
          pass

      class Bar:
        def qux(self, not_tainted_parameter):
          pass

      class Baz:
        def qux(self, tainted_parameter):
          command_unsafe = tainted_parameter
          _test_sink(command_unsafe)

      def taint_across_union_receiver_types(condition, tainted_parameter):
        if condition:
          f = Foo()
        elif condition > 1:
          f = Bar()
        else:
          f = Baz()

        return f.qux(tainted_parameter)
    |}
    [
      outcome ~kind:`Method ~parameter_sinks:[] "qualifier.Foo.qux";
      outcome
        ~kind:`Method
        ~parameter_sinks:[{ name = "tainted_parameter"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.Baz.qux";
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "tainted_parameter"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.taint_across_union_receiver_types";
    ]


let test_tito_via_receiver context =
  assert_taint
    ~context
    {|
      class TitoClass:
        f = {}
        def tito(self, argument1):
            return self.f

      def tito_via_receiver(parameter):
        x = TitoClass()
        x.f = parameter
        return x.tito('')
    |}
    [
      outcome
        ~kind:`Method
        ~parameter_titos:[{ name = "self"; titos = [Sinks.LocalReturn] }]
        "qualifier.TitoClass.tito";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "parameter"; titos = [Sinks.LocalReturn] }]
        "qualifier.tito_via_receiver";
    ]


let test_sequential_call_path context =
  (* Testing the setup to get this out of the way. *)
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      class Foo:
        def sink(self, argument) -> Foo:
            _test_sink(argument)
            return self
    |}
    [
      outcome
        ~kind:`Method
        ~parameter_sinks:[{ name = "argument"; sinks = [Sinks.NamedSink "Test"] }]
        ~parameter_titos:[{ name = "self"; titos = [Sinks.LocalReturn] }]
        "qualifier.Foo.sink";
    ];
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      class Foo:
        def sink(self, argument) -> Foo:
            _test_sink(argument)
            return self

      def sequential_with_single_sink(first, second, third):
        x = Foo()
        x.sink(first)
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "first"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.sequential_with_single_sink";
    ];
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      class Foo:
        def sink(self, argument) -> Foo:
            _test_sink(argument)
            return self

      def sequential_with_two_sinks(first, second, third):
        x = Foo()
        x.sink(first)
        x.sink(second)
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:
          [
            { name = "first"; sinks = [Sinks.NamedSink "Test"] };
            { name = "second"; sinks = [Sinks.NamedSink "Test"] };
          ]
        "qualifier.sequential_with_two_sinks";
    ];
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      class Foo:
        def sink(self, argument) -> Foo:
            _test_sink(argument)
            return self

      def sequential_with_redefine(first, second, third):
        x = Foo()
        x.sink(first)
        x = Foo()
        x.sink(second)
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:
          [
            { name = "first"; sinks = [Sinks.NamedSink "Test"] };
            { name = "second"; sinks = [Sinks.NamedSink "Test"] };
          ]
        "qualifier.sequential_with_redefine";
    ];
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      class Foo:
        def sink(self, argument) -> Foo:
            _test_sink(argument)
            return self

      def sequential_with_distinct_sinks(first, second, third):
        x = Foo()
        x.sink(first)
        a = Foo()
        a.sink(second)
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:
          [
            { name = "first"; sinks = [Sinks.NamedSink "Test"] };
            { name = "second"; sinks = [Sinks.NamedSink "Test"] };
          ]
        "qualifier.sequential_with_distinct_sinks";
    ];
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      class Foo:
        def sink(self, argument) -> Foo:
            _test_sink(argument)
            return self

      def sequential_with_self_propagation(first, second, third):
        x = Foo()
        x = x.sink(first)
        x.sink(second)
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:
          [
            { name = "first"; sinks = [Sinks.NamedSink "Test"] };
            { name = "second"; sinks = [Sinks.NamedSink "Test"] };
          ]
        "qualifier.sequential_with_self_propagation";
    ]


let test_chained_call_path context =
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      class Foo:
        def sink(self, argument1) -> Foo:
            _test_sink(argument1)
            return self

      def chained(parameter0, parameter1, parameter2):
        x = Foo()
        x.sink(parameter0).sink(parameter2)
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:
          [
            { name = "parameter0"; sinks = [Sinks.NamedSink "Test"] };
            { name = "parameter2"; sinks = [Sinks.NamedSink "Test"] };
          ]
        "qualifier.chained";
    ];
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      class Foo:
        def tito(self, argument1) -> Foo:
            return self

        def sink(self, argument1) -> Foo:
            _test_sink(argument1)
            return self

      def chained_with_tito(parameter0, parameter1, parameter2):
        x = Foo()
        x.sink(parameter0).tito(parameter1).sink(parameter2)
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:
          [
            { name = "parameter0"; sinks = [Sinks.NamedSink "Test"] };
            { name = "parameter2"; sinks = [Sinks.NamedSink "Test"] };
          ]
        "qualifier.chained_with_tito";
    ]


let test_dictionary context =
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      def dictionary_sink(arg):
        {
          "a": _test_sink(arg),
        }

      def dictionary_tito(arg):
        return {
          "a": arg,
        }

      def dictionary_same_index(arg):
        dict = {
          "a": arg,
        }
        return dict["a"]

      def dictionary_different_index(arg):
        dict = {
          "a": arg,
        }
        return dict["b"]

      def dictionary_unknown_read_index(arg, index):
        dict = {
          "a": arg,
        }
        return dict[index]

      def dictionary_unknown_write_index(arg, index):
        dict = {
          index: arg,
        }
        return dict["a"]
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.dictionary_sink";
      outcome
        ~kind:`Function
        ~parameter_sinks:[]
        ~parameter_titos:[{ name = "arg"; titos = [Sinks.LocalReturn] }]
        "qualifier.dictionary_tito";
      outcome
        ~kind:`Function
        ~parameter_sinks:[]
        ~parameter_titos:[{ name = "arg"; titos = [Sinks.LocalReturn] }]
        "qualifier.dictionary_same_index";
      outcome
        ~kind:`Function
        ~parameter_sinks:[]
        ~parameter_titos:[]
        "qualifier.dictionary_different_index";
      outcome
        ~kind:`Function
        ~parameter_sinks:[]
        ~parameter_titos:[{ name = "arg"; titos = [Sinks.LocalReturn] }]
        "qualifier.dictionary_unknown_read_index";
      outcome
        ~kind:`Function
        ~parameter_sinks:[]
        ~parameter_titos:[{ name = "arg"; titos = [Sinks.LocalReturn] }]
        "qualifier.dictionary_unknown_write_index";
    ];
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      def dictionary_sink(arg):
        second = { **(_test_sink(arg)) }
        return second
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.dictionary_sink";
    ];
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      def dictionary_sink(arg):
        d = { _test_sink(arg): "a" }
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.dictionary_sink";
    ];
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      def dictionary_sink(arg):
        d = { _test_sink(a): "a" for a in arg }
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.dictionary_sink";
    ];
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      def dictionary_sink(arg):
        d = { "a": _test_sink(a) for a in arg }
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.dictionary_sink";
    ];
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      def key_sink(arg1, arg2):
        d = { arg1: arg2 }
        for k, v in d.items():
          _test_sink(k)
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg1"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.key_sink";
    ]


let test_comprehensions context =
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      def sink_in_iterator(arg):
          [ x for x in _test_sink(arg) ]

      def sink_in_expression(data):
          [ _test_sink(x) for x in data ]

      def tito(data):
          return [x for x in data ]

      def sink_in_set_iterator(arg):
          { x for x in _test_sink(arg) }

      def sink_in_set_expression(data):
          { _test_sink(x) for x in data }

      def tito_set(data):
          return { x for x in data }

      def sink_in_generator_iterator(arg):
          gen = (x for x in _test_sink(arg))

      def sink_in_generator_expression(data):
          gen = (_test_sink(x) for x in data)

      def tito_generator(data):
          return (x for x in data)
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.sink_in_iterator";
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "data"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.sink_in_expression";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "data"; titos = [Sinks.LocalReturn] }]
        "qualifier.tito";
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.sink_in_set_iterator";
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "data"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.sink_in_set_expression";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "data"; titos = [Sinks.LocalReturn] }]
        "qualifier.tito_set";
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.sink_in_generator_iterator";
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "data"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.sink_in_generator_expression";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "data"; titos = [Sinks.LocalReturn] }]
        "qualifier.tito_generator";
    ]


let test_list context =
  let arg_tito = { name = "arg"; titos = [Sinks.LocalReturn] } in
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      def sink_in_list(arg):
          return [ 1, _test_sink(arg), "foo" ]

      def list_same_index(arg):
          list = [ 1, arg, "foo" ]
          return list[1]

      def list_different_index(arg):
          list = [ 1, arg, "foo" ]
          return list[2]

      def list_unknown_index(arg, index):
          list = [ 1, arg, "foo" ]
          return list[index]

      def list_pattern_same_index(arg):
          [_, result, _] = [ 1, arg, "foo" ]
          return result

      def list_pattern_different_index(arg):
          [_, _, result] = [ 1, arg, "foo" ]
          return result

      def list_pattern_star_index(arg):
          [_, _, *result] = [ 1, arg, "foo" ]
          return result

      def list_same_index_assignment(arg):
        l = []
        l[0] = arg
        return l[0]

      def list_different_index_assignment(arg):
        l = []
        l[0] = arg
        return l[1]

      def list_index_assignment_return_list(arg):
        l = []
        l[0] = arg
        return l

      def list_nested_assignment_1(arg):
        l = []
        l[0][1] = arg
        return l[0][1]

      def list_nested_assignment_2(arg):
        l = []
        l[0][1] = arg
        return l[0]

      def list_nested_assignment_non_tito(arg):
        l = []
        l[0][1] = arg
        return l[1]
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.sink_in_list";
      outcome ~kind:`Function ~parameter_titos:[arg_tito] "qualifier.list_same_index";
      outcome ~kind:`Function "qualifier.list_different_index";
      outcome ~kind:`Function ~parameter_titos:[arg_tito] "qualifier.list_unknown_index";
      outcome ~kind:`Function ~parameter_titos:[arg_tito] "qualifier.list_pattern_same_index";
      outcome ~kind:`Function "qualifier.list_pattern_different_index";
      outcome ~kind:`Function ~parameter_titos:[arg_tito] "qualifier.list_pattern_star_index";
      outcome ~kind:`Function ~parameter_titos:[arg_tito] "qualifier.list_same_index_assignment";
      outcome ~kind:`Function "qualifier.list_different_index_assignment";
      outcome
        ~kind:`Function
        ~parameter_titos:[arg_tito]
        "qualifier.list_index_assignment_return_list";
      (* TODO(T187636576) The combination of `redirect_assignments` rewriting subscript targets
       * into `__setitem__` calls and `redirect_expressions` rewriting subscript expressions into
       * `__getitem__` calls lead to a regression in the handling of changed subscripts in
       * assignments.
       *
       * outcome ~kind:`Function ~parameter_titos:[arg_tito] "qualifier.list_nested_assignment_1";
       * outcome ~kind:`Function ~parameter_titos:[arg_tito] "qualifier.list_nested_assignment_2";
       *)
      outcome ~kind:`Function ~parameter_titos:[] "qualifier.list_nested_assignment_non_tito";
    ]


let test_tuple context =
  let arg_tito = { name = "arg"; titos = [Sinks.LocalReturn] } in
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      def sink_in_tuple(arg):
          return ( 1, _test_sink(arg), "foo" )

      def tuple_same_index(arg):
          tuple = ( 1, arg, "foo" )
          return tuple[1]

      def tuple_different_index(arg):
          tuple = ( 1, arg, "foo" )
          return tuple[2]

      def tuple_unknown_index(arg, index):
          tuple = ( 1, arg, "foo" )
          return tuple[index]

      def tuple_pattern_same_index(arg):
          (_, result, _) = ( 1, arg, "foo" )
          return result

      def tuple_pattern_different_index(arg):
          (_, _, result) = ( 1, arg, "foo" )
          return result
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.sink_in_tuple";
      outcome ~kind:`Function ~parameter_titos:[arg_tito] "qualifier.tuple_same_index";
      outcome ~kind:`Function "qualifier.tuple_different_index";
      outcome ~kind:`Function ~parameter_titos:[arg_tito] "qualifier.tuple_unknown_index";
      outcome ~kind:`Function ~parameter_titos:[arg_tito] "qualifier.tuple_pattern_same_index";
      outcome ~kind:`Function "qualifier.tuple_pattern_different_index";
    ];
  assert_taint
    ~context
    {|
      def clear_taint_in_tuple(arg):
        result = arg
        x, result = 1, 2
        return result
    |}
    [outcome ~kind:`Function ~parameter_sinks:[] "qualifier.clear_taint_in_tuple"]


let test_lambda context =
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      def sink_in_lambda(arg):
          f = lambda x : x + _test_sink(arg)

      def lambda_tito(arg):
          f = lambda x : x + arg
          return f
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.sink_in_lambda";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "arg"; titos = [Sinks.LocalReturn] }]
        "qualifier.lambda_tito";
    ]


let test_set context =
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      def sink_in_set(arg):
          return { 1, _test_sink(arg), "foo" }

      def set_index(arg):
          set = { 1, arg, "foo" }
          return set[2]

      def set_unknown_index(arg, index):
          set = { 1, arg, "foo" }
          return set[index]
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.sink_in_set";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "arg"; titos = [Sinks.LocalReturn] }]
        "qualifier.set_index";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "arg"; titos = [Sinks.LocalReturn] }]
        "qualifier.set_unknown_index";
    ]


let test_starred context =
  assert_taint
    ~context
    {|
      from pysa import _test_sink, _tito

      def sink_in_starred(arg):
          _tito( *[ 1, _test_sink(arg), "foo" ] )

      def sink_in_starred_starred(arg):
          _tito( **{
              "a": 1,
              "b": _test_sink(arg),
              "c": "foo",
          })

      def tito_in_starred(arg):
          return _tito( *[ 1, arg, "foo" ] )

      def tito_in_starred_starred(arg):
          return _tito( **{
              "a": 1,
              "b": arg,
              "c": "foo",
          })
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.sink_in_starred";
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.sink_in_starred_starred";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "arg"; titos = [Sinks.LocalReturn] }]
        "qualifier.tito_in_starred";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "arg"; titos = [Sinks.LocalReturn] }]
        "qualifier.tito_in_starred_starred";
    ]


let test_ternary context =
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      def sink_in_then(arg, cond):
          x = _test_sink(arg) if cond else None

      def sink_in_else(arg, cond):
          x = "foo" if cond else _test_sink(arg)

      def sink_in_both(arg1, arg2, cond):
          x = _test_sink(arg1) if cond else _test_sink(arg2)

      def sink_in_cond(arg1, arg2, cond):
          x = arg1 if _test_sink(cond) else arg2

      def tito_in_then(arg, cond):
          return arg if cond else None

      def tito_in_else(arg, cond):
          return "foo" if cond else arg

      def tito_in_both(arg1, arg2, cond):
          return arg1 if cond else arg2
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.sink_in_then";
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.sink_in_else";
      outcome
        ~kind:`Function
        ~parameter_sinks:
          [
            { name = "arg1"; sinks = [Sinks.NamedSink "Test"] };
            { name = "arg2"; sinks = [Sinks.NamedSink "Test"] };
          ]
        "qualifier.sink_in_both";
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "cond"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.sink_in_cond";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "arg"; titos = [Sinks.LocalReturn] }]
        "qualifier.tito_in_then";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "arg"; titos = [Sinks.LocalReturn] }]
        "qualifier.tito_in_else";
      outcome
        ~kind:`Function
        ~parameter_titos:
          [
            { name = "arg1"; titos = [Sinks.LocalReturn] };
            { name = "arg2"; titos = [Sinks.LocalReturn] };
          ]
        "qualifier.tito_in_both";
    ]


let test_unary context =
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      def sink_in_unary(arg):
          x = not _test_sink(arg)

      def tito_via_unary(arg):
          return not arg
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.sink_in_unary";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "arg"; titos = [Sinks.LocalReturn] }]
        "qualifier.tito_via_unary";
    ]


let test_walrus context =
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      def sink_in_walrus(arg):
          (x := _test_sink(arg))

      def tito_via_walrus(arg):
          return (x := arg)
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.sink_in_walrus";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "arg"; titos = [Sinks.LocalReturn] }]
        "qualifier.tito_via_walrus";
    ]


let test_yield context =
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      def sink_in_yield(arg):
          yield _test_sink(arg)

      def tito_via_yield(arg):
          yield arg

      def sink_in_yield_from(arg):
          yield from _test_sink(arg)

      def tito_via_yield_from(arg):
          yield from arg
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.sink_in_yield";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "arg"; titos = [Sinks.LocalReturn] }]
        "qualifier.tito_via_yield";
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.sink_in_yield_from";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "arg"; titos = [Sinks.LocalReturn] }]
        "qualifier.tito_via_yield_from";
    ]


let test_named_arguments context =
  assert_taint
    ~context
    {|
      def with_kw(a, b, **kw):
          return kw

      def no_kw_tito(arg0, arg1, arg2, arg3):
          return with_kw(arg0, arg1, arg2, arg3)

      def no_kw_tito_with_named_args(arg0, arg1):
          return with_kw(b = arg0, a = arg1, c = 5)

      def kw_tito_with_named_args(arg0, arg1):
          return with_kw(b = arg0, c = arg1)

      def kw_tito_with_dict(arg0, dict):
          return with_kw(b = arg0, c = 5, **dict)
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "**"; titos = [Sinks.LocalReturn] }]
        "qualifier.with_kw";
      outcome ~kind:`Function ~parameter_titos:[] "qualifier.no_kw_tito";
      outcome ~kind:`Function ~parameter_titos:[] "qualifier.no_kw_tito_with_named_args";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "arg1"; titos = [Sinks.LocalReturn] }]
        "qualifier.kw_tito_with_named_args";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "dict"; titos = [Sinks.LocalReturn] }]
        "qualifier.kw_tito_with_dict";
    ]


let test_actual_parameter_matching context =
  assert_taint
    ~context
    {|
      def before_star(a, b, *rest, c, d, **kw):
          return b

      def at_star(a, b, *rest, c, d, **kw):
          return rest[0]

      def at_star_plus_one(a, b, *rest, c, d, **kw):
          return rest[1]

      def at_all_star(a, b, *rest, c, d, **kw):
          return rest[x]

      def after_star(a, b, *rest, c, d, **kw):
          return c

      def star_star_q(a, b, *rest, c, d, **kw):
          return kw['q']

      def star_star_all(a, b, *rest, c, d, **kw):
          return kw[x]

      def pass_positional_before_star(arg, no_tito):
          return before_star(
            no_tito,
            arg,
            no_tito,
            *no_tito,
            no_tito,
            c = no_tito,
            q = no_tito,
            r = no_tito,
            **no_tito,
          )

      def pass_positional_at_star(arg, approximate, no_tito):
          return at_star(
            no_tito,
            no_tito,
            arg,
            no_tito,
            *approximate,
            c = no_tito,
            q = no_tito,
            r = no_tito,
            **no_tito,
          )

      def pass_positional_at_star_plus_one(arg, approximate, no_tito):
          return at_star_plus_one(
            no_tito,
            no_tito,
            no_tito,
            arg,
            no_tito,
            *approximate,
            c = no_tito,
            q = no_tito,
            r = no_tito,
            **no_tito,
          )

      def pass_positional_at_all_star(arg, approximate, no_tito):
          return at_all_star(
            no_tito,
            no_tito,
            2,
            arg,
            3,
            *approximate,
            c = no_tito,
            q = no_tito,
            r = no_tito,
            **no_tito,
          )

      def pass_named_after_star(arg, approximate, no_tito):
          return after_star(
            no_tito,
            no_tito,
            no_tito,
            *no_tito,
            no_tito,
            *no_tito,
            c = arg,
            d = no_tito,
            q = no_tito,
            r = no_tito,
            **approximate,
          )

      def pass_named_as_positional(arg, no_tito):
          return before_star(
            no_tito,
            0,
            *no_tito,
            a = no_tito,
            b = arg,
            c = no_tito,
            q = no_tito,
            **no_tito,
          )

      def pass_named_as_star_star_q(arg, approximate_one, approximate_two, no_tito):
          return star_star_q(
            no_tito,
            no_tito,
            no_tito,
            *no_tito,
            no_tito,
            *no_tito,
            no_tito,
            c = no_tito,
            d = no_tito,
            q = arg,
            r = no_tito,
            **approximate_one,
            **approximate_two,
          )

      def pass_named_as_star_star_all(arg, approximate_one, approximate_two, no_tito):
          return star_star_all(
            no_tito,
            no_tito,
            no_tito,
            *no_tito,
            no_tito,
            *no_tito,
            no_tito,
            c = no_tito,
            d = no_tito,
            r = arg,
            **approximate_one,
            **approximate_two,
          )

      def pass_list_before_star(listarg, arg, no_tito):
          return before_star(
            no_tito,
            *listarg,
            arg,
            no_tito,
            *no_tito,
            no_tito,
            c = no_tito,
            d = no_tito,
            q = no_tito,
            r = no_tito,
          )

      def pass_list_at_star(listarg_one, listarg_two, approximate, no_tito):
          return at_star(
            no_tito,
            *listarg_one,
            approximate,
            *listarg_two,
            c = no_tito,
            d = no_tito,
            q = no_tito,
            r = no_tito,
          )
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "b"; titos = [Sinks.LocalReturn] }]
        "qualifier.before_star";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "*"; titos = [Sinks.LocalReturn] }]
        "qualifier.at_star";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "c"; titos = [Sinks.LocalReturn] }]
        "qualifier.after_star";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "**"; titos = [Sinks.LocalReturn] }]
        "qualifier.star_star_q";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "**"; titos = [Sinks.LocalReturn] }]
        "qualifier.star_star_all";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "arg"; titos = [Sinks.LocalReturn] }]
        "qualifier.pass_positional_before_star";
      outcome
        ~kind:`Function
        ~parameter_titos:
          [
            { name = "approximate"; titos = [Sinks.LocalReturn] };
            { name = "arg"; titos = [Sinks.LocalReturn] };
          ]
        "qualifier.pass_positional_at_star";
      outcome
        ~kind:`Function
        ~parameter_titos:
          [
            { name = "approximate"; titos = [Sinks.LocalReturn] };
            { name = "arg"; titos = [Sinks.LocalReturn] };
          ]
        "qualifier.pass_positional_at_star_plus_one";
      outcome
        ~kind:`Function
        ~parameter_titos:
          [
            { name = "approximate"; titos = [Sinks.LocalReturn] };
            { name = "arg"; titos = [Sinks.LocalReturn] };
          ]
        "qualifier.pass_positional_at_all_star";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "arg"; titos = [Sinks.LocalReturn] }]
        "qualifier.pass_named_after_star";
      outcome
        ~kind:`Function
        ~parameter_titos:[{ name = "arg"; titos = [Sinks.LocalReturn] }]
        "qualifier.pass_named_as_positional";
      outcome
        ~kind:`Function
        ~parameter_titos:
          [
            { name = "approximate_one"; titos = [Sinks.LocalReturn] };
            { name = "approximate_two"; titos = [Sinks.LocalReturn] };
            { name = "arg"; titos = [Sinks.LocalReturn] };
          ]
        "qualifier.pass_named_as_star_star_q";
      outcome
        ~kind:`Function
        ~parameter_titos:
          [
            { name = "approximate_one"; titos = [Sinks.LocalReturn] };
            { name = "approximate_two"; titos = [Sinks.LocalReturn] };
            { name = "arg"; titos = [Sinks.LocalReturn] };
          ]
        "qualifier.pass_named_as_star_star_all";
      outcome
        ~kind:`Function
        ~parameter_titos:
          [
            { name = "arg"; titos = [Sinks.LocalReturn] };
            { name = "listarg"; titos = [Sinks.LocalReturn] };
          ]
        "qualifier.pass_list_before_star";
      outcome
        ~kind:`Function
        ~parameter_titos:
          [
            { name = "approximate"; titos = [Sinks.LocalReturn] };
            { name = "listarg_one"; titos = [Sinks.LocalReturn] };
            { name = "listarg_two"; titos = [Sinks.LocalReturn] };
          ]
        "qualifier.pass_list_at_star";
    ]


let test_constructor_argument_tito context =
  let self_root =
    AccessPath.Root.PositionalParameter { position = 0; name = "self"; positional_only = false }
  in
  let tito_to_return = { name = "tito"; titos = [Sinks.LocalReturn] } in
  let tito_to_self = { name = "tito"; titos = [Sinks.ParameterUpdate self_root] } in
  assert_taint
    ~skip_for_pyrefly:true (* TODO(T225700656): Missing tito for DerivedData.__init__ *)
    ~context
    {|
      class Data:
        def __init__(self, tito, no_tito):
          self.field = tito

      def tito_via_construction(tito, no_tito):
          x = Data(tito, no_tito)
          return x

      def no_tito_via_construction(tito, no_tito):
          x = Data(tito, no_tito)
          return x.no_tito

      def precise_tito_via_construction(tito, no_tito):
          x = Data(tito, no_tito)
          return x.field

      def deep_tito_via_assignments(tito, no_tito):
          x = {}
          x.f = tito
          y = {}
          y.g = x
          return y

      def apply_deep_tito_some(tito, no_tito):
          x = deep_tito_via_assignments(tito, no_tito)
          return x.g.f

      def apply_deep_tito_none(tito, no_tito):
          x = deep_tito_via_assignments(tito, no_tito)
          return x.f.g

      def deep_tito_via_objects(tito, no_tito):
          x = { 'f': tito }
          y = { 'g': x }
          return y

      def apply_deep_tito_via_objects_some(tito, no_tito):
          x = deep_tito_via_objects(tito, no_tito)
          return x.g.f

      def apply_deep_tito_via_objects_none(tito, no_tito):
          x = deep_tito_via_objects(tito, no_tito)
          return x.f.g

      def deep_tito_wrapper(tito, no_tito):
          return deep_tito_via_assignments(tito, no_tito)

      def deep_tito_via_multiple(tito, no_tito):
          x = { 'f': tito, 'h': tito }
          y = { 'g': x }
          return y

      def test_tito_via_multiple_some(tito, no_tito):
          x = deep_tito_via_multiple(tito, no_tito)
          return x.g.f

      def test_tito_via_multiple_some_more(tito, no_tito):
          x = deep_tito_via_multiple(tito, no_tito)
          return x.g.h

      def test_tito_via_multiple_none(tito, no_tito):
          x = deep_tito_via_multiple(tito, no_tito)
          return x.g.q

      class DerivedData(Data):
        def __init__(self, tito, no_tito):
          super(Data, self).__init__(tito, no_tito)

    |}
    [
      outcome ~kind:`Method ~parameter_titos:[tito_to_self] "qualifier.Data.__init__";
      outcome ~kind:`Function ~parameter_titos:[tito_to_return] "qualifier.tito_via_construction";
      outcome ~kind:`Function ~parameter_titos:[] "qualifier.no_tito_via_construction";
      outcome
        ~kind:`Function
        ~parameter_titos:[tito_to_return]
        "qualifier.precise_tito_via_construction";
      outcome
        ~kind:`Function
        ~parameter_titos:[tito_to_return]
        "qualifier.deep_tito_via_assignments";
      outcome ~kind:`Function ~parameter_titos:[tito_to_return] "qualifier.apply_deep_tito_some";
      outcome ~kind:`Function ~parameter_titos:[] "qualifier.apply_deep_tito_none";
      outcome ~kind:`Function ~parameter_titos:[tito_to_return] "qualifier.deep_tito_via_objects";
      outcome
        ~kind:`Function
        ~parameter_titos:[tito_to_return]
        "qualifier.apply_deep_tito_via_objects_some";
      outcome ~kind:`Function ~parameter_titos:[] "qualifier.apply_deep_tito_via_objects_none";
      outcome ~kind:`Function ~parameter_titos:[tito_to_return] "qualifier.deep_tito_wrapper";
      outcome ~kind:`Function ~parameter_titos:[tito_to_return] "qualifier.deep_tito_via_multiple";
      outcome
        ~kind:`Function
        ~parameter_titos:[tito_to_return]
        "qualifier.test_tito_via_multiple_some";
      outcome
        ~kind:`Function
        ~parameter_titos:[tito_to_return]
        "qualifier.test_tito_via_multiple_some_more";
      outcome ~kind:`Function ~parameter_titos:[] "qualifier.test_tito_via_multiple_none";
      outcome ~kind:`Method ~parameter_titos:[tito_to_self] "qualifier.DerivedData.__init__";
    ]


let test_assignment context =
  assert_taint
    ~context
    ~skip_for_pyrefly:true (* TODO(T225700656): False negative with pyrefly *)
    {|
      import pysa

      def assigns_to_sink(assigned_to_sink):
        pysa._global_sink = assigned_to_sink
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "assigned_to_sink"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.assigns_to_sink";
    ];
  assert_taint
    ~context
    {|
      from pysa import ClassWithSinkAttribute
      
      def assigns_to_sink(assigned_to_sink):
        sink = ClassWithSinkAttribute()
        sink.attribute = assigned_to_sink
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "assigned_to_sink"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.assigns_to_sink";
    ];
  assert_taint
    ~context
    {|
      from typing import Optional
      from pysa import ClassWithSinkAttribute

      def assigns_to_sink(optional_sink: Optional[ClassWithSinkAttribute], assigned_to_sink):
        optional_sink.attribute = assigned_to_sink
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "assigned_to_sink"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.assigns_to_sink";
    ]


let test_access_paths context =
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      def access_downward_closed(arg):
        o = { 'a': arg }
        x = o.a
        _test_sink(x.g)

      def access_non_taint(arg):
        o = { 'a': arg }
        x = o.b
        _test_sink(x.g)
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.access_downward_closed";
      outcome ~kind:`Function ~parameter_sinks:[] "qualifier.access_non_taint";
    ];
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      def access_through_expression(arg):
        _test_sink(" ".join(arg))
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.access_through_expression";
    ]


let test_for_loops context =
  assert_taint
    ~context
    {|
      from pysa import _test_sink

      def sink_through_for(arg):
        for element in arg:
          _test_sink(element)
    |}
    [
      outcome
        ~kind:`Function
        ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
        "qualifier.sink_through_for";
    ]


let () =
  "backwardAnalysis"
  >::: [
         "access_paths" >:: test_access_paths;
         "actual_parameter_matching" >:: test_actual_parameter_matching;
         "apply_method_model_at_call_site" >:: test_apply_method_model_at_call_site;
         "assignment" >:: test_assignment;
         "asyncio_gather" >:: test_asyncio_gather;
         "call_tito" >:: test_call_taint_in_taint_out;
         "chained_call_path" >:: test_chained_call_path;
         "comprehensions" >:: test_comprehensions;
         "concatenate_taint_in_taint_out" >:: test_concatenate_taint_in_taint_out;
         "constructor_argument_tito" >:: test_constructor_argument_tito;
         "dictionary" >:: test_dictionary;
         "for_loops" >:: test_for_loops;
         "lambda" >:: test_lambda;
         "list" >:: test_list;
         "named_arguments" >:: test_named_arguments;
         "plus_taint_in_taint_out" >:: test_plus_taint_in_taint_out;
         "rce_and_test_sink" >:: test_rce_and_test_sink;
         "rce_sink" >:: test_rce_sink;
         "seqential_call_path" >:: test_sequential_call_path;
         "set" >:: test_set;
         "sink" >:: test_sink;
         "starred" >:: test_starred;
         "ternary" >:: test_ternary;
         "tito_sink" >:: test_tito_sink;
         "tito_via_receiver" >:: test_tito_via_receiver;
         "tuple" >:: test_tuple;
         "unary" >:: test_unary;
         "walrus" >:: test_walrus;
         "yield" >:: test_yield;
       ]
  |> Test.run
