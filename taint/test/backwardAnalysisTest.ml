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
open TestHelper


let assert_taint source expected =
  Annotated.Class.Attribute.Cache.clear ();
  let source =
    parse ~qualifier:(Access.create "qualifier") source
    |> Preprocessing.preprocess
  in
  let configuration = Test.mock_configuration in
  let environment = Test.environment ~configuration () in
  Service.Environment.populate ~configuration environment [source];
  TypeCheck.check ~configuration ~environment ~source |> ignore;
  let defines =
    source
    |> Preprocessing.defines ~include_stubs:true
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
        "Analyzing %a"
        Interprocedural.Callable.pp
        call_target
    in
    let backward = BackwardAnalysis.run ~environment ~define in
    let model = { Taint.Result.empty_model with backward } in
    Result.empty_model
    |> Result.with_model Taint.Result.kind model
    |> Fixpoint.add_predefined Fixpoint.Epoch.predefined call_target
  in
  let () = List.iter ~f:analyze_and_store_in_order defines in
  List.iter ~f:check_expectation expected


let test_plus_taint_in_taint_out _ =
  assert_taint
    {|
    def test_plus_taint_in_taint_out(tainted_parameter1, parameter2):
      tainted_value = tainted_parameter1 + 5
      return tainted_value
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.test_plus_taint_in_taint_out";
        sink_parameters = [];
        tito_parameters = ["tainted_parameter1"];
        returns = [];
        errors = [];
      };
    ]


let test_concatenate_taint_in_taint_out _ =
  assert_taint
    {|
      def test_concatenate_taint_in_taint_out(parameter0, tainted_parameter1):
        unused_parameter = parameter0
        command_unsafe = 'echo' + tainted_parameter1 + ' >> /dev/null'
        return command_unsafe
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.test_concatenate_taint_in_taint_out";
        sink_parameters = [];
        tito_parameters = ["tainted_parameter1"];
        returns = [];
        errors = [];
      };
    ]


let test_call_taint_in_taint_out _ =
  assert_taint
    {|
      def test_base_tito(parameter0, tainted_parameter1):
        return tainted_parameter1

      def test_called_tito(tainted_parameter0, parameter1):
        return test_base_tito(parameter1, tainted_parameter0)
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.test_base_tito";
        sink_parameters = [];
        tito_parameters = ["tainted_parameter1"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.test_called_tito";
        sink_parameters = [];
        tito_parameters = ["tainted_parameter0"];
        returns = [];
        errors = [];
      };
    ]


let test_sink _ =
  assert_taint
    {|
      def test_sink(parameter0, tainted_parameter1):
        unused_parameter = parameter0
        command_unsafe = 'echo' + tainted_parameter1 + ' >> /dev/null'
        __testSink(command_unsafe)
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.test_sink";
        sink_parameters = [
          {
            name = "tainted_parameter1";
            sinks = [Taint.Sinks.Test];
          };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
    ]


let test_rce_sink _ =
  assert_taint
    {|
      def test_rce_sink(parameter0, tainted_parameter1):
        unused_parameter = parameter0
        command_unsafe = 'echo' + tainted_parameter1 + ' >> /dev/null'
        eval(command_unsafe)
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.test_rce_sink";
        sink_parameters = [
          {
            name = "tainted_parameter1";
            sinks = [Taint.Sinks.RemoteCodeExecution];
          }
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
    ]


let test_hardcoded_rce_sink _ =
  assert_taint
    {|
      def test_hardcoded_rce_sink(input):
        subprocess.call(input, shell=True)

      def test_hardcoded_rce_sink_with_shell_false_explicit(input):
        subprocess.call(input, shell=False)

      def test_hardcoded_rce_sink_with_shell_false_implicit(input):
        subprocess.call(input, shell=False)

      def test_hardcoded_rce_sink_with_string_argument(input: str):
        subprocess.check_call(input, shell=True)

      def test_hardcoded_rce_sink_with_list_literal():
        subprocess.call([], shell=True)

      def test_hardcoded_rce_sink_with_list_argument(input: typing.List[str]):
        subprocess.call(input, shell=True)
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.test_hardcoded_rce_sink";
        sink_parameters = [{ name = "input"; sinks = [Taint.Sinks.RemoteCodeExecution] }];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.test_hardcoded_rce_sink_with_shell_false_explicit";
        sink_parameters = [];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.test_hardcoded_rce_sink_with_shell_false_implicit";
        sink_parameters = [];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.test_hardcoded_rce_sink_with_string_argument";
        sink_parameters = [{ name = "input"; sinks = [Taint.Sinks.RemoteCodeExecution] }];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.test_hardcoded_rce_sink_with_list_literal";
        sink_parameters = [];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.test_hardcoded_rce_sink_with_list_argument";
        sink_parameters = [];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
    ]


let test_rce_and_test_sink _ =
  assert_taint
    {|
      def test_rce_and_test_sink(test_only, rce_only, both):
        __testSink(test_only)
        eval(rce_only)
        if True:
          __testSink(both)
        else:
          eval(both)
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.test_rce_and_test_sink";
        sink_parameters = [
          { name = "test_only"; sinks = [Taint.Sinks.Test]; };
          { name = "rce_only"; sinks = [Taint.Sinks.RemoteCodeExecution]; };
          { name = "both"; sinks = [Taint.Sinks.RemoteCodeExecution; Taint.Sinks.Test]; };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      }
    ]


let test_tito_sink _ =
  assert_taint
    {|
      def test_base_tito(parameter0, tainted_parameter1):
        return tainted_parameter1

      def test_called_tito(tainted_parameter0, parameter1):
        return test_base_tito(parameter1, tainted_parameter0)

      def test_tito_sink(parameter0, tainted_parameter1):
        tainted = test_called_tito(tainted_parameter1, parameter0)
        __testSink(tainted)
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.test_tito_sink";
        sink_parameters = [
          { name = "tainted_parameter1"; sinks = [Taint.Sinks.Test]; };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
    ]


let test_apply_method_model_at_call_site _ =
  assert_taint
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
    [
      {
        kind = `Function;
        define_name = "qualifier.taint_across_methods";
        sink_parameters = [
          { name = "tainted_parameter"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
    ];

  assert_taint
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
    [
      {
        kind = `Function;
        define_name = "qualifier.taint_across_methods";
        sink_parameters = [];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
    ];

  assert_taint
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
    [
      {
        kind = `Function;
        define_name = "qualifier.taint_across_methods";
        sink_parameters = [
          { name = "tainted_parameter"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
    ];

  assert_taint
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
    [
      {
        kind = `Function;
        define_name = "qualifier.taint_across_methods";
        sink_parameters = [];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
    ] ;

  assert_taint
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
    [
      {
        kind = `Function;
        define_name = "qualifier.taint_across_union_receiver_types";
        sink_parameters = [
          { name = "tainted_parameter"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
    ];

  assert_taint
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
    [
      {
        kind = `Method;
        define_name = "qualifier.Foo.qux";
        sink_parameters = [];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Method;
        define_name = "qualifier.Baz.qux";
        sink_parameters = [
          { name = "tainted_parameter"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.taint_across_union_receiver_types";
        sink_parameters = [
          { name = "tainted_parameter"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
    ]


let test_tito_via_receiver _ =
  assert_taint
    {|
      class Foo:
        def tito(self, argument1):
            return self.f

      def tito_via_receiver(parameter):
        x = Foo()
        x.f = parameter
        return f.tito
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.tito_via_receiver";
        sink_parameters = [];
        tito_parameters = ["parameter"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.Foo.tito";
        sink_parameters = [];
        tito_parameters = ["self"];
        returns = [];
        errors = [];
      };
    ]


let test_sequential_call_path _ =
  (* Testing the setup to get this out of the way. *)
  assert_taint
    {|
      class Foo:
        def sink(self, argument) -> Foo:
            __testSink(argument)
            return self
    |}
    [
      {
        kind = `Method;
        define_name = "qualifier.Foo.sink";
        sink_parameters = [
          { name = "argument"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = ["self"];
        returns = [];
        errors = [];
      };
    ];

  assert_taint
    {|
      class Foo:
        def sink(self, argument) -> Foo:
            __testSink(argument)
            return self

      def sequential_with_single_sink(first, second, third):
        x = Foo()
        x.sink(first)
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.sequential_with_single_sink";
        sink_parameters = [
          { name = "first"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
    ];
  assert_taint
    {|
      class Foo:
        def sink(self, argument) -> Foo:
            __testSink(argument)
            return self

      def sequential_with_two_sinks(first, second, third):
        x = Foo()
        x.sink(first)
        x.sink(second)
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.sequential_with_two_sinks";
        sink_parameters = [
          { name = "first"; sinks = [Taint.Sinks.Test] };
          { name = "second"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
    ];
  assert_taint
    {|
      class Foo:
        def sink(self, argument) -> Foo:
            __testSink(argument)
            return self

      def sequential_with_redefine(first, second, third):
        x = Foo()
        x.sink(first)
        x = Foo()
        x.sink(second)
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.sequential_with_redefine";
        sink_parameters = [
          { name = "first"; sinks = [Taint.Sinks.Test] };
          { name = "second"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
    ];
  assert_taint
    {|
      class Foo:
        def sink(self, argument) -> Foo:
            __testSink(argument)
            return self

      def sequential_with_distinct_sinks(first, second, third):
        x = Foo()
        x.sink(first)
        a = Foo()
        a.sink(second)
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.sequential_with_distinct_sinks";
        sink_parameters = [
          { name = "first"; sinks = [Taint.Sinks.Test] };
          { name = "second"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
    ];
  assert_taint
    {|
      class Foo:
        def sink(self, argument) -> Foo:
            __testSink(argument)
            return self

      def sequential_with_self_propagation(first, second, third):
        x = Foo()
        x = x.sink(first)
        x.sink(second)
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.sequential_with_self_propagation";
        sink_parameters = [
          { name = "first"; sinks = [Taint.Sinks.Test] };
          { name = "second"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
    ]


let test_chained_call_path _ =
  assert_taint
    {|
      class Foo:
        def sink(self, argument1) -> Foo:
            __testSink(argument1)
            return self

      def chained(parameter0, parameter1, parameter2):
        x = Foo()
        x.sink(parameter0).sink(parameter2)
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.chained";
        sink_parameters = [
          { name = "parameter0"; sinks = [Taint.Sinks.Test] };
          { name = "parameter2"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
    ];
  assert_taint
    {|
      class Foo:
        def tito(self, argument1) -> Foo:
            return self

        def sink(self, argument1) -> Foo:
            __testSink(argument1)
            return self

      def chained_with_tito(parameter0, parameter1, parameter2):
        x = Foo()
        x.sink(parameter0).tito(parameter1).sink(parameter2)
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.chained_with_tito";
        sink_parameters = [
          { name = "parameter0"; sinks = [Taint.Sinks.Test] };
          { name = "parameter2"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
    ]


let test_dictionary _ =
  assert_taint
    {|
      def dictionary_sink(arg):
        {
          "a": __testSink(arg),
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
      {
        kind = `Function;
        define_name = "qualifier.dictionary_sink";
        sink_parameters = [
          { name = "arg"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.dictionary_tito";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.dictionary_same_index";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.dictionary_different_index";
        sink_parameters = [];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.dictionary_unknown_read_index";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.dictionary_unknown_write_index";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
    ]


let test_comprehensions _ =
  assert_taint
    {|
      def sink_in_iterator(arg):
          [ x for x in __testSink(arg) ]

      def sink_in_expression(data):
          [ __testSink(x) for x in data ]

      def tito(data):
          return [x for x in data ]

      def sink_in_set_iterator(arg):
          { x for x in __testSink(arg) }

      def sink_in_set_expression(data):
          { __testSink(x) for x in data }

      def tito_set(data):
          return { x for x in data }

      def sink_in_generator_iterator(arg):
          gen = (x for x in __testSink(arg))

      def sink_in_generator_expression(data):
          gen = (__testSink(x) for x in data)

      def tito_generator(data):
          return (x for x in data)
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.sink_in_iterator";
        sink_parameters = [
          { name = "arg"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.sink_in_expression";
        sink_parameters = [
          { name = "data"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tito";
        sink_parameters = [];
        tito_parameters = ["data"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.sink_in_set_iterator";
        sink_parameters = [
          { name = "arg"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.sink_in_set_expression";
        sink_parameters = [
          { name = "data"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tito_set";
        sink_parameters = [];
        tito_parameters = ["data"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.sink_in_generator_iterator";
        sink_parameters = [
          { name = "arg"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.sink_in_generator_expression";
        sink_parameters = [
          { name = "data"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tito_generator";
        sink_parameters = [];
        tito_parameters = ["data"];
        returns = [];
        errors = [];
      };
    ]


let test_list _ =
  assert_taint
    {|
      def sink_in_list(arg):
          return [ 1, __testSink(arg), "foo" ]

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
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.sink_in_list";
        sink_parameters = [
          { name = "arg"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.list_same_index";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.list_different_index";
        sink_parameters = [];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.list_unknown_index";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.list_pattern_same_index";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.list_pattern_different_index";
        sink_parameters = [];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.list_pattern_star_index";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
    ]


let test_tuple _ =
  assert_taint
    {|
      def sink_in_tuple(arg):
          return ( 1, __testSink(arg), "foo" )

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
      {
        kind = `Function;
        define_name = "qualifier.sink_in_tuple";
        sink_parameters = [
          { name = "arg"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tuple_same_index";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tuple_different_index";
        sink_parameters = [];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tuple_unknown_index";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tuple_pattern_same_index";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tuple_pattern_different_index";
        sink_parameters = [];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
    ]


let test_lambda _ =
  assert_taint
    {|
      def sink_in_lambda(arg):
          f = lambda x : x + __testSink(arg)

      def lambda_tito(arg):
          f = lambda x : x + arg
          return f
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.sink_in_lambda";
        sink_parameters = [
          { name = "arg"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.lambda_tito";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
    ]


let test_set _ =
  assert_taint
    {|
      def sink_in_set(arg):
          return { 1, __testSink(arg), "foo" }

      def set_index(arg):
          set = { 1, arg, "foo" }
          return set[2]

      def set_unknown_index(arg, index):
          set = { 1, arg, "foo" }
          return set[index]
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.sink_in_set";
        sink_parameters = [
          { name = "arg"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.set_index";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.set_unknown_index";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
    ]


let test_starred _ =
  assert_taint
    {|
      def sink_in_starred(arg):
          __tito( *[ 1, __testSink(arg), "foo" ] )

      def sink_in_starred_starred(arg):
          __tito( **{
              "a": 1,
              "b": __testSink(arg),
              "c": "foo",
          })

      def tito_in_starred(arg):
          return __tito( *[ 1, arg, "foo" ] )

      def tito_in_starred_starred(arg):
          return __tito( **{
              "a": 1,
              "b": arg,
              "c": "foo",
          })
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.sink_in_starred";
        sink_parameters = [
          { name = "arg"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.sink_in_starred_starred";
        sink_parameters = [
          { name = "arg"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tito_in_starred";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tito_in_starred_starred";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
    ]


let test_ternary _ =
  assert_taint
    {|
      def sink_in_then(arg, cond):
          x = __testSink(arg) if cond else None

      def sink_in_else(arg, cond):
          x = "foo" if cond else __testSink(arg)

      def sink_in_both(arg1, arg2, cond):
          x = __testSink(arg1) if cond else __testSink(arg2)

      def sink_in_cond(arg1, arg2, cond):
          x = arg1 if __testSink(cond) else arg2

      def tito_in_then(arg, cond):
          return arg if cond else None

      def tito_in_else(arg, cond):
          return "foo" if cond else arg

      def tito_in_both(arg1, arg2, cond):
          return arg1 if cond else arg2
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.sink_in_then";
        sink_parameters = [
          { name = "arg"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.sink_in_else";
        sink_parameters = [
          { name = "arg"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.sink_in_both";
        sink_parameters = [
          { name = "arg1"; sinks = [Taint.Sinks.Test] };
          { name = "arg2"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.sink_in_cond";
        sink_parameters = [
          { name = "cond"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tito_in_then";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tito_in_else";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tito_in_both";
        sink_parameters = [];
        tito_parameters = ["arg1"; "arg2"];
        returns = [];
        errors = [];
      };
    ]


let test_unary _ =
  assert_taint
    {|
      def sink_in_unary(arg):
          x = not __testSink(arg)

      def tito_via_unary(arg):
          return not arg
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.sink_in_unary";
        sink_parameters = [
          { name = "arg"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tito_via_unary";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
    ]


let test_yield _ =
  assert_taint
    {|
      def sink_in_yield(arg):
          yield __testSink(arg)

      def tito_via_yield(arg):
          yield arg

      def sink_in_yield_from(arg):
          yield from __testSink(arg)

      def tito_via_yield_from(arg):
          yield from arg
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.sink_in_yield";
        sink_parameters = [
          { name = "arg"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tito_via_yield";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.sink_in_yield_from";
        sink_parameters = [
          { name = "arg"; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tito_via_yield_from";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
    ]


let test_named_arguments _ =
  assert_taint
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
      {
        kind = `Function;
        define_name = "qualifier.with_kw";
        sink_parameters = [];
        tito_parameters = ["**"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.no_kw_tito";
        sink_parameters = [];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.no_kw_tito_with_named_args";
        sink_parameters = [];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.kw_tito_with_named_args";
        sink_parameters = [];
        tito_parameters = ["arg1"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.kw_tito_with_dict";
        sink_parameters = [];
        tito_parameters = ["dict"];
        returns = [];
        errors = [];
      };
    ]


let test_actual_parameter_matching _ =
  assert_taint
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
      {
        kind = `Function;
        define_name = "qualifier.before_star";
        sink_parameters = [];
        tito_parameters = ["b"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.at_star";
        sink_parameters = [];
        tito_parameters = ["*"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.after_star";
        sink_parameters = [];
        tito_parameters = ["c"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.star_star_q";
        sink_parameters = [];
        tito_parameters = ["**"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.star_star_all";
        sink_parameters = [];
        tito_parameters = ["**"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.pass_positional_before_star";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.pass_positional_at_star";
        sink_parameters = [];
        tito_parameters = ["approximate"; "arg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.pass_positional_at_star_plus_one";
        sink_parameters = [];
        tito_parameters = ["approximate"; "arg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.pass_positional_at_all_star";
        sink_parameters = [];
        tito_parameters = ["approximate"; "arg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.pass_named_after_star";
        sink_parameters = [];
        tito_parameters = ["approximate"; "arg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.pass_named_as_positional";
        sink_parameters = [];
        tito_parameters = ["arg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.pass_named_as_star_star_q";
        sink_parameters = [];
        tito_parameters = ["approximate_one"; "approximate_two"; "arg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.pass_named_as_star_star_all";
        sink_parameters = [];
        tito_parameters = ["approximate_one"; "approximate_two"; "arg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.pass_list_before_star";
        sink_parameters = [];
        tito_parameters = ["arg"; "listarg"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.pass_list_at_star";
        sink_parameters = [];
        tito_parameters = ["approximate"; "listarg_one"; "listarg_two"];
        returns = [];
        errors = [];
      };
    ]


let test_constructor_argument_tito _ =
  assert_taint
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
    |}
    [
      {
        kind = `Method;
        define_name = "qualifier.Data.__init__";
        sink_parameters = [];
        tito_parameters = ["self"; "tito"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tito_via_construction";
        sink_parameters = [];
        tito_parameters = ["tito"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.no_tito_via_construction";
        sink_parameters = [];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.precise_tito_via_construction";
        sink_parameters = [];
        tito_parameters = ["tito"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.deep_tito_via_assignments";
        sink_parameters = [];
        tito_parameters = ["tito"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.apply_deep_tito_some";
        sink_parameters = [];
        tito_parameters = ["tito"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.apply_deep_tito_none";
        sink_parameters = [];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.deep_tito_via_objects";
        sink_parameters = [];
        tito_parameters = ["tito"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.apply_deep_tito_via_objects_some";
        sink_parameters = [];
        tito_parameters = ["tito"];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.apply_deep_tito_via_objects_none";
        sink_parameters = [];
        tito_parameters = [];
        returns = [];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.deep_tito_wrapper";
        returns = [];
        sink_parameters = [];
        tito_parameters = ["tito"];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.deep_tito_via_multiple";
        returns = [];
        sink_parameters = [];
        tito_parameters = ["tito"];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.test_tito_via_multiple_some";
        returns = [];
        sink_parameters = [];
        tito_parameters = ["tito"];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.test_tito_via_multiple_some_more";
        returns = [];
        sink_parameters = [];
        tito_parameters = ["tito"];
        errors = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.test_tito_via_multiple_none";
        returns = [];
        sink_parameters = [];
        tito_parameters = [];
        errors = [];
      };
    ]


let () =
  "taint">:::[
    "plus_taint_in_taint_out">::test_plus_taint_in_taint_out;
    "concatenate_taint_in_taint_out">::test_concatenate_taint_in_taint_out;
    "rce_sink">::test_rce_sink;
    "hardcoded_rce_sink">::test_hardcoded_rce_sink;
    "test_sink">::test_sink;
    "rce_and_test_sink">::test_rce_and_test_sink;
    "test_call_tito">::test_call_taint_in_taint_out;
    "test_tito_sink">::test_tito_sink;
    "test_apply_method_model_at_call_site">::test_apply_method_model_at_call_site;
    "test_seqential_call_path">::test_sequential_call_path;
    "test_chained_call_path">::test_chained_call_path;
    "test_dictionary">::test_dictionary;
    "test_comprehensions">::test_comprehensions;
    "test_list">::test_list;
    "test_lambda">::test_lambda;
    "test_set">::test_set;
    "test_starred">::test_starred;
    "test_ternary">::test_ternary;
    "test_tuple">::test_tuple;
    "test_unary">::test_unary;
    "test_yield">::test_yield;
    "test_named_arguments">::test_named_arguments;
    "test_actual_parameter_matching">::test_actual_parameter_matching;
    "test_constructor_argument_tito">::test_constructor_argument_tito;
  ]
  |> TestHelper.run_with_taint_models
