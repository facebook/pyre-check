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


let assert_taint source expected =
  Annotated.Class.Attribute.Cache.clear ();
  let source =
    parse ~qualifier:(Access.create "qualifier") source
    |> Preprocessing.preprocess
  in
  let configuration = Test.mock_configuration in
  let environment = Test.environment ~configuration () in
  Service.Environment.populate environment [source];
  TypeCheck.check configuration environment source |> ignore;
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
        "Analyzing %s"
        (Interprocedural.Callable.show call_target)
    in
    let backward = BackwardAnalysis.run ~environment ~define in
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

    (* Check sinks. *)
    assert_equal
      (Map.length expected_sinks)
      (Map.length taint_map)
      ~msg:(Format.sprintf "Define %s: List of tainted parameters differ in length." define_name);
    Int.Map.iter2 ~f:check_each_sink_position expected_sinks taint_map;

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
    {|
    def test_plus_taint_in_taint_out(tainted_parameter1, parameter2):
      tainted_value = tainted_parameter1 + 5
      return tainted_value
    |}
    [
      {
        define_name = "qualifier.test_plus_taint_in_taint_out";
        taint_sink_parameters = [];
        tito_parameters = [0];
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
        define_name = "qualifier.test_concatenate_taint_in_taint_out";
        taint_sink_parameters = [];
        tito_parameters = [1];
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
        define_name = "qualifier.test_sink";
        taint_sink_parameters = [
          { position = 1; sinks = [Taint.Sinks.Test]; };
        ];
        tito_parameters = [];
      };
    ]


let test_rce_sink _ =
  assert_taint
    {|
      def test_rce_sink(parameter0, tainted_parameter1):
        unused_parameter = parameter0
        command_unsafe = 'echo' + tainted_parameter1 + ' >> /dev/null'
        __eval(command_unsafe)
    |}
    [
      {
        define_name = "qualifier.test_rce_sink";
        taint_sink_parameters = [
          { position = 1; sinks = [Taint.Sinks.RemoteCodeExecution]; }
        ];
        tito_parameters = [];
      };
    ]


let test_rce_and_test_sink _ =
  assert_taint
    {|
      def test_rce_and_test_sink(test_only, rce_only, both):
        __testSink(test_only)
        __eval(rce_only)
        if True:
          __testSink(both)
        else:
          __eval(both)
    |}
    [
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
        define_name = "qualifier.test_tito_sink";
        taint_sink_parameters = [
          { position = 1; sinks = [Taint.Sinks.Test]; };
        ];
        tito_parameters = [];
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
        define_name = "qualifier.taint_across_methods";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
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
        define_name = "qualifier.taint_across_methods";
        taint_sink_parameters = [];
        tito_parameters = [];
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
        define_name = "qualifier.taint_across_methods";
        taint_sink_parameters = [
          { position = 1; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
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
        define_name = "qualifier.taint_across_methods";
        taint_sink_parameters = [];
        tito_parameters = [];
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

      def taint_across_union_receiver_types(condition, tainted_parameter):
        if condition:
          f = Foo()
        else:
          f = Bar()

        return f.qux(tainted_parameter)
    |}
    [
      {
        define_name = "qualifier.taint_across_union_receiver_types";
        taint_sink_parameters = [
          { position = 1; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
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
        define_name = "qualifier.Foo.sink";
        taint_sink_parameters = [
          { position = 1; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [0];
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
        define_name = "qualifier.sequential_with_single_sink";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
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
        define_name = "qualifier.sequential_with_two_sinks";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
          { position = 1; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
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
        define_name = "qualifier.sequential_with_redefine";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
          { position = 1; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
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
        define_name = "qualifier.sequential_with_distinct_sinks";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
          { position = 1; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
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
        define_name = "qualifier.sequential_with_self_propagation";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
          { position = 1; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
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
        define_name = "qualifier.chained";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
          { position = 2; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
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
        define_name = "qualifier.chained_with_tito";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
          { position = 2; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
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
        define_name = "qualifier.dictionary_sink";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
      };
      {
        define_name = "qualifier.dictionary_tito";
        taint_sink_parameters = [];
        tito_parameters = [0];
      };
      {
        define_name = "qualifier.dictionary_same_index";
        taint_sink_parameters = [];
        tito_parameters = [0];
      };
      {
        define_name = "qualifier.dictionary_different_index";
        taint_sink_parameters = [];
        tito_parameters = [];
      };
      {
        define_name = "qualifier.dictionary_unknown_read_index";
        taint_sink_parameters = [];
        tito_parameters = [0];
      };
      {
        define_name = "qualifier.dictionary_unknown_write_index";
        taint_sink_parameters = [];
        tito_parameters = [0];
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
    |}
    [
      {
        define_name = "qualifier.sink_in_iterator";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
      };
      {
        define_name = "qualifier.sink_in_expression";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
      };
      {
        define_name = "qualifier.tito";
        taint_sink_parameters = [];
        tito_parameters = [0];
      };
      {
        define_name = "qualifier.sink_in_set_iterator";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
      };
      {
        define_name = "qualifier.sink_in_set_expression";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
      };
      {
        define_name = "qualifier.tito_set";
        taint_sink_parameters = [];
        tito_parameters = [0];
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
    |}
    [
      {
        define_name = "qualifier.sink_in_list";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
      };
      {
        define_name = "qualifier.list_same_index";
        taint_sink_parameters = [];
        tito_parameters = [0];
      };
      {
        define_name = "qualifier.list_different_index";
        taint_sink_parameters = [];
        tito_parameters = [];
      };
      {
        define_name = "qualifier.list_unknown_index";
        taint_sink_parameters = [];
        tito_parameters = [0];
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
        define_name = "qualifier.sink_in_lambda";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
      };
      {
        define_name = "qualifier.lambda_tito";
        taint_sink_parameters = [];
        tito_parameters = [0];
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
        define_name = "qualifier.sink_in_set";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
      };
      {
        define_name = "qualifier.set_index";
        taint_sink_parameters = [];
        tito_parameters = [0];
      };
      {
        define_name = "qualifier.set_unknown_index";
        taint_sink_parameters = [];
        tito_parameters = [0];
      };
    ]


let test_starred _ =
  assert_taint
    {|
      def sink_in_starred(arg):
          __tito(*[ 1, __testSink(arg), "foo" ])

      def sink_in_starred_starred(arg):
          __tito(**{
              "a": 1,
              "b": __testSink(arg),
              "c": "foo",
          })

      def tito_in_starred(arg):
          return __tito(*[ 1, arg, "foo" ])

      def tito_in_starred_starred(arg):
          return __tito(**{
              "a": 1,
              "b": arg,
              "c": "foo",
          })
    |}
    [
      {
        define_name = "qualifier.sink_in_starred";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
      };
      {
        define_name = "qualifier.sink_in_starred_starred";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
      };
      {
        define_name = "qualifier.tito_in_starred";
        taint_sink_parameters = [];
        tito_parameters = [0];
      };
      {
        define_name = "qualifier.tito_in_starred_starred";
        taint_sink_parameters = [];
        tito_parameters = [0];
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
        define_name = "qualifier.sink_in_then";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
      };
      {
        define_name = "qualifier.sink_in_else";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
      };
      {
        define_name = "qualifier.sink_in_both";
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
          { position = 1; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
      };
      {
        define_name = "qualifier.sink_in_cond";
        taint_sink_parameters = [
          { position = 2; sinks = [Taint.Sinks.Test] };
        ];
        tito_parameters = [];
      };
      {
        define_name = "qualifier.tito_in_then";
        taint_sink_parameters = [];
        tito_parameters = [0];
      };
      {
        define_name = "qualifier.tito_in_else";
        taint_sink_parameters = [];
        tito_parameters = [0];
      };
      {
        define_name = "qualifier.tito_in_both";
        taint_sink_parameters = [];
        tito_parameters = [0; 1];
      };
    ]


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
    "test_seqential_call_path">::test_sequential_call_path;
    "test_chained_call_path">::test_chained_call_path;
    "test_dictionary">::test_dictionary;
    "test_comprehensions">::test_comprehensions;
    "test_list">::test_list;
    "test_lambda">::test_lambda;
    "test_set">::test_set;
    "test_starred">::test_starred;
    "test_ternary">::test_ternary;
  ]
  |> Test.run_with_taint_models
