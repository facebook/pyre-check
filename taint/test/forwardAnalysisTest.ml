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

open Test
open Interprocedural
open TestHelper


let assert_taint ?(qualifier = Access.create "qualifier") ?models source expect =
  let configuration = Test.mock_configuration in

  let source =
    parse ~qualifier source
    |> Preprocessing.preprocess
  in

  let environment =
    let models =
      models
      >>| (fun model -> [Test.parse model])
      |> Option.value ~default:[]
    in
    let environment = Test.environment ~sources:(Test.typeshed_stubs @ models) ~configuration () in
    Service.Environment.populate ~configuration environment [source];
    environment
  in

  models
  >>| Test.trim_extra_indentation
  >>| (fun model_source -> Service.StaticAnalysis.add_models ~environment ~model_source)
  |> ignore;

  TypeCheck.check ~configuration ~environment ~source |> ignore;
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
        "Analyzing %a"
        Interprocedural.Callable.pp
        call_target
    in
    let forward, _errors = ForwardAnalysis.run ~environment ~define in
    let model = { Taint.Result.empty_model with forward } in
    Result.empty_model
    |> Result.with_model Taint.Result.kind model
    |> Fixpoint.add_predefined Fixpoint.Epoch.predefined call_target;
  in
  let () = List.iter ~f:analyze_and_store_in_order defines in
  List.iter ~f:check_expectation expect



let test_no_model _ =
  let assert_no_model _ =
    assert_taint
      ?qualifier:None
      {|
      def copy_source():
        pass
      |}
      [
        {
          kind = `Function;
          define_name = "does_not_exist";
          returns = [];
          errors = [];
          sink_parameters = [];
          tito_parameters = [];
        };
      ]
  in
  assert_raises
    (OUnitTest.OUnit_failure "model not found for `Function (\"does_not_exist\")")
    assert_no_model


let test_simple_source _ =
  assert_taint
    {|
      def simple_source():
        return __testSource()
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.simple_source";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ];
  assert_taint
    ~models:{|
      def custom_source() -> TaintSource[Test]: ...
    |}
    {|
      def simple_source():
        return custom_source()
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.simple_source";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ]


let test_hardcoded_source _ =
  assert_taint
    ~models:{|
      django.http.Request.GET: TaintSource[UserControlled] = ...
      django.http.Request.POST: TaintSource[UserControlled] = ...
    |}
    {|
      def get(request: django.http.Request):
        return request.GET
      def post(request: django.http.Request):
        return request.POST
      def meta(request: django.http.Request):
        return request.META
      def files(request: django.http.Request):
        return request.FILES
    |}
    [
      {
        kind = `Object;
        define_name = "django.http.Request.GET";
        returns = [Sources.UserControlled];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.get";
        returns = [Sources.UserControlled];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.post";
        returns = [Sources.UserControlled];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ];
  assert_taint
    ~models:{|
      django.http.Request.GET: TaintSource[UserControlled] = ...
      def dict.__getitem__(self: TaintInTaintOut[LocalReturn], key): ...
    |}
    {|
      def get_field(request: django.http.Request):
        return request.GET['field']
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.get_field";
        returns = [Sources.UserControlled];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ];
  assert_taint
    ~models:{|
      os.environ: TaintSource[UserControlled] = ...
    |}
    {|
      def get_environment_variable():
        return os.environ
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.get_environment_variable";
        returns = [Sources.UserControlled];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ];
  assert_taint
    ~models:{|
      os.environ: TaintSource[UserControlled] = ...
      def dict.__getitem__(self: TaintInTaintOut[LocalReturn], key): ...
    |}
    {|
      def get_environment_variable_with_getitem():
        return os.environ['BAD']
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.get_environment_variable_with_getitem";
        returns = [Sources.UserControlled];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ];
  assert_taint
    {|
      class Request(django.http.Request): ...

      def get_field(request: Request):
        return request.GET['field']
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.get_field";
        returns = [Sources.UserControlled];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ]


let test_local_copy _ =
  assert_taint
    {|
      def copy_source():
        var = __testSource()
        return var
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.copy_source";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ]


let test_access_paths _ =
  assert_taint
    {|
      def access_downward_closed():
        o = { 'a': __testSource() }
        x = o.a
        return x.g

      def access_non_taint():
        o = { 'a': __testSource() }
        x = o.b
        return x.g
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.access_downward_closed";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.access_non_taint";
        returns = [];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ]


let test_class_model _ =
  assert_taint
    {|
      class Foo:
        def bar():
          return __testSource()
    |}
    [
      {
        kind = `Method;
        define_name = "qualifier.Foo.bar";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ]


let test_apply_method_model_at_call_site _ =
  assert_taint
    {|
      class Foo:
        def qux():
          return __testSource()

      class Bar:
        def qux():
          return not_tainted()

      def taint_across_methods():
        f = Foo()
        return f.qux()
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.taint_across_methods";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ];

  assert_taint
    {|
      class Foo:
        def qux():
          return __testSource()

      class Bar:
        def qux():
          return not_tainted()

      def taint_across_methods():
        f = Bar()
        return f.qux()
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.taint_across_methods";
        returns = [];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ];

  assert_taint
    {|
      class Foo:
        def qux():
          return __testSource()

      class Bar:
        def qux():
          return not_tainted()

      def taint_across_methods(f: Foo):
        return f.qux()
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.taint_across_methods";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      }
    ];

  assert_taint
    {|
      class Foo:
        def qux():
          return __testSource()

      class Bar:
        def qux():
          return not_tainted()

      def taint_across_methods(f: Bar):
        return f.qux()
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.taint_across_methods";
        returns = [];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ];

  assert_taint
    {|
      class Foo:
        def qux():
          return __testSource()

      class Bar:
        def qux():
          return not_tainted()

      def taint_with_union_type(condition):
        if condition:
          f = Foo()
        else:
          f = Bar()

        return f.qux()
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.taint_with_union_type";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ];

  assert_taint
    {|
      class Foo:
        def qux():
          return not_tainted()

      class Bar:
        def qux():
          return not_tainted()

      class Baz:
        def qux():
          return __testSource()

      def taint_with_union_type(condition):
        if condition:
          f = Foo()
        elif condition > 1:
          f = Bar()
        else:
          f = Baz()

        return f.qux()
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.taint_with_union_type";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ];

  assert_taint
    {|
      class Indirect:
        def direct(self) -> Direct: ...

      class Direct:
        def source():
          return __testSource()

      def taint_indirect_concatenated_call(indirect: Indirect):
        direct = indirect.direct()
        return direct.source()
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.taint_indirect_concatenated_call";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      }
    ];
  assert_taint
    {|
      class Indirect:
        def direct(self) -> Direct: ...

      class Direct:
        def source():
          return __testSource()

      def taint_indirect_concatenated_call(indirect: Indirect):
        return indirect.direct().source()
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.taint_indirect_concatenated_call";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      }
    ]


let test_taint_in_taint_out_application _ =
  assert_taint
    {|
      def simple_source():
        return __testSource()

      def taint_with_tito():
        y = simple_source()
        x = __tito(y)
        return x
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.simple_source";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ];

  assert_taint
    {|
      def simple_source():
        return __testSource()

      def __no_tito(y):
        pass

      def no_tito_taint():
        y = simple_source()
        x = __no_tito(y)
        return x
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.no_tito_taint";
        returns = [];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ]


let test_dictionary _ =
  assert_taint
    {|
      def dictionary_source():
        return {
          "a": __testSource(),
        }

      def dictionary_same_index():
        dict = {
          "a": __testSource(),
        }
        return dict["a"]

      def dictionary_different_index():
        dict = {
          "a": __testSource(),
        }
        return dict["b"]

      def dictionary_unknown_read_index(index):
        dict = {
          "a": __testSource(),
        }
        return dict[index]

      def dictionary_unknown_write_index(index):
        dict = {
          index: __testSource(),
        }
        return dict["a"]
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.dictionary_source";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.dictionary_same_index";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.dictionary_different_index";
        returns = [];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.dictionary_unknown_read_index";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.dictionary_unknown_write_index";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ]


let test_comprehensions _ =
  assert_taint
    {|
      def source_in_iterator():
          return [ x for x in __testSource() ]

      def source_in_expression(data):
          return [ __testSource() for x in data ]

      def source_in_set_iterator():
          return { x for x in __testSource() }

      def source_in_set_expression(data):
          return { __testSource() for x in data }

      def source_in_generator_iterator():
          return (x for x in __testSource())

      def source_in_generator_expression(data):
          return ( __testSource() for x in data )
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.source_in_iterator";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.source_in_expression";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.source_in_set_iterator";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.source_in_set_expression";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.source_in_generator_iterator";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.source_in_generator_expression";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ]


let test_list _ =
  assert_taint
    {|
      def source_in_list():
          return [ 1, __testSource(), "foo" ]

      def list_same_index():
          list = [ 1, __testSource(), "foo" ]
          return list[1]

      def list_different_index():
          list = [ 1, __testSource(), "foo" ]
          return list[2]

      def list_unknown_index(index):
          list = [ 1, __testSource(), "foo" ]
          return list[index]

      def list_pattern_same_index():
          [_, match, _] = [ 1, __testSource(), "foo" ]
          return match

      def list_pattern_different_index():
          [_, _, no_match] = [ 1, __testSource(), "foo" ]
          return no_match

      def list_pattern_star_index():
          # False positive because we don't know size of RHS in general.
          [*match, _, _] = [ 1, __testSource(), "foo" ]
          return match
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.source_in_list";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.list_same_index";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.list_different_index";
        returns = [];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.list_unknown_index";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.list_pattern_same_index";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.list_pattern_different_index";
        returns = [];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.list_pattern_star_index";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ]


let test_tuple _ =
  assert_taint
    {|
      def source_in_tuple():
          return ( 1, __testSource(), "foo" )

      def tuple_same_index():
          tuple = ( 1, __testSource(), "foo" )
          return tuple[1]

      def tuple_different_index():
          tuple = ( 1, __testSource(), "foo" )
          return tuple[2]

      def tuple_unknown_index(index):
          tuple = ( 1, __testSource(), "foo" )
          return tuple[index]

      def tuple_pattern_same_index():
          (_, match, _) = ( 1, __testSource(), "foo" )
          return match

      def tuple_pattern_different_index():
          (_, _, no_match) = ( 1, __testSource(), "foo" )
          return no_match

      def tuple_pattern_star_index():
          # False positive because we don't know size of RHS in general.
          ( *match, _, _ ) = ( 1, __testSource(), "foo" )
          return match
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.source_in_tuple";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tuple_same_index";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tuple_different_index";
        returns = [];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tuple_unknown_index";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tuple_pattern_same_index";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tuple_pattern_different_index";
        returns = [];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tuple_pattern_star_index";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ]


let test_lambda _ =
  assert_taint
    {|
      def source_in_lambda():
          return lambda x : x + __testSource()
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.source_in_lambda";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ]


let test_set _ =
  assert_taint
    {|
      def source_in_set():
          return { 1, __testSource(), "foo" }

      def set_index():
          set = { 1, __testSource(), "foo" }
          return set[2]

      def set_unknown_index(index):
          set = { 1, __testSource(), "foo" }
          return set[index]
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.source_in_set";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.set_index";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.set_unknown_index";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ]


let test_starred _ =
  assert_taint
    {|
      def source_in_starred():
          list = [ 1, __testSource(), "foo" ]
          return __tito( *list )

      def source_in_starred_starred():
          dict = {
              "a": 1,
              "b": __testSource(),
              "c": "foo",
          }
          return __tito( **dict )
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.source_in_starred";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.source_in_starred_starred";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ]


let test_string _ =
  assert_taint
    {|
      def normal_string() -> str:
        return ""

      def untainted_format_string() -> str:
        return f"{1} {2}"

      def tainted_format_string() -> str:
        input = __testSource()
        return f"{input}"
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.normal_string";
        returns = [];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.untainted_format_string";
        returns = [];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.tainted_format_string";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ]


let test_ternary _ =
  assert_taint
    {|
      def source_in_then(cond):
          return __testSource() if cond else None

      def source_in_else(cond):
          return "foo" if cond else __testSource()

      def source_in_both(cond, request: django.http.Request):
        return __testSource() if cond else request.GET['field']

      def source_in_cond(cond):
          return "foo" if __testSource() else "bar"

    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.source_in_then";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.source_in_else";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.source_in_both";
        returns = [Sources.Test; Sources.UserControlled];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.source_in_cond";
        returns = [];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ]


let test_unary _ =
  assert_taint
    {|
      def source_in_unary():
          return not __testSource()
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.source_in_unary";
        returns = [Taint.Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ]


let test_yield _ =
  assert_taint
    {|
      def source_in_yield():
          yield __testSource()

      def source_in_yield_from():
          yield from __testSource()
    |}
    [
      {
        kind = `Function;
        define_name = "qualifier.source_in_yield";
        returns = [Taint.Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.source_in_yield_from";
        returns = [Taint.Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ]


let test_construction _ =
  assert_taint
    ~models:{|
      def qualifier.Data.__init__(self, capture: TaintInTaintOut[LocalReturn]): ...
    |}
    {|
      class Data:
        pass

      def test_capture():
        x = __testSource();
        d = Data(x, 5)
        return d

      def test_no_capture():
        x = __testSource();
        d = Data(5, x)
        return d
    |}
    [
      {
        kind = `Method;
        define_name = "qualifier.Data.__init__";
        returns = [];
        errors = [];
        sink_parameters = [];
        tito_parameters = ["capture"];
      };
      {
        kind = `Function;
        define_name = "qualifier.test_capture";
        returns = [Taint.Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
      {
        kind = `Function;
        define_name = "qualifier.test_no_capture";
        returns = [];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ]


let () =
  "taint">:::[
    "no_model">::test_no_model;
    "simple">::test_simple_source;
    "hardcoded">::test_hardcoded_source;
    "copy">::test_local_copy;
    "test_access_paths">::test_access_paths;
    "class_model">::test_class_model;
    "test_apply_method_model_at_call_site">::test_apply_method_model_at_call_site;
    "test_taint_in_taint_out_application">::test_taint_in_taint_out_application;
    "test_union">::test_taint_in_taint_out_application;
    "test_dictionary">::test_dictionary;
    "test_comprehensions">::test_comprehensions;
    "test_list">::test_list;
    "test_lambda">::test_lambda;
    "test_set">::test_set;
    "test_starred">::test_starred;
    "test_string">::test_string;
    "test_ternary">::test_ternary;
    "test_tuple">::test_tuple;
    "test_unary">::test_unary;
    "test_yield">::test_yield;
    "test_construction">::test_construction;
  ]
  |> TestHelper.run_with_taint_models
