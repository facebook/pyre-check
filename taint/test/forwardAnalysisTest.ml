(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Pyre
open Taint

open Interprocedural
open TestHelper


let assert_taint ?(qualifier = "qualifier") ?models source expect =
  let configuration =
    Configuration.Analysis.create
      ~project_root:(Path.current_working_directory ())
      ()
  in

  let source =
    let path = Test.mock_path (qualifier ^ ".py") in
    let file = File.create ~content:(Test.trim_extra_indentation source) path in
    let handle = File.handle file ~configuration in
    Ast.SharedMemory.Sources.remove ~handles:[handle];
    Service.Parser.parse_sources
      ~configuration
      ~scheduler:(Scheduler.mock ())
      ~preprocessing_state:None
      ~files:[file]
    |> ignore;
    match Ast.SharedMemory.Sources.get handle with
    | Some source -> source
    | None -> failwith "Unable to parse source."
  in

  let environment =
    let models =
      models
      >>| (fun model -> [Test.parse model])
      |> Option.value ~default:[]
    in
    let environment =
      Test.environment ~sources:(Test.typeshed_stubs () @ models) ~configuration ()
    in
    Service.Environment.populate ~configuration environment [source];
    environment
  in

  models
  >>| Test.trim_extra_indentation
  >>| (fun model_source ->
      Model.parse
        ~resolution:(TypeCheck.resolution environment ())
        ~source:model_source
        ~configuration:TaintConfiguration.default
        Callable.Map.empty
      |> Callable.Map.map ~f:(Interprocedural.Result.make_model Taint.Result.kind)
      |> Interprocedural.Analysis.record_initial_models ~functions:[] ~stubs:[])
  |> ignore;

  TypeCheck.run ~configuration ~environment ~source |> ignore;
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
    let forward, _errors =
      ForwardAnalysis.run
        ~environment
        ~define
        ~existing_model:Taint.Result.empty_model
    in
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
        outcome
          ~kind:`Function
          "does_not_exist";
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
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.simple_source";
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
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.simple_source";
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
      outcome
        ~kind:`Object
        ~returns:[Sources.UserControlled]
        "django.http.Request.GET";
      outcome
        ~kind:`Function
        ~returns:[Sources.UserControlled]
        "qualifier.get";
      outcome
        ~kind:`Function
        ~returns:[Sources.UserControlled]
        "qualifier.post";
    ];
  assert_taint
    ~models:{|
      django.http.Request.GET: TaintSource[UserControlled] = ...
      def dict.__getitem__(self: TaintInTaintOut, key): ...
    |}
    {|
      def get_field(request: django.http.Request):
        return request.GET['field']
    |}
    [
      outcome
        ~kind:`Function
        ~returns:[Sources.UserControlled]
        "qualifier.get_field";
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
      outcome
        ~kind:`Function
        ~returns:[Sources.UserControlled]
        "qualifier.get_environment_variable";
    ];
  assert_taint
    ~models:{|
      os.environ: TaintSource[UserControlled] = ...
      def dict.__getitem__(self: TaintInTaintOut, key): ...
    |}
    {|
      def get_environment_variable_with_getitem():
        return os.environ['BAD']
    |}
    [
      outcome
        ~kind:`Function
        ~returns:[Sources.UserControlled]
        "qualifier.get_environment_variable_with_getitem";
    ];
  assert_taint
    {|
      class Request(django.http.Request): ...

      def get_field(request: Request):
        return request.GET['field']
    |}
    [
      outcome
        ~kind:`Function
        ~returns:[Sources.UserControlled]
        "qualifier.get_field";
    ]


let test_local_copy _ =
  assert_taint
    {|
      def copy_source():
        var = __testSource()
        return var
    |}
    [
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.copy_source";
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
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.access_downward_closed";
      outcome
        ~kind:`Function
        ~returns:[]
        "qualifier.access_non_taint";
    ]


let test_class_model _ =
  assert_taint
    {|
      class Foo:
        def bar():
          return __testSource()
    |}
    [
      outcome
        ~kind:`Method
        ~returns:[Sources.Test]
        "qualifier.Foo.bar";
    ];
  assert_taint
    {|
      class Class:
        self.tainted = ...
        @property
        def property(self):
          return self.tainted

      def uses_property(c: Class):
        c.tainted = __testSource()
        return c.property
    |}
    [
      outcome
        ~kind:`Method
        ~returns:[]
        "qualifier.Class.property";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.uses_property";
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
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.taint_across_methods";
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
      outcome
        ~kind:`Function
        ~returns:[]
        "qualifier.taint_across_methods";
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
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.taint_across_methods";
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
      outcome
        ~kind:`Function
        ~returns:[]
        "qualifier.taint_across_methods";
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
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.taint_with_union_type";
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
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.taint_with_union_type";
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
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.taint_indirect_concatenated_call";
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
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.taint_indirect_concatenated_call";
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
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.simple_source";
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
      outcome
        ~kind:`Function
        ~returns:[]
        "qualifier.no_tito_taint";
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
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.dictionary_source";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.dictionary_same_index";
      outcome
        ~kind:`Function
        ~returns:[]
        "qualifier.dictionary_different_index";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.dictionary_unknown_read_index";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.dictionary_unknown_write_index";
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
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.source_in_iterator";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.source_in_expression";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.source_in_set_iterator";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.source_in_set_expression";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.source_in_generator_iterator";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.source_in_generator_expression";
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
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.source_in_list";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.list_same_index";
      outcome
        ~kind:`Function
        ~returns:[]
        "qualifier.list_different_index";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.list_unknown_index";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.list_pattern_same_index";
      outcome
        ~kind:`Function
        ~returns:[]
        "qualifier.list_pattern_different_index";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.list_pattern_star_index";
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
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.source_in_tuple";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.tuple_same_index";
      outcome
        ~kind:`Function
        ~returns:[]
        "qualifier.tuple_different_index";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.tuple_unknown_index";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.tuple_pattern_same_index";
      outcome
        ~kind:`Function
        ~returns:[]
        "qualifier.tuple_pattern_different_index";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.tuple_pattern_star_index";
    ]


let test_lambda _ =
  assert_taint
    {|
      def source_in_lambda():
          return lambda x : x + __testSource()
    |}
    [
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.source_in_lambda";
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
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.source_in_set";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.set_index";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.set_unknown_index";
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
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.source_in_starred";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.source_in_starred_starred";
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
      outcome
        ~kind:`Function
        ~returns:[]
        "qualifier.normal_string";
      outcome
        ~kind:`Function
        ~returns:[]
        "qualifier.untainted_format_string";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.tainted_format_string";
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
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.source_in_then";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.source_in_else";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test; Sources.UserControlled]
        "qualifier.source_in_both";
      outcome
        ~kind:`Function
        ~returns:[]
        "qualifier.source_in_cond";
    ]


let test_unary _ =
  assert_taint
    {|
      def source_in_unary():
          return not __testSource()
    |}
    [
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.source_in_unary";
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
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.source_in_yield";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.source_in_yield_from";
    ]


let test_construction _ =
  assert_taint
    ~models:{|
      def qualifier.Data.__init__(self, capture: TaintInTaintOut): ...
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
      outcome
        ~kind:`Method
        ~returns:[]
        ~tito_parameters:["capture"]
        "qualifier.Data.__init__";
      outcome
        ~kind:`Function
        ~returns:[Sources.Test]
        "qualifier.test_capture";
      outcome
        ~kind:`Function
        ~returns:[]
        "qualifier.test_no_capture";
    ]


let test_composed_models _ =
  assert_taint
    ~models:{|
      def composed_model(x: TaintSink[Test], y, z) -> TaintSource[UserControlled]: ...
      def composed_model(x, y: TaintSink[Demo], z: TaintInTaintOut): ...
    |}
    {|
    |}
    [
      outcome
        ~kind:`Function
        ~returns:[Sources.UserControlled]
        ~sink_parameters:[
          { name = "x"; sinks = [Taint.Sinks.Test] };
          { name = "y"; sinks = [Taint.Sinks.Demo] };
        ]
        ~tito_parameters:["z"]
        "composed_model";
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
    "test_composed_models">::test_composed_models;
  ]
  |> TestHelper.run_with_taint_models
