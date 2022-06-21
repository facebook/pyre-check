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

let assert_taint ?models ?models_source ~context source expect =
  let handle = "qualifier.py" in
  let qualifier = Ast.Reference.create "qualifier" in
  let sources =
    match models_source with
    | Some models_source -> [handle, source; "models.py", models_source]
    | None -> [handle, source]
  in
  let project = Test.ScratchProject.setup ~context sources in
  let configuration = Test.ScratchProject.configuration_of project in
  let static_analysis_configuration = Configuration.StaticAnalysis.create configuration () in
  let { Test.ScratchProject.BuiltTypeEnvironment.type_environment = environment; _ } =
    Test.ScratchProject.build_type_environment project
  in
  let source =
    AstEnvironment.ReadOnly.get_processed_source
      (TypeEnvironment.ReadOnly.ast_environment environment)
      qualifier
    |> fun option -> Option.value_exn option
  in
  let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
  let models =
    models >>| Test.trim_extra_indentation |> Option.value ~default:TestHelper.initial_models_source
  in
  let initial_models =
    let { ModelParser.models; errors; _ } =
      ModelParser.parse
        ~resolution:(TypeCheck.resolution global_resolution (module TypeCheck.DummyContext))
        ~source:models
        ~configuration:TaintConfiguration.default
        ~callables:None
        ~stubs:(Target.HashSet.create ())
        ()
    in
    let () = assert_bool "Error while parsing models." (List.is_empty errors) in
    models
  in
  let defines = source |> Preprocessing.defines |> List.rev in
  let analyze_and_store_in_order models define =
    let call_target = Target.create define in
    let () = Log.log ~section:`Taint "Analyzing %a" Target.pp call_target in
    let call_graph_of_define =
      CallGraph.call_graph_of_define
        ~static_analysis_configuration
        ~environment
        ~override_graph:(OverrideGraph.SharedMemory.get_for_testing_only ())
        ~attribute_targets:(Registry.object_targets models)
        ~qualifier
        ~define:(Ast.Node.value define)
    in
    let forward, _errors, _ =
      ForwardAnalysis.run
        ?profiler:None
        ~environment
        ~class_interval_graph:(ClassIntervalSetGraph.SharedMemory.get_for_testing_only ())
        ~qualifier
        ~callable:call_target
        ~define
        ~call_graph_of_define
        ~get_callee_model:(Registry.get models)
        ~existing_model:Model.empty_model
    in
    let model = { Model.empty_model with forward } in
    Registry.set models ~target:call_target ~model
  in
  let models = List.fold ~f:analyze_and_store_in_order ~init:initial_models defines in
  let get_model = Registry.get models in
  let get_errors _ = [] in
  List.iter ~f:(check_expectation ~environment ~get_model ~get_errors) expect


let test_no_model context =
  let assert_no_model _ =
    assert_taint
      ~context
      {|
      def copy_source():
        pass
      |}
      [outcome ~kind:`Function "does_not_exist"]
  in
  assert_raises
    ("Model not found for (Function { name = \"does_not_exist\"; kind = Normal })"
    |> Base.Error.of_string
    |> Base.Error.to_exn)
    assert_no_model


let test_simple_source context =
  assert_taint
    ~context
    {|
      def simple_source():
        return _test_source()
    |}
    [outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.simple_source"];
  assert_taint
    ~context
    ~models:{|
      def models.custom_source() -> TaintSource[Test]: ...
    |}
    ~models_source:"def custom_source() -> int: ..."
    {|
      def simple_source():
        return models.custom_source()
    |}
    [outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.simple_source"];
  ()


let test_global_taint context =
  assert_taint
    ~context
    ~models:{|
       django.http.Request.GET: TaintSource[UserControlled] = ...
    |}
    {|
      sink = 0
      def inferred_source(request: django.http.Request):
        sink = request.GET
        return sink
    |}
    [
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "UserControlled"]
        "qualifier.inferred_source";
    ]


let test_hardcoded_source context =
  assert_taint
    ~context
    ~models:
      {|
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
      outcome ~kind:`Object ~returns:[Sources.NamedSource "UserControlled"] "django.http.Request.GET";
      outcome ~kind:`Function ~returns:[Sources.NamedSource "UserControlled"] "qualifier.get";
      outcome ~kind:`Function ~returns:[Sources.NamedSource "UserControlled"] "qualifier.post";
    ];
  assert_taint
    ~context
    ~models:
      {|
      django.http.Request.GET: TaintSource[UserControlled] = ...
      def dict.__getitem__(self: TaintInTaintOut, __k): ...
    |}
    {|
      def get_field(request: django.http.Request):
        return request.GET['field']
    |}
    [outcome ~kind:`Function ~returns:[Sources.NamedSource "UserControlled"] "qualifier.get_field"];
  assert_taint
    ~context
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
        ~returns:[Sources.NamedSource "UserControlled"]
        "qualifier.get_environment_variable";
    ];
  assert_taint
    ~context
    ~models:
      {|
      os.environ: TaintSource[UserControlled] = ...
      def dict.__getitem__(self: TaintInTaintOut, __k): ...
    |}
    {|
      def get_environment_variable_with_getitem():
        return os.environ['BAD']
    |}
    [
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "UserControlled"]
        "qualifier.get_environment_variable_with_getitem";
    ];
  assert_taint
    ~models:
      {|
      django.http.Request.GET: TaintSource[UserControlled] = ...
      def dict.__getitem__(self: TaintInTaintOut, __k): ...
    |}
    ~context
    {|
      class Request(django.http.Request): ...

      def get_field(request: Request):
        return request.GET['field']
    |}
    [outcome ~kind:`Function ~returns:[Sources.NamedSource "UserControlled"] "qualifier.get_field"]


let test_local_copy context =
  assert_taint
    ~context
    {|
      def copy_source():
        var = _test_source()
        return var
    |}
    [outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.copy_source"]


let test_access_paths context =
  assert_taint
    ~context
    {|
      def access_downward_closed():
        o = { 'a': _test_source() }
        x = o.a
        return x.g

      def access_non_taint():
        o = { 'a': _test_source() }
        x = o.b
        return x.g
    |}
    [
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "Test"]
        "qualifier.access_downward_closed";
      outcome ~kind:`Function ~returns:[] "qualifier.access_non_taint";
    ];
  assert_taint
    ~context
    {|
      def access_through_expression():
        return " ".join(_test_source())
    |}
    [
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "Test"]
        "qualifier.access_through_expression";
    ]


let test_class_model context =
  assert_taint
    ~context
    {|
      class Foo:
        def bar():
          return _test_source()
    |}
    [outcome ~kind:`Method ~returns:[Sources.NamedSource "Test"] "qualifier.Foo.bar"];
  assert_taint
    ~context
    ~models:{|
      qualifier.A.ATTRIBUTE: TaintSource[Test] = ...
    |}
    {|
      class A:
        ATTRIBUTE = 1
      def as_instance_attribute(a: A):
        return a.ATTRIBUTE
      def as_class_attribute():
        return A.ATTRIBUTE
    |}
    [
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.as_instance_attribute";
      outcome ~kind:`Function ~returns:[] "qualifier.as_class_attribute";
    ];
  assert_taint
    ~context
    ~models:{|
      qualifier.B.__class__.ATTRIBUTE: TaintSource[Test] = ...
    |}
    {|
      class B:
        ATTRIBUTE = 1
      def as_instance_attribute(b: B):
        return b.ATTRIBUTE
      def as_class_attribute():
        return B.ATTRIBUTE
    |}
    [
      outcome ~kind:`Function ~returns:[] "qualifier.as_instance_attribute";
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.as_class_attribute";
    ];

  (* Optionals. *)
  assert_taint
    ~context
    ~models:{|
      qualifier.Data.ATTRIBUTE: TaintSource[Test] = ...
    |}
    {|
      class Data:
        ATTRIBUTE = 1
      def optional(data: typing.Optional[Data]):
        return data.ATTRIBUTE
    |}
    [outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.optional"]


let test_apply_method_model_at_call_site context =
  assert_taint
    ~context
    {|
      class Foo:
        def qux():
          return _test_source()

      class Bar:
        def qux():
          return not_tainted()

      def taint_across_methods():
        f = Foo()
        return f.qux()
    |}
    [outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.taint_across_methods"];
  assert_taint
    ~context
    {|
      class Foo:
        def qux():
          return _test_source()

      class Bar:
        def qux():
          return not_tainted()

      def taint_across_methods():
        f = Bar()
        return f.qux()
    |}
    [outcome ~kind:`Function ~returns:[] "qualifier.taint_across_methods"];
  assert_taint
    ~context
    {|
      class Foo:
        def qux():
          return _test_source()

      class Bar:
        def qux():
          return not_tainted()

      def taint_across_methods(f: Foo):
        return f.qux()
    |}
    [outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.taint_across_methods"];
  assert_taint
    ~context
    {|
      class Foo:
        def qux():
          return _test_source()

      class Bar:
        def qux():
          return not_tainted()

      def taint_across_methods(f: Bar):
        return f.qux()
    |}
    [outcome ~kind:`Function ~returns:[] "qualifier.taint_across_methods"];
  assert_taint
    ~context
    {|
      class Foo:
        def qux():
          return _test_source()

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
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.taint_with_union_type";
    ];
  assert_taint
    ~context
    {|
      class Foo:
        def qux():
          return not_tainted()

      class Bar:
        def qux():
          return not_tainted()

      class Baz:
        def qux():
          return _test_source()

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
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.taint_with_union_type";
    ];
  assert_taint
    ~context
    {|
      class Indirect:
        def direct(self) -> Direct: ...

      class Direct:
        def source():
          return _test_source()

      def taint_indirect_concatenated_call(indirect: Indirect):
        direct = indirect.direct()
        return direct.source()
    |}
    [
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "Test"]
        "qualifier.taint_indirect_concatenated_call";
    ];
  assert_taint
    ~context
    {|
      class Indirect:
        def direct(self) -> Direct: ...

      class Direct:
        def source():
          return _test_source()

      def taint_indirect_concatenated_call(indirect: Indirect):
        return indirect.direct().source()
    |}
    [
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "Test"]
        "qualifier.taint_indirect_concatenated_call";
    ]


let test_taint_in_taint_out_application context =
  assert_taint
    ~context
    {|
      def simple_source():
        return _test_source()

      def taint_with_tito():
        y = simple_source()
        x = _tito(y)
        return x
    |}
    [outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.simple_source"];
  assert_taint
    ~context
    {|
      def simple_source():
        return _test_source()

      def __no_tito(y):
        pass

      def no_tito_taint():
        y = simple_source()
        x = __no_tito(y)
        return x
    |}
    [outcome ~kind:`Function ~returns:[] "qualifier.no_tito_taint"]


let test_dictionary context =
  assert_taint
    ~context
    {|
      def dictionary_source():
        return {
          "a": _test_source(),
        }

      def dictionary_same_index():
        dict = {
          "a": _test_source(),
        }
        return dict["a"]

      def dictionary_different_index():
        dict = {
          "a": _test_source(),
        }
        return dict["b"]

      def dictionary_unknown_read_index(index):
        dict = {
          "a": _test_source(),
        }
        return dict[index]

      def dictionary_unknown_write_index(index):
        dict = {
          index: _test_source(),
        }
        return dict["a"]
    |}
    [
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.dictionary_source";
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "Test"]
        "qualifier.dictionary_same_index";
      outcome ~kind:`Function ~returns:[] "qualifier.dictionary_different_index";
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "Test"]
        "qualifier.dictionary_unknown_read_index";
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "Test"]
        "qualifier.dictionary_unknown_write_index";
    ];
  assert_taint
    ~context
    {|
      def dictionary_source():
        first = {
          "a": _test_source(),
        }
        second = { **first }
        return second
    |}
    [outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.dictionary_source"];
  assert_taint
    ~context
    {|
      def dictionary_source():
        first = {
          "a": _test_source(),
        }
        second = { **first }
        return second["a"]
    |}
    [outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.dictionary_source"];

  (* We don't collapse the taint for keywords. *)
  assert_taint
    ~context
    {|
      def dictionary_source():
        first = {
          "a": _test_source(),
        }
        second = { **first }
        return second["b"]
    |}
    [outcome ~kind:`Function ~returns:[] "qualifier.dictionary_source"];

  (* Keys. *)
  assert_taint
    ~context
    {|
      def dictionary_source():
        d = { _test_source(): "a" }
        return d
    |}
    [outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.dictionary_source"];

  (* We're imprecise, and don't filter key taint when accessing a specific field at the moment. *)
  assert_taint
    ~context
    {|
      def dictionary_source_keys_two():
        d = { _test_source(): "a" }
        return d[0]
    |}
    [outcome ~kind:`Function ~returns:[] "qualifier.dictionary_source_keys_two"];

  (* Comprehensions. *)
  assert_taint
    ~context
    {|
      def dictionary_source():
        d = { 1: x for x in [_test_source()] }
        return d
    |}
    [outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.dictionary_source"];
  assert_taint
    ~context
    {|
      def dictionary_source():
        d = { x: 1 for x in [_test_source()] }
        return d
    |}
    [outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.dictionary_source"];
  assert_taint
    ~context
    {|
      def dictionary_source():
        d = { x: 1 for x in [_test_source()] }
        return d[0]
    |}
    [outcome ~kind:`Function ~returns:[] "qualifier.dictionary_source"]


let test_comprehensions context =
  assert_taint
    ~context
    {|
      def source_in_iterator():
          return [ x for x in _test_source() ]

      def source_in_expression(data):
          return [ _test_source() for x in data ]

      def source_in_set_iterator():
          return { x for x in _test_source() }

      def source_in_set_expression(data):
          return { _test_source() for x in data }

      def source_in_generator_iterator():
          return (x for x in _test_source())

      def source_in_generator_expression(data):
          return ( _test_source() for x in data )
    |}
    [
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.source_in_iterator";
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.source_in_expression";
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "Test"]
        "qualifier.source_in_set_iterator";
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "Test"]
        "qualifier.source_in_set_expression";
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "Test"]
        "qualifier.source_in_generator_iterator";
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "Test"]
        "qualifier.source_in_generator_expression";
    ]


let test_list context =
  assert_taint
    ~context
    {|
      def source_in_list():
          return [ 1, _test_source(), "foo" ]

      def list_same_index():
          list = [ 1, _test_source(), "foo" ]
          return list[1]

      def list_different_index():
          list = [ 1, _test_source(), "foo" ]
          return list[2]

      def list_unknown_index(index):
          list = [ 1, _test_source(), "foo" ]
          return list[index]

      def list_pattern_same_index():
          [_, match, _] = [ 1, _test_source(), "foo" ]
          return match

      def list_pattern_different_index():
          [_, _, no_match] = [ 1, _test_source(), "foo" ]
          return no_match

      def list_pattern_star_index():
          # False positive because we don't know size of RHS in general.
          [*match, _, _] = [ 1, _test_source(), "foo" ]
          return match
    |}
    [
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.source_in_list";
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.list_same_index";
      outcome ~kind:`Function ~returns:[] "qualifier.list_different_index";
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.list_unknown_index";
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "Test"]
        "qualifier.list_pattern_same_index";
      outcome ~kind:`Function ~returns:[] "qualifier.list_pattern_different_index";
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "Test"]
        "qualifier.list_pattern_star_index";
    ]


let test_tuple context =
  assert_taint
    ~context
    {|
      def source_in_tuple():
          return ( 1, _test_source(), "foo" )

      def tuple_same_index():
          tuple = ( 1, _test_source(), "foo" )
          return tuple[1]

      def tuple_different_index():
          tuple = ( 1, _test_source(), "foo" )
          return tuple[2]

      def tuple_unknown_index(index):
          tuple = ( 1, _test_source(), "foo" )
          return tuple[index]

      def tuple_pattern_same_index():
          (_, match, _) = ( 1, _test_source(), "foo" )
          return match

      def tuple_pattern_different_index():
          (_, _, no_match) = ( 1, _test_source(), "foo" )
          return no_match

      def tuple_pattern_star_index():
          # False positive because we don't know size of RHS in general.
          ( *match, _, _ ) = ( 1, _test_source(), "foo" )
          return match
    |}
    [
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.source_in_tuple";
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.tuple_same_index";
      outcome ~kind:`Function ~returns:[] "qualifier.tuple_different_index";
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.tuple_unknown_index";
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "Test"]
        "qualifier.tuple_pattern_same_index";
      outcome ~kind:`Function ~returns:[] "qualifier.tuple_pattern_different_index";
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "Test"]
        "qualifier.tuple_pattern_star_index";
    ]


let test_asyncio_gather context =
  assert_taint
    ~context
    {|
      import asyncio
      def benign_through_asyncio():
        a, b = asyncio.gather(0, _test_source())
        return a

      def source_through_asyncio():
        a, b = asyncio.gather(0, _test_source())
        return b
    |}
    [
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "Test"]
        "qualifier.source_through_asyncio";
      outcome ~kind:`Function ~returns:[] "qualifier.benign_through_asyncio";
    ];
  (* We also support asyncio.gather imported from other modules. *)
  assert_taint
    ~context
    {|
      import foo
      def benign_through_asyncio():
        a, b = foo.asyncio.gather(0, _test_source())
        return a

      def source_through_asyncio():
        a, b = foo.asyncio.gather(0, _test_source())
        return b
    |}
    [
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "Test"]
        "qualifier.source_through_asyncio";
      outcome ~kind:`Function ~returns:[] "qualifier.benign_through_asyncio";
    ]


let test_lambda context =
  assert_taint
    ~context
    {|
      def source_in_lambda():
          return lambda x : x + _test_source()
    |}
    [outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.source_in_lambda"];
  assert_taint
    ~context
    {|
      def optional_lambda():
        if 1 > 2:
          f = None
        else:
          f = lambda x: x + _test_source()
        return f
    |}
    [outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.optional_lambda"]


let test_set context =
  assert_taint
    ~context
    {|
      def source_in_set():
          return { 1, _test_source(), "foo" }

      def set_index():
          set = { 1, _test_source(), "foo" }
          return set[2]

      def set_unknown_index(index):
          set = { 1, _test_source(), "foo" }
          return set[index]
    |}
    [
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.source_in_set";
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.set_index";
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.set_unknown_index";
    ]


let test_starred context =
  assert_taint
    ~context
    {|
      def source_in_starred():
          list = [ 1, _test_source(), "foo" ]
          return _tito( *list )

      def source_in_starred_starred():
          dict = {
              "a": 1,
              "b": _test_source(),
              "c": "foo",
          }
          return _tito( **dict )
    |}
    [
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.source_in_starred";
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "Test"]
        "qualifier.source_in_starred_starred";
    ]


let test_string context =
  assert_taint
    ~context
    {|
      def normal_string() -> str:
        return ""

      def untainted_format_string() -> str:
        return f"{1} {2}"

      def tainted_format_string() -> str:
        input = _test_source()
        return f"{input}"
    |}
    [
      outcome ~kind:`Function ~returns:[] "qualifier.normal_string";
      outcome ~kind:`Function ~returns:[] "qualifier.untainted_format_string";
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "Test"]
        "qualifier.tainted_format_string";
    ]


let test_ternary context =
  assert_taint
    ~context
    ~models:
      {|
       def _test_source() -> TaintSource[Test]: ...
       django.http.Request.GET: TaintSource[UserControlled] = ...
    |}
    {|
      def source_in_then(cond):
          return _test_source() if cond else None

      def source_in_else(cond):
          return "foo" if cond else _test_source()

      def source_in_both(cond, request: django.http.Request):
        return _test_source() if cond else request.GET['field']

      def source_in_cond(cond):
          return "foo" if _test_source() else "bar"

    |}
    [
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.source_in_then";
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.source_in_else";
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "Test"; Sources.NamedSource "UserControlled"]
        "qualifier.source_in_both";
      outcome ~kind:`Function ~returns:[] "qualifier.source_in_cond";
    ]


let test_unary context =
  assert_taint
    ~context
    {|
      def source_in_unary():
          return not _test_source()
    |}
    [outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.source_in_unary"]


let test_parameter_default_values context =
  assert_taint
    ~context
    {|
      def source_in_default(totally_innocent=_test_source()):
        return totally_innocent
    |}
    [outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.source_in_default"];
  assert_taint
    ~context
    {|
      def source_in_default(benign, tainted=_test_source()):
        return benign
    |}
    [outcome ~kind:`Function ~returns:[] "qualifier.source_in_default"]


let test_walrus context =
  assert_taint
    ~context
    {|
      def source_in_walrus():
          return (x := _test_source())
    |}
    [outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.source_in_walrus"]


let test_yield context =
  assert_taint
    ~context
    {|
      def source_in_yield():
          yield _test_source()

      def source_in_yield_from():
          yield from _test_source()
    |}
    [
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.source_in_yield";
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.source_in_yield_from";
    ]


let test_construction context =
  assert_taint
    ~context
    ~models:
      {|
      def _test_source() -> TaintSource[Test]: ...
      def qualifier.Data.__init__(self, capture: TaintInTaintOut): ...
    |}
    {|
      class Data:
        def __init__(self, capture) -> None: ...

      def test_capture():
        x = _test_source();
        d = Data(x, 5)
        return d

      def test_no_capture():
        x = _test_source();
        d = Data(5, x)
        return d
    |}
    [
      outcome
        ~kind:`Method
        ~returns:[]
        ~tito_parameters:[{ name = "capture"; sinks = [Sinks.LocalReturn] }]
        "qualifier.Data.__init__";
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.test_capture";
      outcome ~kind:`Function ~returns:[] "qualifier.test_no_capture";
    ]


let test_composed_models context =
  assert_taint
    ~context
    ~models:
      {|
      def models.composed_model(x: TaintSink[Test], y, z) -> TaintSource[UserControlled]: ...
      def models.composed_model(x, y: TaintSink[Demo], z: TaintInTaintOut): ...
    |}
    ~models_source:"def composed_model(x, y, z): ..."
    {|
    |}
    [
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "UserControlled"]
        ~sink_parameters:
          [
            { name = "x"; sinks = [Taint.Sinks.NamedSink "Test"] };
            { name = "y"; sinks = [Taint.Sinks.NamedSink "Demo"] };
          ]
        ~tito_parameters:[{ name = "z"; sinks = [Sinks.LocalReturn] }]
        "models.composed_model";
    ]


let test_tito_side_effects context =
  assert_taint
    ~context
    ~models:
      {|
      def _test_source() -> TaintSource[Test]: ...
      def models.change_arg0(arg0, arg1: TaintInTaintOut[Updates[arg0]]): ...
      def models.change_arg1(arg0: TaintInTaintOut[Updates[arg1]], arg1): ...
      def qualifier.MyList.append(self, arg: TaintInTaintOut[Updates[self]]): ...
    |}
    ~models_source:
      {|
      def change_arg0(arg0, arg1): ...
      def change_arg1(arg0, arg1): ...
      |}
    {|
      def test_from_1_to_0():
        x = 0
        models.change_arg0(x, _test_source())
        return x

      def test_from_0_to_1():
        y = 0
        models.change_arg1(_test_source(), y)
        return y

      def test_from_1_to_0_nested():
        x = {}
        models.change_arg0(x.foo, _test_source())
        return x.foo

      def test_from_1_to_0_nested_distinct():
        x = {}
        models.change_arg0(x.foo, _test_source())
        return x.bar

      def test_weak_assign():
        x = _test_source()
        models.change_arg0(x, 'no taint')
        return x

      class MyList:
        def append(self, arg): ...

      def test_list_append():
        l = MyList()
        l.append(_test_source())
        return l
    |}
    [
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.test_from_1_to_0";
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.test_from_0_to_1";
      outcome
        ~kind:`Function
        ~returns:[Sources.NamedSource "Test"]
        "qualifier.test_from_1_to_0_nested";
      outcome ~kind:`Function ~returns:[] "qualifier.test_from_1_to_0_nested_distinct";
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.test_weak_assign";
      outcome
        ~kind:`Method
        ~tito_parameters:[{ name = "arg"; sinks = [Sinks.ParameterUpdate 0] }]
        "qualifier.MyList.append";
      outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.test_list_append";
    ]


let test_taint_in_taint_out_transform context =
  assert_taint
    ~context
    ~models:
      {|
      def _test_source() -> TaintSource[Test]: ...
      def models.test_transform(arg: TaintInTaintOut[Transform[TestTransform]]): ...
    |}
    ~models_source:{|
      def test_transform(arg): ...
    |}
    {|
      def simple_source():
        return _test_source()

      def taint_with_tito_transform():
        x = simple_source()
        y = models.test_transform(x)
        return y
    |}
    [
      outcome
        ~kind:`Function
        ~returns:
          [
            Sources.Transform
              {
                local = TaintTransforms.of_named_transforms [TaintTransform.Named "TestTransform"];
                global = TaintTransforms.empty;
                base = Sources.NamedSource "Test";
              };
          ]
        "qualifier.taint_with_tito_transform";
    ];
  assert_taint
    ~context
    ~models:
      {|
      def _test_source() -> TaintSource[Test]: ...
      def models.test_transform(arg: TaintInTaintOut[Transform[TestTransform]]): ...
      def models.demo_transform(arg: TaintInTaintOut[Transform[DemoTransform]]): ...
    |}
    ~models_source:{|
      def test_transform(arg): ...
      def demo_transform(arg): ...
    |}
    {|
      def simple_source():
        return _test_source()

      def taint_with_tito_transform():
        x = simple_source()
        y = models.test_transform(x)
        return y

      def taint_transforming_transform():
        x = taint_with_tito_transform()
        y = models.demo_transform(x)
        return y
    |}
    [
      outcome
        ~kind:`Function
        ~returns:
          [
            Sources.Transform
              {
                local = TaintTransforms.of_named_transforms [TaintTransform.Named "DemoTransform"];
                global = TaintTransforms.of_named_transforms [TaintTransform.Named "TestTransform"];
                base = Sources.NamedSource "Test";
              };
          ]
        "qualifier.taint_transforming_transform";
    ];
  assert_taint
    ~context
    ~models:
      {|
      def _test_source() -> TaintSource[Test]: ...
      def models.test_transform(arg: TaintInTaintOut[Transform[TestTransform]]): ...
      def models.demo_transform(arg: TaintInTaintOut[Transform[DemoTransform]]): ...
    |}
    ~models_source:{|
      def test_transform(arg): ...
      def demo_transform(arg): ...
    |}
    {|
      def simple_source():
        return _test_source()

      def taint_with_two_tito_transform():
        x = simple_source()
        y = models.test_transform(x)
        z = models.demo_transform(y)
        return z
    |}
    [
      outcome
        ~kind:`Function
        ~returns:
          [
            Sources.Transform
              {
                local =
                  TaintTransforms.of_named_transforms
                    [TaintTransform.Named "DemoTransform"; TaintTransform.Named "TestTransform"];
                global = TaintTransforms.empty;
                base = Sources.NamedSource "Test";
              };
          ]
        "qualifier.taint_with_two_tito_transform";
    ]


let () =
  "forwardAnalysis"
  >::: [
         "access_paths" >:: test_access_paths;
         "apply_method_model_at_call_site" >:: test_apply_method_model_at_call_site;
         "asyncio_gather" >:: test_asyncio_gather;
         "class_model" >:: test_class_model;
         "composed_models" >:: test_composed_models;
         "comprehensions" >:: test_comprehensions;
         "construction" >:: test_construction;
         "copy" >:: test_local_copy;
         "dictionary" >:: test_dictionary;
         "global_taint" >:: test_global_taint;
         "hardcoded" >:: test_hardcoded_source;
         "lambda" >:: test_lambda;
         "list" >:: test_list;
         "no_model" >:: test_no_model;
         "parameter_default_values" >:: test_parameter_default_values;
         "set" >:: test_set;
         "simple" >:: test_simple_source;
         "starred" >:: test_starred;
         "string" >:: test_string;
         "taint_in_taint_out_application" >:: test_taint_in_taint_out_application;
         "taint_in_taint_out_transform" >:: test_taint_in_taint_out_transform;
         "ternary" >:: test_ternary;
         "tito_side_effects" >:: test_tito_side_effects;
         "tuple" >:: test_tuple;
         "unary" >:: test_unary;
         "walrus" >:: test_walrus;
         "yield" >:: test_yield;
       ]
  |> Test.run
