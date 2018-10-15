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


let assert_taint ?(qualifier = Access.create "qualifier") ?models source expect =
  models
  >>| Test.trim_extra_indentation
  >>| (fun model_source -> Service.StaticAnalysis.add_models ~model_source)
  |> ignore;

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
    let forward, _errors = ForwardAnalysis.run ~environment ~define in
    let model = { Taint.Result.empty_model with forward } in
    Result.empty_model
    |> Result.with_model Taint.Result.kind model
    |> Fixpoint.add_predefined call_target;
  in
  let () = List.iter ~f:analyze_and_store_in_order defines in
  let check_expectation { define_name; returns } =
    let open Taint.Result in
    let expected_call_target = Callable.create_real (Access.create define_name) in
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
          define_name = "does_not_exist";
          returns = [];
        };
      ]
  in
  assert_raises
    (OUnitTest.OUnit_failure "no model for does_not_exist")
    assert_no_model


let test_simple_source _ =
  assert_taint
    {|
      def simple_source():
        return __testSource()
    |}
    [
      {
        define_name = "qualifier.simple_source";
        returns = [Sources.Test];
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
        define_name = "qualifier.simple_source";
        returns = [Sources.Test];
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
        define_name = "qualifier.get";
        returns = [Sources.UserControlled];
      };
      {
        define_name = "qualifier.post";
        returns = [Sources.UserControlled];
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
        define_name = "qualifier.get_field";
        returns = [Sources.UserControlled];
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
        define_name = "qualifier.get_field";
        returns = [Sources.UserControlled];
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
        define_name = "qualifier.copy_source";
        returns = [Sources.Test];
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
        define_name = "qualifier.Foo.bar";
        returns = [Sources.Test];
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
        define_name = "qualifier.taint_across_methods";
        returns = [Sources.Test];
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
        define_name = "qualifier.taint_across_methods";
        returns = [];
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
        define_name = "qualifier.taint_across_methods";
        returns = [Sources.Test];
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
        define_name = "qualifier.taint_across_methods";
        returns = [];
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
        define_name = "qualifier.taint_with_union_type";
        returns = [Sources.Test];
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
        define_name = "qualifier.taint_with_union_type";
        returns = [Sources.Test];
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
        define_name = "qualifier.taint_indirect_concatenated_call";
        returns = [Sources.Test];
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
        define_name = "qualifier.taint_indirect_concatenated_call";
        returns = [Sources.Test];
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
        define_name = "qualifier.simple_source";
        returns = [Sources.Test];
      };
    ];

  assert_taint
    {|
      def simple_source():
        return __testSource()

      def no_tito_taint():
        y = simple_source()
        x = __no_tito(y)
        return x
    |}
    [
      {
        define_name = "qualifier.no_tito_taint";
        returns = [];
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
        define_name = "qualifier.dictionary_source";
        returns = [Sources.Test];
      };
      {
        define_name = "qualifier.dictionary_same_index";
        returns = [Sources.Test];
      };
      {
        define_name = "qualifier.dictionary_different_index";
        returns = [];
      };
      {
        define_name = "qualifier.dictionary_unknown_read_index";
        returns = [Sources.Test];
      };
      {
        define_name = "qualifier.dictionary_unknown_write_index";
        returns = [Sources.Test];
      };
    ]


let test_comprehensions _ =
  assert_taint
    {|
      def source_in_iterator():
          return [ x for x in __testSource() ]

      def source_in_expression(data):
          return [ __testSource() for x in data ]
    |}
    [
      {
        define_name = "qualifier.source_in_iterator";
        returns = [Sources.Test];
      };
      {
        define_name = "qualifier.source_in_expression";
        returns = [Sources.Test];
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
    |}
    [
      {
        define_name = "qualifier.source_in_list";
        returns = [Sources.Test];
      };
      {
        define_name = "qualifier.list_same_index";
        returns = [Sources.Test];
      };
      {
        define_name = "qualifier.list_different_index";
        returns = [];
      };
      {
        define_name = "qualifier.list_unknown_index";
        returns = [Sources.Test];
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
        define_name = "qualifier.source_in_lambda";
        returns = [Sources.Test];
      };
    ]


let () =
  "taint">:::[
    "no_model">::test_no_model;
    "simple">::test_simple_source;
    "hardcoded">::test_hardcoded_source;
    "copy">::test_local_copy;
    "class_model">::test_class_model;
    "test_apply_method_model_at_call_site">::test_apply_method_model_at_call_site;
    "test_taint_in_taint_out_application">::test_taint_in_taint_out_application;
    "test_union">::test_taint_in_taint_out_application;
    "test_dictionary">::test_dictionary;
    "test_comprehensions">::test_comprehensions;
    "test_list">::test_list;
    "test_lambda">::test_lambda;
  ]
  |> Test.run_with_taint_models
