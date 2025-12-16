(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Taint
open TestHelper

let assert_class_models ~context ?user_models ~source ~expected () =
  let project =
    Test.ScratchPyrePysaProject.setup
      ~context
      ~requires_type_of_expressions:false
      ["test.py", source]
  in
  let pyre_api = Test.ScratchPyrePysaProject.read_only_api project in
  let user_models =
    match user_models with
    | Some user_models ->
        let { ModelParseResult.models; errors; _ } =
          ModelParser.parse
            ~pyre_api
            ~source:(Test.trim_extra_indentation user_models)
            ~taint_configuration:TaintConfiguration.Heap.default
            ~source_sink_filter:None
            ~definitions:None
            ~stubs:
              ([] |> Target.HashsetSharedMemory.from_heap |> Target.HashsetSharedMemory.read_only)
            ~python_version:(ModelParser.PythonVersion.create ())
            ()
        in
        assert_bool
          (Format.sprintf
             "The models shouldn't have any parsing errors:\n%s."
             (List.map errors ~f:ModelVerificationError.display |> String.concat ~sep:"\n"))
          (List.is_empty errors);
        models
        |> Registry.to_alist
        |> TaintFixpoint.SharedModels.of_alist_parallel ~scheduler:(Test.mock_scheduler ())
    | None -> TaintFixpoint.SharedModels.create ()
  in
  let actual_models =
    ClassModels.infer
      ~scheduler:(Test.mock_scheduler ())
      ~scheduler_policies:Configuration.SchedulerPolicies.empty
      ~pyre_api
      ~user_models:(TaintFixpoint.SharedModels.read_only user_models)
  in
  let get_model = Registry.get actual_models in
  let get_errors _ = [] in
  List.iter
    ~f:
      (check_expectation
         ~pyre_api
         ~taint_configuration:TaintConfiguration.Heap.default
         ~get_model
         ~get_errors)
    expected


let self_root =
  AccessPath.Root.PositionalParameter { position = 0; name = "self"; positional_only = false }


let test_dataclass context =
  assert_class_models
    ~context
    ~source:
      {|
      from dataclasses import dataclass

      @dataclass
      class Foo:
        x: int
        y: int
     |}
    ~expected:
      [
        outcome
          ~kind:`Method
          ~parameter_titos:
            [
              { name = "x"; titos = [Sinks.ParameterUpdate self_root] };
              { name = "y"; titos = [Sinks.ParameterUpdate self_root] };
            ]
          "test.Foo.__init__";
      ]
    ();
  assert_class_models
    ~context
    ~source:
      {|
      from dataclasses import dataclass

      @dataclass
      class Foo:
        x: int
        y: int
     |}
    ~user_models:{|
      test.Foo.x: TaintSink[Test] = ...
   |}
    ~expected:
      [
        outcome
          ~kind:`Method
          ~parameter_titos:
            [
              { name = "x"; titos = [Sinks.ParameterUpdate self_root] };
              { name = "y"; titos = [Sinks.ParameterUpdate self_root] };
            ]
          ~parameter_sinks:[{ name = "x"; sinks = [Sinks.NamedSink "Test"] }]
          "test.Foo.__init__";
      ]
    ();
  ()


let test_named_tuple context =
  assert_class_models
    ~context
    ~source:
      {|
      from typing import NamedTuple

      class Foo(NamedTuple):
        x: int
        y: int
     |}
    ~expected:[outcome ~kind:`Method ~parameter_titos:[] "test.Foo.__new__"]
    ();
  ()


let test_typed_dict context =
  assert_class_models
    ~context
    ~source:
      {|
      from typing import TypedDict

      class Foo(TypedDict):
        x: int
        y: int
     |}
    ~expected:
      [
        outcome
          ~kind:`Method
          ~parameter_titos:
            [
              { name = "x"; titos = [Sinks.ParameterUpdate self_root] };
              { name = "y"; titos = [Sinks.ParameterUpdate self_root] };
              { name = "__iterable"; titos = [Sinks.ParameterUpdate self_root] };
              { name = "**"; titos = [Sinks.ParameterUpdate self_root] };
            ]
          "test.Foo.__init__";
      ]
    ();
  assert_class_models
    ~context
    ~source:
      {|
      from typing import TypedDict

      class Foo(TypedDict):
        x: int
        y: int
     |}
    ~user_models:{|
      test.Foo.x: TaintSink[Test] = ...
   |}
    ~expected:
      [
        outcome
          ~kind:`Method
          ~parameter_titos:
            [
              { name = "x"; titos = [Sinks.ParameterUpdate self_root] };
              { name = "y"; titos = [Sinks.ParameterUpdate self_root] };
              { name = "__iterable"; titos = [Sinks.ParameterUpdate self_root] };
              { name = "**"; titos = [Sinks.ParameterUpdate self_root] };
            ]
          ~parameter_sinks:[{ name = "x"; sinks = [Sinks.NamedSink "Test"] }]
          "test.Foo.__init__";
      ]
    ();
  assert_class_models
    ~context
    ~source:
      {|
      from typing import TypedDict

      class Foo(TypedDict, total=False):
        x: int
        y: int
     |}
    ~expected:
      [
        outcome
          ~kind:`Method
          ~parameter_titos:
            [
              { name = "x"; titos = [Sinks.ParameterUpdate self_root] };
              { name = "y"; titos = [Sinks.ParameterUpdate self_root] };
              { name = "__iterable"; titos = [Sinks.ParameterUpdate self_root] };
              { name = "**"; titos = [Sinks.ParameterUpdate self_root] };
            ]
          "test.Foo.__init__";
      ]
    ();
  ()


let () =
  "class_models"
  >::: [
         "dataclass" >:: test_dataclass;
         "named_tuple" >:: test_named_tuple;
         "typed_dict" >:: test_typed_dict;
       ]
  |> Test.run
