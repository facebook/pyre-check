(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Core
open OUnit2
open Server
open Test
open Pyre
open CommandTest

let test_remove_dot _ =
  let assert_remove_dot ~position ~original ~expected =
    let actual = AutoComplete.remove_dot ~cursor_position:position original in
    assert_equal ~printer:ident expected actual
  in
  assert_remove_dot
    ~position:{ line = 3; column = 30 }
    ~original:{|
      a = 1 + 1
      function_call(variable1., variable2)
      b = 2 + 2
    |}
    ~expected:{|
      a = 1 + 1
      function_call(variable1, variable2)
      b = 2 + 2
    |};

  (* Edge case: DOT is the first character in a line *)
  assert_remove_dot
    ~position:{ line = 1; column = 1 }
    ~original:"., variable2)"
    ~expected:", variable2)";

  (* Edge case: DOT is the last character in a line *)
  assert_remove_dot
    ~position:{ line = 3; column = 16 }
    ~original:{|
      a = 1 + 1
      variable1.
      b = 2 + 2
    |}
    ~expected:{|
      a = 1 + 1
      variable1
      b = 2 + 2
    |}


let test_find_module_reference _ =
  let assert_resolved_module ~position ~source ~expected =
    let actual = AutoComplete.find_module_reference ~cursor_position:position source in
    match actual with
    | Some actual -> assert_equal ~printer:Reference.show expected actual
    | None -> failwith "Module reference not found."
  in
  assert_resolved_module
    ~position:{ line = 3; column = 32 }
    ~source:{|
      a = 1 + 1
      function_call(module_name.)
      b = 2 + 2
    |}
    ~expected:(Reference.create "module_name");
  assert_resolved_module
    ~position:{ line = 3; column = 18 }
    ~source:{|
      a = 1 + 1
      module_name.
      b = 2 + 2
    |}
    ~expected:(Reference.create "module_name")


let test_get_completion_items context =
  let open LanguageServer in
  let assert_completion_items ~cursor_position ~source ~expected =
    let handle = "test.py" in
    let { ScratchServer.configuration; state; _ } = ScratchServer.start ~context [handle, ""] in
    let path =
      let { Configuration.Analysis.local_root; _ } = configuration in
      Path.create_relative ~root:local_root ~relative:handle
    in
    let state = { state with open_documents = PyrePath.Map.singleton path source } in
    let actual = AutoComplete.get_completion_items ~state ~configuration ~path ~cursor_position in
    assert_equal ~printer:Types.CompletionItems.show expected actual
  in
  let create_completion_item
      ~cursor_position:{ Location.line; column }
      ~label
      ~kind
      ~detail
      ~new_text
    =
    let position = Types.Position.from_pyre_position ~line ~column in
    let range = { Types.Range.start = position; end_ = position } in
    { Types.CompletionItems.label; kind; detail; textEdit = { range; newText = new_text } }
  in
  (* Class attributes completion *)
  let source =
    {|
      class A:
        foo: bool
        def bar(self) -> int:
          return 3
      def main() -> None:
        a = A()
        a.
    |}
    |> trim_extra_indentation
  in
  let cursor_position = { Location.line = 8; column = 4 } in
  assert_completion_items
    ~cursor_position
    ~source
    ~expected:
      [ create_completion_item
          ~cursor_position
          ~label:"bar() -> int"
          ~kind:Types.CompletionItems.Kind.Function
          ~detail:"() -> int"
          ~new_text:"bar()";
        create_completion_item
          ~cursor_position
          ~label:"foo"
          ~kind:Types.CompletionItems.Kind.Variable
          ~detail:"bool"
          ~new_text:"foo" ];

  (* Module members completion *)
  let source =
    {|
      import abc
      def main() -> None:
        abc.
    |} |> trim_extra_indentation
  in
  let cursor_position = { Location.line = 4; column = 6 } in
  assert_completion_items
    ~cursor_position
    ~source
    ~expected:
      [ create_completion_item
          ~cursor_position
          ~label:"Type"
          ~kind:Types.CompletionItems.Kind.Variable
          ~detail:"_SpecialForm"
          ~new_text:"Type";
        create_completion_item
          ~cursor_position
          ~label:"TypeVar"
          ~kind:Types.CompletionItems.Kind.Variable
          ~detail:"object"
          ~new_text:"TypeVar";
        create_completion_item
          ~cursor_position
          ~label:"ABCMeta"
          ~kind:Types.CompletionItems.Kind.Variable
          ~detail:"Type[ABCMeta]"
          ~new_text:"ABCMeta";
        create_completion_item
          ~cursor_position
          ~label:"abstractmethod(callable: unknown) -> unknown"
          ~kind:Types.CompletionItems.Kind.Function
          ~detail:"(callable: unknown) -> unknown"
          ~new_text:"abstractmethod()";
        create_completion_item
          ~cursor_position
          ~label:"abstractproperty"
          ~kind:Types.CompletionItems.Kind.Variable
          ~detail:"Type[abstractproperty]"
          ~new_text:"abstractproperty";
        create_completion_item
          ~cursor_position
          ~label:"ABC"
          ~kind:Types.CompletionItems.Kind.Variable
          ~detail:"Type[ABC]"
          ~new_text:"ABC" ]


let test_untracked_path context =
  let tracked_handle = "test.pyi" in
  let assert_tracked ~relative expected =
    let source =
      {|
      class A:
        foo: bool
        def bar() -> int:
          return 3
      def main() -> None:
        a = A()
        a.
    |}
      |> trim_extra_indentation
    in
    let { ScratchServer.configuration; state; _ } =
      ScratchServer.start ~context [tracked_handle, ""]
    in
    let cursor_position = { Location.line = 8; column = 4 } in
    let path =
      let { Configuration.Analysis.local_root; _ } = configuration in
      Path.create_relative ~root:local_root ~relative
    in
    let state = { state with open_documents = PyrePath.Map.singleton path source } in
    let completion_items =
      AutoComplete.get_completion_items ~state ~configuration ~path ~cursor_position
    in
    let actual = not (List.is_empty completion_items) in
    assert_equal ~cmp:Bool.equal ~printer:Bool.to_string expected actual
  in
  assert_tracked ~relative:"test.pyi" true;
  assert_tracked ~relative:"test.py" false;
  assert_tracked ~relative:"not_tracked.py" false


let () =
  "autoComplete"
  >::: [ "remove_dot" >:: test_remove_dot;
         "find_module_reference" >:: test_find_module_reference;
         "get_completion_items" >:: test_get_completion_items;
         "untracked_path" >:: test_untracked_path ]
  |> Test.run
