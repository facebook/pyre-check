(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open Test

let run_check_module ~environment source =
  let type_environment = TypeEnvironment.read_only environment in
  source
  |> Preprocessing.defines ~include_toplevels:false ~include_nested:true
  |> List.map
       ~f:(fun
            {
              Ast.Node.value =
                { Ast.Statement.Define.signature = { Ast.Statement.Define.Signature.name; _ }; _ };
              _;
            }
          -> name)
  |> List.map ~f:(GlobalLeakCheck.check_qualifier ~type_environment)
  |> List.map ~f:Caml.Option.get
  |> List.concat


let instantiate_and_stringify ~lookup errors =
  let to_string error =
    let description = AnalysisError.Instantiated.concise_description error in
    let path = AnalysisError.Instantiated.path error in
    let line = AnalysisError.Instantiated.location error |> Ast.Location.WithPath.line in
    Format.sprintf "%s %d: %s" path line description
  in
  List.map errors ~f:(AnalysisError.instantiate ~show_error_traces:false ~lookup)
  |> List.map ~f:to_string


let assert_global_leak_errors
    ?(skip_type_check = false)
    ?(other_sources = [])
    ~context
    source
    expected
  =
  let source_with_imports = "      from typing import *" ^ source in
  let preliminary_type_check_errors =
    let environment = List.map ~f:(fun { handle; source } -> handle, source) other_sources in
    let project =
      ScratchProject.setup ~context ~strict:true (("test.py", source_with_imports) :: environment)
    in
    project
    |> ScratchProject.errors_environment
    |> ErrorsEnvironment.ReadOnly.get_all_errors
    |> instantiate_and_stringify
         ~lookup:
           (ScratchProject.module_tracker project |> ModuleTracker.ReadOnly.lookup_relative_path)
  in
  if skip_type_check || List.is_empty preliminary_type_check_errors then
    let check ~environment ~source = run_check_module ~environment source in
    assert_errors
      ~context
      ~check
      ~strict:true
      ~include_suppressed_errors:true
      ~debug:true
      ~other_sources
      source_with_imports
      expected
  else
    Format.sprintf "Type check errors: %s" ([%show: string list] preliminary_type_check_errors)
    |> failwith


let test_global_assignment context =
  let assert_global_leak_errors = assert_global_leak_errors ~context in
  assert_global_leak_errors
    (* Assignment to a non-global doesn't error. *)
    {|
      def foo() -> None:
         x = 1
    |}
    [];
  assert_global_leak_errors
    (* Assignment to a non-global doesn't error. *)
    {|
      my_global: int = 1
      def foo() -> None:
        x = 1
    |}
    [];
  assert_global_leak_errors
    (* Assignment to a non-global doesn't error. *)
    (* Note: my_global here is not actually a global, this is a valid assignment. *)
    {|
      my_global: int = 1
      def foo() -> None:
        my_global = 2
    |}
    [];
  assert_global_leak_errors
    (* Assignment to a non-global doesn't error. *)
    {|
      my_global: int = 1
      def foo() -> None:
        non_local = 2
        def inner() -> None:
          non_local = 3
        inner()
    |}
    [];
  assert_global_leak_errors
    (* Assignment to a global errors. *)
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        my_global = 2
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    (* Assignment to a global errors. *)
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        my_global = a = 2
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    (* Assignment to a global errors. *)
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        a = my_global = 2
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    (* Assignment to a non-global doesn't error. *)
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        x, y = 2, 3
    |}
    [];
  assert_global_leak_errors
    (* Assignment to a global errors. *)
    {|
      my_global: int = 1
      def foo() -> None:
        def inner() -> None:
          global my_global
          my_global = 2
        inner()
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    (* Assignment to a non-global doesn't error. *)
    {|
      my_global: int = 1
      def foo() -> None:
         if my_local := 0:
           pass
    |}
    [];
  assert_global_leak_errors
    (* Assignment to a global errors. *)
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        if my_global := (a := 3):
          print("hi")
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    (* Assignment to a global errors. *)
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        if a := (my_global := 3):
          print("hi")
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    (* Assignment to a global using the walrus operator errors. *)
    {|
      my_global: List[int] = [1, 2, 3]
      def foo() -> None:
        (a := my_global).append(4)
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.List[int]`."];
  ()


let test_global_exceptions context =
  let assert_global_leak_errors = assert_global_leak_errors ~context in
  assert_global_leak_errors
    (* Raising an exception doesn't result in an error. *)
    {|
      def foo() -> None:
        raise Exception()
    |}
    [];
  assert_global_leak_errors
    (* Raising an exception doesn't result in an error. *)
    {|
      def foo() -> None:
        raise Exception() from Exception()
    |}
    [];
  assert_global_leak_errors
    (* Raising an exception results in an error if it mutates a global. *)
    {|
      my_global: Dict[str, int] = {}
      def setdefault_global_dict() -> None:
        raise TypeError(str(my_global.setdefault("a", 1)))
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    (* Raising an exception results in an error if it mutates a global. *)
    {|
      my_global: Dict[str, int] = {}
      def setdefault_global_dict() -> None:
        raise Exception() from Exception(my_global.setdefault("a", 1))
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    (* Handling exceptions doesn't result in an error. *)
    {|
      def foo() -> None:
        try:
          x = 5
        except:
          print("An exception occurred")
    |}
    [];
  assert_global_leak_errors
    (* Handling exceptions doesn't result in an error. *)
    {|
      def foo() -> None:
        try:
          x = 5
        except:
          print("An exception occurred")
        else:
          print("No exception")
    |}
    [];
  assert_global_leak_errors
    (* Handling exceptions doesn't result in an error. *)
    {|
      def foo() -> None:
        try:
          x = 5
        except:
          print("An exception occurred")
        finally:
          print("try except is complete")
    |}
    [];
  assert_global_leak_errors
    (* Handling exceptions doesn't result in an error. *)
    {|
      def foo() -> None:
        try:
          my_local = 1
        except Exception as e:
          my_local = 2
    |}
    [];
  assert_global_leak_errors
    (* Handling an exception results in an error if it mutates a global. *)
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        try:
          my_local = 1
        except Exception as my_global:
          my_local = 2
    |}
    ["Global leak [3100]: Data is leaked to global `my_global` of type `unknown`."];
  assert_global_leak_errors
    (* Handling an exception results in an error if it mutates a global. *)
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        try:
          my_global = 1
        except:
          print("An exception occurred")
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    (* Handling an exception results in an error if it mutates a global. *)
    {|
      my_global: int = 1
      def foo() -> None:
        try:
          global my_global
          my_global = 1
        except:
          print("An exception occurred")
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    (* Handling exceptions doesn't result in an error. *)
    {|
      def foo() -> None:
        try:
          my_local= 5
        except NameError:
          print("Variable my_local is not defined")
        except:
          print("An exception occurred")
    |}
    [];
  ()


let test_list_global_leaks context =
  let assert_global_leak_errors = assert_global_leak_errors ~context in
  assert_global_leak_errors
    (* Appending to a local list doesn't error. *)
    {|
      my_global: List[int] = []
      def foo() -> None:
        my_local: List[int] = []
        my_local.append(123)
    |}
    [];
  assert_global_leak_errors
    (* Clearing and setting a local doesn't error. *)
    {|
      def foo() -> None:
        my_local: List[int] = []
        my_local[:] = [1,2]
    |}
    [];
  assert_global_leak_errors
    (* Getting a known mutable method from a local doesn't error. *)
    {|
      my_global: List[int] = []
      def foo() -> None:
        my_local: List[int] = []
        my_local.append
    |}
    [];
  assert_global_leak_errors
    (* Clearing and setting a global results in an error. *)
    {|
      my_global: List[int] = []
      def foo() -> None:
        my_global[:] = [1,2]
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.List[int]`."];
  assert_global_leak_errors
    (* Appending to a global results in an error. *)
    {|
      my_global: List[int] = []
      def foo() -> None:
        my_global.append(123)
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.List[int]`."];
  assert_global_leak_errors
    (* Getting a known mutable method from a global results in an error. *)
    {|
      my_global: List[int] = []
      def foo() -> None:
        my_global.append
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.List[int]`."];
  assert_global_leak_errors
    (* Setting an index in a local doesn't result in an error. *)
    {|
      my_global: List[int] = []
      def foo() -> None:
        my_local: List[int] = []
        my_local[0] = 123
    |}
    [];
  assert_global_leak_errors
    (* Setting an index in a global results in an error. *)
    {|
      my_global: List[int] = []
      def foo() -> None:
        my_global[0] = 123
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.List[int]`."];
  assert_global_leak_errors
    (* Calling a known mutable method on a list results in an error. *)
    {|
      my_global: List[int] = []
      def insert_global_list() -> None:
        my_global.insert(0, 1)
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.List[int]`."];
  assert_global_leak_errors
    (* Calling a known mutable method on a list results in an error. *)
    {|
      my_global: List[int] = []
      def extend_global_list() -> None:
        local_list: List[int] = [1]
        my_global.extend(local_list)
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.List[int]`."];
  assert_global_leak_errors
    (* In-place list extension on a local does not result in an error. *)
    {|
      my_global: List[int] = [1]
      def iadd_local_list() -> None:
        global my_global
        my_local: List[int] = [1]
        my_local += my_local
    |}
    [];
  assert_global_leak_errors
    (* In-place list extension on a global results in an error. *)
    {|
      my_global: List[int] = [1]
      def iadd_global_list() -> None:
        global my_global
        my_global += [1]
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.List[int]`."];
  assert_global_leak_errors
    (* Calling a non-mutable method on a global list does not result in an error. *)
    {|
      my_global: List[int] = []
      def non_mutable_global_call() -> None:
        global my_global
        my_global.clear()
    |}
    [];
  assert_global_leak_errors
    (* Calling a known mutable method on a global in a starred expression results in an error. *)
    {|
      my_global: Dict[str, List[int]] = {}
      def test() -> None:
        global my_global
        [
          *my_global.setdefault("x", [1])
        ]
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       typing.List[int]]`.";
    ];
  assert_global_leak_errors
    (* Calling a known mutable method on a global in list construction results in an error. *)
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        global my_global
        ls = [1, my_global.setdefault("a", 2), 3]
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    (* Calling a known mutable method on a global in the generator of a list comprehension results
       in an error. *)
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        global my_global
        ls = [i for i in range(my_global.setdefault("a", 1))]
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    (* Calling a known mutable method on a global in the element of a list comprehension results in
       an error. *)
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        global my_global
        ls = [my_global.setdefault("a", 1) for _ in range(10)]
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    (* Calling a known mutable method on a global in the condition of a list comprehension results
       in an error. *)
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        global my_global
        ls = [i for i in range(10) if i == my_global.setdefault("a", 1)]
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    (* Calling a known mutable method on a global the accessor of a list results in an error. *)
    {|
      my_global: Dict[str, int] = {}

      def foo() -> None:
        my_list: List[List[int]] = [[1], [], [2, 3]]

        my_list[my_global.setdefault("A", 1)].append(4)
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    (* Using a global in a list accessor when the global is not mutated does not result in an
       error. *)
    {|
      my_global: int = 1

      def foo() -> None:
        my_list: List[List[int]] = [[1], [], [2, 3]]

        my_list[my_global].append(4)
    |}
    [];
  assert_global_leak_errors
    (* Mutating a global with the walrus operator in a list constructor results in an error. *)
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        my_local = [1, my_global := 2, 3]
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    (* Assignment to a non-global with list deconstruction doesn't error. *)
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        my_local = 1
        [my_local, b] = 2, 3
    |}
    [];
  assert_global_leak_errors
    (* Assignment to a global in a list deconstruction errors. *)
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        [my_global, b] = 2, 3
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    (* Assignment to a position of a global in a list deconstruction errors. *)
    {|
      my_global: List[int] = []
      def foo() -> None:
        global my_global
        [my_global[0], b] = 2, 3
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.List[int]`."];
  assert_global_leak_errors
    (* Assignment to a starred global results in error. *)
    {|
      my_global: List[int] = []
      def foo() -> None:
        global my_global
        a, *my_global = [1, 2, 3]
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.List[int]`."];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo() -> None:
        my_list: List[int] = []
        my_list.append(my_global)
    |}
    [ (* TODO (T142189949): leaks should be detected for writing a global into a local *) ];
  ()


let test_tuple_global_leaks context =
  let assert_global_leak_errors = assert_global_leak_errors ~context in
  assert_global_leak_errors
    (* Calling a known mutable method on a global in tuple construction results in an error. *)
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        global my_global
        ls = (1, my_global.setdefault("a", 2), 3)
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    (* Calling a known mutable method on a global in the generator of a tuple comprehension results
       in an error. *)
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        global my_global
        generator = (i for i in range(my_global.setdefault("a", 1)))
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    (* Destructuring a tuple into a global results in an error. *)
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        my_global, y = 2, 3
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    (* Destructuring a tuple into a global results in an error. *)
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        x, my_global = 2, 3
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    (* Destructuring a tuple into a global results in an error. *)
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        (my_global, my_global), my_global = (1, 2), 3
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `int`.";
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `int`.";
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `int`.";
    ];
  assert_global_leak_errors
    (* Assignment to a global in a tuple deconstruction errors. *)
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        (my_global, b) = 2, 3
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    (* Assignment to a position of a non-global in a tuple deconstruction does not errors. *)
    {|
      my_global: List[int] = []
      def foo() -> None:
        global my_global
        my_local: List[int] = []
        (my_local[0], y) = 123, 456
    |}
    [];
  assert_global_leak_errors
    (* Assignment to a position of a global in a tuple deconstruction errors. *)
    {|
      my_global: List[int] = []
      def foo() -> None:
        global my_global
        (my_global[0], y) = 123, 456
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.List[int]`."];
  assert_global_leak_errors
    (* Assignment to a key of a global in a tuple deconstruction errors. *)
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        (my_global["x"], y) = 1, 2
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo() -> None:
        x: int
        y: int
        x, y = my_global, 1
    |}
    [ (* TODO (T142189949): leaks should be detected on global assignment to a local variable
         through tuple deconstruction *) ];
  ()


let test_dict_global_leaks context =
  let assert_global_leak_errors = assert_global_leak_errors ~context in
  assert_global_leak_errors
    (* Mutating a local dict does not result in an error. *)
    {|
      def foo() -> None:
        my_local: Dict[str, int] = {}
        my_local[str(1)] = 1
    |}
    [];
  assert_global_leak_errors
    (* Setting the key and value of a global dict results in an error. *)
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        my_global[str(1)] = 1
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    (* Setting the key and value of a global dict results in an error. *)
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        my_global["x"] = 1
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    (* Calling a known mutable method on a dict results in an error. *)
    {|
      my_global: Dict[str, int] = {}
      def update_global_dict() -> None:
        local_dict: Dict[str, int] = {"a": 1, "b": 2}
        my_global.update(local_dict)
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    (* Calling a known mutable method on a dict results in an error. *)
    {|
      my_global: Dict[str, int] = {}
      def setdefault_global_dict() -> None:
        my_global.setdefault("a", 1)
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
  (* Calling a known mutable method on a starred dict results in an error. *)
    ~skip_type_check:true (* Pyre is finding an issue with the test, but it is runnable in repl. *)
    {|
      my_global: Dict[str, int] = {}
      def test() -> Dict[str, int]:
        global my_global
        return {**my_global.setdefault("x", {"a": 1})}
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    (* Calling a known mutable method in dict construction results in an error. *)
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        global my_global
        local_dict = {"a": my_global.setdefault("a", 1)}
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    (* Calling a known mutable method in dict construction results in an error. *)
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        global my_global
        local_dict = {my_global.setdefault("a", 1): "a"}
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    (* Calling a known mutable method in a dict comprehension for element key results in an
       error. *)
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        global my_global
        local_dict = {my_global.setdefault("a", 1): i for i in range(10)}
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    (* Calling a known mutable method in a dict comprehension for element value results in an
       error. *)
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        global my_global
        local_dict = {i: my_global.setdefault("a", 1) for i in range(10)}
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    (* Calling a known mutable method in a dict comprehension for generator results in an error. *)
    {|
      my_global: Dict[str, int] = {"a": 1}
      def foo() -> None:
        global my_global
        local_dict = {i: i for i in range(my_global.setdefault("a", 10))}
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo() -> None:
        my_dict: Dict[str, int] = {}
        my_dict["my_global"] = my_global
    |}
    [ (* TODO (T142189949): leaks should be detected for writing a global into a local *) ];
  ()


let test_set_global_leaks context =
  let assert_global_leak_errors = assert_global_leak_errors ~context in
  assert_global_leak_errors
    (* Calling a known mutable method on a global set results in an error. *)
    {|
      my_global: Set[int] = set()
      def add_global_set() -> None:
        my_global.add(1)
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Set[int]`."];
  assert_global_leak_errors
    (* Calling a known mutable method on a global set results in an error. *)
    {|
      my_global: Set[int] = set()
      def update_my_global() -> None:
        my_global.update({15})
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Set[int]`."];
  assert_global_leak_errors
  (* Calling a known mutable method on a global set results in an error. *)
    ~skip_type_check:true (* Type checker errors on Set not being an AbstractSet. *)
    {|
      my_global: Set[int] = set()
      def ior_my_global() -> None:
        global my_global
        my_local: Set[int] = {1, 2, 3}
        my_global |= my_local
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Set[int]`."];
  assert_global_leak_errors
    (* Calling a known mutable method on a global set results in an error. *)
    {|
      my_global: Set[int] = set()
      def intersection_update_my_global() -> None:
        my_global.intersection_update({50, 23})
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Set[int]`."];
  assert_global_leak_errors
    (* Calling a known mutable method on a global set results in an error. *)
    {|
      my_global: Set[int] = set()
      def iand_my_global() -> None:
        global my_global
        my_global &= {50, 23}
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Set[int]`."];
  assert_global_leak_errors
    (* Calling a known mutable method on a global set results in an error. *)
    {|
      my_global: Set[int] = set()
      def difference_update_my_global() -> None:
        my_global.difference_update({39, 180})
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Set[int]`."];
  assert_global_leak_errors
    (* Calling a known mutable method on a global set results in an error. *)
    {|
      my_global: Set[int] = set()
      def isub_my_global() -> None:
        global my_global
        my_global -= {39, 180}
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Set[int]`."];
  assert_global_leak_errors
    (* Calling a known mutable method on a global set results in an error. *)
    {|
      my_global: Set[int] = set()
      def symmetric_difference_update_my_global() -> None:
        my_global.symmetric_difference_update({1, 2, 3})
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Set[int]`."];
  assert_global_leak_errors
    (* Calling a known mutable method on a global set results in an error. *)
    {|
      my_global: Set[int] = set()
      def ixor_my_global() -> None:
        global my_global
        my_global ^= {1, 2, 3}
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Set[int]`."];
  assert_global_leak_errors
    (* Calling a known mutable method on a global set results in an error. *)
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        global my_global
        ls = {1, my_global.setdefault("a", 2), 3}
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    (* Calling a known mutable method on a global set results in an error. *)
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        global my_global
        ls = {i for i in range(my_global.setdefault("a", 1))}
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];

  ()


let test_setattr_known_mutable_methods context =
  let assert_global_leak_errors = assert_global_leak_errors ~context in
  assert_global_leak_errors
    (* Calling object.__setattr__ on a non-global object does not result in an error. *)
    {|
      class MyClass:
        x: int = 5
        def __init__(self, x: int) -> None:
          object.__setattr__(self, "x", x)
    |}
    [];
  assert_global_leak_errors
    (* Calling setattr on a non-global object does not result in an error. *)
    {|
      class MyClass:
        x: int = 5
        def __init__(self, x: int) -> None:
          setattr(self, "x", x)
    |}
    [];
  assert_global_leak_errors
    (* Calling __setattr__ on a non-global object does not result in an error. *)
    {|
      class MyClass:
        x: int = 5
        def __init__(self, x: int) -> None:
          self.__setattr__("x", x)
    |}
    [];
  assert_global_leak_errors
    (* Calling setitem on a non-global object does not result in an error. *)
    {|
      class MyClass(Dict[str, int]):
        x: int = 5
        def __init__(self, x: int) -> None:
          super().__setitem__("x", 1)
    |}
    [];
  assert_global_leak_errors
    (* Calling object.__setattr__ on a global object results in an error. *)
    {|
      class MyClass:
        x: int = 5
        def __init__(self, x: int) -> None:
          object.__setattr__(MyClass, "x", x)
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.MyClass` of type \
       `typing.Type[test.MyClass]`.";
    ];
  assert_global_leak_errors
    (* Calling setattr on a global object results in an error. *)
    {|
      class MyClass:
        x: int = 5
        def __init__(self, x: int) -> None:
          setattr(MyClass, "x", x)
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.MyClass` of type \
       `typing.Type[test.MyClass]`.";
    ];
  assert_global_leak_errors
    (* Calling object.__setattr__ on a global object results in an error. *)
    {|
      class MyClass:
        x: int = 5

      def foo() -> None:
        object.__setattr__(MyClass, "x", 2)
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.MyClass` of type \
       `typing.Type[test.MyClass]`.";
    ];
  assert_global_leak_errors
    (* Calling setattr on a global object results in an error. *)
    {|
      class MyClass:
        x: int = 5

      def foo() -> None:
        setattr(MyClass, "x", 3)
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.MyClass` of type \
       `typing.Type[test.MyClass]`.";
    ];
  assert_global_leak_errors
    (* Calling setattr with a global mutation in the value position results in an error *)
    {|
      class MyClass:
        x: int = 5

      my_global: Dict[str, int] = {}

      def foo() -> None:
        setattr(MyClass(), "x", my_global.setdefault("x", 2))
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    (* Calling object.__setattr__ with a global mutation in the value position results in an
       error *)
    {|
      class MyClass:
        x: int = 5

      my_global: Dict[str, int] = {}

      def foo() -> None:
        object.__setattr__(MyClass(), "x", my_global.setdefault("x", 2))
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    (* Calling __setitem__ on a global results in an error. *)
    {|
      class MyClass:
        x: int = 5

        def __setitem__(self, key: int, value: int) -> None:
          self.x = value

      my_global: MyClass = MyClass()

      def foo() -> None:
        my_global[-1] = 2
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `test.MyClass`."];
  assert_global_leak_errors
    (* Calling __setitem__ on a local does not result in an error. *)
    {|
      class MyClass:
        x: int = 5

        def __setitem__(self, key: int, value: int) -> None:
          self.x = value

      def foo() -> None:
        my_local: MyClass = MyClass()
        my_local[-1] = 2
    |}
    [];
  ()


let test_object_global_leaks context =
  let assert_global_leak_errors = assert_global_leak_errors ~context in
  assert_global_leak_errors
    {|
      class MyClass:
        x: int
        def __init__(self, x: int) -> None:
          self.x = x

      my_global: MyClass = MyClass(1)

      def foo() -> None:
        my_global.x = 2
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `test.MyClass`."];
  assert_global_leak_errors
    {|
      class MyClass:
        x: int
        def __init__(self, x: int) -> None:
          self.x = x

        def set_x(self, x: int) -> None:
          self.x = x

      my_global: MyClass = MyClass(1)

      def foo() -> None:
        my_global.set_x(2)
    |}
    [ (* TODO (T142189949): leaks should be detected on object attribute mutations *) ];
  assert_global_leak_errors
    {|
      class MyClass:
        y: int
        def __init__(self, y: int) -> None:
          self.y = y

      class MyClass2:
        x: MyClass
        def __init__(self, x: MyClass) -> None:
          self.x = x

      my_global: MyClass2 = MyClass2(MyClass(1))

      def foo() -> None:
        my_global.x.y = 3
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `test.MyClass2`."];
  assert_global_leak_errors
    {|
      class MyClass:
        my_list: List[int]
        def __init__(self, x: int) -> None:
          self.my_list = [x]

      def foo() -> None:
        MyClass(2).my_list.append(1)
    |}
    [];
  assert_global_leak_errors
    {|
      import collections
      class MyClass:
        my_list: List[int] = []

      def foo() -> None:
        MyClass.my_list.append(1)
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.MyClass` of type \
       `typing.Type[test.MyClass]`.";
    ];
  assert_global_leak_errors
    ~other_sources:
      [
        {
          handle = "other_module.py";
          source = {|
        from typing import List
        my_list: List[int] = []
      |};
        };
      ]
    {|
      import other_module
      class MyClass:
        my_list: List[int]
        def __init__(self, x: int) -> None:
          self.my_list = [x]

      def foo() -> None:
        other_module.my_list.append(1)
    |}
    [
      "Global leak [3100]: Data is leaked to global `other_module.my_list` of type \
       `typing.List[int]`.";
    ];
  assert_global_leak_errors
    {|
      class MyClass:
        x: int
        def __init__(self, x: int) -> None:
          self.x = x

      def foo() -> None:
        MyClass.x = 2
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.MyClass` of type \
       `typing.Type[test.MyClass]`.";
    ];
  assert_global_leak_errors
    {|
      class MyClass:
        x = 5

      def foo() -> None:
        MyClass.x = 2
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.MyClass` of type \
       `typing.Type[test.MyClass]`.";
    ];
  assert_global_leak_errors (* Global leaks are found in calls to constructors. *)
    ~skip_type_check:true
      (* The types don't match up correctly, but this is an easy way to verify leaks in calls. *)
    {|
      my_global: Dict[str, int] = {}

      class A:
        x: Dict[str, int]
        def __init__(self, x: Dict[str, int]) -> None:
          self.x = x

      def foo() -> A:
        return A(my_global.setdefault("a", 1))
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    (* Aliasing a class will still properly interact with global leaks. *)
    {|
      class A:
        x: int = 5

      B = A
      C = A

      def foo() -> None:
        B().x = 6
        C.x = 6
    |}
    ["Global leak [3100]: Data is leaked to global `test.C` of type `typing.Type[test.A]`."];
  assert_global_leak_errors
    (* Returning a class from a function will still find a global leak. *)
    {|
      class MyClass:
        x: List[int] = []

      def get_class() -> Type[MyClass]:
        return MyClass

      def foo() -> None:
        get_class().x.append(5)
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.MyClass` of type \
       `typing.Type[test.MyClass]`.";
    ];
  assert_global_leak_errors
    (* Returning a class from a function will still find a global leak. *)
    {|
      class MyClass:
        x: int = 1

      def get_class() -> Type[MyClass]:
        return MyClass

      def foo() -> None:
        get_class().x = 5
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.MyClass` of type \
       `typing.Type[test.MyClass]`.";
    ];
  assert_global_leak_errors
    (* Returning a class from a function will still find a global leak. *)
    {|
      class MyClass:
        x: int = 1

      def foo() -> None:
        def get_class() -> Type[MyClass]:
          return MyClass

        get_class().x = 5
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.MyClass` of type \
       `typing.Type[test.MyClass]`.";
    ];
  assert_global_leak_errors
    (* A mutation on something returned from a class does not result in an error. *)
    {|
      class MyClass:
        current: Optional[MyClass] = None
        x: List[int]

        def __init__(self) -> None:
          self.x = []

        @classmethod
        def get_current(cls) -> MyClass:
          current = cls.current
          if not current:
            current = MyClass()
            cls.current = current
          return current

      def foo() -> None:
        MyClass.get_current().x.append(2)
    |}
    [];
  assert_global_leak_errors
    (* Getting a class and instantiating it from a function finds a leak with assignment. *)
    {|
      class MyClass:
        x: int = 1

      def get_class() -> Type[MyClass]:
        return MyClass

      def foo() -> None:
        get_class()().x = 5
    |}
    [];
  assert_global_leak_errors
    (* Getting a class and instantiating it doesn't find a global leak with a mutable call. *)
    {|
      class MyClass:
        x: List[int] = []

      def get_class() -> Type[MyClass]:
        return MyClass

      def foo() -> None:
        get_class()().x.append(5)
    |}
    [];
  assert_global_leak_errors
    (* A mutation on something returned from a class does not result in an error. *)
    {|
      class MyClass:
        current: Optional[MyClass] = None
        x: int

        def __init__(self) -> None:
          self.x = 1
	
        @classmethod
        def get_current(cls) -> MyClass:
          current = cls.current
          if not current:
            current = MyClass()
            cls.current = current
          return current

      def foo() -> None:
        MyClass.get_current().x = 2
    |}
    [];
  ()


let test_global_statements context =
  let assert_global_leak_errors = assert_global_leak_errors ~context in
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo() -> None:
        y: int = my_global
    |}
    [ (* TODO (T142189949): leaks should be detected on global assignment to a local variable *) ];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo() -> None:
        my_set: Set[int] = set()
        my_set.add(my_global)
    |}
    [ (* TODO (T142189949): leaks should be detected for writing a global into a local *) ];
  assert_global_leak_errors
    {|
      class MyClass:
        x: int
        def __init__(self, x: int) -> None:
          self.x = x

      my_global: int = 1

      def foo() -> None:
        my_obj: MyClass = MyClass(1)
        my_obj.x = my_global
    |}
    [ (* TODO (T142189949): leaks should be detected for writing a global into a local *) ];
  assert_global_leak_errors
    {|
      class MyClass:
        x: int
        def __init__(self, x: int) -> None:
          self.x = x

      my_global: int = 1

      def foo() -> None:
        MyClass(1).x = my_global
    |}
    [ (* TODO (T142189949): leaks should be detected for writing a global into a local *) ];
  assert_global_leak_errors
    {|
      class MyClass:
        x: int
        def __init__(self, x: int) -> None:
          self.x = x

      my_global: MyClass = MyClass(10)

      def foo() -> None:
        my_local = my_global.x
    |}
    [ (* TODO (T142189949): should this be allowed? *) ];
  assert_global_leak_errors
    {|
      async def test() -> AsyncGenerator[None, None]:
        print("Hello")
        yield
    |}
    [];
  assert_global_leak_errors
    {|
      async def test() -> AsyncGenerator[int, None]:
        yield from (i for i in range(10))
    |}
    [];
  assert_global_leak_errors
    {|
      my_global: Dict[str, int] = {}
      def test() -> None:
        global my_global
        ~my_global.setdefault("a", 1)
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    {|
      my_global: Dict[str, int] = {}

      async def async_func(x: int) -> None: ...

      def test() -> None:
        global my_global
        await async_func(my_global.setdefault("a", 1))
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        my_global.setdefault("a", 1) and True
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        True or my_global.setdefault("a", 1)
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        5 > my_global.setdefault("a", 1)
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        my_global.setdefault("a", 1) == 5
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        a = "hello"
        f"{a} world: {my_global.setdefault('a', 1) == 5}"
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    {|
      my_global: List[int] = []
      def foo() -> None:
        a = lambda: my_global.append(1)
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.List[int]`."];
  assert_global_leak_errors
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        a = lambda x = my_global.setdefault("a", 1): x
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        a = my_global.setdefault("a", 1) if False else 3
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        a = 1 if my_global.setdefault("a", 1) else 2
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        a = 1 if True else my_global.setdefault("a", 1)
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    ~skip_type_check:true
      (* This is a bug in Pyre, where scoping rules are not always correctly applied. *)
    {|
      my_global: int = 1
      def foo() -> None:
        if my_global == 1:
          my_global = 3
    |}
    [];
  assert_global_leak_errors
    ~skip_type_check:true
      (* This is a bug in Pyre, where scoping rules are not always correctly applied. *)
    {|
      my_global: int = 1
      def foo() -> None:
        if my_global == 1:
          my_global = 3
        else:
          my_global = 4
    |}
    [];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo() -> None:
        if my_global := 3:
          my_global = 3
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        if my_global := (a := 3):
          print("hi")
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        if True:
          my_global = 2
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        if True:
          my_global = 2
        else:
          my_local = 3
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        if True:
          my_local = 2
        else:
          my_global = 3
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        if True:
          my_local = 2
        elif my_global == 1:
          my_global = 3
        else:
          my_local = 4
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    {|
      my_global: int = 0
      my_global_t: int = 1
      def foo() -> None:
        global my_global
        if True:
          my_local = 2
        elif my_global_t := 3:
          my_local = 3
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global_t` of type `int`."];
  assert_global_leak_errors
    {|
      my_global: int = 0
      def foo() -> None:
        while True:
          my_global += 1
    |}
    [];
  assert_global_leak_errors
    {|
      my_global: int = 0
      def foo() -> None:
        while True:
          my_local += 1
    |}
    [];
  assert_global_leak_errors
    {|
      my_global: int = 0
      def foo() -> None:
        global my_global
        while True:
          my_global += 1
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    {|
      my_global: int = 0
      def foo() -> None:
        global my_global
        while my_global := 3:
          print("hi")
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    {|
      my_global: int = 0
      def foo() -> None:
        while my_global := 3:
          print("hi")
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        for i in "hello":
          print("hi")
    |}
    [];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo() -> None:
        for i in range(0, my_global):
          print("hi")
    |}
    [];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        for i in range(0, 10):
          my_global = 4
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    {|
      my_global: List[int] = []
      def foo() -> None:
        for i in (my_global := [1]):
          print("hi")
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.List[int]`."];
  assert_global_leak_errors
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        for i in range(my_global.setdefault("a", 1)):
          print("hi")
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    {|
      def foo() -> None:
        with open("hello", "r") as f:
          print("hi")
    |}
    [];
  assert_global_leak_errors
    {|
      def foo() -> None:
        with open("hello", "r") as f, open("world", "r") as f1:
          print("hi")
    |}
    [];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        with open("hello", "r") as f:
          my_global = 2
          print("hi")
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        with open("hello", "r") as my_global:
          print("hi")
    |}
    (* TODO (T142189949): should pyre figure out the right type here? *)
    ["Global leak [3100]: Data is leaked to global `my_global` of type `unknown`."];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        with open("hello", "r") as f, open("world", "r") as f1:
          my_global = 2
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo() -> None:
        global my_global
        with open("hello", "r") as my_global, open("world", "r") as f1:
          print("hi")
    |}
    (* TODO (T142189949): should pyre figure out the right type here? *)
    ["Global leak [3100]: Data is leaked to global `my_global` of type `unknown`."];
  assert_global_leak_errors
    ~skip_type_check:true (* This is invalid, but we want to make sure unknown type stuff works. *)
    {|
      from other_module import my_list
      def foo() -> None:
        global my_list
        my_list.append(1)
    |}
    ["Global leak [3100]: Data is leaked to global `other_module.my_list` of type `unknown`."];
  assert_global_leak_errors
    ~other_sources:
      [
        {
          handle = "other_module.py";
          source = {|
        from typing import List
        my_list: List[int] = []
      |};
        };
      ]
    {|
      from other_module import my_list
      def foo() -> None:
        global my_list
        my_list.append(1)
    |}
    [
      "Global leak [3100]: Data is leaked to global `other_module.my_list` of type \
       `typing.List[int]`.";
    ];
  assert_global_leak_errors
    ~other_sources:
      [
        {
          handle = "other_module.py";
          source = {|
        from typing import List
        my_list: List[int] = []
      |};
        };
      ]
    {|
      from other_module import my_list
      def foo() -> None:
        my_list.append(1)
    |}
    [
      "Global leak [3100]: Data is leaked to global `other_module.my_list` of type \
       `typing.List[int]`.";
    ];
  ()


let test_recursive_coverage context =
  let assert_global_leak_errors = assert_global_leak_errors ~context in
  assert_global_leak_errors
    {|
      my_global: List[int] = []
      def foo() -> None:
        print(my_global.append(123))
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.List[int]`."];
  assert_global_leak_errors
    {|
      my_global: List[int] = []
      def foo() -> None:
        global my_global
        a = my_global.append(123)
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.List[int]`."];
  assert_global_leak_errors
    {|
      my_global: List[List[int]] = [[]]
      def foo() -> None:
        global my_global
        my_global[0].append(1)
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type \
       `typing.List[typing.List[int]]`.";
    ];
  assert_global_leak_errors
    {|
      class MyClass:
        def get(self, x: int) -> List[int]:
          return [x] * x

      my_global: MyClass = MyClass()

      def foo() -> None:
        global my_global
        my_global.get(2)[0] = 5
    |}
    [ (* TODO (T142189949): mutations on call returns for globals should not result in error *) ];
  assert_global_leak_errors
    {|
      my_global: Dict[str, int] = {}
      def setdefault_global_dict() -> int:
        return my_global.setdefault("a", 2)
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    {|
      my_global: Dict[str, int] = {}
      def test() -> Generator[int, None, None]:
        global my_global
        yield my_global.setdefault("a", 1)
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    {|
      my_global: Dict[str, int] = {}
      def test() -> Generator[int, None, None]:
        global my_global
        yield my_global.setdefault("a", 1)
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    {|
      my_global: Dict[str, int] = {}
      def test() -> List[Sequence[int]]:
        global my_global
        return [
          (my_global.setdefault("a", 1), 2),
          [my_global.setdefault("a", 1), 2],
        ]
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        my_local = [1, 2, 3]
        my_local[my_global.setdefault("a", 1)] = 0
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        my_local = [1, 2, 3]
        my_local[int(my_global.setdefault("a", 2) < 5)] = 0
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        my_local = [1, 2, 3]
        my_local[int(my_global.setdefault("a", 1) and True)] = 0
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    {|
      my_global: Dict[str, int] = {}
      def foo() -> None:
        a = (1 if my_global.setdefault("a", 1) else 2) if True else 4
    |}
    [
      "Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.Dict[str, \
       int]`.";
    ];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo() -> None:
        try:
          my_local = 1
        except:
          global my_global
          my_global = 1
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo() -> None:
        try:
          my_local = 1
        except:
          my_local = 2
        else:
          global my_global
          my_global = 1
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo() -> None:
        try:
          my_local = 1
        except:
          my_local = 2
        finally:
          global my_global
          my_global = 1
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    {|
      def nested_run() -> None:
        def do_the_thing() -> None:
          my_local: List[int] = []
          my_local.append(1)

        do_the_thing()
    |}
    [];
  assert_global_leak_errors
    {|
      my_global: List[int] = []

      def nested_run() -> None:
        def do_the_thing() -> None:
          my_global.append(1)

        do_the_thing()
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.List[int]`."];
  ()


let test_global_returns context =
  let assert_global_leak_errors = assert_global_leak_errors ~context in
  assert_global_leak_errors
    (* Returning a global results in an error. *)
    {|
      my_global: int = 1
      def foo() -> int:
        return my_global
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `int`."];
  assert_global_leak_errors
    (* Returning a member from a global results in an error. *)
    {|
      my_global: List[int]
      def foo() -> int:
        return my_global[0]
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `typing.List[int]`."];
  assert_global_leak_errors
    (* Returning an attribute from a global results in an error. *)
    {|
      class MyClass:
        x: int = 5

      my_global: MyClass = MyClass()
      def foo() -> int:
        return my_global.x
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global` of type `test.MyClass`."];
  ()


let () =
  "global_leaks"
  >::: [
         "global_assignment" >:: test_global_assignment;
         "list_global_leaks" >:: test_list_global_leaks;
         "tuple_global_leaks" >:: test_tuple_global_leaks;
         "dict_global_leaks" >:: test_dict_global_leaks;
         "set_global_leaks" >:: test_set_global_leaks;
         "other_known_mutable_methods" >:: test_setattr_known_mutable_methods;
         "object_global_leaks" >:: test_object_global_leaks;
         "global_statements" >:: test_global_statements;
         "recursive_coverage" >:: test_recursive_coverage;
         "global_returns" >:: test_global_returns;
         "global_exceptions" >:: test_global_exceptions;
       ]
  |> Test.run
