(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Analysis
open TypeInference.Data
open Test

let setup_environment scratch_project =
  let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
    scratch_project |> ScratchProject.build_global_environment
  in
  global_environment


module Setup = struct
  let set_up_project ~context code =
    let ({ ScratchProject.configuration; _ } as project) =
      ScratchProject.setup ~context ["test.py", code] ~infer:true
    in
    let environment =
      setup_environment project |> TypeEnvironment.create |> TypeEnvironment.read_only
    in
    environment, configuration


  let run_inference ~context ~target code =
    let environment, configuration = set_up_project ~context code in
    let global_resolution = environment |> TypeEnvironment.ReadOnly.global_resolution in
    let source =
      let ast_environment = GlobalResolution.ast_environment global_resolution in
      AstEnvironment.ReadOnly.get_processed_source ast_environment (Reference.create "test")
      |> fun option -> Option.value_exn option
    in
    let module_results =
      TypeInference.Local.infer_for_module ~configuration ~global_resolution ~source
    in
    let is_target { LocalResult.define = { name; _ }; _ } =
      Reference.equal name (Reference.create target)
    in
    let first = function
      | head :: _ -> head
      | [] -> failwith ("Could not find target define " ^ target)
    in
    module_results |> List.filter ~f:is_target |> first
end

let assert_json_equal ~context ~expected result =
  let expected = Yojson.Safe.from_string expected in
  assert_equal ~ctxt:context ~printer:Yojson.Safe.pretty_to_string expected result


let access_by_field_name field body =
  match body with
  | `Assoc list -> List.Assoc.find ~equal:String.equal list field |> Option.value ~default:`Null
  | _ ->
      failwith
        (Format.asprintf
           "TEST FAIL: ERROR ACCESSING FIELD %s IN JSON %s"
           field
           (Yojson.Safe.pretty_to_string body))


let access_by_path field_path body =
  let rec go path_so_far body_so_far =
    match path_so_far with
    | [] -> body_so_far
    | field_name :: rest_of_path ->
        body_so_far |> access_by_field_name field_name |> go rest_of_path
  in
  go field_path body


let check_inference_results ?(field_path = []) ~context ~target ~expected code =
  code
  |> Setup.run_inference ~context ~target
  |> LocalResult.to_yojson
  |> access_by_path field_path
  |> assert_json_equal ~context ~expected


let test_inferred_returns context =
  let check_inference_results = check_inference_results ~field_path:["define"; "return"] ~context in
  check_inference_results
    {|
      def foo():
          pass
    |}
    ~target:"test.foo"
    ~expected:{|"None"|};
  check_inference_results
    {|
      def foo(x: int):
          return x
    |}
    ~target:"test.foo"
    ~expected:{|"int"|};
  check_inference_results
    {|
      def foo() -> int:
          pass
    |}
    ~target:"test.foo"
    ~expected:{|"int"|};
  check_inference_results
    {|
      def foo():
          x = 1
          return x
    |}
    ~target:"test.foo"
    ~expected:{|"int"|};
  check_inference_results
    {|
      def foo():
          return
    |}
    ~target:"test.foo"
    ~expected:{|"None"|};
  check_inference_results
    {|
      def foo():
          return None
    |}
    ~target:"test.foo"
    ~expected:{|"None"|};
  check_inference_results
    {|
      def foo(b: bool):
          if b:
              return "hello"
          else:
              return 0
    |}
    ~target:"test.foo"
    ~expected:{|"typing.Union[int, str]"|};
  check_inference_results
    {|
      def other() -> int:
          return 1

      def foo():
          x = "string"
          x = other()
          return x
    |}
    ~target:"test.foo"
    ~expected:{|"int"|};
  check_inference_results
    {|
      def foo():
          x = undefined
    |}
    ~target:"test.foo"
    ~expected:{|"None"|};
  check_inference_results
    {|
      def foo(a):
          x, _, z = a.b(':')
          return z, x
    |}
    ~target:"test.foo"
    ~expected:{|null|};
  check_inference_results
    {|
    def foo():
        if 1 > 2:
            x = 2
        else:
            assert not True
        return x
    |}
    ~target:"test.foo"
    ~expected:{|"int"|};
  check_inference_results
    {|
      class Test(object):
          def ret_self(self):
              return self
    |}
    ~target:"test.Test.ret_self"
    ~expected:{|"Test"|};
  check_inference_results
    {|
      def foo():
          return [1]
    |}
    ~target:"test.foo"
    ~expected:{|"typing.List[int]"|};
  (* TODO(T84365830): Implement support for empty containers. *)
  check_inference_results
    {|
      def foo():
          return []
    |}
    ~target:"test.foo"
    ~expected:{|null|};
  check_inference_results
    {|
      def foo():
          return {}
    |}
    ~target:"test.foo"
    ~expected:{|null|};
  (* TODO(T84365830): Do a bit more guessing for containers of Any *)
  check_inference_results
    {|
      from typing import Any
      def foo(x: Any):
          return {"": x}
    |}
    ~target:"test.foo"
    ~expected:{|null|};
  check_inference_results
    {|
      def foo(y: bool):
          x = {}
          if y:
              x["a"] = 1
          return x
    |}
    ~target:"test.foo"
    ~expected:{|"typing.Dict[str, int]"|};
  check_inference_results
    {|
      def foo():
          y = {}
          list = [1, 2, 3]
          for num in list:
              y["a"] = num
          return y
    |}
    ~target:"test.foo"
    ~expected:{|"typing.Dict[str, int]"|};
  check_inference_results
    {|
        def foo():
            x = []
            x.append("")
            x.append(1)
            return x
    |}
    ~target:"test.foo"
    ~expected:{|"typing.List[typing.Union[int, str]]"|};
  check_inference_results
    {|
      class A:
          @abstractmethod
          def foo():
              pass
    |}
    ~target:"test.A.foo"
    ~expected:{|null|};
  check_inference_results
    {|
      def foo():
          return ("", "", "", "")
    |}
    ~target:"test.foo"
    ~expected:{|"typing.Tuple[str, ...]"|};
  check_inference_results
    {|
      def foo():
          return ("", "", "", 2)
    |}
    ~target:"test.foo"
    ~expected:{|"typing.Tuple[str, str, str, int]"|};
  check_inference_results
    {|
      def foo():
          def bar(x: int) -> str:
              return ""
          return bar
    |}
    ~target:"test.foo"
    ~expected:{|"typing.Callable[[int], str]"|};
  check_inference_results
    {|
      def foo():
          def bar(x: int, y: str) -> bool:
              pass
          return [bar]
    |}
    ~target:"test.foo"
    ~expected:{|"typing.List[typing.Callable[[int, str], bool]]"|};
  ()


let option_to_json string_option =
  string_option |> Option.map ~f:(Format.asprintf "\"%s\"") |> Option.value ~default:"null"


let test_inferred_function_parameters context =
  let check_inference_results =
    check_inference_results ~field_path:["define"; "parameters"] ~context
  in
  let single_parameter ?(name = "x") ?default type_ =
    Format.asprintf
      {|
        [
          { "name": "%s", "annotation": "%s", "value": %s, "index": 0 }
        ]
      |}
      name
      type_
      (option_to_json default)
  in
  check_inference_results
    {|
      def foo(x: typing.Any) -> None:
          x = 5
    |}
    ~target:"test.foo"
    ~expected:(single_parameter "int");
  check_inference_results
    {|
      def foo(x) -> int:
          return x
    |}
    ~target:"test.foo"
    ~expected:(single_parameter "int");
  check_inference_results
    {|
      def foo(x) -> None:
          y = 1
          x = y
    |}
    ~target:"test.foo"
    ~expected:(single_parameter "int");
  check_inference_results
    {|
      def foo(x) -> int:
          y = x
          return y
    |}
    ~target:"test.foo"
    ~expected:(single_parameter "int");
  check_inference_results
    {|
      def foo(x) -> int:
          y = 5
          x = y
          return x
    |}
    ~target:"test.foo"
    ~expected:(single_parameter "int");
  check_inference_results
    {|
      def foo(y) -> int:
          z = y
          x = y
          return x
    |}
    ~target:"test.foo"
    ~expected:(single_parameter ~name:"y" "int");
  check_inference_results
    {|
      def foo(x = 5) -> int:
          return x
    |}
    ~target:"test.foo"
    ~expected:(single_parameter ~default:"5" "int");
  check_inference_results
    {|
      def foo(x: typing.Any = 5) -> None:
          pass
    |}
    ~target:"test.foo"
    ~expected:(single_parameter ~default:"5" "int");
  (* TODO(T84365830): Ensure we correctly qualify inferred parameter types. *)
  check_inference_results
    {|
      from typing import Optional
      def foo(x) -> Optional[str]:
          return x
    |}
    ~target:"test.foo"
    ~expected:(single_parameter "Optional[str]");
  check_inference_results
    {|
      def foo(y) -> typing.Tuple[int, float]:
          x = y
          z = y
          return (x, z)
    |}
    ~target:"test.foo"
    ~expected:(single_parameter ~name:"y" "int");
  check_inference_results
    {|
      def foo(x) -> typing.Tuple[int, float]:
          z = y
          x = y
          return (x, z)
    |}
    ~target:"test.foo"
    ~expected:(single_parameter "int");
  (* TODO(T84365830): Handle union with default values *)
  check_inference_results
    {|
      def foo(x = None) -> None:
          if x:
              x = ""
    |}
    ~target:"test.foo"
    ~expected:(single_parameter ~default:"None" "str");
  check_inference_results
    {|
      from typing import Optional
      def foo(x: Optional[str]):
          return x

      def bar(x = None) -> None:
          foo(x)
    |}
    ~target:"test.foo"
    ~expected:(single_parameter "typing.Optional[str]");
  check_inference_results
    {|
      from typing import Optional
      def foo(x: Optional[str]):
          return x

      def bar(x = None) -> None:
          foo(x)
    |}
    ~target:"test.bar"
    ~expected:(single_parameter ~default:"None" "Optional[str]");
  let no_inferences =
    {|
      [{ "name": "x", "annotation": null, "value": null, "index": 0 }]
    |}
  in
  (* TODO(T84365830): Support inference on addition. *)
  check_inference_results
    {|
      def foo(x) -> None:
          x += 1
    |}
    ~target:"test.foo"
    ~expected:no_inferences;
  (* Ensure analysis doesn't crash when __iadd__ is called with non-simple names. *)
  check_inference_results
    {|
      def foo(x) -> None:
          x[0] += y[3]
    |}
    ~target:"test.foo"
    ~expected:no_inferences;
  (* TODO(T84365830): Implement support for partial annotations *)
  check_inference_results
    {|
      from typing import Optional
      def foo(x) -> None:
          y: List[Any] = []
          x = y
    |}
    ~target:"test.foo"
    ~expected:no_inferences;
  check_inference_results
    {|
      def foo(x) -> int:
          x = y
          y = z
          return z
    |}
    ~target:"test.foo"
    ~expected:no_inferences;
  check_inference_results
    {|
      def foo(x, y) -> int:
          b = 5
          a, b = x, y
          a += b
          return a
    |}
    ~target:"test.foo"
    ~expected:
      {|
        [
          { "name": "x", "annotation": "int", "value": null, "index": 0 },
          { "name": "y", "annotation": "int", "value": null, "index": 1 }
        ]
      |};
  (* TODO(T84365830): Handle nested tuples. *)
  check_inference_results
    {|
      def foo(x, y, z) -> typing.Tuple[typing.Tuple[str, int], bool]:
          return ((x, y), z)
    |}
    ~target:"test.foo"
    ~expected:
      {|
        [
          { "name": "x", "annotation": null, "value": null, "index": 0 },
          { "name": "y", "annotation": null, "value": null, "index": 1 },
          { "name": "z", "annotation": "bool", "value": null, "index": 2 }
        ]
     |};
  check_inference_results
    {|
      def foo(a, x = 15):
          b = a.c()
          b = int(b)
          if b > x:
              x = b
    |}
    ~target:"test.foo"
    ~expected:
      {|
        [
          { "name": "a", "annotation": null, "value": null, "index": 0 },
          { "name": "x", "annotation": "int", "value": "15", "index": 1 }
        ]
     |};
  ()


let test_inferred_method_parameters context =
  let check_inference_results =
    check_inference_results ~context ~field_path:["define"; "parameters"]
  in
  let single_parameter_method type_ =
    Format.asprintf
      {|
        [
          { "name": "self", "annotation": null, "value": null, "index": 0 },
          { "name": "x", "annotation": %s, "value": null, "index": 1 }
        ]
      |}
      (option_to_json type_)
  in
  check_inference_results
    {|
      class A:
          def foo(self, x: int) -> None: ...
      class B(A):
          def foo(self, x) -> None:
              return x
    |}
    ~target:"test.B.foo"
    ~expected:(single_parameter_method (Some "int"));
  check_inference_results
    {|
      class A:
          def foo(self, x: "A") -> "A": ...
      class B(A):
          def foo(self, x):
              return x
    |}
    ~target:"test.B.foo"
    ~expected:(single_parameter_method (Some "A"));
  (* Don't override explicit annotations if they clash with parent class *)
  check_inference_results
    {|
      class A:
          def foo(self, x: int) -> int: ...
      class B(A):
          def foo(self, x: str) -> str:
              return x
    |}
    ~target:"test.B.foo"
    ~expected:(single_parameter_method (Some "str"));
  let no_inferences = single_parameter_method None in
  check_inference_results
    {|
      from typing import Any
      class A:
          def foo(self, x: Any) -> int: ...
      class B(A):
          def foo(self, x):
              return x
    |}
    ~target:"test.B.foo"
    ~expected:no_inferences;
  check_inference_results
    {|
      from typing import TypeVar
      T = TypeVar("T")
      class A:
          def foo(self, x: T) -> T: ...
      class B(A):
          def foo(self, x):
              return x
    |}
    ~target:"test.B.foo"
    ~expected:no_inferences;
  check_inference_results
    {|
      class A:
          def foo(self, x: int) -> int: ...
      class B(A):
          def foo(self, x):
              return x
      class C(B):
          def foo(self, x):
              return x + 1
    |}
    ~target:"test.C.foo"
    ~expected:no_inferences;
  (* Do not propagate types on `self` *)
  check_inference_results
    {|
      class A:
          def foo(self: "A") -> str: ...
      class B(A):
          def foo(self):
              return x
    |}
    ~target:"test.B.foo"
    ~expected:{|
        [{ "name": "self", "annotation": null, "value": null, "index": 0 }]
     |};
  (* Do not propagate types on `self` *)
  check_inference_results
    {|
      class A:
          def foo(self, x: int, y: str) -> int: ...
      class B(A):
          def foo(self, x, y: str):
              return x
    |}
    ~target:"test.B.foo"
    ~expected:
      {|
        [
          { "name": "self", "annotation": null, "value": null, "index": 0 },
          { "name": "x", "annotation": "int", "value": null, "index": 1 },
          { "name": "y", "annotation": "str", "value": null, "index": 2 }
        ]
      |};
  check_inference_results
    {|
      class A:
          def foo(self, *args: str, **kwargs: float) -> int: ...
      class B(A):
          def foo(self, *args, **kwargs):
              return x
    |}
    ~target:"test.B.foo"
    ~expected:
      {|
        [
          { "name": "self", "annotation": null, "value": null, "index": 0 },
          { "name": "*args", "annotation": "str", "value": null, "index": 1 },
          {
            "name": "**kwargs",
            "annotation": "float",
            "value": null,
            "index": 2
          }
        ]
     |};
  ()


let test_inferred_globals context =
  let check_inference_results = check_inference_results ~context ~field_path:["globals"] in
  check_inference_results
    {|
      def foo() -> int:
          return 1234
      x = foo()
    |}
    ~target:"test.$toplevel"
    ~expected:
      {|
        [
            {
              "name": "x",
              "location": { "qualifier": "test", "path": "test.py", "line": 4 },
              "annotation": "int"
            }
        ]
      |};
  check_inference_results
    {|
      x = 1 + 1
    |}
    ~target:"test.$toplevel"
    ~expected:
      {|
        [
          {
            "name": "x",
            "location": { "qualifier": "test", "path": "test.py", "line": 2 },
            "annotation": "int"
          }
        ]
      |};
  check_inference_results
    {|
      x = unknown
      def foo() -> int:
          return x
    |}
    ~target:"test.$toplevel"
    ~expected:{|[]|};
  (* TODO(T84365830): Implement support for global inference due to local usage. *)
  check_inference_results
    {|
      x = unknown
      def foo() -> None:
          global x
          x = 1
    |}
    ~target:"test.$toplevel"
    ~expected:{|[]|};
  (* Note: locally-inferred None annotations like this are either widenened into Optional types or
     suppressed by GlobalResult *)
  check_inference_results
    {|
      foo = None
    |}
    ~target:"test.$toplevel"
    ~expected:
      {|
        [
          {
            "name": "foo",
            "location": { "qualifier": "test", "path": "test.py", "line": 2 },
            "annotation": "None"
          }
        ]
      |};
  ()


let test_inferred_attributes context =
  let check_inference_results = check_inference_results ~context ~field_path:["attributes"] in
  check_inference_results
    {|
      def foo() -> int:
          return 1
      class Foo:
          x = foo()
    |}
    ~target:"test.Foo.$class_toplevel"
    ~expected:
      {|
        [
          {
            "parent": "Foo",
            "name": "x",
            "location": { "qualifier": "test", "path": "test.py", "line": 5 },
            "annotation": "int"
          }
        ]
      |};
  check_inference_results
    {|
      class Foo:
          x = 1 + 1
    |}
    ~target:"test.Foo.$class_toplevel"
    ~expected:
      {|
        [
          {
            "parent": "Foo",
            "name": "x",
            "location": { "qualifier": "test", "path": "test.py", "line": 3 },
            "annotation": "int"
          }
        ]
      |};
  check_inference_results
    {|
      class Foo:
          def __init__(self) -> None:
              self.x = 1 + 1
    |}
    ~target:"test.Foo.__init__"
    ~expected:
      {|
        [
          {
            "parent": "Foo",
            "name": "x",
            "location": { "qualifier": "test", "path": "test.py", "line": 4 },
            "annotation": "int"
          }
        ]
      |};
  check_inference_results
    {|
      class Foo:
          def __init__(self) -> None:
              self.x = self.foo()

          def foo(self) -> int:
              return 1
    |}
    ~target:"test.Foo.__init__"
    ~expected:
      {|
        [
          {
            "parent": "Foo",
            "name": "x",
            "location": { "qualifier": "test", "path": "test.py", "line": 4 },
            "annotation": "int"
          }
        ]
      |};
  (* Note: locally-inferred None annotations like this are either widenened into Optional types or
     suppressed by GlobalResult *)
  check_inference_results
    {|
      class Foo:
          foo = None
    |}
    ~target:"test.Foo.$class_toplevel"
    ~expected:
      {|
        [
          {
            "parent": "Foo",
            "name": "foo",
            "location": { "qualifier": "test", "path": "test.py", "line": 3 },
            "annotation": "None"
          }
        ]
      |};
  ()


let () =
  "typeInferenceLocalTest"
  >::: [
         "test_inferred_returns" >:: test_inferred_returns;
         "test_inferred_function_parameters" >:: test_inferred_function_parameters;
         "test_inferred_method_parameters" >:: test_inferred_method_parameters;
         "test_inferred_globals" >:: test_inferred_globals;
         "test_inferred_attributes" >:: test_inferred_attributes;
       ]
  |> Test.run
