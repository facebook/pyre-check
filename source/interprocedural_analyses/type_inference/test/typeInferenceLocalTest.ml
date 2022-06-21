(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
module State = TypeInference.Local.State

let configuration = Configuration.Analysis.create ~source_paths:[] ()

let assert_backward ~resolution precondition statement postcondition =
  let module State = State (struct
    let qualifier = Reference.empty

    let configuration = configuration

    let define = +mock_define

    let resolution_fixpoint = Some (LocalAnnotationMap.empty ())

    let error_map = Some (TypeCheck.LocalErrorMap.empty ())
  end)
  in
  let create annotations =
    let resolution =
      let annotation_store =
        let annotify (name, annotation) =
          let annotation =
            let create annotation = Refinement.Unit.create (Annotation.create_mutable annotation) in
            create annotation
          in
          !&name, annotation
        in
        {
          Refinement.Store.annotations =
            List.map annotations ~f:annotify |> Reference.Map.of_alist_exn;
          temporary_annotations = Reference.Map.empty;
        }
      in
      Resolution.with_annotation_store resolution ~annotation_store
    in
    State.create ~resolution ()
  in
  let assert_state_equal =
    assert_equal
      ~cmp:State.equal
      ~printer:(Format.asprintf "%a" State.pp)
      ~pp_diff:(diff ~print:State.pp)
  in
  let parsed =
    parse statement
    |> function
    | { Source.statements; _ } -> statements
  in
  assert_state_equal
    (create postcondition)
    (List.fold_right
       ~f:(fun statement state ->
         State.backward ~statement_key:Cfg.exit_index state ~statement
         |> State.widen_resolution_with_snapshots)
       ~init:(create precondition)
       parsed)


let test_backward_resolution_handling context =
  let resolution = ScratchProject.setup ~context [] |> ScratchProject.build_resolution in
  let assert_backward = assert_backward ~resolution in
  assert_backward ["y", Type.integer] "pass" ["y", Type.integer];

  (* Assignments. *)
  assert_backward ["x", Type.integer] "x = y" ["x", Type.integer; "y", Type.integer];
  assert_backward ["y", Type.integer] "x = z" ["y", Type.integer];
  assert_backward ["x", Type.integer] "x += 1" ["x", Type.integer];
  assert_backward ["x", Type.integer] "x = y = z" ["x", Type.integer; "z", Type.integer];
  assert_backward
    ["x", Type.Primitive "B"; "y", Type.Primitive "C"]
    "x = y = z"
    ["x", Type.Primitive "B"; "y", Type.Primitive "C"; "z", Type.Bottom];
  assert_backward ["a", Type.integer] "a, b = c, d" ["a", Type.integer; "c", Type.integer];
  assert_backward ["a", Type.Top; "b", Type.integer] "a = b" ["b", Type.integer];

  (* Tuples *)
  assert_backward
    ["x", Type.integer; "y", Type.string]
    "x, y = z"
    ["x", Type.integer; "y", Type.string; "z", Type.tuple [Type.integer; Type.string]];
  assert_backward
    ["x", Type.tuple [Type.integer; Type.string]]
    "x = y, z"
    ["x", Type.tuple [Type.integer; Type.string]; "y", Type.integer; "z", Type.string];

  (* Literals. *)
  assert_backward [] "x = 1.0" [];
  assert_backward [] "x = 'string'" [];
  assert_backward ["x", Type.Primitive "Foo"] "x = 'string'" ["x", Type.Primitive "Foo"];
  assert_backward ["x", Type.Primitive "Foo"] "x = 'string'" ["x", Type.Primitive "Foo"];

  (* Calls *)
  assert_backward [] "int_to_str(x)" ["x", Type.integer];
  assert_backward [] "str_float_to_int(x, y)" ["x", Type.string; "y", Type.float];
  assert_backward [] "str_float_tuple_to_int(t)" ["t", Type.tuple [Type.string; Type.float]];
  assert_backward ["x", Type.string] "unknown_to_int(x)" ["x", Type.string];
  assert_backward ["x", Type.float] "x = int_to_str(x)" ["x", Type.integer];
  assert_backward ["y", Type.float] "y = int_to_str(x)" ["y", Type.float; "x", Type.integer];
  assert_backward ["y", Type.integer] "y = int_to_str(x)" ["y", Type.integer; "x", Type.integer];
  assert_backward [] "str_float_to_int(x)" ["x", Type.string];
  assert_backward [] "str_float_to_int(x, 1.0)" ["x", Type.string];
  assert_backward [] "'a'.substr(x)" ["x", Type.integer];
  assert_backward
    ["y", Type.float]
    "y = obj.static_int_to_str(x)"
    ["y", Type.float; "x", Type.integer];
  assert_backward [] "str_float_tuple_to_int((x, y))" ["x", Type.string; "y", Type.float];
  assert_backward
    []
    "nested_tuple_to_int(((x, y), z))"
    ["x", Type.string; "y", Type.float; "z", Type.float];
  assert_backward
    [
      ( "cb",
        Type.Callable.create
          ~parameters:(Defined [Named { name = "arg"; annotation = Type.integer; default = false }])
          ~annotation:Type.none
          () );
    ]
    "cb(x)"
    [
      ( "cb",
        Type.Callable.create
          ~parameters:(Defined [Named { name = "arg"; annotation = Type.integer; default = false }])
          ~annotation:Type.none
          () );
      "x", Type.integer;
    ];
  assert_backward
    [
      ( "cb",
        Type.parametric
          "BoundMethod"
          [
            Single
              (Type.Callable.create
                 ~parameters:
                   (Defined
                      [
                        Named { name = "self"; annotation = Type.string; default = false };
                        Named { name = "arg"; annotation = Type.integer; default = false };
                      ])
                 ~annotation:Type.none
                 ());
            Single Type.string;
          ] );
    ]
    "cb(x)"
    [
      ( "cb",
        Type.parametric
          "BoundMethod"
          [
            Single
              (Type.Callable.create
                 ~parameters:
                   (Defined
                      [
                        Named { name = "self"; annotation = Type.string; default = false };
                        Named { name = "arg"; annotation = Type.integer; default = false };
                      ])
                 ~annotation:Type.none
                 ());
            Single Type.string;
          ] );
      "x", Type.integer;
    ];

  (* TODO(T84365830): Extend implementation to pass starred and unstarred tests *)
  assert_backward [] "str_float_to_int(*(x, y))" [];

  (* "x", Type.string; "y", Type.float *)
  assert_backward [] "str_float_to_int(**{'s': x, 'f': y})" [];

  (* "x", Type.string; "y", Type.float *)
  assert_backward [] "star_int_to_int(*[], y)" []


module Setup = struct
  let set_up_project ~context code =
    let project = ScratchProject.setup ~context ["test.py", code] in
    let environment = ScratchProject.type_environment project in
    environment, ScratchProject.configuration_of project


  let get_environment_data ~context code =
    let environment, configuration = set_up_project ~context code in
    let global_resolution = environment |> TypeEnvironment.ReadOnly.global_resolution in
    let ast_environment = GlobalResolution.ast_environment global_resolution in
    let filename_lookup = Analysis.AstEnvironment.ReadOnly.get_relative ast_environment in
    let source =
      AstEnvironment.ReadOnly.get_processed_source ast_environment (Reference.create "test")
      |> fun option -> Option.value_exn option
    in
    configuration, global_resolution, filename_lookup, source


  let run_inference ?(skip_annotated = false) ~context ~target code =
    let configuration, global_resolution, filename_lookup, source =
      get_environment_data ~context code
    in
    let module_results =
      TypeInference.Local.infer_for_module
        ~skip_annotated
        ~configuration
        ~global_resolution
        ~filename_lookup
        source
    in
    let is_target local_result =
      let name = LocalResult.define_name local_result in
      Reference.equal name (Reference.create target)
    in
    module_results |> List.filter ~f:is_target |> List.hd


  let run_inference_exn ~context ~target code =
    match run_inference ~context ~target code with
    | Some result -> result
    | None -> failwith ("Could not find target define " ^ target)
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


let option_to_json string_option =
  string_option |> Option.map ~f:(Format.asprintf "\"%s\"") |> Option.value ~default:"null"


let check_inference_results ?(field_path = []) ~context ~target ~expected code =
  code
  |> Setup.run_inference_exn ~context ~target
  |> LocalResult.to_yojson
  |> access_by_path field_path
  |> assert_json_equal ~context ~expected


let test_should_analyze_define context =
  let check ~should_analyze ~target code =
    let _, global_resolution, _, source = Setup.get_environment_data ~context code in
    let would_analyze =
      source
      |> TypeInference.Local.Testing.define_names_to_analyze ~global_resolution
      |> List.exists ~f:(Reference.equal target)
    in
    Bool.equal would_analyze should_analyze
    |> assert_bool "Unexpected result from should_analyze_define"
  in
  check
    {|
      def foo() -> int:
          pass
    |}
    ~target:(Reference.create "test.foo")
    ~should_analyze:false;
  check
    {|
      def foo(x: str) -> int:
          pass
    |}
    ~target:(Reference.create "test.foo")
    ~should_analyze:false;
  check
    {|
      def foo() -> int:
          pass
    |}
    ~target:(Reference.create "test.$toplevel")
    ~should_analyze:true;
  check
    {|
      def foo(x: Any) -> int:
          pass
    |}
    ~target:(Reference.create "test.foo")
    ~should_analyze:true;
  ()


let test_inferred_returns context =
  let check_inference_results = check_inference_results ~field_path:["define"; "return"] ~context in
  (* None *)
  check_inference_results
    {|
      def foo():
          pass
    |}
    ~target:"test.foo"
    ~expected:{|"None"|};
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
      def foo():
          x = undefined
    |}
    ~target:"test.foo"
    ~expected:{|"None"|};
  check_inference_results
    {|
      def foo(b: bool):
          if b:
            return 1
    |}
    ~target:"test.foo"
    ~expected:{|"typing.Optional[int]"|};

  (* Locals *)
  check_inference_results
    {|
      def foo(x: int):
          return x
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
    def foo():
        if 1 > 2:
            x = 2
        else:
            assert not True
        return x
    |}
    ~target:"test.foo"
    ~expected:{|"int"|};

  (* Tuples *)
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

  (* Callables *)
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

  (* Self *)
  check_inference_results
    {|
      class Test(object):
          def ret_self(self):
              return self
    |}
    ~target:"test.Test.ret_self"
    ~expected:{|"test.Test"|};

  (* Unknown *)
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
      def foo(x):
          return x, "hello"
    |}
    ~target:"test.foo"
    ~expected:{|null|};
  check_inference_results
    {|
      class A:
          @abstractmethod
          def foo():
              pass
    |}
    ~target:"test.A.foo"
    ~expected:{|null|};

  (* Weakened literals and containers *)
  check_inference_results
    {|
      def foo():
          return [1]
    |}
    ~target:"test.foo"
    ~expected:{|"typing.List[int]"|};
  check_inference_results
    {|
      def foo():
          return 1
    |}
    ~target:"test.foo"
    ~expected:{|"int"|};
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
          return {1 + 1: x}
    |}
    ~target:"test.foo"
    ~expected:{|null|};
  (* This is allowed because of special-casing in Type.contains_prohibited_any *)
  check_inference_results
    {|
      from typing import Any
      def foo(x: Any):
          return {"": x}
    |}
    ~target:"test.foo"
    ~expected:{|"typing.Dict[str, typing.Any]"|};
  ()


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
  let no_inferences =
    {|
      [{ "name": "x", "annotation": null, "value": null, "index": 0 }]
    |}
  in
  let no_inferences_with_default ~default =
    Format.asprintf
      {|
      [{ "name": "x", "annotation": null, "value": "%s", "index": 0 }]
    |}
      default
  in
  (* From Return Annotation *)
  check_inference_results
    {|
      def foo(x) -> int:
          return x
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
  (* TODO(T84365830): Ensure we correctly qualify inferred parameter types. *)
  check_inference_results
    {|
      from typing import Optional
      def foo(x) -> Optional[str]:
          return x
    |}
    ~target:"test.foo"
    ~expected:(single_parameter "typing.Optional[str]");

  (* Explicit Any and Default Value *)
  check_inference_results
    {|
      def foo(x: typing.Any) -> None:
          x = 5
    |}
    ~target:"test.foo"
    ~expected:(single_parameter "int");
  check_inference_results
    {|
      def foo(x = 5) -> int:
          return x
    |}
    ~target:"test.foo"
    ~expected:(single_parameter ~default:"5" "int");
  check_inference_results
    {|
      def foo(x = 5) -> None:
          return
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
  check_inference_results
    {|
      def foo(x = None) -> None:
          x = 1
          return
    |}
    ~target:"test.foo"
    ~expected:(single_parameter ~default:"None" "typing.Optional[int]");

  (* Assignments *)
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
          y = 5
          x = y
          return x
    |}
    ~target:"test.foo"
    ~expected:(single_parameter "int");
  check_inference_results
    {|
      def foo(x) -> int:
          x = unknown()
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
      def foo(x) -> int:
          x = y
          y = z
          return z
    |}
    ~target:"test.foo"
    ~expected:no_inferences;

  (* Self-referential assignments *)
  check_inference_results
    {|
      def foo(x) -> int:
          y += x
          return y
    |}
    ~target:"test.foo"
    ~expected:(single_parameter ~name:"x" "int");
  check_inference_results
    {|
      def foo(x) -> int:
          x += 1
          return x
    |}
    ~target:"test.foo"
    ~expected:(single_parameter ~name:"x" "int");
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
          { "name": "x", "annotation": null, "value": null, "index": 0 },
          { "name": "y", "annotation": "int", "value": null, "index": 1 }
        ]
      |};

  (* Tuple assignments *)
  check_inference_results
    {|
      def foo(x) -> typing.Tuple[int, float]:
          y = x
          z = x
          return (y, z)
    |}
    ~target:"test.foo"
    ~expected:(single_parameter ~name:"x" "int");
  check_inference_results
    {|
      def foo(x) -> typing.Tuple[int, str]:
          y = x
          z = x
          return (y, z)
    |}
    ~target:"test.foo"
    ~expected:no_inferences;
  check_inference_results
    {|
      def foo(x) -> typing.Tuple[int, float]:
          z = y
          x = y
          return (x, z)
    |}
    ~target:"test.foo"
    ~expected:(single_parameter "int");
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

  (* Infix usage *)
  (* TODO(T84365830): Addition with integer should imply integer. *)
  check_inference_results
    {|
      def foo(x = None) -> None:
          z = x + 1
    |}
    ~target:"test.foo"
    ~expected:(no_inferences_with_default ~default:"None");
  check_inference_results
    {|
      def foo(x) -> None:
          x += 1
    |}
    ~target:"test.foo"
    ~expected:(single_parameter "int");

  (* Call usage *)
  check_inference_results
    {|
      from typing import Optional
      def foo(x: Optional[str]):
          return x

      def bar(x = None) -> None:
          foo(x)
    |}
    ~target:"test.bar"
    ~expected:(single_parameter ~default:"None" "typing.Optional[str]");
  check_inference_results
    {|
      def takes_int(input: int) -> None: ...
      def takes_str(input: str) -> None: ...
      def foo(x) -> None:
          x = 1
          takes_int(x)
          x = "string"
          takes_str(x)
    |}
    ~target:"test.foo"
    ~expected:(single_parameter "typing.Union[int, str]");
  check_inference_results
    {|
      def takes_int(input: int) -> None: ...
      def takes_float(input: float) -> None: ...
      def foo(x) -> None:
          takes_int(x)
          takes_float(x)
    |}
    ~target:"test.foo"
    ~expected:(single_parameter "int");
  check_inference_results
    {|
      def takes_int(input: int) -> None: ...
      def takes_unknown(input) -> None: ...
      def foo(x) -> None:
          takes_int(x)
          takes_unknown(x)
    |}
    ~target:"test.foo"
    ~expected:(single_parameter "int");
  check_inference_results
    {|
      def foo(a: int, b: str) -> None:
          pass

      def bar(a, b) -> None:
        foo(b=b, a=a)
    |}
    ~target:"test.bar"
    ~expected:
      {|
        [
          { "name": "a", "annotation": "int", "value": null, "index": 0 },
          { "name": "b", "annotation": "str", "value": null, "index": 1 }
        ]
     |};
  (* Intentionally avoid propagating catch-all annotations *)
  check_inference_results
    {|
      def format( *args: object, **kwargs: object) -> str: ...

      def foo(a, b) -> None:
        format(a=a, b=b)
    |}
    ~target:"test.foo"
    ~expected:
      {|
        [
          { "name": "a", "annotation": null, "value": null, "index": 0 },
          { "name": "b", "annotation": null, "value": null, "index": 1 }
        ]
     |};

  (* Calls with generics *)
  (* TODO(T84365830): Knowing the return type should inform the parameter type. *)
  check_inference_results
    {|
      from typing import Any, Dict, Optional, TypeVar

      # copied from typeshed
      _T = TypeVar("_T")
      def deepcopy(x: _T, memo: Optional[Dict[int, Any]] = ..., _nil: Any = ...) -> _T: ...

      def foo(x):
          return deepcopy(x)
    |}
    ~target:"test.foo"
    ~expected:
      {|
        [
          { "name": "x", "annotation": null, "value": null, "index": 0 }
        ]
     |};

  (* Type refinement calls *)
  (* Inference logic is aggressive in picking up types that are verifiably possible, and not
     throwing all of it away if an unknown alternative exists. *)
  check_inference_results
    {|
      def foo(x) -> None:
          if type(x) is int:
            return
    |}
    ~target:"test.foo"
    ~expected:(single_parameter "int");
  check_inference_results
    {|
      def foo(x) -> None:
          if type(x) is int:
            return
          elif type(x) is str:
            return
    |}
    ~target:"test.foo"
    ~expected:(single_parameter "typing.Union[int, str]");

  (* Conditionals *)
  check_inference_results
    {|
      def foo(x = None) -> None:
          if x:
              x = ""
    |}
    ~target:"test.foo"
    ~expected:(single_parameter "typing.Optional[str]" ~default:"None");
  check_inference_results
    {|
      def takes_int(input: int) -> None: ...
      def takes_str(input: str) -> None: ...
      def ret_bool() -> bool: ...
      def foo(x) -> None:
          if ret_bool():
              takes_int(x)
          else:
              takes_str(x)
    |}
    ~target:"test.foo"
    ~expected:(single_parameter "typing.Union[int, str]");

  (* Containers *)
  check_inference_results
    {|
      def foo(x) -> None:
          x = []
          x.append(1)
    |}
    ~target:"test.foo"
    ~expected:(single_parameter "typing.List[int]");
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
  let no_inferences = single_parameter_method None in
  (* Overrides *)
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
    ~expected:(single_parameter_method (Some "test.A"));
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
  (* Don't override existing explicit annotations *)
  check_inference_results
    {|
      class A:
          def foo(self, x: int, y: str, z: int) -> int: ...
      class B(A):
          def foo(self, x, y: str, z: int):
              return x
    |}
    ~target:"test.B.foo"
    ~expected:
      {|
        [
          { "name": "self", "annotation": null, "value": null, "index": 0 },
          { "name": "x", "annotation": "int", "value": null, "index": 1 },
          { "name": "y", "annotation": null, "value": null, "index": 2 },
          { "name": "z", "annotation": null, "value": null, "index": 3 }
        ]
      |};
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

  (* Starred arguments *)
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

  (* Generic overrides *)
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

  (* Multiple override *)
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
  ()


let test_inferred_globals context =
  let check_inference_results = check_inference_results ~context ~field_path:["globals"] in
  (* Function call *)
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

  (* Local usage *)
  (* TODO(T84365830): Implement support for global inference due to local usage. *)
  check_inference_results
    {|
      x = unknown
      def foo() -> int:
          return x
    |}
    ~target:"test.$toplevel"
    ~expected:{|[]|};
  check_inference_results
    {|
      x = unknown
      def foo() -> None:
          global x
          x = 1
    |}
    ~target:"test.$toplevel"
    ~expected:{|[]|};

  (* Containers *)
  check_inference_results
    {|
      foo = []
    |}
    ~target:"test.$toplevel"
    ~expected:{|
        []
      |};

  (* None *)
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
  (* Toplevel *)
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
            "parent": "test.Foo",
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
            "parent": "test.Foo",
            "name": "x",
            "location": { "qualifier": "test", "path": "test.py", "line": 3 },
            "annotation": "int"
          }
        ]
      |};

  (* Constructor *)
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
            "parent": "test.Foo",
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
            "parent": "test.Foo",
            "name": "x",
            "location": { "qualifier": "test", "path": "test.py", "line": 4 },
            "annotation": "int"
          }
        ]
      |};

  (* Local usage *)
  (* TODO(T84365830): Implement support for global inference due to local usage. *)
  check_inference_results
    {|
      class Foo:
        x = unknown()

      def test(f: Foo) -> None:
        f.x = 1
    |}
    ~target:"test.Foo.$class_toplevel"
    ~expected:{|[]|};

  (* None *)
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
            "parent": "test.Foo",
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
         "test_backward_resolution_handling" >:: test_backward_resolution_handling;
         "test_should_analyze_define" >:: test_should_analyze_define;
         "test_inferred_returns" >:: test_inferred_returns;
         "test_inferred_function_parameters" >:: test_inferred_function_parameters;
         "test_inferred_method_parameters" >:: test_inferred_method_parameters;
         "test_inferred_globals" >:: test_inferred_globals;
         "test_inferred_attributes" >:: test_inferred_attributes;
       ]
  |> Test.run
