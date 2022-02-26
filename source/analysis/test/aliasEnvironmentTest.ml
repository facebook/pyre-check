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
open Pyre
open Test

let test_simple_registration context =
  let assert_registers source name expected =
    let project = ScratchProject.setup ["test.py", source] ~include_typeshed_stubs:false ~context in
    let ast_environment = ScratchProject.build_ast_environment project in
    let alias_environment = AliasEnvironment.create ast_environment in
    let update_result =
      AliasEnvironment.update_this_and_all_preceding_environments
        alias_environment
        ~scheduler:(mock_scheduler ())
        ~configuration:(ScratchProject.configuration_of project)
        ColdStart
    in
    let read_only = AliasEnvironment.UpdateResult.read_only update_result in
    let expected = expected >>| fun expected -> Type.TypeAlias (Type.Primitive expected) in
    let printer v = v >>| Type.show_alias |> Option.value ~default:"none" in
    assert_equal ~printer expected (AliasEnvironment.ReadOnly.get_alias read_only name)
  in
  assert_registers {|
    class C:
      pass
    X = C
  |} "test.X" (Some "test.C");
  assert_registers {|
    class D:
      pass
    X = D
    Y = X
  |} "test.Y" (Some "test.D");
  assert_registers
    {|
    class E:
      pass
    X = E
    Y = X
    Z = Y
  |}
    "test.Z"
    (Some "test.E");
  assert_registers {|
    X = Z
    Y = X
    Z = Y
  |} "test.Z" None;
  assert_registers {|
    x = None
  |} "test.x" None;
  ()


let test_harder_registrations context =
  let assert_registers ~expected_alias source name =
    let project = ScratchProject.setup ["test.py", source] ~context in
    let ast_environment = ScratchProject.build_ast_environment project in
    let alias_environment = AliasEnvironment.create ast_environment in
    let update_result =
      AliasEnvironment.update_this_and_all_preceding_environments
        alias_environment
        ~scheduler:(mock_scheduler ())
        ~configuration:(ScratchProject.configuration_of project)
        ColdStart
    in
    let read_only = AliasEnvironment.UpdateResult.read_only update_result in
    let printer alias =
      alias >>| Type.sexp_of_alias >>| Sexp.to_string_hum |> Option.value ~default:"none"
    in
    assert_equal ~printer expected_alias (AliasEnvironment.ReadOnly.get_alias read_only name)
  in
  let parsed_assert_registers source name expected =
    let parser expression =
      parse_single_expression expression |> Type.create ~aliases:Type.empty_aliases
    in
    let expected_alias = expected >>| parser >>| fun alias -> Type.TypeAlias alias in
    assert_registers ~expected_alias source name
  in
  let unparsed_assert_registers source name expected =
    let expected_alias = expected >>| fun alias -> Type.TypeAlias alias in
    assert_registers ~expected_alias source name
  in
  parsed_assert_registers {|
    X = int
  |} "test.X" (Some "int");
  parsed_assert_registers
    {|
    from typing import Tuple
    X = int
    Y = Tuple[X, X]
  |}
    "test.Y"
    (Some "typing.Tuple[int, int]");
  parsed_assert_registers
    {|
    from typing import Tuple, List
    B = int
    A = List[B]
    Z = Tuple[A, B]
  |}
    "test.Z"
    (Some "typing.Tuple[typing.List[int], int]");
  unparsed_assert_registers
    {|
    from mypy_extensions import TypedDict
    X = int
    class Q(TypedDict):
      a: X
  |}
    "test.Q"
    (* TypedDicts are treated as proper classes, not aliases. *)
    None;
  parsed_assert_registers {|
    class Foo: ...
    X = Foo[unknown.get("key")]
  |} "test.X" None;
  (* Don't treat a global string assignment as an alias unless it is marked as `TypeAlias`. *)
  assert_registers {|
    X = int
    Y = "X"
  |} "test.Y" ~expected_alias:None;
  assert_registers
    {|
    import typing

    X = int
    Y: typing_extensions.TypeAlias = "X"
  |}
    "test.Y"
    ~expected_alias:(Some (Type.TypeAlias Type.integer));

  (* Recursive alias. *)
  assert_registers
    {|
      from typing import Tuple, Union

      Tree = Union[int, Tuple["Tree", "Tree"]]
    |}
    "test.Tree"
    ~expected_alias:
      (Some
         (Type.TypeAlias
            (Type.RecursiveType.create
               ~name:"test.Tree"
               ~body:
                 (Type.union
                    [
                      Type.integer;
                      Type.tuple [Type.Primitive "test.Tree"; Type.Primitive "test.Tree"];
                    ]))));
  (* Forbid directly-recursive aliases. *)
  assert_registers {|
      Tree = "Tree"
    |} "test.Tree" ~expected_alias:None;
  assert_registers
    {|
      from typing import Union

      X = Union[int, "X"]
    |}
    "test.X"
    ~expected_alias:None;
  assert_registers
    {|
      from typing import Annotated

      X = Annotated["X", int]
    |}
    "test.X"
    ~expected_alias:None;
  assert_registers
    {|
      from typing import Tuple, TypeVar, Union

      T = TypeVar("T")
      GenericTree = Union[T, Tuple["GenericTree[T]", "GenericTree[T]"]]
    |}
    "test.GenericTree"
    ~expected_alias:None;
  (* Aliases referring to recursive aliases. *)
  assert_registers
    {|
      from typing import List, Union

      X = List["X"]
      Y = List[X]
    |}
    "test.Y"
    ~expected_alias:
      (Some
         (Type.TypeAlias
            (Type.list
               (Type.RecursiveType.create
                  ~name:"test.X"
                  ~body:(Type.list (Type.Primitive "test.X"))))));
  assert_registers
    {|
      from typing import List, Sequence, Union

      X = Union[
          Sequence["X"],
          List["X"]
      ]
      Y = Union[int, X]
    |}
    "test.Y"
    ~expected_alias:
      (Some
         (Type.TypeAlias
            (Type.union
               [
                 Type.integer;
                 Type.RecursiveType.create
                   ~name:"test.X"
                   ~body:
                     (Type.union
                        [
                          Type.list (Type.Primitive "test.X");
                          Type.parametric "typing.Sequence" [Single (Type.Primitive "test.X")];
                        ]);
               ])));
  assert_registers
    {|
      from typing import List, Sequence, Union

      X = Union[
          Sequence["X"],
          List["X"]
      ]
      Y = Union[int, X]
      Z = List[Y]
    |}
    "test.Z"
    ~expected_alias:
      (Some
         (Type.TypeAlias
            (Type.list
               (Type.union
                  [
                    Type.integer;
                    Type.RecursiveType.create
                      ~name:"test.X"
                      ~body:
                        (Type.union
                           [
                             Type.list (Type.Primitive "test.X");
                             Type.parametric "typing.Sequence" [Single (Type.Primitive "test.X")];
                           ]);
                  ]))));
  assert_registers
    {|
    from typing import Generic, TypeVar
    from pyre_extensions import TypeVarTuple, Unpack
    from typing_extensions import Literal as L

    T = TypeVar("T")
    Ts = TypeVarTuple("Ts")

    class Tensor(Generic[T, Unpack[Ts]]): ...

    FloatTensor = Tensor[float, Unpack[Ts]]
  |}
    "test.FloatTensor"
    ~expected_alias:
      (Some
         (Type.TypeAlias
            (Type.parametric
               "test.Tensor"
               [
                 Single Type.float;
                 Unpacked
                   (Type.OrderedTypes.Concatenation.create_unpackable
                      (Type.Variable.Variadic.Tuple.create "test.Ts"));
               ])));
  ()


let test_updates context =
  let assert_updates
      ?(original_sources = [])
      ?(new_sources = [])
      ~middle_actions
      ~expected_triggers
      ?post_actions
      ()
    =
    Memory.reset_shared_memory ();
    let project =
      ScratchProject.setup
        ~include_typeshed_stubs:false
        ~incremental_style:FineGrained
        original_sources
        ~context
    in
    let configuration = ScratchProject.configuration_of project in
    let ast_environment = ScratchProject.build_ast_environment project in
    let alias_environment = AliasEnvironment.create ast_environment in
    let update trigger =
      let scheduler = Test.mock_scheduler () in
      AliasEnvironment.update_this_and_all_preceding_environments
        alias_environment
        ~scheduler
        ~configuration
        trigger
    in
    let update_result = update ColdStart in
    let read_only = AliasEnvironment.UpdateResult.read_only update_result in
    let execute_action (alias_name, dependency, expectation) =
      let printer v =
        v >>| Type.sexp_of_alias >>| Sexp.to_string_hum |> Option.value ~default:"none"
      in
      let expectation =
        expectation
        >>| parse_single_expression
        >>| Type.create ~aliases:Type.empty_aliases
        >>| fun alias -> Type.TypeAlias alias
      in
      AliasEnvironment.ReadOnly.get_alias read_only ~dependency alias_name
      |> assert_equal ~printer expectation
    in
    List.iter middle_actions ~f:execute_action;
    let delete_file
        { ScratchProject.configuration = { Configuration.Analysis.local_root; _ }; _ }
        relative
      =
      PyrePath.create_relative ~root:local_root ~relative |> PyrePath.absolute |> Core.Unix.remove
    in
    let add_file
        { ScratchProject.configuration = { Configuration.Analysis.local_root; _ }; _ }
        content
        ~relative
      =
      let content = trim_extra_indentation content in
      let file = File.create ~content (PyrePath.create_relative ~root:local_root ~relative) in
      File.write file
    in
    List.iter original_sources ~f:(fun (path, _) -> delete_file project path);
    List.iter new_sources ~f:(fun (relative, content) -> add_file project ~relative content);
    let update_result =
      let { ScratchProject.module_tracker; _ } = project in
      let { Configuration.Analysis.local_root; _ } = configuration in
      let paths =
        List.map new_sources ~f:(fun (relative, _) ->
            PyrePath.create_relative ~root:local_root ~relative)
      in
      ModuleTracker.update ~configuration ~paths module_tracker
      |> (fun updates -> AstEnvironment.Update updates)
      |> update
    in
    let printer set =
      SharedMemoryKeys.DependencyKey.RegisteredSet.elements set
      |> List.map ~f:SharedMemoryKeys.DependencyKey.get_key
      |> List.to_string ~f:SharedMemoryKeys.show_dependency
    in
    let expected_triggers =
      SharedMemoryKeys.DependencyKey.RegisteredSet.of_list expected_triggers
    in
    assert_equal
      ~printer
      expected_triggers
      (AliasEnvironment.UpdateResult.locally_triggered_dependencies update_result);
    post_actions >>| List.iter ~f:execute_action |> Option.value ~default:()
  in
  let dependency =
    SharedMemoryKeys.DependencyKey.Registry.register (TypeCheckDefine (Reference.create "dep"))
  in
  let assert_test_py_updates ?original_source ?new_source =
    assert_updates
      ?original_sources:(original_source >>| fun source -> ["test.py", source])
      ?new_sources:(new_source >>| fun source -> ["test.py", source])
  in
  assert_test_py_updates
    ~original_source:{|
      class C:
        pass
      X = C
    |}
    ~new_source:{|
      class C:
        pass
      X = C
    |}
    ~middle_actions:["test.X", dependency, Some "test.C"]
    ~expected_triggers:[]
    ~post_actions:["test.X", dependency, Some "test.C"]
    ();
  assert_test_py_updates
    ~original_source:{|
      class C:
        pass
      X = C
    |}
    ~new_source:{|
      X = C
    |}
    ~middle_actions:["test.X", dependency, Some "test.C"]
    ~expected_triggers:[dependency]
    ~post_actions:["test.X", dependency, None]
    ();
  assert_test_py_updates
    ~original_source:{|
      class C:
        pass
      X = C
    |}
    ~new_source:{|
      class C:
        pass
      Y = C
      X = Y
    |}
    ~middle_actions:
      ["test.X", dependency, Some "test.C"] (* Even if the route to the alias changed, no trigger *)
    ~expected_triggers:[]
    ~post_actions:["test.X", dependency, Some "test.C"]
    ();
  assert_updates
    ~original_sources:
      [
        "test.py", {|
          from placeholder import Q
          X = Q
        |};
        "placeholder.pyi", {|
          # pyre-placeholder-stub
        |};
      ]
    ~new_sources:
      [
        "test.py", {|
          from placeholder import Q
          X = Q
        |};
        "placeholder.pyi", {|
        |};
      ]
    ~middle_actions:["test.X", dependency, Some "typing.Any"]
    ~expected_triggers:[dependency]
    ~post_actions:["test.X", dependency, None]
    ();

  (* Addition should trigger previous failed reads *)
  assert_updates
    ~original_sources:["test.py", {|
        |}]
    ~new_sources:["test.py", {|
          class C:
           pass
          X = C
        |}]
    ~middle_actions:["test.X", dependency, None]
    ~expected_triggers:[dependency]
    ~post_actions:["test.X", dependency, Some "test.C"]
    ();
  ()


let () =
  "environment"
  >::: [
         "simple_registration" >:: test_simple_registration;
         "compounds" >:: test_harder_registrations;
         "updates" >:: test_updates;
       ]
  |> Test.run
