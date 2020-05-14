open Core
open Test
open Ast
open Analysis
open OUnit2

let assert_incremental_check_errors ~context ~initial_sources ~updated_sources ~expected =
  let trim (is_external, relative, content) =
    is_external, (relative, Test.trim_extra_indentation content)
  in
  let initial_sources = List.map initial_sources ~f:trim in
  let updated_sources = List.map updated_sources ~f:trim in
  (* Setup a server. *)
  let ( ({ ScratchProject.module_tracker; _ } as project),
        ({ Server.State.ast_environment; _ } as state) )
    =
    ServerTest.initialize_server ~incremental_style:FineGrained ~context ~initial_sources
  in
  let update_module_tracker (is_external, source) =
    ScratchProject.add_source project ~is_external source
  in
  List.iter updated_sources ~f:update_module_tracker;

  (* This is kind of convoluted - I wanted to go through the module tracker API to avoid having to
     leak information from ScratchProject. *)
  let configuration = ScratchProject.configuration_of project in
  let paths =
    List.map updated_sources ~f:(fun (_, (relative, _)) ->
        SourcePath.qualifier_of_relative relative)
    |> List.filter_map ~f:(ModuleTracker.lookup_source_path module_tracker)
    |> List.map ~f:(SourcePath.full_path ~configuration)
  in
  let errors =
    let description error =
      AnalysisError.instantiate
        error
        ~lookup:
          (AstEnvironment.ReadOnly.get_real_path_relative
             ~configuration
             (AstEnvironment.read_only ast_environment))
      |> AnalysisError.Instantiated.description ~show_error_traces:false ~concise:false
    in
    Server.IncrementalCheck.recheck_with_state ~state ~configuration paths
    |> fst
    |> (fun { Server.State.errors; _ } -> errors)
    |> Reference.Table.data
    |> List.concat
    |> List.map ~f:description
  in
  assert_equal ~printer:(String.concat ~sep:"\n") expected errors;
  Memory.reset_shared_memory ()


let test_incremental_check context =
  (* We warn on extraneous ignores. *)
  assert_incremental_check_errors
    ~context
    ~initial_sources:
      [
        ( false,
          "a.py",
          {|
        # pyre-ignore
        def unused_pyre_ignore() -> int:
          return 0
      |}
        );
      ]
    ~updated_sources:
      [
        ( false,
          "a.py",
          {|
        # pyre-ignore
        def still_unused_pyre_ignore() -> int:
          return 0
      |}
        );
      ]
    ~expected:
      [
        "Unused ignore [0]: The `pyre-ignore` or `pyre-fixme` comment is not suppressing type \
         errors, please remove it.";
      ];

  (* If an external source is updated, it is filtered properly. *)
  assert_incremental_check_errors
    ~context
    ~initial_sources:
      [
        ( true,
          "external.py",
          {|
        # pyre-ignore
        def unused_pyre_ignore() -> int:
          return 0
      |}
        );
      ]
    ~updated_sources:
      [
        ( true,
          "external.py",
          {|
        # pyre-ignore
        def still_unused_pyre_ignore() -> int:
          return 0
      |}
        );
      ]
    ~expected:[];

  assert_incremental_check_errors
    ~context
    ~initial_sources:
      [
        false, "a.py", {|
         class A:
           y: int = 7
      |};
        ( false,
          "b.py",
          {|
          from a import A
          x = A()
          reveal_type(x.y)
      |} );
      ]
    ~updated_sources:[false, "a.py", {|
         class A:
           y: str = "A"
      |}]
    ~expected:["Revealed type [-1]: Revealed type for `x.y` is `str`."];
  assert_incremental_check_errors
    ~context
    ~initial_sources:
      [
        false, "a.py", {|
         class A(int):
          pass
      |};
        ( false,
          "b.py",
          {|
          from a import A
          def foo(x: int) -> None:
            pass
          foo(A())
      |}
        );
      ]
    ~updated_sources:[false, "a.py", {|
         class A(str):
          pass
      |}]
    ~expected:
      [
        "Incompatible parameter type [6]: Expected `int` for 1st positional only parameter to call \
         `foo` but got `A`.";
      ];

  assert_incremental_check_errors
    ~context
    ~initial_sources:
      [
        ( false,
          "a.py",
          {|
        from typing import Callable
        def dec(x: Callable[[int], int]) -> Callable[[int], str]: ...
      |}
        );
        ( false,
          "b.py",
          {|
          from a import dec
          @dec
          def foo(x: int) -> int:
            pass
          reveal_type(foo)
      |}
        );
      ]
    ~updated_sources:
      [
        ( false,
          "a.py",
          {|
        from typing import Callable
        def dec(x: Callable[[int], int]) -> Callable[[int], bool]: ...
      |}
        );
        ( false,
          "b.py",
          {|
          from a import dec
          @dec
          def foo(x: int) -> int:
            pass
          reveal_type(foo)
          irr = 8
      |}
        );
      ]
    ~expected:["Revealed type [-1]: Revealed type for `b.foo` is `typing.Callable[[int], bool]`."];

  assert_incremental_check_errors
    ~context
    ~initial_sources:
      [
        ( false,
          "a.py",
          {|
           from q import Doggo

           def bar() -> Doggo: ...

           def foo(d: Doggo) -> None:
               reveal_type(bar())
               reveal_type(d)
          |}
        );
        false, "q.py", "Doggo = int";
      ]
    ~updated_sources:[false, "q.py", "Doggo = str"]
    ~expected:
      [
        "Revealed type [-1]: Revealed type for `d` is `str`.";
        "Revealed type [-1]: Revealed type for `a.bar()` is `str`.";
      ];

  assert_incremental_check_errors
    ~context
    ~initial_sources:
      [
        ( false,
          "a.py",
          {|
            # pyre-strict
            from typing import Any, List
            from dataclasses import dataclass

            @dataclass(frozen=True)
            class C:
                # pyre-ignore
                x: List[Any]
          |}
        );
      ]
    ~updated_sources:
      [
        ( false,
          "a.py",
          {|
             # pyre-strict
             from typing import Any, List
             from dataclasses import dataclass

             def foo() -> None:
                 pass

             @dataclass(frozen=True)
             class C:
                 # pyre-ignore
                 x: List[Any]
          |}
        );
      ]
    ~expected:[];
  assert_incremental_check_errors
    ~context
    ~initial_sources:
      [
        ( false,
          "a.py",
          {|
            class Base:
              pass
            class Initial(Base):
              pass
          |}
        );
        ( false,
          "b.py",
          {|
            from a import Initial, Base
            from typing import Generic, TypeVar

            T = TypeVar("T", bound=Base)
            class G(Generic[T]):
              attribute: T
              def __init__(self, t: T) -> None:
                self.attribute = t

            def foo(g: G[Initial]) -> None:
              x = g.attribute
          |}
        );
      ]
    ~updated_sources:
      [
        ( false,
          "a.py",
          {|
            class Base:
              pass
            class Renamed(Base):
              pass
          |}
        );
        ( false,
          "b.py",
          {|
            from a import Renamed, Base
            from typing import Generic, TypeVar

            T = TypeVar("T", bound=Base)
            class G(Generic[T]):
              attribute: T = ...
              def __init__(self, t: T) -> None:
                self.attribute = t

            def foo(g: G[Renamed]) -> None:
              x = g.attribute
          |}
        );
      ]
    ~expected:[];

  ()


let () = "incremental_check" >::: ["incremental_check" >:: test_incremental_check] |> Test.run
