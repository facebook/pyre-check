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
open Statement
open Test
open ReadOnlyCheck
open ReadOnlyness

let test_forward_expression context =
  let global_resolution, type_environment =
    let source =
      {|
      from pyre_extensions import ReadOnly

      class Foo:
        readonly_attribute: ReadOnly[int]
    |}
    in
    let project = ScratchProject.setup ~context ["test.py", source] in
    ScratchProject.build_global_resolution project, ScratchProject.type_environment project
  in
  let dummy_statement_key = 0 in
  let type_resolution =
    TypeCheck.resolution_with_key
      ~global_resolution
      ~local_annotations:None
      ~parent:None
      ~statement_key:dummy_statement_key
      (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
      (module TypeCheck.DummyContext)
  in
  let module Context = struct
    let qualifier = !&"test"

    let define =
      parse_single_define {|
      def foo() -> None: ...
    |}
      |> Node.create_with_default_location


    let error_map = Some (LocalErrorMap.empty ())

    let global_resolution = global_resolution

    let local_annotations =
      TypeEnvironment.ReadOnly.get_or_recompute_local_annotations
        type_environment
        (Node.value define |> Define.name)


    let type_resolution_for_statement =
      TypeCheck.resolution_with_key
        (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
        (module TypeCheck.DummyContext)
  end
  in
  let module State = State (Context) in
  let assert_resolved
      ?(type_resolution = type_resolution)
      ?(resolution = Resolution.of_list [])
      expression
      expected_type
    =
    let { Resolved.resolved; _ } =
      parse_single_expression expression |> State.forward_expression ~type_resolution ~resolution
    in
    assert_equal ~cmp:[%compare.equal: t] ~printer:show expected_type resolved
  in
  assert_resolved "..." Mutable;
  assert_resolved "False" Mutable;
  assert_resolved "True" Mutable;
  assert_resolved "1.2" Mutable;
  assert_resolved "42" Mutable;
  assert_resolved "'hello'" Mutable;
  assert_resolved "b'hello'" Mutable;
  assert_resolved "None" Mutable;
  assert_resolved ~resolution:(Resolution.of_list [!&"x", ReadOnly]) "x" ReadOnly;
  assert_resolved ~resolution:(Resolution.of_list [!&"x", Mutable]) "x" Mutable;
  assert_resolved ~resolution:(Resolution.of_list []) "x" Mutable;
  assert_resolved ~resolution:(Resolution.of_list [!&"x", Mutable]) "x.y" Mutable;
  assert_resolved ~resolution:(Resolution.of_list [!&"x", ReadOnly]) "x.y" ReadOnly;
  assert_resolved ~resolution:(Resolution.of_list [!&"x", ReadOnly]) "x.y.z" ReadOnly;
  assert_resolved
    ~resolution:(Resolution.of_list [!&"x", Mutable])
    ~type_resolution:
      (TypeResolution.new_local
         type_resolution
         ~reference:!&"x"
         ~annotation:(Annotation.create_mutable (Type.Primitive "test.Foo")))
    "x.readonly_attribute"
    ReadOnly;
  ()


let test_check_arguments_against_parameters context =
  let open AttributeResolution in
  let open Type.Callable in
  let global_resolution, type_environment =
    let project = ScratchProject.setup ~context [] in
    ScratchProject.build_global_resolution project, ScratchProject.type_environment project
  in
  let module Context = struct
    let qualifier = !&"test"

    let define =
      parse_single_define {|
      def foo() -> None: ...
    |}
      |> Node.create_with_default_location


    let error_map = Some (LocalErrorMap.empty ())

    let global_resolution = global_resolution

    let local_annotations =
      TypeEnvironment.ReadOnly.get_or_recompute_local_annotations
        type_environment
        (Node.value define |> Define.name)


    let type_resolution_for_statement =
      TypeCheck.resolution_with_key
        (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
        (module TypeCheck.DummyContext)
  end
  in
  let module State = State (Context) in
  let assert_arguments_against_parameters ~parameter_argument_mapping expected_error_kinds =
    let actual_error_kinds =
      State.check_arguments_against_parameters ~function_name:None parameter_argument_mapping
      |> List.map ~f:(function
             | { Error.kind = Error.ReadOnlynessMismatch mismatch; _ } -> mismatch
             | _ -> failwith "Expected ReadOnlyness mismatch")
    in
    assert_equal
      ~printer:[%show: Error.ReadOnly.readonlyness_mismatch list]
      ~cmp:[%compare.equal: Error.ReadOnly.readonlyness_mismatch list]
      expected_error_kinds
      actual_error_kinds
  in
  assert_arguments_against_parameters
    ~parameter_argument_mapping:
      (Parameter.Map.of_alist_exn
         [
           ( Named { name = "x"; annotation = Type.string; default = false },
             [
               make_matched_argument
                 {
                   Argument.WithPosition.resolved = ReadOnlyness.ReadOnly;
                   kind = Positional;
                   expression = None;
                   position = 1;
                 };
             ] );
           Named { name = "y"; annotation = Type.integer; default = false }, [Default];
         ])
    [
      IncompatibleParameterType
        {
          name = None;
          position = 1;
          callee = None;
          mismatch = { actual = ReadOnlyness.ReadOnly; expected = ReadOnlyness.Mutable };
        };
    ];
  assert_arguments_against_parameters
    ~parameter_argument_mapping:
      (Parameter.Map.of_alist_exn
         [
           ( Named { name = "y"; annotation = Type.integer; default = false },
             [
               make_matched_argument
                 {
                   Argument.WithPosition.resolved = ReadOnlyness.ReadOnly;
                   kind = Named (Node.create_with_default_location "y");
                   expression = None;
                   position = 1;
                 };
             ] );
         ])
    [
      IncompatibleParameterType
        {
          name = Some "y";
          position = 1;
          callee = None;
          mismatch = { actual = ReadOnlyness.ReadOnly; expected = ReadOnlyness.Mutable };
        };
    ];
  ()


let test_callable_data_list_for_callee context =
  let source =
    {|
    from pyre_extensions import ReadOnly

    def foo(x: int) -> None: ...
    def bar(y: str) -> bool: ...

    my_union = foo if 1 + 1 == 2 else bar

    class Foo:
      def return_readonly(self, x: int) -> ReadOnly[int]: ...
  |}
  in
  let resolution =
    ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_resolution
  in
  let assert_callable_data_list ?self_readonlyness ~callee expected_callable_data_list =
    parse_single_expression callee
    |> TypeResolution.resolve_expression_to_type resolution
    |> callable_data_list_for_callee ?self_readonlyness
    |> assert_equal
         ~printer:[%show: callable_data_for_function_call list]
         ~cmp:[%compare.equal: callable_data_for_function_call list]
         expected_callable_data_list
  in
  assert_callable_data_list
    ~callee:"test.foo"
    [
      {
        selected_signature =
          {
            annotation = Type.none;
            parameters =
              Defined [Named { name = "$parameter$x"; annotation = Type.integer; default = false }];
          };
        instantiated_return_type = Type.none;
        function_name = Some !&"test.foo";
        self_readonlyness = None;
      };
    ];
  (* TODO(T130377746): Support union types. *)
  assert_callable_data_list ~callee:"test.my_union" [];
  assert_callable_data_list
    ~callee:"test.Foo().return_readonly"
    ~self_readonlyness:Mutable
    [
      {
        selected_signature =
          {
            annotation = Type.ReadOnly.create Type.integer;
            parameters =
              Defined
                [
                  Named
                    {
                      name = "$parameter$self";
                      annotation = Type.Primitive "test.Foo";
                      default = false;
                    };
                  Named { name = "$parameter$x"; annotation = Type.integer; default = false };
                ];
          };
        instantiated_return_type = Type.ReadOnly.create Type.integer;
        function_name = Some !&"test.Foo.return_readonly";
        self_readonlyness = Some Mutable;
      };
    ];
  ()


let assert_readonly_errors ~context =
  let check ~environment ~source =
    let errors_for_define define =
      let environment = TypeEnvironment.read_only environment in
      ReadOnlyCheck.readonly_errors_for_define
        ~type_resolution_for_statement:
          (TypeCheck.resolution_with_key
             (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
             (module TypeCheck.DummyContext))
        ~global_resolution:(TypeEnvironment.ReadOnly.global_resolution environment)
        ~local_annotations:
          (TypeEnvironment.ReadOnly.get_or_recompute_local_annotations
             environment
             (Node.value define |> Define.name))
        ~qualifier:!&"test"
        define
    in
    source |> Preprocessing.defines ~include_toplevels:true |> List.concat_map ~f:errors_for_define
  in
  assert_errors ~context ~check


let test_assignment context =
  let assert_readonly_errors = assert_readonly_errors ~context in
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      def main() -> None:
        x: ReadOnly[int] = 42
        y = x
        z: int = y
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: z is declared to have readonlyness \
       `ReadOnlyness.Mutable` but is used as readonlyness `ReadOnlyness.ReadOnly`.";
    ];
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      def main() -> None:
        x = 42
        y = x
        z: ReadOnly[int] = y
    |}
    [];
  (* Treat constants, such as `42` or `...`, as assignable to mutable types. *)
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        # This is treated as `some_attribute: int = ...`.
        some_attribute: int

      def main() -> None:
        x: int = 42
    |}
    [];
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        mutable_attribute: int

      def main() -> None:
        readonly_foo: ReadOnly[Foo]
        mutable_foo: Foo

        x1: int = readonly_foo.mutable_attribute
        x2: int = mutable_foo.mutable_attribute
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: x1 is declared to have readonlyness \
       `ReadOnlyness.Mutable` but is used as readonlyness `ReadOnlyness.ReadOnly`.";
    ];
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      class Bar:
        bar_attribute: int

      class Foo:
        foo_attribute: Bar

      def main() -> None:
        readonly_foo: ReadOnly[Foo]
        mutable_foo: Foo

        x1: int = readonly_foo.foo_attribute.bar_attribute
        x2: int = mutable_foo.foo_attribute.bar_attribute
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: x1 is declared to have readonlyness \
       `ReadOnlyness.Mutable` but is used as readonlyness `ReadOnlyness.ReadOnly`.";
    ];
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        readonly_attribute: ReadOnly[int]

      def main() -> None:
        readonly_foo: ReadOnly[Foo]
        mutable_foo: Foo

        x1: int = readonly_foo.readonly_attribute
        x2: int = mutable_foo.readonly_attribute
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: x2 is declared to have readonlyness \
       `ReadOnlyness.Mutable` but is used as readonlyness `ReadOnlyness.ReadOnly`.";
      "ReadOnly violation - Incompatible variable type [3001]: x1 is declared to have readonlyness \
       `ReadOnlyness.Mutable` but is used as readonlyness `ReadOnlyness.ReadOnly`.";
    ];
  ()


let test_function_call context =
  let assert_readonly_errors = assert_readonly_errors ~context in
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      def expect_mutable_and_readonly(x: int, y: ReadOnly[int]) -> ReadOnly[int]: ...

      def main() -> None:
        x: ReadOnly[int] = 42
        y: int = expect_mutable_and_readonly(x, x)
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have readonlyness \
       `ReadOnlyness.Mutable` but is used as readonlyness `ReadOnlyness.ReadOnly`.";
      "ReadOnly violation - Incompatible parameter type [3002]: In call \
       `test.expect_mutable_and_readonly`, for 1st positional only parameter expected \
       `ReadOnlyness.Mutable` but got `ReadOnlyness.ReadOnly`.";
    ];
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      def foo(x: int) -> ReadOnly[int]: ...

      def main(x: int) -> None:
        y: int = foo(foo(x))
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have readonlyness \
       `ReadOnlyness.Mutable` but is used as readonlyness `ReadOnlyness.ReadOnly`.";
      "ReadOnly violation - Incompatible parameter type [3002]: In call `test.foo`, for 1st \
       positional only parameter expected `ReadOnlyness.Mutable` but got `ReadOnlyness.ReadOnly`.";
    ];
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      def expect_mutable(x: int) -> ReadOnly[int]: ...

      def main() -> None:
        x: ReadOnly[int] = 42
        expect_mutable(x)
    |}
    [
      "ReadOnly violation - Incompatible parameter type [3002]: In call `test.expect_mutable`, for \
       1st positional only parameter expected `ReadOnlyness.Mutable` but got \
       `ReadOnlyness.ReadOnly`.";
    ];
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        def return_readonly(self, x: int) -> ReadOnly[int]: ...

      def main() -> None:
        foo: Foo
        x: ReadOnly[int]
        y: int = foo.return_readonly(x)
    |}
    [
      "ReadOnly violation - Incompatible parameter type [3002]: In call \
       `test.Foo.return_readonly`, for 1st positional only parameter expected \
       `ReadOnlyness.Mutable` but got `ReadOnlyness.ReadOnly`.";
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have readonlyness \
       `ReadOnlyness.Mutable` but is used as readonlyness `ReadOnlyness.ReadOnly`.";
    ];
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        def return_readonly(self, x: int) -> ReadOnly[int]: ...

      def return_foo(x: int) -> Foo: ...

      def main() -> None:
        foo: Foo
        x: ReadOnly[int]
        y: int = return_foo(x).return_readonly(x)
    |}
    [
      "ReadOnly violation - Incompatible parameter type [3002]: In call \
       `test.Foo.return_readonly`, for 1st positional only parameter expected \
       `ReadOnlyness.Mutable` but got `ReadOnlyness.ReadOnly`.";
      "ReadOnly violation - Incompatible parameter type [3002]: In call `test.return_foo`, for 1st \
       positional only parameter expected `ReadOnlyness.Mutable` but got `ReadOnlyness.ReadOnly`.";
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have readonlyness \
       `ReadOnlyness.Mutable` but is used as readonlyness `ReadOnlyness.ReadOnly`.";
    ];
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        def expect_readonly_self(self: ReadOnly[Foo], x: int) -> None: ...

      def main() -> None:
        readonly_foo: ReadOnly[Foo]
        mutable_foo: Foo
        x: int
        readonly_foo.expect_readonly_self(x)
        mutable_foo.expect_readonly_self(x)
    |}
    [];
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        def expect_mutable_self(self, x: int) -> None: ...

      def main() -> None:
        readonly_foo: ReadOnly[Foo]
        mutable_foo: Foo
        x: int
        readonly_foo.expect_mutable_self(x)
        mutable_foo.expect_mutable_self(x)
    |}
    [
      "ReadOnly violation - Incompatible parameter type [3002]: In call \
       `test.Foo.expect_mutable_self`, for 0th positional only parameter expected \
       `ReadOnlyness.Mutable` but got `ReadOnlyness.ReadOnly`.";
    ];
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        some_attribute: int = 42

      def main() -> None:
        readonly_foo: ReadOnly[Foo]
        readonly_foo.some_attribute = 99
    |}
    [
      "ReadOnly violation - Assigning to readonly attribute [3003]: Cannot assign to attribute \
       `some_attribute` since it is readonly";
    ];
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      class Foo:
        readonly_attribute: ReadOnly[int] = 42
        mutable_attribute: int = 42

      def main() -> None:
        mutable_foo: Foo
        mutable_foo.readonly_attribute = 99
        mutable_foo.mutable_attribute = 99
    |}
    [
      "ReadOnly violation - Assigning to readonly attribute [3003]: Cannot assign to attribute \
       `readonly_attribute` since it is readonly";
    ];
  (* Technically, reassigning to a variable doesn't mutate it. So, we don't emit an error in this
     case. *)
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      def main() -> None:
        x: ReadOnly[int] = 42
        x = 99
    |}
    [];
  ()


let test_await context =
  let assert_readonly_errors = assert_readonly_errors ~context in
  (* TODO(T130377746): This should have a readonly violation error. *)
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      async def return_readonly() -> ReadOnly[int]: ...

      async def main() -> None:
        y: int = await return_readonly()
    |}
    [];
  ()


let test_parameters context =
  let assert_readonly_errors = assert_readonly_errors ~context in
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      def main(my_mutable: int, my_readonly: ReadOnly[int]) -> None:
        y: int = my_readonly
        y2: ReadOnly[int] = my_mutable
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: y is declared to have readonlyness \
       `ReadOnlyness.Mutable` but is used as readonlyness `ReadOnlyness.ReadOnly`.";
    ];
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      def main(unannotated) -> None:
        y: int = unannotated
    |}
    [];
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      def main(x: ReadOnly[int], /, y: ReadOnly[int], *, z: ReadOnly[int]) -> None:
        y1: int = x
        y2: int = y
        y3: int = z
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: y3 is declared to have readonlyness \
       `ReadOnlyness.Mutable` but is used as readonlyness `ReadOnlyness.ReadOnly`.";
      "ReadOnly violation - Incompatible variable type [3001]: y2 is declared to have readonlyness \
       `ReadOnlyness.Mutable` but is used as readonlyness `ReadOnlyness.ReadOnly`.";
      "ReadOnly violation - Incompatible variable type [3001]: y1 is declared to have readonlyness \
       `ReadOnlyness.Mutable` but is used as readonlyness `ReadOnlyness.ReadOnly`.";
    ];
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      def main(*args: ReadOnly[int], **kwargs: ReadOnly[int]) -> None:
        y1: int = args
        y2: int = kwargs
    |}
    [
      "ReadOnly violation - Incompatible variable type [3001]: y2 is declared to have readonlyness \
       `ReadOnlyness.Mutable` but is used as readonlyness `ReadOnlyness.ReadOnly`.";
      "ReadOnly violation - Incompatible variable type [3001]: y1 is declared to have readonlyness \
       `ReadOnlyness.Mutable` but is used as readonlyness `ReadOnlyness.ReadOnly`.";
    ];
  (* Check for errors in constructing the default value of a parameter. *)
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      def return_readonly() -> ReadOnly[int]: ...

      def expect_mutable(x: int) -> int: ...

      def main(x: int = expect_mutable(return_readonly())) -> None:
        pass
    |}
    [
      "ReadOnly violation - Incompatible parameter type [3002]: In call `test.expect_mutable`, for \
       1st positional only parameter expected `ReadOnlyness.Mutable` but got \
       `ReadOnlyness.ReadOnly`.";
    ];
  (* Don't consider `x` in scope for the default value of parameter `y`. We don't need to emit an
     error here, because the type checking analysis will. *)
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      def foo(x: ReadOnly[int], y: int = x) -> None:
        pass
    |}
    [];
  ()


let test_reveal_type context =
  let assert_readonly_errors = assert_readonly_errors ~context in
  assert_readonly_errors
    {|
      from pyre_extensions import ReadOnly

      def main(my_mutable: int, my_readonly: ReadOnly[int]) -> None:
        y1 = my_readonly
        y2 = my_mutable
        reveal_type(y1)
        reveal_type(y2)
    |}
    [
      "ReadOnly - Revealed type [3004]: Revealed type for `y2` is ReadOnlyness.Mutable.";
      "ReadOnly - Revealed type [3004]: Revealed type for `y1` is ReadOnlyness.ReadOnly.";
    ];
  ()


let () =
  "readOnly"
  >::: [
         "forward_expression" >:: test_forward_expression;
         "check_arguments_against_parameters" >:: test_check_arguments_against_parameters;
         "callable_data_list_for_callee" >:: test_callable_data_list_for_callee;
         "assignment" >:: test_assignment;
         "function_call" >:: test_function_call;
         "await" >:: test_await;
         "parameters" >:: test_parameters;
         "reveal_type" >:: test_reveal_type;
       ]
  |> Test.run
