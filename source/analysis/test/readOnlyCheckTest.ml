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
  assert_resolved ~resolution:(Resolution.of_list [!&"x", ReadOnly]) {| f"hello, {x}" |} Mutable;
  assert_resolved ~resolution:(Resolution.of_list [!&"x", Mutable]) {| f"hello, {x}" |} Mutable;
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
                   Argument.WithPosition.resolved = ReadOnly;
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
          keyword_argument_name = None;
          position = 1;
          callee = None;
          mismatch = { actual = ReadOnly; expected = Mutable };
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
                   Argument.WithPosition.resolved = ReadOnly;
                   kind = Named (Node.create_with_default_location "y");
                   expression = None;
                   position = 1;
                 };
             ] );
         ])
    [
      IncompatibleParameterType
        {
          keyword_argument_name = Some "y";
          position = 1;
          callee = None;
          mismatch = { actual = ReadOnly; expected = Mutable };
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


let () =
  "readOnly"
  >::: [
         "forward_expression" >:: test_forward_expression;
         "check_arguments_against_parameters" >:: test_check_arguments_against_parameters;
         "callable_data_list_for_callee" >:: test_callable_data_list_for_callee;
       ]
  |> Test.run
