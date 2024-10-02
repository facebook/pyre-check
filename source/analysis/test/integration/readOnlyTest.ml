(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_readonly =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from typing import TypedDict
           from typing_extensions import ReadOnly

           class Band(TypedDict):
             name: str
             members: ReadOnly[list[str]]

           blur: Band = {"name": "blur", "members": []}
           blur["members"] = ["Damon Albarn"]  # Type check error: "members" is read-only
            |}
           ["Invalid TypedDict operation [54]: Cannot write to `Band` read-only field `members`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
           from typing import TypedDict
           from typing_extensions import ReadOnly

           class Band(TypedDict):
             name: str
             members: ReadOnly[list[str]]

           blur: Band = {"name": "blur", "members": []}
           blur["members"].append("Damon Albarn")  # OK: list is mutable
            |}
           [];
    ]


let test_interaction_with_required_and_annotated =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
          from typing import Annotated, TypedDict
          from typing_extensions import NotRequired, ReadOnly, Required

          class TD(TypedDict):
            x: Annotated[ReadOnly[int], 42]
            y: Required[ReadOnly[str]]
            z: NotRequired[ReadOnly[bytes]]

          # Test that NotRequired on 'z' is respected
          td1: TD = {'x': 0, 'y': ""}  # OK

          # Test that Required on 'y' is respected
          td2: TD = {'x': 0, 'z': b""}  # Error
          |}
           ["TypedDict initialization error [55]: Missing required field `y` for TypedDict `TD`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
          from typing import Annotated, TypedDict
          from typing_extensions import NotRequired, ReadOnly, Required

          class TD(TypedDict):
            x: ReadOnly[Annotated[int, 42]]
            y: ReadOnly[Required[str]]
            z: ReadOnly[NotRequired[bytes]]

          # Test that NotRequired on 'z' is respected
          td1: TD = {'x': 0, 'y': ""}  # OK

          # Test that Required on 'y' is respected
          td2: TD = {'x': 0, 'z': b""}  # Error
            |}
           ["TypedDict initialization error [55]: Missing required field `y` for TypedDict `TD`."];
    ]


let test_assignability =
  (* Tests assignability rules from
     https://typing.readthedocs.io/en/latest/spec/typeddict.html#id4 *)
  test_list
    [
      (* For each item in A, B has the corresponding key, unless the item in A is read-only, not
         required, and of top value type (ReadOnly[NotRequired[object]]).*)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
        from typing import TypedDict
        from typing_extensions import NotRequired, ReadOnly

        class A(TypedDict):
          x: int
          y: ReadOnly[NotRequired[object]]

        class B_Ok(TypedDict):
          x: int

        class B_Error(TypedDict):
          y: ReadOnly[NotRequired[object]]

        def f(b_ok: B_Ok, b_error: B_Error) -> None:
          a: A = b_ok
          a: A = b_error
      |}
           [
             "Incompatible variable type [9]: a is declared to have type `A` but is used as type \
              `B_Error`.";
           ];
      (* For each item in A, if B has the corresponding key, the corresponding value type in B is
         assignable to the value type in A. *)
      (* For each non-read-only item in A, its value type is assignable to the corresponding value
         type in B. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
        from typing import TypedDict
        from typing_extensions import ReadOnly

        class A1(TypedDict):
          x: int

        class A2(TypedDict):
          x: ReadOnly[int]

        class B1(TypedDict):
          x: bool

        class B2(TypedDict):
          x: str

        def f(b1: B1, b2: B2) -> None:
          a1: A1 = b1  # error
          a2: A2 = b1  # ok
          a3: A2 = b2  # error
      |}
           [
             "Incompatible variable type [9]: a1 is declared to have type `A1` but is used as type \
              `B1`.";
             "Incompatible variable type [9]: a3 is declared to have type `A2` but is used as type \
              `B2`.";
           ];
      (* For each required key in A, the corresponding key is required in B. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
        from typing import TypedDict
        from typing_extensions import NotRequired, Required

        class A(TypedDict):
          x: Required[int]

        class B1(TypedDict):
          x: int

        class B2(TypedDict):
          x: Required[int]

        class B3(TypedDict):
          x: NotRequired[int]

        def f(b1: B1, b2: B2, b3: B3) -> None:
          a1: A = b1  # ok
          a2: A = b2  # ok
          a3: A = b3  # error
      |}
           [
             "Incompatible variable type [9]: a3 is declared to have type `A` but is used as type \
              `B3`.";
           ];
      (* For each non-required key in A, if the item is not read-only in A, the corresponding key is
         not required in B. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
        from typing import TypedDict
        from typing_extensions import NotRequired, ReadOnly, Required

        class A1(TypedDict):
          x: NotRequired[int]

        class A2(TypedDict):
          x: NotRequired[ReadOnly[int]]

        class B1(TypedDict):
          x: NotRequired[int]

        class B2(TypedDict):
          x: Required[int]

        def f(b1: B1, b2: B2) -> None:
          a1: A1 = b1  # ok
          a2: A1 = b2  # error
          a3: A2 = b1  # ok
          a4: A2 = b2  # ok
      |}
           [
             "Incompatible variable type [9]: a2 is declared to have type `A1` but is used as type \
              `B2`.";
           ];
      (* A non-ReadOnly item cannot be redeclared as ReadOnly. This is covered in the conformance
         tests. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
        from typing import TypedDict
        from typing_extensions import ReadOnly

        class A(TypedDict):
          x: int

        class B(TypedDict):
          x: ReadOnly[int]

        def f(b: B) -> None:
          a: A = b  # error
      |}
           [
             "Incompatible variable type [9]: a is declared to have type `A` but is used as type \
              `B`.";
           ];
    ]


let test_inheritance =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import TypedDict
      from typing_extensions import ReadOnly

      class A(TypedDict):
        name: ReadOnly[str]

      class B(A):
        name: str
        year: int

      def f(b: B) -> None:
        b["name"] = "Hello World"  # ok
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import TypedDict
      from typing_extensions import ReadOnly

      class A(TypedDict):
        name: ReadOnly[str]

      class B(A):
        year: int

      def f(b: B) -> None:
        b["name"] = "Hello World"  # error
      |}
           ["Invalid TypedDict operation [54]: Cannot write to `B` read-only field `name`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
        from typing import TypedDict
        from typing_extensions import ReadOnly

        class A(TypedDict):
          x: ReadOnly[int]

        class B_Ok(A):
          x: ReadOnly[bool]

        class B_Err(A):
          x: ReadOnly[str]
      |}
           [
             "Inconsistent override [15]: `x` overrides attribute defined in `A` inconsistently. \
              Type `str` is not a subtype of the overridden attribute `int`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
        from typing import TypedDict
        from typing_extensions import NotRequired, ReadOnly, Required

        class A(TypedDict):
          x: NotRequired[ReadOnly[str]]

        class B(A):
          x: Required[ReadOnly[str]]
      |}
           [];
    ]


let test_dict_update =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import TypedDict
      from typing_extensions import ReadOnly

      class A(TypedDict):
        x: ReadOnly[int]
        y: str

      def f(a: A) -> None:
        a.update(x=5)  # error
        a.update(y='')  # ok
      |}
           [
             "Incompatible parameter type [6]: In call `TypedDictionary.update`, for argument `x`, \
              cannot update read-only field.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
    from typing import TypedDict
    from typing_extensions import ReadOnly

    class A(TypedDict):
      x: ReadOnly[int]

    def f(a1: A, a2: A) -> None:
      a1.update(a2)
    |}
           [
             "Incompatible parameter type [6]: In call `TypedDictionary.update`, for 1st \
              positional argument, cannot update read-only fields of `A`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
    from typing import Never, TypedDict
    from typing_extensions import NotRequired, ReadOnly

    class A(TypedDict):
      x: ReadOnly[int]

    class B(TypedDict):
      x: NotRequired[Never]

    def f(a: A, b: B) -> None:
      a.update(b)
    |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
    from typing import TypedDict
    from typing_extensions import ReadOnly

    class A(TypedDict):
      x: ReadOnly[int]
      y: str

    class B_Ok(TypedDict):
      y: str

    class B_Err(TypedDict):
      y: bool

    def f(a: A, b_ok: B_Ok, b_err: B_Err) -> None:
      a.update(b_ok)
      a.update(b_err)
          |}
           [
             "Incompatible parameter type [6]: In call `TypedDictionary.update`, for 1st \
              positional argument, expected `A` but got `B_Err`.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           (* Tests a case where B is not assignable to A but the update() call is still valid. *)
           {|
      from typing import TypedDict
      from typing_extensions import ReadOnly

      class A(TypedDict):
        x: int

      class B(TypedDict):
        x: ReadOnly[int]

      def f(a: A, b: B) -> None:
        a.update(b)
      |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
      from typing import TypedDict
      from typing_extensions import ReadOnly

      class A(TypedDict, total=False):
        x: ReadOnly[int]
        y: str

      class B(TypedDict):
        y: str

      def f(a: A, b: B) -> None:
        a.update(b)
      |}
           [];
    ]


let () =
  "readOnly"
  >::: [
         test_readonly;
         test_interaction_with_required_and_annotated;
         test_assignability;
         test_inheritance;
         test_dict_update;
       ]
  |> Test.run
