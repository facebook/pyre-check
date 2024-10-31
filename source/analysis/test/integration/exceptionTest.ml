(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_raise =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              raise None
            |}
           [
             "Invalid Exception [48]: Expression `None` has type `None` but must extend \
              BaseException.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors {|
              raise Exception()
            |} [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors {|
              raise ValueError(1)
            |} [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              raise ExceptionGroup("eg", [ValueError(1), TypeError(2), OSError(3), OSError(4)])
            |}
           [];
    ]


let test_exception_handlers =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              try:
                pass
              except int as x:
                pass
            |}
           [
             "Invalid except clause [66]: Exception handler type annotation `int` must extend \
              BaseException.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              try:
                pass
              except* int as e:
                pass
            |}
           [
             "Invalid except clause [66]: Exception handler type annotation `int` must extend \
              BaseException.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              try:
                pass
              except int:
                pass
            |}
           [
             "Invalid except clause [66]: Exception handler type annotation `int` must extend \
              BaseException.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              try:
                pass
              except* int:
                pass
            |}
           [
             "Invalid except clause [66]: Exception handler type annotation `int` must extend \
              BaseException.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              try:
                pass
              except (Exception, int):
                pass
            |}
           [
             "Invalid except clause [66]: Exception handler type annotation `int` must extend \
              BaseException.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              try:
                pass
              except* (Exception, int):
                pass
            |}
           [
             "Invalid except clause [66]: Exception handler type annotation `int` must extend \
              BaseException.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              try:
                pass
              except Exception as e:
                pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              try:
                pass
              except* Exception as e:
                pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              try:
                pass
              except Exception:
                pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              try:
                pass
              except* Exception:
                pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              try:
                pass
              except ValueError as e:
                pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              try:
                pass
              except* ValueError as e:
                reveal_type(e)
            |}
           ["Revealed type [-1]: Revealed type for `e` is `ValueError`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              try:
                pass
              except* (ValueError, NameError) as e:
                reveal_type(e)
            |}
           ["Revealed type [-1]: Revealed type for `e` is `typing.Union[NameError, ValueError]`."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              try:
                pass
              except ValueError:
                pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              try:
                pass
              except* ValueError:
                pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Type
              exception_type: Type[Exception] = ...
              try:
                pass
              except exception_type:
                pass
            |}
           [];
      (* even though exception_type could be an ExceptionGroup which is not allowed for except*, we
         allow it to pass because that check is best-effort *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Type
              exception_type: Type[Exception] = ...
              try:
                pass
              except* exception_type:
                pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Type, Tuple
              exception_type: Tuple[Type[Exception]] = ...
              try:
                pass
              except exception_type:
                pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Type, Tuple
              exception_type: Tuple[Type[Exception]] = ...
              try:
                pass
              except* exception_type:
                pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Type, Tuple
              exception_type: Tuple[Type[Exception], ...] = ...
              try:
                pass
              except exception_type:
                pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Type, Tuple
              exception_type: Tuple[Type[Exception], ...] = ...
              try:
                pass
              except* exception_type:
                pass
            |}
           [];
    ]


let test_exception_group_handlers =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              class MyExceptionGroup(ExceptionGroup[ValueError]): ...
              try:
                pass
              except MyExceptionGroup as e:
                pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              class MyExceptionGroup(ExceptionGroup[ValueError]): ...
              try:
                pass
              except* MyExceptionGroup as e:
                pass
            |}
           [
             "Invalid except* clause [67]: Exception group handler type annotation \
              `MyExceptionGroup` may not extend BaseExceptionGroup.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              class MyExceptionGroup(ExceptionGroup[ValueError]): ...
              try:
                pass
              except MyExceptionGroup:
                pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              class MyExceptionGroup(ExceptionGroup[ValueError]): ...
              try:
                pass
              except* MyExceptionGroup:
                pass
            |}
           [
             "Invalid except* clause [67]: Exception group handler type annotation \
              `MyExceptionGroup` may not extend BaseExceptionGroup.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              class MyExceptionGroup(ExceptionGroup[ValueError]): ...
              try:
                pass
              except (Exception, MyExceptionGroup):
                pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              class MyExceptionGroup(ExceptionGroup[ValueError]): ...
              try:
                pass
              except* (Exception, MyExceptionGroup):
                pass
            |}
           [
             "Invalid except* clause [67]: Exception group handler type annotation \
              `MyExceptionGroup` may not extend BaseExceptionGroup.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Type
              exception_type: Type[ExceptionGroup[ValueError]] = ...
              try:
                pass
              except exception_type:
                pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Type
              exception_type: Type[ExceptionGroup[ValueError]] = ...
              try:
                pass
              except* exception_type:
                pass
            |}
           [
             "Invalid except* clause [67]: Exception group handler type annotation \
              `ExceptionGroup[ValueError]` may not extend BaseExceptionGroup.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Type, Tuple
              exception_type: Tuple[Type[ExceptionGroup[ValueError]]] = ...
              try:
                pass
              except exception_type:
                pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Type, Tuple
              exception_type: Tuple[Type[ExceptionGroup[ValueError]]] = ...
              try:
                pass
              except* exception_type:
                pass
            |}
           [
             "Invalid except* clause [67]: Exception group handler type annotation \
              `ExceptionGroup[ValueError]` may not extend BaseExceptionGroup.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Type, Tuple
              exception_type: Tuple[Type[ExceptionGroup[ValueError]], ...] = ...
              try:
                pass
              except exception_type:
                pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Type, Tuple
              exception_type: Tuple[Type[ExceptionGroup[ValueError]], ...] = ...
              try:
                pass
              except* exception_type:
                pass
            |}
           [
             "Invalid except* clause [67]: Exception group handler type annotation \
              `ExceptionGroup[ValueError]` may not extend BaseExceptionGroup.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Type
              exception_type: Type[ExceptionGroup[ValueError]] = ...
              try:
                pass
              except exception_type:
                pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           {|
              from typing import Type
              exception_type: Type[ExceptionGroup[ValueError]] = ...
              try:
                pass
              except* exception_type:
                pass
            |}
           [
             "Invalid except* clause [67]: Exception group handler type annotation \
              `ExceptionGroup[ValueError]` may not extend BaseExceptionGroup.";
           ];
    ]


let () =
  "exception" >::: [test_raise; test_exception_handlers; test_exception_group_handlers] |> Test.run
