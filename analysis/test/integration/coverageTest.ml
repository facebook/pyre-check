(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open IntegrationTest
open Test

let test_check_coverage context =
  let preprocess source =
    trim_extra_indentation source
    |> String.lstrip
    |> String.split ~on:'\n'
    |> List.map ~f:(fun line -> "    " ^ line)
    |> String.concat ~sep:"\n"
    |> String.substr_replace_all ~pattern:"ERROR" ~with_:"a.undefined"
    |> Format.asprintf "def foo(a: A) -> None:\n%s\n"
  in
  let assert_covered ?(additional_errors = []) source =
    assert_type_errors
      ~context
      (preprocess source)
      (additional_errors @ ["Undefined attribute [16]: `A` has no attribute `undefined`."])
  in
  let assert_not_covered ?(additional_errors = []) source =
    assert_type_errors ~context (preprocess source) additional_errors
  in
  (* Return statement. *)
  assert_covered
    ~additional_errors:["Incompatible return type [7]: Expected `None` but got `unknown`."]
    "return ERROR";

  (* Assignment. *)
  assert_covered "b = ERROR";

  (* Assertion. *)
  assert_covered "assert ERROR";

  (* Nested definitions. *)
  assert_covered "class B: ERROR";
  assert_covered ~additional_errors:[] "def nested() -> None: ERROR";

  (* Expressions. *)
  assert_covered "ERROR";
  assert_covered "typing.cast(int, ERROR)";
  assert_covered "isinstance(ERROR, int)";

  (* Yield. *)
  assert_covered
    ~additional_errors:
      [ "Incompatible return type [7]: Expected `None` but got "
        ^ "`typing.Generator[unknown, None, None]`." ]
    "yield ERROR";
  assert_covered
    ~additional_errors:
      [ "Incompatible return type [7]: Expected `None` but got "
        ^ "`typing.Generator[unknown, None, None]`." ]
    "yield from ERROR";

  (* Control statements. *)
  assert_covered "for i in ERROR: pass";
  assert_covered "while ERROR: pass";
  assert_covered "if ERROR: pass";

  (* Raise. *)
  assert_covered
    ~additional_errors:
      [ "Invalid Exception [48]: Expression `a.undefined` has type `unknown` but must extend \
         BaseException." ]
    "raise ERROR";
  assert_covered {|
      try:
        pass
      except ERROR:
        pass
    |};
  assert_covered {|
      with ERROR:
        pass
    |};
  assert_covered {|
      with ERROR as derp:
        pass
    |};

  (* Await. *)
  assert_covered
    ~additional_errors:
      ["Incompatible awaitable type [12]: Expected an awaitable but got `unknown`."]
    "await ERROR";

  (* Binary operator. *)
  assert_covered "ERROR | 1";
  assert_covered "1 % ERROR";

  (* Boolean operator. *)
  assert_covered "ERROR or False";

  (* True or UNDEFINED evaluates to True in python. *)
  assert_not_covered "True or ERROR";
  assert_covered "ERROR and False";
  assert_covered "True and ERROR";

  (* Comparison operator. *)
  assert_covered "1 == ERROR";
  assert_covered "ERROR < 1";

  (* Dictionaries. *)
  assert_covered "{ ERROR: 1 }";
  assert_covered "{ 1: ERROR }";
  assert_covered "{ ERROR: i for i in {1: 1} }";
  assert_covered "{ i: ERROR for i in {1: 1} }";
  assert_covered "{ i: 1 for i in ERROR }";

  (* Format string. *)
  assert_covered {|f"format{ERROR}"|};

  (* Generator. *)
  assert_covered "(ERROR for i in [1])";

  (* Lambdas. *)
  assert_covered "lambda x: ERROR";

  (* Lists. *)
  assert_covered "[1, ERROR]";
  assert_covered "[ERROR for i in [1]]";
  assert_covered "[i for i in ERROR]";

  (* Sets. *)
  assert_covered "{1, ERROR}";
  assert_covered "{ERROR for i in [1]}";
  assert_covered "{i for i in ERROR}";

  (* Starred. *)
  assert_covered "*ERROR";
  assert_covered "**ERROR";

  (* Ternary. *)
  assert_covered "ERROR if True else 1";
  assert_covered "True if ERROR else 1";
  assert_covered "True if True else ERROR";

  (* Tuples. *)
  assert_covered "ERROR, True";
  assert_covered "True, ERROR";

  (* Unary operators. *)
  assert_covered "not ERROR";
  assert_covered "-ERROR"


let () = "coverage" >::: ["check_coverage" >:: test_check_coverage] |> Test.run
