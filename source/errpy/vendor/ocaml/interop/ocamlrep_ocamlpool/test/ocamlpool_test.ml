(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 *)

external test : unit -> unit = "test"
external test_call_ocaml_from_rust : unit -> unit = "test_call_ocaml_from_rust"

let f_unit_to_unit (): unit = ()
let f_one_arg_to_unit (x: int) = assert (x = 3)
let f_sum_tuple ((x,y):int*int): int = x + y

(* Although the test is entirely written in rust,
 * we need to build the rust code with the ocaml runtime dependencies
 * in order to allocate memory for ocaml. Calling rust from ocaml
 * is a good way of ensuring this dependecy is built.
 *)
let () = begin
    print_endline "[ocamlpool_test][info]: start";

    Callback.register "f_unit_to_unit" f_unit_to_unit;
    Callback.register "f_one_arg_to_unit" f_one_arg_to_unit;
    Callback.register "f_sum_tuple" f_sum_tuple;

    test ();

    test_call_ocaml_from_rust ();

    print_endline "[ocamlpool_test][info]: finish"
end
