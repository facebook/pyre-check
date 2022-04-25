(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2


module StringKey = struct
  type t = string

  let to_string = Fn.id

  let compare = String.compare

  let from_string = Fn.id
end

module StringValue = struct
  type t = string

  let prefix = Prefix.make ()

  let description = "string for testing"
end

module StringStringTable = SharedMemory.FirstClass.WithCache.Make (StringKey) (StringValue)

let test_first_class _ =
  (* Initialize a table, set and check a value *)
  let table0 = StringStringTable.create () in
  assert_equal
    ~cmp:(Option.equal String.equal)
    (StringStringTable.get table0 "x")
    None;
  StringStringTable.add table0 "x" "x0";
  assert_equal
    ~cmp:(Option.equal String.equal)
    (StringStringTable.get table0 "x")
    (Some "x0");
  (* Initialize a second table, set and check the same value and another *)
  let table1 = StringStringTable.create () in
  assert_equal
    ~cmp:(Option.equal String.equal)
    (StringStringTable.get table1 "x")
    None;
  StringStringTable.add table1 "x" "x1";
  assert_equal
    ~cmp:(Option.equal String.equal)
    (StringStringTable.get table1 "x")
    (Some "x1");
  assert_equal
    ~cmp:(Option.equal String.equal)
    (StringStringTable.get table1 "y")
    None;
  StringStringTable.add table1 "y" "y1";
  assert_equal
    ~cmp:(Option.equal String.equal)
    (StringStringTable.get table1 "y")
    (Some "y1");
  (* Make sure the value in table1 did not interfere with table0 *)
  assert_equal
    ~cmp:(Option.equal String.equal)
    (StringStringTable.get table0 "x")
    (Some "x0");
  assert_equal
    ~cmp:(Option.equal String.equal)
    (StringStringTable.get table0 "y")
    None;
  (* Check a batched operation, since we use different wrappers to build these functions *)
  let actual =
    StringStringTable.KeySet.of_list ["x"; "y"; "z"]
    |> StringStringTable.get_batch table1
    |> StringStringTable.KeyMap.elements
    |> List.sort ~compare:[%compare: string * string option]
  in
  let expected = [("x", Some "x1"); ("y", Some "y1"); ("z", None)] in
  assert_equal
    ~cmp:[%eq: (string * string option) list]
    ~printer:[%show: (string * string option) list]
    expected
    actual;
  ()

let () =
  "shared_memory"
  >::: [
         "first_class" >:: test_first_class;
       ]
  |> Test.run
