(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Test

let test_content context =
  let data = "file" in
  let path, _ = bracket_tmpfile context in
  Out_channel.write_all ~data path;
  let path = PyrePath.create_absolute path in
  assert_equal (File.create path |> File.content) (Some data);
  assert_equal (File.create ~content:"content" path |> File.content) (Some "content");
  assert_is_none (File.create (PyrePath.create_relative ~root:path ~relative:"derp") |> File.content)


let test_lines context =
  let path, _ = bracket_tmpfile context in
  assert_equal
    ~cmp:(List.equal String.equal)
    (File.create ~content:"foo\nbar" (PyrePath.create_absolute path)
    |> File.lines
    |> fun lines -> Option.value_exn lines)
    ["foo"; "bar"]


let () = "file" >::: ["content" >:: test_content; "lines" >:: test_lines] |> Test.run
