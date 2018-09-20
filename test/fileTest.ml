(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Pyre
open Test


let test_content context =
  let data = "file" in

  let path, _ = bracket_tmpfile context in
  Out_channel.write_all ~data path;

  let path = Path.create_absolute path in

  assert_equal (File.create path |> File.content) (Some data);
  assert_equal (File.create ~content:"content" path |> File.content) (Some "content");
  assert_is_none
    (File.create (Path.create_relative ~root:path ~relative:"derp") |> File.content)


let test_lines context =
  let path, _ = bracket_tmpfile context in
  assert_equal
    ~cmp:(List.equal ~equal:String.equal)
    (File.create ~content:"foo\nbar" (Path.create_absolute path)
     |> File.lines
     |> (fun lines -> Option.value_exn lines))
    ["foo"; "bar"]


let test_handle _ =
  let assert_handle ~absolute ~handle =
    let path = Path.create_absolute ~follow_symbolic_links:false in
    let configuration =
      Configuration.create
        ~local_root:(path "/root")
        ~search_path:[path "/root/stubs"; path "/external"]
        ~typeshed:(path "/typeshed")
        ()
    in
    match handle with
    | None ->
        let message =
          let roots =
            List.to_string
              ["/root/stubs"; "/external"; "/typeshed/stdlib"; "/typeshed/third_party"; "/root"]
              ~f:ident
          in
          Format.sprintf "Unable to construct handle for %s. Possible roots: %s" absolute roots
        in
        assert_raises (File.NonexistentHandle message)
          (fun () -> File.handle ~configuration (File.create (path absolute)))
    | Some handle ->
        let expected =
          File.handle ~configuration (File.create (path absolute))
          |> File.Handle.show
        in
        assert_equal expected handle
  in
  assert_handle ~absolute:"/root/a.py" ~handle:(Some "a.py");

  assert_handle ~absolute:"/external/b/c.py" ~handle:(Some "b/c.py");
  assert_handle ~absolute:"/root/stubs/stub.pyi" ~handle:(Some "stub.pyi");

  assert_handle ~absolute:"/typeshed/stdlib/3/builtins.pyi" ~handle:(Some "3/builtins.pyi");
  assert_handle ~absolute:"/typeshed/third_party/3/django.pyi" ~handle:(Some "3/django.pyi");
  assert_handle ~absolute:"/typeshed/3/whoops.pyi" ~handle:None;

  assert_handle ~absolute:"/untracked/a.py" ~handle:None


let () =
  "file">:::[
    "content">::test_content;
    "lines">::test_lines;
    "handle">::test_handle;
  ]
  |> Test.run
