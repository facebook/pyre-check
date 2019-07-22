(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Ast
open Test

let test_empty_stub _ =
  assert_true
    (Module.create_for_testing ~local_mode:Source.PlaceholderStub ~stub:true |> Module.empty_stub);
  assert_false
    (Module.create_for_testing ~local_mode:Source.PlaceholderStub ~stub:false |> Module.empty_stub);
  assert_false
    (Module.create_for_testing ~local_mode:Source.Default ~stub:true |> Module.empty_stub)


let test_aliased_export _ =
  let assert_aliased_exports ?(handle = "") source aliased_exports =
    let module_definition = Module.create (parse ~handle source) in
    let assert_aliased_export (source, expected_target) =
      let actual_target =
        Reference.create source
        |> Module.aliased_export module_definition
        |> (fun value -> Option.value value ~default:(Reference.create "$not_exported"))
        |> Reference.show
      in
      assert_equal ~printer:Fn.id expected_target actual_target
    in
    List.iter ~f:assert_aliased_export aliased_exports
  in
  assert_aliased_exports
    {|
      from other.module import Class
      from different.module import function
      import blah
      import standard_library_module as module
    |}
    [ "Class", "other.module.Class";
      "function", "different.module.function";
      "blah", "blah";
      "module", "standard_library_module" ];
  assert_aliased_exports "from some.module import aliased as alias" ["alias", "some.module.aliased"];
  assert_aliased_exports
    "from some.module import one, two"
    ["one", "some.module.one"; "two", "some.module.two"];
  assert_aliased_exports "from some.module import *" ["*", "some.module"];
  assert_aliased_exports
    ~handle:"some/module.py"
    "from . import path as other"
    ["other", "some.path"];
  assert_aliased_exports
    ~handle:"some/long/module.py"
    "from .relative import path as other"
    ["other", "some.long.relative.path"];
  assert_aliased_exports
    ~handle:"some/long/module.py"
    "from ..relative import path as other"
    ["other", "some.relative.path"];
  assert_aliased_exports
    ~handle:"some/long/module.py"
    "from ...relative import path as other"
    ["other", "relative.path"];
  assert_aliased_exports
    ~handle:"some/module.py"
    "from some.module.derp import path as other"
    ["other", "some.module.derp.path"];
  assert_aliased_exports
    ~handle:"some/module.py"
    "from some.module.other import other as other"
    ["other", "some.module.other.other"];
  assert_aliased_exports "from builtins import path as other" ["other", "path"];
  assert_aliased_exports
    {|
      from other import thing
      from other import thing
    |}
    ["thing", "other.thing"];

  (* Exports through assignments. *)
  assert_aliased_exports
    ~handle:"requests.py"
    {|
      from . import api
      $local_requests$post = requests.api.post
      $local_requests$call = requests.api.call()
      $local_requests$not_exported = other.assignment
    |}
    ["post", "requests.api.post"; "call", "$not_exported"; "not_exported", "$not_exported"]


let () =
  "module"
  >::: ["empty_stub" >:: test_empty_stub; "aliased_export" >:: test_aliased_export]
  |> Test.run
