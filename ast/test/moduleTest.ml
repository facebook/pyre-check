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
    ( Module.create ~qualifier:Reference.empty ~local_mode:Source.PlaceholderStub ~stub:true []
    |> Module.empty_stub );
  assert_false
    ( Module.create ~qualifier:Reference.empty ~local_mode:Source.PlaceholderStub ~stub:false []
    |> Module.empty_stub );
  assert_false
    ( Module.create ~qualifier:Reference.empty ~local_mode:Source.Default ~stub:true []
    |> Module.empty_stub )


let test_handle _ =
  assert_equal
    ( Module.create ~qualifier:Reference.empty ~local_mode:Source.Default ~stub:true []
    |> Module.handle )
    None;
  assert_equal
    ( Module.create
        ~qualifier:Reference.empty
        ~local_mode:Source.Default
        ~handle:(File.Handle.create_for_testing "voodoo.py")
        ~stub:false
        []
    |> Module.handle )
    (Some (File.Handle.create_for_testing "voodoo.py"))


let test_aliased_export _ =
  let assert_aliased_exports ?(qualifier = Reference.empty) ?handle source aliased_exports =
    let module_definition =
      let { Source.statements; _ } = parse source in
      Module.create ?handle ~qualifier ~local_mode:Source.Default ~stub:false statements
    in
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
    ~qualifier:(Reference.create "some.module")
    "from . import path as other"
    ["other", "some.path"];
  assert_aliased_exports
    ~qualifier:(Reference.create "some.long.module")
    "from .relative import path as other"
    ["other", "some.long.relative.path"];
  assert_aliased_exports
    ~qualifier:(Reference.create "some.long.module")
    "from ..relative import path as other"
    ["other", "some.relative.path"];
  assert_aliased_exports
    ~qualifier:(Reference.create "some.long.module")
    "from ...relative import path as other"
    ["other", "relative.path"];
  assert_aliased_exports
    ~qualifier:(Reference.create "some.module")
    "from some.module.derp import path as other"
    ["other", "some.module.derp.path"];
  assert_aliased_exports
    ~qualifier:(Reference.create "some.module")
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
    ~qualifier:(Reference.create "requests")
    ~handle:(File.Handle.create_for_testing "requests/__init__.pyi")
    {|
      from . import api
      $local_requests$post = requests.api.post
      $local_requests$call = requests.api.call()
      $local_requests$not_exported = other.assignment
    |}
    ["post", "requests.api.post"; "call", "$not_exported"; "not_exported", "$not_exported"]


let test_wildcard_exports _ =
  let module_from_source ~source ~qualifier =
    let { Source.statements; _ } = parse source in
    Module.create ~qualifier ~local_mode:Source.Default ~stub:false statements
  in
  let assert_wildcard_exports ?(qualifier = Reference.empty) source expected =
    assert_equal
      ~cmp:(List.equal ~equal:Reference.equal)
      ~printer:(fun expression_list ->
        List.map expression_list ~f:Reference.show |> String.concat ~sep:", ")
      (List.map expected ~f:Reference.create)
      (module_from_source ~source ~qualifier |> Module.wildcard_exports)
  in
  let assert_in_wildcard_exports ?(qualifier = Reference.empty) source reference expected_bool =
    if expected_bool then
      assert_true
        (Module.in_wildcard_exports
           (module_from_source ~source ~qualifier)
           (Reference.create reference))
    else
      assert_false
        (Module.in_wildcard_exports
           (module_from_source ~source ~qualifier)
           (Reference.create reference))
  in
  assert_wildcard_exports
    {|
      from other.module import Class
      from different.module import function
      import blah
      import standard_library_module as module
      def foo(): pass
      variable = 1
      class Bar: pass
      $local_module$Qualified = 1
    |}
    ["Class"; "function"; "blah"; "module"; "foo"; "variable"; "Bar"; "Qualified"];
  assert_wildcard_exports
    {|
      from other.module import Class
      from different.module import function
      import blah
      import standard_library_module as module
      def foo(): pass
      variable = 1
      class Bar: pass
      __all__ = ["only_export"]
    |}
    ["only_export"];
  assert_wildcard_exports
    {|
      def foo(): ...
      variable = ...
      class Bar: ...
    |}
    ["foo"; "variable"; "Bar"];
  assert_wildcard_exports
    {|
      import standard_library_module as _module
      def _foo(): pass
      _variable = 1
      class _Bar: pass
    |}
    [];
  assert_wildcard_exports
    ~qualifier:(Reference.create "_underscore")
    {|
      def foo(): ...
      variable = ...
      class Bar: ...
    |}
    ["foo"; "variable"; "Bar"];
  assert_wildcard_exports
    ~qualifier:(Reference.create "qualified")
    {|
      def qualified.foo(): ...
      qualified.variable = ...
      class qualified.Bar: ...
    |}
    ["foo"; "variable"; "Bar"];
  assert_in_wildcard_exports
    {|
      def foo(): ...
      variable = ...
      class Bar: ...
    |}
    "foo"
    true


let () =
  "module"
  >::: [ "empty_stub" >:: test_empty_stub;
         "handle" >:: test_handle;
         "aliased_export" >:: test_aliased_export;
         "wildcard_exports" >:: test_wildcard_exports ]
  |> Test.run
