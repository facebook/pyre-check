(** Copyright (c) 2019-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the LICENSE file in the root
    directory of this source tree. *)

open Core
open Ast
open Pyre
open OUnit2
module ModuleTracker = Service.ModuleTracker
module SourceFile = ModuleTracker.SourceFile

let test_creation context =
  let touch root relative =
    Path.create_relative ~root ~relative |> File.create ~content:"" |> File.write
  in
  let create ~configuration root relative =
    let path = Path.create_relative ~root ~relative in
    SourceFile.create ~configuration path
  in
  let create_exn ~configuration root relative =
    match create ~configuration root relative with
    | None ->
        let message =
          Format.asprintf "Failed to create source file %s under %a" relative Path.pp root
        in
        assert_failure message
    | Some result -> result
  in
  let assert_create_fail ~configuration root relative =
    match create ~configuration root relative with
    | None -> ()
    | Some _ ->
        let message =
          Format.asprintf
            "Creating source file %s under %a is supposed to fail"
            relative
            Path.pp
            root
        in
        assert_failure message
  in
  let lookup_exn tracker reference =
    match ModuleTracker.lookup tracker reference with
    | Some source_file -> source_file
    | None ->
        let message =
          Format.asprintf "Cannot find module %a in the module tracker" Reference.pp reference
        in
        assert_failure message
  in
  let assert_source_file
      ?priority
      ?is_stub
      ?is_external
      ?is_init
      ~search_root
      ~relative
      { SourceFile.relative_path = actual_relative_path;
        priority = actual_priority;
        is_stub = actual_is_stub;
        is_external = actual_is_external;
        is_init = actual_is_init
      }
    =
    let expected_path = Path.create_relative ~root:search_root ~relative in
    assert_equal
      ~cmp:Path.equal
      ~printer:Path.show
      expected_path
      (Path.Relative actual_relative_path);
    Option.iter priority ~f:(fun expected_priority ->
        assert_equal ~cmp:Int.equal ~printer:Int.to_string expected_priority actual_priority);
    Option.iter is_stub ~f:(fun expected_is_stub ->
        assert_equal ~cmp:Bool.equal ~printer:Bool.to_string expected_is_stub actual_is_stub);
    Option.iter is_external ~f:(fun expected_is_external ->
        assert_equal
          ~cmp:Bool.equal
          ~printer:Bool.to_string
          expected_is_external
          actual_is_external);
    Option.iter is_init ~f:(fun expected_is_init ->
        assert_equal ~cmp:Bool.equal ~printer:Bool.to_string expected_is_init actual_is_init)
  in
  let assert_same_module_greater left right =
    let left_qualifier = SourceFile.qualifier left in
    let right_qualifier = SourceFile.qualifier right in
    assert_equal ~cmp:Reference.equal ~printer:Reference.show left_qualifier right_qualifier;
    let compare_result = SourceFile.same_module_compare left right in
    let message =
      Format.asprintf
        "\'%a\' is supposed to be greater than \'%a\'"
        Sexp.pp_hum
        (SourceFile.sexp_of_t left)
        Sexp.pp_hum
        (SourceFile.sexp_of_t right)
    in
    assert_bool message (compare_result > 0)
  in
  let test_basic () =
    (* SETUP:
     * local_root/a.py
     * local_root/b.py
     * local_root/c.py
     * local_root/c.pyi
     * local_root/d.py
     * local_root/d/__init__.pyi
     * local_root/e.py
     * external_root/a.py
     * external_root/b.pyi
     * external_root/b/__init__.py
     * external_root/c.py
     * external_root/c.pyi *)
    let local_root = bracket_tmpdir context |> Path.create_absolute in
    let external_root = bracket_tmpdir context |> Path.create_absolute in
    let local_d_path = Path.absolute local_root ^ "/d" in
    let external_b_path = Path.absolute external_root ^ "/b" in
    Sys_utils.mkdir_no_fail local_d_path;
    Sys_utils.mkdir_no_fail external_b_path;
    List.iter ~f:(touch local_root) ["a.py"; "b.py"; "c.py"; "c.pyi"; "d.py"; "e.py"];
    let () =
      let path = local_d_path |> Path.create_absolute in
      touch path "__init__.pyi"
    in
    List.iter ~f:(touch external_root) ["a.py"; "b.pyi"; "c.py"; "c.pyi"];
    let () =
      let path = external_b_path |> Path.create_absolute in
      touch path "__init__.py"
    in
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~excludes:[".*/thereisnospoon.py"]
        ~search_path:[SearchPath.Root external_root]
        ()
    in
    let create_exn = create_exn ~configuration in
    let assert_create_fail = assert_create_fail ~configuration in
    (* Creation test *)
    let local_a = create_exn local_root "a.py" in
    assert_source_file
      local_a
      ~search_root:local_root
      ~relative:"a.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:false;
    let local_b = create_exn local_root "b.py" in
    assert_source_file
      local_b
      ~search_root:local_root
      ~relative:"b.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:false;
    let local_c = create_exn local_root "c.py" in
    assert_source_file
      local_c
      ~search_root:local_root
      ~relative:"c.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:false;
    let local_cstub = create_exn local_root "c.pyi" in
    assert_source_file
      local_cstub
      ~search_root:local_root
      ~relative:"c.pyi"
      ~is_stub:true
      ~is_external:false
      ~is_init:false;
    let local_d = create_exn local_root "d.py" in
    assert_source_file
      local_d
      ~search_root:local_root
      ~relative:"d.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:false;
    let local_dinit = create_exn local_root "d/__init__.pyi" in
    assert_source_file
      local_dinit
      ~search_root:local_root
      ~relative:"d/__init__.pyi"
      ~is_stub:true
      ~is_external:false
      ~is_init:true;
    let local_e = create_exn local_root "e.py" in
    assert_source_file
      local_e
      ~search_root:local_root
      ~relative:"e.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:false;
    let external_a = create_exn external_root "a.py" in
    assert_source_file
      external_a
      ~search_root:external_root
      ~relative:"a.py"
      ~is_stub:false
      ~is_external:true
      ~is_init:false;
    let external_bstub = create_exn external_root "b.pyi" in
    assert_source_file
      external_bstub
      ~search_root:external_root
      ~relative:"b.pyi"
      ~is_stub:true
      ~is_external:true
      ~is_init:false;
    let external_binit = create_exn external_root "b/__init__.py" in
    assert_source_file
      external_binit
      ~search_root:external_root
      ~relative:"b/__init__.py"
      ~is_stub:false
      ~is_external:true
      ~is_init:true;
    let external_c = create_exn external_root "c.py" in
    assert_source_file
      external_c
      ~search_root:external_root
      ~relative:"c.py"
      ~is_stub:false
      ~is_external:true
      ~is_init:false;
    let external_cstub = create_exn external_root "c.pyi" in
    assert_source_file
      external_cstub
      ~search_root:external_root
      ~relative:"c.pyi"
      ~is_stub:true
      ~is_external:true
      ~is_init:false;
    assert_create_fail external_root "thereisnospoon.py";
    assert_create_fail external_root "foo/thereisnospoon.py";

    (* Comparison test *)
    assert_same_module_greater external_a local_a;
    assert_same_module_greater external_bstub local_b;
    assert_same_module_greater external_binit local_b;
    assert_same_module_greater external_bstub external_binit;
    assert_same_module_greater local_cstub local_c;
    assert_same_module_greater external_cstub external_c;
    assert_same_module_greater external_cstub local_cstub;
    assert_same_module_greater external_cstub local_c;
    assert_same_module_greater local_cstub external_c;
    assert_same_module_greater external_c local_c;
    assert_same_module_greater local_dinit local_d;

    (* ModuleTracker initialization test *)
    let tracker = ModuleTracker.create configuration in
    assert_source_file
      (lookup_exn tracker (Reference.create "a"))
      ~search_root:external_root
      ~relative:"a.py"
      ~is_stub:false
      ~is_external:true
      ~is_init:false;
    assert_source_file
      (lookup_exn tracker (Reference.create "b"))
      ~search_root:external_root
      ~relative:"b.pyi"
      ~is_stub:true
      ~is_external:true
      ~is_init:false;
    assert_source_file
      (lookup_exn tracker (Reference.create "c"))
      ~search_root:external_root
      ~relative:"c.pyi"
      ~is_stub:true
      ~is_external:true
      ~is_init:false;
    assert_source_file
      (lookup_exn tracker (Reference.create "d"))
      ~search_root:local_root
      ~relative:"d/__init__.pyi"
      ~is_stub:true
      ~is_external:false
      ~is_init:true;
    assert_source_file
      (lookup_exn tracker (Reference.create "e"))
      ~search_root:local_root
      ~relative:"e.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:false
  in
  let test_priority () =
    let local_root = bracket_tmpdir context |> Path.create_absolute in
    let external_root0 = bracket_tmpdir context |> Path.create_absolute in
    let external_root1 = bracket_tmpdir context |> Path.create_absolute in
    let external_paths0 = ["a.py"; "b.py"; "c.pyi"; "d.py"; "e.pyi"; "f.pyi"; "g.pyi"] in
    List.iter ~f:(touch external_root0) external_paths0;
    let external_paths1 = ["a.py"; "b.py"; "c.py"; "d.pyi"; "e.py"; "f.pyi"; "g.pyi"] in
    List.iter ~f:(touch external_root1) external_paths1;
    let local_paths = ["a.py"; "b.pyi"; "c.py"; "d.py"; "e.pyi"; "f.py"; "g.pyi"] in
    List.iter ~f:(touch local_root) local_paths;
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~search_path:[SearchPath.Root external_root0; SearchPath.Root external_root1]
        ()
    in
    let create_exn = create_exn ~configuration in
    (* Creation test *)
    List.iter local_paths ~f:(fun path ->
        assert_source_file
          (create_exn local_root path)
          ~search_root:local_root
          ~relative:path
          ~is_external:false
          ~is_init:false);
    List.iter external_paths0 ~f:(fun path ->
        assert_source_file
          (create_exn external_root0 path)
          ~search_root:external_root0
          ~relative:path
          ~priority:0
          ~is_external:true
          ~is_init:false);
    List.iter external_paths1 ~f:(fun path ->
        assert_source_file
          (create_exn external_root1 path)
          ~search_root:external_root1
          ~relative:path
          ~priority:1
          ~is_external:true
          ~is_init:false);

    (* ModuleTracker initialization test *)
    let tracker = ModuleTracker.create configuration in
    assert_source_file
      (lookup_exn tracker (Reference.create "a"))
      ~search_root:external_root0
      ~relative:"a.py"
      ~priority:0
      ~is_stub:false
      ~is_external:true;
    assert_source_file
      (lookup_exn tracker (Reference.create "b"))
      ~search_root:local_root
      ~relative:"b.pyi"
      ~is_stub:true
      ~is_external:false;
    assert_source_file
      (lookup_exn tracker (Reference.create "c"))
      ~search_root:external_root0
      ~relative:"c.pyi"
      ~is_stub:true
      ~is_external:true;
    assert_source_file
      (lookup_exn tracker (Reference.create "d"))
      ~search_root:external_root1
      ~relative:"d.pyi"
      ~is_stub:true
      ~is_external:true;
    assert_source_file
      (lookup_exn tracker (Reference.create "e"))
      ~search_root:external_root0
      ~relative:"e.pyi"
      ~is_stub:true
      ~is_external:true;
    assert_source_file
      (lookup_exn tracker (Reference.create "f"))
      ~search_root:external_root0
      ~relative:"f.pyi"
      ~is_stub:true
      ~is_external:true;
    assert_source_file
      (lookup_exn tracker (Reference.create "g"))
      ~search_root:external_root0
      ~relative:"g.pyi"
      ~is_stub:true
      ~is_external:true
  in
  let test_overlapping () =
    (* SETUP: external_root0 lives under local_root external_root1 lives under external_root0
       Module resolution boils down to which root comes first in the search path *)
    let local_root = bracket_tmpdir context |> Path.create_absolute in
    let external_root0_path = Path.absolute local_root ^ "/external0" in
    Sys_utils.mkdir_no_fail external_root0_path;
    let external_root1_path = external_root0_path ^ "/external1" in
    Sys_utils.mkdir_no_fail external_root1_path;
    let external_root0 = Path.create_absolute external_root0_path in
    let external_root1 = Path.create_absolute external_root1_path in
    touch external_root0 "a.py";
    touch external_root1 "a.py";
    touch local_root "a.py";
    touch local_root "b.py";

    let test_external_root_0_before_1 () =
      let configuration =
        Configuration.Analysis.create
          ~local_root
          ~search_path:[SearchPath.Root external_root0; SearchPath.Root external_root1]
          ()
      in
      let create_exn = create_exn ~configuration in
      assert_source_file
        (create_exn local_root "a.py")
        ~search_root:local_root
        ~relative:"a.py"
        ~is_external:false;
      assert_source_file
        (create_exn external_root0 "a.py")
        ~search_root:external_root0
        ~relative:"a.py"
        ~is_external:true;
      assert_source_file
        (create_exn local_root "external0/a.py")
        ~search_root:external_root0
        ~relative:"a.py"
        ~is_external:true;

      (* Resolves to external1.a since external_root0 has higher precedence *)
      assert_source_file
        (create_exn external_root1 "a.py")
        ~search_root:external_root0
        ~relative:"external1/a.py"
        ~is_external:true;
      assert_source_file
        (create_exn external_root0 "external1/a.py")
        ~search_root:external_root0
        ~relative:"external1/a.py"
        ~is_external:true;
      assert_source_file
        (create_exn local_root "external0/external1/a.py")
        ~search_root:external_root0
        ~relative:"external1/a.py"
        ~is_external:true
    in
    let test_external_root_1_before_0 () =
      let configuration =
        Configuration.Analysis.create
          ~local_root
          ~search_path:[SearchPath.Root external_root1; SearchPath.Root external_root0]
          ()
      in
      let create_exn = create_exn ~configuration in
      assert_source_file
        (create_exn local_root "a.py")
        ~search_root:local_root
        ~relative:"a.py"
        ~is_external:false;
      assert_source_file
        (create_exn external_root0 "a.py")
        ~search_root:external_root0
        ~relative:"a.py"
        ~is_external:true;
      assert_source_file
        (create_exn local_root "external0/a.py")
        ~search_root:external_root0
        ~relative:"a.py"
        ~is_external:true;
      assert_source_file
        (create_exn external_root1 "a.py")
        ~search_root:external_root1
        ~relative:"a.py"
        ~is_external:true;
      assert_source_file
        (create_exn external_root0 "external1/a.py")
        ~search_root:external_root1
        ~relative:"a.py"
        ~is_external:true;
      assert_source_file
        (create_exn local_root "external0/external1/a.py")
        ~search_root:external_root1
        ~relative:"a.py"
        ~is_external:true
    in
    test_external_root_0_before_1 ();
    test_external_root_1_before_0 ()
  in
  test_basic ();
  test_priority ();
  test_overlapping ()


let () = "environment" >::: ["creation" >:: test_creation] |> Test.run
