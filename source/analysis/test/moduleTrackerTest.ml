(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open OUnit2
module ModuleTracker = Analysis.ModuleTracker

let touch path = File.create path ~content:"" |> File.write

let create_source_path ~configuration root relative =
  let path = PyrePath.create_relative ~root ~relative in
  SourcePath.create ~configuration path


let create_source_path_exn ~configuration root relative =
  match create_source_path ~configuration root relative with
  | None ->
      let message =
        Format.asprintf "Failed to create source file %s under %a" relative PyrePath.pp root
      in
      assert_failure message
  | Some result -> result


let lookup_exn tracker reference =
  match ModuleTracker.lookup_source_path tracker reference with
  | Some source_path -> source_path
  | None ->
      let message =
        Format.asprintf "Cannot find module %a in the module tracker" Reference.pp reference
      in
      assert_failure message


module ModuleStatus = struct
  type t =
    | Untracked
    | Explicit
    | Implicit
  [@@deriving sexp, compare]
end

let test_creation context =
  let assert_create_fail ~configuration root relative =
    match create_source_path ~configuration root relative with
    | None -> ()
    | Some _ ->
        let message =
          Format.asprintf
            "Creating source file %s under %a is supposed to fail"
            relative
            PyrePath.pp
            root
        in
        assert_failure message
  in
  let assert_source_path
      ?priority
      ?is_stub
      ?is_external
      ?is_init
      ~configuration
      ~search_root
      ~relative
      ({
         SourcePath.priority = actual_priority;
         is_stub = actual_is_stub;
         is_external = actual_is_external;
         is_init = actual_is_init;
         _;
       } as source_path)
    =
    let expected_path = PyrePath.create_relative ~root:search_root ~relative in
    let actual_path = SourcePath.full_path ~configuration source_path in
    assert_equal ~cmp:PyrePath.equal ~printer:PyrePath.show expected_path actual_path;
    Option.iter priority ~f:(fun expected_priority ->
        assert_equal ~cmp:Int.equal ~printer:Int.to_string expected_priority actual_priority);
    Option.iter is_stub ~f:(fun expected_is_stub ->
        assert_equal ~cmp:Bool.equal ~printer:Bool.to_string expected_is_stub actual_is_stub);
    Option.iter is_external ~f:(fun expected_is_external ->
        assert_equal ~cmp:Bool.equal ~printer:Bool.to_string expected_is_external actual_is_external);
    Option.iter is_init ~f:(fun expected_is_init ->
        assert_equal ~cmp:Bool.equal ~printer:Bool.to_string expected_is_init actual_is_init)
  in
  let assert_same_module_greater
      ~configuration
      ({ SourcePath.qualifier = left_qualifier; _ } as left)
      ({ SourcePath.qualifier = right_qualifier; _ } as right)
    =
    assert_equal ~cmp:Reference.equal ~printer:Reference.show left_qualifier right_qualifier;
    let compare_result = SourcePath.same_module_compare ~configuration left right in
    let message =
      Format.asprintf
        "\'%a\' is supposed to be greater than \'%a\'"
        Sexp.pp_hum
        (SourcePath.sexp_of_t left)
        Sexp.pp_hum
        (SourcePath.sexp_of_t right)
    in
    assert_bool message (compare_result > 0)
  in
  let touch root relative = touch (PyrePath.create_relative ~root ~relative) in
  let test_basic () =
    (* SETUP:
     * local_root/a.py
     * local_root/b.py
     * local_root/c.py
     * local_root/c.pyi
     * local_root/d.py
     * local_root/d/__init__.py
     * local_root/e.py
     * external_root/a.py
     * external_root/b.pyi
     * external_root/b/__init__.py
     * external_root/c.py
     * external_root/c.pyi *)
    let local_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let external_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let local_d_path = PyrePath.absolute local_root ^ "/d" in
    let external_b_path = PyrePath.absolute external_root ^ "/b" in
    Sys_utils.mkdir_no_fail local_d_path;
    Sys_utils.mkdir_no_fail external_b_path;
    List.iter ~f:(touch local_root) ["a.py"; "b.py"; "c.py"; "c.pyi"; "d.py"; "e.py"];
    let () =
      let path = local_d_path |> PyrePath.create_absolute ~follow_symbolic_links:true in
      touch path "__init__.py"
    in
    List.iter ~f:(touch external_root) ["a.py"; "b.pyi"; "c.py"; "c.pyi"];
    let () =
      let path = external_b_path |> PyrePath.create_absolute ~follow_symbolic_links:true in
      touch path "__init__.py"
    in
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~source_paths:[SearchPath.Root local_root]
        ~excludes:[".*/thereisnospoon.py"]
        ~search_paths:[SearchPath.Root external_root]
        ~filter_directories:[local_root]
        ~extensions:
          (List.map
             ~f:Configuration.Extension.create_extension
             [".first"; ".second"; ".third"; ".special$include_suffix_in_module_qualifier"])
        ()
    in
    let create_exn = create_source_path_exn ~configuration in
    let assert_source_path = assert_source_path ~configuration in
    let assert_same_module_greater = assert_same_module_greater ~configuration in
    let assert_create_fail = assert_create_fail ~configuration in
    (* Creation test *)
    let local_a = create_exn local_root "a.py" in
    assert_source_path
      local_a
      ~search_root:local_root
      ~relative:"a.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:false;
    let local_b = create_exn local_root "b.py" in
    assert_source_path
      local_b
      ~search_root:local_root
      ~relative:"b.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:false;
    let local_c = create_exn local_root "c.py" in
    assert_source_path
      local_c
      ~search_root:local_root
      ~relative:"c.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:false;
    let local_cstub = create_exn local_root "c.pyi" in
    assert_source_path
      local_cstub
      ~search_root:local_root
      ~relative:"c.pyi"
      ~is_stub:true
      ~is_external:false
      ~is_init:false;
    let local_d = create_exn local_root "d.py" in
    assert_source_path
      local_d
      ~search_root:local_root
      ~relative:"d.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:false;
    let local_dinit = create_exn local_root "d/__init__.py" in
    assert_source_path
      local_dinit
      ~search_root:local_root
      ~relative:"d/__init__.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:true;
    let local_e = create_exn local_root "e.py" in
    assert_source_path
      local_e
      ~search_root:local_root
      ~relative:"e.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:false;
    let local_f = create_exn local_root "f.special" in
    assert_source_path
      local_f
      ~search_root:local_root
      ~relative:"f.special"
      ~is_stub:false
      ~is_external:false
      ~is_init:false;
    let external_a = create_exn external_root "a.py" in
    assert_source_path
      external_a
      ~search_root:external_root
      ~relative:"a.py"
      ~is_stub:false
      ~is_external:true
      ~is_init:false;
    let external_bstub = create_exn external_root "b.pyi" in
    assert_source_path
      external_bstub
      ~search_root:external_root
      ~relative:"b.pyi"
      ~is_stub:true
      ~is_external:true
      ~is_init:false;
    let external_binit = create_exn external_root "b/__init__.py" in
    assert_source_path
      external_binit
      ~search_root:external_root
      ~relative:"b/__init__.py"
      ~is_stub:false
      ~is_external:true
      ~is_init:true;
    let external_c = create_exn external_root "c.py" in
    assert_source_path
      external_c
      ~search_root:external_root
      ~relative:"c.py"
      ~is_stub:false
      ~is_external:true
      ~is_init:false;
    let external_cstub = create_exn external_root "c.pyi" in
    assert_source_path
      external_cstub
      ~search_root:external_root
      ~relative:"c.pyi"
      ~is_stub:true
      ~is_external:true
      ~is_init:false;
    assert_create_fail external_root "thereisnospoon.py";
    assert_create_fail external_root "foo/thereisnospoon.py";
    assert_create_fail local_root "untracked_extension.txt";
    assert_create_fail external_root "untracked_extension.txt";
    let extension_first = create_exn local_root "dir/a.first" in
    let extension_second = create_exn local_root "dir/a.second" in
    let extension_third = create_exn local_root "dir/a.third" in
    let extension_py = create_exn local_root "dir/a.py" in
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
    assert_same_module_greater extension_first extension_second;
    assert_same_module_greater extension_first extension_third;
    assert_same_module_greater extension_py extension_first;

    (* ModuleTracker initialization test *)
    let tracker = ModuleTracker.create configuration in
    assert_source_path
      (lookup_exn tracker (Reference.create "a"))
      ~search_root:external_root
      ~relative:"a.py"
      ~is_stub:false
      ~is_external:true
      ~is_init:false;
    assert_source_path
      (lookup_exn tracker (Reference.create "b"))
      ~search_root:external_root
      ~relative:"b.pyi"
      ~is_stub:true
      ~is_external:true
      ~is_init:false;
    assert_source_path
      (lookup_exn tracker (Reference.create "c"))
      ~search_root:external_root
      ~relative:"c.pyi"
      ~is_stub:true
      ~is_external:true
      ~is_init:false;
    assert_source_path
      (lookup_exn tracker (Reference.create "d"))
      ~search_root:local_root
      ~relative:"d/__init__.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:true;
    assert_source_path
      (lookup_exn tracker (Reference.create "e"))
      ~search_root:local_root
      ~relative:"e.py"
      ~is_stub:false
      ~is_external:false
      ~is_init:false
  in
  let test_submodules () =
    (* SETUP:
     * local_root/a.py
     * local_root/b/c.py
     * local_root/d/__init__.py
     * local_root/d/e/f.py
     * external_root/d/g.py
     * external_root/h/i/j/__init__.pyi
     *)
    let local_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let external_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    List.iter
      ~f:Sys_utils.mkdir_no_fail
      [
        PyrePath.absolute local_root ^ "/b";
        PyrePath.absolute local_root ^ "/d";
        PyrePath.absolute local_root ^ "/d/e";
        PyrePath.absolute external_root ^ "/d";
        PyrePath.absolute external_root ^ "/h";
        PyrePath.absolute external_root ^ "/h/i";
        PyrePath.absolute external_root ^ "/h/i/j";
      ];
    List.iter ~f:(touch local_root) ["a.py"; "b/c.py"; "d/__init__.py"; "d/e/f.py"];
    List.iter ~f:(touch external_root) ["d/g.py"; "h/i/j/__init__.pyi"];
    let module_tracker =
      Configuration.Analysis.create
        ~local_root
        ~source_paths:[SearchPath.Root local_root]
        ~search_paths:[SearchPath.Root external_root]
        ~filter_directories:[local_root]
        ()
      |> ModuleTracker.create
    in
    let assert_module ~expected qualifier =
      let actual =
        match ModuleTracker.lookup module_tracker qualifier with
        | None -> ModuleStatus.Untracked
        | Some (ModuleTracker.ModuleLookup.Explicit _) -> ModuleStatus.Explicit
        | Some (ModuleTracker.ModuleLookup.Implicit _) -> ModuleStatus.Implicit
      in
      assert_equal
        ~cmp:[%compare.equal: ModuleStatus.t]
        ~printer:(fun status -> ModuleStatus.sexp_of_t status |> Sexp.to_string_hum)
        expected
        actual;

      (* Also make sure `is_module_tracked` result is sensible *)
      let expected_is_tracked =
        match expected with
        | ModuleStatus.Untracked -> false
        | _ -> true
      in
      let actual_is_tracked = ModuleTracker.is_module_tracked module_tracker qualifier in
      assert_equal ~cmp:Bool.equal ~printer:Bool.to_string expected_is_tracked actual_is_tracked
    in
    let open Test in
    assert_module !&"a" ~expected:ModuleStatus.Explicit;
    assert_module !&"a.b" ~expected:ModuleStatus.Untracked;
    assert_module !&"b" ~expected:ModuleStatus.Implicit;
    assert_module !&"b.c" ~expected:ModuleStatus.Explicit;
    assert_module !&"b.d" ~expected:ModuleStatus.Untracked;
    assert_module !&"d" ~expected:ModuleStatus.Explicit;
    assert_module !&"d.e" ~expected:ModuleStatus.Implicit;
    assert_module !&"d.g" ~expected:ModuleStatus.Explicit;
    assert_module !&"d.e.f" ~expected:ModuleStatus.Explicit;
    assert_module !&"d.e.g" ~expected:ModuleStatus.Untracked;
    assert_module !&"h" ~expected:ModuleStatus.Implicit;
    assert_module !&"h.i" ~expected:ModuleStatus.Implicit;
    assert_module !&"h.i.j" ~expected:ModuleStatus.Explicit;
    assert_module !&"h.i.j.k" ~expected:ModuleStatus.Untracked
  in
  let test_search_path_subdirectory () =
    let local_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let search_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let search_subdirectory_path = PyrePath.absolute search_root ^ "/sub" in
    Sys_utils.mkdir_no_fail search_subdirectory_path;
    let search_subdirectory =
      PyrePath.create_absolute ~follow_symbolic_links:true search_subdirectory_path
    in
    touch local_root "a.py";
    touch search_root "b.py";
    touch search_subdirectory "c.py";
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~source_paths:[SearchPath.Root local_root]
        ~search_paths:[SearchPath.Subdirectory { root = search_root; subdirectory = "sub" }]
        ~filter_directories:[local_root]
        ()
    in
    let assert_path = assert_equal ~cmp:PyrePath.equal ~printer:PyrePath.show in
    assert_create_fail ~configuration search_root "b.py";
    let source_path_a = create_source_path_exn ~configuration local_root "a.py" in
    assert_path
      (PyrePath.create_relative ~root:local_root ~relative:"a.py")
      (SourcePath.full_path ~configuration source_path_a);
    let source_path_b = create_source_path_exn ~configuration search_subdirectory "c.py" in
    assert_path
      (PyrePath.create_relative ~root:search_subdirectory ~relative:"c.py")
      (SourcePath.full_path ~configuration source_path_b)
  in
  let test_priority () =
    let local_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let external_root0 =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let external_root1 =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let external_paths0 = ["a.py"; "b.py"; "c.pyi"; "d.py"; "e.pyi"; "f.pyi"; "g.pyi"] in
    List.iter ~f:(touch external_root0) external_paths0;
    let external_paths1 = ["a.py"; "b.py"; "c.py"; "d.pyi"; "e.py"; "f.pyi"; "g.pyi"] in
    List.iter ~f:(touch external_root1) external_paths1;
    let local_paths = ["a.py"; "b.pyi"; "c.py"; "d.py"; "e.pyi"; "f.py"; "g.pyi"] in
    List.iter ~f:(touch local_root) local_paths;
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~source_paths:[SearchPath.Root local_root]
        ~search_paths:[SearchPath.Root external_root0; SearchPath.Root external_root1]
        ~filter_directories:[local_root]
        ()
    in
    let create_exn = create_source_path_exn ~configuration in
    let assert_source_path = assert_source_path ~configuration in
    (* Creation test *)
    List.iter local_paths ~f:(fun path ->
        assert_source_path
          (create_exn local_root path)
          ~search_root:local_root
          ~relative:path
          ~is_external:false
          ~is_init:false);
    List.iter external_paths0 ~f:(fun path ->
        assert_source_path
          (create_exn external_root0 path)
          ~search_root:external_root0
          ~relative:path
          ~priority:0
          ~is_external:true
          ~is_init:false);
    List.iter external_paths1 ~f:(fun path ->
        assert_source_path
          (create_exn external_root1 path)
          ~search_root:external_root1
          ~relative:path
          ~priority:1
          ~is_external:true
          ~is_init:false);

    (* ModuleTracker initialization test *)
    let tracker = ModuleTracker.create configuration in
    assert_source_path
      (lookup_exn tracker (Reference.create "a"))
      ~search_root:external_root0
      ~relative:"a.py"
      ~priority:0
      ~is_stub:false
      ~is_external:true;
    assert_source_path
      (lookup_exn tracker (Reference.create "b"))
      ~search_root:local_root
      ~relative:"b.pyi"
      ~is_stub:true
      ~is_external:false;
    assert_source_path
      (lookup_exn tracker (Reference.create "c"))
      ~search_root:external_root0
      ~relative:"c.pyi"
      ~is_stub:true
      ~is_external:true;
    assert_source_path
      (lookup_exn tracker (Reference.create "d"))
      ~search_root:external_root1
      ~relative:"d.pyi"
      ~is_stub:true
      ~is_external:true;
    assert_source_path
      (lookup_exn tracker (Reference.create "e"))
      ~search_root:external_root0
      ~relative:"e.pyi"
      ~is_stub:true
      ~is_external:true;
    assert_source_path
      (lookup_exn tracker (Reference.create "f"))
      ~search_root:external_root0
      ~relative:"f.pyi"
      ~is_stub:true
      ~is_external:true;
    assert_source_path
      (lookup_exn tracker (Reference.create "g"))
      ~search_root:external_root0
      ~relative:"g.pyi"
      ~is_stub:true
      ~is_external:true
  in
  let test_priority_multi_source_paths () =
    let local_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let source_root0_path = PyrePath.absolute local_root ^ "/source0" in
    Sys_utils.mkdir_no_fail source_root0_path;
    let source_root0 = PyrePath.create_absolute source_root0_path in
    let source_root1_path = PyrePath.absolute local_root ^ "/source1" in
    Sys_utils.mkdir_no_fail source_root1_path;
    let source_root1 = PyrePath.create_absolute source_root1_path in
    let source_paths0 = ["a.py"; "b.py"; "c.pyi"; "d.py"; "e.pyi"; "f.pyi"; "g.pyi"] in
    List.iter ~f:(touch source_root0) source_paths0;
    let source_paths1 = ["a.py"; "b.py"; "c.py"; "d.pyi"; "e.py"; "f.pyi"; "g.pyi"] in
    List.iter ~f:(touch source_root1) source_paths1;
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~source_paths:[SearchPath.Root source_root0; SearchPath.Root source_root1]
        ~filter_directories:[local_root]
        ()
    in
    let create_exn = create_source_path_exn ~configuration in
    let assert_source_path = assert_source_path ~configuration in
    (* Creation test *)
    List.iter source_paths0 ~f:(fun path ->
        assert_source_path
          (create_exn source_root0 path)
          ~search_root:source_root0
          ~relative:path
          ~priority:0
          ~is_external:false
          ~is_init:false);
    List.iter source_paths1 ~f:(fun path ->
        assert_source_path
          (create_exn source_root1 path)
          ~search_root:source_root1
          ~relative:path
          ~priority:1
          ~is_external:false
          ~is_init:false);

    (* ModuleTracker initialization test *)
    let tracker = ModuleTracker.create configuration in
    assert_source_path
      (lookup_exn tracker (Reference.create "a"))
      ~search_root:source_root0
      ~relative:"a.py"
      ~priority:0
      ~is_stub:false
      ~is_external:false;
    assert_source_path
      (lookup_exn tracker (Reference.create "c"))
      ~search_root:source_root0
      ~relative:"c.pyi"
      ~is_stub:true
      ~is_external:false;
    assert_source_path
      (lookup_exn tracker (Reference.create "d"))
      ~search_root:source_root1
      ~relative:"d.pyi"
      ~is_stub:true
      ~is_external:false;
    assert_source_path
      (lookup_exn tracker (Reference.create "e"))
      ~search_root:source_root0
      ~relative:"e.pyi"
      ~is_stub:true
      ~is_external:false;
    assert_source_path
      (lookup_exn tracker (Reference.create "f"))
      ~search_root:source_root0
      ~relative:"f.pyi"
      ~is_stub:true
      ~is_external:false;
    assert_source_path
      (lookup_exn tracker (Reference.create "g"))
      ~search_root:source_root0
      ~relative:"g.pyi"
      ~is_stub:true
      ~is_external:false
  in
  let test_exclude () =
    (* Test that ${SOURCE_DIRECTORY} gets correctly replaced *)
    let local_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let external_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    touch local_root "foo.py";
    touch local_root "bar.py";
    touch local_root "baz.py";
    touch external_root "foo.py";
    touch external_root "bar.py";
    touch external_root "baz.py";

    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~source_paths:[SearchPath.Root local_root]
        ~search_paths:[SearchPath.Root external_root]
        ~excludes:["${SOURCE_DIRECTORY}/ba.*"]
        ()
    in
    let create_exn = create_source_path_exn ~configuration in
    let assert_source_path = assert_source_path ~configuration in
    let assert_create_fail = assert_create_fail ~configuration in
    assert_source_path (create_exn local_root "foo.py") ~search_root:local_root ~relative:"foo.py";
    assert_create_fail local_root "bar.py";
    assert_create_fail local_root "baz.py";
    assert_source_path
      (create_exn external_root "foo.py")
      ~search_root:external_root
      ~relative:"foo.py";
    assert_source_path
      (create_exn external_root "bar.py")
      ~search_root:external_root
      ~relative:"bar.py";
    assert_source_path
      (create_exn external_root "baz.py")
      ~search_root:external_root
      ~relative:"baz.py"
  in
  let test_directory_filter () =
    (* SETUP:
     * - all_root is the parent of both local_root and external_root
     * - local_root is the local root
     * - search_root is the root of other search paths
     * - both derp and durp lives under search_root
     * - search_root is allowlisted with filter_directories and durp is denylisted with ignore_all_errors
     * We want to make sure that the is_external field is correct for this setup. *)
    let local_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let search_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let derp_path = PyrePath.absolute search_root ^ "/derp" in
    Sys_utils.mkdir_no_fail derp_path;
    let durp_path = PyrePath.absolute search_root ^ "/durp" in
    Sys_utils.mkdir_no_fail durp_path;
    let derp = PyrePath.create_absolute ~follow_symbolic_links:true derp_path in
    let durp = PyrePath.create_absolute ~follow_symbolic_links:true durp_path in
    touch local_root "a.py";
    touch search_root "b.py";
    touch derp "c.py";
    touch durp "d.py";
    touch local_root "e.py";

    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~source_paths:[SearchPath.Root local_root]
        ~search_paths:[SearchPath.Root search_root]
        ~filter_directories:[search_root]
        ~ignore_all_errors:[durp; PyrePath.create_relative ~root:local_root ~relative:"e.py"]
        ()
    in
    let assert_source_path = assert_source_path ~configuration in
    let create_exn = create_source_path_exn ~configuration in
    assert_source_path
      (create_exn local_root "a.py")
      ~search_root:local_root
      ~relative:"a.py"
      ~is_external:true;
    assert_source_path
      (create_exn search_root "b.py")
      ~search_root
      ~relative:"b.py"
      ~is_external:false;
    assert_source_path
      (create_exn search_root "derp/c.py")
      ~search_root
      ~relative:"derp/c.py"
      ~is_external:false;
    assert_source_path
      (create_exn search_root "durp/d.py")
      ~search_root
      ~relative:"durp/d.py"
      ~is_external:true;
    assert_source_path
      (create_exn local_root "e.py")
      ~search_root:local_root
      ~relative:"e.py"
      ~is_external:true
  in
  let test_directory_filter2 () =
    (* SETUP:
     * - local_root is the local root
     * - search_root is the root of other search paths
     * We want to test the case when `ignore_all_errors` contains nonexistent directories. *)
    let local_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let search_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let nonexist_root = PyrePath.create_absolute "/whosyourdaddy" in
    assert (not (PyrePath.file_exists nonexist_root));

    touch local_root "a.py";
    touch search_root "b.py";
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~source_paths:[SearchPath.Root local_root]
        ~search_paths:[SearchPath.Root search_root]
        ~filter_directories:[local_root]
        ~ignore_all_errors:[search_root; nonexist_root]
        ()
    in
    let create_exn = create_source_path_exn ~configuration in
    assert_source_path
      (create_exn local_root "a.py")
      ~configuration
      ~search_root:local_root
      ~relative:"a.py"
      ~is_external:false;
    assert_source_path
      (create_exn search_root "b.py")
      ~configuration
      ~search_root
      ~relative:"b.py"
      ~is_external:true
  in
  let test_directory_filter3 () =
    (* We want test that filter_directories follows symlinks *)
    let local_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let search_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let link_local_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let link_search_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    touch local_root "a.py";
    touch search_root "b.py";
    Unix.symlink
      ~link_name:
        (PyrePath.create_relative ~root:link_local_root ~relative:"a.py" |> PyrePath.absolute)
      ~target:(PyrePath.create_relative ~root:local_root ~relative:"a.py" |> PyrePath.absolute);
    Unix.symlink
      ~link_name:
        (PyrePath.create_relative ~root:link_search_root ~relative:"b.py" |> PyrePath.absolute)
      ~target:(PyrePath.create_relative ~root:search_root ~relative:"b.py" |> PyrePath.absolute);

    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~source_paths:[SearchPath.Root local_root]
        ~search_paths:[SearchPath.Root link_search_root; SearchPath.Root link_local_root]
        ~filter_directories:[local_root]
        ~ignore_all_errors:[search_root]
        ()
    in
    let create_exn = create_source_path_exn ~configuration in
    assert_source_path
      (create_exn link_local_root "a.py")
      ~configuration
      ~search_root:link_local_root
      ~relative:"a.py"
      ~is_external:false;
    assert_source_path
      (create_exn link_search_root "b.py")
      ~configuration
      ~search_root:link_search_root
      ~relative:"b.py"
      ~is_external:true
  in
  let test_overlapping () =
    (* SETUP:
     * - external_root0 lives under local_root
     * - external_root1 lives under external_root0
     * Module resolution boils down to which root comes first in the search path *)
    let local_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let external_root0_path = PyrePath.absolute local_root ^ "/external0" in
    Sys_utils.mkdir_no_fail external_root0_path;
    let external_root1_path = external_root0_path ^ "/external1" in
    Sys_utils.mkdir_no_fail external_root1_path;
    let external_root0 = PyrePath.create_absolute ~follow_symbolic_links:true external_root0_path in
    let external_root1 = PyrePath.create_absolute ~follow_symbolic_links:true external_root1_path in
    touch external_root0 "a.py";
    touch external_root1 "a.py";
    touch local_root "a.py";
    touch local_root "b.py";

    let test_external_root_0_before_1 () =
      let configuration =
        Configuration.Analysis.create
          ~local_root
          ~source_paths:[SearchPath.Root local_root]
          ~search_paths:[SearchPath.Root external_root0; SearchPath.Root external_root1]
          ~filter_directories:[local_root]
          ~ignore_all_errors:[external_root0; external_root1]
          ()
      in
      let create_exn = create_source_path_exn ~configuration in
      let assert_source_path = assert_source_path ~configuration in
      assert_source_path
        (create_exn local_root "a.py")
        ~search_root:local_root
        ~relative:"a.py"
        ~is_external:false;
      assert_source_path
        (create_exn external_root0 "a.py")
        ~search_root:external_root0
        ~relative:"a.py"
        ~is_external:true;
      assert_source_path
        (create_exn local_root "external0/a.py")
        ~search_root:external_root0
        ~relative:"a.py"
        ~is_external:true;

      (* Resolves to external1.a since external_root0 has higher precedence *)
      assert_source_path
        (create_exn external_root1 "a.py")
        ~search_root:external_root0
        ~relative:"external1/a.py"
        ~is_external:true;
      assert_source_path
        (create_exn external_root0 "external1/a.py")
        ~search_root:external_root0
        ~relative:"external1/a.py"
        ~is_external:true;
      assert_source_path
        (create_exn local_root "external0/external1/a.py")
        ~search_root:external_root0
        ~relative:"external1/a.py"
        ~is_external:true
    in
    let test_external_root_1_before_0 () =
      let configuration =
        Configuration.Analysis.create
          ~local_root
          ~source_paths:[SearchPath.Root local_root]
          ~search_paths:[SearchPath.Root external_root1; SearchPath.Root external_root0]
          ~filter_directories:[local_root]
          ~ignore_all_errors:[external_root0; external_root1]
          ()
      in
      let create_exn = create_source_path_exn ~configuration in
      let assert_source_path = assert_source_path ~configuration in
      assert_source_path
        (create_exn local_root "a.py")
        ~search_root:local_root
        ~relative:"a.py"
        ~is_external:false;
      assert_source_path
        (create_exn external_root0 "a.py")
        ~search_root:external_root0
        ~relative:"a.py"
        ~is_external:true;
      assert_source_path
        (create_exn local_root "external0/a.py")
        ~search_root:external_root0
        ~relative:"a.py"
        ~is_external:true;
      assert_source_path
        (create_exn external_root1 "a.py")
        ~search_root:external_root1
        ~relative:"a.py"
        ~is_external:true;
      assert_source_path
        (create_exn external_root0 "external1/a.py")
        ~search_root:external_root1
        ~relative:"a.py"
        ~is_external:true;
      assert_source_path
        (create_exn local_root "external0/external1/a.py")
        ~search_root:external_root1
        ~relative:"a.py"
        ~is_external:true
    in
    let test_local_root_rules_it_all () =
      let configuration =
        Configuration.Analysis.create
          ~local_root
          ~source_paths:
            [
              SearchPath.Root local_root;
              SearchPath.Root external_root0;
              SearchPath.Root external_root1;
            ]
          ~filter_directories:[local_root; external_root0; external_root1]
          ()
      in
      let create_exn = create_source_path_exn ~configuration in
      let assert_source_path = assert_source_path ~configuration in
      assert_source_path
        (create_exn local_root "a.py")
        ~search_root:local_root
        ~relative:"a.py"
        ~is_external:false;
      assert_source_path
        (create_exn external_root0 "a.py")
        ~search_root:local_root
        ~relative:"external0/a.py"
        ~is_external:false;
      assert_source_path
        (create_exn local_root "external0/a.py")
        ~search_root:local_root
        ~relative:"external0/a.py"
        ~is_external:false;
      assert_source_path
        (create_exn external_root1 "a.py")
        ~search_root:local_root
        ~relative:"external0/external1/a.py"
        ~is_external:false;
      assert_source_path
        (create_exn external_root0 "external1/a.py")
        ~search_root:local_root
        ~relative:"external0/external1/a.py"
        ~is_external:false;
      assert_source_path
        (create_exn local_root "external0/external1/a.py")
        ~search_root:local_root
        ~relative:"external0/external1/a.py"
        ~is_external:false
    in
    test_external_root_0_before_1 ();
    test_external_root_1_before_0 ();
    test_local_root_rules_it_all ()
  in
  let test_overlapping2 () =
    (* SETUP:
     * - stubs_root lives under local_root (project-internal stubs)
     * - venv_root lives outside local_root (project-external stubs)
     *)
    let local_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let stubs_path = PyrePath.absolute local_root ^ "/stubs" in
    Sys_utils.mkdir_no_fail stubs_path;
    let stubs_root = PyrePath.create_absolute ~follow_symbolic_links:true stubs_path in
    let venv_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    touch local_root "a.py";
    touch local_root "b.pyi";
    touch local_root "c.pyi";
    touch stubs_root "a.pyi";
    touch venv_root "a.pyi";
    touch venv_root "b.pyi";
    touch venv_root "c.py";
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~source_paths:[SearchPath.Root local_root]
        ~search_paths:[SearchPath.Root stubs_root; SearchPath.Root venv_root]
        ~filter_directories:[local_root]
        ()
    in
    let create_exn = create_source_path_exn ~configuration in
    let assert_source_path = assert_source_path ~configuration in
    let assert_same_module_greater = assert_same_module_greater ~configuration in
    assert_source_path
      (create_exn local_root "a.py")
      ~search_root:local_root
      ~is_stub:false
      ~relative:"a.py"
      ~is_external:false;
    assert_source_path
      (create_exn stubs_root "a.pyi")
      ~search_root:stubs_root
      ~relative:"a.pyi"
      ~is_stub:true
      ~is_external:false;
    assert_source_path
      (create_exn venv_root "a.pyi")
      ~search_root:venv_root
      ~relative:"a.pyi"
      ~is_stub:true
      ~is_external:true;
    assert_source_path
      (create_exn venv_root "b.pyi")
      ~search_root:venv_root
      ~relative:"b.pyi"
      ~is_stub:true
      ~is_external:true;
    assert_source_path
      (create_exn local_root "b.pyi")
      ~search_root:local_root
      ~relative:"b.pyi"
      ~is_stub:true
      ~is_external:false;
    assert_source_path
      (create_exn venv_root "c.py")
      ~search_root:venv_root
      ~relative:"c.py"
      ~is_stub:false
      ~is_external:true;
    assert_source_path
      (create_exn local_root "c.pyi")
      ~search_root:local_root
      ~relative:"c.pyi"
      ~is_stub:true
      ~is_external:false;

    assert_same_module_greater (create_exn stubs_root "a.pyi") (create_exn venv_root "a.pyi");
    assert_same_module_greater (create_exn stubs_root "a.pyi") (create_exn local_root "a.py");
    assert_same_module_greater (create_exn venv_root "a.pyi") (create_exn local_root "a.py");
    assert_same_module_greater (create_exn venv_root "b.pyi") (create_exn local_root "b.pyi");
    assert_same_module_greater (create_exn local_root "c.pyi") (create_exn venv_root "c.py")
  in
  let test_root_independence () =
    (* We want to test that `ModuleTracker` creation is independent of where the root is located at. *)
    let local_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let external_root0 =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let external_root1 =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let local_root_copy =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let external_root0_copy =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let external_root1_copy =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    let setup local_root external_root0 external_root1 =
      touch local_root "a.py";
      touch local_root "b.pyi";
      touch external_root0 "c.py";
      touch external_root0 "d.pyi";
      touch external_root1 "e.py";
      touch external_root1 "f.pyi";
      Configuration.Analysis.create
        ~local_root
        ~source_paths:[SearchPath.Root local_root]
        ~search_paths:[SearchPath.Root external_root0; SearchPath.Root external_root1]
        ~filter_directories:[local_root; external_root0]
        ()
      |> ModuleTracker.create
      |> ModuleTracker.all_source_paths
      |> List.sort ~compare:SourcePath.compare
    in
    let source_paths_original = setup local_root external_root0 external_root1 in
    let source_paths_copy = setup local_root_copy external_root0_copy external_root1_copy in
    assert_equal
      ~cmp:(List.equal SourcePath.equal)
      ~printer:(List.to_string ~f:(Format.asprintf "%a" SourcePath.pp))
      source_paths_original
      source_paths_copy
  in
  let test_hidden_files () =
    let local_root =
      bracket_tmpdir context |> PyrePath.create_absolute ~follow_symbolic_links:true
    in
    List.iter ~f:Sys_utils.mkdir_no_fail [PyrePath.absolute local_root ^ "/b"];
    List.iter ~f:(touch local_root) [".a.py"; ".b/c.py"];
    let module_tracker =
      Configuration.Analysis.create
        ~local_root
        ~source_paths:[SearchPath.Root local_root]
        ~filter_directories:[local_root]
        ()
      |> ModuleTracker.create
    in
    assert_equal
      ~cmp:Int.equal
      ~printer:Int.to_string
      0
      (ModuleTracker.explicit_module_count module_tracker)
  in
  test_basic ();
  test_submodules ();
  test_search_path_subdirectory ();
  test_exclude ();
  test_directory_filter ();
  test_directory_filter2 ();
  test_directory_filter3 ();
  test_priority ();
  test_priority_multi_source_paths ();
  test_overlapping ();
  test_overlapping2 ();
  test_root_independence ();
  test_hidden_files ()


module IncrementalTest = struct
  module FileOperation = struct
    type t =
      | Add
      | Update
      | Remove
      | LeftAlone
  end

  module Event = struct
    type t =
      | NewExplicit of {
          relative: string;
          is_external: bool;
        }
      | NewImplicit of string
      | Delete of string
    [@@deriving sexp, compare]

    let create_new_explicit ?(is_external = false) relative = NewExplicit { relative; is_external }

    let equal = [%compare.equal: t]
  end

  type t = {
    handle: string;
    operation: FileOperation.t;
  }

  open Test

  let assert_incremental ?(external_setups = []) ~context ~expected setups =
    let get_old_inputs setups =
      List.filter_map setups ~f:(fun { handle; operation } ->
          match operation with
          | FileOperation.Add -> None
          | _ -> Some (handle, ""))
    in
    let update_filesystem_state { Configuration.Analysis.local_root; search_paths; _ } =
      let update_file ~root { handle; operation } =
        let path = PyrePath.create_relative ~root ~relative:handle in
        match operation with
        | Add
        | Update ->
            (* A file is added/updated *)
            File.create path ~content:"" |> File.write;
            Some path
        | Remove ->
            (* A file is removed *)
            PyrePath.remove path;
            Some path
        | LeftAlone -> None
      in
      let paths = List.filter_map setups ~f:(update_file ~root:local_root) in
      let external_paths =
        let external_root = List.hd_exn search_paths |> SearchPath.get_root in
        List.filter_map external_setups ~f:(update_file ~root:external_root)
      in
      List.append external_paths paths
    in
    (* Set up the initial project *)
    let configuration, module_tracker =
      let old_external_sources = get_old_inputs external_setups in
      let old_sources = get_old_inputs setups in
      let { ScratchProject.configuration; module_tracker; _ } =
        ScratchProject.setup ~context ~external_sources:old_external_sources old_sources
      in
      configuration, module_tracker
    in
    (* Compute the updates *)
    let paths = update_filesystem_state configuration in
    let updates = ModuleTracker.update ~configuration ~paths module_tracker in
    let actual =
      let create_event = function
        | ModuleTracker.IncrementalUpdate.NewExplicit { SourcePath.relative; is_external; _ } ->
            Event.NewExplicit { relative; is_external }
        | ModuleTracker.IncrementalUpdate.NewImplicit qualifier ->
            Event.NewImplicit (Reference.show qualifier)
        | ModuleTracker.IncrementalUpdate.Delete qualifier ->
            Event.Delete (Reference.show qualifier)
      in
      List.map updates ~f:create_event
    in
    (* Check that the computed incremental update is expected *)
    assert_equal
      ~cmp:(List.equal Event.equal)
      ~printer:(fun events -> [%message (events : Event.t list)] |> Sexp.to_string_hum)
      (List.sort ~compare:Event.compare expected)
      (List.sort ~compare:Event.compare actual);

    (* Also check that the module tracker is in a consistent state: we should track exactly the same
       modules and source files after the update as if we build a fresh module tracker from scratch. *)
    let actual_source_paths =
      ModuleTracker.all_source_paths module_tracker |> List.sort ~compare:SourcePath.compare
    in
    let expected_source_paths =
      ModuleTracker.create configuration
      |> ModuleTracker.all_source_paths
      |> List.sort ~compare:SourcePath.compare
    in
    assert_equal
      ~cmp:(List.equal SourcePath.equal)
      ~printer:(fun source_paths ->
        [%message (source_paths : SourcePath.t list)] |> Sexp.to_string_hum)
      expected_source_paths
      actual_source_paths
end

let test_update context =
  let open IncrementalTest in
  let assert_incremental = assert_incremental ~context in
  (* Baseline: no update *)
  assert_incremental
    [{ handle = "a.py"; operation = FileOperation.LeftAlone }]
    ~external_setups:[{ handle = "b.py"; operation = FileOperation.LeftAlone }]
    ~expected:[];

  (* Adding new file for a new module *)
  assert_incremental
    [{ handle = "a.py"; operation = FileOperation.Add }]
    ~expected:[Event.create_new_explicit "a.py"];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.Add };
      { handle = "a.pyi"; operation = FileOperation.Add };
    ]
    ~expected:[Event.create_new_explicit "a.pyi"];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.Update };
      { handle = "a.pyi"; operation = FileOperation.Add };
    ]
    ~expected:[Event.create_new_explicit "a.pyi"];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.Add };
      { handle = "b.thrift"; operation = FileOperation.Add };
    ]
    ~expected:[Event.create_new_explicit "a.py"];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.LeftAlone };
      { handle = "a.thrift"; operation = FileOperation.Add };
    ]
    ~expected:[];

  (* Adding new shadowing file for an existing module *)
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.LeftAlone };
      { handle = "a.pyi"; operation = FileOperation.Add };
    ]
    ~expected:[Event.create_new_explicit "a.pyi"];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.LeftAlone };
      { handle = "a.pyi"; operation = FileOperation.Add };
    ]
    ~external_setups:[{ handle = "a.pyi"; operation = FileOperation.Add }]
    ~expected:[Event.create_new_explicit ~is_external:true "a.pyi"];
  assert_incremental
    [{ handle = "a.py"; operation = FileOperation.LeftAlone }]
    ~external_setups:
      [
        { handle = "a.py"; operation = FileOperation.Add };
        { handle = "a.pyi"; operation = FileOperation.LeftAlone };
        { handle = "a/__init__.pyi"; operation = FileOperation.Add };
      ]
    ~expected:[Event.create_new_explicit ~is_external:true "a/__init__.pyi"];

  (* Adding new shadowed file for an existing module *)
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.LeftAlone };
      { handle = "a/__init__.py"; operation = FileOperation.Add };
    ]
    ~external_setups:[{ handle = "a.pyi"; operation = FileOperation.LeftAlone }]
    ~expected:[];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.LeftAlone };
      { handle = "a.pyi"; operation = FileOperation.Add };
    ]
    ~external_setups:
      [
        { handle = "a.pyi"; operation = FileOperation.LeftAlone };
        { handle = "a.py"; operation = FileOperation.Add };
      ]
    ~expected:[];

  (* Removing a module *)
  assert_incremental
    [{ handle = "a.py"; operation = FileOperation.Remove }]
    ~expected:[Event.Delete "a"];
  assert_incremental
    []
    ~external_setups:[{ handle = "a.py"; operation = FileOperation.Remove }]
    ~expected:[Event.Delete "a"];
  assert_incremental
    [{ handle = "a.py"; operation = FileOperation.Remove }]
    ~external_setups:[{ handle = "a.pyi"; operation = FileOperation.Remove }]
    ~expected:[Event.Delete "a"];
  assert_incremental
    [{ handle = "a.py"; operation = FileOperation.Remove }]
    ~external_setups:
      [
        { handle = "a.pyi"; operation = FileOperation.Remove };
        { handle = "a/__init__.pyi"; operation = FileOperation.Remove };
      ]
    ~expected:[Event.Delete "a"];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.Remove };
      { handle = "a.thrift"; operation = FileOperation.Remove };
    ]
    ~expected:[Event.Delete "a"];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.LeftAlone };
      { handle = "b.thrift"; operation = FileOperation.Remove };
    ]
    ~expected:[];

  (* Removing shadowing file for a module *)
  assert_incremental
    [{ handle = "a.py"; operation = FileOperation.LeftAlone }]
    ~external_setups:[{ handle = "a.pyi"; operation = FileOperation.Remove }]
    ~expected:[Event.create_new_explicit "a.py"];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.LeftAlone };
      { handle = "a.pyi"; operation = FileOperation.Add };
    ]
    ~external_setups:[{ handle = "a.pyi"; operation = FileOperation.Remove }]
    ~expected:[Event.create_new_explicit "a.pyi"];
  assert_incremental
    [{ handle = "a.py"; operation = FileOperation.LeftAlone }]
    ~external_setups:
      [
        { handle = "a.pyi"; operation = FileOperation.Remove };
        { handle = "a/__init__.pyi"; operation = FileOperation.Remove };
      ]
    ~expected:[Event.create_new_explicit "a.py"];

  (* Removing shadowed file for a module *)
  assert_incremental
    [{ handle = "a.py"; operation = FileOperation.Remove }]
    ~external_setups:[{ handle = "a.pyi"; operation = FileOperation.LeftAlone }]
    ~expected:[];
  assert_incremental
    [{ handle = "a.py"; operation = FileOperation.Remove }]
    ~external_setups:
      [
        { handle = "a.pyi"; operation = FileOperation.Remove };
        { handle = "a/__init__.pyi"; operation = LeftAlone };
      ]
    ~expected:[];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.Remove };
      { handle = "a.pyi"; operation = FileOperation.Add };
    ]
    ~external_setups:[{ handle = "a.pyi"; operation = FileOperation.LeftAlone }]
    ~expected:[];

  (* Update file *)
  assert_incremental
    [{ handle = "a.py"; operation = FileOperation.Update }]
    ~expected:[Event.create_new_explicit "a.py"];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.Update };
      { handle = "a.pyi"; operation = FileOperation.Update };
    ]
    ~expected:[Event.create_new_explicit "a.pyi"];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.LeftAlone };
      { handle = "a.pyi"; operation = FileOperation.Update };
    ]
    ~expected:[Event.create_new_explicit "a.pyi"];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.Update };
      { handle = "a.pyi"; operation = FileOperation.LeftAlone };
    ]
    ~expected:[];
  assert_incremental [{ handle = "a.thrift"; operation = FileOperation.Update }] ~expected:[];
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.Update };
      { handle = "b.thrift"; operation = FileOperation.Update };
    ]
    ~expected:[Event.create_new_explicit "a.py"];

  (* Removing and adding the same module *)
  assert_incremental
    [
      { handle = "a.py"; operation = FileOperation.Remove };
      { handle = "a.pyi"; operation = FileOperation.Add };
    ]
    ~expected:[Event.create_new_explicit "a.pyi"];

  (* Implicit submodule insertion *)
  assert_incremental
    [{ handle = "a/b.py"; operation = FileOperation.Add }]
    ~expected:[Event.create_new_explicit "a/b.py"; Event.NewImplicit "a"];
  assert_incremental
    [{ handle = "a/b/c.py"; operation = FileOperation.Add }]
    ~expected:[Event.create_new_explicit "a/b/c.py"; Event.NewImplicit "a"; Event.NewImplicit "a.b"];
  assert_incremental
    [
      { handle = "a/b/c.py"; operation = FileOperation.Add };
      { handle = "a/b/d.py"; operation = FileOperation.Add };
    ]
    ~expected:
      [
        Event.create_new_explicit "a/b/c.py";
        Event.create_new_explicit "a/b/d.py";
        Event.NewImplicit "a";
        Event.NewImplicit "a.b";
      ];
  assert_incremental
    [
      { handle = "a/b/c.py"; operation = FileOperation.Add };
      { handle = "a/b/d.py"; operation = FileOperation.LeftAlone };
    ]
    ~expected:[Event.create_new_explicit "a/b/c.py"];
  assert_incremental
    [
      { handle = "a/b/c.py"; operation = FileOperation.Add };
      { handle = "a/b/d.py"; operation = FileOperation.LeftAlone };
    ]
    ~expected:[Event.create_new_explicit "a/b/c.py"];
  assert_incremental
    [
      { handle = "a/b/c.py"; operation = FileOperation.Add };
      { handle = "a/b/d.py"; operation = FileOperation.Update };
    ]
    ~expected:[Event.create_new_explicit "a/b/c.py"; Event.create_new_explicit "a/b/d.py"];

  (* Implicit submodule remove *)
  assert_incremental
    [{ handle = "a/b/c.py"; operation = FileOperation.Remove }]
    ~expected:[Event.Delete "a.b.c"; Event.Delete "a.b"; Event.Delete "a"];
  assert_incremental
    [
      { handle = "a/b/c.py"; operation = FileOperation.Remove };
      { handle = "a/b/d.py"; operation = FileOperation.LeftAlone };
    ]
    ~expected:[Event.Delete "a.b.c"];
  assert_incremental
    [
      { handle = "a/b/c.py"; operation = FileOperation.Remove };
      { handle = "a/d.py"; operation = FileOperation.LeftAlone };
    ]
    ~expected:[Event.Delete "a.b.c"; Event.Delete "a.b"];
  assert_incremental
    [
      { handle = "a/b/c.py"; operation = FileOperation.LeftAlone };
      { handle = "a/d.py"; operation = FileOperation.Remove };
    ]
    ~expected:[Event.Delete "a.d"];
  assert_incremental
    [
      { handle = "a/b.py"; operation = FileOperation.LeftAlone };
      { handle = "a/b.pyi"; operation = FileOperation.Remove };
    ]
    ~expected:[Event.create_new_explicit "a/b.py"];
  assert_incremental
    [
      { handle = "a/b.py"; operation = FileOperation.Remove };
      { handle = "a/b.pyi"; operation = FileOperation.Remove };
    ]
    ~expected:[Event.Delete "a.b"; Event.Delete "a"];
  assert_incremental
    [
      { handle = "a/b/c.py"; operation = FileOperation.LeftAlone };
      { handle = "a/d/e.py"; operation = FileOperation.Remove };
      { handle = "a/d/f.py"; operation = FileOperation.Remove };
    ]
    ~expected:[Event.Delete "a.d"; Event.Delete "a.d.e"; Event.Delete "a.d.f"];

  (* Implicit submodule add+remove *)
  assert_incremental
    [
      { handle = "a/b/c.py"; operation = FileOperation.Add };
      { handle = "a/b/d.py"; operation = FileOperation.Update };
    ]
    ~expected:[Event.create_new_explicit "a/b/c.py"; Event.create_new_explicit "a/b/d.py"];
  assert_incremental
    [
      { handle = "a/b/c.py"; operation = FileOperation.Add };
      { handle = "a/b/d.py"; operation = FileOperation.Remove };
      { handle = "a/b/e.py"; operation = FileOperation.Remove };
    ]
    ~expected:[Event.create_new_explicit "a/b/c.py"; Event.Delete "a.b.d"; Event.Delete "a.b.e"];

  (* NOTE: These tests are likely to change if we want to allow a/b.py to shadow a/b/... *)
  assert_incremental
    [
      { handle = "a/b.py"; operation = FileOperation.Remove };
      { handle = "a/b/c.py"; operation = FileOperation.LeftAlone };
      { handle = "a/b/d.py"; operation = FileOperation.LeftAlone };
    ]
    ~expected:[Event.Delete "a.b"];
  assert_incremental
    [
      { handle = "a/b.py"; operation = FileOperation.Remove };
      { handle = "a/b/c.py"; operation = FileOperation.Add };
      { handle = "a/b/d.py"; operation = FileOperation.Add };
    ]
    ~expected:
      [
        Event.Delete "a.b";
        Event.create_new_explicit "a/b/c.py";
        Event.create_new_explicit "a/b/d.py";
      ];
  assert_incremental
    [
      { handle = "a/b.py"; operation = FileOperation.Add };
      { handle = "a/b/c.py"; operation = FileOperation.Remove };
      { handle = "a/b/d.py"; operation = FileOperation.Remove };
    ]
    ~expected:[Event.Delete "a.b.c"; Event.Delete "a.b.d"; Event.create_new_explicit "a/b.py"];
  ()


let () = "environment" >::: ["creation" >:: test_creation; "update" >:: test_update] |> Test.run
