(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Expression
open Pyre
open Taint

open Interprocedural


type parameter_taint = {
  name: string;
  sinks: Taint.Sinks.t list;
}


type error_expectation = {
  code: int;
  pattern: string;
}


type expectation = {
  define_name: string;
  sink_parameters: parameter_taint list;
  tito_parameters: string list;
  returns: Taint.Sources.t list;
  errors: error_expectation list;
}


let check_expectation
    ~get_model
    { define_name; sink_parameters; tito_parameters; returns; errors }
  =
  let open Taint.Result in
  let extract_sinks_by_parameter_name root sink_tree sink_map =
    match AccessPath.Root.parameter_name root with
    | Some name ->
        let sinks =
          Domains.BackwardState.collapse sink_tree
          |> Domains.BackwardTaint.leaves
        in
        let sinks =
          String.Map.find sink_map name
          |> Option.value ~default:[]
          |> List.rev_append sinks
          |> List.dedup_and_sort ~compare:Taint.Sinks.compare
        in
        String.Map.set sink_map ~key:name ~data:sinks
    | _ ->
        sink_map
  in
  let backward, forward =
    let model = get_model (Callable.create_real (Access.create define_name)) in
    match model with
    | None -> Format.sprintf "model not found for %s" define_name |> assert_failure
    | Some { backward; forward } -> backward, forward
  in
  let taint_map =
    Domains.BackwardState.fold
      backward.sink_taint
      ~f:extract_sinks_by_parameter_name
      ~init:String.Map.empty
  in
  let extract_tito_parameter_name root _ positions =
    match AccessPath.Root.parameter_name root with
    | Some name -> String.Set.add positions name
    | _ -> positions
  in
  let taint_in_taint_out_names =
    Domains.BackwardState.fold
      backward.taint_in_taint_out
      ~f:extract_tito_parameter_name
      ~init:String.Set.empty
  in
  let check_each_sink ~key:name ~data =
    match data with
    | `Both (expected, actual) ->
        assert_equal
          ~cmp:(List.equal ~equal:Taint.Sinks.equal)
          ~printer:(fun list -> Sexp.to_string [%message (list: Taint.Sinks.t list)])
          ~msg:(Format.sprintf "Define %s Parameter %s" define_name name)
          expected
          actual
    | `Left expected ->
        assert_equal
          ~cmp:(List.equal ~equal:Taint.Sinks.equal)
          ~printer:(fun list -> Sexp.to_string [%message (list: Taint.Sinks.t list)])
          ~msg:(Format.sprintf "Define %s Parameter %s" define_name name)
          expected
          []
    | `Right _ ->
        (* Okay, we may have outcomes we don't care about *)
        ()
  in
  let expected_sinks =
    List.map ~f:(fun { name; sinks; } -> name, sinks) sink_parameters
    |> String.Map.of_alist_exn
  in
  (* Check sources. *)
  let returned_sources =
    Domains.ForwardState.read AccessPath.Root.LocalResult forward.source_taint
    |> Domains.ForwardState.collapse
    |> Domains.ForwardTaint.leaves
    |> List.map ~f:Sources.show
    |> String.Set.of_list
  in
  let expected_sources =
    List.map ~f:Sources.show returns
    |> String.Set.of_list
  in
  let assert_error { code; pattern } error =
    if code <> Error.code error then
      Format.sprintf "Expected error code %d for %s, but got %d"
        code
        define_name
        (Error.code error)
      |> assert_failure;
    let error_string = Error.description ~detailed:true error in
    let regexp = Str.regexp pattern in
    if not (Str.string_match regexp error_string 0) then
      Format.sprintf "Expected error for %s to match %s, but got %s"
        define_name
        pattern
        error_string
      |> assert_failure
  in
  let assert_errors error_patterns errors =
    assert_equal
      (List.length error_patterns)
      (List.length errors)
      ~msg:(Format.sprintf "Number of errors for %s" define_name)
      ~printer:Int.to_string;
    List.iter2_exn ~f:assert_error error_patterns errors
  in
  assert_equal
    ~cmp:String.Set.equal
    ~printer:(fun set ->
        Format.sprintf "%s: %s"
          define_name
          (Sexp.to_string [%message (set: String.Set.t)]))
    expected_sources
    returned_sources;

  (* Check sinks. *)
  assert_equal
    (Map.length expected_sinks)
    (Map.length taint_map)
    ~msg:(Format.sprintf "Define %s: List of tainted parameters differ in length." define_name);
  String.Map.iter2 ~f:check_each_sink expected_sinks taint_map;

  let expected_tito = tito_parameters |> String.Set.of_list in
  assert_equal
    ~cmp:String.Set.equal
    ~printer:(fun set -> Sexp.to_string [%message (set: String.Set.t)])
    ~msg:(Format.sprintf "Define %s Tito positions" define_name)
    expected_tito
    taint_in_taint_out_names;

  (* Checke errors *)
  let actual_errors =
    let call_target = Callable.create_real (Access.create define_name) in
    Fixpoint.get_result call_target
    |> Result.get_result Taint.Result.kind
    >>| List.map ~f:Flow.generate_error
    |> Option.value ~default:[]
  in
  assert_errors errors actual_errors
