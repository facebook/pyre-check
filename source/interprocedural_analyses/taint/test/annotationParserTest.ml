(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Core
open Taint

let named name = { AnnotationParser.name; kind = Named }

let parametric name = { AnnotationParser.name; kind = Parametric }

let test_parse_source _ =
  let assert_parsed ~allowed ?subkind ~expected source =
    let actual =
      AnnotationParser.parse_source ~allowed ?subkind source |> Core.Result.ok_or_failwith
    in
    assert_equal ~cmp:Sources.equal actual expected
  in
  let assert_parse_error ~allowed ?subkind ~expected source =
    let error =
      Option.value_exn (AnnotationParser.parse_source ~allowed ?subkind source |> Core.Result.error)
    in
    assert_equal ~cmp:String.equal expected error
  in
  assert_parsed ~allowed:[named "A"] ~expected:(Sources.NamedSource "A") "A";
  assert_parsed ~allowed:[parametric "A"] ~expected:(Sources.NamedSource "A") "A";
  assert_parsed
    ~allowed:[parametric "A"]
    ~expected:(Sources.ParametricSource { source_name = "A"; subkind = "Subkind" })
    ~subkind:"Subkind"
    "A";
  assert_parse_error
    ~expected:"Unsupported taint source `A`"
    ~allowed:[named "A"]
    ~subkind:"Subkind"
    "A";
  ()


let test_parse_sink _ =
  let assert_parsed ~allowed ?subkind ~expected sink =
    let actual = AnnotationParser.parse_sink ~allowed ?subkind sink |> Core.Result.ok_or_failwith in
    assert_equal ~cmp:Sinks.equal actual expected
  in
  let assert_parse_error ~allowed ?subkind ~expected sink =
    let error =
      Option.value_exn (AnnotationParser.parse_sink ~allowed ?subkind sink |> Core.Result.error)
    in
    assert_equal ~printer:(Format.asprintf "%s") ~cmp:String.equal expected error
  in

  assert_parsed ~allowed:[named "A"] ~expected:(Sinks.NamedSink "A") "A";
  assert_parsed ~allowed:[parametric "A"] ~expected:(Sinks.NamedSink "A") "A";
  assert_parsed
    ~allowed:[parametric "A"]
    ~expected:(Sinks.ParametricSink { sink_name = "A"; subkind = "Subkind" })
    ~subkind:"Subkind"
    "A";
  assert_parse_error
    ~expected:"Unsupported taint sink `A`"
    ~allowed:[named "A"]
    ~subkind:"Subkind"
    "A";
  assert_parse_error ~allowed:[] ~expected:"Unsupported taint sink `LocalReturn`" "LocalReturn";
  assert_parse_error
    ~allowed:[]
    ~expected:"Unsupported taint sink `ParameterUpdate2`"
    "ParameterUpdate2";
  ()


let test_parse_tito _ =
  let assert_parsed ?(allowed_transforms = []) ?subkind ~expected sink =
    let actual =
      AnnotationParser.parse_tito ~allowed_transforms ?subkind sink |> Core.Result.ok_or_failwith
    in
    assert_equal ~cmp:Sinks.equal actual expected
  in
  let assert_parse_error ?(allowed_transforms = []) ?subkind ~expected sink =
    let error =
      Option.value_exn
        (AnnotationParser.parse_tito ~allowed_transforms ?subkind sink |> Core.Result.error)
    in
    assert_equal ~cmp:String.equal expected error
  in
  assert_parsed ~expected:Sinks.LocalReturn "LocalReturn";
  assert_parsed ~expected:(Sinks.ParameterUpdate 2) "ParameterUpdate2";
  assert_parsed
    ~allowed_transforms:[TaintTransform.Named "T"]
    ~expected:
      (Sinks.Transform
         {
           local = TaintTransforms.of_named_transforms [TaintTransform.Named "T"];
           global = TaintTransforms.empty;
           base = Sinks.LocalReturn;
         })
    ~subkind:"T"
    "Transform";
  assert_parse_error
    ~expected:"Unsupported taint in taint out specification `foo`"
    ~subkind:"Subkind"
    "foo";
  assert_parse_error
    ~allowed_transforms:[TaintTransform.Named "T"]
    ~expected:"Unsupported transform `U`"
    ~subkind:"U"
    "Transform";
  assert_parse_error
    ~allowed_transforms:[TaintTransform.Named "T"]
    ~expected:"Tito transform requires name of the transform as parameter"
    "Transform"


let () =
  "annotationParser"
  >::: [
         "parse_source" >:: test_parse_source;
         "parse_sink" >:: test_parse_sink;
         "parse_tito" >:: test_parse_tito;
       ]
  |> Test.run
