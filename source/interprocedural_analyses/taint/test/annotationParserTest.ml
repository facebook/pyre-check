(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Core
open Taint
module AccessPath = Analysis.TaintAccessPath

let named name = { AnnotationParser.KindDefinition.name; kind = Named; location = None }

let parametric name = { AnnotationParser.KindDefinition.name; kind = Parametric; location = None }

let test_parse_source _ =
  let assert_parsed ~allowed ~expected source =
    let actual = AnnotationParser.parse_source ~allowed source |> Core.Result.ok_or_failwith in
    assert_equal ~printer:Sources.show ~cmp:Sources.equal actual expected
  in
  let assert_parse_error ~allowed ~expected source =
    let error =
      Option.value_exn (AnnotationParser.parse_source ~allowed source |> Core.Result.error)
    in
    assert_equal ~printer:Fn.id ~cmp:String.equal expected error
  in
  assert_parsed
    ~allowed:[named "A"]
    ~expected:(Sources.NamedSource "A")
    (AnnotationParser.KindExpression.Name { name = "A"; subkind = None });
  assert_parsed
    ~allowed:[parametric "A"]
    ~expected:(Sources.NamedSource "A")
    (AnnotationParser.KindExpression.Name { name = "A"; subkind = None });
  assert_parsed
    ~allowed:[parametric "A"]
    ~expected:(Sources.ParametricSource { source_name = "A"; subkind = "Subkind" })
    (AnnotationParser.KindExpression.Name { name = "A"; subkind = Some "Subkind" });
  assert_parse_error
    ~expected:"Unsupported taint source `A`"
    ~allowed:[named "A"]
    (AnnotationParser.KindExpression.Name { name = "A"; subkind = Some "Subkind" });
  assert_parse_error
    ~expected:"Unsupported taint source `Updates[]`"
    ~allowed:[named "A"]
    (AnnotationParser.KindExpression.Updates
       (AccessPath.Root.PositionalParameter { name = "x"; position = 0; positional_only = false }));
  ()


let test_parse_sink _ =
  let assert_parsed ~allowed ~expected sink =
    let actual = AnnotationParser.parse_sink ~allowed sink |> Core.Result.ok_or_failwith in
    assert_equal ~cmp:Sinks.equal actual expected
  in
  let assert_parse_error ~allowed ~expected sink =
    let error = Option.value_exn (AnnotationParser.parse_sink ~allowed sink |> Core.Result.error) in
    assert_equal ~printer:(Format.asprintf "%s") ~cmp:String.equal expected error
  in

  assert_parsed
    ~allowed:[named "A"]
    ~expected:(Sinks.NamedSink "A")
    (AnnotationParser.KindExpression.Name { name = "A"; subkind = None });
  assert_parsed
    ~allowed:[parametric "A"]
    ~expected:(Sinks.NamedSink "A")
    (AnnotationParser.KindExpression.Name { name = "A"; subkind = None });
  assert_parsed
    ~allowed:[parametric "A"]
    ~expected:(Sinks.ParametricSink { sink_name = "A"; subkind = "Subkind" })
    (AnnotationParser.KindExpression.Name { name = "A"; subkind = Some "Subkind" });
  assert_parse_error
    ~expected:"Unsupported taint sink `A`"
    ~allowed:[named "A"]
    (AnnotationParser.KindExpression.Name { name = "A"; subkind = Some "Subkind" });
  assert_parse_error
    ~allowed:[]
    ~expected:"Unsupported taint sink `LocalReturn`"
    (AnnotationParser.KindExpression.Name { name = "LocalReturn"; subkind = None });
  assert_parse_error
    ~allowed:[]
    ~expected:"Unsupported taint sink `Updates[]`"
    (AnnotationParser.KindExpression.Updates
       (AccessPath.Root.PositionalParameter { name = "x"; position = 0; positional_only = false }));
  ()


let test_parse_tito _ =
  let assert_parsed ?(allowed_transforms = []) ~expected sink =
    let actual =
      AnnotationParser.parse_tito ~allowed_transforms sink |> Core.Result.ok_or_failwith
    in
    assert_equal ~cmp:Sinks.equal actual expected
  in
  let assert_parse_error ?(allowed_transforms = []) ~expected sink =
    let error =
      Option.value_exn (AnnotationParser.parse_tito ~allowed_transforms sink |> Core.Result.error)
    in
    assert_equal ~cmp:String.equal expected error
  in
  assert_parsed
    ~expected:Sinks.LocalReturn
    (AnnotationParser.KindExpression.Name { name = "LocalReturn"; subkind = None });
  assert_parsed
    ~expected:
      (Sinks.ParameterUpdate
         (AccessPath.Root.PositionalParameter { position = 2; name = "z"; positional_only = false }))
    (AnnotationParser.KindExpression.Updates
       (AccessPath.Root.PositionalParameter { position = 2; name = "z"; positional_only = false }));
  assert_parsed
    ~allowed_transforms:[TaintTransform.Named "T"]
    ~expected:
      (Sinks.Transform
         {
           local = TaintTransforms.of_named_transforms [TaintTransform.Named "T"];
           global = TaintTransforms.empty;
           base = Sinks.LocalReturn;
         })
    (AnnotationParser.KindExpression.Name { name = "Transform"; subkind = Some "T" });
  assert_parse_error
    ~expected:"Unsupported taint in taint out specification `foo`"
    (AnnotationParser.KindExpression.Name { name = "foo"; subkind = Some "Subkind" });
  assert_parse_error
    ~allowed_transforms:[TaintTransform.Named "T"]
    ~expected:"Unsupported transform `U`"
    (AnnotationParser.KindExpression.Name { name = "Transform"; subkind = Some "U" });
  assert_parse_error
    ~allowed_transforms:[TaintTransform.Named "T"]
    ~expected:"Tito transform requires name of the transform as parameter"
    (AnnotationParser.KindExpression.Name { name = "Transform"; subkind = None });
  ()


let () =
  "annotationParser"
  >::: [
         "parse_source" >:: test_parse_source;
         "parse_sink" >:: test_parse_sink;
         "parse_tito" >:: test_parse_tito;
       ]
  |> Test.run
