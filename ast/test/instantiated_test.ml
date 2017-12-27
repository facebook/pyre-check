(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Expression
open Statement

open Test


let test_is_method _ =
  let define name parent =
    let parent = if parent = "" then None else (Some (Access.create parent)) in
    {
      Define.name = Access.create name;
      parameters = [];
      body = [+Pass];
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
      generated = false;
      parent;
    } in
  assert_true (Instantiated.Define.is_method (define "foo" "path.source"));
  assert_false (Instantiated.Define.is_method (define "foo" ""));
  assert_false (Instantiated.Define.is_method (define "foo.bar" "path.source"))
  

let test_decorator _ =
  let define decorators =
    {
      Define.name = Access.create "foo";
      parameters = [];
      body = [+Pass];
      decorators;
      docstring = None;
      return_annotation = None;
      async = false;
      generated = false;
      parent = None;
    } in

  assert_false (Instantiated.Define.is_static_method (define []));
  assert_false (Instantiated.Define.is_static_method (define [!"foo"]));
  assert_true (Instantiated.Define.is_static_method (define [!"staticmethod"]));
  assert_true (Instantiated.Define.is_static_method (define [!"foo"; !"staticmethod"]));
  assert_true (Instantiated.Define.is_class_method (define [!"classmethod"]));

  assert_false (Instantiated.Define.is_abstract_method (define []));
  assert_false (Instantiated.Define.is_abstract_method (define [!"foo"]));
  assert_true (Instantiated.Define.is_abstract_method (define [!"abstractmethod"]));
  assert_true (Instantiated.Define.is_abstract_method (define [!"foo"; !"abstractmethod"]));
  assert_true (Instantiated.Define.is_abstract_method (define [!"abc.abstractmethod"]));
  assert_true (Instantiated.Define.is_abstract_method (define [!"abstractproperty"]));
  assert_true (Instantiated.Define.is_abstract_method (define [!"abc.abstractproperty"]));

  assert_true (Instantiated.Define.is_overloaded_method (define [!"overload"]));
  assert_true (Instantiated.Define.is_overloaded_method (define [!"typing.overload"]))


let test_is_constructor _ =
  let assert_is_constructor ~name ?(parent = None) expected =
    let parent =
      if Option.is_some parent then
        Some (Access.create (Option.value_exn parent))
      else None
    in
    let define =
      {
        Define.name = Access.create name;
        parameters = [];
        body = [+Pass];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        generated = false;
        parent;
      }
    in
    assert_equal (Instantiated.Define.is_constructor define) expected
  in
  assert_is_constructor ~name:"__init__" ~parent:(Some "foo") true;
  assert_is_constructor ~name:"__init__" false;
  assert_is_constructor ~name:"bar" ~parent:(Some "foo") false


let test_dump _ =
  let assert_dump source expected =
    assert_equal expected (parse_single_define source |> Instantiated.Define.dump)
  in

  assert_dump "def foo(): pass" false;
  assert_dump "def foo(): pyre_dump()" true;
  assert_dump "def foo(): pyre_dump_cfg()" false;
  assert_dump
    {|
      def foo():
        """docstring"""
        pass
        pyre_dump()
    |}
    true


let () =
  "define">:::[
    "is_method">::test_is_method;
    "decorator">::test_decorator;
    "is_constructor">::test_is_constructor;
    "dump">::test_dump;
  ]
  |> run_test_tt_main
