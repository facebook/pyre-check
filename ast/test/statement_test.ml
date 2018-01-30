(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Expression
open Pyre
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
  assert_true (Define.is_method (define "foo" "path.source"));
  assert_false (Define.is_method (define "foo" ""));
  assert_false (Define.is_method (define "foo.bar" "path.source"))


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

  assert_false (Define.is_static_method (define []));
  assert_false (Define.is_static_method (define [!"foo"]));
  assert_true (Define.is_static_method (define [!"staticmethod"]));
  assert_true (Define.is_static_method (define [!"foo"; !"staticmethod"]));
  assert_true (Define.is_class_method (define [!"classmethod"]));

  assert_false (Define.is_abstract_method (define []));
  assert_false (Define.is_abstract_method (define [!"foo"]));
  assert_true (Define.is_abstract_method (define [!"abstractmethod"]));
  assert_true (Define.is_abstract_method (define [!"foo"; !"abstractmethod"]));
  assert_true (Define.is_abstract_method (define [!"abc.abstractmethod"]));
  assert_true (Define.is_abstract_method (define [!"abstractproperty"]));
  assert_true (Define.is_abstract_method (define [!"abc.abstractproperty"]));

  assert_true (Define.is_overloaded_method (define [!"overload"]));
  assert_true (Define.is_overloaded_method (define [!"typing.overload"]))


let test_is_constructor _ =
  let assert_is_constructor ?(in_test = false) ~name ?(parent = None) expected =
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
    assert_equal (Define.is_constructor ~in_test define) expected
  in
  assert_is_constructor ~name:"__init__" ~parent:(Some "foo") true;
  assert_is_constructor ~in_test:true ~name:"setUp" ~parent:(Some "foo") true;
  assert_is_constructor ~name:"__init__" false;
  assert_is_constructor ~name:"bar" ~parent:(Some "foo") false


let test_dump _ =
  let assert_dump source expected =
    assert_equal expected (parse_single_define source |> Define.dump)
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


let test_constructor _ =
  let assert_constructor source ~exists =
    let definition =
      match parse_single_statement source with
      | { Node.value = Class definition; _ } -> definition
      | _ -> failwith "Could not parse class"
    in
    let constructor = Class.constructor definition in
    if exists then
      assert_is_some constructor
    else
      assert_is_none constructor
  in

  assert_constructor
    {|
      class Foo:
        def __init__(self):
          pass
    |}
    ~exists:true;
  assert_constructor
    {|
      class Foo:
        pass
    |}
    ~exists:false


let test_attribute_assigns _ =
  let create_assign ~target ~annotation ~value =
    {
      Assign.target = Node.create (Expression.Access (Access.create target));
      annotation;
      value = value >>| (fun value -> (Node.create (Expression.Access (Access.create value))));
      compound = None;
      parent = None;
    }
    |> Node.create
  in
  let assign_equal { Node.value = left; _ } { Node.value = right; _ } =
    let open Assign in
    Expression.equal left.target right.target &&
    Option.equal Expression.equal left.annotation right.annotation &&
    Option.equal Expression.equal left.value right.value
  in

  (* Test define field assigns. *)
  let assert_implicit_attribute_assigns source expected =
    let expected =
      List.map
        ~f:(fun (target, annotation, value) -> create_assign ~target ~annotation ~value)
        expected
    in
    let definition =
      {
        Record.Class.name = [];
        bases = [];
        body = [];
        decorators = [];
        docstring = None;
      }
    in
    assert_equal
      ~cmp:(List.equal ~equal:assign_equal)
      expected
      (parse_single_define source |> Define.implicit_attribute_assigns ~definition |> Map.data)
  in
  assert_implicit_attribute_assigns "def foo(): pass" [];

  assert_implicit_attribute_assigns
    {|
      def foo():
        self.attribute = value
        self.attribute: int = value
    |}
    ["attribute", Some (Type.expression Type.integer), None];
  assert_implicit_attribute_assigns
    {|
      def foo():
        self.attribute: int = value
        self.attribute = value
    |}
    ["attribute", Some (Type.expression Type.integer), None];
  assert_implicit_attribute_assigns
    {|
      def foo():
        self.attribute: int = value
        self.attribute: str = value
    |}
    ["attribute", Some (Type.expression (Type.Union [Type.string; Type.integer])), None];

  (* Implicit arguments in branches. *)
  assert_implicit_attribute_assigns
    {|
      def foo():
        a = 1
        self.attribute = value
        if True:
          self.other = value
          if False:
            self.nested = value
    |}
    ["attribute", None, None; "nested", None, None; "other", None, None];

  (* Test define field assigns. *)
  let assert_property_attribute_assign source expected =
    let expected =
      expected
      >>| fun (target, annotation, value) -> create_assign ~target ~annotation ~value
    in
    assert_equal
      ~cmp:(Option.equal assign_equal)
      expected
      (parse_single_define source |> Define.property_attribute_assign ~location:Location.any)
  in
  assert_property_attribute_assign "def foo(): pass" None;
  assert_property_attribute_assign "@property\ndef foo(): pass" (Some ("foo", None, None));
  assert_property_attribute_assign
    "@abc.abstractproperty\ndef foo() -> int: pass"
    (Some ("foo", Some (Type.expression Type.integer), None));
  assert_property_attribute_assign
    "@util.etc.lazy_property\ndef foo() -> int: pass"
    (Some ("foo", Some (Type.expression Type.integer), None));

  (* Test class field assigns. *)
  let assert_attribute_assigns ?(include_properties = true) source expected =
    let expected =
      List.map
        ~f:(fun (target, annotation, value) -> create_assign ~target ~annotation ~value)
        expected
    in
    let printer assigns =
      List.map ~f:(fun { Node.value; _ } -> Assign.show value) assigns
      |> String.concat ~sep:", "
    in
    assert_equal
      ~cmp:(List.equal ~equal:assign_equal)
      ~printer
      expected
      (parse_single_class source |> Class.attribute_assigns ~include_properties |> Map.data)
  in
  assert_attribute_assigns
    {|
      class Foo:
        attribute: int = value
    |}
    ["attribute", Some (Type.expression Type.integer), Some "value"];
  assert_attribute_assigns
    {|
      class Foo:
        def __init__(self):
          self.implicit = implicit
          self.whatever()['asdf'] = 5
          if True:
            self.ignored = ignored
        attribute: int = value
        whatever()['asdf'] = 5
    |}
    [
      "__init__", None, None;
      "attribute", Some (Type.expression Type.integer), Some "value";
      "ignored", None, None;
      "implicit", None, None;
    ];
  assert_attribute_assigns
    {|
      class Foo:
        def __init__(self):
          self.attribute = value  # Prioritize explicit declaration
        attribute: int = value
    |}
    [
      "__init__", None, None;
      "attribute", Some (Type.expression Type.integer), Some "value";
    ];
  assert_attribute_assigns
    {|
      class Foo:
        def __init__(self):
          self._init()
        def _init(self):
          self.attribute: int = value
        def not_inlined(self):
          self.other: int = 1
    |}
    [
      "__init__", None, None;
      "_init", None, None;
      "attribute", Some (Type.expression Type.integer), None;
      "not_inlined", None, None;
    ];
  assert_attribute_assigns
    {|
      class Foo:
        attribute: int = value
        @property
        def property(self) -> int:
          pass
    |}
    [
      "attribute", Some (Type.expression Type.integer), Some "value";
      "property", Some (Type.expression Type.integer), None;
    ];
  assert_attribute_assigns
    {|
      class Foo:
        @property
        def property(self) -> int:
          pass
    |}
    ["property", Some (Type.expression Type.integer), None];
  assert_attribute_assigns
    {|
      class Foo:
        @property
        def property(self) -> int: ...
    |}
    ["property", Some (Type.expression Type.integer), None];

  (* Class properties. *)
  assert_attribute_assigns
    {|
      class Foo:
        @util.etc.class_property
        def property(self) -> int:
          pass
    |}
    ["property", Some (Type.expression (Type.parametric "typing.ClassVar" [Type.integer])), None];
  assert_attribute_assigns
    {|
      class Foo:
        @util.classproperty
        def property(self) -> int:
          pass
    |}
    ["property", Some (Type.expression (Type.parametric "typing.ClassVar" [Type.integer])), None];

  (* Named tuple attributes. *)
  assert_attribute_assigns
    {|
      class Foo(typing.NamedTuple('Foo', ['one', 'two'])):
        attribute: int
    |}
    [
      "attribute", Some (Type.expression Type.integer), None;
      "one", None, None;
      "two", None, None;
    ];
  assert_attribute_assigns
    {|
      class Foo(typing.NamedTuple('Foo', [('one', int), 'two'])):
        attribute: int
    |}
    [
      "attribute", Some (Type.expression Type.integer), None;
      "one", Some (Type.expression Type.integer), None;
      "two", None, None;
    ]


let test_strip _ =
  let assert_stripped source definition =
    assert_equal
      ~printer:Class.show
      ~cmp:Class.equal
      (parse_single_class source |> Class.strip)
      definition
  in

  assert_stripped
    {|
      class Foo:
        def __init__():
          pass
        def setUp():
          pass
        def _private():
          pass
        def method():
          pass
    |}
    {
      Class.name = Access.create "Foo";
      bases = [];
      body = [
        +Define {
          Define.name = Access.create "__init__";
          parameters = [];
          body = [+Pass];  (* Not stripped! *)
          decorators = [];
          docstring = None;
          return_annotation = None;
          async = false;
          generated = false;
          parent = Some (Access.create "Foo")
        };
        +Define {
          Define.name = Access.create "setUp";
          parameters = [];
          body = [+Pass];  (* Not stripped! *)
          decorators = [];
          docstring = None;
          return_annotation = None;
          async = false;
          generated = false;
          parent = Some (Access.create "Foo")
        };
        +Define {
          Define.name = Access.create "_private";
          parameters = [];
          body = [+Pass];  (* Not stripped! *)
          decorators = [];
          docstring = None;
          return_annotation = None;
          async = false;
          generated = false;
          parent = Some (Access.create "Foo")
        };
        +Define {
          Define.name = Access.create "method";
          parameters = [];
          body = [];
          decorators = [];
          docstring = None;
          return_annotation = None;
          async = false;
          generated = false;
          parent = Some (Access.create "Foo")
        };
      ];
      decorators = [];
      docstring = None;
    }


let test_update _ =
  let assert_updated ~stub ~definition expected =
    assert_equal
      ~printer:Class.show
      ~cmp:Class.equal
      (parse_single_class expected)
      (Class.update (parse_single_class stub) ~definition:(parse_single_class definition))
  in

  assert_updated
    ~stub:{|
      class Foo:
        i: int = ...
    |}
    ~definition:{|
      class Foo:
        def foo():
          pass
    |}
    {|
      class Foo:
        i: int = ...
        def foo():
          pass
    |};
  assert_updated
    ~stub:{|
      class Foo:
        i: int = ...
    |}
    ~definition:{|
      class Foo:
        i: int = 5
    |}
    {|
      class Foo:
        i: int = 5
    |};
  assert_updated
    ~stub:{|
      class Foo:
        i: int = ...
        def foo(i: int) -> str: ...
    |}
    ~definition:{|
      class Foo:
        def foo(i):
          pass
    |}
    {|
      class Foo:
        i: int = ...
        def foo(i: int) -> str:
          pass
    |}


let test_assume _ =
  assert_equal
    (assume (+True))
    (+Assert {
       Assert.test = +True;
       message = None;
     })


let test_terminates _ =
  assert_true
    (parse ({|
        x = 1
        return x
     |})
     |> (fun source -> terminates source.Source.statements));
  assert_true
    (parse ({|
       x = 1
       raise
    |})
     |> (fun source -> terminates source.Source.statements));
  assert_false
    (parse ({|
         if x:
          return x
         x = 1
     |})
     |> (fun source -> terminates source.Source.statements))


let test_docstring _ =
  assert_equal
    (parse_single_statement {|
         def foo():
           """doc
              string
               end"""
           pass
    |}
     |> (function
         | { Node.value = Define { Define.docstring; _ }; _ } -> docstring
         | _ -> None))
    (Some "doc\nstring\n end")


let test_pp _ =
  let to_lines = String.split_on_chars ~on:['\n'] in

  let test_equal pretty_print_expected source =
    let pp_diff _ (got, want) =
      let to_debug_char = function
        | '\n' -> "\\n"
        | '\t' -> "\\t"
        | c -> Format.sprintf "%c" c
      in
      let print_verbose_chars =
        String.iter ~f:(fun c -> Format.printf "%s," @@ to_debug_char c)
      in
      Format.printf "You wanted: @.";
      print_verbose_chars want;
      Format.printf "@.But it's: @.";
      print_verbose_chars got;
    in

    let pretty_print_expected =
      pretty_print_expected
      |> String.lstrip ~drop:((=) '\n')
      |> Test.trim_extra_indentation
    in

    let source =
      Test.trim_extra_indentation source
      |> String.rstrip ~drop:((=) '\n')
    in

    let pretty_print_of_source =
      to_lines source
      |> PythonParse.parse
      |> List.map ~f:Statement.show
      |> String.concat ~sep:"\n"
      |> String.rstrip ~drop:((=) '\n')
    in

    assert_equal ~pp_diff
      pretty_print_of_source pretty_print_expected
  in

  (* Test 1 : simple def *)
  let source =
    {|
      def foo(bar):
        x = "hello world"
    |}
  in

  let pretty_print_expect =
    {|
      def foo(bar):
        x = "hello world"
    |}
  in

  test_equal pretty_print_expect source;

  (* Test 2 : def with multiple decorators *)
  let source =
    {|
      @decorator1
      @decorator2
      def foo(bar):
        x = "hello world"
    |}
  in

  let pretty_print_expect =
    {|
      @(decorator1, decorator2)
      def foo(bar):
        x = "hello world"
    |}
  in

  test_equal pretty_print_expect source;

  (* Test 3 : multiple defs and statements *)
  let source =
    {|
      @decorator1
      @decorator2
      def foo(bar):
        x = "hello world"

      @decorator3
      def foo(baz):
        x = "hello squirrel"
        y = 5
    |}
  in

  let pretty_print_expect =
    {|
      @(decorator1, decorator2)
      def foo(bar):
        x = "hello world"

      @(decorator3)
      def foo(baz):
        x = "hello squirrel"
        y = 5
    |}
  in

  test_equal pretty_print_expect source;

  (* Test 4 : cover classes, for loops, compound statements *)
  let source =
    {|
      @class_decorator
      class Foo(Bar):
        def baz(quux):
          for i in xrange(quux):
            i += 1
          i *= 2
    |}
  in

  let pretty_print_expect =
    {|
      @(class_decorator)
      class Foo(Bar):
        def Foo.baz(quux):
          for i in xrange(quux):
            i += 1
          i *= 2

    |}
  in

  test_equal pretty_print_expect source;

  (* Test 5 : try/except/finally blocks *)
  let source =
    {|
      try:
        raise Exception("whoops")
      except SomeError as e:
        pass
      except (AnotherError, YetAnotherError):
        x = 1
        pass
      except:
       pass
      else:
        pass
      finally:
        pass
    |}
  in

  let pretty_print_expect =
    {|
      try:
        raise Exception("whoops")
      except SomeError as `e`:
        pass
      except (AnotherError, YetAnotherError):
        x = 1
        pass
      except:
        pass
      else:
        pass
      finally:
        pass
    |}
  in

  test_equal pretty_print_expect source;

  (* Test 6 : while and if/then/else and list access *)
  let source =
    {|
      while x:
        i += 1
        if i > 0:
          i -= 1
        else:
          i -= 2
        j += 2
      i[j] += 3
      i[j::1] += i[:j]
    |}
  in

  let pretty_print_expect =
    {|
      while x:
        i += 1
        if i > 0:
          i -= 1
        else:
          i -= 2
        j += 2
      i[j] += 3
      i[j::1] += i[:j]
    |}
  in

  test_equal pretty_print_expect source;

  let source =
    {|
      @some.decorator('with_a_string')
      def decorator_test():
        return 5
    |}
  in

  let pretty_print_expect =
    {|
      @(some.decorator("with_a_string"))
      def decorator_test():
        return 5
    |}
  in

  test_equal pretty_print_expect source


let () =
  "define">:::[
    "is_method">::test_is_method;
    "decorator">::test_decorator;
    "is_constructor">::test_is_constructor;
    "dump">::test_dump;
  ]
  |> run_test_tt_main;
  "class">:::[
    "constructor">::test_constructor;
    "attribute_assigns">::test_attribute_assigns;
    "strip">::test_strip;
    "update">::test_update;
  ]
  |> run_test_tt_main;
  "statement">:::[
    "assume">::test_assume;
    "terminates">::test_terminates;
    "pp">::test_pp;
  ]
  |> run_test_tt_main
