(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Expression
open Statement
open Test

let test_is_method _ =
  let define ~module_name ~parent =
    let prefix = NestingContext.to_qualifier ~module_name parent in
    let define_name = Reference.create ~prefix "test" in
    let legacy_parent =
      match parent with
      | NestingContext.Class _ -> Some prefix
      | _ -> None
    in
    {
      Define.signature =
        {
          Define.Signature.name = define_name;
          parameters = [];
          decorators = [];
          return_annotation = None;
          async = false;
          generator = false;
          parent;
          legacy_parent;
          type_params = [];
        };
      captures = [];
      unbound_names = [];
      body = [+Statement.Pass];
    }
  in
  assert_true
    (Define.is_method
       (define
          ~module_name:!&"path"
          ~parent:NestingContext.(create_class ~parent:(create_toplevel ()) "source")));
  assert_false
    (Define.is_method (define ~module_name:!&"foo" ~parent:(NestingContext.create_toplevel ())))


let decorator ?arguments name =
  {
    Decorator.name = + !&name;
    arguments;
    original_expression =
      Decorator.create_original_expression
        ~create_origin_for_reference:(fun _ -> None)
        ~call_origin:None
        ~name:(+ !&name)
        ~arguments;
  }


let test_is_classmethod _ =
  let define name decorators =
    {
      Define.signature =
        {
          Define.Signature.name = !&name;
          parameters = [];
          decorators;
          return_annotation = None;
          async = false;
          generator = false;
          parent = NestingContext.(create_class ~parent:(create_toplevel ()) "bar");
          legacy_parent = Some !&"bar";
          type_params = [];
        };
      captures = [];
      unbound_names = [];
      body = [+Statement.Pass];
    }
  in
  assert_false (Define.is_class_method (define "foo" []));
  assert_false (Define.is_class_method (define "__init__" []));
  assert_true (Define.is_class_method (define "foo" [!"classmethod"]));
  assert_true (Define.is_class_method (define "__init_subclass__" []));
  assert_true (Define.is_class_method (define "__new__" []));
  assert_true (Define.is_class_method (define "__class_getitem__" []))


let test_is_class_property _ =
  let define name decorators =
    {
      Define.signature =
        {
          Define.Signature.name = !&name;
          parameters = [];
          decorators;
          return_annotation = None;
          async = false;
          generator = false;
          parent = NestingContext.(create_class ~parent:(create_toplevel ()) "bar");
          legacy_parent = Some !&"bar";
          type_params = [];
        };
      captures = [];
      unbound_names = [];
      body = [+Statement.Pass];
    }
  in
  assert_false (Define.is_class_property (define "foo" []));
  assert_false (Define.is_class_property (define "__init__" []));
  assert_true (Define.is_class_property (define "foo" [!"__classproperty__"]));
  assert_false (Define.is_class_property (define "__new__" []))


let test_decorator _ =
  let define decorators =
    {
      Define.signature =
        {
          Define.Signature.name = !&"foo";
          parameters = [];
          decorators = List.map decorators ~f:Decorator.to_expression;
          return_annotation = None;
          async = false;
          generator = false;
          parent = NestingContext.create_toplevel ();
          legacy_parent = None;
          type_params = [];
        };
      captures = [];
      unbound_names = [];
      body = [+Statement.Pass];
    }
  in
  assert_false (Define.is_static_method (define []));
  assert_false (Define.is_static_method (define [decorator "foo"]));
  assert_true (Define.is_static_method (define [decorator "staticmethod"]));
  assert_true (Define.is_static_method (define [decorator "foo"; decorator "staticmethod"]));
  assert_false (Define.is_abstract_method (define []));
  assert_false (Define.is_abstract_method (define [decorator "foo"]));
  assert_true (Define.is_abstract_method (define [decorator "abstractmethod"]));
  assert_true (Define.is_abstract_method (define [decorator "foo"; decorator "abstractmethod"]));
  assert_true (Define.is_abstract_method (define [decorator "abc.abstractmethod"]));
  assert_true (Define.is_abstract_method (define [decorator "abstractproperty"]));
  assert_true (Define.is_abstract_method (define [decorator "abc.abstractproperty"]));
  assert_true (Define.is_overloaded_function (define [decorator "overload"]));
  assert_true (Define.is_overloaded_function (define [decorator "typing.overload"]));
  assert_true (Define.is_overloaded_function (define [decorator "typing_extensions.overload"]));
  assert_true (Define.is_overloaded_function (define [decorator "_pytest.compat.overload"]));
  assert_true (Define.is_property_setter (define [decorator "foo.setter"]));
  assert_false (Define.is_property_setter (define [decorator "setter"]));
  assert_false (Define.is_property_setter (define [decorator "bar.setter"]))


let test_is_constructor _ =
  let assert_is_constructor ?(in_test = false) ~method_name ~parent expected =
    let prefix = NestingContext.to_qualifier ~module_name:Reference.empty parent in
    let define_name = Reference.create ~prefix method_name in
    let legacy_parent =
      match parent with
      | NestingContext.Class _ -> Some prefix
      | _ -> None
    in
    let define =
      {
        Define.signature =
          {
            Define.Signature.name = define_name;
            parameters = [];
            decorators = [];
            return_annotation = None;
            async = false;
            generator = false;
            parent;
            legacy_parent;
            type_params = [];
          };
        captures = [];
        unbound_names = [];
        body = [+Statement.Pass];
      }
    in
    assert_equal expected (Define.is_constructor ~in_test define)
  in
  let foo_parent = NestingContext.(create_class ~parent:(create_toplevel ()) "Foo") in
  assert_is_constructor ~method_name:"__init__" ~parent:foo_parent true;
  assert_is_constructor ~method_name:"__init_subclass__" ~parent:foo_parent true;
  assert_is_constructor ~in_test:true ~method_name:"setUp" ~parent:foo_parent true;
  assert_is_constructor ~in_test:true ~method_name:"async_setUp" ~parent:foo_parent true;
  assert_is_constructor ~in_test:true ~method_name:"asyncSetUp" ~parent:foo_parent true;
  assert_is_constructor ~in_test:false ~method_name:"asyncSetUp" ~parent:foo_parent false;
  assert_is_constructor ~in_test:false ~method_name:"async_setUp" ~parent:foo_parent false;
  assert_is_constructor ~in_test:true ~method_name:"with_context" ~parent:foo_parent true;
  assert_is_constructor ~in_test:true ~method_name:"async_with_context" ~parent:foo_parent true;
  assert_is_constructor ~in_test:false ~method_name:"async_with_context" ~parent:foo_parent false;
  assert_is_constructor ~method_name:"__init__" ~parent:(NestingContext.create_toplevel ()) false;
  assert_is_constructor ~method_name:"bar" ~parent:foo_parent false


let test_pyre_dump _ =
  let assert_dump ?(dump = false) ?(dump_cfg = false) ?(dump_locations = false) source =
    let parsed = parse_single_define source in
    assert_equal dump (parsed |> Define.dump);
    assert_equal dump_cfg (parsed |> Define.dump_cfg);
    assert_equal dump_locations (parsed |> Define.dump_locations)
  in
  assert_dump "def foo(): pass";
  assert_dump "def foo(): pyre_dump()" ~dump:true;
  assert_dump "def foo(): pyre_dump_cfg()" ~dump_cfg:true;
  assert_dump "def foo(): pyre_dump_locations()" ~dump_locations:true;
  assert_dump
    {|
      def foo():
        """docstring"""
        pass
        pyre_dump()
    |}
    ~dump:true


let test_constructor _ =
  let assert_constructor source ~exists =
    let definition =
      match parse_single_statement source with
      | { Node.value = Statement.Class definition; _ } -> definition
      | _ -> failwith "Could not parse class"
    in
    let constructors = Class.constructors definition in
    if exists then
      assert_false (List.is_empty constructors)
    else
      assert_true (List.is_empty constructors)
  in
  assert_constructor
    {|
      class Foo:
        def __init__(self):
          pass
    |}
    ~exists:true;
  assert_constructor {|
      class Foo:
        pass
    |} ~exists:false


let test_defines _ =
  let assert_define source ~method_name ~exists ~total =
    let definition =
      match parse_single_statement source with
      | { Node.value = Statement.Class definition; _ } -> definition
      | _ -> failwith "Could not parse class"
    in
    assert_equal
      total
      (List.length (Class.defines definition))
      ~msg:"Wrong number of defines"
      ~printer:Int.to_string;
    let method_id = method_name in
    match Class.find_define definition ~method_name:method_id with
    | Some { Node.value = { Define.signature = { Define.Signature.name; _ }; _ }; _ } when exists ->
        assert_equal name !&method_id ~printer:Reference.show
    | None when not exists -> ()
    | Some { Node.value = { Define.signature = { Define.Signature.name; _ }; _ }; _ } ->
        Format.asprintf
          "method %a found when not expected (looking for %s)"
          Reference.pp
          name
          method_name
        |> assert_failure
    | None -> Format.sprintf "method %s not found when expected" method_name |> assert_failure
  in
  assert_define
    {|
      class Foo:
        def method(self):
          pass

        def other(self):
          pass
    |}
    ~total:2
    ~method_name:"method"
    ~exists:true;
  assert_define
    {|
      class Foo:
        def method(self):
          pass

        def other(self):
          pass

        def third(self):
          pass
    |}
    ~total:3
    ~method_name:"non_existant"
    ~exists:false


let test_with_block_preamble _ =
  let assert_preamble block preamble =
    let block =
      match parse_single_statement block with
      | { Node.value = Statement.With block; _ } -> block
      | _ -> failwith "Could not parse `with` statement."
    in
    let { Source.statements = preamble; _ } = parse ~coerce_special_methods:true preamble in
    assert_equal
      ~cmp:(List.equal (fun left right -> Statement.location_insensitive_compare left right = 0))
      ~printer:(fun statements -> List.map ~f:show statements |> String.concat ~sep:", ")
      preamble
      (With.preamble block)
  in
  assert_preamble "with item: pass" "item.__enter__()";
  assert_preamble "with item, other: pass" "item.__enter__(); other.__enter__()";
  assert_preamble "with item as name: pass" "name = item.__enter__()";
  assert_preamble "async with item as name: pass" "name = await item.__aenter__()";
  ()


let test_try_block_preamble _ =
  let assert_preamble block preambles =
    let handlers =
      match parse_single_statement block with
      | { Node.value = Statement.Try { Try.handlers; _ }; _ } -> handlers
      | _ -> failwith "Could not parse `try` statement."
    in
    let preambles =
      let preamble source =
        let { Source.statements = preamble; _ } = parse ~coerce_special_methods:true source in
        preamble
      in
      List.map preambles ~f:preamble
    in
    let printer preambles =
      List.map preambles ~f:(fun preamble -> List.map ~f:show preamble |> String.concat ~sep:", ")
      |> String.concat ~sep:"\n"
    in
    assert_equal
      ~cmp:
        (List.equal
           (List.equal (fun left right -> Statement.location_insensitive_compare left right = 0)))
      ~printer
      preambles
      (List.map handlers ~f:(Try.preamble ~handles_exception_group:false))
  in
  assert_preamble
    {|
      try:
        pass
      except Exception:
        pass
    |}
    ["Exception"];
  assert_preamble
    {|
      try:
        pass
      except Exception as error:
        pass
    |}
    ["error = ...\nassert isinstance(error, Exception)"];
  assert_preamble
    {|
      try:
        pass
      except IOError as error:
        pass
      except Exception as error:
        pass
    |}
    [
      "error = ...\nassert isinstance(error, IOError)";
      "error = ...\nassert isinstance(error, Exception)";
    ];
  assert_preamble
    {|
      try:
        pass
      except (IOError, ValueError) as error:
        pass
    |}
    ["error=...\nassert isinstance(error, (IOError, ValueError))"];

  let assert_preamble_with_locations ~block expected_preambles =
    let handlers =
      match parse_single_statement block with
      | { Node.value = Statement.Try { Try.handlers; _ }; _ } -> handlers
      | _ -> failwith "Could not parse `try-except` statement."
    in
    assert_equal
      ~cmp:[%compare.equal: Statement.t list]
      ~printer:(fun statements -> [%sexp_of: Statement.t list] statements |> Sexp.to_string_hum)
      expected_preambles
      (List.concat_map handlers ~f:(Try.preamble ~handles_exception_group:false))
  in
  let ( ~@ ) = parse_location in
  assert_preamble_with_locations
    ~block:{|
    try:
      pass
    except IOError as error:
      pass
  |}
    [
      {
        Node.location = ~@"4:7-4:14";
        value =
          Statement.Assign
            {
              Assign.target =
                { Node.location = ~@"4:18-5:2"; value = Expression.Name (Name.Identifier "error") };
              annotation = None;
              value =
                Some (Node.create ~location:~@"4:7-4:14" (Expression.Constant Constant.Ellipsis));
            };
      };
      {
        Node.location = ~@"4:7-4:14";
        value =
          Statement.Assert
            {
              Assert.test =
                {
                  Node.location = ~@"4:7-4:14";
                  value =
                    Expression.Call
                      {
                        Call.callee =
                          {
                            Node.location = ~@"4:7-4:14";
                            value = Expression.Name (Name.Identifier "isinstance");
                          };
                        arguments =
                          [
                            {
                              Call.Argument.name = None;
                              value =
                                {
                                  Node.location = ~@"4:18-5:2";
                                  value = Expression.Name (Name.Identifier "error");
                                };
                            };
                            {
                              Call.Argument.name = None;
                              value =
                                {
                                  Node.location = ~@"4:7-4:14";
                                  value = Expression.Name (Name.Identifier "IOError");
                                };
                            };
                          ];
                        origin =
                          Some
                            { Origin.location = ~@"4:7-4:14"; kind = Origin.TryHandlerIsInstance };
                      };
                };
              message = None;
              origin = Some { Node.location = ~@"4:7-4:14"; value = Assert.Origin.TryHandler };
            };
      };
    ];
  ()


let test_for_loop_preamble _ =
  let assert_preamble ~block expected_preamble =
    let block =
      match parse_single_statement block with
      | { Node.value = Statement.For block; _ } -> block
      | _ -> failwith "Could not parse `for` statement."
    in
    let expected_preamble =
      match parse ~coerce_special_methods:true expected_preamble with
      | { Source.statements = [expected_preamble]; _ } -> expected_preamble
      | _ -> failwith "Expected exactly one statement"
    in
    assert_equal
      ~cmp:(fun left right -> Statement.location_insensitive_compare left right = 0)
      ~printer:[%show: Statement.t]
      expected_preamble
      (For.synthetic_preamble block)
  in
  assert_preamble ~block:{| for a in b: pass |} {| a = b.__iter__().__next__() |};
  assert_preamble ~block:{| for a, b in c: pass |} {| a, b = c.__iter__().__next__() |};
  assert_preamble ~block:{| for a in [1, 2, 3]: pass |} {| a = [1, 2, 3].__iter__().__next__() |};
  assert_preamble ~block:{| async for a in b: pass |} {| a = await b.__aiter__().__anext__() |};

  let assert_preamble_with_locations ~block expected_preamble =
    let block =
      match parse_single_statement block with
      | { Node.value = Statement.For block; _ } -> block
      | _ -> failwith "Could not parse `for` statement."
    in
    assert_equal
      ~cmp:[%compare.equal: Statement.t]
      ~printer:(fun statement -> [%sexp_of: Statement.t] statement |> Sexp.to_string_hum)
      expected_preamble
      (For.synthetic_preamble block)
  in
  let ( ~@ ) = parse_location in
  assert_preamble_with_locations
    ~block:{| for a in b: pass |}
    {
      Node.value =
        Statement.Assign
          {
            Assign.target =
              { Node.value = Expression.Name (Name.Identifier "a"); location = ~@"1:4-1:5" };
            annotation = None;
            value =
              Some
                {
                  Node.value =
                    Expression.Call
                      {
                        Call.callee =
                          {
                            Node.location = ~@"1:4-1:10";
                            value =
                              Expression.Name
                                (Name.Attribute
                                   {
                                     Name.Attribute.base =
                                       {
                                         Node.location = ~@"1:4-1:10";
                                         value =
                                           Expression.Call
                                             {
                                               Call.callee =
                                                 {
                                                   Node.location = ~@"1:4-1:10";
                                                   value =
                                                     Expression.Name
                                                       (Name.Attribute
                                                          {
                                                            Name.Attribute.base =
                                                              {
                                                                Node.value =
                                                                  Expression.Name
                                                                    (Name.Identifier "b");
                                                                location = ~@"1:9-1:10";
                                                              };
                                                            attribute = "__iter__";
                                                            origin =
                                                              Some
                                                                {
                                                                  Origin.location = ~@"1:9-1:10";
                                                                  kind = Origin.ForIter;
                                                                };
                                                          });
                                                 };
                                               arguments = [];
                                               origin =
                                                 Some
                                                   {
                                                     Origin.location = ~@"1:9-1:10";
                                                     kind = Origin.ForIter;
                                                   };
                                             };
                                       };
                                     attribute = "__next__";
                                     origin =
                                       Some
                                         { Origin.location = ~@"1:9-1:10"; kind = Origin.ForNext };
                                   });
                          };
                        arguments = [];
                        origin = Some { Origin.location = ~@"1:9-1:10"; kind = Origin.ForNext };
                      };
                  location = ~@"1:4-1:10";
                };
          };
      location = ~@"1:4-1:10";
    };
  assert_preamble_with_locations
    ~block:{|
      async for a in (
        xs
      ): pass |}
    {
      Node.value =
        Statement.Assign
          {
            Assign.target =
              { Node.value = Expression.Name (Name.Identifier "a"); location = ~@"2:10-2:11" };
            annotation = None;
            value =
              Some
                {
                  Node.value =
                    Expression.Await
                      {
                        Node.value =
                          Expression.Call
                            {
                              Call.callee =
                                {
                                  Node.location = ~@"2:10-3:4";
                                  value =
                                    Expression.Name
                                      (Name.Attribute
                                         {
                                           Name.Attribute.base =
                                             {
                                               Node.location = ~@"2:10-3:4";
                                               value =
                                                 Expression.Call
                                                   {
                                                     Call.callee =
                                                       {
                                                         Node.location = ~@"2:10-3:4";
                                                         value =
                                                           Expression.Name
                                                             (Name.Attribute
                                                                {
                                                                  Name.Attribute.base =
                                                                    {
                                                                      Node.value =
                                                                        Expression.Name
                                                                          (Name.Identifier "xs");
                                                                      location = ~@"3:2-3:4";
                                                                    };
                                                                  attribute = "__aiter__";
                                                                  origin =
                                                                    Some
                                                                      {
                                                                        Origin.location =
                                                                          ~@"3:2-3:4";
                                                                        kind = Origin.ForIter;
                                                                      };
                                                                });
                                                       };
                                                     arguments = [];
                                                     origin =
                                                       Some
                                                         {
                                                           Origin.location = ~@"3:2-3:4";
                                                           kind = Origin.ForIter;
                                                         };
                                                   };
                                             };
                                           attribute = "__anext__";
                                           origin =
                                             Some
                                               {
                                                 Origin.location = ~@"3:2-3:4";
                                                 kind = Origin.ForNext;
                                               };
                                         });
                                };
                              arguments = [];
                              origin = Some { Origin.location = ~@"3:2-3:4"; kind = Origin.ForNext };
                            };
                        location = ~@"2:10-3:4";
                      };
                  location = ~@"2:10-3:4";
                };
          };
      location = ~@"2:10-3:4";
    };
  ()


let test_assume _ =
  assert_equal
    (Statement.assume ~origin:None (+Expression.Constant Constant.True))
    (+Statement.Assert
        { Assert.test = +Expression.Constant Constant.True; message = None; origin = None })


let test_pp _ =
  let assert_pretty_print ~expected source =
    let pretty_print_expected =
      expected |> String.lstrip ~drop:(Char.equal '\n') |> Test.trim_extra_indentation
    in
    let source = Test.trim_extra_indentation source |> String.rstrip ~drop:(Char.equal '\n') in
    let pretty_print_of_source =
      source
      |> String.split_on_chars ~on:['\n']
      |> PyreMenhirParser.Parser.parse_exn
      |> List.map ~f:show
      |> String.concat ~sep:"\n"
      |> String.rstrip ~drop:(Char.equal '\n')
      |> Test.trim_extra_indentation
    in
    assert_equal
      ~printer:Fn.id
      ~pp_diff:(Test.diff ~print:(fun formatter -> Format.fprintf formatter "%s"))
      pretty_print_expected
      pretty_print_of_source
  in
  (* Test 1 : simple def *)
  assert_pretty_print
    {|
      def foo(bar):
        x = "hello world"
    |}
    ~expected:{|
      def foo(bar):
        x = "hello world"
    |};

  (* Test 2 : def with multiple decorators *)
  assert_pretty_print
    {|
      @decorator1
      @decorator2
      def foo(bar):
        x = "hello world"
    |}
    ~expected:
      {|
      @(decorator1, decorator2)
      def foo(bar):
        x = "hello world"
    |};

  (* Test 3 : multiple defs and statements *)
  assert_pretty_print
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
    ~expected:
      {|
      @(decorator1, decorator2)
      def foo(bar):
        x = "hello world"

      @(decorator3)
      def foo(baz):
        x = "hello squirrel"
        y = 5
    |};

  (* Test 4 : cover classes, for loops, compound statements *)
  assert_pretty_print
    {|
      @class_decorator
      class Foo(Bar):
        def baz(quux):
          for i in xrange(quux):
            i = 1
          i = 2
    |}
    ~expected:
      {|
      @(class_decorator)
      class Foo(Bar):
        def Foo#baz(quux):
          for i in xrange(quux):
            i = 1
          i = 2
      |};

  (* Test 5 : try/except/finally blocks *)
  assert_pretty_print
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
    ~expected:
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
    |};

  (* Test 6 : while and if/then/else and list access *)
  assert_pretty_print
    {|
      while x:
        i = 1
        if i > 0:
          i = 1
        else:
          i = 2
        j = 2
      i[j] = 3
      i[j::1] = i[:j]
    |}
    ~expected:
      {|
      while x:
        i = 1
        if i > 0:
          i = 1
        else:
          i = 2
        j = 2
      i[j] = 3
      i[j::1] = i[:j]
    |};
  assert_pretty_print
    {|
      @some.decorator('with_a_string')
      def decorator_test():
        return 5
    |}
    ~expected:
      {|
      @(some.decorator("with_a_string"))
      def decorator_test():
        return 5
    |};
  assert_pretty_print {|
      global a
    |} ~expected:{|
      global a
    |};
  (* Nested functions.

     Note that the newline after each nested function actually has whitespace to keep it on the same
     indent as the other statements in the outer function. *)
  assert_pretty_print
    {|
      def foo(bar):
        def inner(bar):
          y = "hello world"
        def inner2(bar):
          y = "hello world"
          def inner3(bar):
            y = "hello world"
            z = "hello world"
          def inner4(bar):
            y = "hello world"
        x = "hello world"
    |}
    ~expected:
      ({|
      def foo(bar):
        def inner(bar):
          y = "hello world"|}
      ^ "\n  "
      ^ {|
        def inner2(bar):
          y = "hello world"
          def inner3(bar):
            y = "hello world"
            z = "hello world"|}
      ^ "\n    "
      ^ {|
          def inner4(bar):
            y = "hello world"|}
      ^ "\n    "
      ^ "\n  "
      ^ {|
        x = "hello world"
        |});
  ()


let test_is_generator context =
  let assert_is_generator ~expected source =
    let { Source.statements; _ } = Test.parse ~handle:"test.py" source in
    let actual = Ast.Statement.is_generator statements in
    assert_equal ~ctxt:context ~cmp:Bool.equal ~printer:Bool.to_string expected actual
  in

  assert_is_generator "yield" ~expected:true;
  assert_is_generator "yield from []" ~expected:true;
  assert_is_generator "x = 2" ~expected:false;
  assert_is_generator "x = yield 2" ~expected:true;
  assert_is_generator "assert (yield True)" ~expected:true;
  assert_is_generator "raise (yield True)" ~expected:true;
  assert_is_generator "return (yield True)" ~expected:true;
  assert_is_generator ~expected:true {|
      for x in y:
        yield
    |};
  assert_is_generator ~expected:true {|
      if x:
        yield
    |};
  assert_is_generator ~expected:true {|
      if x:
        return
      else:
        yield
    |};
  assert_is_generator ~expected:true {|
      while x:
        yield
    |};
  assert_is_generator ~expected:true {|
      try:
        pass
      except:
        yield
    |};
  assert_is_generator ~expected:true {|
      with foo() as bar:
        yield
    |};
  assert_is_generator ~expected:false {|
      def bar():
        yield
    |};
  assert_is_generator
    ~expected:false
    {|
      class A:
        def bar(self):
          yield
    |};
  assert_is_generator ~expected:false {|
      if x:
        def bar():
          yield
    |};
  assert_is_generator
    ~expected:false
    {|
      with foo() as bar:
        class A:
          def baz(self):
            yield
    |};
  assert_is_generator
    ~expected:false
    {|
      try:
        def foo():
          yield
      finally:
        class A:
          def bar(self):
            yield
    |};
  ()


let test_decorator_from_expression context =
  let assert_decorator ~expected expression =
    let actual = Decorator.from_expression expression in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: Decorator.t option]
      ~printer:(fun decorator ->
        Format.asprintf "%a" Sexp.pp_hum ([%sexp_of: Decorator.t option] decorator))
      expected
      actual
  in

  assert_decorator !"a" ~expected:(Some (decorator "a"));
  assert_decorator !"a.b" ~expected:(Some (decorator "a.b"));
  assert_decorator
    (+Expression.Call { Call.callee = !"a"; arguments = []; origin = None })
    ~expected:(Some (decorator "a" ~arguments:[]));
  assert_decorator
    (+Expression.Call { Call.callee = !"a.b"; arguments = []; origin = None })
    ~expected:(Some (decorator "a.b" ~arguments:[]));
  assert_decorator
    (+Expression.Call
        {
          Call.callee = !"a";
          arguments = [{ Call.Argument.name = None; value = !"b" }];
          origin = None;
        })
    ~expected:(Some (decorator "a" ~arguments:[{ Call.Argument.name = None; value = !"b" }]));

  assert_decorator (+Expression.Tuple []) ~expected:None;
  assert_decorator
    (+Expression.UnaryOperator
        { UnaryOperator.operator = UnaryOperator.Not; operand = !"a"; origin = None })
    ~expected:None;
  assert_decorator
    (+Expression.ComparisonOperator
        {
          ComparisonOperator.left = !"a";
          operator = ComparisonOperator.LessThan;
          right = !"b";
          origin = None;
        })
    ~expected:None;
  assert_decorator
    (+Expression.Name
        (Name.Attribute
           { Name.Attribute.base = +Expression.List []; attribute = "b"; origin = None }))
    ~expected:None;
  assert_decorator
    (+Expression.Call { Call.callee = +Expression.List [!"a"]; arguments = []; origin = None })
    ~expected:None;
  ()


let () =
  "define"
  >::: [
         "is_method" >:: test_is_method;
         "classmethod" >:: test_is_classmethod;
         "is_class_property" >:: test_is_class_property;
         "decorator" >:: test_decorator;
         "is_constructor" >:: test_is_constructor;
         "pyre_dump" >:: test_pyre_dump;
       ]
  |> Test.run;
  "class" >::: ["constructor" >:: test_constructor; "defines" >:: test_defines] |> Test.run;

  "statement"
  >::: [
         "assume" >:: test_assume;
         "with_block_preamble" >:: test_with_block_preamble;
         "try_block_preamble" >:: test_try_block_preamble;
         "for_loop_preamble" >:: test_for_loop_preamble;
         "pp" >:: test_pp;
         "is_generator" >:: test_is_generator;
         "decorator_from_expression" >:: test_decorator_from_expression;
       ]
  |> Test.run
