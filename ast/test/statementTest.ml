(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Expression
open Pyre
open PyreParser
open Statement

open Test


let test_is_method _ =
  let define ~name ~parent =
    {
      Define.name = Access.create name;
      parameters = [];
      body = [+Pass];
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
      parent = parent >>| Access.create;
    }
  in
  assert_true (Define.is_method (define ~name:"path.source.foo" ~parent:(Some "path.source")));
  assert_false (Define.is_method (define ~name:"foo" ~parent:None))


let test_is_classmethod _ =
  let define name decorators =
    {
      Define.name = Access.create name;
      parameters = [];
      body = [+Pass];
      decorators;
      docstring = None;
      return_annotation = None;
      async = false;
      parent = Some (Access.create "bar");
    } in

  assert_false (Define.is_class_method (define "foo" []));
  assert_false (Define.is_class_method (define "__init__" []));
  assert_true (Define.is_class_method (define "foo" [!"classmethod"]));
  assert_true (Define.is_class_method (define "__init_subclass__" []));
  assert_true (Define.is_class_method (define "__new__" []));
  assert_true (Define.is_class_method (define "__class_getitem__" []))


let test_is_class_property _ =
  let define name decorators =
    {
      Define.name = Access.create name;
      parameters = [];
      body = [+Pass];
      decorators;
      docstring = None;
      return_annotation = None;
      async = false;
      parent = Some (Access.create "bar");
    }
  in
  assert_false (Define.is_class_property (define "foo" []));
  assert_false (Define.is_class_property (define "__init__" []));
  assert_true (Define.is_class_property (define "foo" [!"pyre_extensions.classproperty"]));
  assert_false (Define.is_class_property (define "__new__" []))


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
      parent = None;
    } in

  assert_false (Define.is_static_method (define []));
  assert_false (Define.is_static_method (define [!"foo"]));
  assert_true (Define.is_static_method (define [!"staticmethod"]));
  assert_true (Define.is_static_method (define [!"foo"; !"staticmethod"]));

  assert_false (Define.is_abstract_method (define []));
  assert_false (Define.is_abstract_method (define [!"foo"]));
  assert_true (Define.is_abstract_method (define [!"abstractmethod"]));
  assert_true (Define.is_abstract_method (define [!"foo"; !"abstractmethod"]));
  assert_true (Define.is_abstract_method (define [!"abc.abstractmethod"]));
  assert_true (Define.is_abstract_method (define [!"abstractproperty"]));
  assert_true (Define.is_abstract_method (define [!"abc.abstractproperty"]));

  assert_true (Define.is_overloaded_method (define [!"overload"]));
  assert_true (Define.is_overloaded_method (define [!"typing.overload"]));

  assert_true ((Define.is_property_setter) (define [!"foo.setter"]));
  assert_false ((Define.is_property_setter) (define [!"setter"]));
  assert_false ((Define.is_property_setter) (define [!"bar.setter"]))


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
        parent;
      }
    in
    assert_equal expected (Define.is_constructor ~in_test define)
  in
  assert_is_constructor ~name:"Foo.__init__" ~parent:(Some "Foo") true;
  assert_is_constructor ~in_test:true ~name:"Foo.setUp" ~parent:(Some "Foo") true;
  assert_is_constructor ~in_test:true ~name:"Foo.async_setUp" ~parent:(Some "Foo") true;
  assert_is_constructor ~in_test:false ~name:"Foo.async_setUp" ~parent:(Some "Foo") false;
  assert_is_constructor ~in_test:true ~name:"Foo.with_context" ~parent:(Some "Foo") true;
  assert_is_constructor ~name:"__init__" false;
  assert_is_constructor ~name:"Foo.bar" ~parent:(Some "Foo") false


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
  assert_constructor
    {|
      class Foo:
        pass
    |}
    ~exists:false


let test_defines _ =
  let assert_define source ~method_name ~exists ~total =
    let definition =
      match parse_single_statement source with
      | { Node.value = Class definition; _ } -> definition
      | _ -> failwith "Could not parse class"
    in
    assert_equal
      total
      (List.length (Class.defines definition))
      ~msg:"Wrong number of defines"
      ~printer:Int.to_string;
    let method_id = Identifier.create method_name in
    match Class.find_define definition ~method_name:method_id with
    | Some define when exists ->
        assert_equal
          define.Node.value.Define.name
          [Access.Identifier method_id]
          ~printer:Access.show
    | None when not exists ->
        ()
    | Some { Node.value = { Define.name; _ }; _ } ->
        Format.asprintf
          "method %a found when not expected (looking for %s)"
          Access.pp name
          method_name
        |> assert_failure
    | None ->
        Format.sprintf "method %s not found when expected" method_name
        |> assert_failure
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


let test_attributes _ =
  let create_attribute
      ?(primitive = false)
      ?(property = false)
      ?(setter = false)
      ~target
      ~annotation
      ?defines
      ~value
      () =
    {
      Attribute.target = Expression.Access.expression (Access.create target);
      annotation;
      defines;
      value;
      async = false;
      setter;
      property;
      primitive;
    }
    |> Node.create_with_default_location
  in
  (* Test define field assigns. *)
  let assert_implicit_attributes source expected =
    let expected =
      let attribute (target, annotation, value) =
        create_attribute
          ~target
          ~annotation
          ~value
          ~primitive:true
          ()
      in
      List.map expected ~f:attribute
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
      ~cmp:(List.equal ~equal:Attribute.equal)
      ~printer:(fun attributes -> List.map ~f:Attribute.show attributes |> String.concat ~sep:"\n")
      expected
      (parse_single_define source
       |> Define.implicit_attributes ~definition
       |> Access.SerializableMap.bindings
       |> List.map ~f:snd)
  in
  assert_implicit_attributes "def foo(): pass" [];

  assert_implicit_attributes
    {|
      def foo():
        self.attribute = value
        self.attribute: int = value
    |}
    ["attribute", Some (Type.expression Type.integer), Some !"value"];
  assert_implicit_attributes
    {|
      def foo():
        self.attribute: int = value
        self.attribute = value
    |}
    ["attribute", Some (Type.expression Type.integer), Some !"value"];
  assert_implicit_attributes
    {|
      def foo():
        self.attribute: int = value
        self.attribute: str = value
    |}
    ["attribute", Some (Type.expression (Type.Union [Type.string; Type.integer])), Some !"value"];

  assert_implicit_attributes
    {|
      def foo():
        self.attribute, self.other = derp()
    |}
    [
      "attribute",
      None,
      Some (parse_single_expression "derp()");
      "other",
      None,
      Some (parse_single_expression "derp()");
    ];
  assert_implicit_attributes
    {|
      def foo(self, derp: int):
        self.attribute, self.other = derp
    |}
    [
      "attribute",
      None,
      Some (parse_single_expression "derp");
      "other",
      None,
      Some (parse_single_expression "derp");
    ];

  assert_implicit_attributes
    {|
      def foo(self, argument: str):
        self.attribute = argument
    |}
    ["attribute", Some (Type.expression Type.string), Some !"argument";];

  assert_implicit_attributes
    {|
      def foo(self, argument: str):
        self.argument = unknown
    |}
    ["argument", None, Some !"unknown";];

  (* Implicit arguments in branches. *)
  assert_implicit_attributes
    {|
      def foo():
        a = 1
        self.attribute = value
        if True:
          self.other = value
          if False:
            self.nested = value
    |}
    [
      "attribute", None, Some !"value";
      "nested", None, Some !"value";
      "other", None, Some !"value";
    ];

  (* `self` isn't special cased if a self parameter is passed into the function. *)
  assert_implicit_attributes
    {|
    def foo(renamed_self):
      renamed_self.attribute: int = value
  |}
    ["attribute", Some (Type.expression Type.integer), Some !"value"];

  (* Test define field assigns. *)
  let assert_property_attribute source expected =
    let expected =
      expected
      >>| fun (target, annotation, value, setter) ->
      create_attribute ~setter ~target ~annotation ~value ~property:true ()
    in
    let define =
      let define = parse_single_define source in
      { define with Define.parent = Some (Access.create "Parent") }
    in
    assert_equal
      ~cmp:(Option.equal Attribute.equal)
      expected
      (Define.property_attribute define ~location:Location.Reference.any)
  in
  assert_property_attribute "def Parent.foo(): pass" None;
  assert_property_attribute "@property\ndef Parent.foo(): pass" (Some ("foo", None, None, false));
  assert_property_attribute
    {|
      @foo.setter
      def Parent.foo(self, value: int) -> None: pass
    |}
    (Some ("foo", Some (Type.expression Type.integer), None, true));
  assert_property_attribute
    {|
      @__property__
      def Parent.foo(): pass
    |}
    (Some ("foo", None, None, false));
  assert_property_attribute
    {|
      @abc.abstractproperty
      def Parent.foo() -> int: pass
    |}
    (Some ("foo", Some (Type.expression Type.integer), None, false));

  (* Test class attributes. *)
  let attribute ~target ?annotation ?value ?(setter = false) ?(number_of_defines = 0) () =
    let annotation =
      annotation
      >>| Type.expression
    in
    let value =
      value
      >>| Access.create
      >>| Access.expression
    in
    target, annotation, value, setter, number_of_defines
  in
  let assert_attributes
      ?(in_test = false)
      ?(include_generated_attributes = true)
      source
      expected =
    let expected =
      let attribute (target, annotation, value, setter, number_of_defines) =
        let defines =
          if number_of_defines > 0 then
            let define = {
              Statement.Define.name = Access.create "foo";
              parameters = [];
              body = [];
              decorators = [];
              docstring = None;
              return_annotation = Some !"int";
              async = false;
              parent = None;
            }
            in
            Some (List.init ~f:(fun _ -> define) number_of_defines)
          else
            None
        in
        create_attribute ~target ~annotation ?defines ~value ~setter ()
      in
      List.map expected ~f:attribute
    in
    let printer attributes =
      List.map attributes ~f:Attribute.show
      |> String.concat ~sep:"\n\n"
    in
    let equal { Node.value = left; _ } { Node.value = right; _ } =
      let open Attribute in
      left.async = right.async &&
      left.setter = right.setter &&
      Expression.equal left.target right.target &&
      Option.equal Expression.equal left.annotation right.annotation &&
      Option.equal Expression.equal left.value right.value &&
      Option.equal Int.equal (left.defines >>| List.length) (right.defines >>| List.length)
    in
    assert_equal
      ~cmp:(List.equal ~equal)
      ~printer
      expected
      (parse_single_class source
       |> Class.attributes ~in_test ~include_generated_attributes
       |> Access.SerializableMap.bindings
       |> List.map ~f:snd)
  in

  assert_attributes
    {|
      class Foo:
        Foo.attribute: int = value
    |}
    [
      attribute ~target:"attribute" ~annotation:Type.integer ~value:"value" ();
    ];
  assert_attributes
    {|
      class Foo:
        def Foo.__init__(self):
          self.implicit = implicit
          self.whatever()['asdf'] = 5
          if True:
            self.ignored = ignored
        Foo.attribute: int = value
        whatever()['asdf'] = 5
    |}
    [
      attribute ~target:"__init__" ~number_of_defines:1 ();
      attribute ~target:"attribute" ~annotation:Type.integer ~value:"value" ();
      attribute ~target:"ignored" ~value:"ignored" ();
      attribute ~target:"implicit" ~value:"implicit" ();
    ];
  assert_attributes
    {|
      class Foo:
        @overload
        def Foo.f(self, x: int) -> int: ...
        def Foo.f(self, x: str) -> str: ...
    |}
    [
      attribute ~target:"f" ~number_of_defines:2 ();
    ];
  assert_attributes
    {|
      class Foo:
        def Foo.__init__(self):
          self.attribute = value  # Prioritize explicit declaration
        Foo.attribute: int = value
    |}
    [
      attribute ~target:"__init__" ~number_of_defines:1 ();
      attribute ~target:"attribute" ~annotation:Type.integer ~value:"value" ();
    ];
  assert_attributes
    {|
      class Foo:
        def Foo.__init__(self):
          self.init()
        def Foo.init(self):
          self.attribute: int = value
        def Foo.not_inlined(self):
          self.other: int = 1
    |}
    [
      attribute ~target:"__init__" ~number_of_defines:1 ();
      attribute ~target:"attribute" ~annotation:Type.integer ~value:"value" ();
      attribute ~target:"init" ~number_of_defines:1 ();
      attribute ~target:"not_inlined" ~number_of_defines:1 ();
    ];
  assert_attributes
    {|
      class Foo:
        @property
        def Foo.property(self) -> int:
          pass
    |}
    [attribute ~target:"property" ~annotation:Type.integer ()];
  assert_attributes
    {|
      class Foo:
        @property
        def Foo.property(self) -> int: ...
    |}
    [attribute ~target:"property" ~annotation:Type.integer ()];
  assert_attributes
    {|
      class Foo:
        class Foo.Bar:  # no preprocessing in tests
          ...
    |}
    [
      attribute
        ~target:"Bar"
        ~annotation:(Type.class_variable (Type.meta (Type.primitive "Foo.Bar")))
        ();
    ];
  assert_attributes
    {|
      class Foo:
        @property.setter
        def Foo.property(self, value: str) -> None: ...
    |}
    [attribute ~target:"property" ~annotation:Type.string ~setter:true ()];
  assert_attributes
    {|
      class Foo:
        @property
        def Foo.x(self) -> int: ...
        @x.setter
        def Foo.x(self, value:str) -> None: ...
    |}
    [attribute ~target:"x" ~annotation:Type.string ~value:"int" ~setter:true ()];

  (* Simultaneous assignment *)
  assert_attributes
    {|
      class Foo:
        Foo.a, Foo.b = 1, 2
     |}
    [
      "a", None, Some (parse_single_expression "1"), false, 0;
      "b", None, Some (parse_single_expression "2"), false, 0;
    ];
  assert_attributes
    {|
      class Foo:
        Foo.a, Foo.b = list(range(2))
    |}
    [
      "a", None, Some (parse_single_expression "list(range(2))[0]"), false, 0;
      "b", None, Some (parse_single_expression "list(range(2))[1]"), false, 0;
    ];

  (* Implicit attributes in tests. *)
  assert_attributes
    ~in_test:true
    {|
      class Test:
        def Test.setUp(self):
          self.attribute = value
    |}
    [
      attribute ~target:"attribute" ~value:"value" ();
      attribute ~target:"setUp" ~number_of_defines:1 ();
    ];
  assert_attributes
    ~in_test:true
    {|
      class Test:
        def Test.setUp(self):
          self.attribute = value
        def Test.with_context(self):
          self.context = value
    |}
    [
      attribute ~target:"attribute" ~value:"value" ();
      attribute ~target:"context" ~value:"value" ();
      attribute ~target:"setUp" ~number_of_defines:1 ();
      attribute ~target:"with_context" ~number_of_defines:1 ();
    ];

  (* __slot__ attributes *)
  assert_attributes
    {|
      class Foo:
        __slots__ = ['attribute']
    |}
    [
      attribute ~target:"attribute" ();
    ];
  assert_attributes
    {|
      class Foo:
        __slots__ = ['name', 'identifier']
    |}
    [
      attribute ~target:"identifier" ();
      attribute ~target:"name" ();
    ]


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


let test_preamble _ =
  let assert_preamble block preamble =
    let block =
      match parse_single_statement block with
      | { Node.value = With block; _ } -> block
      | _ -> failwith "Could not parse `with` statement."
    in
    let { Source.statements = preamble; _ } = parse preamble in
    assert_equal
      ~cmp:(List.equal ~equal:Statement.equal)
      ~printer:(fun statements -> List.map ~f:Statement.show statements |> String.concat ~sep:", ")
      preamble
      (With.preamble block)
  in

  assert_preamble "with item: pass" "item"  ;
  assert_preamble "with item, other: pass" "item; other";
  assert_preamble "with item as name: pass" "name = item.__enter__()";
  assert_preamble "async with item as name: pass" "name = await item.__aenter__()";

  let assert_preamble block preamble =
    let block =
      match parse_single_statement block with
      | { Node.value = For block; _ } -> block
      | _ -> failwith "Could not parse `for` statement."
    in
    let { Source.statements = preamble; _ } = parse preamble in
    assert_equal
      ~cmp:(List.equal ~equal:Statement.equal)
      ~printer:(fun statements -> List.map ~f:Statement.show statements |> String.concat ~sep:", ")
      preamble
      [For.preamble block]
  in

  assert_preamble "for a in b: pass" "a = b.__iter__().__next__()";
  assert_preamble "for a, b in c: pass" "a, b = c.__iter__().__next__()";
  assert_preamble "for a in [1, 2, 3]: pass" "a = [1, 2, 3].__iter__().__next__()";
  assert_preamble "async for a in b: pass" "a = await b.__aiter__().__anext__()";

  let assert_preamble block preambles =
    let handlers =
      match parse_single_statement block with
      | { Node.value = Try { Try.handlers; _ }; _ } -> handlers
      | _ -> failwith "Could not parse `try` statement."
    in
    let preambles =
      let preamble source =
        let { Source.statements = preamble; _ } = parse source in
        preamble
      in
      List.map preambles ~f:preamble
    in
    let printer preambles =
      List.map
        preambles
        ~f:(fun preamble -> List.map ~f:Statement.show preamble |> String.concat ~sep:", ")
      |> String.concat ~sep:"\n"
    in
    assert_equal
      ~cmp:(List.equal ~equal:(List.equal ~equal:Statement.equal))
      ~printer
      preambles
      (List.map handlers ~f:Try.preamble)
  in

  assert_preamble "try: pass" [];
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
    ["error: Exception"];
  assert_preamble
    {|
      try:
        pass
      except IOError as error:
        pass
      except Exception as error:
        pass
    |}
    ["error: IOError"; "error: Exception"];
  assert_preamble
    {|
      try:
        pass
      except (IOError, ValueError) as error:
        pass
    |}
    ["error: typing.Union[IOError, ValueError]"]


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
  let assert_pretty_print ~expected source =
    let pretty_print_expected =
      expected
      |> String.lstrip ~drop:((=) '\n')
      |> Test.trim_extra_indentation
    in
    let source =
      Test.trim_extra_indentation source
      |> String.rstrip ~drop:((=) '\n')
    in
    let pretty_print_of_source =
      source
      |> String.split_on_chars ~on:['\n']
      |> Parser.parse
      |> List.map ~f:Statement.show
      |> String.concat ~sep:"\n"
      |> String.rstrip ~drop:((=) '\n')
    in
    assert_equal
      ~printer:Fn.id
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
      def #foo(bar):
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
    ~expected:{|
      @(decorator1, decorator2)
      def #foo(bar):
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
    ~expected:{|
      @(decorator1, decorator2)
      def #foo(bar):
        x = "hello world"

      @(decorator3)
      def #foo(baz):
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
    ~expected:{|
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
    ~expected:{|
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
      i[j] += 3
      i[j][7] = 8
      i[j::1] = i[:j]
    |}
    ~expected:{|
      while x:
        i = 1
        if i > 0:
          i = 1
        else:
          i = 2
        j = 2
      i.__setitem__(j,3)
      i.__setitem__(j,i[j].__add__(3))
      i[j].__setitem__(7,8)
      i.__setitem__(slice(j,None,1),i[slice(None,j,None)])
    |};

  assert_pretty_print
    "i[j] = 5 if 1 else 1"
    ~expected:"i.__setitem__(j,5 if 1 else 1)";

  assert_pretty_print
    "x = i[j] = y"
    ~expected:{|
      x = y
      i.__setitem__(j,y)
    |};

  assert_pretty_print
    "j[i] = x = i[j] = y"
    ~expected:{|
      j.__setitem__(i,y)
      x = y
      i.__setitem__(j,y)
    |};

  assert_pretty_print
    "x, i[j] = y"
    ~expected:{|
      (x, i[j]) = y
    |};

  assert_pretty_print
    " i[j] = x =  ... # type: Something"
    ~expected:{|
      i.__setitem__(j,...)
      x: "Something" = ...
    |};

  assert_pretty_print
    {|
      @some.decorator('with_a_string')
      def decorator_test():
        return 5
    |}
    ~expected:{|
      @(some.decorator("with_a_string"))
      def #decorator_test():
        return 5
    |}


let () =
  "define">:::[
    "is_method">::test_is_method;
    "classmethod">::test_is_classmethod;
    "is_class_property">::test_is_class_property;
    "decorator">::test_decorator;
    "is_constructor">::test_is_constructor;
    "dump">::test_dump;
  ]
  |> Test.run;
  "class">:::[
    "constructor">::test_constructor;
    "defines">::test_defines;
    "attributes">::test_attributes;
    "update">::test_update;
  ]
  |> Test.run;
  "statement">:::[
    "assume">::test_assume;
    "preamble">::test_preamble;
    "terminates">::test_terminates;
    "pp">::test_pp;
  ]
  |> Test.run
