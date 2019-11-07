(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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
      Define.signature =
        {
          name = !&name;
          parameters = [];
          decorators = [];
          docstring = None;
          return_annotation = None;
          async = false;
          generator = false;
          parent = parent >>| Reference.create;
          nesting_define = None;
        };
      body = [+Statement.Pass];
    }
  in
  assert_true (Define.is_method (define ~name:"path.source.foo" ~parent:(Some "path.source")));
  assert_false (Define.is_method (define ~name:"foo" ~parent:None))


let test_is_classmethod _ =
  let define name decorators =
    {
      Define.signature =
        {
          name = !&name;
          parameters = [];
          decorators;
          docstring = None;
          return_annotation = None;
          async = false;
          generator = false;
          parent = Some !&"bar";
          nesting_define = None;
        };
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
          name = !&name;
          parameters = [];
          decorators;
          docstring = None;
          return_annotation = None;
          async = false;
          generator = false;
          parent = Some !&"bar";
          nesting_define = None;
        };
      body = [+Statement.Pass];
    }
  in
  assert_false (Define.is_class_property (define "foo" []));
  assert_false (Define.is_class_property (define "__init__" []));
  assert_true (Define.is_class_property (define "foo" [!"pyre_extensions.classproperty"]));
  assert_false (Define.is_class_property (define "__new__" []))


let test_decorator _ =
  let define decorators =
    {
      Define.signature =
        {
          name = !&"foo";
          parameters = [];
          decorators;
          docstring = None;
          return_annotation = None;
          async = false;
          generator = false;
          parent = None;
          nesting_define = None;
        };
      body = [+Statement.Pass];
    }
  in
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
  assert_true (Define.is_overloaded_function (define [!"overload"]));
  assert_true (Define.is_overloaded_function (define [!"typing.overload"]));
  assert_true (Define.is_property_setter (define [!"foo.setter"]));
  assert_false (Define.is_property_setter (define [!"setter"]));
  assert_false (Define.is_property_setter (define [!"bar.setter"]))


let test_is_constructor _ =
  let assert_is_constructor ?(in_test = false) ~name ?(parent = None) expected =
    let define =
      {
        Define.signature =
          {
            name = !&name;
            parameters = [];
            decorators = [];
            docstring = None;
            return_annotation = None;
            async = false;
            generator = false;
            parent = parent >>| Reference.create;
            nesting_define = None;
          };
        body = [+Statement.Pass];
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
  assert_constructor {|
      class Foo:
        pass
    |} ~exists:false


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
    let method_id = method_name in
    match Class.find_define definition ~method_name:method_id with
    | Some define when exists ->
        assert_equal define.Node.value.Define.signature.name !&method_id ~printer:Reference.show
    | None when not exists -> ()
    | Some { Node.value = { Define.signature = { name; _ }; _ }; _ } ->
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


let test_attributes _ =
  let create_attribute
      ~annotation
      ?(frozen = false)
      ?(implicit = true)
      ?(location = Location.Reference.any)
      ~name
      ?(primitive = false)
      ?(toplevel = true)
      ~value
      ()
    =
    { Attribute.kind = Simple { annotation; frozen; implicit; primitive; toplevel; value }; name }
    |> Node.create ~location
  in
  (* Test define field assigns. *)
  let assert_implicit_attributes source expected =
    let expected =
      let attribute (name, annotation, value, toplevel) =
        create_attribute
          ~name
          ~annotation:(annotation >>| Type.expression)
          ~value:(value >>| parse_single_expression)
          ~toplevel
          ~primitive:true
          ()
      in
      List.map expected ~f:attribute
    in
    let definition =
      { Class.name = !&""; bases = []; body = []; decorators = []; docstring = None }
    in
    assert_equal
      ~cmp:(List.equal Attribute.equal)
      ~printer:(fun attributes -> List.map ~f:Attribute.show attributes |> String.concat ~sep:"\n")
      expected
      ( parse_single_define source
      |> Define.implicit_attributes ~definition
      |> Identifier.SerializableMap.bindings
      |> List.map ~f:snd )
  in
  assert_implicit_attributes "def foo(): pass" [];
  assert_implicit_attributes
    {|
      def foo():
        self.attribute = value
        self.attribute: int = value
    |}
    ["attribute", Some Type.integer, Some "value", true];
  assert_implicit_attributes
    {|
      def foo():
        self.attribute: int = value
        self.attribute = value
    |}
    ["attribute", Some Type.integer, Some "value", true];
  assert_implicit_attributes
    {|
      def foo():
        self.attribute: int = value
        self.attribute: str = value
    |}
    ["attribute", Some (Type.Union [Type.string; Type.integer]), Some "value", true];
  assert_implicit_attributes
    {|
      def foo():
        self.attribute, self.other = derp()
    |}
    ["attribute", None, Some "derp()", true; "other", None, Some "derp()", true];
  assert_implicit_attributes
    {|
      def foo(self, derp: int):
        self.attribute, self.other = derp
    |}
    ["attribute", None, Some "derp", true; "other", None, Some "derp", true];
  assert_implicit_attributes
    {|
      def foo(self, argument: str):
        self.attribute = argument
    |}
    ["attribute", None, Some "argument", true];
  assert_implicit_attributes
    {|
      def foo(self, attribute: str):
        self.attribute = attribute
    |}
    ["attribute", Some Type.string, Some "attribute", true];
  assert_implicit_attributes
    {|
      def foo(self, attribute: MyType):
        self.attribute = attribute
    |}
    ["attribute", Some (Type.Primitive "MyType"), Some "attribute", true];
  assert_implicit_attributes
    {|
      def foo(self, attribute: str, test: bool):
        if test:
          self.attribute = attribute
    |}
    ["attribute", None, Some "attribute", false];
  assert_implicit_attributes
    {|
      def foo(self, argument: str):
        self.argument = unknown
    |}
    ["argument", None, Some "unknown", true];

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
      "attribute", None, Some "value", true;
      "nested", None, Some "value", false;
      "other", None, Some "value", false;
    ];

  (* `self` isn't special cased if a self parameter is passed into the function. *)
  assert_implicit_attributes
    {|
    def foo(renamed_self):
      renamed_self.attribute: int = value
  |}
    ["attribute", Some Type.integer, Some "value", true];

  (* Test class attributes. *)
  let assert_attributes ?(in_test = false) ?(include_generated_attributes = true) source expected =
    let expected =
      let attribute
          (name, location, annotation, value, setter, number_of_defines, property, class_property)
        =
        let location =
          match location with
          | None -> None
          | Some ((start_line, start_column), (stop_line, stop_column)) ->
              Some
                {
                  Location.path = Reference.empty;
                  start = { Location.line = start_line; column = start_column };
                  stop = { Location.line = stop_line; column = stop_column };
                }
        in
        let kind =
          if number_of_defines > 0 then
            let define =
              {
                Define.Signature.name = !&"foo";
                parameters = [];
                decorators = [];
                docstring = None;
                return_annotation = Some !"int";
                async = false;
                generator = false;
                parent = None;
                nesting_define = None;
              }
            in
            let signatures = List.init ~f:(fun _ -> define) number_of_defines in
            Attribute.Method { signatures; static = false; final = false }
          else if property then
            let kind =
              if setter then
                Attribute.ReadWrite
                  {
                    getter_annotation = value >>| parse_single_expression;
                    setter_annotation = annotation >>| Type.expression;
                  }
              else
                Attribute.ReadOnly { getter_annotation = annotation >>| Type.expression }
            in
            Attribute.Property { kind; async = false; class_property }
          else
            Attribute.Simple
              {
                annotation = annotation >>| Type.expression;
                primitive = true;
                value = value >>| parse_single_expression;
                frozen = false;
                toplevel = true;
                implicit = false;
              }
        in
        { Attribute.kind; name }
        |> Node.create ~location:(Option.value location ~default:Location.Reference.any)
      in
      List.map expected ~f:attribute
    in
    let printer attributes = List.map attributes ~f:Attribute.show |> String.concat ~sep:"\n\n" in
    let equal
        { Node.value = { Attribute.kind = left; name = left_name }; location = left_location }
        { Node.value = { Attribute.kind = right; name = right_name }; location = right_location }
      =
      let open Attribute in
      let equal_kind =
        match left, right with
        | Simple left, Simple right ->
            Option.equal Expression.equal left.annotation right.annotation
            && Option.equal Expression.equal left.value right.value
        | Method left, Method right ->
            Int.equal (left.signatures |> List.length) (right.signatures |> List.length)
        | _ -> Attribute.equal_kind left right
      in
      equal_kind
      && String.equal left_name right_name
      && ( Location.equal left_location Location.Reference.any
         || Location.equal left_location right_location )
    in
    assert_equal
      ~cmp:(List.equal equal)
      ~printer
      expected
      ( parse_single_class source
      |> Class.AttributeComponents.create
      |> Class.attributes ~in_test ~include_generated_attributes
      |> Identifier.SerializableMap.bindings
      |> List.map ~f:snd )
  in
  let attribute
      ~name
      ?location
      ?annotation
      ?value
      ?(number_of_defines = 0)
      ?(property = false)
      ?(setter = false)
      ?(class_property = false)
      ()
    =
    name, location, annotation, value, setter, number_of_defines, property, class_property
  in
  assert_attributes
    {|
      class Foo:
        Foo.attribute: int = value
    |}
    [attribute ~name:"attribute" ~annotation:Type.integer ~value:"value" ()];
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
      attribute ~name:"__init__" ~number_of_defines:1 ();
      attribute ~name:"attribute" ~annotation:Type.integer ~value:"value" ();
      attribute ~name:"ignored" ~value:"ignored" ();
      attribute ~name:"implicit" ~value:"implicit" ();
    ];
  assert_attributes
    {|
      class Foo:
        @overload
        def Foo.f(self, x: int) -> int: ...
        def Foo.f(self, x: str) -> str: ...
    |}
    [attribute ~name:"f" ~number_of_defines:2 ()];
  assert_attributes
    {|
      class Foo:
        def Foo.__init__(self):
          self.attribute = value  # Prioritize explicit declaration
        Foo.attribute: int = value
    |}
    [
      attribute ~name:"__init__" ~number_of_defines:1 ();
      attribute ~name:"attribute" ~annotation:Type.integer ~value:"value" ();
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
      attribute ~name:"__init__" ~number_of_defines:1 ();
      attribute ~name:"attribute" ~annotation:Type.integer ~value:"value" ();
      attribute ~name:"init" ~number_of_defines:1 ();
      attribute ~name:"not_inlined" ~number_of_defines:1 ();
    ];
  assert_attributes
    {|
      class Foo:
        @property
        def Foo.property(self) -> int:
          pass
    |}
    [attribute ~name:"property" ~annotation:Type.integer ~property:true ()];
  assert_attributes
    {|
      class Foo:
        @property
        def Foo.property(self) -> int: ...
    |}
    [attribute ~name:"property" ~annotation:Type.integer ~property:true ()];
  assert_attributes
    {|
      class Foo:
        class Foo.Bar:  # no preprocessing in tests
          ...
    |}
    [
      attribute
        ~name:"Bar"
        ~annotation:(Type.class_variable (Type.meta (Type.Primitive "Foo.Bar")))
        ();
    ];
  assert_attributes
    {|
      class Foo:
        @property
        def Foo.x(self) -> int: ...
        @x.setter
        def Foo.x(self, value:str) -> None: ...
    |}
    [attribute ~name:"x" ~annotation:Type.string ~value:"int" ~property:true ~setter:true ()];

  assert_attributes
    {|
      class Foo:
        @pyre_extensions.classproperty
        def Foo.property(self) -> int: ...
    |}
    [attribute ~name:"property" ~annotation:Type.integer ~property:true ~class_property:true ()];

  (* Simultaneous assignment *)
  assert_attributes
    {|
      class Foo:
        Foo.a, Foo.b = 1, 2
     |}
    [
      "a", None, None, Some "1", false, 0, false, false;
      "b", None, None, Some "2", false, 0, false, false;
    ];
  assert_attributes
    {|
      class Foo:
        Foo.a, Foo.b = list(range(2))
    |}
    [
      "a", None, None, Some "list(range(2))[0]", false, 0, false, false;
      "b", None, None, Some "list(range(2))[1]", false, 0, false, false;
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
      attribute ~name:"attribute" ~value:"value" ();
      attribute ~name:"setUp" ~number_of_defines:1 ();
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
      attribute ~name:"attribute" ~value:"value" ();
      attribute ~name:"context" ~value:"value" ();
      attribute ~name:"setUp" ~number_of_defines:1 ();
      attribute ~name:"with_context" ~number_of_defines:1 ();
    ];

  (* __slot__ attributes *)
  assert_attributes
    {|
      class Foo:
        __slots__ = ['attribute']
    |}
    [attribute ~name:"attribute" ()];
  assert_attributes
    {|
      class Foo:
        __slots__ = ['name', 'identifier']
    |}
    [attribute ~name:"identifier" (); attribute ~name:"name" ()];
  assert_attributes
    {|
      class Foo:
        Foo.x, Foo.y = 1, 2
    |}
    [
      attribute ~location:((3, 2), (3, 7)) ~name:"x" ~value:"1" ();
      attribute ~location:((3, 9), (3, 14)) ~name:"y" ~value:"2" ();
    ]


let test_preamble _ =
  let assert_preamble block preamble =
    let block =
      match parse_single_statement block with
      | { Node.value = With block; _ } -> block
      | _ -> failwith "Could not parse `with` statement."
    in
    let { Source.statements = preamble; _ } = parse ~coerce_special_methods:true preamble in
    assert_equal
      ~cmp:(List.equal Statement.equal)
      ~printer:(fun statements -> List.map ~f:show statements |> String.concat ~sep:", ")
      preamble
      (With.preamble block)
  in
  assert_preamble "with item: pass" "item";
  assert_preamble "with item, other: pass" "item; other";
  assert_preamble "with item as name: pass" "name = item.__enter__()";
  assert_preamble "async with item as name: pass" "name = await item.__aenter__()";
  let assert_preamble block preamble =
    let block =
      match parse_single_statement block with
      | { Node.value = For block; _ } -> block
      | _ -> failwith "Could not parse `for` statement."
    in
    let { Source.statements = preamble; _ } = parse ~coerce_special_methods:true preamble in
    assert_equal
      ~cmp:(List.equal Statement.equal)
      ~printer:(fun statements -> List.map ~f:show statements |> String.concat ~sep:", ")
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
      ~cmp:(List.equal (List.equal Statement.equal))
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
    ["error=...\nassert isinstance(error, typing.Union[IOError, ValueError])"]


let test_assume _ =
  assert_equal
    (Statement.assume (+Expression.True))
    (+Statement.Assert
        { Assert.test = +Expression.True; message = None; origin = Assert.Origin.Assertion })


let test_terminates _ =
  assert_true
    ( parse {|
        x = 1
        return x
     |}
    |> fun source -> Statement.terminates source.Source.statements );
  assert_true
    ( parse {|
       x = 1
       raise
    |}
    |> fun source -> Statement.terminates source.Source.statements );
  assert_false
    ( parse {|
         if x:
          return x
         x = 1
     |}
    |> fun source -> Statement.terminates source.Source.statements )


let test_docstring _ =
  assert_equal
    ( parse_single_statement
        {|
         def foo():
           """doc
              string
               end"""
           pass
    |}
    |> function
    | { Node.value = Define { Define.signature = { docstring; _ }; _ }; _ } -> docstring
    | _ -> None )
    (Some "doc\nstring\n end")


let test_pp _ =
  let assert_pretty_print ~expected source =
    let pretty_print_expected =
      expected |> String.lstrip ~drop:(( = ) '\n') |> Test.trim_extra_indentation
    in
    let source = Test.trim_extra_indentation source |> String.rstrip ~drop:(( = ) '\n') in
    let pretty_print_of_source =
      source
      |> String.split_on_chars ~on:['\n']
      |> Parser.parse
      |> List.map ~f:show
      |> String.concat ~sep:"\n"
      |> String.rstrip ~drop:(( = ) '\n')
    in
    assert_equal ~printer:Fn.id pretty_print_expected pretty_print_of_source
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
      i.__setitem__(j, 3)
      i.__setitem__(slice(j, None, 1), i[slice(None, j, None)])
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
    |}


let () =
  "define"
  >::: [
         "is_method" >:: test_is_method;
         "classmethod" >:: test_is_classmethod;
         "is_class_property" >:: test_is_class_property;
         "decorator" >:: test_decorator;
         "is_constructor" >:: test_is_constructor;
         "dump" >:: test_dump;
       ]
  |> Test.run;
  "class"
  >::: [
         "constructor" >:: test_constructor;
         "defines" >:: test_defines;
         "attributes" >:: test_attributes;
       ]
  |> Test.run;

  "statement"
  >::: [
         "assume" >:: test_assume;
         "preamble" >:: test_preamble;
         "terminates" >:: test_terminates;
         "pp" >:: test_pp;
       ]
  |> Test.run
