(** Copyright 2016-present Facebook. All rights reserved. **)

open Core
open OUnit2

open Ast
open Expression
open Statement

open Test

let test_qualify _ =
  let assert_qualify ?(qualifier = "some/qualifier") source expected =
    let parse =
      parse ~qualifier:(Source.qualifier ~path:qualifier) in
    assert_source_equal
      (Preprocessing.qualify (parse source))
      (parse expected) in

  assert_qualify
    {|
      import a as b
      b.c
    |}
    {|
      import a as b
      a.c
    |};

  assert_qualify
    {|
      from a import b
      b.c
    |}
    {|
      from a import b
      a.b.c
    |};

  assert_qualify
    {|
      from a import b
      b()
    |}
    {|
      from a import b
      a.b()
    |};

  assert_qualify
    {|
      from a import b as c
      c.c
    |}
    {|
      from a import b as c
      a.b.c
    |};

  assert_qualify
    {|
      b.b
      import a as b
      a.b
    |}
    {|
      b.b
      import a as b
      a.b
    |};

  assert_qualify
    {|
      b.b
      if True:
        import a as b
      b.b
    |}
    {|
      b.b
      if True:
        import a as b
      a.b
    |};

  (* The import is never reached yet we still pick it up. Don't want to do
     analysis on this though. Not yet at least... *)
  assert_qualify
    {|
      b.b
      if False:
        import a as b
      b.b
    |}
    {|
      b.b
      if False:
        import a as b
      a.b
    |};

  assert_qualify
    ~qualifier:"some/qualifier"
    {|
      from . import b as c
      c.c
    |}
    {|
      from some import b as c
      some.b.c
    |};
  assert_qualify
    ~qualifier:"some/qualifier"
    {|
      from .derp import b as c
      c.c
    |}
    {|
      from some.derp import b as c
      some.derp.b.c
    |};
  assert_qualify
    ~qualifier:"some/qualifier"
    {|
      from .derp import a
      a.b
    |}
    {|
      from some.derp import a
      some.derp.a.b
    |};

  assert_qualify
    {|
      class Foo: pass
      f = Foo()
      class Bar: ...
    |}
    {|
      class some.qualifier.Foo: pass
      some.qualifier.f = some.qualifier.Foo()
      class some.qualifier.Bar: ...
    |};

  assert_qualify
    {|
      def bar():
        return foo()
      def foo(): pass
      def baz(): ...
    |}
    {|
      def some.qualifier.bar():
        return some.qualifier.foo()
      def some.qualifier.foo(): pass
      def some.qualifier.baz(): ...
    |};

  assert_qualify
    {|
      class Foo: pass
      def foo(foo: Foo) -> Foo: pass
    |}
    {|
      class some.qualifier.Foo: pass
      def some.qualifier.foo(foo: some.qualifier.Foo) -> some.qualifier.Foo:
        pass
    |};

  assert_qualify
    {|
      class Foo:
        def foo(): pass
      def bar():
        f = Foo()
        f.foo()
    |}
    {|
      class some.qualifier.Foo:
        def foo(): pass
      def some.qualifier.bar():
        f = some.qualifier.Foo()
        f.foo()
    |};

  assert_qualify
    {|
      constant = 1
      def foo():
        nonconstant = constant
    |}
    {|
      some.qualifier.constant = 1
      def some.qualifier.foo():
        nonconstant = some.qualifier.constant
    |};

  assert_qualify
    {|
      constant = ...
      def foo():
        nonconstant = constant
    |}
    {|
      some.qualifier.constant = ...
      def some.qualifier.foo():
        nonconstant = some.qualifier.constant
    |};

  assert_qualify
    {|
      class Foo:
        class Foo2:
          def foo(): pass
      def bar():
        f = Foo.Foo2()
        f.foo()
    |}
    {|
      class some.qualifier.Foo:
        class some.qualifier.Foo.Foo2:
          def foo(): pass
      def some.qualifier.bar():
        f = some.qualifier.Foo.Foo2()
        f.foo()
    |};

  assert_qualify
    {|
      def hello():
        class Foo2:
          def foo(): pass
        pass
    |}
    {|
      def some.qualifier.hello():
        class some.qualifier.hello.Foo2:
          def foo(): pass
        pass
    |};

  assert_qualify
    {|
      def hello():
        def foo(): pass
        pass
    |}
    {|
      def some.qualifier.hello():
        def some.qualifier.hello.foo(): pass
        pass
    |};

  assert_qualify
    {|
      list = []
      def hello():
        for i in list:
          def foo():
            pass
    |}
    {|
      some.qualifier.list = []
      def some.qualifier.hello():
        for i in some.qualifier.list:
          def some.qualifier.hello.foo():
            pass
    |};

  assert_qualify
    {|
      def hello():
        if True:
          def foo():
            pass
    |}
    {|
      def some.qualifier.hello():
        if True:
          def some.qualifier.hello.foo():
            pass
    |}


let test_expand_optional_assigns _ =
  let assert_expand source expected =
    assert_source_equal
      (Preprocessing.expand_optional_assigns (parse source))
      (parse expected)
  in
  assert_expand
    {|
      if x is None:
        pass
    |}
    {|
      if x is None:
        pass
      else:
        pass
    |};
  assert_expand
    {|
      if x is None:
        pass
      else:
        voodoo()
    |}
    {|
      if x is None:
        pass
      else:
        voodoo()
    |};
  assert_expand
    {|
      if x is None or a:
        pass
    |}
    {|
      if x is None or a:
        pass
      else:
        pass
    |}


let test_expand_operators _ =
  let assert_expand source expected =
    assert_source_equal
      (Preprocessing.expand_operators (parse source))
      (parse expected)
  in

  assert_expand
    {|
      a + b
    |}
    {|
      a.__add__(b)
    |};

  assert_expand
    {|
      a + b + c
    |}
    {|
      a.__add__(b).__add__(c)
    |};

  assert_expand
    {|
      def foo():
        a * b
    |}
    {|
      def foo():
        a.__mul__(b)
    |};

  assert_expand
    {|
      def foo():
        a * b.c
    |}
    {|
      def foo():
        a.__mul__(b.c)
    |};

  assert_expand
    {|
      def foo():
        a * b[i]
    |}
    {|
      def foo():
        a.__mul__(b[i])
    |}


let test_expand_returns _ =
  let assert_expand source expected =
    assert_source_equal
      (Preprocessing.expand_returns (Source.create source))
      (Source.create expected) in

  assert_expand
    [+Return None]
    [+Return None];

  assert_expand
    [+Return (Some !"foo")]
    [
      +Assign {
        Assign.target = !"$return";
        annotation = None;
        value = Some !"foo";
        compound = None;
        parent = None;
      };
      +Return (Some !"$return");
    ];

  assert_expand
    [
      +Define {
        Define.name = Instantiated.Access.create "foo";
        parameters = [];
        body = [+Return None];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ]
    [
      +Define {
        Define.name = Instantiated.Access.create "foo";
        parameters = [];
        body = [+Return None];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ];

  assert_expand
    [
      +Define {
        Define.name = Instantiated.Access.create "foo";
        parameters = [];
        body = [+Pass];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ]
    [
      +Define {
        Define.name = Instantiated.Access.create "foo";
        parameters = [];
        body = [+Pass; +Return None];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ];

  assert_expand
    [
      +Define {
        Define.name = Instantiated.Access.create "foo";
        parameters = [];
        body = [+Statement.Yield (+Expression.Yield None)];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ]
    [
      +Define {
        Define.name = Instantiated.Access.create "foo";
        parameters = [];
        body = [+Statement.Yield (+Expression.Yield None)];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ]


let test_expand_for _ =
  let assert_expand source expected =
    assert_source_equal
      (Preprocessing.expand_for_loop (parse source))
      (parse expected) in

  assert_expand
    {|
      for a in b:
        c
    |}
    {|
      for a in b:
        a = b.__iter__().__next__()
        c
    |};
  assert_expand
    {|
      for a, b in c:
        d
    |}
    {|
      for a, b in c:
        a, b = c.__iter__().__next__()
        d
    |};
  assert_expand
    {|
      for a in [1, 2, 3]:
        c
    |}
    {|
      for a in [1, 2, 3]:
        a = [1, 2, 3].__iter__().__next__()
        c
    |};
  assert_expand
    {|
      async for a in b:
        c
    |}
    {|
      async for a in b:
        a = b.__aiter__().__anext__()
        c
    |}


let test_expand_excepts _ =
  let assert_expand source expected =
    assert_source_equal
      (Preprocessing.expand_excepts (parse source))
      (parse expected) in
  assert_expand
    {|
      try:
        pass
      except Exception:
        pass
    |}
    {|
      try:
        pass
      except Exception:
        pass
    |};
  assert_expand
    {|
      try:
        pass
      except Exception as e:
        pass
    |}
    {|
      try:
        pass
      except Exception as e:
        e: Exception
        pass
    |};
  assert_expand
    {|
      try:
        pass
      except (E1, E2):
        pass
    |}
    {|
      try:
        pass
      except (E1, E2):
        pass
    |};
  assert_expand
    {|
      try:
        pass
      except (E1, E2) as e:
        pass
    |}
    {|
      try:
        pass
      except (E1, E2) as e:
        e: typing.Union[E1, E2]
        pass
    |}


let test_expand_ternary _ =
  let assert_expand source expected =
    assert_source_equal
      (Preprocessing.expand_ternary_assign (parse source))
      (parse expected) in

  assert_expand
    {|
      a = 5 if 1 else 3
    |}
    {|
      if 1:
        a = 5
      else:
        a = 3
    |}


let test_defines _ =
  let assert_defines statements defines =
    assert_equal
      ~cmp:(List.equal ~equal:(Define.equal Statement.equal))
      (Preprocessing.defines (Source.create statements)
       |> List.map ~f:Node.value)
      defines in

  let define =
    {
      Define.name = Instantiated.Access.create "foo";
      parameters = [
        +{
          Parameter.name = ~~"a";
          value = None;
          annotation = None;
        };
      ];
      body = [+Expression (+Float 1.0)];
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
      parent = None;
    }
  in
  let toplevel =
    {
      Define.name = Instantiated.Access.create "$toplevel";
      parameters = [];
      body = [+Define define];
      decorators = [];
      docstring = None;
      return_annotation = Some (Type.expression Type.void);
      async = false;
      parent = None;
    }
  in
  assert_defines
    [+Define define]
    [toplevel; define];

  let inner =
    {
      Define.name = Instantiated.Access.create "foo";
      parameters = [
        +{
          Parameter.name = ~~"a";
          value = None;
          annotation = None;
        };
      ];
      body = [+Expression (+Float 1.0)];
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
      parent = None;
    }
  in
  let define =
    {
      Define.name = Instantiated.Access.create "foo";
      parameters = [
        +{
          Parameter.name = ~~"a";
          value = None;
          annotation = None;
        };
      ];
      body = [+Expression (+Float 1.0); +Define inner];
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
      parent = None;
    }
  in
  let toplevel =
    {
      Define.name = Instantiated.Access.create "$toplevel";
      parameters = [];
      body = [+Define define];
      decorators = [];
      docstring = None;
      return_annotation = Some (Type.expression Type.void);
      async = false;
      parent = None;
    }
  in
  assert_defines
    [+Define define]
    [toplevel; define; inner]


let test_classes _ =
  let assert_classes statements class_defines =
    assert_equal
      ~cmp:(List.equal ~equal:(Class.equal Statement.equal))
      (Preprocessing.classes (Source.create statements)
       |> List.map ~f:Node.value)
      class_defines in

  let class_define =
    {
      Class.name = Instantiated.Access.create "foo";
      bases = [];
      body = [
        +Define {
          Define.name = Instantiated.Access.create "bar";
          parameters = [];
          body = [+Pass];
          decorators = [];
          docstring = None;
          return_annotation = None;
          async = false;
          parent = Some (Instantiated.Access.create "foo");
        };
      ];
      decorators = [];
      docstring = None;
    }
  in
  assert_classes
    [+Class class_define]
    [class_define];

  let inner =
    {
      Class.name = Instantiated.Access.create "bar";
      bases = [];
      body = [+Pass];
      decorators = [];
      docstring = None;
    }
  in
  let class_define =
    {
      Class.name = Instantiated.Access.create "foo";
      bases = [];
      body = [
        +Class inner;
      ];
      decorators = [];
      docstring = None;
    }
  in
  assert_classes
    [+Class class_define]
    [class_define; inner]


let () =
  "preprocessing">:::[
    "qualify">::test_qualify;
    "expand_optional_assigns">::test_expand_optional_assigns;
    "expand_operators">::test_expand_operators;
    "expand_returns">::test_expand_returns;
    "expand_for_loop">::test_expand_for;
    "expand_excepts">::test_expand_excepts;
    "expand_ternary_assigns">::test_expand_ternary;
    "defines">::test_defines;
    "classes">::test_classes;
  ]
  |> run_test_tt_main
