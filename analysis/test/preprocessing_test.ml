(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

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
      if python_version > 3.6:
        def bar():
          return 1
        class C:
          pass
    |}
    {|
      if python_version > 3.6:
        def some.qualifier.bar():
          return 1
        class some.qualifier.C:
          pass
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
      class Foo: pass
      def foo(foo: Foo) -> 'Foo': pass
    |}
    {|
      class some.qualifier.Foo: pass
      def some.qualifier.foo(foo: some.qualifier.Foo) -> 'some.qualifier.Foo':
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


let test_remove_python2_stub_code _ =
  let assert_removed source expected =
    assert_source_equal
      (Preprocessing.remove_python2_stub_code (parse source))
      (parse expected)
  in
  assert_removed
    {|
      if sys.version_info < (3, 0):
        class C():
         def incompatible()->int:
           ...
      else:
        class C():
          def compatible()->str:
            ...
    |}
    {|
      if sys.version_info < (3, 0):
        pass
      else:
        class C():
          def compatible()->str:
            ...
    |};

  assert_removed
    {|
      if (3,) > sys.version_info:
        class C():
          def incompatible()->int:
            ...
      else:
        class C():
          def compatible()->str:
            ...
    |}
    {|
      if (3,) > sys.version_info:
        pass
      else:
        class C():
          def compatible()->str:
            ...
    |};

  assert_removed
    {|
      if sys.version_info <= (3, 0):
        class C():
          def incompatible()->int:
            ...
      else:
        class C():
          def compatible()->str:
            ...
    |}
    {|
      if sys.version_info <= (3, 0):
        class C():
          def incompatible()->int:
            ...
      else:
        class C():
          def compatible()->str:
            ...
    |};

  assert_removed
    {|
      if sys.version_info < 3:
        class C():
          def incompatible()->int:
            ...
      else:
        class C():
          def compatible()->str:
            ...
    |}
    {|
      if sys.version_info < 3:
        class C():
          def incompatible()->int:
            ...
      else:
        class C():
          def compatible()->str:
            ...
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
      (Preprocessing.expand_returns (parse source))
      (parse expected) in

  assert_expand
    "return None"
    {|
      $return = None
      return $return
    |};
  assert_expand
    "return foo"
    {|
      $return = foo
      return $return
    |};

  assert_expand
    {|
      def foo():
        return None
    |}
    {|
      def foo():
        $return = None
        return $return
    |};
  assert_expand
    {|
      def foo():
        pass
    |}
    {|
      def foo():
        pass
        return
    |};

  assert_expand
    {|
      def foo():
        yield None
    |}
    {|
      def foo():
        yield None
    |};

  assert_expand
    {|
      def foo():
        try:
          pass
        finally:
          pass
    |}
    {|
      def foo():
        try:
          pass
        finally:
          pass
        return
    |};
  assert_expand
    {|
      def foo():
        try:
          pass
        finally:
          return 1
    |}
    {|
      def foo():
        try:
          pass
        finally:
          $return = 1
          return $return
    |}


let test_simplify_access_chains _ =
  let assert_expand source expected =
    assert_source_equal
      (Preprocessing.simplify_access_chains (parse source))
      (parse expected)
  in
  assert_expand
    {|
      x = a.b
    |}
    {|
      x = a.b
    |};
  assert_expand
    {|
      x = a.b[1][2]
    |}
    {|
      x = a.b[1][2]
    |};
  assert_expand
    {|
      x = a.b().c().d()
    |}
    {|
      $1 = a.b()
      $2 = $1.c()
      x = $2.d()
    |};
  assert_expand
    {|
      x = a.b.c.d().e()
    |}
    {|
      $1 = a.b.c.d()
      x = $1.e()
    |};
  assert_expand
    {|
      x = a.b().c().d()
      foo = boo()
      foo = boo()
      y = x().y().z()
    |}
    {|
      $1 = a.b()
      $2 = $1.c()
      x = $2.d()
      foo = boo()
      foo = boo()
      $6 = x()
      $7 = $6.y()
      y = $7.z()
    |};
  assert_expand
    {|
      x = a().b(c().d())
    |}
    {|
      $1 = a()
      x = $1.b(c().d())
    |};
  assert_expand
    {|
      a.b().c()
    |}
    {|
      $1 = a.b()
      $1.c()
    |};
  assert_expand
    {|
      x = super()
      x.foo('asdf')
    |}
    {|
      x = super()
      x.foo('asdf')
    |}


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
      ~cmp:(List.equal ~equal:Define.equal)
      (Preprocessing.defines (Source.create statements)
       |> List.map ~f:Node.value)
      defines in

  let define =
    {
      Define.name = Access.create "foo";
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
      generated = false;
      parent = None;
    }
  in
  let toplevel =
    {
      Define.name = Access.create "$toplevel";
      parameters = [];
      body = [+Define define];
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
      generated = false;
      parent = None;
    }
  in
  assert_defines
    [+Define define]
    [toplevel; define];

  let inner =
    {
      Define.name = Access.create "foo";
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
      generated = false;
      parent = None;
    }
  in
  let define =
    {
      Define.name = Access.create "foo";
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
      generated = false;
      parent = None;
    }
  in
  let toplevel =
    {
      Define.name = Access.create "$toplevel";
      parameters = [];
      body = [+Define define];
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
      generated = false;
      parent = None;
    }
  in
  assert_defines
    [+Define define]
    [toplevel; define; inner]


let test_classes _ =
  let assert_classes statements class_defines =
    assert_equal
      ~cmp:(List.equal ~equal:Class.equal)
      (Preprocessing.classes (Source.create statements)
       |> List.map ~f:Node.value)
      class_defines in

  let class_define =
    {
      Class.name = Access.create "foo";
      bases = [];
      body = [
        +Define {
          Define.name = Access.create "bar";
          parameters = [];
          body = [+Pass];
          decorators = [];
          docstring = None;
          return_annotation = None;
          async = false;
          generated = false;
          parent = Some (Access.create "foo");
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
      Class.name = Access.create "bar";
      bases = [];
      body = [+Pass];
      decorators = [];
      docstring = None;
    }
  in
  let class_define =
    {
      Class.name = Access.create "foo";
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
    "remove_python2_stub_code">::test_remove_python2_stub_code;
    "expand_optional_assigns">::test_expand_optional_assigns;
    "expand_operators">::test_expand_operators;
    "expand_returns">::test_expand_returns;
    "expand_for_loop">::test_expand_for;
    "expand_multiple_calls">::test_simplify_access_chains;
    "expand_excepts">::test_expand_excepts;
    "expand_ternary_assigns">::test_expand_ternary;
    "defines">::test_defines;
    "classes">::test_classes;
  ]
  |> run_test_tt_main
