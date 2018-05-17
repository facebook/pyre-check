(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Expression
open Statement

open Test


let test_expand_string_annotations _ =
  let assert_expand ?(qualifier = "qualifier") source expected =
    let parse =
      parse ~qualifier:(Source.qualifier ~path:qualifier) in
    assert_source_equal
      (parse expected)
      (Preprocessing.expand_string_annotations (parse source))
  in

  assert_expand
    {|
      class qualifier.Foo: ...
      def foo(f: 'qualifier.Foo') -> 'qualifier.Foo': ...
    |}
    {|
      class qualifier.Foo: ...
      def foo(f: qualifier.Foo) -> qualifier.Foo: ...
    |};
  assert_expand
    "def foo(f: '1234'): ..."
    "def foo(f): ...";
  assert_expand
    {|
      class Foo: ...
      def foo(f: 'Foo[K, V]'): ...
    |}
    {|
      class Foo: ...
      def foo(f: Foo[K, V]): ...
    |}


let test_qualify _ =
  let assert_qualify ?(path = "qualifier.py") source expected =
    ignore path;
    let parse = parse ~qualifier:(Source.qualifier ~path) ~path in
    assert_source_equal (parse expected) (Preprocessing.qualify (parse source))
  in

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
      from builtins import b
      b.c
    |}
    {|
      from builtins import b
      b.c
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
      from a import b
      b().derp
    |}
    {|
      from a import b
      a.b().derp
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
    {|
      from typing import List
      LI = List[int]
      b: LI = []
    |}
    {|
      from typing import List
      qualifier.LI = typing.List[int]
      qualifier.b: qualifier.LI = []
    |};

  assert_qualify
    {|
      from typing import List
      LI = List[int]
      def f() -> int:
        b: LI = []
    |}
    {|
      from typing import List
      qualifier.LI = typing.List[int]
      def qualifier.f() -> int:
        $renamed_b: qualifier.LI = []
    |};


  assert_qualify
    ~path:"module/qualifier.py"
    "from . import something"
    "from module import something";
  assert_qualify
    ~path:"module/submodule/qualifier.py"
    "from .other import something"
    "from module.submodule.other import something";
  assert_qualify
    ~path:"module/submodule/qualifier.py"
    "from ..other import something"
    "from module.other import something";
  (* `__init__` modules are special. *)
  assert_qualify
    ~path:"module/__init__.py"
    "from . import something"
    "from module import something";

  assert_qualify
    {|
      class Foo: pass
      f = Foo()
      class Bar: ...
    |}
    {|
      class qualifier.Foo: pass
      qualifier.f = qualifier.Foo()
      class qualifier.Bar: ...
    |};

  assert_qualify
    {|
      def bar():
        return foo()
      def foo(): pass
      def baz(): ...
    |}
    {|
      def qualifier.bar():
        return qualifier.foo()
      def qualifier.foo(): pass
      def qualifier.baz(): ...
    |};
  assert_qualify
    {|
      class Foo: pass
      def foo(foo: Foo, *args, **kwargs) -> Foo:
        foo, args, kwargs
    |}
    {|
      class qualifier.Foo: pass
      def qualifier.foo(
          $renamed_foo: qualifier.Foo,
          *$renamed_args,
          **$renamed_kwargs) -> qualifier.Foo:
        $renamed_foo, $renamed_args, $renamed_kwargs
    |};
  assert_qualify
    {|
      class Foo: pass
      def foo(foo: Foo) -> 'Foo': pass
    |}
    {|
      class qualifier.Foo: pass
      def qualifier.foo($renamed_foo: qualifier.Foo) -> 'qualifier.Foo':
        pass
    |};
  assert_qualify
    ~path:"signal.py"
    {|
      CONSTANT: int = 1
      def signal(): pass
    |}
    {|
      signal.CONSTANT: int = 1
      def signal.signal(): pass
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
      class qualifier.Foo:
        def foo(): pass
      def qualifier.bar():
        $renamed_f = qualifier.Foo()
        $renamed_f.foo()
    |};
  assert_qualify
    {|
      def bar(): ...
      def foo() -> None:
        a, b = bar()
        return a + b
    |}
    {|
      def qualifier.bar(): ...
      def qualifier.foo() -> None:
        $renamed_a, $renamed_b = qualifier.bar()
        return $renamed_a + $renamed_b
    |};

  assert_qualify
    {|
      argument = None
      def foo() -> None:
        y = lambda argument: argument.attribute
        x = argument or 0
    |}
    {|
      qualifier.argument = None
      def qualifier.foo() -> None:
        $renamed_y = lambda $renamed_argument: $renamed_argument.attribute
        $renamed_x = qualifier.argument or 0
    |};

  assert_qualify
    {|
      constant = 1
      def foo():
        nonconstant = constant
    |}
    {|
      qualifier.constant = 1
      def qualifier.foo():
        $renamed_nonconstant = qualifier.constant
    |};

  assert_qualify
    {|
      constant = ...
      def foo():
        nonconstant = constant
    |}
    {|
      qualifier.constant = ...
      def qualifier.foo():
        $renamed_nonconstant = qualifier.constant
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
      class qualifier.Foo:
        class qualifier.Foo.Foo2:
          def foo(): pass
      def qualifier.bar():
        $renamed_f = qualifier.Foo.Foo2()
        $renamed_f.foo()
    |};

  assert_qualify
    {|
      def hello():
        class Foo2:
          def foo(): pass
        pass
    |}
    {|
      def qualifier.hello():
        class qualifier.hello.Foo2:
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
      def qualifier.hello():
        def qualifier.hello.foo(): pass
        pass
    |};

  assert_qualify
    {|
      list = []
      def hello():
        for i in list:
          a = i
          def foo():
            pass
    |}
    {|
      qualifier.list = []
      def qualifier.hello():
        for $renamed_i in qualifier.list:
          $renamed_a = $renamed_i
          def qualifier.hello.foo():
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
      def qualifier.hello():
        if True:
          def qualifier.hello.foo():
            pass
    |};

  (* Do not rewrite declarations. *)
  assert_qualify
    {|
      import a as b
      def foo(a: A):
        a.attribute = 1
    |}
    {|
      import a as b
      def qualifier.foo($renamed_a: A):
        $renamed_a.attribute = 1
    |};

  assert_qualify
    {|
      from a import B
      def foo(B)->None:
        B.c = 3
    |}
    {|
      from a import B
      def qualifier.foo($renamed_B)->None:
        $renamed_B.c = 3
    |};

  assert_qualify
    {|
      class Foo:
         a: typing.ClassVar[int]
      def f() -> int:
        return Foo.a
    |}
    {|
      class qualifier.Foo:
        a: typing.ClassVar[int]
      def qualifier.f() -> int:
        return qualifier.Foo.a
    |};

  assert_qualify
    {|
      class Foo:
         a: typing.ClassVar[int]
      def f() -> int:
        Foo.a = 3
        return Foo.a
    |}
    {|
      class qualifier.Foo:
        a: typing.ClassVar[int]
      def qualifier.f() -> int:
        qualifier.Foo.a = 3
        return qualifier.Foo.a
    |};

  assert_qualify
    {|
      from a import B
      def f() -> B:
        a = None
        return B
    |}
    {|
      from a import B
      def qualifier.f() -> a.B:
        $renamed_a = None
        return a.B
    |};

  (* Do not rename type annotations. *)
  assert_qualify
    {|
      from a import b
      def foo(a: a.b):
        a = 5
    |}
    {|
      from a import b
      def qualifier.foo($renamed_a: a.b):
        $renamed_a = 5
    |};

  assert_qualify
    {|
      import a
      def foo(a: a):
        a = 5
    |}
    {|
      import a
      def qualifier.foo($renamed_a: a):
        $renamed_a = 5
    |};

  (* Qualify type annotations. *)
  assert_qualify
    {|
      _Q = typing.TypeVar('_Q')
      def identity(input: _Q) -> _Q:
        def nested(input: _Q) -> _Q: ...
    |}
    {|
      qualifier._Q = typing.TypeVar('_Q')
      def qualifier.identity($renamed_input: qualifier._Q) -> qualifier._Q:
        def qualifier.identity.nested(input: qualifier._Q) -> qualifier._Q: ...
    |};
  assert_qualify
    ~path:"typing.py"
    {|
      Any = object()
      TypeVar = object()
      Type: _SpecialForm = ...
      _T = TypeVar('_T')
      def cast(target: Type[_T], value: Any) -> _T: ...
    |}
    {|
      typing.Any = object()
      typing.TypeVar = object()
      typing.Type: _SpecialForm = ...
      typing._T = typing.TypeVar('_T')
      def typing.cast(
          $renamed_target: typing.Type[typing._T],
          $renamed_value: typing.Any) -> typing._T: ...
    |};
  assert_qualify
    {|
      def foo() -> typing.Optional[int]:
        try:
          x = 1
        except:
          return None
        else:
          return x
    |}
    {|
      def qualifier.foo() -> typing.Optional[int]:
        try:
          $renamed_x = 1
        except:
          return None
        else:
          return $renamed_x
    |}


let test_replace_version_specific_code _ =
  let assert_preprocessed ?(path="stub.pyi") source expected =
    assert_source_equal
      (parse ~path expected)
      (Preprocessing.replace_version_specific_code (parse ~path source))
  in
  assert_preprocessed
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
      class C():
        def compatible()->str:
          ...
    |};

  assert_preprocessed
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
      class C():
        def compatible()->str:
          ...
    |};

  assert_preprocessed
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
      class C():
        def compatible()->str:
          ...
    |};

  assert_preprocessed
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
    |};
  assert_preprocessed
    {|
       class C():
         if sys.version_info >= (3, ):
          def compatible()->str:
            ...
    |}
    {|
       class C():
         def compatible()->str:
           ...
    |};
  assert_preprocessed
    {|
       class C():
         if sys.version_info <= (3, ):
          def incompatible()->int:
            ...
    |}
    {|
       class C():
         pass
    |};
  assert_preprocessed
    {|
       class C():
         if sys.version_info >= (3, ):
          def compatible()->str:
            ...
    |}
    {|
       class C():
         def compatible()->str:
           ...
    |};
  assert_preprocessed ~path:"file.py"
    {|
      if sys.version_info >= (3, 5):
        from A import B
      else:
        from A import C
    |}
    {|
       from A import B
    |}


let test_expand_type_checking_imports _ =
  let assert_expanded source expected =
    assert_source_equal
      (parse expected)
      (Preprocessing.expand_type_checking_imports (parse source))
  in
  assert_expanded
    {|
      if typing.TYPE_CHECKING:
        pass
    |}
    {|
      pass
    |};
  assert_expanded
    {|
      from typing import TYPE_CHECKING
      if TYPE_CHECKING:
        pass
    |}
    {|
      from typing import TYPE_CHECKING
      pass
    |};
  assert_expanded
    {|
      from typing import TYPE_CHECKING, TypeVar
      if TYPE_CHECKING:
        T = TypeVar('T')
      else:
        class T:
          pass
    |}
    {|
      from typing import TYPE_CHECKING, TypeVar
      T = TypeVar('T')
    |};
  assert_expanded
    {|
      from whoops import TYPE_CHECKING
      if TYPE_CHECKING:
        pass
    |}
    {|
      from whoops import TYPE_CHECKING
      pass
    |}


let test_expand_returns _ =
  let assert_expand source expected =
    assert_source_equal
      (parse expected)
      (Preprocessing.expand_returns (parse source))
  in

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
    |};

  (* Lol termination analysis. *)
  assert_expand
    {|
      def foo():
        while derp:
          pass
    |}
    {|
      def foo():
        while derp:
          pass
        return
    |};
  assert_expand
    {|
      def foo():
        while True:
          pass
    |}
    {|
      def foo():
        while True:
          pass
    |}


let test_expand_for _ =
  let assert_expand source expected =
    assert_source_equal
      (parse expected)
      (Preprocessing.expand_for_loop (parse source))
  in

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
      (parse expected)
      (Preprocessing.expand_excepts (parse source))
  in
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
        Exception
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
        (E1, E2)
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
      (parse expected)
      (Preprocessing.expand_ternary_assign (parse source))
  in

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


let test_expand_named_tuples _ =
  let assert_expand source expected =
    assert_source_equal
      (parse expected)
      (Preprocessing.expand_named_tuples (parse source))
  in
  assert_expand
    {|
      T = typing.NamedTuple('T')
    |}
    {|
      class T(typing.NamedTuple):
        pass
    |};
  assert_expand
    {|
      T = collections.namedtuple('T', ['a'])
    |}
    {|
      class T(typing.NamedTuple):
        a: typing.Any
    |};
  assert_expand
    {|
      T = typing.NamedTuple('T', ['one', 'two'])
    |}
    {|
      class T(typing.NamedTuple):
        one: typing.Any
        two: typing.Any
    |};
  assert_expand
    {|
      T = typing.NamedTuple('T', [('one', int), ('two', str)])
    |}
    {|
      class T(typing.NamedTuple):
        one: int
        two: str
    |};
  assert_expand
    {|
      T = collections.namedtuple('T', 'a b c')
    |}
    {|
      class T(typing.NamedTuple):
        a: typing.Any
        b: typing.Any
        c: typing.Any
    |};

  assert_expand
    {|
      class Foo(Bar, collections.namedtuple('T', ['one', 'two'])):
        three: int = 1
    |}
    {|
      class Foo(Bar, typing.NamedTuple):
        one: typing.Any
        two: typing.Any
        three: int = 1
    |};

  (* Don't transform non-toplevel statements. *)
  assert_expand
    {|
      def foo():
        T = typing.NamedTuple('T')
    |}
    {|
      def foo():
        T = typing.NamedTuple('T')
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
    [toplevel; define]


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


let test_preprocess _ =
  let assert_preprocess ?(qualifier = "qualifier") source expected =
    let parse = parse ~qualifier:(Source.qualifier ~path:qualifier) in
    assert_source_equal
      (parse expected)
      (Preprocessing.preprocess (parse source))
  in

  assert_preprocess
    {|
      a = 1
    |}
    {|
      qualifier.a = 1
    |};
  assert_preprocess
    {|
      if sys.version_info > (3, 0):
        a = 1
    |}
    {|
      qualifier.a = 1
    |};

  (* String annotations get qualified. *)
  assert_preprocess
    {|
      class Foo: ...
      def foo(f: 'Foo') -> 'Foo': ...
      def bar(f: 'List[Foo]') -> 'Foo':
        pass
    |}
    {|
      class qualifier.Foo: ...
      def qualifier.foo($renamed_f: qualifier.Foo) -> qualifier.Foo: ...
      def qualifier.bar($renamed_f: List.__getitem__(qualifier.Foo)) -> qualifier.Foo:
        pass
        return
    |};

  assert_preprocess
    {|
    class Foo:
      def foo(f: 'Foo') -> 'Foo': ...
  |}
    {|
    class qualifier.Foo:
      def foo($renamed_f: qualifier.Foo) -> qualifier.Foo: ...
  |};

  assert_preprocess
    {|
      a = 1
      def access() -> int:
         return a
      def assign() -> None:
         a = 2
      def globalvar() -> None:
         global a
         a = 3
         return a
    |}
    {|
      qualifier.a = 1
      def qualifier.access() -> int:
         $return = qualifier.a
         return $return
      def qualifier.assign() -> None:
         $renamed_a = 2
         return
      def qualifier.globalvar() -> None:
         global a
         qualifier.a = 3
         $return = qualifier.a
         return $return
    |};

  assert_preprocess
    {|
      a = 1
      def indirect_access_1() -> int:
         access(a)
         return a
      def indirect_access_2() -> int:
         b = a
         return a
      def indirect_access_3() -> int:
         b = access(a)
         return a
    |}
    {|
      qualifier.a = 1
      def qualifier.indirect_access_1() -> int:
         access(qualifier.a)
         $return = qualifier.a
         return $return
      def qualifier.indirect_access_2() -> int:
         $renamed_b = qualifier.a
         $return = qualifier.a
         return $return
      def qualifier.indirect_access_3() -> int:
         $renamed_b = access(qualifier.a)
         $return = qualifier.a
         return $return
    |};

  assert_preprocess
    {|
      a = 1
      def access_with_parameter(a: int) -> int:
         return a
      def assign_with_parameter() -> None:
         a = 2
    |}
    {|
      qualifier.a = 1
      def qualifier.access_with_parameter($renamed_a: int) -> int:
         $return = $renamed_a
         return $return
      def qualifier.assign_with_parameter() -> None:
         $renamed_a = 2
         return
    |}


let () =
  "preprocessing">:::[
    "expand_string_annotations">::test_expand_string_annotations;
    "qualify">::test_qualify;
    "replace_version_specific_code">::test_replace_version_specific_code;
    "expand_type_checking_imports">::test_expand_type_checking_imports;
    "expand_returns">::test_expand_returns;
    "expand_for_loop">::test_expand_for;
    "expand_excepts">::test_expand_excepts;
    "expand_ternary_assigns">::test_expand_ternary;
    "expand_named_tuples">::test_expand_named_tuples;
    "defines">::test_defines;
    "classes">::test_classes;
    "preprocess">::test_preprocess;
  ]
  |> run_test_tt_main
