(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Ast
open Analysis
open Expression
open Statement
open Test

let test_expand_relative_imports _ =
  let assert_expand ~handle source expected =
    let parse = parse ~handle in
    assert_source_equal
      ~location_insensitive:true
      (parse expected)
      (Preprocessing.expand_relative_imports (parse source))
  in
  assert_expand
    ~handle:"module/submodule/test.py"
    {|
      from builtins import str
      from future.builtins import str
      from . import a
      from .relative import b
      from .. import c
      from ..relative import d
    |}
    {|
      from builtins import str
      from future.builtins import str
      from module.submodule import a
      from module.submodule.relative import b
      from module import c
      from module.relative import d
    |};
  assert_expand
    ~handle:"module/submodule/test/__init__.py"
    {|
      from . import a
      from .relative import b
      from .. import c
      from ..relative import d
    |}
    {|
      from module.submodule.test import a
      from module.submodule.test.relative import b
      from module.submodule import c
      from module.submodule.relative import d
    |}


let test_expand_string_annotations _ =
  let assert_expand ?(handle = "qualifier.py") source expected =
    let parse = parse ~handle in
    assert_source_equal
      ~location_insensitive:true
      (parse expected)
      (Preprocessing.expand_string_annotations (parse source))
  in
  assert_expand
    {|
      class Foo:
        attribute: 'Foo'
        class_attribute: typing.ClassVar['Foo']
        tuple: typing.Tuple['int', 'str']
      constant: 'Foo' = ...
      def foo(f: 'Foo') -> 'Foo': ...
    |}
    {|
      class Foo:
        attribute: Foo
        class_attribute: typing.ClassVar[Foo]
        tuple: typing.Tuple[int, str]
      constant: Foo = ...
      def foo(f: Foo) -> Foo: ...
    |};
  assert_expand "def foo(f: '1234'): ..." "def foo(f: $unparsed_annotation): ...";
  assert_expand
    {|
      class Foo: ...
      def foo(f: 'Foo[K, V]'): ...
    |}
    {|
      class Foo: ...
      def foo(f: Foo[K, V]): ...
    |};
  assert_expand
    {|
      from typing import cast
      class A: ...
      class B(A): ...
      def foo(o: object) -> B:
        s = cast('str', o)
        i = 42 + cast('int', o)
        a = cast('A', o)
        return cast('B', cast('A', o))
      class Foo:
        x: B = cast('float', 42)
    |}
    {|
      from typing import cast
      class A: ...
      class B(A): ...
      def foo(o: object) -> B:
        s = cast(str, o)
        i = 42 + cast(int, o)
        a = cast(A, o)
        return cast(B, cast(A, o))
      class Foo:
        x: B = cast(float, 42)
    |};
  assert_expand "x = typing.cast('1234', 42)" "x = typing.cast($unparsed_annotation, 42)";
  assert_expand
    {|
        from pyre_extensions import safe_cast
        class A: ...
        class B(A): ...
        def foo(o: object) -> B:
          s = safe_cast('str', o)
          i = 42 + safe_cast('int', o)
          a = safe_cast('A', o)
          return safe_cast('B', safe_cast('A', o))
        class Foo:
          x: B = safe_cast('float', 42)
      |}
    {|
        from pyre_extensions import safe_cast
        class A: ...
        class B(A): ...
        def foo(o: object) -> B:
          s = safe_cast(str, o)
          i = 42 + safe_cast(int, o)
          a = safe_cast(A, o)
          return safe_cast(B, safe_cast(A, o))
        class Foo:
          x: B = safe_cast(float, 42)
      |};
  assert_expand
    "x = pyre_extensions.safe_cast('1234', 42)"
    "x = pyre_extensions.safe_cast($unparsed_annotation, 42)";
  assert_expand
    {|
      import typing
      class Foo: ...
      def foo(x: typing.Any):
        y = typing.cast('typing.List[Foo]', x)
        z = typing.cast("typing.Dict[str, Foo]", x)
    |}
    {|
      import typing
      class Foo: ...
      def foo(x: typing.Any):
        y = typing.cast(typing.List[Foo], x)
        z = typing.cast(typing.Dict[str, Foo], x)
    |};
  assert_expand "def foo(f: Literal['A']): ..." "def foo(f: Literal['A']): ...";
  assert_expand
    "def foo(f: typing_extensions.Literal['A']): ..."
    "def foo(f: typing_extensions.Literal['A']): ...";
  assert_expand
    "def foo(f: typing_extensions.Literal['A', 'B']): ..."
    "def foo(f: typing_extensions.Literal['A', 'B']): ...";
  assert_expand "def foo(f: te.Literal['A', 'B']): ..." "def foo(f: te.Literal['A', 'B']): ...";
  assert_expand "class Foo(typing.List['str']): ..." "class Foo(typing.List[str]): ...";
  assert_expand "class Foo('str'): ..." "class Foo('str'): ...";
  ()


let test_expand_format_string _ =
  let assert_format_string source value expressions =
    let handle = "test.py" in
    assert_source_equal
      ~location_insensitive:true
      (Source.create
         ~relative:handle
         [+Statement.Expression (+Expression.String (StringLiteral.create ~expressions value))])
      (Preprocessing.expand_format_string (parse_untrimmed ~handle source))
  in
  assert_format_string "f'foo'" "foo" [];
  assert_format_string "f'{1}'" "{1}" [+Expression.Integer 1];
  assert_format_string "f'foo{1}'" "foo{1}" [+Expression.Integer 1];
  assert_format_string "f'foo{1}' 'foo{2}'" "foo{1}foo{2}" [+Expression.Integer 1];
  assert_format_string "'foo{1}' f'foo{2}'" "foo{1}foo{2}" [+Expression.Integer 2];
  assert_format_string
    "f'foo{1}' f'foo{2}'"
    "foo{1}foo{2}"
    [+Expression.Integer 1; +Expression.Integer 2];
  assert_format_string
    "f'foo{1}{2}foo'"
    "foo{1}{2}foo"
    [+Expression.Integer 1; +Expression.Integer 2];
  assert_format_string "f'foo{{1}}'" "foo{{1}}" [];
  assert_format_string "f'foo{{ {1} }}'" "foo{{ {1} }}" [+Expression.Integer 1];
  assert_format_string "f'foo{{{1} }}'" "foo{{{1} }}" [+Expression.Integer 1];
  assert_format_string "f'foo{{ {1}}}'" "foo{{ {1}}}" [+Expression.Integer 1];
  assert_format_string "f'foo{{{1}}}'" "foo{{{1}}}" [+Expression.Integer 1];
  assert_format_string "f'foo{{'" "foo{{" [];
  assert_format_string "f'foo}}'" "foo}}" [];
  assert_format_string
    "f'foo{1+2}'"
    "foo{1+2}"
    [
      +Expression.Call
         {
           callee =
             +Expression.Name
                (Name.Attribute
                   { base = +Expression.Integer 1; attribute = "__add__"; special = true });
           arguments = [{ Call.Argument.name = None; value = +Expression.Integer 2 }];
         };
    ];

  (* Ensure we fix up locations. *)
  let assert_locations source statements =
    let parsed_source = parse source |> Preprocessing.expand_format_string in
    let expected_source = { parsed_source with Source.statements } in
    assert_source_equal_with_locations expected_source parsed_source
  in
  assert_locations
    "f'foo{1}'"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 9)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 9)
              (Expression.String
                 {
                   StringLiteral.kind =
                     StringLiteral.Format [node ~start:(1, 6) ~stop:(1, 7) (Expression.Integer 1)];
                   value = "foo{1}";
                 })));
    ];
  assert_locations
    "f'foo{123}a{456}'"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 17)
        (Statement.Expression
           (node
              ~start:(1, 0)
              ~stop:(1, 17)
              (Expression.String
                 {
                   StringLiteral.kind =
                     StringLiteral.Format
                       [
                         node ~start:(1, 6) ~stop:(1, 9) (Expression.Integer 123);
                         node ~start:(1, 12) ~stop:(1, 15) (Expression.Integer 456);
                       ];
                   value = "foo{123}a{456}";
                 })));
    ];
  assert_locations
    "return f'foo{123}a{456}'"
    [
      node
        ~start:(1, 0)
        ~stop:(1, 24)
        (Statement.Return
           {
             is_implicit = false;
             expression =
               Some
                 (node
                    ~start:(1, 7)
                    ~stop:(1, 24)
                    (Expression.String
                       {
                         StringLiteral.kind =
                           StringLiteral.Format
                             [
                               node ~start:(1, 13) ~stop:(1, 16) (Expression.Integer 123);
                               node ~start:(1, 19) ~stop:(1, 22) (Expression.Integer 456);
                             ];
                         value = "foo{123}a{456}";
                       }));
           });
    ];
  assert_locations
    {|
       f'''
       foo{123}a{456}
       b{789}
       '''
     |}
    [
      node
        ~start:(2, 0)
        ~stop:(5, 3)
        (Statement.Expression
           (node
              ~start:(2, 0)
              ~stop:(5, 3)
              (Expression.String
                 {
                   StringLiteral.kind =
                     StringLiteral.Format
                       [
                         node ~start:(3, 4) ~stop:(3, 7) (Expression.Integer 123);
                         node ~start:(3, 10) ~stop:(3, 13) (Expression.Integer 456);
                         node ~start:(4, 2) ~stop:(4, 5) (Expression.Integer 789);
                       ];
                   value = "\nfoo{123}a{456}\nb{789}\n";
                 })));
    ]


let test_qualify _ =
  let assert_qualify ?(handle = "qualifier.py") source expected =
    let parse = parse ~handle in
    assert_source_equal
      ~location_insensitive:true
      (parse expected)
      (Preprocessing.qualify (parse source))
  in
  (* Base cases for aliasing. *)
  assert_qualify "from a import b; b" "from a import b; a.b";
  assert_qualify "from a import b, c; b, c" "from a import b, c; a.b, a.c";
  assert_qualify "from a import b; b.c" "from a import b; a.b.c";
  assert_qualify "from a import b; b()" "from a import b; a.b()";
  assert_qualify "from a import b as c; c" "from a import b as c; a.b";
  assert_qualify "from builtins import b; b" "from builtins import b; b";
  assert_qualify "b; import a as b; b" "b; import a as b; a";

  (* Qualification in different places. *)
  let assert_qualify_statement actual expected =
    let import = "import a as b;" in
    assert_qualify (import ^ actual) (import ^ expected)
  in
  assert_qualify_statement "call(b)" "call(a)";
  assert_qualify_statement "List[b]" "List[a]";
  assert_qualify_statement "await b" "await a";
  assert_qualify_statement "(await b).c" "(await a).c";
  assert_qualify_statement "b or True" "a or True";
  assert_qualify_statement "True and b" "True and a";
  assert_qualify_statement "1 < b <= b" "1 < a <= a";
  assert_qualify_statement "{ b: b }" "{ a: a }";
  assert_qualify_statement
    "{ a: b for a, b in b }"
    "{ $target$a: $target$b for $target$a, $target$b in a }";
  assert_qualify_statement
    "{ b: b for b in b if b }"
    "{ $target$b: $target$b for $target$b in a if $target$b }";
  assert_qualify_statement "lambda b: b" "lambda $parameter$b: $parameter$b";
  assert_qualify_statement "lambda a: a + b" "lambda $parameter$a: $parameter$a + a";
  assert_qualify_statement "[b, b]" "[a, a]";
  assert_qualify_statement "[b for b in b]" "[$target$b for $target$b in a]";
  assert_qualify_statement "{b, b}" "{a, a}";
  assert_qualify_statement "{b for b in b}" "{$target$b for $target$b in a}";
  assert_qualify_statement "*b" "*a";
  assert_qualify_statement "**b" "**a";
  assert_qualify_statement "b if b else b" "a if a else a";
  assert_qualify_statement "(b, b)" "(a, a)";
  assert_qualify_statement "-b" "-a";
  assert_qualify_statement "assert b" "assert a";
  assert_qualify_statement "del b" "del a";
  assert_qualify_statement
    "b = 1\nfor b in []: pass"
    "$local_qualifier$b = 1\nfor $local_qualifier$b in []: pass";
  assert_qualify_statement "\nif b:\n\tb\nelse:\n\tb" "\nif a:\n\ta\nelse:\n\ta";
  assert_qualify_statement "raise b" "raise a";
  assert_qualify_statement "return b" "return a";
  assert_qualify_statement
    "\ntry:\n\tb\nexcept b as b:\n\tb\nelse:\n\tb\nfinally:\n\tb"
    "\ntry:\n\ta\nexcept a as $target$b:\n\t$target$b\nelse:\n\ta\nfinally:\n\ta";
  assert_qualify_statement "\nwith b as b: b" "\nwith a as $target$b: $target$b";
  assert_qualify_statement "\nwhile b: b" "\nwhile a: a";
  assert_qualify_statement "yield b" "yield a";
  assert_qualify_statement "yield from b" "yield from a";

  (* Always pick conditional imports. *)
  assert_qualify
    {|
      b
      if True:
        import a as b
      b
    |}
    {|
      b
      if True:
        import a as b
      a
    |};
  assert_qualify
    {|
      b
      if False:
        import a as b
      b
    |}
    {|
      b
      if False:
        import a as b
      a
    |};

  (* Qualify assignments. *)
  assert_qualify
    {|
      from typing import List
      from module import constant
      a: List[int] = constant
    |}
    {|
      from typing import List
      from module import constant
      $local_qualifier$a: typing.List[int] = module.constant
    |};
  assert_qualify
    {|
      a, *b = [1]
    |}
    {|
      $local_qualifier$a, *$local_qualifier$b = [1]
    |};
  assert_qualify
    {|
      [*a, b] = [1]
    |}
    {|
      [*$local_qualifier$a, $local_qualifier$b] = [1]
    |};

  (* Qualify walrus assignments. *)
  (* TODO(T53600647): Qualify `a`. *)
  assert_qualify
    {|
      from module import constant
      a := constant
    |}
    {|
      from module import constant
      a := module.constant
    |};

  (* Qualify classes. *)
  assert_qualify
    {|
      from typing import List
      class Class():
        attribute: List[int] = 1
        first, second = 1, 2
        def method(self: Class):
          self.attribute = 1
          self.attribute
    |}
    {|
      from typing import List
      class qualifier.Class():
        qualifier.Class.attribute: typing.List[int] = 1
        qualifier.Class.first, qualifier.Class.second = 1, 2
        def qualifier.Class.method($parameter$self: qualifier.Class):
          $parameter$self.attribute = 1
          $parameter$self.attribute
    |};
  assert_qualify
    {|
      from typing import Union
      class Class:
        _Alias = Union[int, str]
        a = _Alias
        def method(self, alias: _Alias): ...
    |}
    {|
      from typing import Union
      class qualifier.Class:
        qualifier.Class._Alias = typing.Union[int, str]
        qualifier.Class.a = qualifier.Class._Alias
        def qualifier.Class.method($parameter$self, $parameter$alias: qualifier.Class._Alias): ...
    |};
  assert_qualify "class Class: ..." "class qualifier.Class: ...";
  assert_qualify
    {|
      from decorators import transform
      @transform
      class Class(): pass
    |}
    {|
      from decorators import transform
      @decorators.transform
      class qualifier.Class(): pass
    |};
  assert_qualify
    {|
      class Class():
        class Nested():
          def method(self): pass
    |}
    {|
      class qualifier.Class():
        class qualifier.Class.Nested():
          def qualifier.Class.Nested.method($parameter$self): pass
    |};
  assert_qualify
    {|
      class Class():
        @classmethod
        def classmethod(): pass
    |}
    {|
      class qualifier.Class():
        @classmethod
        def qualifier.Class.classmethod(): pass
    |};
  assert_qualify
    {|
      from module import C
      a = C
      class C: ...
      b = C
    |}
    {|
      from module import C
      $local_qualifier$a = module.C
      class qualifier.C: ...
      $local_qualifier$b = qualifier.C
    |};
  assert_qualify
    {|
      class C:
        INDENT = 1
        INDENT, other = 2, 3
    |}
    {|
      class qualifier.C:
         qualifier.C.INDENT = 1
         qualifier.C.INDENT, qualifier.C.other = (2, 3)
    |};
  assert_qualify
    {|
      local = 0
      class C:
        def __init__(self):
          self.local = 1
    |}
    {|
      $local_qualifier$local = 0
      class qualifier.C:
        def qualifier.C.__init__($parameter$self):
          $parameter$self.local = 1
    |};
  assert_qualify
    {|
      INDENT = 0
      class C:
        INDENT = 1
        INDENT, other = 2, 3
    |}
    {|
      $local_qualifier$INDENT = 0
      class qualifier.C:
         qualifier.C.INDENT = 1
         qualifier.C.INDENT, qualifier.C.other = (2, 3)
    |};

  (* Treat special forms like class definitions. *)
  assert_qualify
    ~handle:"typing.pyi"
    {|
      Type: _SpecialForm = ...
      def foo(l: Type[int]): ...
    |}
    {|
      Type: _SpecialForm = ...
      def typing.foo($parameter$l: typing.Type[int]): ...
    |};

  (* Only qualify strings for annotations and potential type aliases. *)
  assert_qualify
    {|
      from typing import List
      T = 'List'
      def foo() -> 'List[int]': ...
      a = ['List']
      d = {'List': 'List'}
      f = MayBeType['List']
    |}
    {|
      from typing import List
      $local_qualifier$T = 'typing.List'
      def qualifier.foo() -> 'typing.List[int]': ...
      $local_qualifier$a = ['List']
      $local_qualifier$d = {'List': 'List'}
      $local_qualifier$f = MayBeType['typing.List']
    |};
  assert_qualify
    {|
      from typing import List
      def f():
        T = 'List'
        def foo() -> 'List[int]': ...
        a = ['List']
        d = {'List': 'List'}
        f = MayBeType['List']
    |}
    {|
      from typing import List
      def qualifier.f():
        $local_qualifier?f$T = 'List'
        def $local_qualifier?f$foo() -> 'typing.List[int]': ...
        $local_qualifier?f$a = ['List']
        $local_qualifier?f$d = {'List': 'List'}
        $local_qualifier?f$f = MayBeType['List']
    |};
  assert_qualify
    {|
      def foo(arg):
        x = 'arg'
        return {'arg': x}
    |}
    {|
      def qualifier.foo($parameter$arg):
        $local_qualifier?foo$x = 'arg'
        return {'arg': $local_qualifier?foo$x}
    |};
  assert_qualify
    {|
      from typing import TypeVar as TV
      class C:
        T = 'C'
        TSelf = TV("TSelf", bound="C")
    |}
    {|
      from typing import TypeVar as TV
      class qualifier.C():
        qualifier.C.T = "C"
        qualifier.C.TSelf = typing.TypeVar("TSelf",$parameter$bound = "qualifier.C")
    |};
  assert_qualify
    {|
      from typing import TypeVar as TV
      class X: pass
      class A: pass
      class C:
        T = 'C'
        TSelf = TV("TSelf", "C", X, "A")
    |}
    {|
      from typing import TypeVar as TV
      class qualifier.X(): pass
      class qualifier.A(): pass
      class qualifier.C():
        qualifier.C.T = "C"
        qualifier.C.TSelf = typing.TypeVar("TSelf", "qualifier.C", qualifier.X, "qualifier.A")
    |};

  (* Qualify parameters *)
  assert_qualify
    {|
      class A: pass
      def foo(x: 'A'): ...
    |}
    {|
      class qualifier.A(): pass
      def qualifier.foo($parameter$x: 'qualifier.A'): ...
    |};
  assert_qualify
    {|
      from typing_extensions import Literal
      class A: pass
      def foo(x: Literal['A']): ...
    |}
    {|
      from typing_extensions import Literal
      class qualifier.A(): pass
      def qualifier.foo($parameter$x: typing_extensions.Literal['A']): ...
    |};

  (* Qualify functions. *)
  assert_qualify
    {|
      def foo(): pass
      foo()
    |}
    {|
      def qualifier.foo(): pass
      qualifier.foo()
    |};
  assert_qualify
    {|
      from abc import foo
      def foo(): pass
      foo()
    |}
    {|
      from abc import foo
      def qualifier.foo(): pass
      qualifier.foo()
    |};
  assert_qualify
    {|
      from abc import foo
      def foo(): pass
      foo()
      def foo(): pass
      foo()
    |}
    {|
      from abc import foo
      def qualifier.foo(): pass
      qualifier.foo()
      def qualifier.foo(): pass
      qualifier.foo()
    |};

  assert_qualify
    {|
      def foo():
        def foo():
          pass
        def foo():
          pass
    |}
    {|
      def qualifier.foo():
        def $local_qualifier?foo$foo():
          pass
        def $local_qualifier?foo$foo():
          pass
    |};
  assert_qualify
    {|
      def foo():
        def foo():
          def foo():
            pass
        def bar():
          pass
    |}
    {|
      def qualifier.foo():
        def $local_qualifier?foo$foo():
          def $local_qualifier?foo?foo$foo():
            pass
        def $local_qualifier?foo$bar():
          pass
    |};

  (* TODO(T47589601): We cannot correctly handle this case for now due to our current limited
     aliases recording mechanism. *)
  assert_qualify
    {|
      def foo():
        from abc import bar
        bar()
        def bar(): pass
        bar()
    |}
    {|
      def qualifier.foo():
        from abc import bar
        abc.bar()
        def $local_qualifier?foo$bar(): pass
        $local_qualifier?foo$bar()
    |};
  assert_qualify
    {|
      from abc import foo
      foo();
      def foo(): pass
      foo()
    |}
    {|
      from abc import foo
      abc.foo();
      def qualifier.foo(): pass
      qualifier.foo()
    |};

  (* TODO(T47589601): We cannot correctly handle this case for now due to our current limited
     aliases recording mechanism. *)
  assert_qualify
    {|
      from abc import foo
      def bar(): foo()
      def foo(): pass
      foo()
      |}
    {|
      from abc import foo
      def qualifier.bar(): abc.foo()
      def qualifier.foo(): pass
      qualifier.foo()
    |};
  assert_qualify
    {|
      from abc import foo
      foo();
      def foo(): pass
      foo()
      from abc import foo
      foo()
      def foo(): pass
      foo()
    |}
    {|
      from abc import foo
      abc.foo();
      def qualifier.foo(): pass
      qualifier.foo()
      from abc import foo
      abc.foo();
      def qualifier.foo(): pass
      qualifier.foo()
    |};
  assert_qualify
    {|
      for b in []: pass
      def b(): pass
    |}
    {|
      for $target$b in []: pass
      def qualifier.b(): pass
    |};
  assert_qualify "def foo(): ..." "def qualifier.foo(): ...";
  assert_qualify
    {|
      def foo(): ...
      "expression".foo()
    |}
    {|
      def qualifier.foo(): ...
      "expression".foo()
    |};
  assert_qualify
    {|
      foo()
      def foo(): pass
    |}
    {|
      qualifier.foo()
      def qualifier.foo(): pass
    |};
  assert_qualify
    {|
      from decorators import memoize
      @memoize
      def foo(): pass
    |}
    {|
      from decorators import memoize
      @decorators.memoize
      def qualifier.foo(): pass
    |};
  assert_qualify
    {|
      @foo.setter
      def foo(): pass
    |}
    {|
      @qualifier.foo.setter
      def qualifier.foo(): pass
    |};
  assert_qualify
    {|
      from typing import List
      def foo() -> List[int]: pass
    |}
    {|
      from typing import List
      def qualifier.foo() -> typing.List[int]: pass
    |};

  (* TODO(T46137017): This is incorrect, as the variable should be hoisted *)
  assert_qualify
    {|
      x = 7
      def foo():
        print(x)
        x = 9
    |}
    {|
      $local_qualifier$x = 7
      def qualifier.foo():
        print($local_qualifier$x)
        $local_qualifier?foo$x = 9
    |};

  (* Nested defines. *)
  assert_qualify
    {|
      def foo():
        def nested():
          x = 7
          nested()
    |}
    {|
      def qualifier.foo():
        def $local_qualifier?foo$nested():
          $local_qualifier?foo?nested$x = 7
          $local_qualifier?foo$nested()
    |};

  (* Hoisting is implemented for nested functions *)
  assert_qualify
    {|
      def foo():
        def nestedA():
          nestedB()
        def nestedB():
          nestedA()
    |}
    {|
      def qualifier.foo():
        def $local_qualifier?foo$nestedA():
          $local_qualifier?foo$nestedB()
        def $local_qualifier?foo$nestedB():
          $local_qualifier?foo$nestedA()
    |};
  assert_qualify
    {|
      def foo():
        def nested(a):
          def a(): pass
    |}
    {|
      def qualifier.foo():
        def $local_qualifier?foo$nested($parameter$a):
          def $parameter$a(): pass
    |};

  (* SSA-gutted. *)
  assert_qualify
    {|
      constant = 1
      constant = constant
    |}
    {|
      $local_qualifier$constant = 1
      $local_qualifier$constant = $local_qualifier$constant
    |};
  assert_qualify
    {|
      constant: int = 1
      def foo():
        constant = 2
        constant = 3
    |}
    {|
      $local_qualifier$constant: int = 1
      def qualifier.foo():
        $local_qualifier?foo$constant = 2
        $local_qualifier?foo$constant = 3
    |};
  assert_qualify
    {|
      def foo():
        foo()
        foo = 1
        foo()
    |}
    {|
      def qualifier.foo():
        qualifier.foo()
        $local_qualifier?foo$foo = 1
        $local_qualifier?foo$foo()
    |};
  assert_qualify
    {|
      constant: int = 1
      constant = 2
    |}
    {|
      $local_qualifier$constant: int = 1
      $local_qualifier$constant = 2
    |};
  assert_qualify
    {|
      constant = 1
      def foo():
        constant = 2
        global constant
    |}
    {|
      $local_qualifier$constant = 1
      def qualifier.foo():
        $local_qualifier$constant = 2
        global constant
    |};
  assert_qualify
    {|
      def foo(parameter):
        parameter = 1
    |}
    {|
      def qualifier.foo($parameter$parameter):
        $parameter$parameter = 1
    |};
  assert_qualify
    {|
      def foo(parameter: int, other: parameter.T):
        parameter = 1
    |}
    {|
      def qualifier.foo($parameter$parameter: int, $parameter$other: parameter.T):
        $parameter$parameter = 1
    |};
  assert_qualify
    {|
      flag = False
      if flag:
        variable = 1
      else:
        other = 1
      result = variable
    |}
    {|
      $local_qualifier$flag = False
      if $local_qualifier$flag:
        $local_qualifier$variable = 1
      else:
        $local_qualifier$other = 1
      $local_qualifier$result = $local_qualifier$variable
    |};
  assert_qualify
    {|
      flag = False
      if flag:
        variable = 1
      else:
        variable = 2
      result = variable
    |}
    {|
      $local_qualifier$flag = False
      if $local_qualifier$flag:
        $local_qualifier$variable = 1
      else:
        $local_qualifier$variable = 2
      $local_qualifier$result = $local_qualifier$variable
    |};
  assert_qualify
    {|
      variable = None
      if variable is None:
        variable = 1
      return variable
    |}
    {|
      $local_qualifier$variable = None
      if $local_qualifier$variable is None:
        $local_qualifier$variable = 1
      return $local_qualifier$variable
    |};
  assert_qualify
    {|
      def foo(foo):
        return foo
    |}
    {|
      def qualifier.foo($parameter$foo):
        return $parameter$foo
    |};
  assert_qualify
    {|
      variable = 0
      with item:
        variable = 1
      result = variable
    |}
    {|
      $local_qualifier$variable = 0
      with item:
        $local_qualifier$variable = 1
      $local_qualifier$result = $local_qualifier$variable
    |};
  assert_qualify
    {|
      try:
        variable = 1
      except:
        return None
      else:
        return variable
    |}
    {|
      try:
        $local_qualifier$variable = 1
      except:
        return None
      else:
        return $local_qualifier$variable
    |};
  assert_qualify
    {|
       a: x = ...
       def x():
         ...
    |}
    {|
       $local_qualifier$a: qualifier.x = ...
       def qualifier.x(): ...
    |};
  assert_qualify
    {|
       def f(a: x): ...
       def x(): ...
    |}
    {|
       def qualifier.f($parameter$a: qualifier.x): ...
       def qualifier.x():
         ...
    |};
  assert_qualify
    {|
      class C:
        def f(parameter: x):
          ...
        def x():
          ...
    |}
    {|
      class qualifier.C:
        def qualifier.C.f($parameter$parameter: qualifier.C.x):
          ...
        def qualifier.C.x():
          ...
    |};
  assert_qualify
    {|
      class slice:
        pass
      class C:
        slice: int = ...
    |}
    {|
      class qualifier.slice:
        pass
      class qualifier.C:
        qualifier.C.slice: int = ...
    |};
  assert_qualify
    {|
      def f():
        pass
      class C:
        def f():
          return f()
        a = f
        def g():
          return f()
    |}
    {|
      def qualifier.f():
        pass
      class qualifier.C:
        def qualifier.C.f():
          return qualifier.f()
        qualifier.C.a = qualifier.C.f
        def qualifier.C.g():
          return qualifier.f()
    |};
  assert_qualify
    {|
      class C:
        alias = int
        def f() -> alias:
          return alias
        def g(x: alias):
          pass
    |}
    {|
      class qualifier.C:
        qualifier.C.alias = int
        def qualifier.C.f() -> qualifier.C.alias:
          return alias
        def qualifier.C.g($parameter$x: qualifier.C.alias):
          pass
    |};

  (* Decorator qualification tests *)
  assert_qualify
    {|
      def mydecorator(decorated):
        return decorated
      @mydecorator
      def f():
        pass
    |}
    {|
      def qualifier.mydecorator($parameter$decorated):
        return $parameter$decorated
      @qualifier.mydecorator
      def qualifier.f():
        pass
    |};
  assert_qualify
    {|
      def mydecorator(decorated):
        return decorated
      class C:
        @mydecorator
        def f(self):
            pass
    |}
    {|
      def qualifier.mydecorator($parameter$decorated):
        return $parameter$decorated
      class qualifier.C:
        @qualifier.mydecorator
        def qualifier.C.f($parameter$self):
          pass
    |};
  assert_qualify
    {|
      class D:
        @staticmethod
        def mydecorator(decorated):
          return decorated
      class C:
        @D.mydecorator
        def f(self):
          pass
    |}
    {|
      class qualifier.D:
        @staticmethod
        def qualifier.D.mydecorator($parameter$decorated):
          return $parameter$decorated
      class qualifier.C:
        @qualifier.D.mydecorator
        def qualifier.C.f($parameter$self):
          pass
    |};
  assert_qualify
    {|
      def mydecoratorwrapper(x):
        def mydecorator(decorated):
          return decorated
        return mydecorator
      x = 42
      @mydecoratorwrapper(x)
      def f():
        pass
    |}
    {|
      def qualifier.mydecoratorwrapper($parameter$x):
        def $local_qualifier?mydecoratorwrapper$mydecorator($parameter$decorated):
          return $parameter$decorated
        return $local_qualifier?mydecoratorwrapper$mydecorator
      $local_qualifier$x = 42
      @qualifier.mydecoratorwrapper($local_qualifier$x)
      def qualifier.f():
        pass
    |};
  assert_qualify
    {|
      def mydecoratorwrapper(x):
        def mydecorator(decorated):
          return decorated
        return mydecorator
      class A:
        x = 42
        @mydecoratorwrapper(x)
        def f(self):
            pass
    |}
    {|
      def qualifier.mydecoratorwrapper($parameter$x):
        def $local_qualifier?mydecoratorwrapper$mydecorator($parameter$decorated):
          return $parameter$decorated
        return $local_qualifier?mydecoratorwrapper$mydecorator
      class qualifier.A:
        qualifier.A.x = 42
        @qualifier.mydecoratorwrapper(qualifier.A.x)
        def qualifier.A.f($parameter$self):
            pass
    |};
  assert_qualify
    {|
      def mydecoratorwrapper(x):
        def mydecorator(decorated):
          return decorated
        return mydecorator
      x = 42
      class A:
        y = 42
        @mydecoratorwrapper(x + y)
        def f(self):
            pass
    |}
    {|
      def qualifier.mydecoratorwrapper($parameter$x):
        def $local_qualifier?mydecoratorwrapper$mydecorator($parameter$decorated):
          return $parameter$decorated
        return $local_qualifier?mydecoratorwrapper$mydecorator
      $local_qualifier$x = 42
      class qualifier.A:
        qualifier.A.y = 42
        @qualifier.mydecoratorwrapper($local_qualifier$x + qualifier.A.y)
        def qualifier.A.f($parameter$self):
            pass
    |};
  assert_qualify
    {|
      class A:
        @property
        def f(self):
            return 42
    |}
    {|
      class qualifier.A:
        @property
        def qualifier.A.f($parameter$self):
            return 42
    |};
  assert_qualify
    {|
      class A:
        @property
        def f(self):
            return 42
        @f.setter
        def f(self, f):
            pass
    |}
    {|
      class qualifier.A:
        @property
        def qualifier.A.f($parameter$self):
            return 42
        @f.setter
        def qualifier.A.f($parameter$self, $parameter$f):
            pass
    |}


let test_replace_version_specific_code _ =
  let assert_preprocessed ?(handle = "stub.pyi") source expected =
    assert_source_equal
      ~location_insensitive:true
      (parse ~handle expected)
      (Preprocessing.replace_version_specific_code (parse ~handle source))
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
        if sys.version_info < (3,):
            _encodable = Union[bytes, Text]
            _decodable = Union[bytes, Text]
        elif sys.version_info < (3, 3):
            _encodable = bytes
            _decodable = bytes
        elif sys.version_info[:2] == (3, 3):
            _encodable = bytes
            _decodable = Union[bytes, str]
        elif sys.version_info >= (3, 4):
            _encodable = Union[bytes, bytearray, memoryview]
            _decodable = Union[bytes, bytearray, memoryview, str]
  |}
    {|
        _encodable = Union[bytes, bytearray, memoryview]
        _decodable = Union[bytes, bytearray, memoryview, str]
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
  assert_preprocessed
    ~handle:"file.py"
    {|
      if sys.version_info >= (3, 5):
        from A import B
      else:
        from A import C
    |}
    {|
       from A import B
    |};
  assert_preprocessed
    {|
      if sys.version_info[0] >= 3:
        a = 1
      else:
        a = 2
    |}
    {|
      a = 1
    |};
  assert_preprocessed
    {|
      if sys.version_info[0] < 3:
        a = 1
      else:
        a = 2
    |}
    {|
      a = 2
    |}


let test_replace_platform_specific_code _ =
  let assert_preprocessed ?(handle = "stub.pyi") source expected =
    assert_source_equal
      ~location_insensitive:true
      (parse ~handle expected)
      (Preprocessing.replace_platform_specific_code (parse ~handle source))
  in
  assert_preprocessed
    {|
      if sys.platform != 'win32':
        a = 1
    |}
    {|
      a = 1
    |};
  assert_preprocessed
    {|
      if sys.platform != 'win32':
        a = 2
        b = 3
      else:
        c = 4
    |}
    {|
      a = 2
      b = 3
    |};
  assert_preprocessed
    {|
      if sys.platform != 'win32':
        a = 5
      else:
        b = 6
        c = 7
    |}
    {|
      a = 5
    |};
  assert_preprocessed
    {|
      if sys.platform != 'linux':
        a = 8
    |}
    {|
      if sys.platform != 'linux':
        a = 8
    |};
  assert_preprocessed
    {|
      if sys.platform != 'linux':
        a = 9
      else:
        b = 10
        c = 11
    |}
    {|
      if sys.platform != 'linux':
        a = 9
      else:
        b = 10
        c = 11
    |};
  assert_preprocessed
    {|
      if sys.platform == 'win32' or sys.platform == 'darwin':
        a = 12
    |}
    {|
      if sys.platform == 'win32' or sys.platform == 'darwin':
        a = 12
    |};
  assert_preprocessed
    {|
      if sys.platform == 'win32' or sys.platform == 'darwin':
        a = 13
      else:
        b = 14
    |}
    {|
      if sys.platform == 'win32' or sys.platform == 'darwin':
        a = 13
      else:
        b = 14
    |};
  assert_preprocessed
    {|
      if sys.platform == 'win32' or flag:
        a = 15
      else:
        b = 16
    |}
    {|
      if sys.platform == 'win32' or flag:
        a = 15
      else:
        b = 16
    |};
  assert_preprocessed
    {|
      if sys.platform == 'linux' or flag:
        a = 17
      else:
        b = 18
    |}
    {|
      if sys.platform == 'linux' or flag:
        a = 17
      else:
        b = 18
    |};
  assert_preprocessed
    {|
      if 'win32' == sys.platform:
        a = 19
      else:
        b = 20
        c = 21
    |}
    {|
      b = 20
      c = 21
    |};
  assert_preprocessed
    {|
      if 'linux' == sys.platform:
        a = 22
        b = 23
      else:
        c = 24
    |}
    {|
      if 'linux' == sys.platform:
        a = 22
        b = 23
      else:
        c = 24
    |}


let test_expand_type_checking_imports _ =
  let assert_expanded source expected =
    assert_source_equal
      ~location_insensitive:true
      (parse expected)
      (Preprocessing.expand_type_checking_imports (parse source))
  in
  assert_expanded {|
      if typing.TYPE_CHECKING:
        pass
    |} {|
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
    |};

  (* Nested. *)
  assert_expanded
    {|
      def foo():
        if typing.TYPE_CHECKING:
          pass
    |}
    {|
      def foo():
        pass
    |};

  (* Inverted. *)
  assert_expanded
    {|
      if not typing.TYPE_CHECKING:
        pass
      else:
        1
    |}
    {|
      1
    |}


let test_expand_wildcard_imports context =
  let assert_expanded external_sources check_source expected =
    let ast_environment, _ =
      ScratchProject.setup ~context ~external_sources ["test.py", check_source]
      |> ScratchProject.parse_sources
    in
    assert_equal
      ~cmp:(List.equal (fun left right -> Statement.location_insensitive_compare left right = 0))
      ~printer:(fun statement_list -> List.map statement_list ~f:show |> String.concat ~sep:", ")
      (Source.statements (parse expected))
      (Source.statements
         (Option.value_exn
            (AstEnvironment.ReadOnly.get_processed_source
               (AstEnvironment.read_only ast_environment)
               !&"test")))
  in
  assert_expanded
    ["a.py", "def foo(): pass"]
    {|
      from a import b
    |}
    {|
      from a import b
    |};
  assert_expanded
    ["a.py", "def foo(): pass"]
    {|
      from a import *
    |}
    {|
      from a import foo as foo
    |};
  assert_expanded
    ["a.py", "def foo(): pass"; "b.py", "def bar(): pass"]
    {|
      from a import *
      from b import *
    |}
    {|
      from a import foo as foo
      from b import bar as bar
    |};
  assert_expanded
    [
      ( "a.py",
        {|
        from x import y
        def foo(): pass
        def bar(): pass
        def _private(): pass
      |}
      );
    ]
    {|
      from a import *
    |}
    {|
      from a import bar as bar, foo as foo, y as y
    |};
  assert_expanded
    [
      ( "a.py",
        {|
        from x import y
        def foo(): pass
        def bar(): pass
        __all__ = ["bar"]
      |}
      );
    ]
    {|
      from a import *
    |}
    {|
      from a import bar as bar
    |};

  (* Empty files *)
  assert_expanded ["a.py", ""] {|
      from a import *
    |} "";
  assert_expanded
    ["a.py", ""; "b.py", "x = 1"]
    {|
      from a import *
      from b import *
    |}
    {|
      from b import x as x
    |};
  assert_expanded
    ["a.py", ""; "b.py", "x = 1"]
    {|
      from a import *
      from b import *
    |}
    {|
      from b import x as x
    |};
  assert_expanded
    ["a.py", ""; "b.py", "from a import *"; "c.py", "x = 1"]
    {|
      from a import *
      from b import *
      from c import *
    |}
    {|
      from c import x as x
    |};
  ()


let test_expand_implicit_returns _ =
  let assert_expand source expected =
    assert_source_equal
      ~location_insensitive:true
      (parse expected)
      (Preprocessing.expand_implicit_returns (parse source))
  in
  let assert_expand_implicit_returns source expected_body =
    let handle = "test.py" in
    assert_source_equal
      ~location_insensitive:true
      (Preprocessing.expand_implicit_returns (parse ~handle source))
      (Source.create
         ~relative:handle
         [
           +Statement.Define
              {
                signature =
                  {
                    name = + !&"foo";
                    parameters = [];
                    decorators = [];
                    return_annotation = None;
                    async = false;
                    generator = false;
                    parent = None;
                    nesting_define = None;
                  };
                captures = [];
                unbound_names = [];
                body = expected_body;
              };
         ])
  in
  assert_expand_implicit_returns
    {|
      def foo():
        pass
    |}
    [+Statement.Pass; +Statement.Return { Return.expression = None; is_implicit = true }];
  assert_expand
    {|
      def foo():
        yield None
    |}
    {|
      def foo():
        yield None
    |};
  assert_expand_implicit_returns
    {|
      def foo():
        try:
          pass
        finally:
          pass
    |}
    [
      +Statement.Try
         { Try.body = [+Statement.Pass]; handlers = []; orelse = []; finally = [+Statement.Pass] };
      +Statement.Return { Return.expression = None; is_implicit = true };
    ];

  (* Lol termination analysis. *)
  assert_expand_implicit_returns
    {|
      def foo():
        while derp:
          pass
    |}
    [
      +Statement.While
         {
           While.test = +Expression.Name (Name.Identifier "derp");
           body = [+Statement.Pass];
           orelse = [];
         };
      +Statement.Return { Return.expression = None; is_implicit = true };
    ];
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
    |};
  let assert_implicit_return_location source expected_location =
    let expanded = Preprocessing.expand_implicit_returns (parse ~handle:"test.py" source) in
    match List.rev (Source.statements expanded) with
    | { Node.value = Define { body; _ }; _ } :: _ -> (
        match List.rev body with
        | return :: _ -> assert_equal ~printer:Location.show expected_location return.location
        | _ -> failwith "Preprocessed source's Define body is empty" )
    | _ -> failwith "Preprocessed source failed"
  in
  assert_implicit_return_location
    {|
       def foo() -> int:
         pass
     |}
    {
      Location.start = { Location.line = 3; Location.column = 2 };
      stop = { Location.line = 3; Location.column = 6 };
    };
  assert_implicit_return_location
    {|
     def foo(x: bool) -> int:
       for i in range(10):
         if x:  # multiple
           # lines
           # of
           # comments
           return 1
         elif x:
           return 2
     |}
    {
      Location.start = { Location.line = 10; Location.column = 6 };
      stop = { Location.line = 10; Location.column = 14 };
    };
  assert_implicit_return_location
    {|
       def foo(x: bool) -> int:
         try:
           if x:
             y = 5/0
         except ZeroDivisionError:
           print "tried to divide by 0"
         else:
           pass
       |}
    {
      Location.start = { Location.line = 9; Location.column = 4 };
      stop = { Location.line = 9; Location.column = 8 };
    };
  assert_implicit_return_location
    {|
       def foo(x: bool) -> int:
         try:
           if x:
             y = 5/0
         except ZeroDivisionError:
           print "tried to divide by 0"
         else:
           return 2
         finally:
           for i in range(10):
             if x:
               pass
       |}
    {
      Location.start = { Location.line = 13; Location.column = 8 };
      stop = { Location.line = 13; Location.column = 12 };
    };
  assert_implicit_return_location
    {|
       def foo(x: bool) -> int:
         y = 10
         while y > 0:
           if x:
             y -= 1
         else:
           if y > -1:
             return 0
           else:
             pass
       |}
    {
      Location.start = { Location.line = 11; Location.column = 6 };
      stop = { Location.line = 11; Location.column = 10 };
    }


let test_defines _ =
  let assert_defines statements defines =
    let printer defines = List.map defines ~f:Define.show |> String.concat ~sep:"\n" in
    let source = Source.create statements in
    assert_equal
      ~cmp:(List.equal Define.equal)
      ~printer
      defines
      (Preprocessing.defines ~include_toplevels:true source |> List.map ~f:Node.value);
    assert_equal
      ~cmp:Int.equal
      ~printer:Int.to_string
      ( Preprocessing.defines ~include_stubs:true ~include_nested:true ~include_toplevels:true source
      |> List.length )
      (Preprocessing.count_defines source)
  in
  let create_define name =
    {
      Define.signature =
        {
          name = + !&name;
          parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
          decorators = [];
          return_annotation = None;
          async = false;
          generator = false;
          parent = None;
          nesting_define = None;
        };
      captures = [];
      unbound_names = [];
      body = [+Statement.Expression (+Expression.Float 1.0)];
    }
  in
  let create_toplevel body =
    {
      Define.signature =
        {
          name = + !&"$toplevel";
          parameters = [];
          decorators = [];
          return_annotation = None;
          async = false;
          generator = false;
          parent = None;
          nesting_define = None;
        };
      captures = [];
      unbound_names = [];
      body;
    }
  in
  let create_class_toplevel ~parent ~body =
    {
      Define.signature =
        {
          name = + !&(parent ^ ".$class_toplevel");
          parameters = [];
          decorators = [];
          return_annotation = None;
          async = false;
          generator = false;
          parent = Some (Reference.create parent);
          nesting_define = None;
        };
      captures = [];
      unbound_names = [];
      body;
    }
  in
  let define = create_define "foo" in
  assert_defines [+Statement.Define define] [create_toplevel [+Statement.Define define]; define];
  let inner =
    {
      Define.signature =
        {
          name = + !&"foo";
          parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
          decorators = [];
          return_annotation = None;
          async = false;
          generator = false;
          parent = None;
          nesting_define = None;
        };
      captures = [];
      unbound_names = [];
      body = [+Statement.Expression (+Expression.Float 1.0)];
    }
  in
  let define =
    {
      Define.signature =
        {
          name = + !&"foo";
          parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
          decorators = [];
          return_annotation = None;
          async = false;
          generator = false;
          parent = None;
          nesting_define = None;
        };
      captures = [];
      unbound_names = [];
      body = [+Statement.Expression (+Expression.Float 1.0); +Statement.Define inner];
    }
  in
  assert_defines [+Statement.Define define] [create_toplevel [+Statement.Define define]; define];
  let if_define =
    { If.test = +Expression.Ellipsis; body = [+Statement.Define define]; orelse = [] }
  in
  assert_defines [+Statement.If if_define] [create_toplevel [+Statement.If if_define]];

  (* Note: Defines are returned in reverse order. *)
  let define_foo = create_define "foo" in
  let define_bar = create_define "bar" in
  let body = [+Statement.Define define_foo; +Statement.Define define_bar] in
  let parent =
    { Class.name = + !&"Foo"; bases = []; body; decorators = []; top_level_unbound_names = [] }
  in
  assert_defines
    [+Statement.Class parent]
    [
      create_toplevel [+Statement.Class parent];
      create_class_toplevel ~parent:"Foo" ~body;
      define_bar;
      define_foo;
    ]


let test_classes _ =
  let assert_classes statements class_defines =
    assert_equal
      ~cmp:(List.equal Class.equal)
      (Preprocessing.classes (Source.create statements) |> List.map ~f:Node.value)
      class_defines
  in
  let class_define =
    {
      Class.name = + !&"foo";
      bases = [];
      body =
        [
          +Statement.Define
             {
               signature =
                 {
                   name = + !&"bar";
                   parameters = [];
                   decorators = [];
                   return_annotation = None;
                   async = false;
                   generator = false;
                   parent = Some !&"foo";
                   nesting_define = None;
                 };
               captures = [];
               unbound_names = [];
               body = [+Statement.Pass];
             };
        ];
      decorators = [];
      top_level_unbound_names = [];
    }
  in
  assert_classes [+Statement.Class class_define] [class_define];
  let inner =
    {
      Class.name = + !&"bar";
      bases = [];
      body = [+Statement.Pass];
      decorators = [];
      top_level_unbound_names = [];
    }
  in
  let class_define =
    {
      Class.name = + !&"foo";
      bases = [];
      body = [+Statement.Class inner];
      decorators = [];
      top_level_unbound_names = [];
    }
  in
  assert_classes [+Statement.Class class_define] [class_define; inner]


let test_replace_mypy_extensions_stub _ =
  let given =
    parse
      ~handle:"mypy_extensions.pyi"
      {|
      from typing import Dict, Type, TypeVar, Optional, Union, Any, Generic

      _T = TypeVar('_T')
      _U = TypeVar('_U')

      def TypedDict(typename: str, fields: Dict[str, Type[_T]], total: bool = ...) -> Type[dict]:
        ...

      def Arg(type: _T = ..., name: Optional[str] = ...) -> _T: ...
      def DefaultArg(type: _T = ..., name: Optional[str] = ...) -> _T: ...
    |}
  in
  let expected =
    parse
      ~handle:"mypy_extensions.pyi"
      {|
      from typing import Dict, Type, TypeVar, Optional, Union, Any, Generic

      _T = TypeVar('_T')
      _U = TypeVar('_U')

      TypedDict: typing._SpecialForm = ...

      def Arg(type: _T = ..., name: Optional[str] = ...) -> _T: ...
      def DefaultArg(type: _T = ..., name: Optional[str] = ...) -> _T: ...
    |}
  in
  assert_source_equal
    ~location_insensitive:true
    expected
    (Preprocessing.replace_mypy_extensions_stub given)


let test_expand_typed_dictionaries _ =
  let assert_expand ?(handle = "") source expected =
    let expected = parse ~handle ~coerce_special_methods:true expected |> Preprocessing.qualify in
    let actual =
      parse ~handle source
      |> Preprocessing.qualify
      |> Preprocessing.expand_typed_dictionary_declarations
    in
    assert_source_equal ~location_insensitive:true expected actual
  in
  (* Vary the module from which TypedDict is imported. *)
  assert_expand
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
    |}
    {|
      class Movie(TypedDictionary):
        name: str
        year: int
    |};
  assert_expand
    {|
      Movie = typing_extensions.TypedDict('Movie', {'name': str, 'year': int})
    |}
    {|
      class Movie(TypedDictionary):
        name: str
        year: int
    |};
  assert_expand
    {|
      Movie = typing.TypedDict('Movie', {'name': str, 'year': int})
    |}
    {|
      class Movie(TypedDictionary):
        name: str
        year: int
    |};
  assert_expand
    {|
      Movie = mypy_extensions.TypedDict('Movie', {})
    |}
    {|
      class Movie(TypedDictionary): pass
    |};
  assert_expand
    {|
      Movie = mypy_extensions.TypedDict('Movie', {}, total=False)
    |}
    {|
      class Movie(TypedDictionary, NonTotalTypedDictionary): pass
    |};
  assert_expand
    {|
      Movie = mypy_extensions.TypedDict('Movie', {}, total=True)
    |}
    {|
      class Movie(TypedDictionary): pass
    |};
  assert_expand
    {|
      class Movie(mypy_extensions.TypedDict):
        name: str
        year: int
    |}
    {|
      class Movie(TypedDictionary):
        name: str
        year: int
    |};
  assert_expand
    {|
      class Movie(typing_extensions.TypedDict):
        name: str
        year: int
    |}
    {|
      class Movie(TypedDictionary):
        name: str
        year: int
    |};
  assert_expand
    {|
      class Movie(typing.TypedDict):
        name: str
        year: int
    |}
    {|
      class Movie(TypedDictionary):
        name: str
        year: int
    |};
  assert_expand
    {|
      class Movie(mypy_extensions.TypedDict):
        name: str
        year: int
    |}
    {|
      class Movie(TypedDictionary):
        name: str
        year: int
    |};
  assert_expand
    {|
      class Movie(mypy_extensions.TypedDict, total=False):
        name: str
        year: int
    |}
    {|
      class Movie(TypedDictionary, NonTotalTypedDictionary):
        name: str
        year: int
    |};
  assert_expand
    {|
      class Movie(mypy_extensions.TypedDict, total=True):
        name: str
        year: int
    |}
    {|
      class Movie(TypedDictionary):
        name: str
        year: int
    |};
  assert_expand
    {|
      class Movie(mypy_extensions.TypedDict, total=True):
        """docstring"""
        name: str
        year: int
    |}
    {|
      class Movie(TypedDictionary):
        name: str
        year: int
    |};

  (* Invalid TypedDicts *)
  assert_expand
    {|
      NamelessTypedDict = mypy_extensions.TypedDict({'name': str, 'year': int})
    |}
    {|
      NamelessTypedDict = mypy_extensions.TypedDict({'name': str, 'year': int})
    |};
  assert_expand
    {|
      TypedDictWithWrongArity = mypy_extensions.TypedDict(A, B, C)
    |}
    {|
      TypedDictWithWrongArity = mypy_extensions.TypedDict(A, B, C)
    |};
  assert_expand
    {|
      class Movie(mypy_extensions.TypedDict, total=True):
        name: str
        year: int
        def ignored_method(self) -> None: pass
    |}
    {|
      class Movie(TypedDictionary):
        name: str
        year: int
    |};
  assert_expand
    {|
      class Movie(mypy_extensions.TypedDict, total=True, total=False):
        name: str
        year: int
    |}
    {|
      class Movie(TypedDictionary):
        name: str
        year: int
    |};
  assert_expand
    {|
      class Movie(mypy_extensions.TypedDict, garbage=7):
        name: str
        year: int
    |}
    {|
      class Movie(TypedDictionary):
        name: str
        year: int
    |};
  assert_expand
    {|
      class Movie(mypy_extensions.TypedDict, OtherClass):
        name: str
        year: int
    |}
    {|
      class Movie(TypedDictionary):
        name: str
        year: int
    |};
  assert_expand
    {|
      class Movie(TypedDictionary):
        name: str
        year: int
    |}
    {|
      class Movie(TypedDictionary):
        name: str
        year: int
    |};
  assert_expand
    {|
      class Movie(MovieBase, total=True):
        name: str
        year: int
    |}
    {|
      class Movie(MovieBase):
        name: str
        year: int
    |};
  assert_expand
    {|
      class Movie(MovieBase, total=False):
        name: str
        year: int
    |}
    {|
      class Movie(MovieBase, NonTotalTypedDictionary):
        name: str
        year: int
    |};
  ()


let test_sqlalchemy_declarative_base _ =
  let assert_expand ?(handle = "") source expected =
    let expected = parse ~handle ~coerce_special_methods:true expected |> Preprocessing.qualify in
    let actual =
      parse ~handle source
      |> Preprocessing.qualify
      |> Preprocessing.expand_sqlalchemy_declarative_base
    in
    assert_source_equal ~location_insensitive:true expected actual
  in
  assert_expand
    {|
      Base = sqlalchemy.ext.declarative.declarative_base()
    |}
    {|
      class Base(metaclass=sqlalchemy.ext.declarative.DeclarativeMeta):
        pass
    |};
  ()


let test_transform_ast _ =
  let assert_expand ?(handle = "qualifier.py") source expected =
    let parse source =
      parse source ~handle |> Preprocessing.qualify |> Preprocessing.expand_implicit_returns
    in
    assert_source_equal
      ~location_insensitive:true
      (parse expected)
      (Preprocessing.expand_named_tuples (parse source))
  in
  assert_expand
    {|
      T = typing.NamedTuple('T')
    |}
    {|
      class T(typing.NamedTuple):
        def __new__(cls) -> typing.NamedTuple: ...
        def __init__(self) -> None: ...
        _fields: typing.Tuple[()] = ()
    |};
  assert_expand
    {|
      T = collections.namedtuple('T', ['a'])
    |}
    {|
      class T(typing.NamedTuple):
        def __new__(cls, a: typing.Any) -> typing.NamedTuple: ...
        def __init__(self, a: typing.Any) -> None:
          self.a = a
        _fields: typing.Tuple[str] = ('a',)
        a: typing.Final[typing.Any]
    |};
  assert_expand
    {|
      T = typing.NamedTuple('T', ['one', 'two'])
    |}
    {|
      class T(typing.NamedTuple):
        def __new__(cls, one: typing.Any, two: typing.Any) -> typing.NamedTuple: ...
        def __init__(self, one: typing.Any, two: typing.Any) -> None:
         self.one = one
         self.two = two
        _fields: typing.Tuple[str, str] = ('one', 'two')
        one: typing.Final[typing.Any]
        two: typing.Final[typing.Any]
    |};
  assert_expand
    {|
      T = typing.NamedTuple('T', [('one', int), ('two', str)])
    |}
    {|
      class T(typing.NamedTuple):
        def __new__(cls, one: int, two: str) -> typing.NamedTuple: ...
        def __init__(self, one: int, two: str) -> None:
         self.one = one
         self.two = two
        _fields: typing.Tuple[str, str] = ('one', 'two')
        one: typing.Final[int]
        two: typing.Final[str]
    |};
  assert_expand
    {|
      T = collections.namedtuple('T', 'a b c')
    |}
    {|
      class T(typing.NamedTuple):
        def __new__(
          cls,
          a: typing.Any,
          b: typing.Any,
          c: typing.Any) -> typing.NamedTuple: ...
        def __init__(self, a: typing.Any, b: typing.Any, c: typing.Any) -> None:
          self.a = a
          self.b = b
          self.c = c
        _fields: typing.Tuple[str, str, str] = ('a', 'b', 'c')
        a: typing.Final[typing.Any]
        b: typing.Final[typing.Any]
        c: typing.Final[typing.Any]
    |};
  assert_expand
    {|
      class Foo(Bar, collections.namedtuple('T', ['one', 'two'])):
        three: int = 1
    |}
    {|
      class Foo(Bar, typing.NamedTuple):
        def __new__(cls, one: typing.Any, two: typing.Any) -> typing.NamedTuple: ...
        def __init__(self, one: typing.Any, two: typing.Any) -> None:
          self.one = one
          self.two = two
        _fields: typing.Tuple[str, str] = ('one', 'two')
        one: typing.Final[typing.Any]
        two: typing.Final[typing.Any]
        three: int = 1
    |};
  assert_expand
    {|
      class Foo(typing.NamedTuple):
        a: int
        b: str
        c: int = 3
    |}
    {|
      class Foo(typing.NamedTuple):
        def __new__(cls, a: int, b: str, c: int = 3) -> typing.NamedTuple: ...
        def __init__(self, a: int, b: str, c: int = 3) -> None:
          self.a = a
          self.b = b
          self.c = c
        _fields: typing.Tuple[str, str, str] = ('a', 'b', 'c')
        a: typing.Final[int]
        b: typing.Final[str]
        c: typing.Final[int]
    |};
  assert_expand
    {|
      class Foo(collections.namedtuple("PatchDocument", ("op", "path", "value", "ts", "lazy"))):
        pass
    |}
    {|
      class Foo(typing.NamedTuple):
         def __new__(
           cls,
           op: typing.Any,
           path: typing.Any,
           value: typing.Any,
           ts: typing.Any,
           lazy: typing.Any) -> typing.NamedTuple:
           ...
         def __init__(
           self,
           op: typing.Any,
           path: typing.Any,
           value: typing.Any,
           ts: typing.Any,
           lazy: typing.Any) -> None:
           self.op = op
           self.path = path
           self.value = value
           self.ts = ts
           self.lazy = lazy
         _fields: typing.Tuple[str, str, str, str, str] = ('op', 'path', 'value', 'ts', 'lazy')
         op: typing.Final[typing.Any]
         path: typing.Final[typing.Any]
         value: typing.Final[typing.Any]
         ts: typing.Final[typing.Any]
         lazy: typing.Final[typing.Any]
         pass
    |};
  assert_expand
    {|
      class Foo:
        T = collections.namedtuple('T', ("a", "b"))
    |}
    {|
      class Foo:
        class T(typing.NamedTuple):
          def __new__(cls, a: typing.Any, b: typing.Any) -> typing.NamedTuple: ...
          def __init__(self, a: typing.Any, b: typing.Any) -> None:
            self.a = a
            self.b = b
          _fields: typing.Tuple[str, str] = ('a', 'b')
          a: typing.Final[typing.Any]
          b: typing.Final[typing.Any]
    |};
  assert_expand
    {|
      def foo():
        T = typing.NamedTuple('T')
    |}
    {|
      def foo():
        class T(typing.NamedTuple):
          def __new__(cls) -> typing.NamedTuple: ...
          def __init__(self) -> None: ...
          _fields: typing.Tuple[()] = ()
    |};
  assert_expand
    {|
      class Foo:
        def __new__(cls) -> typing.NamedTuple:
          cls.t = typing.NamedTuple('T', 'a')
    |}
    {|
      class Foo:
        def __new__(cls) -> typing.NamedTuple:
          cls.t = typing.NamedTuple('T', 'a')
    |};
  assert_expand
    {|
      class Foo(collections.namedtuple('T', ['one', 'two'])):
        def __new__(cls, one) -> typing.NamedTuple:
          return super(Foo, cls).__new__(cls, one, two=0)
    |}
    {|
      class Foo(typing.NamedTuple):
        _fields: typing.Tuple[str, str] = ('one', 'two')
        one: typing.Final[typing.Any]
        two: typing.Final[typing.Any]
        def __new__(cls, one) -> typing.NamedTuple:
          return super(Foo, cls).__new__(cls, one, two=0)
    |};
  assert_expand
    {|
      T = typing.NamedTuple('T', one=int, two=str)
    |}
    {|
      class T(typing.NamedTuple):
        def __new__(cls, one: int, two: str) -> typing.NamedTuple: ...
        def __init__(self, one: int, two: str) -> None:
          self.one = one
          self.two = two
        _fields: typing.Tuple[str, str] = ('one', 'two')
        one: typing.Final[int]
        two: typing.Final[str]
    |}


let test_populate_nesting_define _ =
  let assert_populated source_text expected =
    let source = Test.parse ~handle:"test.py" (trim_extra_indentation source_text) in
    let { Source.statements = actual; _ } = Preprocessing.populate_nesting_defines source in
    assert_equal
      ~cmp:(List.equal (fun a b -> Statement.location_insensitive_compare a b = 0))
      ~printer:
        (List.to_string ~f:(fun statement -> Sexp.to_string_hum (Statement.sexp_of_t statement)))
      expected
      actual
  in
  assert_populated
    "def foo():\n  def bar():\n    1\n    2\n3"
    [
      +Statement.Define
         {
           signature =
             {
               name = + !&"foo";
               parameters = [];
               decorators = [];
               return_annotation = None;
               async = false;
               generator = false;
               parent = None;
               nesting_define = None;
             };
           captures = [];
           unbound_names = [];
           body =
             [
               +Statement.Define
                  {
                    signature =
                      {
                        name = + !&"bar";
                        parameters = [];
                        decorators = [];
                        return_annotation = None;
                        async = false;
                        generator = false;
                        parent = None;
                        nesting_define = Some !&"foo";
                      };
                    captures = [];
                    unbound_names = [];
                    body =
                      [
                        +Statement.Expression (+Expression.Integer 1);
                        +Statement.Expression (+Expression.Integer 2);
                      ];
                  };
             ];
         };
      +Statement.Expression (+Expression.Integer 3);
    ];

  assert_populated
    {|
        def foo():
          def bar():
            pass
          def baz():
            pass
    |}
    [
      +Statement.Define
         {
           signature =
             {
               name = + !&"foo";
               parameters = [];
               decorators = [];
               return_annotation = None;
               async = false;
               generator = false;
               parent = None;
               nesting_define = None;
             };
           captures = [];
           unbound_names = [];
           body =
             [
               +Statement.Define
                  {
                    signature =
                      {
                        name = + !&"bar";
                        parameters = [];
                        decorators = [];
                        return_annotation = None;
                        async = false;
                        generator = false;
                        parent = None;
                        nesting_define = Some !&"foo";
                      };
                    captures = [];
                    unbound_names = [];
                    body = [+Statement.Pass];
                  };
               +Statement.Define
                  {
                    signature =
                      {
                        name = + !&"baz";
                        parameters = [];
                        decorators = [];
                        return_annotation = None;
                        async = false;
                        generator = false;
                        parent = None;
                        nesting_define = Some !&"foo";
                      };
                    captures = [];
                    unbound_names = [];
                    body = [+Statement.Pass];
                  };
             ];
         };
    ];
  assert_populated
    {|
        def foo():
          def bar():
            def baz():
              pass
    |}
    [
      +Statement.Define
         {
           signature =
             {
               name = + !&"foo";
               parameters = [];
               decorators = [];
               return_annotation = None;
               async = false;
               generator = false;
               parent = None;
               nesting_define = None;
             };
           captures = [];
           unbound_names = [];
           body =
             [
               +Statement.Define
                  {
                    signature =
                      {
                        name = + !&"bar";
                        parameters = [];
                        decorators = [];
                        return_annotation = None;
                        async = false;
                        generator = false;
                        parent = None;
                        nesting_define = Some !&"foo";
                      };
                    captures = [];
                    unbound_names = [];
                    body =
                      [
                        +Statement.Define
                           {
                             signature =
                               {
                                 name = + !&"baz";
                                 parameters = [];
                                 decorators = [];
                                 return_annotation = None;
                                 async = false;
                                 generator = false;
                                 parent = None;
                                 nesting_define = Some !&"bar";
                               };
                             captures = [];
                             unbound_names = [];
                             body = [+Statement.Pass];
                           };
                      ];
                  };
             ];
         };
    ];
  assert_populated
    {|
        def foo():
          if True:
            def bar():
              pass
          else:
            def baz():
              pass
    |}
    [
      +Statement.Define
         {
           signature =
             {
               name = + !&"foo";
               parameters = [];
               decorators = [];
               return_annotation = None;
               async = false;
               generator = false;
               parent = None;
               nesting_define = None;
             };
           captures = [];
           unbound_names = [];
           body =
             [
               +Statement.If
                  {
                    If.test = +Expression.True;
                    body =
                      [
                        +Statement.Define
                           {
                             signature =
                               {
                                 name = + !&"bar";
                                 parameters = [];
                                 decorators = [];
                                 return_annotation = None;
                                 async = false;
                                 generator = false;
                                 parent = None;
                                 nesting_define = Some !&"foo";
                               };
                             captures = [];
                             unbound_names = [];
                             body = [+Statement.Pass];
                           };
                      ];
                    orelse =
                      [
                        +Statement.Define
                           {
                             signature =
                               {
                                 name = + !&"baz";
                                 parameters = [];
                                 decorators = [];
                                 return_annotation = None;
                                 async = false;
                                 generator = false;
                                 parent = None;
                                 nesting_define = Some !&"foo";
                               };
                             captures = [];
                             unbound_names = [];
                             body = [+Statement.Pass];
                           };
                      ];
                  };
             ];
         };
    ];
  assert_populated
    {|
        def foo():
          while True:
            def bar():
              pass
          else:
            def baz():
              pass
    |}
    [
      +Statement.Define
         {
           signature =
             {
               name = + !&"foo";
               parameters = [];
               decorators = [];
               return_annotation = None;
               async = false;
               generator = false;
               parent = None;
               nesting_define = None;
             };
           captures = [];
           unbound_names = [];
           body =
             [
               +Statement.While
                  {
                    While.test = +Expression.True;
                    body =
                      [
                        +Statement.Define
                           {
                             signature =
                               {
                                 name = + !&"bar";
                                 parameters = [];
                                 decorators = [];
                                 return_annotation = None;
                                 async = false;
                                 generator = false;
                                 parent = None;
                                 nesting_define = Some !&"foo";
                               };
                             captures = [];
                             unbound_names = [];
                             body = [+Statement.Pass];
                           };
                      ];
                    orelse =
                      [
                        +Statement.Define
                           {
                             signature =
                               {
                                 name = + !&"baz";
                                 parameters = [];
                                 decorators = [];
                                 return_annotation = None;
                                 async = false;
                                 generator = false;
                                 parent = None;
                                 nesting_define = Some !&"foo";
                               };
                             captures = [];
                             unbound_names = [];
                             body = [+Statement.Pass];
                           };
                      ];
                  };
             ];
         };
    ];
  assert_populated
    {|
        def foo():
          with True:
            def bar():
              pass
    |}
    [
      +Statement.Define
         {
           signature =
             {
               name = + !&"foo";
               parameters = [];
               decorators = [];
               return_annotation = None;
               async = false;
               generator = false;
               parent = None;
               nesting_define = None;
             };
           captures = [];
           unbound_names = [];
           body =
             [
               +Statement.With
                  {
                    With.items = [+Expression.True, None];
                    async = false;
                    body =
                      [
                        +Statement.Define
                           {
                             signature =
                               {
                                 name = + !&"bar";
                                 parameters = [];
                                 decorators = [];
                                 return_annotation = None;
                                 async = false;
                                 generator = false;
                                 parent = None;
                                 nesting_define = Some !&"foo";
                               };
                             captures = [];
                             unbound_names = [];
                             body = [+Statement.Pass];
                           };
                      ];
                  };
             ];
         };
    ];
  assert_populated
    {|
        def foo():
          try:
            def bar():
              pass
          except:
            def baz():
              pass
          finally:
            def qux():
              pass
    |}
    [
      +Statement.Define
         {
           signature =
             {
               name = + !&"foo";
               parameters = [];
               decorators = [];
               return_annotation = None;
               async = false;
               generator = false;
               parent = None;
               nesting_define = None;
             };
           captures = [];
           unbound_names = [];
           body =
             [
               +Statement.Try
                  {
                    Try.body =
                      [
                        +Statement.Define
                           {
                             signature =
                               {
                                 name = + !&"bar";
                                 parameters = [];
                                 decorators = [];
                                 return_annotation = None;
                                 async = false;
                                 generator = false;
                                 parent = None;
                                 nesting_define = Some !&"foo";
                               };
                             captures = [];
                             unbound_names = [];
                             body = [+Statement.Pass];
                           };
                      ];
                    orelse = [];
                    handlers =
                      [
                        {
                          Try.Handler.kind = None;
                          name = None;
                          body =
                            [
                              +Statement.Define
                                 {
                                   signature =
                                     {
                                       name = + !&"baz";
                                       parameters = [];
                                       decorators = [];
                                       return_annotation = None;
                                       async = false;
                                       generator = false;
                                       parent = None;
                                       nesting_define = Some !&"foo";
                                     };
                                   captures = [];
                                   unbound_names = [];
                                   body = [+Statement.Pass];
                                 };
                            ];
                        };
                      ];
                    finally =
                      [
                        +Statement.Define
                           {
                             signature =
                               {
                                 name = + !&"qux";
                                 parameters = [];
                                 decorators = [];
                                 return_annotation = None;
                                 async = false;
                                 generator = false;
                                 parent = None;
                                 nesting_define = Some !&"foo";
                               };
                             captures = [];
                             unbound_names = [];
                             body = [+Statement.Pass];
                           };
                      ];
                  };
             ];
         };
    ];
  assert_populated
    {|
        class C:
          def bar():
            def baz():
              pass
    |}
    [
      +Statement.Class
         {
           Class.name = + !&"C";
           bases = [];
           decorators = [];
           body =
             [
               +Statement.Define
                  {
                    signature =
                      {
                        name = + !&"bar";
                        parameters = [];
                        decorators = [];
                        return_annotation = None;
                        async = false;
                        generator = false;
                        parent = Some !&"C";
                        nesting_define = None;
                      };
                    captures = [];
                    unbound_names = [];
                    body =
                      [
                        +Statement.Define
                           {
                             signature =
                               {
                                 name = + !&"baz";
                                 parameters = [];
                                 decorators = [];
                                 return_annotation = None;
                                 async = false;
                                 generator = false;
                                 parent = None;
                                 nesting_define = Some !&"bar";
                               };
                             captures = [];
                             unbound_names = [];
                             body = [+Statement.Pass];
                           };
                      ];
                  };
             ];
           top_level_unbound_names = [];
         };
    ];
  assert_populated
    {|
        def foo():
          class C:
            def bar():
              def baz():
                pass
    |}
    [
      +Statement.Define
         {
           signature =
             {
               name = + !&"foo";
               parameters = [];
               decorators = [];
               return_annotation = None;
               async = false;
               generator = false;
               parent = None;
               nesting_define = None;
             };
           captures = [];
           unbound_names = [];
           body =
             [
               +Statement.Class
                  {
                    Class.name = + !&"C";
                    bases = [];
                    decorators = [];
                    body =
                      [
                        +Statement.Define
                           {
                             signature =
                               {
                                 name = + !&"bar";
                                 parameters = [];
                                 decorators = [];
                                 return_annotation = None;
                                 async = false;
                                 generator = false;
                                 parent = Some !&"C";
                                 nesting_define = None;
                               };
                             captures = [];
                             unbound_names = [];
                             body =
                               [
                                 +Statement.Define
                                    {
                                      signature =
                                        {
                                          name = + !&"baz";
                                          parameters = [];
                                          decorators = [];
                                          return_annotation = None;
                                          async = false;
                                          generator = false;
                                          parent = None;
                                          nesting_define = Some !&"bar";
                                        };
                                      captures = [];
                                      unbound_names = [];
                                      body = [+Statement.Pass];
                                    };
                               ];
                           };
                      ];
                    top_level_unbound_names = [];
                  };
             ];
         };
    ];
  ()


let location (start_line, start_column) (stop_line, stop_column) =
  {
    Location.start = { Location.line = start_line; Location.column = start_column };
    stop = { Location.line = stop_line; Location.column = stop_column };
  }


let test_populate_captures _ =
  let assert_captures ~expected source_text =
    let source =
      Test.parse ~handle:"test.py" source_text
      |> Preprocessing.expand_format_string
      |> Preprocessing.populate_captures
    in
    let defines =
      Preprocessing.defines
        ~include_toplevels:true
        ~include_nested:true
        ~include_methods:true
        source
    in
    let capture_map =
      let build_capture_map
          sofar
          { Node.value = { Define.signature = { Define.Signature.name; _ }; captures; _ }; _ }
        =
        Reference.Map.set sofar ~key:(Node.value name) ~data:captures
      in
      List.fold defines ~init:Reference.Map.empty ~f:build_capture_map
    in
    let assert_captures name expected =
      let expected = List.map expected ~f:(fun (name, kind) -> { Define.Capture.name; kind }) in
      let actual = Reference.Map.find capture_map name |> Option.value ~default:[] in
      assert_equal
        ~cmp:[%compare.equal: Define.Capture.t list]
        ~printer:(fun captures -> Sexp.to_string_hum [%message (captures : Define.Capture.t list)])
        expected
        actual
    in
    List.iter expected ~f:(fun (name, captures) -> assert_captures name captures)
  in
  let tuple_annotation value_annotation start stop =
    Node.create
      (Expression.Call
         {
           callee =
             Node.create
               (Expression.Name
                  (Name.Attribute
                     {
                       base =
                         Node.create
                           (Expression.Name
                              (create_name ~location:(location start stop) "typing.Tuple"))
                           ~location:(location start stop);
                       attribute = "__getitem__";
                       special = true;
                     }))
               ~location:(location start stop);
           arguments =
             [
               {
                 Call.Argument.name = None;
                 value =
                   Node.create
                     ~location:(location start stop)
                     (Expression.Tuple
                        [
                          value_annotation;
                          Node.create ~location:(location start stop) Expression.Ellipsis;
                        ]);
               };
             ];
         })
      ~location:(location start stop)
  in
  let dict_annotation value_annotation start stop =
    Node.create
      (Expression.Call
         {
           callee =
             Node.create
               ~location:(location start stop)
               (Expression.Name
                  (Name.Attribute
                     {
                       base =
                         Node.create
                           (Expression.Name
                              (create_name ~location:(location start stop) "typing.Dict"))
                           ~location:(location start stop);
                       attribute = "__getitem__";
                       special = true;
                     }));
           arguments =
             [
               {
                 Call.Argument.name = None;
                 value =
                   Node.create
                     ~location:(location start stop)
                     (Expression.Tuple
                        [
                          Node.create
                            (Expression.Name (create_name ~location:(location start stop) "str"))
                            ~location:(location start stop);
                          value_annotation;
                        ]);
               };
             ];
         })
      ~location:(location start stop)
  in
  let int_annotation start stop =
    Node.create
      (Expression.Name (create_name ~location:(location start stop) "int"))
      ~location:(location start stop)
  in
  let any_annotation start stop =
    Node.create
      (Expression.Name (create_name ~location:(location start stop) "typing.Any"))
      ~location:(location start stop)
  in
  let tuple_int_annotation (start, stop) (int_start, int_stop) =
    tuple_annotation (int_annotation int_start int_stop) start stop
  in
  let tuple_any_annotation start stop = tuple_annotation (any_annotation start stop) start stop in
  let dict_int_annotation (start, stop) (int_start, int_stop) =
    dict_annotation (int_annotation int_start int_stop) start stop
  in
  let dict_any_annotation (start, stop) = dict_annotation (any_annotation start stop) start stop in
  assert_captures
    {|
     def foo():
       pass
  |}
    ~expected:[!&"foo", []; !&"test.$toplevel", []];
  assert_captures
    {|
     def foo(x: int):
       def bar():
         y = x
  |}
    ~expected:[!&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14)))]];
  assert_captures
    {|
     def foo(x: int):
       def bar(y: int) -> int:
         return y
       def baz():
         return bar()
  |}
    ~expected:
      [
        ( !&"baz",
          [
            ( "bar",
              DefineSignature
                {
                  Define.Signature.name = Node.create ~location:(location (3, 6) (3, 9)) !&"bar";
                  parameters =
                    [
                      Node.create
                        {
                          Parameter.name = "y";
                          value = None;
                          annotation = Some (int_annotation (3, 13) (3, 16));
                        }
                        ~location:(location (3, 10) (3, 11));
                    ];
                  decorators = [];
                  return_annotation = Some (int_annotation (3, 21) (3, 24));
                  async = false;
                  generator = false;
                  parent = None;
                  nesting_define = None;
                } );
          ] );
      ];
  assert_captures
    {|
     def foo(x: int):
       def bar():
         nonlocal x
         x = 1
  |}
    ~expected:[!&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14)))]];
  (* x in `bar` will shadow x in `foo` *)
  assert_captures
    {|
     def foo(x: int):
       def bar():
         x = 1
  |}
    ~expected:[!&"bar", []];
  assert_captures
    {|
     def foo(x: int):
       def bar():
         print(x)
         x = 2
  |}
    ~expected:[!&"bar", []];
  (* Do not capture global variables *)
  assert_captures
    {|
     x: int = 1
     def foo():
       def bar():
         x = 1
  |}
    ~expected:[!&"bar", []];
  assert_captures
    {|
     x: int = 1
     def foo():
       def bar():
         global x
         x = 1
  |}
    ~expected:[!&"bar", []];
  (* Do not capture unused bindings *)
  assert_captures
    {|
     def foo(x: int):
       def bar():
         pass
  |}
    ~expected:[!&"foo", []; !&"bar", []];
  (* Do not capture imported names *)
  assert_captures
    {|
     def foo():
       import bar
       def baz():
         return bar.derp()
  |}
    ~expected:[!&"bar", []];

  (* Nesting functions should be correctly detected *)
  assert_captures
    {|
     def foo(x: int):
       y = 1
       def bar():
         def baz():
           return x
         def qux():
           return y
  |}
    ~expected:
      [
        !&"baz", ["x", Annotation (Some (int_annotation (2, 11) (2, 14)))];
        !&"qux", ["y", Annotation None];
      ];
  assert_captures
    {|
     def foo(x: int):
       y = 1
       class Bar:
         z: int = 2
         def baz(self):
           return x + self.z
         def qux(self):
           return y + self.z
  |}
    ~expected:
      [
        !&"baz", ["x", Annotation (Some (int_annotation (2, 11) (2, 14)))];
        !&"qux", ["y", Annotation None];
      ];

  (* Test accesses collection *)
  assert_captures
    {|
     def foo(x: int):
       y = 1
       def bar():
         assert (x == y)
  |}
    ~expected:
      [!&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None]];
  assert_captures
    {|
     def foo(x: int):
       y = 1
       def bar():
         x
  |}
    ~expected:[!&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14)))]];
  assert_captures
    {|
     def foo(x: int):
       y = 1
       def bar(z: int = y):
         x
  |}
    ~expected:[!&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14)))]];
  assert_captures
    {|
     def foo(x: int):
       y = 1
       def bar():
        def baz(z: int = y):
            x
  |}
    ~expected:[!&"bar", ["y", Annotation None]];
  assert_captures
    {|
     def foo(x: int):
       y = 1
       def bar():
         yield x
         return y
  |}
    ~expected:
      [!&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None]];
  assert_captures
    {|
     def foo(x: int):
       y = 1
       def bar():
         for i in range(x):
           print(y)
  |}
    ~expected:
      [!&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None]];
  assert_captures
    {|
     def foo(x: int):
       y = 1
       def bar(flag: bool):
         if flag:
           return x
         else:
           return y
  |}
    ~expected:
      [!&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None]];
  assert_captures
    {|
     def foo(x: int):
       y = 1
       def bar():
         while x > 1:
           print(y)
  |}
    ~expected:
      [!&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None]];
  assert_captures
    {|
     def foo(x: int):
       y = 1
       def bar():
         raise ValueError(x + y)
  |}
    ~expected:
      [!&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None]];
  assert_captures
    {|
     def foo(x: int):
       y = 1
       def bar():
         try:
           return x
         except:
           return y
         finally:
           return x + y
  |}
    ~expected:
      [!&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None]];
  assert_captures
    {|
     def foo(x: int):
       y = 1
       def bar():
         with open('test.txt', 'r' if x > 1 else 'w') as f:
           print(y)
  |}
    ~expected:
      [!&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None]];
  assert_captures
    {|
     def foo(x: int):
       y = 1
       async def bar():
         await baz(x, y)
  |}
    ~expected:
      [!&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None]];
  assert_captures
    {|
     def foo(x: int):
       y = 1
       def bar():
         return [x, y]
  |}
    ~expected:
      [!&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None]];
  assert_captures
    {|
     def foo(x: int):
       y = 1
       def bar():
         return x, y
  |}
    ~expected:
      [!&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None]];
  assert_captures
    {|
     def foo(x: int):
       y = 1
       def bar():
         return {x, y}
  |}
    ~expected:
      [!&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None]];
  assert_captures
    {|
     def foo(x: int):
       y = 1
       def bar():
         return {x: y}
  |}
    ~expected:
      [!&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None]];
  assert_captures
    {|
     def foo(x: int):
       y = 1
       def bar():
         return f"x = {x}, y = {y}"
  |}
    ~expected:
      [!&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None]];
  assert_captures
    {|
     def foo(x: int):
       y = 1
       def bar():
         @decorator(x)
         def baz():
           return y
         return 42
  |}
    ~expected:
      [
        !&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14)))];
        !&"baz", ["y", Annotation None];
      ];
  (* Lambda bounds are excluded *)
  assert_captures
    {|
     def foo(x: int):
       y = 1
       def bar():
         return (lambda x: x + y)
  |}
    ~expected:[!&"bar", ["y", Annotation None]];
  (* Comprehension bounds are excluded *)
  assert_captures
    {|
     def foo(x: int):
       y = 1
       def bar():
         return [x for x in range(y) if x < 9]
  |}
    ~expected:[!&"bar", ["y", Annotation None]];

  (* Capture *args *)
  assert_captures
    {|
     def foo( *args):
       def bar():
         return args[0]
  |}
    ~expected:[!&"bar", ["args", Annotation (Some (tuple_any_annotation (2, 9) (2, 14)))]];
  assert_captures
    {|
     def foo( *args: int):
       def bar():
         return args[0]
  |}
    ~expected:
      [
        ( !&"bar",
          ["args", Annotation (Some (tuple_int_annotation ((2, 9), (2, 14)) ((2, 16), (2, 19))))] );
      ];
  assert_captures
    {|
     def foo( *derp: int):
       def bar():
         return derp[0]
  |}
    ~expected:
      [
        ( !&"bar",
          ["derp", Annotation (Some (tuple_int_annotation ((2, 9), (2, 14)) ((2, 16), (2, 19))))] );
      ];
  assert_captures
    {|
     def foo( *args: int):
       def bar( *args: str):
         return args[0]
  |}
    ~expected:[!&"bar", []];

  (* Capture **kwargs *)
  assert_captures
    {|
     def foo( **kwargs):
       def bar():
         return kwargs["derp"]
  |}
    ~expected:[!&"bar", ["kwargs", Annotation (Some (dict_any_annotation ((2, 9), (2, 17))))]];
  assert_captures
    {|
     def foo( **kwargs: int):
       def bar():
         return kwargs["derp"]
  |}
    ~expected:
      [
        ( !&"bar",
          ["kwargs", Annotation (Some (dict_int_annotation ((2, 9), (2, 17)) ((2, 19), (2, 22))))] );
      ];
  assert_captures
    {|
     def foo( **durp):
       def bar():
         return durp["derp"]
  |}
    ~expected:[!&"bar", ["durp", Annotation (Some (dict_any_annotation ((2, 9), (2, 15))))]];
  assert_captures
    {|
     def foo( **kwargs: int):
       def bar( **kwargs: str):
         return kwargs["derp"]
  |}
    ~expected:[!&"bar", []];

  (* Capture self *)
  assert_captures
    {|
     class Foo:
       x: int
       def foo(self) -> None:
         def bar() -> int:
           return self.x
  |}
    ~expected:[!&"bar", ["self", Self !&"Foo"]];
  assert_captures
    {|
     class Foo:
       x: int
       def foo(self) -> None:
         def bar(self) -> int:
           return self.x
  |}
    ~expected:[!&"bar", []];
  assert_captures
    {|
     class Foo:
       x: int
       def foo(this) -> None:
         def bar() -> int:
           return this.x
  |}
    ~expected:[!&"bar", ["this", Self !&"Foo"]];
  assert_captures
    {|
     class Foo:
       x: int
       def foo(self) -> None:
         def bar() -> None:
           nonlocal self
           def baz() -> int:
             return self.x
  |}
    ~expected:[!&"baz", ["self", Self !&"Foo"]];
  assert_captures
    {|
     class Foo:
       x: int
       def foo(self: T) -> T:
         def bar() -> T:
           return self
  |}
    ~expected:
      [
        ( !&"bar",
          [
            ( "self",
              Annotation
                (Some
                   (Node.create
                      ~location:(location (4, 16) (4, 17))
                      (Expression.Name (Identifier "T")))) );
          ] );
      ];
  assert_captures
    {|
     class Foo:
       x: int
       @staticmethod
       def foo(self) -> None:
         def bar() -> int:
           return self.x
  |}
    ~expected:[!&"bar", ["self", Annotation None]];
  assert_captures
    {|
     class Foo:
       x: int
       class Bar:
         y: int
         def foo(self) -> None:
           def bar() -> int:
             return self.y
  |}
    ~expected:[!&"bar", ["self", Self !&"Bar"]];
  assert_captures
    {|
     class Foo:
       x: int
       def foo(self) -> None:
         class Bar:
           y: int
           def bar(self) -> None:
             def baz() -> int:
               return self.y
  |}
    ~expected:[!&"baz", ["self", Self !&"Bar"]];

  (* Capture cls *)
  assert_captures
    {|
     class Foo:
       x: int
       @classmethod
       def foo(cls) -> None:
         def bar() -> "Foo":
           return cls
  |}
    ~expected:[!&"bar", ["cls", ClassSelf !&"Foo"]];
  assert_captures
    {|
     class Foo:
       x: int
       @classmethod
       def foo(cls) -> None:
         def bar(cls):
           return cls
  |}
    ~expected:[!&"bar", []];
  assert_captures
    {|
     class Foo:
       x: int
       @classmethod
       def foo(clazz) -> None:
         def bar() -> "Foo":
           return clazz
  |}
    ~expected:[!&"bar", ["clazz", ClassSelf !&"Foo"]];
  assert_captures
    {|
     class Foo:
       x: int
       @classmethod
       def foo(cls) -> None:
         def bar() -> None:
           nonlocal cls
           def baz() -> "Foo":
             return cls
  |}
    ~expected:[!&"baz", ["cls", ClassSelf !&"Foo"]];
  assert_captures
    {|
     class Foo:
       x: int
       @classmethod
       def foo(cls: T) -> T:
         def bar() -> T:
           return cls
  |}
    ~expected:
      [
        ( !&"bar",
          [
            ( "cls",
              Annotation
                (Some
                   (Node.create
                      ~location:(location (5, 15) (5, 16))
                      (Expression.Name (Identifier "T")))) );
          ] );
      ];

  (* Capture decorators correctly *)
  assert_captures
    {|
     def foo(decorator) -> None:
       def bar() -> None:
         @decorator # This decorator should refer to the argument of foo
         def baz(decorator) -> None:
           pass
    |}
    ~expected:[!&"bar", ["decorator", Annotation None]; !&"baz", []];
  assert_captures
    {|
     def decorator(func): ...
     def foo(decorator) -> None:
       def bar() -> None:
         @decorator # This decorator should refer to the argument of foo
         def baz(decorator) -> None:
           pass
    |}
    ~expected:[!&"bar", ["decorator", Annotation None]; !&"baz", []];
  assert_captures
    {|
     def decorator(func): ...
     def foo() -> None:
       def bar() -> None:
         @decorator # This decorator should refer to the global foo
         def baz(decorator) -> None:
           pass
    |}
    ~expected:[!&"bar", []; !&"baz", []];
  assert_captures
    {|
     def foo() -> None:
       def decorator(func): ...
       def bar() -> None:
         @decorator # This decorator should refer to the foo defined above
         def baz(decorator) -> None:
           pass
    |}
    ~expected:
      [
        ( !&"bar",
          [
            ( "decorator",
              DefineSignature
                {
                  Define.Signature.name =
                    Node.create ~location:(location (3, 6) (3, 15)) !&"decorator";
                  parameters =
                    [
                      Node.create
                        { Parameter.name = "func"; value = None; annotation = None }
                        ~location:(location (3, 16) (3, 20));
                    ];
                  decorators = [];
                  return_annotation = None;
                  async = false;
                  generator = false;
                  parent = None;
                  nesting_define = None;
                } );
          ] );
        !&"baz", [];
      ];

  ()


let test_populate_unbound_names _ =
  let assert_unbound_names ~expected source_text =
    let source =
      Test.parse ~handle:"test.py" source_text
      |> Preprocessing.expand_format_string
      |> Preprocessing.populate_unbound_names
    in
    let defines =
      Preprocessing.defines
        ~include_toplevels:true
        ~include_nested:true
        ~include_methods:true
        source
    in
    let unbound_map =
      let build_unbound_map
          sofar
          { Node.value = { Define.signature = { Define.Signature.name; _ }; unbound_names; _ }; _ }
        =
        Reference.Map.set sofar ~key:(Node.value name) ~data:unbound_names
      in
      List.fold defines ~init:Reference.Map.empty ~f:build_unbound_map
    in
    let assert_unbound_names name expected =
      let expected =
        List.map expected ~f:(fun (name, location) -> { Define.NameAccess.name; location })
      in
      let actual = Reference.Map.find unbound_map name |> Option.value ~default:[] in
      assert_equal
        ~cmp:[%compare.equal: Define.NameAccess.t list]
        ~printer:(fun unbound_names ->
          Sexp.to_string_hum [%message (unbound_names : Define.NameAccess.t list)])
        expected
        actual
    in
    List.iter expected ~f:(fun (name, unbound_names) -> assert_unbound_names name unbound_names)
  in
  let toplevel_name = !&"test.$toplevel" in

  assert_unbound_names "derp" ~expected:[toplevel_name, ["derp", location (1, 0) (1, 4)]];
  assert_unbound_names
    {|
       x = 42
       y = x + z
    |}
    ~expected:[toplevel_name, ["z", location (3, 8) (3, 9)]];
  assert_unbound_names
    {|
      def foo() -> None:
        derp
    |}
    ~expected:[!&"foo", ["derp", location (3, 2) (3, 6)]];
  assert_unbound_names
    {|
      def foo(derp: int) -> None:
        derp
    |}
    ~expected:[!&"foo", []];
  assert_unbound_names
    {|
      import derp
      def foo() -> None:
        derp
    |}
    ~expected:[!&"foo", []];
  assert_unbound_names
    {|
      import chocobo.river
      def foo() -> None:
        chocobo
    |}
    ~expected:[!&"foo", []];
  assert_unbound_names
    {|
      def foo(x: int) -> None:
        bar(x)
    |}
    ~expected:[!&"foo", ["bar", location (3, 2) (3, 5)]];
  assert_unbound_names
    {|
      def bar() -> None: ...
      def foo(x: int) -> None:
        bar(x)
    |}
    ~expected:[!&"foo", []];
  assert_unbound_names
    {|
      from some_module import bar
      def foo() -> None:
        bar(x=1, y=z)
    |}
    ~expected:[!&"foo", ["z", location (4, 13) (4, 14)]];
  assert_unbound_names
    {|
      def foo():
        f = (lambda x: x)
        g = (lambda *y: y)
        h = (lambda **z: z)
        return f or g or h
    |}
    ~expected:[!&"foo", []];
  assert_unbound_names
    {|
      def foo():
        f = (lambda x: x + y)
        return f
    |}
    ~expected:[!&"foo", ["y", location (3, 21) (3, 22)]];
  assert_unbound_names
    {|
      def foo():
        return [(x, y, z) for x, *y in [[1], [2,3]]]
    |}
    ~expected:[!&"foo", ["z", location (3, 17) (3, 18)]];
  assert_unbound_names
    {|
       class A:
         def foo() -> None:
           self.bar()
    |}
    ~expected:[!&"foo", ["self", location (4, 4) (4, 8)]];
  assert_unbound_names
    {|
       class A:
         def foo(self) -> None:
           self.bar()
    |}
    ~expected:[!&"foo", []];
  assert_unbound_names
    {|
       class A:
         @staticmethod
         def foo() -> None:
           A.bar()
    |}
    ~expected:[!&"foo", []];
  assert_unbound_names
    {|
      def foo() -> None:
        def bar(x) -> int:
          return x + baz
        x = bar(qux)
        y = bar(x)
    |}
    ~expected:
      [!&"foo", ["qux", location (5, 10) (5, 13)]; !&"bar", ["baz", location (4, 15) (4, 18)]];
  assert_unbound_names
    {|
      def foo(x: int):
        reveal_type(x)
      pyre_dump()
    |}
    ~expected:[!&"foo", []; toplevel_name, []];
  assert_unbound_names
    {|
      def foo():
        y = int("42")
        return [x for x in range(y)]
    |}
    ~expected:[!&"foo", []];
  assert_unbound_names
    {|
      def foo():
        return [y for x in range(42) for y in x]
    |}
    ~expected:[!&"foo", []];
  assert_unbound_names
    {|
      def bar() -> None: ...
      def foo():
        try:
          bar()
        except Derp:
          pass
        except ValueError:
          pass
    |}
    ~expected:[!&"foo", ["Derp", location (6, 9) (6, 13)]];
  assert_unbound_names
    {|
      def foo() -> Derp:
        pass
    |}
    ~expected:[toplevel_name, ["Derp", location (2, 13) (2, 17)]];
  assert_unbound_names
    {|
      def foo(d: Derp) -> None:
        pass
    |}
    ~expected:[toplevel_name, ["Derp", location (2, 11) (2, 15)]];
  assert_unbound_names
    {|
      def foo(d: int = derp()) -> None:
        pass
    |}
    ~expected:[toplevel_name, ["derp", location (2, 17) (2, 21)]];
  assert_unbound_names
    {|
      from some_module import derp
      def foo() -> None:
        x: Derp = derp()
    |}
    ~expected:[!&"foo", ["Derp", location (4, 5) (4, 9)]];
  assert_unbound_names
    {|
      def foo() -> None:
        @derp
        def bar() -> None:
          pass
    |}
    ~expected:[!&"foo", ["derp", location (3, 3) (3, 7)]];

  let class_foo_toplevel_name = !&"Foo.$class_toplevel" in
  assert_unbound_names
    {|
      class Foo(Derp):
        def foo(self) -> None:
          pass
    |}
    ~expected:[toplevel_name, ["Derp", location (2, 10) (2, 14)]];
  assert_unbound_names
    {|
      class Foo:
        class Baz(Bar):
          pass
    |}
    ~expected:[class_foo_toplevel_name, ["Bar", location (3, 12) (3, 15)]];
  assert_unbound_names
    {|
      class Foo:
        class Bar:
          pass
        class Baz(Bar):
          pass
    |}
    ~expected:[class_foo_toplevel_name, []];
  assert_unbound_names
    {|
      class Foo:
        @derp
        def foo(self) -> None:
          pass
    |}
    ~expected:[class_foo_toplevel_name, ["derp", location (3, 3) (3, 7)]];
  assert_unbound_names
    {|
      class Foo:
        @property
        def foo(self) -> int:
          return 42
        @foo.setter
        def foo(self, value: int) -> None:
          pass
    |}
    ~expected:[class_foo_toplevel_name, []];

  (* Test that the pass does not nuke Python2 sources. *)
  assert_unbound_names
    {|
      #!/usr/bin/env python2
      def foo() -> None:
        derp
    |}
    ~expected:[!&"foo", ["derp", location (4, 2) (4, 6)]];
  ()


let () =
  "preprocessing"
  >::: [
         "expand_relative_imports" >:: test_expand_relative_imports;
         "expand_string_annotations" >:: test_expand_string_annotations;
         "expand_format_string" >:: test_expand_format_string;
         "qualify" >:: test_qualify;
         "replace_version_specific_code" >:: test_replace_version_specific_code;
         "replace_platform_specific_code" >:: test_replace_platform_specific_code;
         "expand_type_checking_imports" >:: test_expand_type_checking_imports;
         "expand_wildcard_imports" >:: test_expand_wildcard_imports;
         "expand_implicit_returns" >:: test_expand_implicit_returns;
         "defines" >:: test_defines;
         "classes" >:: test_classes;
         "transform_ast" >:: test_transform_ast;
         "typed_dictionary_stub_fix" >:: test_replace_mypy_extensions_stub;
         "typed_dictionaries" >:: test_expand_typed_dictionaries;
         "sqlalchemy_declarative_base" >:: test_sqlalchemy_declarative_base;
         "nesting_define" >:: test_populate_nesting_define;
         "captures" >:: test_populate_captures;
         "unbound_names" >:: test_populate_unbound_names;
       ]
  |> Test.run
