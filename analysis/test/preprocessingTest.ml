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
    assert_source_equal (parse expected) (Preprocessing.expand_relative_imports (parse source))
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
      from module.submodule import a
      from module.submodule.relative import b
      from module import c
      from module.relative import d
    |}


let test_expand_string_annotations _ =
  let assert_expand ?(handle = "qualifier.py") source expected =
    let parse = parse ~handle in
    assert_source_equal (parse expected) (Preprocessing.expand_string_annotations (parse source))
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
      (Source.create
         ~relative:handle
         [+Expression (+String (StringLiteral.create ~expressions value))])
      (Preprocessing.expand_format_string (parse_untrimmed ~handle source))
  in
  assert_format_string "f'foo'" "foo" [];
  assert_format_string "f'{1}'" "{1}" [+Integer 1];
  assert_format_string "f'foo{1}'" "foo{1}" [+Integer 1];
  assert_format_string "f'foo{1}' 'foo{2}'" "foo{1}foo{2}" [+Integer 1];
  assert_format_string "'foo{1}' f'foo{2}'" "foo{1}foo{2}" [+Integer 2];
  assert_format_string "f'foo{1}' f'foo{2}'" "foo{1}foo{2}" [+Integer 1; +Integer 2];
  assert_format_string "f'foo{1}{2}foo'" "foo{1}{2}foo" [+Integer 1; +Integer 2];
  assert_format_string "f'foo{{1}}'" "foo{{1}}" [];
  assert_format_string "f'foo{{ {1} }}'" "foo{{ {1} }}" [+Integer 1];
  assert_format_string "f'foo{{{1} }}'" "foo{{{1} }}" [+Integer 1];
  assert_format_string "f'foo{{ {1}}}'" "foo{{ {1}}}" [+Integer 1];
  assert_format_string "f'foo{{{1}}}'" "foo{{{1}}}" [+Integer 1];
  assert_format_string "f'foo{{'" "foo{{" [];
  assert_format_string "f'foo}}'" "foo}}" [];
  assert_format_string
    "f'foo{1+2}'"
    "foo{1+2}"
    [ +Call
         {
           callee =
             +Name (Name.Attribute { base = +Integer 1; attribute = "__add__"; special = true });
           arguments = [{ Call.Argument.name = None; value = +Integer 2 }];
         } ];

  (* Ensure we fix up locations. *)
  let assert_locations source statements =
    let parsed_source = parse source |> Preprocessing.expand_format_string in
    let expected_source = { parsed_source with Source.statements } in
    assert_source_equal_with_locations expected_source parsed_source
  in
  assert_locations
    "f'foo{1}'"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(1, 9)
            (String
               {
                 StringLiteral.kind =
                   StringLiteral.Format [node ~start:(1, 6) ~stop:(1, 7) (Integer 1)];
                 value = "foo{1}";
               })) ];
  assert_locations
    "f'foo{123}a{456}'"
    [ +Expression
         (node
            ~start:(1, 0)
            ~stop:(1, 17)
            (String
               {
                 StringLiteral.kind =
                   StringLiteral.Format
                     [ node ~start:(1, 6) ~stop:(1, 9) (Integer 123);
                       node ~start:(1, 12) ~stop:(1, 15) (Integer 456) ];
                 value = "foo{123}a{456}";
               })) ];
  assert_locations
    "return f'foo{123}a{456}'"
    [ +Return
         {
           is_implicit = false;
           expression =
             Some
               (node
                  ~start:(1, 7)
                  ~stop:(1, 24)
                  (String
                     {
                       StringLiteral.kind =
                         StringLiteral.Format
                           [ node ~start:(1, 13) ~stop:(1, 16) (Integer 123);
                             node ~start:(1, 19) ~stop:(1, 22) (Integer 456) ];
                       value = "foo{123}a{456}";
                     }));
         } ];
  assert_locations
    {|
       f'''
       foo{123}a{456}
       b{789}
       '''
     |}
    [ +Expression
         (node
            ~start:(2, 0)
            ~stop:(5, 3)
            (String
               {
                 StringLiteral.kind =
                   StringLiteral.Format
                     [ node ~start:(3, 4) ~stop:(3, 7) (Integer 123);
                       node ~start:(3, 10) ~stop:(3, 13) (Integer 456);
                       node ~start:(4, 2) ~stop:(4, 5) (Integer 789) ];
                 value = "\nfoo{123}a{456}\nb{789}\n";
               })) ]


let test_qualify _ =
  let assert_qualify ?(handle = "qualifier.py") source expected =
    let parse = parse ~handle in
    assert_source_equal (parse expected) (Preprocessing.qualify (parse source))
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
        def abc.bar(): pass
        abc.bar()
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
      @(qualifier.mydecoratorwrapper($local_qualifier$x))
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
        @(qualifier.mydecoratorwrapper(qualifier.A.x))
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
        @(qualifier.mydecoratorwrapper($local_qualifier$x + qualifier.A.y))
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
    let _, ast_environment =
      ScratchProject.setup ~context ~external_sources ["test.py", check_source]
      |> ScratchProject.parse_sources
    in
    assert_equal
      ~cmp:(List.equal Statement.equal)
      ~printer:(fun statement_list ->
        List.map statement_list ~f:Statement.show |> String.concat ~sep:", ")
      (Source.statements (parse expected))
      (Source.statements (Option.value_exn (AstEnvironment.get_source ast_environment !&"test")))
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
      from a import foo
    |};
  assert_expanded
    ["a.py", "def foo(): pass"; "b.py", "def bar(): pass"]
    {|
      from a import *
      from b import *
    |}
    {|
      from a import foo
      from b import bar
    |};
  assert_expanded
    [ ( "a.py",
        {|
        from x import y
        def foo(): pass
        def bar(): pass
        def _private(): pass
      |}
      ) ]
    {|
      from a import *
    |}
    {|
      from a import bar, foo, y
    |};
  assert_expanded
    [ ( "a.py",
        {|
        from x import y
        def foo(): pass
        def bar(): pass
        __all__ = ["bar"]
      |}
      ) ]
    {|
      from a import *
    |}
    {|
      from a import bar
    |}


let test_expand_implicit_returns _ =
  let assert_expand source expected =
    assert_source_equal (parse expected) (Preprocessing.expand_implicit_returns (parse source))
  in
  let assert_expand_implicit_returns source expected_body =
    let handle = "test.py" in
    assert_source_equal
      (Preprocessing.expand_implicit_returns (parse ~handle source))
      (Source.create
         ~relative:handle
         [ +Define
              {
                signature =
                  {
                    name = !&"foo";
                    parameters = [];
                    decorators = [];
                    docstring = None;
                    return_annotation = None;
                    async = false;
                    parent = None;
                  };
                body = expected_body;
              } ])
  in
  assert_expand_implicit_returns
    {|
      def foo():
        pass
    |}
    [+Pass; +Return { Return.expression = None; is_implicit = true }];
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
    [ +Try { Try.body = [+Pass]; handlers = []; orelse = []; finally = [+Pass] };
      +Return { Return.expression = None; is_implicit = true } ];

  (* Lol termination analysis. *)
  assert_expand_implicit_returns
    {|
      def foo():
        while derp:
          pass
    |}
    [ +While { While.test = +Name (Name.Identifier "derp"); body = [+Pass]; orelse = [] };
      +Return { Return.expression = None; is_implicit = true } ];
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
      | return :: _ -> assert_equal return.location expected_location
      | _ -> failwith "Preprocessed source's Define body is empty" )
    | _ -> failwith "Preprocessed source failed"
  in
  assert_implicit_return_location
    {|
       def foo() -> int:
         pass
     |}
    {
      Location.path = !&"test";
      start = { Location.line = 3; Location.column = 2 };
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
      Location.path = !&"test";
      start = { Location.line = 10; Location.column = 6 };
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
      Location.path = !&"test";
      start = { Location.line = 9; Location.column = 4 };
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
      Location.path = !&"test";
      start = { Location.line = 13; Location.column = 8 };
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
      Location.path = !&"test";
      start = { Location.line = 11; Location.column = 6 };
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
      ( Preprocessing.defines
          ~include_stubs:true
          ~include_nested:true
          ~include_toplevels:true
          source
      |> List.length )
      (Preprocessing.count_defines source)
  in
  let create_define name =
    {
      Define.signature =
        {
          name = !&name;
          parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
          decorators = [];
          docstring = None;
          return_annotation = None;
          async = false;
          parent = None;
        };
      body = [+Expression (+Float 1.0)];
    }
  in
  let create_toplevel body =
    {
      Define.signature =
        {
          name = !&"$toplevel";
          parameters = [];
          decorators = [];
          docstring = None;
          return_annotation = None;
          async = false;
          parent = None;
        };
      body;
    }
  in
  let create_class_toplevel ~parent ~body =
    {
      Define.signature =
        {
          name = !&(parent ^ ".$class_toplevel");
          parameters = [];
          decorators = [];
          docstring = None;
          return_annotation = None;
          async = false;
          parent = Some (Reference.create parent);
        };
      body;
    }
  in
  let define = create_define "foo" in
  assert_defines [+Define define] [create_toplevel [+Define define]; define];
  let inner =
    {
      Define.signature =
        {
          name = !&"foo";
          parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
          decorators = [];
          docstring = None;
          return_annotation = None;
          async = false;
          parent = None;
        };
      body = [+Expression (+Float 1.0)];
    }
  in
  let define =
    {
      Define.signature =
        {
          name = !&"foo";
          parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
          decorators = [];
          docstring = None;
          return_annotation = None;
          async = false;
          parent = None;
        };
      body = [+Expression (+Float 1.0); +Define inner];
    }
  in
  assert_defines [+Define define] [create_toplevel [+Define define]; define];
  let if_define = { If.test = +Ellipsis; body = [+Define define]; orelse = [] } in
  assert_defines [+If if_define] [create_toplevel [+If if_define]];

  (* Note: Defines are returned in reverse order. *)
  let define_foo = create_define "foo" in
  let define_bar = create_define "bar" in
  let body = [+Define define_foo; +Define define_bar] in
  let parent =
    { Statement.Class.name = !&"Foo"; bases = []; body; decorators = []; docstring = None }
  in
  assert_defines
    [+Class parent]
    [ create_toplevel [+Class parent];
      create_class_toplevel ~parent:"Foo" ~body;
      define_bar;
      define_foo ]


let test_classes _ =
  let assert_classes statements class_defines =
    assert_equal
      ~cmp:(List.equal Class.equal)
      (Preprocessing.classes (Source.create statements) |> List.map ~f:Node.value)
      class_defines
  in
  let class_define =
    {
      Class.name = !&"foo";
      bases = [];
      body =
        [ +Define
             {
               signature =
                 {
                   name = !&"bar";
                   parameters = [];
                   decorators = [];
                   docstring = None;
                   return_annotation = None;
                   async = false;
                   parent = Some !&"foo";
                 };
               body = [+Pass];
             } ];
      decorators = [];
      docstring = None;
    }
  in
  assert_classes [+Class class_define] [class_define];
  let inner =
    { Class.name = !&"bar"; bases = []; body = [+Pass]; decorators = []; docstring = None }
  in
  let class_define =
    { Class.name = !&"foo"; bases = []; body = [+Class inner]; decorators = []; docstring = None }
  in
  assert_classes [+Class class_define] [class_define; inner]


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
  assert_source_equal expected (Preprocessing.replace_mypy_extensions_stub given)


let test_expand_typed_dictionaries _ =
  let assert_expand ?(handle = "") source expected =
    let expected = parse ~handle ~coerce_special_methods:true expected |> Preprocessing.qualify in
    let actual =
      parse ~handle source
      |> Preprocessing.qualify
      |> Preprocessing.expand_typed_dictionary_declarations
    in
    assert_source_equal expected actual
  in
  assert_expand
    {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
    |}
    ( "Movie: "
    ^ "typing.Type[mypy_extensions.TypedDict[('Movie', True, ('name', str), ('year', int))]] = "
    ^ "mypy_extensions.TypedDict[('Movie', True, ('name', str), ('year', int))]" );
  assert_expand
    {|
      Movie = mypy_extensions.TypedDict('Movie', {})
    |}
    {|
      Movie: typing.Type[mypy_extensions.TypedDict[('Movie', True)]] = (
        mypy_extensions.TypedDict[('Movie', True)])
    |};
  assert_expand
    {|
      Movie = mypy_extensions.TypedDict('Movie', {}, total=False)
    |}
    {|
      Movie: typing.Type[mypy_extensions.TypedDict[('Movie', False)]] = (
        mypy_extensions.TypedDict[('Movie', False)])
    |};
  assert_expand
    {|
      Movie = mypy_extensions.TypedDict('Movie', {}, total=True)
    |}
    {|
      Movie: typing.Type[mypy_extensions.TypedDict[('Movie', True)]] = (
        mypy_extensions.TypedDict[('Movie', True)])
    |};
  assert_expand
    {|
      class Movie(mypy_extensions.TypedDict):
        name: str
        year: int
    |}
    ( "Movie: "
    ^ "typing.Type[mypy_extensions.TypedDict[('Movie', True, ('name', str), ('year', int))]] = "
    ^ "mypy_extensions.TypedDict[('Movie', True, ('name', str), ('year', int))]" );
  assert_expand
    {|
      class Movie(mypy_extensions.TypedDict):
        name: str
        year: int
    |}
    ( "Movie: "
    ^ "typing.Type[mypy_extensions.TypedDict[('Movie', True, ('name', str), ('year', int))]] = "
    ^ "mypy_extensions.TypedDict[('Movie', True, ('name', str), ('year', int))]" )
    ~handle:"foo/bar.py";
  assert_expand
    {|
      class Movie(mypy_extensions.TypedDict, total=False):
        name: str
        year: int
    |}
    ( "Movie: "
    ^ "typing.Type[mypy_extensions.TypedDict[('Movie', False, ('name', str), ('year', int))]] = "
    ^ "mypy_extensions.TypedDict[('Movie', False, ('name', str), ('year', int))]" );
  assert_expand
    {|
      class Movie(mypy_extensions.TypedDict, total=True):
        name: str
        year: int
    |}
    ( "Movie: "
    ^ "typing.Type[mypy_extensions.TypedDict[('Movie', True, ('name', str), ('year', int))]] = "
    ^ "mypy_extensions.TypedDict[('Movie', True, ('name', str), ('year', int))]" );
  assert_expand
    {|
      class Movie(mypy_extensions.TypedDict, total=True):
        """docstring"""
        name: str
        year: int
    |}
    ( "Movie: "
    ^ "typing.Type[mypy_extensions.TypedDict[('Movie', True, ('name', str), ('year', int))]] = "
    ^ "mypy_extensions.TypedDict[('Movie', True, ('name', str), ('year', int))]" );

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
    ( "Movie: "
    ^ "typing.Type[mypy_extensions.TypedDict[('Movie', True, ('name', str), ('year', int))]] = "
    ^ "mypy_extensions.TypedDict[('Movie', True, ('name', str), ('year', int))]" );
  assert_expand
    {|
      class Movie(mypy_extensions.TypedDict, total=True, total=False):
        name: str
        year: int
    |}
    ( "Movie: "
    ^ "typing.Type[mypy_extensions.TypedDict[('Movie', True, ('name', str), ('year', int))]] = "
    ^ "mypy_extensions.TypedDict[('Movie', True, ('name', str), ('year', int))]" );
  assert_expand
    {|
      class Movie(mypy_extensions.TypedDict, garbage=7):
        name: str
        year: int
    |}
    ( "Movie: "
    ^ "typing.Type[mypy_extensions.TypedDict[('Movie', True, ('name', str), ('year', int))]] = "
    ^ "mypy_extensions.TypedDict[('Movie', True, ('name', str), ('year', int))]" );
  assert_expand
    {|
      class Movie(mypy_extensions.TypedDict, OtherClass):
        name: str
        year: int
    |}
    ( "Movie: "
    ^ "typing.Type[mypy_extensions.TypedDict[('Movie', True, ('name', str), ('year', int))]] = "
    ^ "mypy_extensions.TypedDict[('Movie', True, ('name', str), ('year', int))]" );
  ()


let () =
  "preprocessing"
  >::: [ "expand_string_annotations" >:: test_expand_string_annotations;
         "expand_format_string" >:: test_expand_format_string;
         "qualify" >:: test_qualify;
         "replace_version_specific_code" >:: test_replace_version_specific_code;
         "replace_platform_specific_code" >:: test_replace_platform_specific_code;
         "expand_type_checking_imports" >:: test_expand_type_checking_imports;
         "expand_wildcard_imports" >:: test_expand_wildcard_imports;
         "expand_implicit_returns" >:: test_expand_implicit_returns;
         "defines" >:: test_defines;
         "classes" >:: test_classes;
         "typed_dictionary_stub_fix" >:: test_replace_mypy_extensions_stub;
         "typed_dictionaries" >:: test_expand_typed_dictionaries ]
  |> Test.run
