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
  assert_expand
    "def foo(f: '1234'): ..."
    "def foo(f: $unparsed_annotation): ...";
  assert_expand
    {|
      class Foo: ...
      def foo(f: 'Foo[K, V]'): ...
    |}
    {|
      class Foo: ...
      def foo(f: Foo[K, V]): ...
    |}


let test_expand_format_string _ =
  let assert_ast_equal source expected_value expected_expression_list =
    assert_source_equal
      (Preprocessing.expand_format_string (parse_untrimmed source))
      (Source.create ~path:"test.py"
         [
           +Expression
             (+FormatString {
                FormatString.value = expected_value;
                expression_list = expected_expression_list;
              });
         ];)
  in

  assert_ast_equal "f'foo'" "foo" [];

  assert_ast_equal "f'{1}'" "{1}" [+Integer 1];

  assert_ast_equal "f'foo{1}'" "foo{1}" [+Integer 1];

  assert_ast_equal "f'foo{1}{2}foo'" "foo{1}{2}foo" [+Integer 1; +Integer 2];

  assert_ast_equal "f'foo{{1}}'" "foo{{1}}" [+Integer 1];

  assert_ast_equal
    "f'foo{1+2}'"
    "foo{1+2}"
    [
      +Access [
        Access.Expression (+Integer 1);
        Access.Identifier ~~"__add__";
        Access.Call (+[{ Argument.name = None; value = +Integer 2 }]);
      ]
    ]


let test_qualify _ =
  let assert_qualify ?(path = "qualifier.py") source expected =
    let parse = parse ~qualifier:(Source.qualifier ~path) ~path in
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
    "\nfor b in b:\n\tb\nelse:\n\tb"
    "\nfor $target$b in a:\n\t$target$b\nelse:\n\t$target$b";
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
    ~path:"typing.pyi"
    {|
      Type: _SpecialForm = ...
      def foo(l: Type[int]): ...
    |}
    {|
      Type: _SpecialForm = ...
      def typing.foo($parameter$l: typing.Type[int]): ...
    |};


  (* Qualify strings. *)
  assert_qualify
    {|
      from typing import List
      T = 'List'
      def foo() -> 'List[int]': ...
    |}
    {|
      from typing import List
      $local_qualifier$T = 'typing.List'
      def qualifier.foo() -> 'typing.List.__getitem__(int)': ...
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

  assert_qualify "def foo(): ..." "def qualifier.foo(): ...";
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

  (* Nested defines. *)
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
        def qualifier.foo.nestedA():
          qualifier.foo.nestedB()
        def qualifier.foo.nestedB():
          qualifier.foo.nestedA()
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
  let assert_expand_implicit_returns source expected_body =
    assert_source_equal
      (Preprocessing.expand_returns (parse source))
      (Source.create ~path:"test.py"
         [
           +Define {
             Define.name = Access.create "foo";
             parameters = [];
             body = expected_body;
             decorators = [];
             docstring = None;
             return_annotation = None;
             async = false;
             generated = false;
             parent = None;
           };
         ];)
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
  assert_expand_implicit_returns
    {|
      def foo():
        pass
    |}
    [
      +Pass;
      +Return {
        Return.expression = None;
        is_implicit = true
      }
    ];

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
      +Try {
        Try.body = [+Pass];
        handlers = [];
        orelse = [];
        finally = [+Pass];
      };
      +Return {
        Return.expression = None;
        is_implicit = true
      }
    ];
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
  assert_expand_implicit_returns
    {|
      def foo():
        while derp:
          pass
    |}
    [
      +While {
        While.test = !"derp";
        body = [+Pass];
        orelse = [];
      };
      +Return {
        Return.expression = None;
        is_implicit = true
      }
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


let () =
  "preprocessing">:::[
    "expand_string_annotations">::test_expand_string_annotations;
    "expand_format_string">::test_expand_format_string;
    "qualify">::test_qualify;
    "replace_version_specific_code">::test_replace_version_specific_code;
    "expand_type_checking_imports">::test_expand_type_checking_imports;
    "expand_returns">::test_expand_returns;
    "expand_ternary_assigns">::test_expand_ternary;
    "defines">::test_defines;
    "classes">::test_classes;
  ]
  |> run_test_tt_main
