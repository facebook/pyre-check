(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Analysis
open Expression
open Statement
open Test

let location (start_line, start_column) (stop_line, stop_column) =
  {
    Location.start = { Location.line = start_line; Location.column = start_column };
    stop = { Location.line = stop_line; Location.column = stop_column };
  }


(* This function is here for backward compatibility reason only. Please avoid using it and prefer
   `Test.parse` instead. *)
let legacy_parse ~handle source =
  let lines = String.split (Test.trim_extra_indentation source) ~on:'\n' in
  match PyreMenhirParser.Parser.parse ~relative:handle lines with
  | Result.Ok statements ->
      let typecheck_flags =
        let qualifier = ModulePath.qualifier_from_relative_path handle in
        Source.TypecheckFlags.parse ~qualifier lines
      in
      Source.create ~typecheck_flags ~relative:handle statements
  | Result.Error
      {
        PyreMenhirParser.Parser.Error.location = { Location.start = { Location.line; column }; _ };
        _;
      } ->
      let error =
        Format.asprintf
          "Could not parse test source at line %d, column %d. Test input:\n%s"
          line
          column
          source
      in
      failwith error


let test_expand_relative_imports =
  let assert_expand ~handle source expected _ =
    let parse = parse ~handle in
    assert_source_equal
      ~location_insensitive:true
      (parse expected)
      (Preprocessing.expand_relative_imports (parse source))
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
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
    |};
    ]


let test_expand_string_annotations =
  let assert_expand ?(handle = "qualifier.py") source expected _ =
    let parse = parse ~handle in
    assert_source_equal
      ~location_insensitive:true
      (parse expected)
      (Preprocessing.expand_string_annotations ~preserve_original_location:true (parse source))
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand "def foo(f: '1234'): ..." "def foo(f: \"1234\"): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      class Foo: ...
      def foo(f: 'Foo[K, V]'): ...
    |}
           {|
      class Foo: ...
      def foo(f: Foo[K, V]): ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand "x = typing.cast('1234', 42)" "x = typing.cast(\"1234\", 42)";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           "x = pyre_extensions.safe_cast('1234', 42)"
           "x = pyre_extensions.safe_cast(\"1234\", 42)";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
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
      (* `expand_string_annotations` is called after `qualify`. So, we should exempt only the
         qualified `Literal` types. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           "def foo(f: typing_extensions.Literal['A']): ..."
           "def foo(f: typing_extensions.Literal['A']): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           "def foo(f: typing_extensions.Literal['A', 'B']): ..."
           "def foo(f: typing_extensions.Literal['A', 'B']): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand "def foo(f: Literal['A']): ..." "def foo(f: Literal[A]): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand "def foo(f: te.Literal['A', 'B']): ..." "def foo(f: te.Literal[A, B]): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand "class Foo(typing.List['str']): ..." "class Foo(typing.List[str]): ...";
      (* assert_expand "class Foo('str'): ..." "class Foo('str'): ..."; *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      def foo(
        x: "int",
      ):
        return x
    |}
           {|
      def foo(x: int):
        return x
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      def foo(
        x: "int,"
      ):
        return x
    |}
           {|
      def foo(x: "int,"):
        return x
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      valid_string_literal: typing.Annotated["int", test.Foo["hello"]]
    |}
           {|
      valid_string_literal: typing.Annotated[int, test.Foo[hello]]
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      valid_string_literal: typing.Annotated["int", test.Foo("hello")]
    |}
           {|
      valid_string_literal: typing.Annotated[int, test.Foo("hello")]
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      def foo() -> Parametric["a"]: ...
    |}
           {|
      def foo() -> Parametric[a]: ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      def foo() -> TakesParamSpec[["a"]]: ...
    |}
           {|
      def foo() -> TakesParamSpec[[a]]: ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      def foo() -> TakesParamSpec[["a", "b"]]: ...
    |}
           {|
      def foo() -> TakesParamSpec[[a, b]]: ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      X: typing.TypeAlias = "Dict[int, X] | int"
    |}
           {|
      X: typing.TypeAlias = Dict[int, X] | int
    |};
      (* Ensure init subclass arguments are not counted as annotations to be expanded *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      class Foo("BaseClass"):
        pass

      class Bar(metaclass="BaseClass"):
        pass

      class Baz(init_subclass_arg="BaseClass"):
        pass
    |}
           {|
      class Foo(BaseClass):
        pass

      class Bar(metaclass=BaseClass):
        pass

      class Baz(init_subclass_arg="BaseClass"):
        pass
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      class Foo("BaseClass", BaseClass2, metaclass="BaseClass3", arbitrary="BaseClass4"):
        pass
    |}
           {|
      class Foo(BaseClass, BaseClass2, metaclass=BaseClass3, arbitrary="BaseClass4"):
        pass
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      T = typing.TypeVar("T", bound='typing.List[Any]')
    |}
           {|
      T = typing.TypeVar("T", bound=typing.List[Any])
    |};
    ]


let test_expand_type_alias_body =
  let assert_expand expression expected _ =
    assert_equal
      ~printer:Expression.show
      ~cmp:(fun left right -> Expression.location_insensitive_compare left right = 0)
      (parse_single_expression expected)
      (Preprocessing.expand_strings_in_annotation_expression
         ~preserve_original_location:true
         (parse_single_expression expression))
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand {|
      SomeGeneric["A", int]
    |} {|
      SomeGeneric[A, int]
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      typing_extensions.Literal["A"]
    |}
           {|
      typing_extensions.Literal["A"]
    |};
      (* Type variable definitions are treated as aliases. We should not unquote the "T". *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand {|
      typing.TypeVar("T")
    |} {|
      typing.TypeVar("T")
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      typing_extensions.IntVar("N")
    |}
           {|
      typing_extensions.IntVar("N")
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      typing.TypeVar("T", bound=Union[int, "A"])
    |}
           {|
      typing.TypeVar("T", bound=Union[int, A])
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      typing.TypeVar("T", int, "A")
    |}
           {|
      typing.TypeVar("T", int, A)
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      typing.TypeVar("T", bound='Union[Any, Any]')
    |}
           {|
      typing.TypeVar("T", bound=Union[Any, Any])
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      typing.TypeVar("T", bound=Union[Any, 'Any'])
    |}
           {|
      typing.TypeVar("T", bound=Union[Any, Any])
    |};
    ]


let test_expand_string_annotation_preserves_locations =
  test_list
    [
      (labeled_test_case __FUNCTION__ __LINE__
      @@ fun _ ->
      assert_equal
        ~printer:(fun expression -> [%sexp_of: Expression.t] expression |> Sexp.to_string_hum)
        ~cmp:[%compare.equal: Expression.t]
        {
          Node.location = location (1, 5) (1, 10);
          value =
            Name
              (Attribute
                 {
                   base =
                     { Node.location = location (1, 5) (1, 10); value = Name (Identifier "test") };
                   attribute = "Foo";
                   origin = None;
                 });
        }
        (Preprocessing.expand_strings_in_annotation_expression
           ~preserve_original_location:true
           {
             Node.location = location (1, 5) (1, 10);
             value =
               Constant
                 (Constant.String { StringLiteral.value = "test.Foo"; kind = StringLiteral.String });
           }));
    ]


let test_qualify_source =
  (* Expected sources may make parent info qualified which we don't care about in this test. So,
     strip them. *)
  let dequalify_parent source =
    let module SanitizeDefines = Transform.MakeStatementTransformer (struct
      type t = unit

      let rec dequalify_parent = function
        | NestingContext.TopLevel as top_level -> top_level
        | NestingContext.Function { name; parent } ->
            NestingContext.create_function
              ~parent:(dequalify_parent parent)
              (Identifier.sanitized name |> Reference.create |> Reference.last)
        | NestingContext.Class { name; parent } ->
            NestingContext.create_class
              ~parent:(dequalify_parent parent)
              (Identifier.sanitized name |> Reference.create |> Reference.last)


      let statement _ = function
        | { Node.value = Statement.Class ({ Class.parent; _ } as class_body); location } ->
            ( (),
              [
                {
                  Node.value =
                    Statement.Class { class_body with Class.parent = dequalify_parent parent };
                  location;
                };
              ] )
        | {
            Node.value =
              Statement.Define
                ({ Define.signature = { Define.Signature.parent; _ } as signature; _ } as define);
            location;
          } ->
            ( (),
              [
                {
                  Node.value =
                    Statement.Define
                      {
                        define with
                        Define.signature =
                          { signature with Define.Signature.parent = dequalify_parent parent };
                      };
                  location;
                };
              ] )
        | statement -> (), [statement]
    end)
    in
    let { SanitizeDefines.source; _ } = SanitizeDefines.transform () source in
    source
  in
  let assert_qualify ?(handle = "qualifier.py") source expected _ =
    let parsed = parse ~handle source in
    let processed = Preprocessing.qualify parsed in
    let expected = legacy_parse ~handle expected |> dequalify_parent in
    assert_source_equal ~location_insensitive:true expected processed;
    (* Qualifying twice should not change the source. *)
    assert_source_equal ~location_insensitive:true expected (Preprocessing.qualify processed)
  in
  let assert_qualify_statement actual expected context =
    let import = "import a as b;" in
    assert_qualify (import ^ actual) (import ^ expected) context
  in
  test_list
    [
      (* Base cases for aliasing. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify "from a import b; b" "from a import b; a.b";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify "from a import b, c; b, c" "from a import b, c; a.b, a.c";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify "from a import b; b.c" "from a import b; a.b.c";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify "from a import b; b()" "from a import b; a.b()";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify "from a import b as c; c" "from a import b as c; a.b";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify "from builtins import b; b" "from builtins import b; b";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify "b; import a as b; b" "a; import a as b; a";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify "import builtins; builtins.b" "import builtins; b";
      (* Qualification in different places. *)
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_qualify_statement "call(b)" "call(a)";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_qualify_statement "List[b]" "List[a]";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_qualify_statement "await b" "await a";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_statement "(await b).c" "(await a).c";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_qualify_statement "b or True" "a or True";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_qualify_statement "True and b" "True and a";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_qualify_statement "1 < b <= b" "1 < a <= a";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_qualify_statement "{ b: b }" "{ a: a }";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_statement
           "{ a: b for a, b in b }"
           "{ $target$a: $target$b for $target$a, $target$b in a }";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_statement
           "{ b: b for b in b if b }"
           "{ $target$b: $target$b for $target$b in a if $target$b }";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_statement "lambda b: b" "lambda $parameter$b: $parameter$b";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_statement "lambda a: a + b" "lambda $parameter$a: $parameter$a + a";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_qualify_statement "[b, b]" "[a, a]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_statement "[b for b in b]" "[$target$b for $target$b in a]";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_qualify_statement "{b, b}" "{a, a}";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_statement "{b for b in b}" "{$target$b for $target$b in a}";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_qualify_statement "*b" "*a";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_statement "b if b else b" "a if a else a";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_qualify_statement "(b, b)" "(a, a)";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_qualify_statement "-b" "-a";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_qualify_statement "assert b" "assert a";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_qualify_statement "del b" "del a";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_statement
           "b = 1\nfor b in []: pass"
           "$local_qualifier$b = 1\nfor $local_qualifier$b in []: pass";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_statement
           "b = 1\n[b for b in []]"
           "$local_qualifier$b = 1\n[$target$b for $target$b in []]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_statement
           "b = 1\n[b for a in []]"
           "$local_qualifier$b = 1\n[$local_qualifier$b for $target$a in []]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_statement
           "b = 1\n[b for b in []]"
           "$local_qualifier$b = 1\n[$target$b for $target$b in []]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_statement "\nif b:\n\tb\nelse:\n\tb" "\nif a:\n\ta\nelse:\n\ta";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_qualify_statement "raise b" "raise a";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_qualify_statement "return b" "return a";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_statement
           "\ntry:\n\tb\nexcept b as b:\n\tb\nelse:\n\tb\nfinally:\n\tb"
           "\n\
            try:\n\
            \t$local_qualifier$b\n\
            except $local_qualifier$b as $local_qualifier$b:\n\
            \t$local_qualifier$b\n\
            else:\n\
            \t$local_qualifier$b\n\
            finally:\n\
            \t$local_qualifier$b";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_statement
           "\nwith b as b: b"
           "\nwith $local_qualifier$b as $local_qualifier$b: $local_qualifier$b";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_statement "\nwhile b: b" "\nwhile a: a";
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_qualify_statement "yield b" "yield a";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_statement "yield from b" "yield from a";
      (* Always pick conditional imports. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      b
      if True:
        import a as b
      b
    |}
           {|
      a
      if True:
        import a as b
      a
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      b
      if False:
        import a as b
      b
    |}
           {|
      a
      if False:
        import a as b
      a
    |};
      (* Qualify assignments. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      a, *b = [1]
    |}
           {|
      $local_qualifier$a, *$local_qualifier$b = [1]
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      [*a, b] = [1]
    |}
           {|
      [*$local_qualifier$a, $local_qualifier$b] = [1]
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      a = [1, 2, 3]
      a[0], b = 4, 5
    |}
           {|
      $local_qualifier$a = [1, 2, 3]
      ($local_qualifier$a[0], $local_qualifier$b) = 4, 5
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      a = [[1], [2]]
      a[0][0], b = 4, 5
    |}
           {|
      $local_qualifier$a = [[1], [2]]
      ($local_qualifier$a[0][0], $local_qualifier$b) = 4, 5
    |};
      (* Qualify walrus assignments. *)
      (* TODO(T53600647): Qualify `a`. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      from module import constant
      (a := constant)
    |}
           {|
      from module import constant
      (a := module.constant)
    |};
      (* Qualify classes. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify "class Class: ..." "class qualifier.Class: ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      from module import C
      a = C
      class C: ...
      b = C
    |}
           {|
      from module import C
      $local_qualifier$a = qualifier.C
      class qualifier.C: ...
      $local_qualifier$b = qualifier.C
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      def foo(): pass
      class C:
        foo().x = 1
    |}
           {|
      def qualifier.foo(): pass
      class qualifier.C:
        qualifier.foo().x = 1
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      (* Don't qualify a string that is assigned to an explicitly-typed variable. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify {|
      x: str = "x"
    |} {|
      $local_qualifier$x: str = "x"
    |};
      (* We qualify global string assignments where the string matches an in-scope variable because
         these are potential aliases. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      x = "x"
      y = 1
      string_containing_y = "y"
    |}
           {|
      $local_qualifier$x = "$local_qualifier$x"
      $local_qualifier$y = 1
      $local_qualifier$string_containing_y = "$local_qualifier$y"
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      from typing_extensions import TypeAlias

      class C: ...
      my_alias: TypeAlias = "C"
    |}
           {|
      from typing_extensions import TypeAlias

      class qualifier.C: ...
      $local_qualifier$my_alias: typing_extensions.TypeAlias = "qualifier.C"
    |};
      (* Don't qualify the TypeVar name argument. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      from typing import TypeVar

      T: int = 3
      T2 = TypeVar("T")
    |}
           {|
      from typing import TypeVar

      $local_qualifier$T: int = 3
      $local_qualifier$T2 = typing.TypeVar("T")
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      (* Don't qualify strings within `Literal` even if it is aliased. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      from typing_extensions import Literal as MyLiteral

      x: int = 7
      valid_string_literal: MyLiteral["x"]
    |}
           {|
      from typing_extensions import Literal as MyLiteral

      $local_qualifier$x: int = 7
      $local_qualifier$valid_string_literal: typing_extensions.Literal["x"]
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      from typing import Generic, TypeVar

      T = TypeVar("T")
      class NotLiteral(Generic[T]): ...

      x: int = 8
      treats_x_as_annotation: NotLiteral["x"]
    |}
           {|
      from typing import Generic, TypeVar

      $local_qualifier$T = typing.TypeVar("T")
      class qualifier.NotLiteral(typing.Generic[$local_qualifier$T]): ...

      $local_qualifier$x: int = 8
      $local_qualifier$treats_x_as_annotation: qualifier.NotLiteral["$local_qualifier$x"]
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      from typing_extensions import Literal as MyLiteral

      class Foo:
        x: int = 7
        def treats_x_as_string_literal(self, a: MyLiteral["x"]) -> int: ...
    |}
           {|
      from typing_extensions import Literal as MyLiteral

      class qualifier.Foo():
        qualifier.Foo.x: int = 7
        def qualifier.Foo.treats_x_as_string_literal(
          $parameter$self,
          $parameter$a: typing_extensions.Literal["x"]
        ) -> int: ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      from typing import Generic, TypeVar

      T = TypeVar("T")
      class NotLiteral(Generic[T]): ...

      class Foo:
        x: int = 7
        def treats_x_as_attribute(self, a: NotLiteral["x"]) -> int: ...
    |}
           {|
      from typing import Generic, TypeVar

      $local_qualifier$T = typing.TypeVar("T")
      class qualifier.NotLiteral(typing.Generic[$local_qualifier$T]): ...

      class qualifier.Foo:
        qualifier.Foo.x: int = 7
        def qualifier.Foo.treats_x_as_attribute(
          $parameter$self,
          $parameter$a: qualifier.NotLiteral["qualifier.Foo.x"],
        ) -> int: ...
    |};
      (* TODO(T80454483): The alias case might be hard to handle within preprocessing. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      from typing_extensions import Literal

      LiteralAlias = Literal

      x: int = 7
      valid_string_literal: LiteralAlias["x"]
    |}
           {|
      from typing_extensions import Literal

      $local_qualifier$LiteralAlias = typing_extensions.Literal

      $local_qualifier$x: int = 7
      $local_qualifier$valid_string_literal: $local_qualifier$LiteralAlias["$local_qualifier$x"]
    |};
      (* Qualify parameters *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      class A: pass
      def foo(x: 'A'): ...
    |}
           {|
      class qualifier.A(): pass
      def qualifier.foo($parameter$x: 'qualifier.A'): ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      def foo(): pass
      foo()
    |}
           {|
      def qualifier.foo(): pass
      qualifier.foo()
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
        $local_qualifier?foo$bar()
        def $local_qualifier?foo$bar(): pass
        $local_qualifier?foo$bar()
    |};
      (* TODO(T47589601): We cannot correctly handle this case for now due to our current limited
         aliases recording mechanism. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      from abc import foo
      def bar(): foo()
      def foo(): pass
      foo()
      |}
           {|
      from abc import foo
      def qualifier.bar(): qualifier.foo()
      def qualifier.foo(): pass
      qualifier.foo()
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      qualifier.foo();
      def qualifier.foo(): pass
      qualifier.foo()
      from abc import foo
      qualifier.foo();
      def qualifier.foo(): pass
      qualifier.foo()
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      for b in []: pass
      def b(): pass
    |}
           {|
      for qualifier.b in []: pass
      def qualifier.b(): pass
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify "def foo(): ..." "def qualifier.foo(): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      def foo(): ...
      "expression".foo()
    |}
           {|
      def qualifier.foo(): ...
      "expression".foo()
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      foo()
      def foo(): pass
    |}
           {|
      qualifier.foo()
      def qualifier.foo(): pass
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      @foo.setter
      def foo(): pass
    |}
           {|
      @qualifier.foo.setter
      def qualifier.foo(): pass
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      from typing import List
      def foo() -> List[int]: pass
    |}
           {|
      from typing import List
      def qualifier.foo() -> typing.List[int]: pass
    |};
      (* TODO(T46137017): This is incorrect, as the variable should be hoisted *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      x = 7
      def foo():
        print(x)
        x = 9
    |}
           {|
      $local_qualifier$x = 7
      def qualifier.foo():
        print($local_qualifier?foo$x)
        $local_qualifier?foo$x = 9
    |};
      (* Nested defines. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      def foo():
        def nested(a):
          def a(): pass
    |}
           {|
      def qualifier.foo():
        def $local_qualifier?foo$nested($parameter$a):
          def $local_qualifier?foo?nested$a(): pass
    |};
      (* SSA-gutted. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      constant = 1
      constant = constant
    |}
           {|
      $local_qualifier$constant = 1
      $local_qualifier$constant = $local_qualifier$constant
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      def foo():
        foo()
        foo = 1
        foo()
    |}
           {|
      def qualifier.foo():
        $local_qualifier?foo$foo()
        $local_qualifier?foo$foo = 1
        $local_qualifier?foo$foo()
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      constant: int = 1
      constant = 2
    |}
           {|
      $local_qualifier$constant: int = 1
      $local_qualifier$constant = 2
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      constant = 0
      def foo():
        while True:
          constant += 1
    |}
           {|
      $local_qualifier$constant = 0
      def qualifier.foo():
        while True:
          $local_qualifier?foo$constant += 1
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      global_constant = 1
      def foo():
        nonlocal_constant = 2
        def bar():
          global global_constant
          nonlocal nonlocal_constant
          global_constant = 3
          nonlocal_constant = 4
          for global_constant, nonlocal_constant in []:
            pass
   |}
           {|
      $local_qualifier$global_constant = 1
      def qualifier.foo():
        $local_qualifier?foo$nonlocal_constant = 2
        def $local_qualifier?foo$bar():
          global global_constant
          nonlocal nonlocal_constant
          $local_qualifier$global_constant = 3
          $local_qualifier?foo$nonlocal_constant = 4
          for ($local_qualifier$global_constant, $local_qualifier?foo$nonlocal_constant) in []:
            pass
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      def foo(parameter):
        parameter = 1
    |}
           {|
      def qualifier.foo($parameter$parameter):
        $parameter$parameter = 1
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      def foo(parameter: int, other: parameter.T):
        parameter = 1
    |}
           {|
      def qualifier.foo($parameter$parameter: int, $parameter$other: parameter.T):
        $parameter$parameter = 1
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      def foo(foo):
        return foo
    |}
           {|
      def qualifier.foo($parameter$foo):
        return $parameter$foo
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      with item as (a, b):
        foo(a, b)
    |}
           {|
      with item as ($local_qualifier$a, $local_qualifier$b):
        foo($local_qualifier$a, $local_qualifier$b)
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      with item as [a, b]:
        foo(a, b)
    |}
           {|
      with item as [$local_qualifier$a, $local_qualifier$b]:
        foo($local_qualifier$a, $local_qualifier$b)
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      with item as (*a,):
        foo(a)
    |}
           {|
      with item as (*$local_qualifier$a,):
        foo($local_qualifier$a,)
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
       a: x = ...
       def x():
         ...
    |}
           {|
       $local_qualifier$a: qualifier.x = ...
       def qualifier.x(): ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
       def f(a: x): ...
       def x(): ...
    |}
           {|
       def qualifier.f($parameter$a: qualifier.x): ...
       def qualifier.x():
         ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      class C:
        def f(parameter: x):
          ...
        def x():
          ...
    |}
           {|
      class qualifier.C:
        def qualifier.C.f($parameter$parameter: x):
          ...
        def qualifier.C.x():
          ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      x: int = ...
      class C:
        x: int = ...
        y = x
    |}
           {|
      $local_qualifier$x: int = ...
      class qualifier.C:
        qualifier.C.x: int = ...
        qualifier.C.y = qualifier.C.x
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      class slice:
        pass
      class C:
        slice: slice = ...
    |}
           {|
      class qualifier.slice:
        pass
      class qualifier.C:
        qualifier.C.slice: qualifier.slice = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
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
    |};
      (* Qualify the type argument to `cast`. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      from typing import cast, Dict, List
      cast("List[Dict[str, object]]", 1)
    |}
           {|
      from typing import cast, Dict, List
      typing.cast("typing.List[typing.Dict[(str, object)]]", 1)
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      from typing import cast, Dict, List
      cast(List["Dict[str, object]"], 1)
    |}
           {|
      from typing import cast, Dict, List
      typing.cast(typing.List["typing.Dict[(str, object)]"], 1)
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      from typing import cast
      import pyre_extensions
      class A: ...
      class B(A): ...

      def foo(o: object) -> B:
        return cast('B', safe_cast('A', pyre_extensions.safe_cast('B', o)))
    |}
           {|
      from typing import cast
      import pyre_extensions
      class qualifier.A: ...
      class qualifier.B(qualifier.A): ...

      def qualifier.foo($parameter$o: object) -> qualifier.B:
        return typing.cast('qualifier.B', safe_cast('qualifier.A',
          pyre_extensions.safe_cast('qualifier.B', $parameter$o)))
    |};
      (* Treat the second argument to `cast` as a regular string. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      from typing import cast
      class A: ...
      cast('A', 'A')
    |}
           {|
      from typing import cast
      class qualifier.A: ...
      typing.cast('qualifier.A', 'A')
    |};
      (* Qualify quoted base classes. This is an edge case where the base class accepts the current
         class as a parameter. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      from typing import List

      class Bar(List["Bar"]): ...
    |}
           {|
      from typing import List

      class qualifier.Bar(typing.List["qualifier.Bar"]): ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      from typing import Annotated

      class Foo:
        def __init__(self, x: str) -> None: ...

      hello: int = 1
      foo: Annotated["int", Foo("hello")]
    |}
           {|
      from typing import Annotated

      class qualifier.Foo:
        def qualifier.Foo.__init__($parameter$self, $parameter$x: str) -> None: ...

      $local_qualifier$hello: int = 1
      $local_qualifier$foo: typing.Annotated["int", qualifier.Foo("hello")]
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      class A:
         for x in []:
           x
    |}
           {|
      class qualifier.A:
         for $local_qualifier?A$x in []:
           $local_qualifier?A$x
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      class A:
         with item as x:
           x
    |}
           {|
      class qualifier.A:
         with item as $local_qualifier?A$x:
           $local_qualifier?A$x
    |};
      (* Don't qualify x within d["x"]. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      import typing

      def foo(x: int) -> None:
        d: typing.Dict[str, int]
        d["x"]
    |}
           {|
      import typing

      def qualifier.foo($parameter$x: int) -> None:
        $local_qualifier?foo$d: typing.Dict[str, int]
        $local_qualifier?foo$d["x"]
    |};
      (* Recursive alias definition. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      from typing import Tuple, Union

      Tree = Union[int, Tuple["Tree", "Tree"]]
    |}
           {|
      from typing import Tuple, Union

      $local_qualifier$Tree = typing.Union[int, \
        typing.Tuple["$local_qualifier$Tree", "$local_qualifier$Tree"]]
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      def foo( *args, **kwargs): ...
    |}
           {|
      def qualifier.foo( *$parameter$args, **$parameter$kwargs): ...
    |};
      (* Class with the same name as the module. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      class qualifier: ...
    |}
           {|
      class qualifier.qualifier: ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      def foo():
        x: str = ""
        def bar():
          nonlocal x
          x = "x"

        def baz():
          y = x
    |}
           {|
      def qualifier.foo():
        $local_qualifier?foo$x: str = ""
        def $local_qualifier?foo$bar():
          nonlocal x
          $local_qualifier?foo$x = "x"

        def $local_qualifier?foo$baz():
          $local_qualifier?foo?baz$y = $local_qualifier?foo$x
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      def foo(x: str):
        def bar():
          nonlocal x
          x = "x"
    |}
           {|
      def qualifier.foo($parameter$x: str):
        def $local_qualifier?foo$bar():
          nonlocal x
          $parameter$x = "x"
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      def foo(x):
        for x in [1]:
          x
    |}
           {|
      def qualifier.foo($parameter$x):
        for $parameter$x in [1]:
          $parameter$x
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
        for (x, y) in []:
           foo(x, y)
    |}
           {|
        for ($local_qualifier$x, $local_qualifier$y) in []:
           foo($local_qualifier$x, $local_qualifier$y)
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
        for [x, y] in []:
           foo(x, y)
    |}
           {|
        for [$local_qualifier$x, $local_qualifier$y] in []:
           foo($local_qualifier$x, $local_qualifier$y)
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
        for (*x,) in []:
           foo(x)
    |}
           {|
        for (*$local_qualifier$x,) in []:
           foo($local_qualifier$x)
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      def f():
        d = {}
        k = "k"
        d[k], y = "v", 42
    |}
           {|
      def qualifier.f():
        $local_qualifier?f$d = {}
        $local_qualifier?f$k = "k"
        $local_qualifier?f$d[$local_qualifier?f$k], $local_qualifier?f$y = "v", 42
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           {|
      def foo(x: str):
        def bar():
          x = "x"
    |}
           {|
      def qualifier.foo($parameter$x: str):
        def $local_qualifier?foo$bar():
          $local_qualifier?foo?bar$x = "x"
    |};
    ]


let test_qualify_ast =
  let module Qualify = Preprocessing.Qualify in
  let scope =
    {
      Qualify.module_name = Reference.create "qualifier";
      parent = NestingContext.create_toplevel ();
      aliases = String.Map.singleton "a" { Qualify.name = Reference.create "b" };
      locals = String.Set.empty;
    }
  in
  let assert_qualify_statement statement expected _ =
    let qualify = Qualify.qualify_statement ~scope in
    let processed = qualify statement in
    assert_equal
      ~cmp:(fun left right -> Statement.location_insensitive_compare left right = 0)
      ~printer:Statement.show
      expected
      processed;
    (* Qualifying twice should not change the source. *)
    assert_equal
      ~cmp:(fun left right -> Statement.location_insensitive_compare left right = 0)
      ~printer:Statement.show
      expected
      (qualify processed)
  in
  let assert_qualify_match_case match_case expected _ =
    let qualify = Qualify.qualify_match_case ~scope in
    let processed = qualify match_case in
    assert_equal
      ~cmp:(fun left right -> Match.Case.location_insensitive_compare left right = 0)
      ~printer:Match.Case.show
      expected
      processed;
    (* Qualifying twice should not change the source. *)
    assert_equal
      ~cmp:(fun left right -> Match.Case.location_insensitive_compare left right = 0)
      ~printer:Match.Case.show
      expected
      (qualify processed)
  in
  let assert_qualify_pattern pattern expected _ =
    let qualify = Qualify.qualify_pattern ~scope in
    let processed = qualify pattern in
    assert_equal
      ~cmp:(fun left right -> Match.Pattern.location_insensitive_compare left right = 0)
      ~printer:Match.Pattern.show
      expected
      processed;
    (* Qualifying twice should not change the source. *)
    assert_equal
      ~cmp:(fun left right -> Match.Pattern.location_insensitive_compare left right = 0)
      ~printer:Match.Pattern.show
      expected
      (qualify processed)
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_statement (+Statement.Pass) (+Statement.Pass);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_statement
           (+Statement.Class
               {
                 Class.name = Reference.create "a";
                 base_arguments = [];
                 top_level_unbound_names = [];
                 type_params = [];
                 parent = NestingContext.create_toplevel ();
                 body = [];
                 decorators = [];
               })
           (+Statement.Class
               {
                 Class.name = Reference.create "qualifier.a";
                 base_arguments = [];
                 top_level_unbound_names = [];
                 type_params = [];
                 parent = NestingContext.create_toplevel ();
                 body = [];
                 decorators = [];
               });
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_statement
           (+Statement.TypeAlias
               {
                 name = +Expression.Name (Name.Identifier "a");
                 type_params = [];
                 value = +Expression.Name (Name.Identifier "c");
               })
           (+Statement.TypeAlias
               {
                 name = +Expression.Name (Name.Identifier "b");
                 type_params = [];
                 value = +Expression.Name (Name.Identifier "c");
               });
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_statement
           (+Statement.Match { subject = +Expression.Name (Name.Identifier "a"); cases = [] })
           (+Statement.Match { subject = +Expression.Name (Name.Identifier "b"); cases = [] });
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_match_case
           {
             Match.Case.guard = Some (+Expression.Name (Name.Identifier "a"));
             pattern = +Match.Pattern.MatchWildcard;
             body = [];
           }
           {
             Match.Case.guard = Some (+Expression.Name (Name.Identifier "b"));
             pattern = +Match.Pattern.MatchWildcard;
             body = [];
           };
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_match_case
           {
             Match.Case.guard = None;
             pattern = +Match.Pattern.MatchWildcard;
             body = [+Statement.Expression !"a"];
           }
           {
             Match.Case.guard = None;
             pattern = +Match.Pattern.MatchWildcard;
             body = [+Statement.Expression !"b"];
           };
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_pattern
           (+Match.Pattern.MatchAs { pattern = None; name = "a" })
           (+Match.Pattern.MatchAs { pattern = None; name = "b" });
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_pattern
           (+Match.Pattern.MatchAs
               {
                 pattern = Some (+Match.Pattern.MatchAs { pattern = None; name = "a" });
                 name = "x";
               })
           (+Match.Pattern.MatchAs
               {
                 pattern = Some (+Match.Pattern.MatchAs { pattern = None; name = "b" });
                 name = "x";
               });
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_pattern
           (+Match.Pattern.MatchClass
               {
                 class_name = +Ast.Expression.Name.Identifier "a";
                 patterns = [];
                 keyword_attributes = [];
                 keyword_patterns = [];
               })
           (+Match.Pattern.MatchClass
               {
                 class_name = +Ast.Expression.Name.Identifier "b";
                 patterns = [];
                 keyword_attributes = [];
                 keyword_patterns = [];
               });
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_pattern
           (+Match.Pattern.MatchClass
               {
                 class_name = +Ast.Expression.Name.Identifier "x";
                 patterns = [+Match.Pattern.MatchAs { pattern = None; name = "a" }];
                 keyword_attributes = [];
                 keyword_patterns = [];
               })
           (+Match.Pattern.MatchClass
               {
                 class_name = +Ast.Expression.Name.Identifier "x";
                 patterns = [+Match.Pattern.MatchAs { pattern = None; name = "b" }];
                 keyword_attributes = [];
                 keyword_patterns = [];
               });
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_pattern
           (+Match.Pattern.MatchClass
               {
                 class_name = +Ast.Expression.Name.Identifier "x";
                 patterns = [];
                 keyword_attributes = ["a"];
                 keyword_patterns = [+Match.Pattern.MatchAs { pattern = None; name = "a" }];
               })
           (+Match.Pattern.MatchClass
               {
                 class_name = +Ast.Expression.Name.Identifier "x";
                 patterns = [];
                 keyword_attributes = ["a"];
                 keyword_patterns = [+Match.Pattern.MatchAs { pattern = None; name = "b" }];
               });
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_pattern
           (+Match.Pattern.MatchMapping
               {
                 keys = [!"a"];
                 patterns = [+Match.Pattern.MatchAs { pattern = None; name = "x" }];
                 rest = None;
               })
           (+Match.Pattern.MatchMapping
               {
                 keys = [!"b"];
                 patterns = [+Match.Pattern.MatchAs { pattern = None; name = "x" }];
                 rest = None;
               });
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_pattern
           (+Match.Pattern.MatchMapping
               {
                 keys = [!"x"];
                 patterns = [+Match.Pattern.MatchAs { pattern = None; name = "a" }];
                 rest = None;
               })
           (+Match.Pattern.MatchMapping
               {
                 keys = [!"x"];
                 patterns = [+Match.Pattern.MatchAs { pattern = None; name = "b" }];
                 rest = None;
               });
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_pattern
           (+Match.Pattern.MatchMapping
               {
                 keys = [!"x"];
                 patterns = [+Match.Pattern.MatchAs { pattern = None; name = "x" }];
                 rest = Some "a";
               })
           (+Match.Pattern.MatchMapping
               {
                 keys = [!"x"];
                 patterns = [+Match.Pattern.MatchAs { pattern = None; name = "x" }];
                 rest = Some "b";
               });
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_pattern
           (+Match.Pattern.MatchOr
               [
                 +Match.Pattern.MatchAs { pattern = None; name = "x" };
                 +Match.Pattern.MatchAs { pattern = None; name = "a" };
               ])
           (+Match.Pattern.MatchOr
               [
                 +Match.Pattern.MatchAs { pattern = None; name = "x" };
                 +Match.Pattern.MatchAs { pattern = None; name = "b" };
               ]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_pattern
           (+Match.Pattern.MatchSequence
               [
                 +Match.Pattern.MatchAs { pattern = None; name = "x" };
                 +Match.Pattern.MatchAs { pattern = None; name = "a" };
               ])
           (+Match.Pattern.MatchSequence
               [
                 +Match.Pattern.MatchAs { pattern = None; name = "x" };
                 +Match.Pattern.MatchAs { pattern = None; name = "b" };
               ]);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_pattern
           (+Match.Pattern.MatchSingleton Ast.Expression.Constant.NoneLiteral)
           (+Match.Pattern.MatchSingleton Ast.Expression.Constant.NoneLiteral);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_pattern (+Match.Pattern.MatchStar None) (+Match.Pattern.MatchStar None);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_pattern
           (+Match.Pattern.MatchStar (Some "a"))
           (+Match.Pattern.MatchStar (Some "b"));
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify_pattern (+Match.Pattern.MatchValue !"a") (+Match.Pattern.MatchValue !"b");
    ]


let test_qualify_ast_class_with_same_name_as_local =
  let assert_qualify ~module_name statements expected_statements _ =
    let qualified statements =
      Source.create ~relative:module_name statements |> Preprocessing.qualify |> Source.statements
    in
    let assert_equal_statements =
      assert_equal
        ~cmp:(fun left right ->
          List.zip_exn left right
          |> List.for_all ~f:(fun (left, right) ->
                 Statement.location_insensitive_compare left right = 0))
        ~printer:[%show: Statement.t list]
    in
    let actual = qualified statements in
    assert_equal_statements expected_statements actual;
    (* Qualifying twice should not change the source. *)
    assert_equal_statements expected_statements (qualified actual)
  in
  test_list
    [
      (* Module name is `Foo`, global variable is `NotFoo`, and class is `Foo`. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           ~module_name:"Foo"
           [
             +Statement.Assign
                {
                  target = +Expression.Name (Name.Identifier "NotFoo");
                  annotation = None;
                  value = Some (+Expression.Name (Name.Identifier "None"));
                  origin = None;
                };
             (let parent = NestingContext.create_toplevel () in
              +Statement.Class
                 {
                   Class.name = Reference.create "Foo";
                   base_arguments = [];
                   top_level_unbound_names = [];
                   type_params = [];
                   parent;
                   body =
                     (let parent = NestingContext.create_class ~parent "Foo" in
                      [
                        +Statement.Define
                           {
                             signature =
                               {
                                 name = !&"some_method";
                                 parameters =
                                   [+{ Parameter.name = "self"; value = None; annotation = None }];
                                 decorators = [];
                                 return_annotation = None;
                                 async = false;
                                 generator = false;
                                 parent;
                                 legacy_parent = None;
                                 type_params = [];
                               };
                             captures = [];
                             unbound_names = [];
                             body =
                               [
                                 +Statement.Assign
                                    {
                                      target = +Expression.Name (Name.Identifier "x");
                                      annotation = Some (+Expression.Name (Name.Identifier "Foo"));
                                      value =
                                        Some
                                          (+Expression.Name
                                              (Name.Attribute
                                                 {
                                                   base = +Expression.Name (Name.Identifier "self");
                                                   attribute = "some_attribute";
                                                   origin = None;
                                                 }));
                                      origin = None;
                                    };
                               ];
                           };
                      ]);
                   decorators = [];
                 });
           ]
           [
             +Statement.Assign
                {
                  target = +Expression.Name (Name.Identifier "$local_Foo$NotFoo");
                  annotation = None;
                  value = Some (+Expression.Name (Name.Identifier "None"));
                  origin = None;
                };
             (let parent = NestingContext.create_toplevel () in
              +Statement.Class
                 {
                   Class.name = Reference.create "Foo.Foo";
                   base_arguments = [];
                   top_level_unbound_names = [];
                   type_params = [];
                   parent;
                   body =
                     (let parent = NestingContext.create_class ~parent "Foo" in
                      [
                        +Statement.Define
                           {
                             signature =
                               {
                                 name = !&"Foo.Foo.some_method";
                                 parameters =
                                   [
                                     +{
                                        Parameter.name = "$parameter$self";
                                        value = None;
                                        annotation = None;
                                      };
                                   ];
                                 decorators = [];
                                 return_annotation = None;
                                 async = false;
                                 generator = false;
                                 parent;
                                 legacy_parent = None;
                                 type_params = [];
                               };
                             captures = [];
                             unbound_names = [];
                             body =
                               [
                                 +Statement.Assign
                                    {
                                      target =
                                        +Expression.Name
                                           (Name.Identifier "$local_Foo?Foo?some_method$x");
                                      annotation =
                                        Some
                                          (+Expression.Name
                                              (Name.Attribute
                                                 {
                                                   base = +Expression.Name (Name.Identifier "Foo");
                                                   attribute = "Foo";
                                                   origin = None;
                                                 }));
                                      value =
                                        Some
                                          (+Expression.Name
                                              (Name.Attribute
                                                 {
                                                   base =
                                                     +Expression.Name
                                                        (Name.Identifier "$parameter$self");
                                                   attribute = "some_attribute";
                                                   origin = None;
                                                 }));
                                      origin = None;
                                    };
                               ];
                           };
                      ]);
                   decorators = [];
                 });
           ];
      (* Module name is `Foo`, global variable is `Foo`, and class is `Foo`. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           ~module_name:"Foo"
           [
             +Statement.Assign
                {
                  target = +Expression.Name (Name.Identifier "Foo");
                  annotation = None;
                  value = Some (+Expression.Name (Name.Identifier "None"));
                  origin = None;
                };
             (let parent = NestingContext.create_toplevel () in
              +Statement.Class
                 {
                   Class.name = Reference.create "Foo";
                   base_arguments = [];
                   parent;
                   top_level_unbound_names = [];
                   type_params = [];
                   body =
                     (let parent = NestingContext.create_class ~parent "Foo" in
                      [
                        +Statement.Define
                           {
                             signature =
                               {
                                 name = !&"some_method";
                                 parameters =
                                   [+{ Parameter.name = "self"; value = None; annotation = None }];
                                 decorators = [];
                                 return_annotation = None;
                                 async = false;
                                 generator = false;
                                 parent;
                                 legacy_parent = None;
                                 type_params = [];
                               };
                             captures = [];
                             unbound_names = [];
                             body =
                               [
                                 +Statement.Assign
                                    {
                                      target = +Expression.Name (Name.Identifier "x");
                                      annotation = Some (+Expression.Name (Name.Identifier "Foo"));
                                      value =
                                        Some
                                          (+Expression.Name
                                              (Name.Attribute
                                                 {
                                                   base = +Expression.Name (Name.Identifier "self");
                                                   attribute = "some_attribute";
                                                   origin = None;
                                                 }));
                                      origin = None;
                                    };
                               ];
                           };
                      ]);
                   decorators = [];
                 });
           ]
           [
             +Statement.Assign
                {
                  target =
                    +Expression.Name
                       (Name.Attribute
                          {
                            base = +Expression.Name (Name.Identifier "Foo");
                            attribute = "Foo";
                            origin = None;
                          });
                  annotation = None;
                  value = Some (+Expression.Name (Name.Identifier "None"));
                  origin = None;
                };
             (let parent = NestingContext.create_toplevel () in
              +Statement.Class
                 {
                   Class.name = Reference.create "Foo.Foo";
                   base_arguments = [];
                   top_level_unbound_names = [];
                   type_params = [];
                   parent;
                   body =
                     (let parent = NestingContext.create_class ~parent "Foo" in
                      [
                        +Statement.Define
                           {
                             signature =
                               {
                                 name = !&"Foo.Foo.some_method";
                                 parameters =
                                   [
                                     +{
                                        Parameter.name = "$parameter$self";
                                        value = None;
                                        annotation = None;
                                      };
                                   ];
                                 decorators = [];
                                 return_annotation = None;
                                 async = false;
                                 generator = false;
                                 parent;
                                 legacy_parent = None;
                                 type_params = [];
                               };
                             captures = [];
                             unbound_names = [];
                             body =
                               [
                                 +Statement.Assign
                                    {
                                      target =
                                        +Expression.Name
                                           (Name.Identifier "$local_Foo?Foo?some_method$x");
                                      annotation =
                                        Some
                                          (+Expression.Name
                                              (Name.Attribute
                                                 {
                                                   base = +Expression.Name (Name.Identifier "Foo");
                                                   attribute = "Foo";
                                                   origin = None;
                                                 }));
                                      value =
                                        Some
                                          (+Expression.Name
                                              (Name.Attribute
                                                 {
                                                   base =
                                                     +Expression.Name
                                                        (Name.Identifier "$parameter$self");
                                                   attribute = "some_attribute";
                                                   origin = None;
                                                 }));
                                      origin = None;
                                    };
                               ];
                           };
                      ]);
                   decorators = [];
                 });
           ];
      (* Module name is `NotFoo`, global variable is `Foo`, and class is `Foo`. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_qualify
           ~module_name:"NotFoo"
           [
             +Statement.Assign
                {
                  target = +Expression.Name (Name.Identifier "Foo");
                  annotation = None;
                  value = Some (+Expression.Name (Name.Identifier "None"));
                  origin = None;
                };
             (let parent = NestingContext.create_toplevel () in
              +Statement.Class
                 {
                   Class.name = Reference.create "Foo";
                   base_arguments = [];
                   top_level_unbound_names = [];
                   type_params = [];
                   parent;
                   body =
                     (let parent = NestingContext.create_class ~parent "Foo" in
                      [
                        +Statement.Define
                           {
                             signature =
                               {
                                 name = !&"some_method";
                                 parameters =
                                   [+{ Parameter.name = "self"; value = None; annotation = None }];
                                 decorators = [];
                                 return_annotation = None;
                                 async = false;
                                 generator = false;
                                 parent;
                                 legacy_parent = None;
                                 type_params = [];
                               };
                             captures = [];
                             unbound_names = [];
                             body =
                               [
                                 +Statement.Assign
                                    {
                                      target = +Expression.Name (Name.Identifier "x");
                                      annotation = Some (+Expression.Name (Name.Identifier "Foo"));
                                      value =
                                        Some
                                          (+Expression.Name
                                              (Name.Attribute
                                                 {
                                                   base = +Expression.Name (Name.Identifier "self");
                                                   attribute = "some_attribute";
                                                   origin = None;
                                                 }));
                                      origin = None;
                                    };
                               ];
                           };
                      ]);
                   decorators = [];
                 });
           ]
           [
             +Statement.Assign
                {
                  target =
                    +Expression.Name
                       (Name.Attribute
                          {
                            base = +Expression.Name (Name.Identifier "NotFoo");
                            attribute = "Foo";
                            origin = None;
                          });
                  annotation = None;
                  value = Some (+Expression.Name (Name.Identifier "None"));
                  origin = None;
                };
             (let parent = NestingContext.create_toplevel () in
              +Statement.Class
                 {
                   Class.name = Reference.create "NotFoo.Foo";
                   base_arguments = [];
                   top_level_unbound_names = [];
                   type_params = [];
                   parent;
                   body =
                     (let parent = NestingContext.create_class ~parent "Foo" in
                      [
                        +Statement.Define
                           {
                             signature =
                               {
                                 name = !&"NotFoo.Foo.some_method";
                                 parameters =
                                   [
                                     +{
                                        Parameter.name = "$parameter$self";
                                        value = None;
                                        annotation = None;
                                      };
                                   ];
                                 decorators = [];
                                 return_annotation = None;
                                 async = false;
                                 generator = false;
                                 parent;
                                 legacy_parent = None;
                                 type_params = [];
                               };
                             captures = [];
                             unbound_names = [];
                             body =
                               [
                                 +Statement.Assign
                                    {
                                      target =
                                        +Expression.Name
                                           (Name.Identifier "$local_NotFoo?Foo?some_method$x");
                                      annotation =
                                        Some
                                          (+Expression.Name
                                              (Name.Attribute
                                                 {
                                                   base =
                                                     +Expression.Name (Name.Identifier "NotFoo");
                                                   attribute = "Foo";
                                                   origin = None;
                                                 }));
                                      value =
                                        Some
                                          (+Expression.Name
                                              (Name.Attribute
                                                 {
                                                   base =
                                                     +Expression.Name
                                                        (Name.Identifier "$parameter$self");
                                                   attribute = "some_attribute";
                                                   origin = None;
                                                 }));
                                      origin = None;
                                    };
                               ];
                           };
                      ]);
                   decorators = [];
                 });
           ];
    ]


let test_replace_version_specific_code =
  let assert_preprocessed ~major_version ~minor_version ~micro_version source expected _ =
    let handle = "test.py" in
    assert_source_equal
      ~location_insensitive:true
      (parse ~handle expected)
      (Preprocessing.replace_version_specific_code
         ~major_version
         ~minor_version
         ~micro_version
         (parse ~handle source))
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:0
           ~micro_version:1
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:2
           ~minor_version:7
           ~micro_version:15
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
        def incompatible()->int:
          ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:4
           ~micro_version:10
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:2
           ~minor_version:7
           ~micro_version:18
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
        def incompatible()->int:
          ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:2
           ~minor_version:7
           ~micro_version:18
           {|
        if sys.version_info < (3,):
            _encodable = Union[bytes, Text]
            _decodable = Union[bytes, Text]
        elif sys.version_info < (3, 3):
            _encodable = bytes
            _decodable = bytes
        elif sys.version_info >= (3, 4):
            _encodable = Union[bytes, bytearray, memoryview]
            _decodable = Union[bytes, bytearray, memoryview, str]
  |}
           {|
        _encodable = Union[bytes, Text]
        _decodable = Union[bytes, Text]
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:2
           ~micro_version:10
           {|
        if sys.version_info < (3,):
            _encodable = Union[bytes, Text]
            _decodable = Union[bytes, Text]
        elif sys.version_info < (3, 3):
            _encodable = bytes
            _decodable = bytes
        elif sys.version_info >= (3, 4):
            _encodable = Union[bytes, bytearray, memoryview]
            _decodable = Union[bytes, bytearray, memoryview, str]
  |}
           {|
        _encodable = bytes
        _decodable = bytes
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:4
           ~micro_version:10
           {|
        if sys.version_info < (3,):
            _encodable = Union[bytes, Text]
            _decodable = Union[bytes, Text]
        elif sys.version_info < (3, 3):
            _encodable = bytes
            _decodable = bytes
        elif sys.version_info >= (3, 4):
            _encodable = Union[bytes, bytearray, memoryview]
            _decodable = Union[bytes, bytearray, memoryview, str]
    |}
           {|
        _encodable = Union[bytes, bytearray, memoryview]
        _decodable = Union[bytes, bytearray, memoryview, str]
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:0
           ~micro_version:1
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
        def incompatible()->int:
          ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:4
           ~micro_version:10
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:4
           ~micro_version:10
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:4
           ~micro_version:10
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:4
           ~micro_version:10
           {|
       class C():
         if sys.version_info < (3, ):
          def incompatible()->int:
            ...
    |}
           {|
       class C():
         pass
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:6
           ~micro_version:12
           {|
      if sys.version_info >= (3, 5):
        from A import B
      else:
        from A import C
    |}
           {|
       from A import B
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:6
           ~micro_version:12
           {|
      if (3, 5) >= sys.version_info :
        from A import B
      else:
        from A import C
    |}
           {|
       from A import C
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:6
           ~micro_version:12
           {|
      if sys.version_info >= (3, 6, 9):
        from A import B
      else:
        from A import C
    |}
           {|
       from A import B
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:6
           ~micro_version:12
           {|
      if sys.version_info < (3, 6, 9):
        from A import B
      else:
        from A import C
    |}
           {|
       from A import C
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:6
           ~micro_version:12
           {|
      if sys.version_info[0] >= 3:
        a = 1
      else:
        a = 2
    |}
           {|
      a = 1
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:6
           ~micro_version:12
           {|
      if 6 > sys.version_info[1]:
        a = 1
      else:
        a = 2
    |}
           {|
      a = 2
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:4
           ~micro_version:10
           {|
      if sys.version_info[0] < 3:
        a = 1
      else:
        a = 2
    |}
           {|
      a = 2
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:4
           ~micro_version:10
           {|
      if 5 <= sys.version_info[1]:
        a = 1
      else:
        a = 2
    |}
           {|
      a = 2
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:4
           ~micro_version:10
           {|
      if 10 == sys.version_info[2]:
        a = 1
      else:
        a = 2
    |}
           {|
      a = 1
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:4
           ~micro_version:10
           {|
      if (3, 4) == sys.version_info:
        a = 1
      else:
        a = 2
    |}
           {|
      a = 1
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:4
           ~micro_version:10
           {|
      if sys.version_info != (3, 4):
        a = 1
      else:
        a = 2
    |}
           {|
      a = 2
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:4
           ~micro_version:10
           {|
      if 3 == sys.version_info[0]:
        a = 1
      else:
        a = 2
    |}
           {|
      a = 1
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:4
           ~micro_version:10
           {|
      if sys.version_info[1] != 4:
        a = 1
      else:
        a = 2
    |}
           {|
      a = 2
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:4
           ~micro_version:10
           {|
      if sys.version_info.major == 3:
        a = 1
      else:
        a = 2
    |}
           {|
      a = 1
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:4
           ~micro_version:10
           {|
      if sys.version_info.minor == 4:
        a = 1
      else:
        a = 2
    |}
           {|
      a = 1
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:4
           ~micro_version:10
           {|
      if sys.version_info.micro == 10:
        a = 1
      else:
        a = 2
    |}
           {|
      a = 1
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:4
           ~micro_version:10
           {|
      if 3 == sys.version_info.major:
        a = 1
      else:
        a = 2
    |}
           {|
      a = 1
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:4
           ~micro_version:10
           {|
      if 4 == sys.version_info.minor:
        a = 1
      else:
        a = 2
    |}
           {|
      a = 1
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:4
           ~micro_version:10
           {|
      if 10 == sys.version_info.micro:
        a = 1
      else:
        a = 2
    |}
           {|
      a = 1
    |};
      (* TODO(T148972568) Support version ranges *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           ~major_version:3
           ~minor_version:8
           ~micro_version:6
           {|
      if sys.version_info > (3, 8) and sys.version_info < (3, 12):
        a = 5
      else:
        b = 6
    |}
           {|
      if sys.version_info > (3, 8) and sys.version_info < (3, 12):
        a = 5
      else:
        b = 6
    |};
    ]


let test_replace_platform_specific_code =
  let assert_preprocessed ?(handle = "stub.pyi") source expected _ =
    assert_source_equal
      ~location_insensitive:true
      (parse ~handle expected)
      (Preprocessing.replace_platform_specific_code ~sys_platform:"linux" (parse ~handle source))
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           {|
      if not sys.platform.startswith('win32'):
        a = 1
      else:
        a = 2
    |}
           {|
      a = 1
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           {|
      if sys.platform.startswith('win32'):
        a = 1
      else:
        a = 2
    |}
           {|
      a = 2
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           {|
      if sys.platform != 'win32':
        a = 1
    |}
           {|
      a = 1
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           {|
      if sys.platform != 'linux':
        a = 8
    |}
           {|
      pass
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           {|
      if sys.platform == 'linux':
        a = 9
      else:
        b = 10
        c = 11
    |}
           {|
      a = 9
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           {|
      if sys.platform == 'win32' or sys.platform == 'darwin':
        a = 12
    |}
           {|
      if sys.platform == 'win32' or sys.platform == 'darwin':
        a = 12
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_preprocessed
           {|
      if 'linux' == sys.platform:
        a = 22
        b = 23
      else:
        c = 24
    |}
           {|
      a = 22
      b = 23
    |};
    ]


let test_expand_type_checking_imports =
  let assert_expanded source expected _ =
    assert_source_equal
      ~location_insensitive:true
      (parse expected)
      (Preprocessing.expand_type_checking_imports (parse source))
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expanded {|
      if typing.TYPE_CHECKING:
        pass
    |} {|
      pass
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expanded
           {|
      from typing import TYPE_CHECKING
      if TYPE_CHECKING:
        pass
    |}
           {|
      from typing import TYPE_CHECKING
      pass
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expanded
           {|
      from typing import TYPE_CHECKING
      if TYPE_CHECKING or False:
        pass
    |}
           {|
      from typing import TYPE_CHECKING
      pass
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expanded
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expanded
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expanded
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expanded
           {|
      if not typing.TYPE_CHECKING:
        pass
      else:
        1
    |}
           {|
      1
    |};
    ]


let test_expand_wildcard_imports =
  let assert_expanded external_sources check_source expected context =
    let source_code_api =
      ScratchProject.setup ~context ~external_sources ["test.py", check_source]
      |> ScratchProject.get_untracked_source_code_api
    in
    assert_equal
      ~cmp:(List.equal (fun left right -> Statement.location_insensitive_compare left right = 0))
      ~printer:(fun statement_list -> List.map statement_list ~f:show |> String.concat ~sep:", ")
      (Source.statements (parse expected))
      (Source.statements
         (Option.value_exn (SourceCodeApi.source_of_qualifier source_code_api !&"test")))
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expanded
           ["a.py", "def foo(): pass"]
           {|
      from a import b
    |}
           {|
      from a import b
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expanded
           ["a.py", "def foo(): pass"]
           {|
      from a import *
    |}
           {|
      from a import foo as foo
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expanded
           ["a.py", "def foo(): pass"; "b.py", "def bar(): pass"]
           {|
      from a import *
      from b import *
    |}
           {|
      from a import foo as foo
      from b import bar as bar
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expanded
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expanded
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expanded ["a.py", ""] {|
      from a import *
    |} "";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expanded
           ["a.py", ""; "b.py", "x = 1"]
           {|
      from a import *
      from b import *
    |}
           {|
      from b import x as x
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expanded
           ["a.py", ""; "b.py", "x = 1"]
           {|
      from a import *
      from b import *
    |}
           {|
      from b import x as x
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expanded
           ["a.py", ""; "b.py", "from a import *"; "c.py", "x = 1"]
           {|
      from a import *
      from b import *
      from c import *
    |}
           {|
      from c import x as x
    |};
    ]


let test_expand_implicit_returns =
  let assert_expand source expected _ =
    assert_source_equal
      ~location_insensitive:true
      (parse expected)
      (Preprocessing.expand_implicit_returns (parse source))
  in
  let assert_expand_implicit_returns source expected_body _ =
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
                    name = !&"foo";
                    parameters = [];
                    decorators = [];
                    return_annotation = None;
                    async = false;
                    generator = false;
                    parent = NestingContext.create_toplevel ();
                    legacy_parent = None;
                    type_params = [];
                  };
                captures = [];
                unbound_names = [];
                body = expected_body;
              };
         ])
  in
  let assert_implicit_return_location source expected_location _ =
    let expanded = Preprocessing.expand_implicit_returns (parse ~handle:"test.py" source) in
    match List.rev (Source.statements expanded) with
    | { Node.value = Define { body; _ }; _ } :: _ -> (
        match List.rev body with
        | return :: _ -> assert_equal ~printer:Location.show expected_location return.location
        | _ -> failwith "Preprocessed source's Define body is empty")
    | _ -> failwith "Preprocessed source failed"
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand_implicit_returns
           {|
      def foo():
        pass
    |}
           [+Statement.Pass; +Statement.Return { Return.expression = None; is_implicit = true }];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      def foo():
        yield None
    |}
           {|
      def foo():
        yield None
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand_implicit_returns
           {|
      def foo():
        try:
          pass
        finally:
          pass
    |}
           [
             +Statement.Try
                {
                  Try.body = [+Statement.Pass];
                  handlers = [];
                  orelse = [];
                  finally = [+Statement.Pass];
                  handles_exception_group = false;
                };
             +Statement.Return { Return.expression = None; is_implicit = true };
           ];
      (* Lol termination analysis. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand_implicit_returns
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_implicit_return_location
           {|
       def foo() -> int:
         pass
     |}
           {
             Location.start = { Location.line = 3; Location.column = 2 };
             stop = { Location.line = 3; Location.column = 6 };
           };
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_implicit_return_location
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_implicit_return_location
           {|
       def foo(x: bool) -> int:
         try:
           if x:
             y = 5/0
         except ZeroDivisionError:
           print ("tried to divide by 0")
         else:
           pass
       |}
           {
             Location.start = { Location.line = 9; Location.column = 4 };
             stop = { Location.line = 9; Location.column = 8 };
           };
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_implicit_return_location
           {|
       def foo(x: bool) -> int:
         try:
           if x:
             y = 5/0
         except ZeroDivisionError:
           print ("tried to divide by 0")
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_implicit_return_location
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
           };
    ]


let test_defines =
  let assert_defines statements defines _ =
    let printer defines = List.map defines ~f:Define.show |> String.concat ~sep:"\n" in
    let source = Source.create statements in
    assert_equal
      ~cmp:(List.equal [%compare.equal: Define.t])
      ~printer
      defines
      (Preprocessing.defines ~include_toplevels:true source |> List.map ~f:Node.value);
    assert_equal
      ~cmp:Int.equal
      ~printer:Int.to_string
      (Preprocessing.defines ~include_stubs:true ~include_nested:true ~include_toplevels:true source
      |> List.length)
      (Preprocessing.count_defines source)
  in
  let create_define ~parent name =
    {
      Define.signature =
        {
          name = !&name;
          parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
          decorators = [];
          return_annotation = None;
          async = false;
          generator = false;
          parent;
          legacy_parent = None;
          type_params = [];
        };
      captures = [];
      unbound_names = [];
      body = [+Statement.Expression (+Expression.Constant (Constant.Float 1.0))];
    }
  in
  let create_toplevel body =
    {
      Define.signature =
        {
          name = !&"$toplevel";
          parameters = [];
          decorators = [];
          return_annotation = None;
          async = false;
          generator = false;
          parent = NestingContext.create_toplevel ();
          legacy_parent = None;
          type_params = [];
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
          name = !&(parent ^ ".$class_toplevel");
          parameters = [];
          decorators = [];
          return_annotation = None;
          async = false;
          generator = false;
          parent = NestingContext.create_class ~parent:(NestingContext.create_toplevel ()) parent;
          legacy_parent = Some (Reference.create parent);
          type_params = [];
        };
      captures = [];
      unbound_names = [];
      body;
    }
  in
  test_list
    [
      (let define = create_define ~parent:(NestingContext.create_toplevel ()) "foo" in
       labeled_test_case __FUNCTION__ __LINE__
       @@ assert_defines
            [+Statement.Define define]
            [create_toplevel [+Statement.Define define]; define]);
      (let inner =
         {
           Define.signature =
             {
               name = !&"foo";
               parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
               decorators = [];
               return_annotation = None;
               async = false;
               generator = false;
               parent = NestingContext.(create_function ~parent:(create_toplevel ()) "foo");
               legacy_parent = None;
               type_params = [];
             };
           captures = [];
           unbound_names = [];
           body = [+Statement.Expression (+Expression.Constant (Constant.Float 1.0))];
         }
       in
       let define =
         {
           Define.signature =
             {
               name = !&"foo";
               parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
               decorators = [];
               return_annotation = None;
               async = false;
               generator = false;
               parent = NestingContext.create_toplevel ();
               legacy_parent = None;
               type_params = [];
             };
           captures = [];
           unbound_names = [];
           body =
             [
               +Statement.Expression (+Expression.Constant (Constant.Float 1.0));
               +Statement.Define inner;
             ];
         }
       in
       labeled_test_case __FUNCTION__ __LINE__
       @@ assert_defines
            [+Statement.Define define]
            [create_toplevel [+Statement.Define define]; define]);
      (let inner =
         {
           Define.signature =
             {
               name = !&"foo";
               parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
               decorators = [];
               return_annotation = None;
               async = false;
               generator = false;
               parent = NestingContext.(create_function ~parent:(create_toplevel ()) "foo");
               legacy_parent = None;
               type_params = [];
             };
           captures = [];
           unbound_names = [];
           body = [+Statement.Expression (+Expression.Constant (Constant.Float 1.0))];
         }
       in
       let define =
         {
           Define.signature =
             {
               name = !&"foo";
               parameters = [+{ Parameter.name = "a"; value = None; annotation = None }];
               decorators = [];
               return_annotation = None;
               async = false;
               generator = false;
               parent = NestingContext.create_toplevel ();
               legacy_parent = None;
               type_params = [];
             };
           captures = [];
           unbound_names = [];
           body =
             [
               +Statement.Expression (+Expression.Constant (Constant.Float 1.0));
               +Statement.Define inner;
             ];
         }
       in
       let if_define =
         {
           If.test = +Expression.Constant Constant.Ellipsis;
           body = [+Statement.Define define];
           orelse = [];
         }
       in
       labeled_test_case __FUNCTION__ __LINE__
       @@ assert_defines
            [+Statement.If if_define]
            [create_toplevel [+Statement.If if_define]; define]);
      ((* Note: Defines are returned in reverse order. *)
       let parent = NestingContext.(create_class ~parent:(create_toplevel ()) "Foo") in
       let define_foo = create_define ~parent "foo" in
       let define_bar = create_define ~parent "bar" in
       let body = [+Statement.Define define_foo; +Statement.Define define_bar] in
       let parent =
         {
           Class.name = !&"Foo";
           base_arguments = [];
           parent = NestingContext.create_toplevel ();
           body;
           decorators = [];
           top_level_unbound_names = [];
           type_params = [];
         }
       in
       labeled_test_case __FUNCTION__ __LINE__
       @@ assert_defines
            [+Statement.Class parent]
            [
              create_toplevel [+Statement.Class parent];
              create_class_toplevel ~parent:"Foo" ~body;
              define_bar;
              define_foo;
            ]);
    ]


let test_classes =
  let assert_classes statements class_defines _ =
    assert_equal
      ~cmp:(List.equal [%compare.equal: Class.t])
      (Preprocessing.classes (Source.create statements) |> List.map ~f:Node.value)
      class_defines
  in
  test_list
    [
      (let class_define =
         let parent = NestingContext.create_toplevel () in
         {
           Class.name = !&"foo";
           base_arguments = [];
           parent;
           body =
             (let parent = NestingContext.create_class ~parent "foo" in
              [
                +Statement.Define
                   {
                     signature =
                       {
                         name = !&"bar";
                         parameters = [];
                         decorators = [];
                         return_annotation = None;
                         async = false;
                         generator = false;
                         parent;
                         legacy_parent = Some !&"foo";
                         type_params = [];
                       };
                     captures = [];
                     unbound_names = [];
                     body = [+Statement.Pass];
                   };
              ]);
           decorators = [];
           top_level_unbound_names = [];
           type_params = [];
         }
       in
       labeled_test_case __FUNCTION__ __LINE__
       @@ assert_classes [+Statement.Class class_define] [class_define]);
      (let toplevel_context = NestingContext.create_toplevel () in
       let foo_context = NestingContext.create_class ~parent:toplevel_context "foo" in
       let inner =
         {
           Class.name = !&"bar";
           base_arguments = [];
           parent = foo_context;
           body = [+Statement.Pass];
           decorators = [];
           top_level_unbound_names = [];
           type_params = [];
         }
       in
       let class_define =
         {
           Class.name = !&"foo";
           base_arguments = [];
           parent = toplevel_context;
           body = [+Statement.Class inner];
           decorators = [];
           top_level_unbound_names = [];
           type_params = [];
         }
       in
       labeled_test_case __FUNCTION__ __LINE__
       @@ assert_classes [+Statement.Class class_define] [class_define; inner]);
    ]


let test_toplevel_assigns =
  let assert_assigns source expected _ =
    let assign_to_statement { Node.location; Node.value = assign } =
      { Node.location; Node.value = Statement.Assign assign }
    in
    assert_source_equal
      ~location_insensitive:true
      (source
      |> parse
      |> Preprocessing.toplevel_assigns
      |> List.concat_map ~f:Preprocessing.toplevel_expand_tuple_assign
      |> List.map ~f:assign_to_statement
      |> List.rev
      |> Source.create)
      (parse expected)
  in
  (* Dealing with tuple assignments *)
  let simultaneous_assign =
    {
      Node.location = Location.any;
      Node.value =
        {
          Assign.target =
            +Expression.Tuple
               [+Expression.Name (Name.Identifier "a"); +Expression.Name (Name.Identifier "b")];
          annotation = None;
          Assign.value =
            Some
              (+Expression.Tuple
                  [+Expression.Name (Name.Identifier "c"); +Expression.Name (Name.Identifier "d")]);
          origin = None;
        };
    }
  in
  let sequential_assigns =
    [
      {
        Node.location = Location.any;
        Node.value =
          {
            Assign.target = +Expression.Name (Name.Identifier "b");
            annotation = None;
            Assign.value = Some (+Expression.Name (Name.Identifier "d"));
            origin = None;
          };
      };
      {
        Node.location = Location.any;
        Node.value =
          {
            Assign.target = +Expression.Name (Name.Identifier "a");
            annotation = None;
            Assign.value = Some (+Expression.Name (Name.Identifier "c"));
            origin = None;
          };
      };
    ]
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_assigns {|
    x = 1
  |} {|
    x = 1
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_assigns
           {|
    x = 1
    y = 2
    def f(z):
      w = 3
  |}
           {|
    x = 1
    y = 2
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_assigns
           {|
    class F:
      ...

    x = F()
    y = f"hello{x}"
    def f(z):
      def h():
        w = ...
      h = f
    z = f
  |}
           {|
    x = F()
    y = f"hello{x}"
    z = f
  |};
      (labeled_test_case __FUNCTION__ __LINE__
      @@ fun _ ->
      assert_equal
        ~cmp:(List.equal (Node.equal (fun a b -> Assign.location_insensitive_compare a b = 0)))
        ~printer:(fun list -> String.concat (List.map ~f:(Node.show Assign.pp) list))
        (Preprocessing.toplevel_expand_tuple_assign simultaneous_assign)
        sequential_assigns);
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_assigns {|
    a, b = 1, 2
  |} {|
    a = 1
    b = 2
  |};
      (* More complicated example of a tuple assignment to an iterable *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_assigns
           {|
    a, b = "hello world".split()
  |}
           {|
    (a, b) = "hello world".split()
  |};
      (* Example unsoundness from using `toplevel_expand_tuple_assign` *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_assigns {|
    a, b = b, a
  |} {|
      a = b
      b = a
  |};
    ]


let test_replace_lazy_import =
  let my_lazy_import = !&"my.own.lazy_import" in
  let is_lazy_import = Reference.equal my_lazy_import in
  let assert_replaced source expected _ =
    let parse = parse ~handle:"test.py" in
    assert_source_equal
      ~location_insensitive:true
      (parse expected)
      (Preprocessing.replace_lazy_import ~is_lazy_import (parse source))
  in

  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replaced
           {|
       import my
       x = my.own.lazy_import("a.b.c")
    |}
           {|
       import my
       import a.b.c as x
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replaced
           {|
       from my import own
       x = own.lazy_import("a.b.c")
    |}
           {|
       from my import own
       import a.b.c as x
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replaced
           {|
       from my.own import lazy_import
       x = lazy_import("a.b.c")
    |}
           {|
       from my.own import lazy_import
       import a.b.c as x
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replaced
           {|
       import not_my
       x = not_my.own.lazy_import("a.b.c")
    |}
           {|
       import not_my
       x = not_my.own.lazy_import("a.b.c")
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replaced
           {|
       from not_my.own import lazy_import
       x = lazy_import("a.b.c")
    |}
           {|
       from not_my.own import lazy_import
       x = lazy_import("a.b.c")
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replaced
           {|
       import my as derp
       x = derp.own.lazy_import("a.b.c")
    |}
           {|
       import my as derp
       import a.b.c as x
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replaced
           {|
       import my.own as derp
       x = derp.lazy_import("a.b.c")
    |}
           {|
       import my.own as derp
       import a.b.c as x
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replaced
           {|
       from my import own as derp
       x = derp.lazy_import("a.b.c")
    |}
           {|
       from my import own as derp
       import a.b.c as x
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replaced
           {|
       from my.own import lazy_import as derp
       x = derp("a.b.c")
    |}
           {|
       from my.own import lazy_import as derp
       import a.b.c as x
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replaced
           {|
       import my
       x: Any = my.own.lazy_import("a.b.c")
    |}
           {|
       import my
       import a.b.c as x
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replaced
           {|
       import my
       x = my.own.lazy_import("a.b", "c")
    |}
           {|
       import my
       from a.b import c as x
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replaced
           {|
       import my
       if derp:
         x = my.own.lazy_import("a.b.c")
       else:
         y = my.own.lazy_import("a.b", "c")
    |}
           {|
       import my
       if derp:
         import a.b.c as x
       else:
         from a.b import c as y
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replaced
           {|
       import my
       while derp:
         x = my.own.lazy_import("a.b.c")
       else:
         y = my.own.lazy_import("a.b", "c")
    |}
           {|
       import my
       while derp:
         import a.b.c as x
       else:
         from a.b import c as y
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replaced
           {|
       import my
       with derp as d:
         x = my.own.lazy_import("a.b.c")
    |}
           {|
       import my
       with derp as d:
         import a.b.c as x
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replaced
           {|
       import my
       try:
         x = my.own.lazy_import("a.b.c")
       except:
         y = my.own.lazy_import("a.b", "c")
       finally:
         z: Any = my.own.lazy_import("a", "b")
    |}
           {|
       import my
       try:
         import a.b.c as x
       except:
         from a.b import c as y
       finally:
         from a import b as z
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replaced
           {|
       import my
       def foo():
         x = my.own.lazy_import("a.b.c")
       class Foo:
         y = my.own.lazy_import("a.b", "c")
    |}
           {|
       import my
       def foo():
         import a.b.c as x
       class Foo:
         from a.b import c as y
    |};
    ]


let test_expand_typed_dictionaries =
  let assert_expand ?(handle = "") source expected _ =
    let expected = parse ~handle ~coerce_special_methods:true expected in
    let actual = parse ~handle source |> Preprocessing.expand_typed_dictionary_declarations in
    assert_source_equal ~location_insensitive:true expected actual
  in
  test_list
    [
      (* Vary the module from which TypedDict is imported. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      Movie = mypy_extensions.TypedDict('Movie', {'name': str, 'year': int})
    |}
           {|
      class Movie(TypedDictionary):
        name: str = ...
        year: int = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      Movie = typing_extensions.TypedDict('Movie', {'name': str, 'year': int})
    |}
           {|
      class Movie(TypedDictionary):
        name: str = ...
        year: int = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      Movie = typing.TypedDict('Movie', {'name': str, 'year': int})
    |}
           {|
      class Movie(TypedDictionary):
        name: str = ...
        year: int = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      Movie = mypy_extensions.TypedDict('Movie', {})
    |}
           {|
      class Movie(TypedDictionary): pass
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      Movie = mypy_extensions.TypedDict('Movie', {}, total=False)
    |}
           {|
      class Movie(TypedDictionary, NonTotalTypedDictionary): pass
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      Movie = mypy_extensions.TypedDict('Movie', {}, total=True)
    |}
           {|
      class Movie(TypedDictionary): pass
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      class Movie(mypy_extensions.TypedDict):
        name: str
        year: int
    |}
           {|
      class Movie(TypedDictionary):
        name: str = ...
        year: int = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      class Movie(typing_extensions.TypedDict):
        name: str
        year: int
    |}
           {|
      class Movie(TypedDictionary):
        name: str = ...
        year: int = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      class Movie(typing.TypedDict):
        name: str
        year: int
    |}
           {|
      class Movie(TypedDictionary):
        name: str = ...
        year: int = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      class Derp:
        class Movie(typing.TypedDict):
          name: str
          year: int
    |}
           {|
      class Derp:
        class Movie(TypedDictionary):
          name: str = ...
          year: int = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      if foo:
        class Movie(typing.TypedDict):
          name: str
      else:
        class Movie2(typing.TypedDict):
          year: int
    |}
           {|
      if foo:
        class Movie(TypedDictionary):
          name: str = ...
      else:
        class Movie2(TypedDictionary):
          year: int = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      class Movie(mypy_extensions.TypedDict):
        name: str
        year: int
    |}
           {|
      class Movie(TypedDictionary):
        name: str = ...
        year: int = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      class Movie(mypy_extensions.TypedDict, total=False):
        name: str
        year: int
    |}
           {|
      class Movie(TypedDictionary, NonTotalTypedDictionary):
        name: str = ...
        year: int = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      class Movie(mypy_extensions.TypedDict, total=True):
        name: str
        year: int
    |}
           {|
      class Movie(TypedDictionary):
        name: str = ...
        year: int = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      class Movie(mypy_extensions.TypedDict, total=True):
        """docstring"""
        name: str
        year: int
    |}
           {|
      class Movie(TypedDictionary):
        name: str = ...
        year: int = ...
    |};
      (* Invalid TypedDicts *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      NamelessTypedDict = mypy_extensions.TypedDict({'name': str, 'year': int})
    |}
           {|
      NamelessTypedDict = mypy_extensions.TypedDict({'name': str, 'year': int})
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      TypedDictWithWrongArity = mypy_extensions.TypedDict(A, B, C)
    |}
           {|
      TypedDictWithWrongArity = mypy_extensions.TypedDict(A, B, C)
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      class Movie(mypy_extensions.TypedDict, total=True):
        name: str
        year: int
        def ignored_method(self) -> None: pass
    |}
           {|
      class Movie(TypedDictionary):
        name: str = ...
        year: int = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      class Movie(mypy_extensions.TypedDict, garbage=7):
        name: str
        year: int
    |}
           {|
      class Movie(TypedDictionary, garbage=7):
        name: str = ...
        year: int = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      class Movie(mypy_extensions.TypedDict, OtherClass):
        name: str
        year: int
    |}
           {|
      class Movie(TypedDictionary, OtherClass):
        name: str = ...
        year: int = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      class Movie(OtherClass, mypy_extensions.TypedDict):
        name: str
        year: int
    |}
           {|
      class Movie(TypedDictionary, OtherClass):
        name: str = ...
        year: int = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
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
      (* Ignore initial values for TypedDict field. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      class Movie(mypy_extensions.TypedDict):
        name: str
        year: int = 2020
    |}
           {|
      class Movie(TypedDictionary):
        name: str = ...
        year: int = ...
    |};
    ]


let test_expand_typed_dictionaries__required_not_required =
  let assert_expand ?(handle = "") source expected _ =
    let expected = parse ~handle ~coerce_special_methods:true expected in
    let actual = parse ~handle source |> Preprocessing.expand_typed_dictionary_declarations in
    assert_source_equal ~location_insensitive:true expected actual
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      Movie = typing_extensions.TypedDict('Movie', {'name': str, 'year': typing_extensions.NotRequired[int]})
    |}
           {|
      class Movie(TypedDictionary):
        name: str = ...
        year: typing_extensions.NotRequired[int] = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      Movie = typing_extensions.TypedDict(
        'Movie',
        {'name': typing_extensions.Required[str], 'year': typing_extensions.Required[int]},
      )
    |}
           {|
      class Movie(TypedDictionary):
        name: typing_extensions.Required[str] = ...
        year: typing_extensions.Required[int] = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      Movie = typing_extensions.TypedDict(
        'Movie',
        {'name': str, 'year': typing_extensions.Required[int]},
        total=False,
      )
    |}
           {|
      class Movie(TypedDictionary, NonTotalTypedDictionary):
        name: str = ...
        year: typing_extensions.Required[int] = ...
    |};
    ]


let test_sqlalchemy_declarative_base =
  let assert_expand ?(handle = "") source expected _ =
    let expected = parse ~handle ~coerce_special_methods:true expected in
    let actual = parse ~handle source |> Preprocessing.expand_sqlalchemy_declarative_base in
    assert_source_equal ~location_insensitive:true expected actual
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      Base = sqlalchemy.ext.declarative.declarative_base()
    |}
           {|
      class Base(metaclass=sqlalchemy.ext.declarative.DeclarativeMeta):
        pass
    |};
    ]


let test_transform_ast =
  let assert_expand ?(handle = "qualifier.py") source expected _ =
    let parse source = parse source ~handle |> Preprocessing.expand_implicit_returns in
    assert_source_equal
      ~location_insensitive:true
      (parse expected)
      (Preprocessing.expand_named_tuples (parse source))
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      T = typing.NamedTuple('T', [('one', int), ('two', str)], rename=False)
    |}
           {|
      class T(typing.NamedTuple):
        def __new__(cls, one: int, two: str) -> typing.NamedTuple: ...
        def __init__(self, one: int, two: str) -> None:
         self.one = one
         self.two = two
        _fields: typing.ClassVar[typing.Tuple[str, str]] = ('one', 'two')
        one: typing.Final[int]
        two: typing.Final[str]
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      T = typing.NamedTuple('T', [('one', int), ('two', str)], defaults=(1, 2))
    |}
           {|
      class T(typing.NamedTuple):
        def __new__(cls, one: int = 1, two: str = 2) -> typing.NamedTuple: ...
        def __init__(self, one: int = 1, two: str = 2) -> None:
         self.one = one
         self.two = two
        _fields: typing.ClassVar[typing.Tuple[str, str]] = ('one', 'two')
        one: typing.Final[int] = ...
        two: typing.Final[str] = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      T = typing.NamedTuple('T', [('abc', int), ('def', str), ('abc', int)], rename=True)
    |}
           {|
      class T(typing.NamedTuple):
        def __new__(cls, abc: int, _1: str, _2: int) -> typing.NamedTuple: ...
        def __init__(self, abc: int, _1: str, _2: int) -> None:
         self.abc = abc
         self._1 = _1
         self._2 = _2
        _fields: typing.ClassVar[typing.Tuple[str, str, str]] = ('abc', '_1', '_2')
        abc: typing.Final[int]
        _1: typing.Final[str]
        _2: typing.Final[int]
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      T = collections.namedtuple('T', ['a'])
    |}
           {|
      class T(typing.NamedTuple):
        def __new__(cls, a: typing.Any) -> typing.NamedTuple: ...
        def __init__(self, a: typing.Any) -> None:
          self.a = a
        _fields: typing.ClassVar[typing.Tuple[str]] = ('a',)
        a: typing.Final[typing.Any]
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      T = typing.NamedTuple('T', ['one', 'two'])
    |}
           {|
      class T(typing.NamedTuple):
        def __new__(cls, one: typing.Any, two: typing.Any) -> typing.NamedTuple: ...
        def __init__(self, one: typing.Any, two: typing.Any) -> None:
         self.one = one
         self.two = two
        _fields: typing.ClassVar[typing.Tuple[str, str]] = ('one', 'two')
        one: typing.Final[typing.Any]
        two: typing.Final[typing.Any]
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      T = typing.NamedTuple('T', [('one', int), ('two', str)])
    |}
           {|
      class T(typing.NamedTuple):
        def __new__(cls, one: int, two: str) -> typing.NamedTuple: ...
        def __init__(self, one: int, two: str) -> None:
         self.one = one
         self.two = two
        _fields: typing.ClassVar[typing.Tuple[str, str]] = ('one', 'two')
        one: typing.Final[int]
        two: typing.Final[str]
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
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
        _fields: typing.ClassVar[typing.Tuple[str, str, str]] = ('a', 'b', 'c')
        a: typing.Final[typing.Any]
        b: typing.Final[typing.Any]
        c: typing.Final[typing.Any]
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      T = collections.namedtuple('T', 'a, b, c')
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
        _fields: typing.ClassVar[typing.Tuple[str, str, str]] = ('a', 'b', 'c')
        a: typing.Final[typing.Any]
        b: typing.Final[typing.Any]
        c: typing.Final[typing.Any]
    |};
      (* The purpose of the following tests is to test for multiple consecutive commas and
         whitespaces in the field names parameter. Expected behavior is to strip all the commas and
         whitespace and return a valid result, to match the result returned by Python *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      T = collections.namedtuple('T', 'a,,, b, c')
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
        _fields: typing.ClassVar[typing.Tuple[str, str, str]] = ('a', 'b', 'c')
        a: typing.Final[typing.Any]
        b: typing.Final[typing.Any]
        c: typing.Final[typing.Any]
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      T = collections.namedtuple('T', 'a,,,        b')
    |}
           {|
      class T(typing.NamedTuple):
        def __new__(
          cls,
          a: typing.Any,
          b: typing.Any) -> typing.NamedTuple: ...
        def __init__(self, a: typing.Any, b: typing.Any) -> None:
          self.a = a
          self.b = b
        _fields: typing.ClassVar[typing.Tuple[str, str]] = ('a', 'b')
        a: typing.Final[typing.Any]
        b: typing.Final[typing.Any]
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
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
        _fields: typing.ClassVar[typing.Tuple[str, str]] = ('one', 'two')
        one: typing.Final[typing.Any]
        two: typing.Final[typing.Any]
        three: int = 1
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
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
        _fields: typing.ClassVar[typing.Tuple[str, str, str]] = ('a', 'b', 'c')
        a: typing.Final[int]
        b: typing.Final[str]
        c: typing.Final[int] = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      from typing import TypeVar, NamedTuple, Generic, Tuple, Final

      State = TypeVar("State")

      class LoopState(Generic[State], NamedTuple):
        blah: int
        state: State
    |}
           {|
      from typing import TypeVar, NamedTuple, Generic, Tuple, Final

      State = TypeVar("State")

      class LoopState(Generic[State], NamedTuple):
        def __new__(cls, blah: int, state: State) -> typing.NamedTuple: ...
        def __init__(self, blah: int, state: State) -> None:
          self.blah = blah
          self.state = state
        _fields: typing.ClassVar[typing.Tuple[str, str]] = ('blah', 'state')
        blah: typing.Final[int]
        state: typing.Final[State]
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      from typing import TypeVar, NamedTuple, Generic, Tuple, Final

      State = TypeVar("State")

      class LoopState(NamedTuple, Generic[State]):
        blah: int
        state: State
    |}
           {|
      from typing import TypeVar, NamedTuple, Generic, Tuple, Final

      State = TypeVar("State")

      class LoopState(NamedTuple, Generic[State]):
        def __new__(cls, blah: int, state: State) -> typing.NamedTuple: ...
        def __init__(self, blah: int, state: State) -> None:
          self.blah = blah
          self.state = state
        _fields: typing.ClassVar[typing.Tuple[str, str]] = ('blah', 'state')
        blah: typing.Final[int]
        state: typing.Final[State]
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
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
         _fields: typing.ClassVar[typing.Tuple[str, str, str, str, str]] = ('op', 'path', 'value', 'ts', 'lazy')
         op: typing.Final[typing.Any]
         path: typing.Final[typing.Any]
         value: typing.Final[typing.Any]
         ts: typing.Final[typing.Any]
         lazy: typing.Final[typing.Any]
         pass
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
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
          _fields: typing.ClassVar[typing.Tuple[str, str]] = ('a', 'b')
          a: typing.Final[typing.Any]
          b: typing.Final[typing.Any]
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      def foo():
        T = typing.NamedTuple('T')
    |}
           {|
      def foo():
        class T(typing.NamedTuple):
          def __new__(cls) -> typing.NamedTuple: ...
          def __init__(self) -> None: ...
          _fields: typing.ClassVar[typing.Tuple[()]] = ()
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      class Foo(collections.namedtuple('T', ['one', 'two'])):
        def __new__(cls, one) -> typing.NamedTuple:
          return super(Foo, cls).__new__(cls, one, two=0)
    |}
           {|
      class Foo(typing.NamedTuple):
        _fields: typing.ClassVar[typing.Tuple[str, str]] = ('one', 'two')
        one: typing.Final[typing.Any]
        two: typing.Final[typing.Any]
        def __new__(cls, one) -> typing.NamedTuple:
          return super(Foo, cls).__new__(cls, one, two=0)
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      T = typing.NamedTuple('T', one=int, two=str)
    |}
           {|
      class T(typing.NamedTuple):
        def __new__(cls, one: int, two: str) -> typing.NamedTuple: ...
        def __init__(self, one: int, two: str) -> None:
          self.one = one
          self.two = two
        _fields: typing.ClassVar[typing.Tuple[str, str]] = ('one', 'two')
        one: typing.Final[int]
        two: typing.Final[str]
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      T = typing.NamedTuple('T', one=int, two=str, rename=True)
    |}
           {|
      class T(typing.NamedTuple):
        def __new__(cls, one: int, two: str) -> typing.NamedTuple: ...
        def __init__(self, one: int, two: str) -> None:
          self.one = one
          self.two = two
        _fields: typing.ClassVar[typing.Tuple[str, str]] = ('one', 'two')
        one: typing.Final[int]
        two: typing.Final[str]
    |};
    ]


let test_populate_captures =
  let assert_captures ~expected source_text _ =
    let source = Test.parse ~handle:"test.py" source_text |> Preprocessing.populate_captures in
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
        Map.set sofar ~key:name ~data:captures
      in
      List.fold defines ~init:Reference.Map.empty ~f:build_capture_map
    in
    let assert_captures_ name expected =
      let expected = List.map expected ~f:(fun (name, kind) -> { Define.Capture.name; kind }) in
      let actual = Map.find capture_map name |> Option.value ~default:[] in
      assert_equal
        ~cmp:[%compare.equal: Define.Capture.t list]
        ~printer:(fun captures -> Sexp.to_string_hum [%message (captures : Define.Capture.t list)])
        expected
        actual
    in
    List.iter expected ~f:(fun (name, captures) -> assert_captures_ name captures)
  in
  let tuple_annotation value_annotation start stop =
    Node.create
      (Expression.Subscript
         {
           Subscript.base =
             Node.create
               (Expression.Name
                  (create_name
                     ~location:(location start stop)
                     ~create_origin:(fun _ -> None)
                     "typing.Tuple"))
               ~location:(location start stop);
           index =
             Node.create
               ~location:(location start stop)
               (Expression.Tuple
                  [
                    value_annotation;
                    Node.create
                      ~location:(location start stop)
                      (Expression.Constant Constant.Ellipsis);
                  ]);
           origin = None;
         })
      ~location:(location start stop)
  in
  let dict_annotation value_annotation start stop =
    Node.create
      (Expression.Subscript
         {
           base =
             Node.create
               (Expression.Name
                  (create_name
                     ~location:(location start stop)
                     ~create_origin:(fun _ -> None)
                     "typing.Dict"))
               ~location:(location start stop);
           index =
             Node.create
               ~location:(location start stop)
               (Expression.Tuple
                  [
                    Node.create
                      (Expression.Name
                         (create_name
                            ~location:(location start stop)
                            ~create_origin:(fun _ -> None)
                            "str"))
                      ~location:(location start stop);
                    value_annotation;
                  ]);
           origin = None;
         })
      ~location:(location start stop)
  in
  let int_annotation start stop =
    Node.create (Expression.Name (Name.Identifier "int")) ~location:(location start stop)
  in
  let any_annotation start stop =
    Node.create
      (Expression.Name
         (create_name ~location:(location start stop) ~create_origin:(fun _ -> None) "typing.Any"))
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
  let parameter_specification_annotation attribute (start, stop) =
    Node.create
      ~location:(location start stop)
      (Expression.Name
         (Name.Attribute
            {
              base =
                (let stop =
                   let line, column = start in
                   line, column + 1
                 in
                 Node.create ~location:(location start stop) (Expression.Name (Name.Identifier "P")));
              attribute;
              origin = None;
            }))
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo():
       pass
  |}
           ~expected:[!&"foo", []; !&"test.$toplevel", []];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int):
       def bar():
         y = x
  |}
           ~expected:[!&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14)))]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
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
                         Define.Signature.name = !&"bar";
                         parameters =
                           [
                             Node.create
                               {
                                 Parameter.name = "y";
                                 value = None;
                                 annotation = Some (int_annotation (3, 13) (3, 16));
                               }
                               ~location:(location (3, 10) (3, 16));
                           ];
                         decorators = [];
                         return_annotation = Some (int_annotation (3, 21) (3, 24));
                         async = false;
                         generator = false;
                         parent =
                           NestingContext.(create_function ~parent:(create_toplevel ()) "foo");
                         legacy_parent = None;
                         type_params = [];
                       } );
                 ] );
             ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int):
       def bar():
         nonlocal x
         x = 1
  |}
           ~expected:[!&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14)))]];
      (* x in `bar` will shadow x in `foo` *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int):
       def bar():
         x = 1
  |}
           ~expected:[!&"bar", []];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int):
       def bar():
         print(x)
         x = 2
  |}
           ~expected:[!&"bar", []];
      (* Do not capture global variables *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     x: int = 1
     def foo():
       def bar():
         x = 1
  |}
           ~expected:[!&"bar", []];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     x: int = 1
     def foo():
       def bar():
         global x
         x = 1
  |}
           ~expected:[!&"bar", []];
      (* Do not capture unused bindings *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int):
       def bar():
         pass
  |}
           ~expected:[!&"foo", []; !&"bar", []];
      (* Do not capture imported names *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo():
       import bar
       def baz():
         return bar.derp()
  |}
           ~expected:[!&"bar", []];
      (* Nesting functions should be correctly detected *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int):
       y = 1
       def bar():
         assert (x == y)
  |}
           ~expected:
             [
               ( !&"bar",
                 ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None] );
             ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int):
       y = 1
       def bar():
         x
  |}
           ~expected:[!&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14)))]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int):
       y = 1
       def bar(z: int = y):
         x
  |}
           ~expected:[!&"bar", ["x", Annotation (Some (int_annotation (2, 11) (2, 14)))]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int):
       y = 1
       def bar():
        def baz(z: int = y):
            x
  |}
           ~expected:[!&"bar", ["y", Annotation None]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int):
       y = 1
       def bar():
         yield x
         return y
  |}
           ~expected:
             [
               ( !&"bar",
                 ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None] );
             ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int):
       y = 1
       def bar():
         for i in range(x):
           print(y)
  |}
           ~expected:
             [
               ( !&"bar",
                 ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None] );
             ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
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
             [
               ( !&"bar",
                 ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None] );
             ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int):
       y = 1
       def bar():
         while x > 1:
           print(y)
  |}
           ~expected:
             [
               ( !&"bar",
                 ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None] );
             ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int):
       y = 1
       def bar():
         raise ValueError(x + y)
  |}
           ~expected:
             [
               ( !&"bar",
                 ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None] );
             ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
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
             [
               ( !&"bar",
                 ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None] );
             ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int):
       y = 1
       def bar():
         with open('test.txt', 'r' if x > 1 else 'w') as f:
           print(y)
  |}
           ~expected:
             [
               ( !&"bar",
                 ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None] );
             ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int):
       y = 1
       async def bar():
         await baz(x, y)
  |}
           ~expected:
             [
               ( !&"bar",
                 ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None] );
             ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int):
       y = 1
       def bar():
         return [x, y]
  |}
           ~expected:
             [
               ( !&"bar",
                 ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None] );
             ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int):
       y = 1
       def bar():
         return x, y
  |}
           ~expected:
             [
               ( !&"bar",
                 ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None] );
             ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int):
       y = 1
       def bar():
         return {x, y}
  |}
           ~expected:
             [
               ( !&"bar",
                 ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None] );
             ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int):
       y = 1
       def bar():
         return {x: y}
  |}
           ~expected:
             [
               ( !&"bar",
                 ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None] );
             ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int):
       y = 1
       def bar():
         return f"x = {x}, y = {y}"
  |}
           ~expected:
             [
               ( !&"bar",
                 ["x", Annotation (Some (int_annotation (2, 11) (2, 14))); "y", Annotation None] );
             ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int):
       y = 1
       def bar():
         return (lambda x: x + y)
  |}
           ~expected:[!&"bar", ["y", Annotation None]];
      (* Comprehension bounds are excluded *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int):
       y = 1
       def bar():
         return [x for x in range(y) if x < 9]
  |}
           ~expected:[!&"bar", ["y", Annotation None]];
      (* Capture *args *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo( *args):
       def bar():
         return args[0]
  |}
           ~expected:[!&"bar", ["args", Annotation (Some (tuple_any_annotation (2, 10) (2, 14)))]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo( *args: int):
       def bar():
         return args[0]
  |}
           ~expected:
             [
               ( !&"bar",
                 [
                   ( "args",
                     Annotation (Some (tuple_int_annotation ((2, 10), (2, 19)) ((2, 16), (2, 19))))
                   );
                 ] );
             ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo( *derp: int):
       def bar():
         return derp[0]
  |}
           ~expected:
             [
               ( !&"bar",
                 [
                   ( "derp",
                     Annotation (Some (tuple_int_annotation ((2, 10), (2, 19)) ((2, 16), (2, 19))))
                   );
                 ] );
             ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo( *args: int):
       def bar( *args: str):
         return args[0]
  |}
           ~expected:[!&"bar", []];
      (* Capture **kwargs *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo( **kwargs):
       def bar():
         return kwargs["derp"]
  |}
           ~expected:
             [!&"bar", ["kwargs", Annotation (Some (dict_any_annotation ((2, 11), (2, 17))))]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo( **kwargs: int):
       def bar():
         return kwargs["derp"]
  |}
           ~expected:
             [
               ( !&"bar",
                 [
                   ( "kwargs",
                     Annotation (Some (dict_int_annotation ((2, 11), (2, 22)) ((2, 19), (2, 22)))) );
                 ] );
             ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo( **durp):
       def bar():
         return durp["derp"]
  |}
           ~expected:[!&"bar", ["durp", Annotation (Some (dict_any_annotation ((2, 11), (2, 15))))]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo( **kwargs: int):
       def bar( **kwargs: str):
         return kwargs["derp"]
  |}
           ~expected:[!&"bar", []];
      (* ParamSpecs should be treated specially. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo( *args: P.args, **kwargs: P.kwargs):
       def bar():
         return f( *args, **kwargs)
  |}
           ~expected:
             [
               ( !&"bar",
                 [
                   ( "args",
                     Annotation
                       (Some (parameter_specification_annotation "args" ((2, 16), (2, 22)))) );
                   ( "kwargs",
                     Annotation
                       (Some (parameter_specification_annotation "kwargs" ((2, 34), (2, 42)))) );
                 ] );
             ];
      (* Capture self *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     class Foo:
       x: int
       def foo(self) -> None:
         def bar() -> int:
           return self.x
  |}
           ~expected:[!&"bar", ["self", Self !&"Foo"]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     class Foo:
       x: int
       def foo(self) -> None:
         def bar(self) -> int:
           return self.x
  |}
           ~expected:[!&"bar", []];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     class Foo:
       x: int
       def foo(this) -> None:
         def bar() -> int:
           return this.x
  |}
           ~expected:[!&"bar", ["this", Self !&"Foo"]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     class Foo:
       x: int
       @staticmethod
       def foo(self) -> None:
         def bar() -> int:
           return self.x
  |}
           ~expected:[!&"bar", ["self", Annotation None]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     class Foo:
       x: int
       @classmethod
       def foo(cls) -> None:
         def bar() -> "Foo":
           return cls
  |}
           ~expected:[!&"bar", ["cls", ClassSelf !&"Foo"]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     class Foo:
       x: int
       @classmethod
       def foo(cls) -> None:
         def bar(cls):
           return cls
  |}
           ~expected:[!&"bar", []];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     class Foo:
       x: int
       @classmethod
       def foo(clazz) -> None:
         def bar() -> "Foo":
           return clazz
  |}
           ~expected:[!&"bar", ["clazz", ClassSelf !&"Foo"]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(decorator) -> None:
       def bar() -> None:
         @decorator # This decorator should refer to the argument of foo
         def baz(decorator) -> None:
           pass
    |}
           ~expected:[!&"bar", ["decorator", Annotation None]; !&"baz", []];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def decorator(func): ...
     def foo(decorator) -> None:
       def bar() -> None:
         @decorator # This decorator should refer to the argument of foo
         def baz(decorator) -> None:
           pass
    |}
           ~expected:[!&"bar", ["decorator", Annotation None]; !&"baz", []];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def decorator(func): ...
     def foo() -> None:
       def bar() -> None:
         @decorator # This decorator should refer to the global foo
         def baz(decorator) -> None:
           pass
    |}
           ~expected:[!&"bar", []; !&"baz", []];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
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
                         Define.Signature.name = !&"decorator";
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
                         parent =
                           NestingContext.(create_function ~parent:(create_toplevel ()) "foo");
                         legacy_parent = None;
                         type_params = [];
                       } );
                 ] );
               !&"baz", [];
             ];
      (* TODO(T170814146): Missing capture for variable initialized after define *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_captures
           {|
     def foo(x: int) -> None:
       def bar() -> int:
         return y
       y = 1
    |}
           ~expected:[];
    ]


let test_populate_unbound_names =
  let assert_unbound_names ~expected source_text _ =
    let source = Test.parse ~handle:"test.py" source_text |> Preprocessing.populate_unbound_names in
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
        Map.set sofar ~key:name ~data:unbound_names
      in
      List.fold defines ~init:Reference.Map.empty ~f:build_unbound_map
    in
    let assert_unbound_names_ name expected =
      let expected =
        List.map expected ~f:(fun (name, location) -> { Define.NameAccess.name; location })
      in
      let actual = Map.find unbound_map name |> Option.value ~default:[] in
      assert_equal
        ~cmp:[%compare.equal: Define.NameAccess.t list]
        ~printer:(fun unbound_names ->
          Sexp.to_string_hum [%message (unbound_names : Define.NameAccess.t list)])
        expected
        actual
    in
    List.iter expected ~f:(fun (name, unbound_names) -> assert_unbound_names_ name unbound_names)
  in
  let toplevel_name = !&"test.$toplevel" in
  let class_foo_toplevel_name = !&"test.Foo.$class_toplevel" in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names "derp" ~expected:[toplevel_name, ["derp", location (1, 0) (1, 4)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
       x = 42
       y = x + z
    |}
           ~expected:[toplevel_name, ["z", location (3, 8) (3, 9)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo() -> None:
        derp
    |}
           ~expected:[!&"foo", ["derp", location (3, 2) (3, 6)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo(derp: int) -> None:
        derp
    |}
           ~expected:[!&"foo", []];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo() -> None:
        (x := derp)
    |}
           ~expected:[!&"foo", ["derp", location (3, 8) (3, 12)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      import derp
      def foo() -> None:
        derp
    |}
           ~expected:[!&"foo", []];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      import chocobo.river
      def foo() -> None:
        chocobo
    |}
           ~expected:[!&"foo", []];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo(x: int) -> None:
        bar(x)
    |}
           ~expected:[!&"foo", ["bar", location (3, 2) (3, 5)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def bar() -> None: ...
      def foo(x: int) -> None:
        bar(x)
    |}
           ~expected:[!&"foo", []];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      from some_module import bar
      def foo() -> None:
        bar(x=1, y=z)
    |}
           ~expected:[!&"foo", ["z", location (4, 13) (4, 14)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo():
        f = (lambda x: x)
        g = (lambda *y: y)
        h = (lambda **z: z)
        return f or g or h
    |}
           ~expected:[!&"foo", []];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo():
        f = (lambda x: x + y)
        return f
    |}
           ~expected:[!&"foo", ["y", location (3, 21) (3, 22)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo():
        return [(x, y, z) for x, *y in [[1], [2,3]]]
    |}
           ~expected:[!&"foo", ["z", location (3, 17) (3, 18)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
       class A:
         def foo() -> None:
           self.bar()
    |}
           ~expected:[!&"foo", ["self", location (4, 4) (4, 8)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
       class A:
         def foo(self) -> None:
           self.bar()
    |}
           ~expected:[!&"foo", []];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
       class A:
         @staticmethod
         def foo() -> None:
           A.bar()
    |}
           ~expected:[!&"foo", []];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo() -> None:
        def bar(x) -> int:
          return x + baz
        x = bar(qux)
        y = bar(x)
    |}
           ~expected:
             [
               !&"foo", ["qux", location (5, 10) (5, 13)]; !&"bar", ["baz", location (4, 15) (4, 18)];
             ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo(x: int):
        reveal_type(x)
      pyre_dump()
    |}
           ~expected:[!&"foo", []; toplevel_name, []];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo():
        y = int("42")
        return [x for x in range(y)]
    |}
           ~expected:[!&"foo", []];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo():
        return [y for x in range(42)]
    |}
           ~expected:[!&"foo", ["y", location (3, 10) (3, 11)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo():
        return [y for x, y in range(42)]
    |}
           ~expected:[!&"foo", []];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo():
        return [y for x in range(42) for y in x]
    |}
           ~expected:[!&"foo", []];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo():
        return [y for x in range(42) if y > 0 for y in x]
    |}
           ~expected:[!&"foo", ["y", location (3, 34) (3, 35)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo():
        return [y for x in range(42) if x > 0 for y in x if x > 0 and y > 0]
    |}
           ~expected:[!&"foo", []];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo():
        return [x for x in x]
    |}
           ~expected:[!&"foo", ["x", location (3, 21) (3, 22)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo():
        return [y for x in x for y in x]
    |}
           ~expected:[!&"foo", ["x", location (3, 21) (3, 22)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo():
        return [y for x in y for y in x]
    |}
           ~expected:[!&"foo", ["y", location (3, 21) (3, 22)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo():
        match x:
            case base.attribute | Derp() if y:
                return z
    |}
           ~expected:
             [
               ( !&"foo",
                 [
                   "Derp", location (4, 28) (4, 32);
                   "base", location (4, 11) (4, 15);
                   "x", location (3, 8) (3, 9);
                   "y", location (4, 38) (4, 39);
                   "z", location (5, 17) (5, 18);
                 ] );
             ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo() -> Derp:
        pass
    |}
           ~expected:[toplevel_name, ["Derp", location (2, 13) (2, 17)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo(d: Derp) -> None:
        pass
    |}
           ~expected:[toplevel_name, ["Derp", location (2, 11) (2, 15)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo(d: int = derp()) -> None:
        pass
    |}
           ~expected:[toplevel_name, ["derp", location (2, 17) (2, 21)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      from some_module import derp
      def foo() -> None:
        x: Derp = derp()
    |}
           ~expected:[!&"foo", ["Derp", location (4, 5) (4, 9)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo() -> None:
        @derp
        def bar() -> None:
          pass
    |}
           ~expected:[!&"foo", ["derp", location (3, 3) (3, 7)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      class Foo(Derp):
        def foo(self) -> None:
          pass
    |}
           ~expected:[toplevel_name, ["Derp", location (2, 10) (2, 14)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      class Foo:
        class Baz(Bar):
          pass
    |}
           ~expected:[class_foo_toplevel_name, ["Bar", location (3, 12) (3, 15)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      class Foo:
        class Bar:
          pass
        class Baz(Bar):
          pass
    |}
           ~expected:[class_foo_toplevel_name, []];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      class Foo:
        @derp
        def foo(self) -> None:
          pass
    |}
           ~expected:[class_foo_toplevel_name, ["derp", location (3, 3) (3, 7)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
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
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      #!/usr/bin/env python2
      def foo() -> None:
        derp
    |}
           ~expected:[!&"foo", ["derp", location (4, 2) (4, 6)]];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      #!/usr/bin/env python2
      def foo(): # type: (...) -> List[derp]
        pass
    |}
           ~expected:
             [toplevel_name, ["List", location (3, 0) (4, 2); "derp", location (3, 0) (4, 2)]];
      (* TODO(T80454071): This should raise an error about `nonexistent_inside_quotes`. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      from typing import Optional
      def foo(
        self,
        request: "nonexistent_inside_quotes.Foo",
        request2: nonexistent_outside_quotes.Foo,
      ) -> None: ...
    |}
           ~expected:[toplevel_name, ["nonexistent_outside_quotes", location (6, 12) (6, 38)]];
      (* Recursive alias reference should not be considered unbound. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
       from typing import Tuple, Union
       Tree = Union[int, Tuple["Tree", "Tree"]]
    |}
           ~expected:[];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_unbound_names
           {|
      def foo() -> str:
        return __path__
    |}
           ~expected:[!&"foo", []];
    ]


let test_union_shorthand =
  let assert_replace ?(handle = "test.py") source expected _ =
    let expected = parse ~handle ~coerce_special_methods:true expected |> Preprocessing.qualify in
    let actual =
      parse ~handle source |> Preprocessing.qualify |> Preprocessing.replace_union_shorthand
    in
    assert_source_equal ~location_insensitive:true expected actual
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace {|
    x: int | str = 1
  |} {|
    x: typing.Union[int, str] = 1
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
    x: int | str | bool = True
  |}
           {|
    x: typing.Union[int, str, bool] = True
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
      def foo() -> int | str:
        pass
    |}
           {|
      def foo() -> typing.Union[int, str]:
        pass
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
      class A:
        x: bool | typing.List[int] = False
    |}
           {|
      class A:
        x: typing.Union[bool, typing.List[int]] = False
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
      def bar(x: int | str) -> None:
        pass
    |}
           {|
      def bar(x: typing.Union[int, str]) -> None:
        pass
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
      def bar(x: typing.List[int | str]) -> None:
        pass
    |}
           {|
      def bar(x: typing.List[typing.Union[int, str]]) -> None:
        pass
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
      def bar(x: list[int | str]) -> None:
        pass
    |}
           {|
      def bar(x: list[typing.Union[int, str]]) -> None:
        pass
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
      def bar(x: dict[int, int | str]) -> None:
        pass
    |}
           {|
      def bar(x: dict[int, typing.Union[int, str]]) -> None:
        pass
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
      def bar(x: dict[int, list[int | str]]) -> None:
        pass
    |}
           {|
      def bar(x: dict[int, list[typing.Union[int, str]]]) -> None:
        pass
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
      def bar(x: dict[int, list[int | list[str | bool]]]) -> None:
        pass
    |}
           {|
      def bar(x: dict[int, list[typing.Union[int, list[typing.Union[str, bool]]]]]) -> None:
        pass

    |};
      labeled_test_case __FUNCTION__ __LINE__ @@ assert_replace {| 1 | 2 |} {|
     1 | 2
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
    isinstance(x, int | str)
  |}
           {|
    isinstance(x, typing.Union[int, str])
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
    isinstance(x, typing.List[int | str])
  |}
           {|
    isinstance(x, typing.List[typing.Union[int, str]])
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
    isinstance(x, int | str) and isinstance(x, bool | float)
  |}
           {|
    isinstance(x, typing.Union[int, str]) and isinstance(x, typing.Union[bool, float])
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
      issubclass(int, int | float | int)
    |}
           {|
      issubclass(int, typing.Union[int, float, int])
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
      issubclass(int, int | (float | int))
    |}
           {|
      issubclass(int, typing.Union[int, float, int])
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
      issubclass(dict, float | str)
    |}
           {|
      issubclass(dict, typing.Union[float, str])
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
      def foo(x: typing.Callable[[int | str, str], int]) -> int:
        ...

    |}
           {|
      def foo(x: typing.Callable[[typing.Union[int, str], str], int]) -> int:
        ...
    |};
    ]


let test_mangle_private_attributes =
  let assert_replace ?(handle = "test.py") source expected _ =
    let expected = parse ~handle ~coerce_special_methods:true expected in
    let actual = parse ~handle source |> Preprocessing.mangle_private_attributes in
    assert_source_equal ~location_insensitive:true expected actual
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
      class Foo:
        non_private = 1
        __private = 1
    |}
           {|
      class Foo:
        non_private = 1
        _Foo__private = 1
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
      class Foo:
        def _non_private(self) -> None: pass
        def __private(self) -> None: pass
        @staticmethod
        def __private_static(self) -> None: pass

        def __init__(self) -> None:
          x = __private_name  # arbitrary reference
          y = self.__private()

      class Bar(Foo):
        def __init__(self) -> None:
          x = self.__private()
    |}
           {|
      class Foo:
        def _non_private(self) -> None: pass
        def _Foo__private(self) -> None: pass
        @staticmethod
        def _Foo__private_static(self) -> None: pass

        def __init__(self) -> None:
          x = _Foo__private_name  # arbitrary reference
          y = self._Foo__private()

      class Bar(Foo):
        def __init__(self) -> None:
          x = self._Bar__private()
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
      class A:
        class B:
          def __foo(self):
            self.__bar
        __private = 1
    |}
           {|
      class A:
        class B:
          def _B__foo(self):
            self._B__bar
        _A__private = 1
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
      class __A:
        __private = 1
      class _B:
        __private = 1
    |}
           {|
      class __A:
        _A__private = 1
      class _B:
        _B__private = 1
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
      class A:
        class __B:
          def __foo(self):
            return
    |}
           {|
      class A:
        class _A__B:
          def _B__foo(self):
            return
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
      def foo():
        class C:
          def __bar(self):
            pass
    |}
           {|
      def foo():
        class C:
          def _C__bar(self):
            pass
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
      class Foo:
        x = 1
        __y = 1
        def test(self) -> str:
          return f"{self.x}" + f"{self.__y}" + "{self.__y}"
    |}
           {|
      class Foo:
        x = 1
        _Foo__y = 1
        def test(self) -> str:
          return f"{self.x}" + f"{self._Foo__y}" + "{self.__y}"
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
      class __A:
        pass
      class __B(__A):
        def __bar(self):
          pass
    |}
           {|
      class __A:
        pass
      class __B(__A):
        def _B__bar(self):
          pass
    |};
    ]


let test_six_metaclass_decorator =
  let assert_replace ?(handle = "test.py") source expected _ =
    let expected = parse ~handle ~coerce_special_methods:true expected in
    let actual = parse ~handle source |> Preprocessing.inline_six_metaclass in
    assert_source_equal ~location_insensitive:true expected actual
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
    import six

    class FooMetaclass(type): ...
    class Base1: ...
    class Base2: ...

    @six.add_metaclass(FooMetaclass)
    class Make(Base1, Base2):
      existent: int = 1
  |}
           {|
    import six

    class FooMetaclass(type): ...
    class Base1: ...
    class Base2: ...

    class Make(Base1, Base2, metaclass=FooMetaclass):
      existent: int = 1
  |};
      (* Leave class unchanged if there are too many arguments to add_metaclass. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
    import six

    class FooMetaclass(type): ...
    class FooMetaclass2(type): ...
    class Base1: ...
    class Base2: ...

    @six.add_metaclass(FooMetaclass, FooMetaclass2)
    class Make(Base1, Base2):
      existent: int = 1
  |}
           {|
    import six

    class FooMetaclass(type): ...
    class FooMetaclass2(type): ...
    class Base1: ...
    class Base2: ...

    @six.add_metaclass(FooMetaclass, FooMetaclass2)
    class Make(Base1, Base2):
      existent: int = 1
  |};
      (* Leave class unchanged if the argument is not a class. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_replace
           {|
    import six

    class Base1: ...
    class Base2: ...

    def foo() -> object: ...

    @six.add_metaclass(foo())
    class Make(Base1, Base2):
      existent: int = 1
  |}
           {|
    import six

    class Base1: ...
    class Base2: ...

    def foo() -> object: ...

    @six.add_metaclass(foo())
    class Make(Base1, Base2):
      existent: int = 1
  |};
    ]


let test_expand_import_python_calls =
  let assert_expand ?(handle = "test.py") source expected _ =
    let expected = parse ~handle ~coerce_special_methods:true expected in
    let actual = parse ~handle source |> Preprocessing.expand_import_python_calls in
    assert_source_equal ~location_insensitive:true expected actual
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand {|
     import_python("a", "*")
  |} {|
    from a import *
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      import_python("cubism/shared.cinc", "*")
    |}
           {|
      from cubism.shared.cinc import *
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      import_python("cubism/shared.cinc")
    |}
           {|
      import cubism.shared.cinc
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      import_thrift("cubism/shared.thrift")
    |}
           {|
      import cubism.shared.thrift
    |};
    ]


let test_expand_pytorch_register_buffer =
  let assert_expand ?(handle = "test.py") source expected _ =
    let expected = parse ~handle expected in
    let actual = parse ~handle source |> Preprocessing.expand_pytorch_register_buffer in
    assert_source_equal ~location_insensitive:true expected actual
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      import torch
      import torch.nn as nn

      class Foo(nn.Module):
        def __init__(self) -> None:
          super(Foo, self).__init__()
          self.register_buffer("foo", torch.zeros(10, 20))
          self.register_buffer("foo_persistent", torch.zeros(10, 20), persistent=False)
          self.register_buffer("bar", None)

          self.register_buffer(not_a_literal(), torch.zeros(10, 20))

        def some_method() -> None:
          self.register_buffer("foobar", torch.zeros(30, 40))
    |}
           {|
      import torch
      import torch.nn as nn

      class Foo(nn.Module):
        def __init__(self) -> None:
          super(Foo, self).__init__()
          self.foo: torch.Tensor = torch.zeros(10, 20)
          self.foo_persistent: torch.Tensor = torch.zeros(10, 20)
          self.bar = None

          self.register_buffer(not_a_literal(), torch.zeros(10, 20))

        def some_method() -> None:
          self.register_buffer("foobar", torch.zeros(30, 40))
    |};
      (* TODO(T80453653): We shouldn't preprocess in non-Module constructors. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      import torch
      import torch.nn as nn

      class NotAModule:
        def __init__(self) -> None:
          super(NotAModule, self).__init__()
          self.register_buffer("foo", torch.zeros(10, 20))
    |}
           {|
      import torch
      import torch.nn as nn

      class NotAModule:
        def __init__(self) -> None:
          super(NotAModule, self).__init__()
          self.foo: torch.Tensor = torch.zeros(10, 20)
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      import torch
      import torch.nn as nn

      class Outer:
        class Foo(nn.Module):
          def __init__(self) -> None:
            super(Foo, self).__init__()
            self.register_buffer("foo", torch.zeros(10, 20))
            self.register_buffer("foo_persistent", torch.zeros(10, 20), persistent=False)
            self.register_buffer("bar", None)

            self.register_buffer(not_a_literal(), torch.zeros(10, 20))

          def some_method() -> None:
            self.register_buffer("foobar", torch.zeros(30, 40))
    |}
           {|
      import torch
      import torch.nn as nn

      class Outer:
        class Foo(nn.Module):
          def __init__(self) -> None:
            super(Foo, self).__init__()
            self.foo: torch.Tensor = torch.zeros(10, 20)
            self.foo_persistent: torch.Tensor = torch.zeros(10, 20)
            self.bar = None

            self.register_buffer(not_a_literal(), torch.zeros(10, 20))

          def some_method() -> None:
            self.register_buffer("foobar", torch.zeros(30, 40))
    |};
    ]


let test_expand_self_type =
  let assert_expand ?(handle = "test.py") source expected _ =
    let expected = parse ~handle expected in
    let actual = parse ~handle source |> Preprocessing.SelfType.expand_self_type in
    assert_source_equal ~location_insensitive:true expected actual
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      from typing_extensions import Self

      class Shape:
        def __init__(self, scale: float = 0.0) -> None:
          self.scale = scale

        def set_scale(self, scale: float) -> Self:
          self.scale = scale
          return self
  |}
           {|
      _Self_test_Shape__ = typing.TypeVar("_Self_test_Shape__", bound=Shape)

      from typing_extensions import Self

      class Shape:
        def __init__(self, scale: float = 0.0) -> None:
          self.scale = scale

        def set_scale(self: _Self_test_Shape__, scale: float) -> _Self_test_Shape__:
          self.scale = scale
          return self
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      class NoSelf:
        def some_method(self, scale: float) -> None:
          pass
  |}
           {|
      class NoSelf:
        def some_method(self, scale: float) -> None:
          pass
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      def some_function(scale: float) -> None:
        pass
  |}
           {|
      def some_function(scale: float) -> None:
        pass
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      from typing_extensions import Self

      class Outer:
        class Inner:
          def set_scale(self, scale: float) -> Self: ...
  |}
           {|
      _Self_test_Outer_Inner__ = typing.TypeVar("_Self_test_Outer_Inner__", bound=Outer.Inner)

      from typing_extensions import Self

      class Outer:
        class Inner:
          def set_scale(self: _Self_test_Outer_Inner__, scale: float) -> _Self_test_Outer_Inner__: ...
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      from typing_extensions import Self

      class Base:
        def set_scale(self, scale: float) -> Self: ...

      class Circle(Base):
        def set_radius(self, scale: float) -> Self: ...
  |}
           {|
      _Self_test_Base__ = typing.TypeVar("_Self_test_Base__", bound=Base)
      _Self_test_Circle__ = typing.TypeVar("_Self_test_Circle__", bound=Circle)

      from typing_extensions import Self

      class Base:
        def set_scale(self: _Self_test_Base__, scale: float) -> _Self_test_Base__: ...

      class Circle(Base):
        def set_radius(self: _Self_test_Circle__, scale: float) -> _Self_test_Circle__: ...
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      from typing_extensions import Self

      class Shape:
        def __init__(self, scale: int) -> None: ...

        @classmethod
        def from_config(cls, config: dict[str, float]) -> Self:
          return cls(config["scale"])
  |}
           {|
      _Self_test_Shape__ = typing.TypeVar("_Self_test_Shape__", bound=Shape)

      from typing_extensions import Self

      class Shape:
        def __init__(self, scale: int) -> None: ...

        @classmethod
        def from_config(
          cls: typing.Type[_Self_test_Shape__],
          config: dict[str, float]
        ) -> _Self_test_Shape__:
          return cls(config["scale"])
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      from typing_extensions import Self

      class Merger:
        def merge(self, other: Self) -> Self:
          return self

        def merge_with_new_name(self, other: Self, name: str) -> Self:
          return self

        def merge_into(self, other: Self) -> None:
          return self
     |}
           {|
      _Self_test_Merger__ = typing.TypeVar("_Self_test_Merger__", bound=Merger)

      from typing_extensions import Self

      class Merger:
        def merge(self: _Self_test_Merger__, other: _Self_test_Merger__) -> _Self_test_Merger__:
          return self

        def merge_with_new_name(self: _Self_test_Merger__, other: _Self_test_Merger__, name: str) -> _Self_test_Merger__:
          return self

        def merge_into(self: _Self_test_Merger__, other: _Self_test_Merger__) -> None:
          return self
     |};
      (* PyreReadOnly method with `typing_extensions.Self`. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      from typing_extensions import Self
      from pyre_extensions import PyreReadOnly

      class Foo:
        def readonly_method(self: PyreReadOnly[Self]) -> None: ...
      |}
           {|
      _Self_test_Foo__ = typing.TypeVar("_Self_test_Foo__", bound=Foo)

      from typing_extensions import Self
      from pyre_extensions import PyreReadOnly

      class Foo:
        def readonly_method(self: pyre_extensions.PyreReadOnly[_Self_test_Foo__]) -> None: ...
      |};
      (* PyreReadOnly method with `typing.Self`. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      from typing import Self
      from pyre_extensions import PyreReadOnly

      class Foo:
        def readonly_method(self: PyreReadOnly[Self]) -> None: ...
      |}
           {|
      _Self_test_Foo__ = typing.TypeVar("_Self_test_Foo__", bound=Foo)

      from typing import Self
      from pyre_extensions import PyreReadOnly

      class Foo:
        def readonly_method(self: pyre_extensions.PyreReadOnly[_Self_test_Foo__]) -> None: ...
      |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           ~handle:"typing.pyi"
           {|
      Self: _SpecialForm
      class _PyreReadOnly_: pass

      class Foo:
        def readonly_method(self: _PyreReadOnly_[Self]) -> None: ...
      |}
           {|
      _Self_typing_Foo__ = typing.TypeVar("_Self_typing_Foo__", bound=Foo)

      Self: _SpecialForm
      class _PyreReadOnly_: pass

      class Foo:
        def readonly_method(self: pyre_extensions.PyreReadOnly[_Self_typing_Foo__]) -> None: ...
      |};
      (* PyreReadOnly method that does not use `typing_extensions.Self`. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      from typing_extensions import Self
      from pyre_extensions import PyreReadOnly

      class Foo:
        def readonly_method(self: PyreReadOnly[Foo]) -> None: ...
      |}
           {|
      from typing_extensions import Self
      from pyre_extensions import PyreReadOnly

      class Foo:
        def readonly_method(self: PyreReadOnly[Foo]) -> None: ...
      |};
      (* PyreReadOnly classmethod. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      from typing import Type
      from typing_extensions import Self
      from pyre_extensions import PyreReadOnly

      class Foo:
        @classmethod
        def readonly_classmethod(cls: PyreReadOnly[Type[Self]]) -> None: ...
      |}
           {|
      _Self_test_Foo__ = typing.TypeVar("_Self_test_Foo__", bound=Foo)

      from typing import Type
      from typing_extensions import Self
      from pyre_extensions import PyreReadOnly

      class Foo:
        @classmethod
        def readonly_classmethod(cls: pyre_extensions.PyreReadOnly[typing.Type[_Self_test_Foo__]]) -> None: ...
      |};
    ]


let test_inline_keyword_only_attribute =
  let assert_expand ?(handle = "test.py") source expected _ =
    let expected = parse ~handle expected in
    let actual = parse ~handle source |> Preprocessing.add_dataclass_keyword_only_specifiers in
    assert_source_equal ~location_insensitive:true expected actual
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
    from dataclasses import dataclass, KW_ONLY

    @dataclass
    class A:
      _: KW_ONLY
      x: int
  |}
           {|
    from dataclasses import dataclass, KW_ONLY

    @dataclass
    class A:
      x: int = dataclasses.field(kw_only=True, default=...)
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
    from dataclasses import dataclass, KW_ONLY

    @dataclass
    class A:
      _: KW_ONLY
      x: int = 5
  |}
           {|
    from dataclasses import dataclass, KW_ONLY

    @dataclass
    class A:
      x: int = dataclasses.field(kw_only=True, default=5)
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
    from dataclasses import dataclass, KW_ONLY

    @dataclass
    class A:
      z: int
      _: KW_ONLY
      x: int
      y: int
  |}
           {|
    from dataclasses import dataclass, KW_ONLY

    @dataclass
    class A:
      z: int
      x: int = dataclasses.field(kw_only=True, default=...)
      y: int = dataclasses.field(kw_only=True, default=...)
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
    from dataclasses import dataclass, KW_ONLY

    @dataclass
    class A:
      f: KW_ONLY
      x: int
  |}
           {|
    from dataclasses import dataclass, KW_ONLY

    @dataclass
    class A:
      x: int = dataclasses.field(kw_only=True, default=...)
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      from dataclasses import dataclass

      @dataclass
      class A:
        x: int
        init_variable: dataclasses.InitVar[str]
    |}
           {|
      from dataclasses import dataclass

      @dataclass
      class A:
        x: int
        init_variable: dataclasses.InitVar[str]
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      from dataclasses import dataclass, KW_ONLY

      @dataclass
      class A:
        x: int
        _: KW_ONLY
        init_variable: dataclasses.InitVar[str]
    |}
           {|
      from dataclasses import dataclass, KW_ONLY

      @dataclass
      class A:
        x: int
        init_variable: dataclasses.InitVar[str] = dataclasses.field(kw_only=True, default=...)
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
    from dataclasses import dataclass, KW_ONLY, field

    @dataclass
    class A:
      x: int = field(kw_only=True)
      _: KW_ONLY
      y: int
      z: int
  |}
           {|
    from dataclasses import dataclass, KW_ONLY, field

    @dataclass
    class A:
      x: int = field(kw_only=True)
      y: int = dataclasses.field(kw_only=True, default = ...)
      z: int = dataclasses.field(kw_only=True, default = ...)
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
    from dataclasses import dataclass, KW_ONLY, field

    @dataclass
    class A:
      _: KW_ONLY
      x: int = field(kw_only=True)
  |}
           {|
    from dataclasses import dataclass, KW_ONLY, field

    @dataclass
    class A:
      x: int = field(kw_only=True)
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
    from dataclasses import dataclass, KW_ONLY, field

    @dataclass
    class A:
      _: KW_ONLY
      x: int = field(kw_only=False)
  |}
           {|
    from dataclasses import dataclass, KW_ONLY, field

    @dataclass
    class A:
      x: int = field(kw_only=False)
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
    from dataclasses import dataclass, KW_ONLY, field

    @dataclass
    class A:
      x: int = 5
      _: KW_ONLY
      y: int = 5
  |}
           {|
    from dataclasses import dataclass, KW_ONLY, field

    @dataclass
    class A:
      x: int = 5
      y: int = dataclasses.field(kw_only=True, default=5)
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
    from dataclasses import dataclass, KW_ONLY, field

    @dataclass
    class A:
      _: KW_ONLY
      x: List[int] = field(default=[1], default_factory=list, init=False, repr=False, hash=True, compare=False, metadata={"stuff": 0})
  |}
           {|
    from dataclasses import dataclass, KW_ONLY, field

    @dataclass
    class A:
      x: List[int] = field(kw_only = True, default = [1], default_factory = list, init = False, repr = False, hash = True, compare = False, metadata = { "stuff":0 })
  |};
    ]


let test_expand_enum_functional_syntax =
  let assert_expand ?(handle = "test.py") source expected _ =
    let expected = parse ~handle expected in
    let actual = parse ~handle source |> Preprocessing.expand_enum_functional_syntax in
    assert_source_equal ~location_insensitive:true expected actual
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      from enum import Enum

      Color = Enum("Color", ["RED", "GREEN", "BLUE"])
  |}
           {|
      import enum
      from enum import Enum

      class Color(Enum):
        RED = enum.auto()
        GREEN = enum.auto()
        BLUE = enum.auto()
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      from enum import IntEnum

      Color = IntEnum("Color", ["RED", "GREEN", "BLUE"])
  |}
           {|
      import enum
      from enum import IntEnum

      class Color(IntEnum):
        RED = enum.auto()
        GREEN = enum.auto()
        BLUE = enum.auto()
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      from enum import Enum

      Color = Enum("Color", ("RED", "GREEN", "BLUE"))
  |}
           {|
      import enum
      from enum import Enum

      class Color(Enum):
        RED = enum.auto()
        GREEN = enum.auto()
        BLUE = enum.auto()
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      from enum import Enum

      xs = ["NOT", "SUPPORTED"]
      Color = Enum("Color", [x for x in xs])
  |}
           {|
      from enum import Enum

      xs = ["NOT", "SUPPORTED"]
      Color = Enum("Color", [x for x in xs])
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      from enum import Enum

      Color = Enum("Color", "NOT SUPPORTED")
  |}
           {|
      from enum import Enum

      Color = Enum("Color", "NOT SUPPORTED")
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      from enum import Enum

      class Outer:
        Color = Enum("Color", "NOT SUPPORTED")
  |}
           {|
      from enum import Enum

      class Outer:
        Color = Enum("Color", "NOT SUPPORTED")
  |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_expand
           {|
      from enum import Enum

      def foo() -> None:
        Color = Enum("Color", "NOT SUPPORTED")
  |}
           {|
      from enum import Enum

      def foo() -> None:
        Color = Enum("Color", "NOT SUPPORTED")
  |};
    ]


let () =
  "preprocessing"
  >::: [
         test_expand_relative_imports;
         test_expand_string_annotations;
         test_expand_type_alias_body;
         test_expand_string_annotation_preserves_locations;
         test_qualify_source;
         test_qualify_ast;
         test_qualify_ast_class_with_same_name_as_local;
         test_replace_version_specific_code;
         test_replace_platform_specific_code;
         test_expand_type_checking_imports;
         test_expand_wildcard_imports;
         test_expand_implicit_returns;
         test_defines;
         test_classes;
         test_toplevel_assigns;
         test_transform_ast;
         test_replace_lazy_import;
         test_expand_typed_dictionaries;
         test_expand_typed_dictionaries__required_not_required;
         test_sqlalchemy_declarative_base;
         test_populate_captures;
         test_populate_unbound_names;
         test_union_shorthand;
         test_mangle_private_attributes;
         test_six_metaclass_decorator;
         test_expand_import_python_calls;
         test_expand_pytorch_register_buffer;
         test_expand_self_type;
         test_inline_keyword_only_attribute;
         test_expand_enum_functional_syntax;
       ]
  |> Test.run
