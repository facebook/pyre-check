(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Expression
open Statement

open Test


let assert_parsed_equal source statements =
  assert_source_equal
    (Source.create ~handle:(File.Handle.create "test.py") statements)
    (parse_untrimmed source)


let test_lexer _ =
  assert_parsed_equal "1 # comment" [+Expression (+Integer 1)];
  assert_parsed_equal "# comment\n1" [+Expression (+Integer 1)];

  assert_parsed_equal
    "if a:\n\tb # comment\n"
    [
      +If {
        If.test = !"a";
        body = [+Expression !"b"];
        orelse = [];
      };
    ];
  assert_parsed_equal
    "if a:\n\tb\n\n#comment\nelse:\n\tc\n"
    [
      +If {
        If.test = !"a";
        body = [+Expression !"b"];
        orelse = [+Expression !"c"];
      };
    ];
  assert_parsed_equal
    "if a:\n\tb\n# comment"
    [
      +If {
        If.test = !"a";
        body = [+Expression !"b"];
        orelse = [];
      };
    ];
  assert_parsed_equal
    "if a: #comment\n\tb"
    [
      +If {
        If.test = !"a";
        body = [+Expression !"b"];
        orelse = [];
      };
    ];
  assert_parsed_equal
    "if a:\n#comment\n\tb"
    [
      +If {
        If.test = !"a";
        body = [+Expression !"b"];
        orelse = [];
      };
    ];
  assert_parsed_equal
    "if a:\n\t\t#comment\n\tb"
    [
      +If {
        If.test = !"a";
        body = [+Expression !"b"];
        orelse = [];
      };
    ];
  assert_parsed_equal
    "if a:\n\tb\n\n #comment\n #comment\n\n\tc"
    [
      +If {
        If.test = !"a";
        body = [
          +Expression !"b";
          +Expression !"c";
        ];
        orelse = [];
      };
    ];

  assert_parsed_equal
    "print a"
    [+Expression !"a"];

  assert_parsed_equal
    "1 +\\\n 2"
    [
      +Expression
        (+Access [
           Access.Expression (+Integer 1);
           Access.Identifier ~~"__add__";
           Access.Call (+[{ Argument.name = None; value = +Integer 2 }]);
         ]);
    ];
  assert_parsed_equal
    "1 + \\\n 2"
    [
      +Expression
        (+Access [
           Access.Expression (+Integer 1);
           Access.Identifier ~~"__add__";
           Access.Call (+[{ Argument.name = None; value = +Integer 2 }]);
         ]);
    ];
  assert_parsed_equal
    "(1 +\n 2)"
    [
      +Expression
        (+Access [
           Access.Expression (+Integer 1);
           Access.Identifier ~~"__add__";
           Access.Call (+[{ Argument.name = None; value = +Integer 2 }]);
         ]);
    ];
  assert_parsed_equal
    "(1 +\n 2)\n3"
    [
      +Expression
        (+Access [
           Access.Expression (+Integer 1);
           Access.Identifier ~~"__add__";
           Access.Call (+[{ Argument.name = None; value = +Integer 2 }]);
         ]);
      +Expression (+Integer 3)
    ]


let test_number _ =
  assert_parsed_equal "1" [+Expression (+Integer 1)];
  assert_parsed_equal "0" [+Expression (+Integer 0)];
  assert_parsed_equal "00" [+Expression (+Integer 0)];
  assert_parsed_equal "00_0" [+Expression (+Integer 0)];
  assert_parsed_equal "01" [+Expression (+Integer 1)];
  assert_parsed_equal "1_01" [+Expression (+Integer 101)];
  assert_parsed_equal "(1)" [+Expression (+Integer 1)];
  assert_parsed_equal "((1))" [+Expression (+Integer 1)];
  assert_parsed_equal "1;" [+Expression (+Integer 1)];

  assert_parsed_equal "1.0" [+Expression (+Float 1.0)];
  assert_parsed_equal "1_0.1_01" [+Expression (+Float 10.101)];
  assert_parsed_equal ".1" [+Expression (+Float 0.1)];
  assert_parsed_equal "1." [+Expression (+Float 1.0)];
  assert_parsed_equal "1e10" [+Expression (+Float 1e10)];

  assert_parsed_equal "0x1" [+Expression (+Integer 0x1)];
  assert_parsed_equal "0XaBc" [+Expression (+Integer 0xABC)];
  assert_parsed_equal "0o13" [+Expression (+Integer 0o13)];
  assert_parsed_equal "0b01" [+Expression (+Integer 0b01)];
  assert_parsed_equal "0b0_1" [+Expression (+Integer 0b01)];
  assert_parsed_equal "0b_0_1" [+Expression (+Integer 0b01)];

  assert_parsed_equal "0.1j" [+Expression (+Complex 0.1)];
  assert_parsed_equal "1e10j" [+Expression (+Complex 1e10)];
  assert_parsed_equal "1e1_0j" [+Expression (+Complex 1e10)];
  assert_parsed_equal "2j" [+Expression (+Complex 2.0)];
  assert_parsed_equal "2J" [+Expression (+Complex 2.0)];

  assert_raises
    (Failure "Could not parse test")
    (fun () -> parse_untrimmed ~silent:true "0xZ");
  assert_raises
    (Failure "Could not parse test")
    (fun () -> parse_untrimmed ~silent:true "0_1");
  assert_raises
    (Failure "Could not parse test")
    (fun () -> parse_untrimmed ~silent:true "0o9");
  assert_raises
    (Failure "Could not parse test")
    (fun () -> parse_untrimmed ~silent:true "1a3");

  (* Overflow. *)
  assert_parsed_equal
    "0xffffffffff0000000000000000000000"
    [+Expression (+Integer Int.max_value)]


let test_await _ =
  assert_parsed_equal "await 1" [+Expression (+Await (+Integer 1))]


let test_access _ =
  assert_parsed_equal "a" [+Expression !"a"];
  assert_parsed_equal "$a" [+Expression !"$a"];
  assert_parsed_equal "_a" [+Expression !"_a"];
  assert_parsed_equal "_a0" [+Expression !"_a0"];
  assert_parsed_equal
    "a.b"
    [
      +Expression (+Access (Access.create "a.b"));
    ];
  assert_parsed_equal
    "a.async"
    [
      +Expression (+Access (Access.create "a.async"));
    ];
  assert_parsed_equal
    "1.0.b"
    [
      +Expression (+Access [
          Access.Expression (+Float 1.0);
          Access.Identifier ~~"b";
        ]);
    ];
  assert_parsed_equal
    "a.b.c"
    [
      +Expression (+Access (Access.create "a.b.c"));
    ];

  assert_parsed_equal
    "a[1]"
    [
      +Expression
        (+Access [
           Access.Identifier ~~"a";
           Access.Identifier ~~"__getitem__";
           Access.Call (+[{ Argument.name = None; value = +Integer 1 }]);
         ])
    ];
  assert_parsed_equal
    "a[1 < 2]"
    [
      +Expression
        (+Access [
           Access.Identifier ~~"a";
           Access.Identifier ~~"__getitem__";
           Access.Call
             (+[
                {
                  Argument.name = None;
                  value =
                    +ComparisonOperator {
                      ComparisonOperator.left = +Integer 1;
                      operator = ComparisonOperator.LessThan;
                      right = +Integer 2;
                    };
                };
              ]);
         ])
    ];
  assert_parsed_equal
    "a[1].b"
    [
      +Expression
        (+Access [
           Access.Identifier ~~"a";
           Access.Identifier ~~"__getitem__";
           Access.Call (+[{ Argument.name = None; value = +Integer 1 }]);
           Access.Identifier ~~"b";
         ])
    ];
  assert_parsed_equal
    "a[b]"
    [
      +Expression
        (+Access [
           Access.Identifier ~~"a";
           Access.Identifier ~~"__getitem__";
           Access.Call (+[{ Argument.name = None; value = +Access [Access.Identifier ~~"b"] }]);
         ])
    ];
  assert_parsed_equal
    "a[:]"
    [
      +Expression
        (+Access [
           Access.Identifier ~~"a";
           Access.Identifier ~~"__getitem__";
           Access.Call
             (+[
                {
                  Argument.name = None;
                  value = +Access [
                    Access.Identifier ~~"slice";
                    Access.Call (+[
                        { Argument.name = None; value = +Access [Access.Identifier ~~"None"] };
                        { Argument.name = None; value = +Access [Access.Identifier ~~"None"] };
                        { Argument.name = None; value = +Access [Access.Identifier ~~"None"] };
                      ]);
                  ];
                };
              ]);
         ])
    ];
  assert_parsed_equal
    "a[1:]"
    [
      +Expression
        (+Access [
           Access.Identifier ~~"a";
           Access.Identifier ~~"__getitem__";
           Access.Call
             (+[
                {
                  Argument.name = None;
                  value = +Access [
                    Access.Identifier ~~"slice";
                    Access.Call (+[
                        { Argument.name = None; value = +Integer 1 };
                        { Argument.name = None; value = +Access [Access.Identifier ~~"None"] };
                        { Argument.name = None; value = +Access [Access.Identifier ~~"None"] };
                      ]);
                  ];
                };
              ]);
         ])
    ];
  assert_parsed_equal
    "a[::2]"
    [
      +Expression
        (+Access [
           Access.Identifier ~~"a";
           Access.Identifier ~~"__getitem__";
           Access.Call
             (+[
                {
                  Argument.name = None;
                  value = +Access [
                    Access.Identifier ~~"slice";
                    Access.Call (+[
                        { Argument.name = None; value = +Access [Access.Identifier ~~"None"] };
                        { Argument.name = None; value = +Access [Access.Identifier ~~"None"] };
                        { Argument.name = None; value = +Integer 2 };
                      ]);
                  ];
                };
              ]);
         ]);
    ];
  assert_parsed_equal
    "a[:1]"
    [
      +Expression
        (+Access [
           Access.Identifier ~~"a";
           Access.Identifier ~~"__getitem__";
           Access.Call
             (+[
                {
                  Argument.name = None;
                  value = +Access [
                    Access.Identifier ~~"slice";
                    Access.Call (+[
                        { Argument.name = None; value = +Access [Access.Identifier ~~"None"] };
                        { Argument.name = None; value = +Integer 1 };
                        { Argument.name = None; value = +Access [Access.Identifier ~~"None"] };
                      ]);
                  ];
                };
              ]);
         ]);
    ];
  assert_parsed_equal
    "a[:1 if True else 2]"
    [
      +Expression
        (+Access [
           Access.Identifier ~~"a";
           Access.Identifier ~~"__getitem__";
           Access.Call
             (+[
                {
                  Argument.name = None;
                  value = +Access [
                    Access.Identifier ~~"slice";
                    Access.Call (+[
                        { Argument.name = None; value = +Access [Access.Identifier ~~"None"] };
                        {
                          Argument.name = None;
                          value =
                            +Ternary {
                              Ternary.target = +Integer 1;
                              test = +True;
                              alternative = +Integer 2;
                            };
                        };
                        { Argument.name = None; value = +Access [Access.Identifier ~~"None"] };
                      ]);
                  ];
                };
              ]);
         ]);
    ];
  assert_parsed_equal
    "a[1:1]"
    [
      +Expression
        (+Access [
           Access.Identifier ~~"a";
           Access.Identifier ~~"__getitem__";
           Access.Call
             (+[
                {
                  Argument.name = None;
                  value = +Access [
                    Access.Identifier ~~"slice";
                    Access.Call (+[
                        { Argument.name = None; value = +Integer 1 };
                        { Argument.name = None; value = +Integer 1 };
                        { Argument.name = None; value = +Access [Access.Identifier ~~"None"] };
                      ]);
                  ];
                };
              ]);
         ]);
    ];
  assert_parsed_equal
    "a[1,2]"
    [
      +Expression
        (+Access [
           Access.Identifier ~~"a";
           Access.Identifier ~~"__getitem__";
           Access.Call (+[{ Argument.name = None; value = +Tuple [+Integer 1; +Integer 2] }]);
         ]);
    ];
  assert_parsed_equal
    "a[:1,2]"
    [
      +Expression
        (+Access [
           Access.Identifier ~~"a";
           Access.Identifier ~~"__getitem__";
           Access.Call
             (+[
                {
                  Argument.name = None;
                  value =
                    +Tuple [
                      +Access [
                        Access.Identifier ~~"slice";
                        Access.Call
                          (+[
                             { Argument.name = None; value = +Access [Access.Identifier ~~"None"] };
                             { Argument.name = None; value = +Integer 1 };
                             { Argument.name = None; value = +Access [Access.Identifier ~~"None"] };
                           ])
                      ];
                      +Integer 2;
                    ];
                };
              ]);
         ]);
    ]


let test_starred _ =
  assert_parsed_equal
    "*a"
    [+Expression (+Starred (Starred.Once !"a"))];
  assert_parsed_equal
    "*(a)"
    [+Expression (+Starred (Starred.Once !"a"))];
  assert_parsed_equal
    "**a"
    [+Expression (+Starred (Starred.Twice !"a"))]


let test_compound _ =
  assert_parsed_equal
    "1.0\n2"
    [+Expression (+Float 1.0); +Expression (+Integer 2)];
  assert_parsed_equal
    "1.0;2"
    [+Expression (+Float 1.0); +Expression (+Integer 2)];
  assert_parsed_equal
    "\n1.0\n2\n3"
    [
      +Expression (+Float 1.0);
      +Expression (+Integer 2);
      +Expression (+Integer 3)
    ]


let test_define _ =
  assert_parsed_equal
    "def foo(a):\n  1"
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = None;
          };
        ];
        body = [+Expression (+Integer 1)];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ];
  assert_parsed_equal
    "def foo(*, a):\n  1"
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"*";
            value = None;
            annotation = None;
          };
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = None;
          };
        ];
        body = [+Expression (+Integer 1)];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ];
  assert_parsed_equal
    "def foo(**a):\n  1"
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"**a";
            value = None;
            annotation = None;
          };
        ];
        body = [+Expression (+Integer 1)];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ];
  assert_parsed_equal
    "async def foo():\n  1"
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [];
        body = [+Expression (+Integer 1)];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = true;
        parent = None;
      };
    ];
  assert_parsed_equal
    "async def foo():\n  ..."
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [];
        body = [+Expression (+Ellipses)];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = true;
        parent = None;
      }
    ];
  assert_parsed_equal
    "@foo\nasync def foo():\n  1"
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [];
        body = [+Expression (+Integer 1)];
        decorators = [!"foo"];
        docstring = None;
        return_annotation = None;
        async = true;
        parent = None;
      };
    ];
  assert_parsed_equal
    "@decorator\ndef foo(a):\n  1"
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = None;
          };
        ];
        body = [+Expression (+Integer 1)];
        decorators = [!"decorator"];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ];
  assert_parsed_equal
    "@decorator(a=b, c=d)\ndef foo(a):\n  1"
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = None;
          };
        ];
        body = [+Expression (+Integer 1)];
        decorators = [
          (+Access [
             Access.Identifier ~~"decorator";
             Access.Call
               (+[
                  { Argument.name = Some ~+(~~"a"); value = !"b" };
                  { Argument.name = Some ~+(~~"c"); value = !"d" };
                ]);
           ]);
        ];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ];
  assert_parsed_equal
    "@foo\n\n@bar\ndef foo(a):\n  1"
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = None;
          };
        ];
        body = [+Expression (+Integer 1)];
        decorators = [!"foo"; !"bar"];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ];
  assert_parsed_equal
    "def foo(a, b):\n  1"
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = None;
          };
          +{
            Parameter.name = ~~"b";
            value = None;
            annotation = None;
          };
        ];
        body = [+Expression (+Integer 1)];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ];
  assert_parsed_equal
    "def foo(a = 1, b):\n  1"
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = Some (+Integer 1);
            annotation = None;
          };
          +{
            Parameter.name = ~~"b";
            value = None;
            annotation = None;
          };
        ];
        body = [+Expression (+Integer 1)];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ];
  assert_parsed_equal
    "def foo(a=()):\n  1"
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = Some (+Tuple []);
            annotation = None;
          };
        ];
        body = [+Expression (+Integer 1)];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ];
  assert_parsed_equal
    "def foo(): 1; 2"
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [];
        body = [+Expression (+Integer 1); +Expression (+Integer 2)];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ];
  assert_parsed_equal
    "def foo():\n  1\n  2\n3"
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [];
        body = [+Expression (+Integer 1); +Expression (+Integer 2)];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
      +Expression (+Integer 3)
    ];
  assert_parsed_equal
    "def foo():\n  def bar():\n    1\n    2\n3"
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [];
        body = [
          +Define {
            Define.name = Access.create "bar";
            parameters = [];
            body = [+Expression (+Integer 1); +Expression (+Integer 2)];
            decorators = [];
            docstring = None;
            return_annotation = None;
            async = false;
            parent = None;
          };
        ];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
      +Expression (+Integer 3)
    ];

  assert_parsed_equal
    "def foo(a: int):  1"
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = Some !"int";
          };
        ];
        body = [+Expression (+Integer 1)];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ];
  assert_parsed_equal
    "def foo(a: int = 1):  1"
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = Some (+Integer 1);
            annotation = Some !"int";
          };
        ];
        body = [+Expression (+Integer 1)];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ];
  assert_parsed_equal
    "def foo(a: int, b: string):  1"
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = Some !"int";
          };
          +{
            Parameter.name = ~~"b";
            value = None;
            annotation = Some !"string";
          };
        ];
        body = [+Expression (+Integer 1)];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ];
  assert_parsed_equal
    "def foo(a: Tuple[int, str]):\n  1"
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = Some
                (+Access [
                   Access.Identifier ~~"Tuple";
                   Access.Identifier ~~"__getitem__";
                   Access.Call
                     (+[
                        {
                          Argument.name = None;
                          value =
                            +Tuple [
                              +Access [Access.Identifier ~~"int"];
                              +Access [Access.Identifier ~~"str"];
                            ];
                        };
                      ]);
                 ]);
          };
        ];
        body = [+Expression (+Integer 1)];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ];
  assert_parsed_equal
    "def foo(a, b,) -> c:\n  1"
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = None;
          };
          +{
            Parameter.name = ~~"b";
            value = None;
            annotation = None;
          };
        ];
        body = [+Expression (+Integer 1)];
        decorators = [];
        docstring = None;
        return_annotation = Some !"c";
        async = false;
        parent = None;
      };
    ];
  assert_parsed_equal
    "def foo() -> str:\n  1\n  2"
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [];
        body = [+Expression (+Integer 1); +Expression (+Integer 2)];
        decorators = [];
        docstring = None;
        return_annotation = Some !"str";
        async = false;
        parent = None;
      };
    ];
  assert_parsed_equal
    (trim_extra_indentation {|
      def foo():
        # type: (...) -> int
        return 4
    |})
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [];
        body = [
          +Return {
            Return.expression = Some (+Integer 4);
            is_implicit = false;
          }];
        decorators = [];
        docstring = None;
        return_annotation = Some (+String (StringLiteral.create "int"));
        async = false;
        parent = None;
      };
    ];
  assert_parsed_equal
    (trim_extra_indentation {|
      def foo():
        # type: (...) -> 'int'
        return 4
    |})
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [];
        body = [
          +Return {
            Return.expression = Some (+Integer 4);
            is_implicit = false;
          }];
        decorators = [];
        docstring = None;
        return_annotation = Some (+String (StringLiteral.create "int"));
        async = false;
        parent = None;
      };
    ];

  assert_parsed_equal
    (trim_extra_indentation {|
      def foo():
        # type: () -> str
        return 4
    |})
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [];
        body = [
          +Return {
            Return.expression = Some (+Integer 4);
            is_implicit = false;
          }];
        decorators = [];
        docstring = None;
        return_annotation = Some (+String (StringLiteral.create "str"));
        async = false;
        parent = None;
      };
    ];

  assert_parsed_equal
    (trim_extra_indentation {|
      def foo(a):
        # type: (str) -> str
        return 4
    |})
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = Some (+String (StringLiteral.create "str"));
          };
        ];
        body = [
          +Return {
            Return.expression = Some (+Integer 4);
            is_implicit = false;
          }];
        decorators = [];
        docstring = None;
        return_annotation = Some (+String (StringLiteral.create "str"));
        async = false;
        parent = None;
      };
    ];

  assert_parsed_equal
    (trim_extra_indentation {|
      def foo(a): # type: (str) -> str
        return 4
    |})
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = Some (+String (StringLiteral.create "str"));
          };
        ];
        body = [
          +Return {
            Return.expression = Some (+Integer 4);
            is_implicit = false;
          }];
        decorators = [];
        docstring = None;
        return_annotation = Some (+String (StringLiteral.create "str"));
        async = false;
        parent = None;
      };
    ];

  (* Don't use string annotations if list length does not match signature *)
  assert_parsed_equal
    (trim_extra_indentation {|
      def foo(a): # type: (str, str) -> str
        return 4
    |})
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = None;
          };
        ];
        body = [
          +Return {
            Return.expression = Some (+Integer 4);
            is_implicit = false;
          }];
        decorators = [];
        docstring = None;
        return_annotation = Some (+String (StringLiteral.create "str"));
        async = false;
        parent = None;
      };
    ];

  assert_parsed_equal
    (trim_extra_indentation {|
      def foo(a, b): # type: (typing.Union[typing.List[int], str], str) -> str
        return 4
    |})
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = Some
                (+String (StringLiteral.create "typing.Union[typing.List[int], str]"));
          };
          +{
            Parameter.name = ~~"b";
            value = None;
            annotation = Some (+String (StringLiteral.create "str"));
          };
        ];
        body = [
          +Return {
            Return.expression = Some (+Integer 4);
            is_implicit = false;
          }];
        decorators = [];
        docstring = None;
        return_annotation = Some (+String (StringLiteral.create "str"));
        async = false;
        parent = None;
      };
    ];

  assert_parsed_equal
    (trim_extra_indentation {|
      def foo(a, b): # type: (typing.Union[typing.List[int], str], typing.List[str]) -> str
        return 4
    |})
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = Some
                (+String (StringLiteral.create "typing.Union[typing.List[int], str]"));
          };
          +{
            Parameter.name = ~~"b";
            value = None;
            annotation = Some (+String (StringLiteral.create "typing.List[str]"));
          };
        ];
        body = [
          +Return {
            Return.expression = Some (+Integer 4);
            is_implicit = false;
          }];
        decorators = [];
        docstring = None;
        return_annotation = Some (+String (StringLiteral.create "str"));
        async = false;
        parent = None;
      };
    ];

  assert_parsed_equal
    (trim_extra_indentation {|
      def foo():
        # type: (...) ->List[str]
        return 4
    |})
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [];
        body = [
          +Return {
            Return.expression = Some (+Integer 4);
            is_implicit = false;
          }];
        decorators = [];
        docstring = None;
        return_annotation = Some (+String (StringLiteral.create "List[str]"));
        async = false;
        parent = None;
      };
    ];

  assert_parsed_equal
    (trim_extra_indentation {|
      def foo(
        self,
        a,  # type: bool
        b,  # type: bool
      ):  # type: (...) -> int
        pass
    |})
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"self";
            value = None;
            annotation = None;
          };
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = Some (+String (StringLiteral.create "bool"));
          };
          +{
            Parameter.name = ~~"b";
            value = None;
            annotation = Some (+String (StringLiteral.create "bool"));
          };
        ];
        body = [+Pass];
        decorators = [];
        docstring = None;
        return_annotation = Some (+String (StringLiteral.create "int"));
        async = false;
        parent = None;
      };
    ];
  assert_parsed_equal
    (trim_extra_indentation {|
      def foo(
        a,  # type: bool
        b  # type: bool
      ):
        pass
    |})
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = Some (+String (StringLiteral.create "bool"));
          };
          +{
            Parameter.name = ~~"b";
            value = None;
            annotation = Some (+String (StringLiteral.create "bool"));
          };
        ];
        body = [+Pass];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ];
  assert_parsed_equal
    (trim_extra_indentation {|
      def foo(
        a,  # type: bool
        **kwargs
      ):
        pass
    |})
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = Some (+String (StringLiteral.create "bool"));
          };
          +{
            Parameter.name = ~~"**kwargs";
            value = None;
            annotation = None;
          };
        ];
        body = [+Pass];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ]


let test_boolean_operator _ =
  assert_parsed_equal "True" [+Expression (+True)];
  assert_parsed_equal "False" [+Expression (+False)];
  assert_parsed_equal
    "True and False"
    [
      +Expression
        (+BooleanOperator {
           BooleanOperator.left = +True;
           operator = BooleanOperator.And;
           right = +False;
         });
    ];
  assert_parsed_equal
    "1 and False"
    [
      +Expression
        (+BooleanOperator {
           BooleanOperator.left = +Integer 1;
           operator = BooleanOperator.And;
           right = +False;
         });
    ];
  assert_parsed_equal
    "True or 1"
    [
      +Expression
        (+BooleanOperator {
           BooleanOperator.left = +True;
           operator = BooleanOperator.Or;
           right = +Integer 1;
         });
    ];
  assert_parsed_equal
    "1 and 2 or 3"
    [
      +Expression
        (+BooleanOperator {
           BooleanOperator.left =
             (+BooleanOperator {
                BooleanOperator.left = +Integer 1;
                operator = BooleanOperator.And;
                right = +Integer 2;
              });
           operator = BooleanOperator.Or;
           right = +Integer 3;
         });
    ]


let test_binary_operator _ =
  assert_parsed_equal
    "1 + 2"
    [
      +Expression
        (+Access [
           Access.Expression (+Integer 1);
           Access.Identifier ~~"__add__";
           Access.Call (+[{ Argument.name = None; value = +Integer 2 }]);
         ]);
    ];
  assert_parsed_equal
    "1 ^ 2"
    [
      +Expression
        (+Access [
           Access.Expression (+Integer 1);
           Access.Identifier ~~"__xor__";
           Access.Call (+[{ Argument.name = None; value = +Integer 2 }]);
         ]);
    ];
  assert_parsed_equal
    "1 // 2"
    [
      +Expression
        (+Access [
           Access.Expression (+Integer 1);
           Access.Identifier ~~"__floordiv__";
           Access.Call (+[{ Argument.name = None; value = +Integer 2 }]);
         ]);
    ];
  assert_parsed_equal
    "1 - 2 + 3"
    [
      +Expression
        (+Access [
           Access.Expression (+Integer 1);
           Access.Identifier ~~"__sub__";
           Access.Call (+[{ Argument.name = None; value = +Integer 2 }]);
           Access.Identifier ~~"__add__";
           Access.Call (+[{ Argument.name = None; value = +Integer 3 }]);
         ]);
    ];
  assert_parsed_equal
    "a + b.c"
    [
      +Expression
        (+Access [
           Access.Identifier ~~"a";
           Access.Identifier ~~"__add__";
           Access.Call
             (+[
                {
                  Argument.name = None;
                  value = +Access [Access.Identifier ~~"b"; Access.Identifier ~~"c"];
                };
              ]);
         ]);
    ]


let test_unary_operator _ =
  assert_parsed_equal
    "not 1"
    [
      +Expression
        (+UnaryOperator {
           UnaryOperator.operator = UnaryOperator.Not;
           operand = +Integer 1;
         });
    ];
  assert_parsed_equal
    "~1"
    [
      +Expression
        (+UnaryOperator {
           UnaryOperator.operator = UnaryOperator.Invert;
           operand = +Integer 1;
         });
    ];
  assert_parsed_equal
    "+1"
    [
      +Expression
        (+UnaryOperator {
           UnaryOperator.operator = UnaryOperator.Positive;
           operand = +Integer 1;
         });
    ]


let test_lambda _ =
  assert_parsed_equal
    "lambda: 1"
    [+Expression (+Lambda { Lambda.parameters = []; body = +Integer 1 })];
  assert_parsed_equal
    "lambda x,: x"
    [
      +Expression
        (+Lambda {
           Lambda.parameters = [
             +{
               Parameter.name = ~~"x";
               value = None;
               annotation = None;
             };
           ];
           body = !"x";
         });
    ];
  assert_parsed_equal
    "lambda x: x is y"
    [
      +Expression
        (+Lambda {
           Lambda.parameters = [
             +{
               Parameter.name = ~~"x";
               value = None;
               annotation = None;
             };
           ];
           body = +ComparisonOperator {
             ComparisonOperator.left = !"x";
             operator = ComparisonOperator.Is;
             right = !"y";
           };
         });
    ];
  assert_parsed_equal
    "lambda x: x"
    [
      +Expression
        (+Lambda {
           Lambda.parameters = [
             +{
               Parameter.name = ~~"x";
               value = None;
               annotation = None;
             };
           ];
           body = !"x";
         });
    ];
  assert_parsed_equal
    "lambda x = 1, y: x + 1"
    [
      +Expression
        (+Lambda {
           Lambda.parameters = [
             +{
               Parameter.name = ~~"x";
               value = Some (+Integer 1);
               annotation = None;
             };
             +{
               Parameter.name = ~~"y";
               value = None;
               annotation = None;
             };
           ];
           body = +Access [
             Access.Identifier ~~"x";
             Access.Identifier ~~"__add__";
             Access.Call (+[{ Argument.name = None; value = +Integer 1 }]);
           ];
         });
    ]


let test_ternary _ =
  assert_parsed_equal
    "5 if 1 else 1"
    [
      +Expression
        (+Ternary {
           Ternary.target = +Integer 5;
           test = +Integer 1;
           alternative = +Integer 1;
         });
    ];
  assert_parsed_equal
    "a in b if 1 else 1"
    [
      +Expression
        (+Ternary {
           Ternary.target = +ComparisonOperator {
             ComparisonOperator.left = !"a";
             operator = ComparisonOperator.In;
             right = !"b";
           };
           test = +Integer 1;
           alternative = +Integer 1;
         });
    ];
  assert_parsed_equal
    "1 if 2 else 3 if 4 else 5"
    [
      +Expression
        (+Ternary {
           Ternary.target = +Integer 1;
           test =  +Integer 2;
           alternative = +Ternary {
             Ternary.target = +Integer 3;
             test = +Integer 4;
             alternative = +Integer 5;
           }
         });
    ]


let test_dictionary _ =
  assert_parsed_equal
    "{}"
    [+Expression (+Dictionary { Dictionary.entries = []; keywords = [] })];
  assert_parsed_equal
    "{1: 2}"
    [
      +Expression
        (+Dictionary {
           Dictionary.entries = [
             { Dictionary.key = +Integer 1; value = +Integer 2 };
           ];
           keywords = [];
         });
    ];
  assert_parsed_equal
    "{1: 2,}"
    [
      +Expression
        (+Dictionary {
           Dictionary.entries = [
             { Dictionary.key = +Integer 1; value = +Integer 2 };
           ];
           keywords = [];
         });
    ];

  assert_parsed_equal
    "{1: 2, **durp}"
    [
      +Expression
        (+Dictionary {
           Dictionary.entries = [
             { Dictionary.key = +Integer 1; value = +Integer 2 };
           ];
           keywords = [!"durp"];
         });
    ];
  assert_parsed_equal
    "{1: 2, **durp, **hurp}"
    [
      +Expression
        (+Dictionary {
           Dictionary.entries = [
             { Dictionary.key = +Integer 1; value = +Integer 2 };
           ];
           keywords = [!"durp"; !"hurp"];
         });
    ];
  assert_parsed_equal
    "{**[1]}"
    [
      +Expression
        (+Dictionary {
           Dictionary.entries = [];
           keywords = [+List [+Integer 1]];
         });
    ];
  assert_parsed_equal
    "{**durp, 1: 2}"
    [
      +Expression
        (+Dictionary {
           Dictionary.entries = [
             { Dictionary.key = +Integer 1; value = +Integer 2 };
           ];
           keywords = [!"durp"];
         });
    ];

  assert_parsed_equal
    "{1: 1 < 2,}"
    [
      +Expression
        (+Dictionary {
           Dictionary.entries = [{
               Dictionary.key = +Integer 1;
               value = +ComparisonOperator {
                 ComparisonOperator.left = +Integer 1;
                 operator = ComparisonOperator.LessThan;
                 right = +Integer 2;
               };
             }];
           keywords = [];
         });
    ];
  assert_parsed_equal
    "{1: 2, 2: 3}"
    [
      +Expression
        (+Dictionary {
           Dictionary.entries = [
             { Dictionary.key = +Integer 1; value = +Integer 2 };
             { Dictionary.key = +Integer 2; value = +Integer 3 };
           ];
           keywords = [];
         });
    ];
  assert_parsed_equal
    "{\n\t1: 2,\n\t2: 3}"
    [
      +Expression
        (+Dictionary {
           Dictionary.entries = [
             { Dictionary.key = +Integer 1; value = +Integer 2 };
             { Dictionary.key = +Integer 2; value = +Integer 3 };
           ];
           keywords = [];
         });
    ];

  assert_parsed_equal
    "{a: b for a in []}"
    [
      +Expression
        (+DictionaryComprehension {
           Comprehension.element = {
             Dictionary.key = !"a";
             value = !"b";
           };
           generators = [
             {
               Comprehension.target = !"a";
               iterator = +List [];
               conditions = [];
               async = false;
             };
           ];
         });
    ];
  assert_parsed_equal
    "{a if a else a: b for a in []}"
    [
      +Expression
        (+DictionaryComprehension {
           Comprehension.element = {
             Dictionary.key = +Ternary {
               Ternary.target = !"a";
               test = !"a";
               alternative = !"a";
             };
             value = !"b";
           };
           generators = [
             {
               Comprehension.target = !"a";
               iterator = +List [];
               conditions = [];
               async = false;
             };
           ];
         });
    ];
  assert_parsed_equal
    "{a if a else a: b if b else b for a in []}"
    [
      +Expression
        (+DictionaryComprehension {
           Comprehension.element = {
             Dictionary.key = +Ternary {
               Ternary.target = !"a";
               test = !"a";
               alternative = !"a";
             };
             value = +Ternary {
               Ternary.target = !"b";
               test = !"b";
               alternative = !"b";
             };
           };
           generators = [
             {
               Comprehension.target = !"a";
               iterator = +List [];
               conditions = [];
               async = false;
             };
           ];
         });
    ];
  assert_parsed_equal
    "{a: b for c, d in []}"
    [
      +Expression
        (+DictionaryComprehension {
           Comprehension.element = {
             Dictionary.key = !"a";
             value = !"b";
           };
           generators = [
             {
               Comprehension.target = +Tuple [
                 !"c";
                 !"d";
               ];
               iterator = +List [];
               conditions = [];
               async = false;
             };
           ];
         });
    ]


let test_list _ =
  assert_parsed_equal
    "[]"
    [+Expression (+List [])];
  assert_parsed_equal
    "[[]]"
    [+Expression (+List [+List []])];
  assert_parsed_equal
    "[1,]"
    [+Expression (+List [+Integer 1])];
  assert_parsed_equal
    "[1, 2]"
    [+Expression (+List [+Integer 1; +Integer 2])];
  assert_parsed_equal
    "[1 if 2 else 3]"
    [
      +Expression
        (+List [
           +Ternary {
             Ternary.target = +Integer 1;
             test = +Integer 2;
             alternative = +Integer 3;
           };
         ]);
    ];
  assert_parsed_equal
    "[\n\t1,\n\t2\n]"
    [+Expression (+List [+Integer 1; +Integer 2])];

  assert_parsed_equal
    "[a for a in []]"
    [
      +Expression
        (+ListComprehension {
           Comprehension.element = !"a";
           generators = [
             {
               Comprehension.target = !"a";
               iterator = +List [];
               conditions = [];
               async = false;
             };
           ];
         });
    ];
  assert_parsed_equal
    "[a in b for a in []]"
    [
      +Expression
        (+ListComprehension {
           Comprehension.element = +ComparisonOperator {
             ComparisonOperator.left = !"a";
             operator = ComparisonOperator.In;
             right = !"b";
           };
           generators = [
             {
               Comprehension.target = !"a";
               iterator = +List [];
               conditions = [];
               async = false;
             };
           ];
         });
    ];
  assert_parsed_equal
    "[a for a in a for b in []]"
    [
      +Expression
        (+ListComprehension {
           Comprehension.element = !"a";
           generators = [
             {
               Comprehension.target = !"a";
               iterator = !"a";
               conditions = [];
               async = false;
             };
             {
               Comprehension.target = !"b";
               iterator = +List [];
               conditions = [];
               async = false;
             };
           ];
         });
    ];
  assert_parsed_equal
    "[a for a in [] if b]"
    [
      +Expression
        (+ListComprehension {
           Comprehension.element = !"a";
           generators = [
             {
               Comprehension.target = !"a";
               iterator = +List [];
               conditions = [!"b"];
               async = false;
             };
           ];
         });
    ];
  assert_parsed_equal
    "[a for a in (c for c in []) if b]"
    [
      +Expression
        (+ListComprehension {
           Comprehension.element = !"a";
           generators = [
             {
               Comprehension.target = !"a";
               iterator = +Generator {
                 Comprehension.element = !"c";
                 generators = [
                   {
                     Comprehension.target = !"c";
                     iterator = +List [];
                     conditions = [];
                     async = false;
                   };
                 ];
               };
               conditions = [!"b"];
               async = false;
             };
           ];
         });
    ];
  assert_parsed_equal
    "[a for a in [] if 1 < 2]"
    [
      +Expression
        (+ListComprehension {
           Comprehension.element = !"a";
           generators = [
             {
               Comprehension.target = !"a";
               iterator = +List [];
               conditions = [
                 +ComparisonOperator {
                   ComparisonOperator.left = +Integer 1;
                   operator = ComparisonOperator.LessThan;
                   right = +Integer 2;
                 };
               ];
               async = false;
             };
           ];
         });
    ];
  assert_parsed_equal
    "[a for a in [] if a is 1 or True]"
    [
      +Expression
        (+ListComprehension {
           Comprehension.element = !"a";
           generators = [
             {
               Comprehension.target = !"a";
               iterator = +List [];
               conditions = [
                 +BooleanOperator {
                   BooleanOperator.left = +ComparisonOperator {
                     ComparisonOperator.left = !"a";
                     operator = ComparisonOperator.Is;
                     right = +Integer 1;
                   };
                   operator = BooleanOperator.Or;
                   right = +True;
                 };
               ];
               async = false;
             };
           ];
         });
    ];
  assert_parsed_equal
    "[a async for a in []]"
    [
      +Expression
        (+ListComprehension {
           Comprehension.element = !"a";
           generators = [
             {
               Comprehension.target = !"a";
               iterator = +List [];
               conditions = [];
               async = true;
             };
           ];
         });
    ]


let test_set _ =
  assert_parsed_equal
    "{1}"
    [+Expression (+Set [+Integer 1])];
  assert_parsed_equal
    "{*[1]}"
    [+Expression (+Set [+Starred (Starred.Once (+List [+Integer 1]))])];

  assert_parsed_equal
    "{1,}"
    [+Expression (+Set [+Integer 1])];
  assert_parsed_equal
    "{1, 2}"
    [+Expression (+Set [+Integer 1; +Integer 2])];
  assert_parsed_equal
    "{1, 1 if 2 else 3}"
    [
      +Expression
        (+Set [
           +Integer 1;
           +Ternary {
             Ternary.target = +Integer 1;
             test = +Integer 2;
             alternative = +Integer 3;
           };
         ]);
    ];

  assert_parsed_equal
    "{a for a in []}"
    [
      +Expression
        (+SetComprehension {
           Comprehension.element = !"a";
           generators = [
             {
               Comprehension.target = !"a";
               iterator = +List [];
               conditions = [];
               async = false;
             };
           ];
         });
    ];
  assert_parsed_equal
    "{a for a in [] if b}"
    [
      +Expression
        (+SetComprehension {
           Comprehension.element = !"a";
           generators = [
             {
               Comprehension.target = !"a";
               iterator = +List [];
               conditions = [!"b"];
               async = false;
             };
           ];
         });
    ];
  assert_parsed_equal
    "{a for a in [] if b if c}"
    [
      +Expression
        (+SetComprehension {
           Comprehension.element = !"a";
           generators = [
             {
               Comprehension.target = !"a";
               iterator = +List [];
               conditions = [!"b"; !"c"];
               async = false;
             };
           ];
         });
    ]


let test_generator _ =
  assert_parsed_equal
    "(a in b for a in [] if b)"
    [
      +Expression
        (+Generator {
           Comprehension.element = +ComparisonOperator {
             ComparisonOperator.left = !"a";
             operator = ComparisonOperator.In;
             right = !"b";
           };
           generators = [
             {
               Comprehension.target = !"a";
               iterator = +List [];
               conditions = [!"b"];
               async = false;
             };
           ];
         });
    ]


let test_yield _ =
  assert_parsed_equal
    "yield"
    [+Statement.Yield (+Expression.Yield None)];
  assert_parsed_equal
    "yield 1"
    [+Statement.Yield (+Expression.Yield (Some (+Integer 1)))];
  assert_parsed_equal
    "yield from a"
    [
      +Statement.YieldFrom
        (+Expression.Yield
          (Some
             (+Access [
                Access.Identifier ~~"a";
                Access.Identifier ~~"__iter__";
                Access.Call (+[]);
              ])));
    ];
  assert_parsed_equal
    "yield 1, 2"
    [
      +Statement.Yield
        (+Expression.Yield (Some (+Tuple [+Integer 1; +Integer 2])));
    ]


let test_comparison _ =
  assert_parsed_equal
    "a.b < 2"
    [
      +Expression
        (+ComparisonOperator {
           ComparisonOperator.left = +(Access (Access.create "a.b"));
           operator = ComparisonOperator.LessThan;
           right = +Integer 2;
         });
    ];
  assert_parsed_equal
    "1 in []"
    [
      +Expression
        (+ComparisonOperator {
           ComparisonOperator.left = +Integer 1;
           operator = ComparisonOperator.In;
           right = +List [];
         });
    ];
  assert_parsed_equal
    "1 is 1"
    [
      +Expression
        (+ComparisonOperator {
           ComparisonOperator.left = +Integer 1;
           operator = ComparisonOperator.Is;
           right = +Integer 1;
         });
    ];
  assert_parsed_equal
    "1 is not 1"
    [
      +Expression
        (+ComparisonOperator {
           ComparisonOperator.left = +Integer 1;
           operator = ComparisonOperator.IsNot;
           right = +Integer 1;
         });
    ];
  assert_parsed_equal
    "1 == 1"
    [
      +Expression
        (+ComparisonOperator {
           ComparisonOperator.left = +Integer 1;
           operator = ComparisonOperator.Equals;
           right = +Integer 1;
         });
    ];
  assert_parsed_equal
    "1 < 1 < 2"
    [
      +Expression
        (+BooleanOperator {
           BooleanOperator.left =
             (+ComparisonOperator {
                ComparisonOperator.left = +Integer 1;
                operator = ComparisonOperator.LessThan;
                right = +Integer 1;
              });
           operator = BooleanOperator.And;
           right =
             (+ComparisonOperator {
                ComparisonOperator.left = +Integer 1;
                operator = ComparisonOperator.LessThan;
                right = +Integer 2;
              });
         });
    ];
  assert_parsed_equal
    "1 < 1 is 2"
    [
      +Expression
        (+BooleanOperator {
           BooleanOperator.left =
             (+ComparisonOperator {
                ComparisonOperator.left = +Integer 1;
                operator = ComparisonOperator.LessThan;
                right = +Integer 1;
              });
           operator = BooleanOperator.And;
           right =
             (+ComparisonOperator {
                ComparisonOperator.left = +Integer 1;
                operator = ComparisonOperator.Is;
                right = +Integer 2;
              });
         });
    ]


let test_call _ =
  assert_parsed_equal
    "foo()"
    [
      +Expression
        (+Access [
           Access.Identifier ~~"foo";
           Access.Call (+[]);
         ]);
    ];
  assert_parsed_equal
    "foo(a for a in [])"
    [
      +Expression
        (+Access [
           Access.Identifier ~~"foo";
           Access.Call (+[
               {
                 Argument.name = None;
                 value = +Generator {
                   Comprehension.element = !"a";
                   generators = [
                     {
                       Comprehension.target = !"a";
                       iterator = +List [];
                       conditions = [];
                       async = false;
                     };
                   ];
                 };
               };
             ]);
         ]);
    ];
  assert_parsed_equal
    "foo(a for a in [],)"
    [
      +Expression
        (+Access [
           Access.Identifier ~~"foo";
           Access.Call (+[
               {
                 Argument.name = None;
                 value = +Generator {
                   Comprehension.element = !"a";
                   generators = [
                     {
                       Comprehension.target = !"a";
                       iterator = +List [];
                       conditions = [];
                       async = false;
                     };
                   ];
                 };
               }
             ]);
         ]);
    ];
  assert_parsed_equal
    "foo(1, 2,)"
    [
      +Expression
        (+Access [
           Access.Identifier ~~"foo";
           Access.Call (+[
               { Argument.name = None; value = +Integer 1 };
               { Argument.name = None; value = +Integer 2 };
             ]);
         ]);
    ];
  assert_parsed_equal
    "foo((1, 2))"
    [
      +Expression
        (+Access [
           Access.Identifier ~~"foo";
           Access.Call (+[{ Argument.name = None; value = (+Tuple [+Integer 1; +Integer 2]) }]);
         ]);
    ];
  assert_parsed_equal
    "foo(x, 1, (a, b))"
    [
      +Expression
        (+Access [
           Access.Identifier ~~"foo";
           Access.Call (+[
               { Argument.name = None; value = !"x"; };
               { Argument.name = None; value = +Integer 1 };
               { Argument.name = None; value = (+Tuple [!"a"; !"b"]) };
             ]);
         ]);
    ];
  assert_parsed_equal
    "a.foo(x)"
    [
      +Expression
        (+Access [
           Access.Identifier ~~"a";
           Access.Identifier ~~"foo";
           Access.Call (+[{ Argument.name = None; value = !"x"; }]);
         ]);
    ];
  assert_parsed_equal
    "foo(1, a = 1, b = 2)"
    [
      +Expression
        (+Access [
           Access.Identifier ~~"foo";
           Access.Call (+[
               { Argument.name = None; value = +Integer 1 };
               {
                 Argument.name = (Some ~+(~~"a"));
                 value = +Integer 1;
               };
               {
                 Argument.name = (Some ~+(~~"b"));
                 value = +Integer 2;
               };
             ]);
         ]);
    ];
  assert_parsed_equal
    "foo(1, a = 2, *args, **kwargs)"
    [
      +Expression
        (+Access [
           Access.Identifier ~~"foo";
           Access.Call (+[
               { Argument.name = None; value = +Integer 1 };
               {
                 Argument.name = (Some ~+(~~"a"));
                 value = (+Integer 2);
               };
               {
                 Argument.name = None;
                 value = +Starred (Starred.Once !"args");
               };
               {
                 Argument.name = None;
                 value = +Starred (Starred.Twice !"kwargs");
               };
             ]);
         ]);
    ]


let test_call_arguments_location _ =
  let source_code = "fun(1, second = 2)" in
  let statement = parse_single_statement source_code in
  let arguments =
    let collect_arguments accumulator access =
      match access with
      | Access.Call { Node.value = arguments; _ } -> arguments :: accumulator
      | _ -> accumulator
    in
    let print_argument { Argument.name; value } =
      Format.asprintf
        "name=%s value=%a"
        (Option.map name ~f:(fun { Node.value; location } ->
             Format.asprintf "%a/%s"
               Identifier.pp value
               (Location.Reference.show location))
         |> Option.value ~default:"(none)")
        Expression.pp value
    in
    Visit.collect_accesses statement
    |> List.hd_exn
    |> List.fold ~init:[] ~f:collect_arguments
    |> List.concat
    |> List.map ~f:print_argument
  in
  assert_equal
    ~printer:(String.concat ~sep:", ")
    [
      "name=(none) value=1";
      Format.sprintf "name=second/%d:1:7-1:13 value=2" (String.hash "test.py");
    ]
    arguments


let test_string _ =
  assert_parsed_equal "'foo'" [+Expression (+String (StringLiteral.create "foo"))];
  assert_parsed_equal "\"foo\"" [+Expression (+String (StringLiteral.create "foo"))];
  assert_parsed_equal "'''foo'''" [+Expression (+String (StringLiteral.create "foo"))];
  assert_parsed_equal "\"\"\"foo\"\"\"" [+Expression (+String (StringLiteral.create "foo"))];

  assert_parsed_equal "r'foo'" [+Expression (+String (StringLiteral.create "foo"))];
  assert_parsed_equal "R'foo'" [+Expression (+String (StringLiteral.create "foo"))];
  assert_parsed_equal "b'foo'" [+Expression (+String (StringLiteral.create ~bytes:true "foo"))];
  assert_parsed_equal "u'foo'" [+Expression (+String (StringLiteral.create "foo"))];
  assert_parsed_equal "ub'foo'" [+Expression (+String (StringLiteral.create ~bytes:true "foo"))];
  assert_parsed_equal "bR'foo'" [+Expression (+String (StringLiteral.create ~bytes:true "foo"))];
  assert_parsed_equal "'foo' 'bar'" [+Expression (+String (StringLiteral.create "foobar"))];

  assert_parsed_equal
    "f'foo'"
    [+Expression (+String (StringLiteral.create ~expressions:[] "foo"))];
  assert_parsed_equal
    "F'foo'"
    [+Expression (+String (StringLiteral.create ~expressions:[] "foo"))];
  assert_parsed_equal
    "f'foo' f'bar'"
    [+Expression (+String (StringLiteral.create ~expressions:[] "foobar"))];
  assert_parsed_equal
    "f'foo' 'bar'"
    [+Expression (+String (StringLiteral.create ~expressions:[] "foobar"))];

  (* TODO(T29598455): Should return a FormatString intead of a String *)
  assert_parsed_equal "'foo' f'bar'" [+Expression (+String (StringLiteral.create "foobar"))];

  assert_parsed_equal "\"'\"" [+Expression (+String (StringLiteral.create "'"))];
  assert_parsed_equal "'\"'" [+Expression (+String (StringLiteral.create "\""))];
  assert_parsed_equal "\"\\\"\"" [+Expression (+String (StringLiteral.create "\\\""))];
  assert_parsed_equal "\"\\'\"" [+Expression (+String (StringLiteral.create "\\\'"))];

  assert_parsed_equal "\"\"\"\nfoo\"\"\"" [+Expression (+String (StringLiteral.create "\nfoo"))];

  assert_parsed_equal "\"f.o\\no\"" [+Expression (+String (StringLiteral.create "f.o\\no"))];
  assert_parsed_equal
    "'a' + 'b'"
    [
      +Expression
        (+Access [
           Access.Expression (+String (StringLiteral.create "a"));
           Access.Identifier ~~"__add__";
           Access.Call (+[{ Argument.name = None; value = +String (StringLiteral.create "b") }]);
         ]);
    ];
  assert_parsed_equal
    "\"a\" + \"b\""
    [
      +Expression
        (+Access [
           Access.Expression (+String (StringLiteral.create "a"));
           Access.Identifier ~~"__add__";
           Access.Call (+[{ Argument.name = None; value = +String (StringLiteral.create "b") }]);
         ]);
    ];
  assert_parsed_equal
    "'''a''' + '''b'''"
    [
      +Expression
        (+Access [
           Access.Expression (+String (StringLiteral.create "a"));
           Access.Identifier ~~"__add__";
           Access.Call (+[{ Argument.name = None; value = +String (StringLiteral.create "b") }]);
         ]);
    ];
  assert_parsed_equal
    "\"\"\"a\"\"\" + \"\"\"b\"\"\""
    [
      +Expression
        (+Access [
           Access.Expression (+String (StringLiteral.create "a"));
           Access.Identifier ~~"__add__";
           Access.Call (+[{ Argument.name = None; value = +String (StringLiteral.create "b") }]);
         ]);
    ]


let test_class _ =
  assert_parsed_equal
    "@bar\nclass foo():\n\tpass"
    [
      +Class {
        Class.name = Access.create "foo";
        bases = [];
        body = [+Pass];
        decorators = [!"bar"];
        docstring = None;
      };
    ];
  assert_parsed_equal
    "class foo: pass"
    [
      +Class {
        Class.name = Access.create "foo";
        bases = [];
        body = [+Pass];
        decorators = [];
        docstring = None;
      };
    ];

  assert_parsed_equal
    "class foo():\n\tdef bar(): pass"
    [
      +Class {
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
            parent = Some (Access.create "foo");
          };
        ];
        decorators = [];
        docstring = None;
      };
    ];

  assert_parsed_equal
    "class foo():\n\tdef bar():\n\t\tdef baz(): pass"
    [
      +Class {
        Class.name = Access.create "foo";
        bases = [];
        body = [
          +Define {
            Define.name = Access.create "bar";
            parameters = [];
            body = [
              +Define {
                Define.name = Access.create "baz";
                parameters = [];
                body = [+Pass];
                decorators = [];
                docstring = None;
                return_annotation = None;
                async = false;
                parent = None;
              };
            ];
            decorators = [];
            docstring = None;
            return_annotation = None;
            async = false;
            parent = Some (Access.create "foo");
          };
        ];
        decorators = [];
        docstring = None;
      };
    ];

  assert_parsed_equal
    "class foo.bar: pass"
    [
      +Class {
        Class.name = (Access.create "foo.bar");
        bases = [];
        body = [+Pass];
        decorators = [];
        docstring = None;
      };
    ];
  assert_parsed_equal
    "class foo(1, 2):\n\t1"
    [
      +Class {
        Class.name = Access.create "foo";
        bases = [
          { Argument.name = None; value = +Integer 1 };
          { Argument.name = None; value = +Integer 2 };
        ];
        body = [+Expression (+Integer 1)];
        decorators = [];
        docstring = None;
      };
    ];
  assert_parsed_equal
    "class foo(1, **kwargs):\n\t1"
    [
      +Class {
        Class.name = Access.create "foo";
        bases = [
          { Argument.name = None; value = +Integer 1 };
          {
            Argument.name = None;
            value = +Starred (Starred.Twice !"kwargs");
          };
        ];
        body = [+Expression (+Integer 1)];
        decorators = [];
        docstring = None;
      };
    ];

  assert_parsed_equal
    "class foo:\n\tattribute: int = 1"
    [
      +Class {
        Class.name = Access.create "foo";
        bases = [];
        body = [
          +Assign {
            Assign.target = !"attribute";
            annotation = Some !"int";
            value = +Integer 1;
            parent = Some (Access.create "foo");
          };
        ];
        decorators = [];
        docstring = None;
      };
    ];
  assert_parsed_equal
    "class foo:\n\tattribute = 1 # type: int"
    [
      +Class {
        Class.name = Access.create "foo";
        bases = [];
        body = [
          +Assign {
            Assign.target = !"attribute";
            annotation = Some (+String (StringLiteral.create "int"));
            value = +Integer 1;
            parent = Some (Access.create "foo");
          };
        ];
        decorators = [];
        docstring = None;
      };
    ];
  assert_parsed_equal
    "class foo:\n\tattribute: int"
    [
      +Class {
        Class.name = Access.create "foo";
        bases = [];
        body = [
          +Assign {
            Assign.target = !"attribute";
            annotation = Some !"int";
            value = +Ellipses;
            parent = Some (Access.create "foo");
          };
        ];
        decorators = [];
        docstring = None;
      };
    ];

  assert_parsed_equal
    "class foo(superfoo):\n\tdef bar(): pass"
    [
      +Class {
        Class.name = Access.create "foo";
        bases = [{ Argument.name = None; value = !"superfoo" }];
        body = [
          +Define {
            Define.name = Access.create "bar";
            parameters = [];
            body = [+Pass];
            decorators = [];
            docstring = None;
            return_annotation = None;
            async = false;
            parent = Some (Access.create "foo");
          };
        ];
        decorators = [];
        docstring = None;
      };
    ];

  assert_parsed_equal
    "class foo():\n\tdef __init__(self): self.bar = 0"
    [
      +Class {
        Class.name = Access.create "foo";
        bases = [];
        body = [
          +Define {
            Define.name = Access.create "__init__";
            parameters = [
              +{
                Parameter.name = ~~"self";
                value = None;
                annotation = None;
              };
            ];
            body = [
              +Assign {
                Assign.target = +Access (Access.create "self.bar");
                annotation = None;
                value = +Integer 0;
                parent = None;
              };
            ];
            decorators = [];
            docstring = None;
            return_annotation = None;
            async = false;
            parent = Some (Access.create "foo");
          };
        ];
        decorators = [];
        docstring = None;
      };
    ];

  (* We mark parents for functions under if statements. *)
  assert_parsed_equal
    (trim_extra_indentation {|
      class foo():
        if True:
          def bar():
            pass
    |})
    [
      +Class {
        Class.name = Access.create "foo";
        bases = [];
        body = [
          +If {
            If.test = +Expression.True;
            body = [
              +Define {
                Define.name = Access.create "bar";
                parameters = [];
                body = [+Pass];
                decorators = [];
                docstring = None;
                return_annotation = None;
                async = false;
                parent = Some (Access.create "foo");
              };
            ];
            orelse = []
          }];
        decorators = [];
        docstring = None;
      };
    ]


let test_return _ =
  assert_parsed_equal
    "return"
    [
      +Return {
        Return.expression = None;
        is_implicit = false;
      }];
  assert_parsed_equal
    "return 1"
    [
      +Return {
        Return.expression = Some (+Integer 1);
        is_implicit = false;
      }]


let test_delete _ =
  assert_parsed_equal "del a" [+Delete !"a"];
  assert_parsed_equal
    "del a, b"
    [+Delete (+Tuple [!"a"; !"b"])]


let test_assign _ =
  assert_parsed_equal
    "a = b"
    [
      +Assign {
        Assign.target = !"a";
        annotation = None;
        value = !"b";
        parent = None;
      };
    ];
  assert_parsed_equal
    "a = 1"
    [
      +Assign {
        Assign.target = !"a";
        annotation = None;
        value = +Integer 1;
        parent = None;
      };
    ];
  assert_parsed_equal
    "a: int = 1"
    [
      +Assign {
        Assign.target = !"a";
        annotation = Some !"int";
        value = +Integer 1;
        parent = None;
      };
    ];
  assert_parsed_equal
    "a = 1  # type: int"
    [
      +Assign {
        Assign.target = !"a";
        annotation = Some (+String (StringLiteral.create "int"));
        value = +Integer 1;
        parent = None;
      };
    ];
  assert_parsed_equal
    "a = 1  # type: ignore"
    [
      +Assign {
        Assign.target = !"a";
        annotation = None;
        value = +Integer 1;
        parent = None;
      };
    ];
  assert_parsed_equal
    "a, b = 1"
    [
      +Assign {
        Assign.target = (+Tuple [!"a"; !"b"]);
        annotation = None;
        value = +Integer 1;
        parent = None;
      };
    ];
  assert_parsed_equal
    "a = a().foo()"
    [
      +Assign {
        Assign.target = !"a";
        annotation = None;
        value = +Access [
          Access.Identifier ~~"a";
          Access.Call (+[]);
          Access.Identifier ~~"foo";
          Access.Call (+[]);
        ];
        parent = None;
      };
    ];
  assert_parsed_equal
    "a = b = 1"
    [
      +Assign {
        Assign.target = !"a";
        annotation = None;
        value = +Integer 1;
        parent = None;
      };
      +Assign {
        Assign.target = !"b";
        annotation = None;
        value = +Integer 1;
        parent = None;
      };
    ];

  assert_parsed_equal
    "a = yield from b"
    [
      +Assign {
        Assign.target = !"a";
        annotation = None;
        value = +Expression.Yield (Some !"b");
        parent = None;
      };
    ];

  assert_parsed_equal
    "a += 1"
    [
      +Assign {
        Assign.target = !"a";
        annotation = None;
        value = +Access [
          Access.Identifier ~~"a";
          Access.Identifier ~~"__add__";
          Access.Call (+[{ Argument.name = None; value = +Integer 1 }]);
        ];
        parent = None;
      };
    ];
  assert_parsed_equal
    "a.b += 1"
    [
      +Assign {
        Assign.target = +Access (Access.create "a.b");
        annotation = None;
        value = +Access [
          Access.Identifier ~~"a";
          Access.Identifier ~~"b";
          Access.Identifier ~~"__add__";
          Access.Call (+[{ Argument.name = None; value = +Integer 1 }]);
        ];
        parent = None;
      };
    ];
  assert_parsed_equal
    "a = b if b else c"
    [
      +Assign {
        Assign.target = +Access (Access.create "a");
        annotation = None;
        value = +Ternary {
          Ternary.target = !"b";
          test = !"b";
          alternative = !"c";
        };
        parent = None;
      };
    ];
  assert_parsed_equal
    "a = b or c"
    [
      +Assign {
        Assign.target = +Access (Access.create "a");
        annotation = None;
        value = +BooleanOperator {
          BooleanOperator.left = !"b";
          operator = BooleanOperator.Or;
          right = !"c";
        };
        parent = None;
      };
    ];
  assert_parsed_equal
    "a = b or c or d"
    [
      +Assign {
        Assign.target = +Access (Access.create "a");
        annotation = None;
        value = +BooleanOperator {
          BooleanOperator.left = !"b";
          operator = BooleanOperator.Or;
          right = +BooleanOperator {
            BooleanOperator.left = !"c";
            operator = BooleanOperator.Or;
            right = !"d";
          };
        };
        parent = None;
      };
    ]


let test_for _ =
  assert_parsed_equal
    "for a in b: c\n"
    [
      +For {
        For.target = !"a";
        iterator = !"b";
        body = [+Expression !"c"];
        orelse = [];
        async = false;
      };
    ];
  assert_parsed_equal
    "for a, b in c: d\n"
    [
      +For {
        For.target = +Tuple [!"a"; !"b"];
        iterator = !"c";
        body = [+Expression !"d"];
        orelse = [];
        async = false;
      };
    ];
  assert_parsed_equal
    "for a in b: break\n"
    [
      +For {
        For.target = !"a";
        iterator = !"b";
        body = [+ Break];
        orelse = [];
        async = false;
      };
    ];
  assert_parsed_equal
    "for a in b: continue\n"
    [
      +For {
        For.target = !"a";
        iterator = !"b";
        body = [+ Continue];
        orelse = [];
        async = false;
      };
    ];
  assert_parsed_equal
    "async for a in b: c\n"
    [
      +For {
        For.target = !"a";
        iterator = !"b";
        body = [+Expression !"c"];
        orelse = [];
        async = true;
      };
    ];
  assert_parsed_equal
    "for a in b:\n\tc\n"
    [
      +For {
        For.target = !"a";
        iterator = !"b";
        body = [+Expression !"c"];
        orelse = [];
        async = false;
      };
    ];
  assert_parsed_equal
    "for a in b:\n\tc\nelse:\n\td\n"
    [
      +For {
        For.target = !"a";
        iterator = !"b";
        body = [+Expression !"c"];
        orelse = [+Expression !"d"];
        async = false;
      };
    ]


let test_while _ =
  assert_parsed_equal
    "while a: b\n"
    [
      +While {
        While.test = !"a";
        body = [+Expression !"b"];
        orelse = [];
      };
    ];
  assert_parsed_equal
    "while a:\n\tb\nelse:\n\tc\n"
    [
      +While {
        While.test = !"a";
        body = [+Expression !"b"];
        orelse = [+Expression !"c"];
      };
    ]


let test_if _ =
  assert_parsed_equal
    "if a: b\n"
    [
      +If {
        If.test = !"a";
        body = [+Expression !"b"];
        orelse = [];
      };
    ];
  assert_parsed_equal
    "if a: b\nelif c: d"
    [
      +If {
        If.test = !"a";
        body = [+Expression !"b"];
        orelse = [
          +If {
            If.test = !"c";
            body = [+Expression !"d"];
            orelse = [];
          };
        ];
      };
    ];
  assert_parsed_equal
    "if a:\n\n\tb\n"
    [
      +If {
        If.test = !"a";
        body = [+Expression !"b"];
        orelse = [];
      };
    ];
  assert_parsed_equal
    "if a:\n\tb\n\n\tc"
    [
      +If {
        If.test = !"a";
        body = [
          +Expression !"b";
          +Expression !"c";
        ];
        orelse = [];
      };
    ];
  assert_parsed_equal
    "if a:\n\tb\nelse:\n\tc\n"
    [
      +If {
        If.test = !"a";
        body = [+Expression !"b"];
        orelse = [+Expression !"c"];
      };
    ];
  assert_parsed_equal
    "if isinstance(x, int) and x > 0:\n\tb"
    [
      +If {
        If.test = +BooleanOperator {
          BooleanOperator.left = +Access [
            Access.Identifier ~~"isinstance";
            Access.Call (+[
                { Argument.name = None; value = !"x"; };
                { Argument.name = None; value = !"int" };
              ]);
          ];
          operator = BooleanOperator.And;
          right = +ComparisonOperator {
            ComparisonOperator.left = !"x";
            operator = ComparisonOperator.GreaterThan;
            right = +Integer 0;
          };
        };
        body = [+Expression !"b"];
        orelse = [];
      };
    ];
  assert_parsed_equal
    "if x and foo(x) > 0:\n\tb"
    [
      +If {
        If.test = +BooleanOperator {
          BooleanOperator.left = !"x";
          operator = BooleanOperator.And;
          right = +ComparisonOperator {
            ComparisonOperator.left = +Access [
              Access.Identifier ~~"foo";
              Access.Call (+[{ Argument.name = None; value = !"x" }]);
            ];
            operator = ComparisonOperator.GreaterThan;
            right = +Integer 0;
          };
        };
        body = [+Expression !"b"];
        orelse = [];
      };
    ];
  assert_parsed_equal
    "if a is 1 or True:\n\tb"
    [
      +If {
        If.test = +BooleanOperator {
          BooleanOperator.left = +ComparisonOperator {
            ComparisonOperator.left = !"a";
            operator = ComparisonOperator.Is;
            right = +Integer 1;
          };
          operator = BooleanOperator.Or;
          right = +True;
        };
        body = [+Expression !"b"];
        orelse = [];
      };
    ];
  assert_parsed_equal
    "if a is 1 or b is 1:\n\tc"
    [
      +If {
        If.test = +BooleanOperator {
          BooleanOperator.left = +ComparisonOperator {
            ComparisonOperator.left = !"a";
            operator = ComparisonOperator.Is;
            right = +Integer 1;
          };
          operator = BooleanOperator.Or;
          right = +ComparisonOperator {
            ComparisonOperator.left = !"b";
            operator = ComparisonOperator.Is;
            right = +Integer 1;
          };
        };
        body = [+Expression !"c"];
        orelse = [];
      };
    ];
  assert_parsed_equal
    "if a is 1 or b == 1:\n\tc"
    [
      +If {
        If.test = +BooleanOperator {
          BooleanOperator.left = +ComparisonOperator {
            ComparisonOperator.left = !"a";
            operator = ComparisonOperator.Is;
            right = +Integer 1;
          };
          operator = BooleanOperator.Or;
          right = +ComparisonOperator {
            ComparisonOperator.left = !"b";
            operator = ComparisonOperator.Equals;
            right = +Integer 1;
          };
        };
        body = [+Expression !"c"];
        orelse = [];
      };
    ];
  assert_parsed_equal
    "if a is 1 or b is 1 or c is 1:\n\td"
    [
      +If {
        If.test = +BooleanOperator {
          BooleanOperator.left = +ComparisonOperator {
            ComparisonOperator.left = !"a";
            operator = ComparisonOperator.Is;
            right = +Integer 1;
          };
          operator = BooleanOperator.Or;
          right = +BooleanOperator {
            BooleanOperator.left = +ComparisonOperator {
              ComparisonOperator.left = !"b";
              operator = ComparisonOperator.Is;
              right = +Integer 1;
            };
            operator = BooleanOperator.Or;
            right = +ComparisonOperator {
              ComparisonOperator.left = !"c";
              operator = ComparisonOperator.Is;
              right = +Integer 1;
            };
          };
        };
        body = [+Expression !"d"];
        orelse = [];
      };
    ]


let test_with _ =
  assert_parsed_equal
    "with a: b\n"
    [
      +With {
        With.items = [!"a", None];
        body = [+Expression !"b"];
        async = false;
      };
    ];
  assert_parsed_equal
    "with (yield from a): b\n"
    [
      +With {
        With.items = [+Expression.Yield (Some !"a"), None];
        body = [+Expression !"b"];
        async = false;
      };
    ];
  assert_parsed_equal
    "async with a: b\n"
    [
      +With {
        With.items = [!"a", None];
        body = [+Expression !"b"];
        async = true;
      };
    ];
  assert_parsed_equal
    "with a as b: b\n"
    [
      +With {
        With.items = [!"a", Some !"b"];
        body = [+Expression !"b"];
        async = false;
      };
    ];
  assert_parsed_equal
    "with a as b, c as d: b\n"
    [
      +With {
        With.items = [
          !"a", Some !"b";
          !"c", Some !"d";
        ];
        body = [+Expression !"b"];
        async = false;
      };
    ];
  assert_parsed_equal
    "with a, c as d: b\n"
    [
      +With {
        With.items = [
          !"a", None;
          !"c", Some !"d";
        ];
        body = [+Expression !"b"];
        async = false;
      };
    ]


let test_raise _ =
  assert_parsed_equal "raise" [+Raise None];
  assert_parsed_equal "raise a" [+Raise (Some !"a")];
  assert_parsed_equal "raise a from b" [+Raise (Some !"a")]


let test_try _ =
  assert_parsed_equal
    "try: a"
    [
      +Try {
        Try.body = [+Expression !"a"];
        handlers = [];
        orelse = [];
        finally = [];
      };
    ];
  assert_parsed_equal
    "try:\n\ta\nelse:\n\tb"
    [
      +Try {
        Try.body = [+Expression !"a"];
        handlers = [];
        orelse = [+Expression !"b"];
        finally = [];
      };
    ];
  assert_parsed_equal
    "try:\n\ta\nfinally:\n\tb"
    [
      +Try {
        Try.body = [+Expression !"a"];
        handlers = [];
        orelse = [];
        finally = [+Expression !"b"];
      };
    ];
  assert_parsed_equal
    "try:\n\ta\nexcept:\n\tb"
    [
      +Try {
        Try.body = [+Expression !"a"];
        handlers = [
          {
            Try.kind = None;
            name = None;
            handler_body = [+Expression !"b"];
          };
        ];
        orelse = [];
        finally = [];
      };
    ];
  assert_parsed_equal
    "try:\n\ta\nexcept a:\n\tb"
    [
      +Try {
        Try.body = [+Expression !"a"];
        handlers = [
          {
            Try.kind = Some !"a";
            name = None;
            handler_body = [+Expression !"b"];
          };
        ];
        orelse = [];
        finally = [];
      };
    ];
  assert_parsed_equal
    "try:\n\ta\nexcept a as b:\n\tb"
    [
      +Try {
        Try.body = [+Expression !"a"];
        handlers = [
          {
            Try.kind = Some !"a";
            name = Some ~~"b";
            handler_body = [+Expression !"b"];
          };
        ];
        orelse = [];
        finally = [];
      };
    ];
  assert_parsed_equal
    "try:\n\ta\nexcept a, b:\n\tb"
    [
      +Try {
        Try.body = [+Expression !"a"];
        handlers = [
          {
            Try.kind = Some !"a";
            name = Some ~~"b";
            handler_body = [+Expression !"b"];
          };
        ];
        orelse = [];
        finally = [];
      };
    ];
  assert_parsed_equal
    "try:\n\ta\nexcept (a, b) as c:\n\tb"
    [
      +Try {
        Try.body = [+Expression !"a"];
        handlers = [
          {
            Try.kind = Some (+Tuple [!"a"; !"b"]);
            name = Some ~~"c";
            handler_body = [+Expression !"b"];
          };
        ];
        orelse = [];
        finally = [];
      };
    ];
  assert_parsed_equal
    "try:\n\ta\nexcept a as b:\n\tb\nexcept d:\n\te"
    [
      +Try {
        Try.body = [+Expression !"a"];
        handlers = [
          {
            Try.kind = Some !"a";
            name = Some ~~"b";
            handler_body = [+Expression !"b"];
          };
          {
            Try.kind = Some !"d";
            name = None;
            handler_body = [+Expression !"e"];
          };
        ];
        orelse = [];
        finally = [];
      };
    ];
  assert_parsed_equal
    "try:\n\ta\nexcept:\n\tb\nelse:\n\tc\nfinally:\n\td"
    [
      +Try {
        Try.body = [+Expression !"a"];
        handlers = [
          {
            Try.kind = None;
            name = None;
            handler_body = [+Expression !"b"];
          };
        ];
        orelse = [+Expression !"c"];
        finally = [+Expression !"d"];
      };
    ]


let test_assert _ =
  assert_parsed_equal
    "assert a"
    [+Assert { Assert.test = !"a"; message = None }];
  assert_parsed_equal
    "assert a is b"
    [
      +Assert {
        Assert.test = +ComparisonOperator {
          ComparisonOperator.left = !"a";
          operator = ComparisonOperator.Is;
          right = !"b";
        };
        message = None;
      };
    ];
  assert_parsed_equal
    "assert a, b"
    [
      +Assert {
        Assert.test = !"a";
        message = Some !"b";
      };
    ];
  assert_parsed_equal
    "assert a is not None, 'b or c'"
    [
      +Assert {
        Assert.test = +ComparisonOperator {
          ComparisonOperator.left = !"a";
          operator = ComparisonOperator.IsNot;
          right = !"None";
        };
        message = Some (+String (StringLiteral.create "b or c"));
      }
    ]


let test_import _ =
  assert_parsed_equal
    "import a"
    [
      +Import {
        Import.from = None;
        imports = [
          {
            Import.name = Access.create "a";
            alias = None;
          };
        ];
      };
    ];
  assert_parsed_equal
    "import async"
    [
      +Import {
        Import.from = None;
        imports = [
          {
            Import.name = Access.create "async";
            alias = None;
          };
        ];
      };
    ];
  assert_parsed_equal
    "import a.async"
    [
      +Import {
        Import.from = None;
        imports = [
          {
            Import.name = (Access.create "a.async");
            alias = None;
          };
        ];
      };
    ];
  assert_parsed_equal
    "import a.b"
    [
      +Import {
        Import.from = None;
        imports = [
          {
            Import.name = (Access.create "a.b");
            alias = None;
          };
        ];
      };
    ];
  assert_parsed_equal
    "import a as b"
    [
      +Import {
        Import.from = None;
        imports = [
          {
            Import.name = Access.create "a";
            alias = Some (Access.create "b");
          };
        ];
      };
    ];
  assert_parsed_equal
    "import a as b, c, d as e"
    [
      +Import {
        Import.from = None;
        imports = [
          {
            Import.name = Access.create "a";
            alias = Some (Access.create "b");
          };
          {
            Import.name = Access.create "c";
            alias = None;
          };
          {
            Import.name = Access.create "d";
            alias = Some (Access.create "e");
          };
        ];
      };
    ];
  assert_parsed_equal
    "from a import b"
    [
      +Import {
        Import.from = Some (Access.create "a");
        imports = [
          {
            Import.name = Access.create "b";
            alias = None;
          };
        ];
      };
    ];
  assert_parsed_equal
    "from a import *"
    [
      +Import {
        Import.from = Some (Access.create "a");
        imports = [
          {
            Import.name = Access.create "*";
            alias = None;
          };
        ];
      };
    ];

  assert_parsed_equal
    "from . import b"
    [
      +Import {
        Import.from = Some (Access.create ".");
        imports = [
          {
            Import.name = Access.create "b";
            alias = None;
          };
        ];
      };
    ];
  assert_parsed_equal
    "from ...foo import b"
    [
      +Import {
        Import.from = Some (Access.create "...foo");
        imports = [
          {
            Import.name = Access.create "b";
            alias = None;
          };
        ];
      };
    ];
  assert_parsed_equal
    "from .....foo import b"
    [
      +Import {
        Import.from = Some (Access.create ".....foo");
        imports = [
          {
            Import.name = Access.create "b";
            alias = None;
          };
        ];
      };
    ];
  assert_parsed_equal
    "from .a import b"
    [
      +Import {
        Import.from = Some (Access.create ".a");
        imports = [
          {
            Import.name = Access.create "b";
            alias = None;
          };
        ];
      };
    ];
  assert_parsed_equal
    "from ..a import b"
    [
      +Import {
        Import.from = Some (Access.create "..a");
        imports = [
          {
            Import.name = Access.create "b";
            alias = None;
          };
        ];
      };
    ];

  assert_parsed_equal
    "from a import (b, c)"
    [
      +Import {
        Import.from = Some (Access.create "a");
        imports = [
          {
            Import.name = Access.create "b";
            alias = None;
          };
          {
            Import.name = Access.create "c";
            alias = None;
          };
        ];
      };
    ];
  assert_parsed_equal
    "from a.b import c"
    [
      +Import {
        Import.from = Some (Access.create "a.b");
        imports = [
          {
            Import.name = Access.create "c";
            alias = None;
          };
        ]
      };
    ];
  assert_parsed_equal
    "from f import a as b, c, d as e"
    [
      +Import {
        Import.from = Some (Access.create "f");
        imports = [
          {
            Import.name = Access.create "a";
            alias = Some (Access.create "b");
          };
          {
            Import.name = Access.create "c";
            alias = None;
          };
          {
            Import.name = Access.create "d";
            alias = Some (Access.create "e");
          };
        ];
      };
    ]


let test_end_position _ =
  let source_code = "def a():\n    return None" in
  let statement = parse_single_statement source_code in
  let location = statement.Node.location in
  let expected_location = {
    Location.path = String.hash "test.py";
    start = {Location.line = 1; Location.column = 0};
    stop = {Location.line = 2; Location.column = 15};
  } in
  assert_equal
    ~cmp:Location.Reference.equal
    ~printer:(fun location -> Format.asprintf "%a" Location.Reference.pp location)
    ~pp_diff:(diff ~print:Location.Reference.pp)
    expected_location
    location


let assert_statement_location
    ~statement
    ~start:(start_line, start_column)
    ~stop:(stop_line, stop_column) =
  let actual_location = statement.Node.location in
  let expected_location = {
    Location.path = String.hash "test.py";
    start = {Location.line = start_line; Location.column = start_column};
    stop = {Location.line = stop_line; Location.column = stop_column};
  } in
  assert_equal
    ~cmp:Location.Reference.equal
    ~printer:(fun location -> Format.asprintf "%a" Location.Reference.pp location)
    ~pp_diff:(diff ~print:Location.Reference.pp)
    expected_location
    actual_location


let test_string_locations _ =
  let test_one source_code ~start ~stop =
    let statement = parse_single_statement source_code in
    assert_statement_location ~statement ~start ~stop
  in

  test_one "'literal'" ~start:(1, 0) ~stop:(1, 9);
  test_one "\"literal\"" ~start:(1, 0) ~stop:(1, 9);
  test_one "'''multiline\nliteral'''\n" ~start:(1, 0) ~stop:(2, 10);
  test_one "\"\"\"multiline\nliteral\"\"\"\n" ~start:(1, 0) ~stop:(2, 10)


let test_multiline_strings_positions _ =
  let test_one source_code =
    let statement = parse_last_statement source_code in
    assert_statement_location ~statement ~start:(5, 0) ~stop:(5, 4)
  in

  (* variations of the multiline string:
     '''
     AAA
     BBB
     '''
     pass
  *)
  test_one "'''\nAAA\nBBB\n'''\npass";
  test_one "\"\"\"\nAAA\nBBB\n\"\"\"\npass";
  (* variations of the multiline string: (note the backslash in line 2)
     '''
     AAA \
     BBB
     '''
     pass
  *)
  test_one "'''\nAAA \\\nBBB\n'''\npass";
  test_one "\"\"\"\nAAA \\\nBBB\n\"\"\"\npass"


let test_global _ =
  assert_parsed_equal "global a" [+Global [~~"a"]];
  assert_parsed_equal
    "global a, b"
    [+Global [~~"a"; ~~"b"]]


let test_tuple _ =
  assert_parsed_equal "1" [+Expression (+Integer 1)];
  assert_parsed_equal "()" [+Expression (+Tuple [])];
  assert_parsed_equal "(1,)" [+Expression (+Tuple [+Integer 1])];
  assert_parsed_equal "1, 2" [+Expression (+Tuple [+Integer 1; +Integer 2])];
  assert_parsed_equal
    "1, 1 + 1"
    [
      +Expression
        (+Tuple [
           +Integer 1;
           +Access [
             Access.Expression (+Integer 1);
             Access.Identifier ~~"__add__";
             Access.Call (+[{ Argument.name = None; value = +Integer 1 }]);
           ];
         ]);
    ];
  assert_parsed_equal
    "1, 2 if 3 else 4"
    [
      +Expression
        (+Tuple [
           +Integer 1;
           +Ternary {
             Ternary.target = +Integer 2;
             test = +Integer 3;
             alternative = +Integer 4;
           };
         ]);
    ];
  assert_parsed_equal
    "1 + 1, 1"
    [
      +Expression
        (+Tuple [
           +Access [
             Access.Expression (+Integer 1);
             Access.Identifier ~~"__add__";
             Access.Call (+[{ Argument.name = None; value = +Integer 1 }]);
           ];
           +Integer 1;
         ]);
    ];
  assert_parsed_equal
    "(1, 2, 3)"
    [+Expression (+Tuple [+Integer 1; +Integer 2; +Integer 3])]


let test_stubs _ =
  assert_parsed_equal
    "a = ..."
    [
      +Assign {
        Assign.target = !"a";
        annotation = None;
        value = +Ellipses;
        parent = None;
      };
    ];

  assert_parsed_equal
    "a: int = ..."
    [
      +Assign {
        Assign.target = !"a";
        annotation = Some !"int";
        value = +Ellipses;
        parent = None;
      };
    ];

  assert_parsed_equal
    "a = ... # type: int"
    [
      +Assign {
        Assign.target = !"a";
        annotation = Some (+String (StringLiteral.create "int"));
        value = +Ellipses;
        parent = None;
      };
    ];

  assert_parsed_equal
    "a = ... # type: Tuple[str]"
    [
      +Assign {
        Assign.target = !"a";
        annotation = Some (+String (StringLiteral.create "Tuple[str]"));
        value = +Ellipses;
        parent = None;
      };
    ];

  assert_parsed_equal
    "a = ... # type: Tuple[str, ...]"
    [
      +Assign {
        Assign.target = !"a";
        annotation = Some (+String (StringLiteral.create "Tuple[str, ...]"));
        value = +Ellipses;
        parent = None;
      };
    ];

  assert_parsed_equal
    "a: Optional[int] = ..."
    [
      +Assign {
        Assign.target = !"a";
        annotation = Some
            (+Access [
               Access.Identifier ~~"Optional";
               Access.Identifier ~~"__getitem__";
               Access.Call
                 (+[{ Argument.name = None; value = +Access [Access.Identifier ~~"int"] }]);
             ]);
        value = +Ellipses;
        parent = None;
      };
    ];

  assert_parsed_equal
    "class A:\n\ta = ... # type: int"
    [
      +Class {
        Class.name = Access.create "A";
        bases = [];
        body = [
          +Assign {
            Assign.target = !"a";
            annotation = Some (+String (StringLiteral.create "int"));
            value = +Ellipses;
            parent = Some (Access.create "A");
          };
        ];
        decorators = [];
        docstring = None;
      };
    ];

  assert_parsed_equal
    "def foo(a): ..."
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = None;
          };
        ];
        body = [+Expression (+Ellipses)];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ];
  assert_parsed_equal
    "def foo(a): ... # type: ignore"
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = None;
          };
        ];
        body = [+Expression (+Ellipses)];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ];
  assert_parsed_equal
    "def foo(a):\n\t..."
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = None;
            annotation = None;
          };
        ];
        body = [+Expression (+Ellipses)];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ];

  assert_parsed_equal
    "@overload\ndef foo(a: int = ...):  ..."
    [
      +Define {
        Define.name = Access.create "foo";
        parameters = [
          +{
            Parameter.name = ~~"a";
            value = Some (+Ellipses);
            annotation = Some !"int";
          };
        ];
        body = [+Expression (+Ellipses)];
        decorators = [!"overload"];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      };
    ];

  assert_parsed_equal
    "class foo(): ..."
    [
      +Class {
        Class.name = Access.create "foo";
        bases = [];
        body = [+Expression (+Ellipses)];
        decorators = [];
        docstring = None;
      };
    ];
  assert_parsed_equal
    "class foo():\n\t..."
    [
      +Class {
        Class.name = Access.create "foo";
        bases = [];
        body = [+Expression (+Ellipses)];
        decorators = [];
        docstring = None;
      };
    ];
  assert_parsed_equal
    "class foo(): ... # type: ignore"
    [
      +Class {
        Class.name = Access.create "foo";
        bases = [];
        body = [+Expression (+Ellipses)];
        decorators = [];
        docstring = None;
      };
    ]


let test_nonlocal _ =
  assert_parsed_equal "nonlocal a" [+Nonlocal [~~"a"]];
  assert_parsed_equal
    "nonlocal a, b"
    [+Nonlocal [~~"a"; ~~"b"]]


let test_ellipsis _ =
  assert_parsed_equal
    "def __init__(debug = ...):\n\tpass"
    [
      +Define {
        Define.name = Access.create "__init__";
        parameters = [
          +{
            Parameter.name = ~~"debug";
            value = Some (+Ellipses);
            annotation = None;
          };
        ];
        body = [+Pass];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      }
    ];
  assert_parsed_equal
    "if x is ...:\n\tpass"
    [
      +If {
        If.test = +ComparisonOperator {
          ComparisonOperator.left = !"x";
          operator = ComparisonOperator.Is;
          right = +Ellipses;
        };
        body = [+Pass];
        orelse = [];
      };
    ]


let () =
  "parsing">:::[
    "lexer">::test_lexer;
    "number">::test_number;
    "await">::test_await;
    "access">::test_access;
    "starred">::test_starred;
    "compound">::test_compound;
    "define">::test_define;
    "boolean_operator">::test_boolean_operator;
    "binary_operator">::test_binary_operator;
    "unary_operator">::test_unary_operator;
    "lambda">::test_lambda;
    "ternary">::test_ternary;
    "dictionary">::test_dictionary;
    "list">::test_list;
    "set">::test_set;
    "generator">::test_generator;
    "yield">::test_yield;
    "comparison">::test_comparison;
    "call">::test_call;
    "call_arguments_location">::test_call_arguments_location;
    "string">::test_string;
    "class">::test_class;
    "return">::test_return;
    "delete">::test_delete;
    "assign">::test_assign;
    "for">::test_for;
    "while">::test_while;
    "if">::test_if;
    "with">::test_with;
    "raise">::test_raise;
    "try">::test_try;
    "assert">::test_assert;
    "import">::test_import;
    "end_position">::test_end_position;
    "string_locations">::test_string_locations;
    "multiline_strings_positions">::test_multiline_strings_positions;
    "global">::test_global;
    "tuple">::test_tuple;
    "stubs">::test_stubs;
    "nonlocal">::test_nonlocal;
    "ellipsis">::test_ellipsis;
  ]
  |> Test.run
