(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Taint
open TestHelper

let assert_model_query_results ?model_path ~context ~models_source ~source ~expected () =
  let { TestEnvironment.model_query_results; _ } =
    initialize ?model_path ~force_pyre1:true ~models_source ~context source
  in
  let actual = ModelQueryExecution.ExecutionResult.dump_to_string model_query_results in
  let dumped_models_equal left right =
    let left, right = Yojson.Safe.from_string left, Yojson.Safe.from_string right in
    Yojson.Safe.equal left right
  in
  assert_equal ~cmp:dumped_models_equal ~printer:Core.Fn.id expected actual


let test_dump_model_query_results context =
  assert_model_query_results
    ~context
    ~models_source:
      {|
      ModelQuery(
        name = "get_foo",
        find = "functions",
        where = name.matches("foo"),
        model = Returns(TaintSource[Test])
      )
      ModelQuery(
        name = "get_bar",
        find = "functions",
        where = name.matches("bar"),
        model = Returns(TaintSource[Test])
      )
      ModelQuery(
        name = "get_fooo",
        find = "functions",
        where = name.matches("fooo"),
        model = Returns(TaintSource[Test])
      )
    |}
    ~source:
      {|
      def foo1(): ...
      def foo2(): ...
      def bar(): ...
      def barfooo(): ...
      |}
    ~expected:
      {|[
  {
    "get_bar": [
      {
        "callable": "test.bar",
        "sources": [
          {
            "port": "result",
            "taint": [
              { "kinds": [ { "kind": "Test" } ], "declaration": null }
            ]
          }
        ],
        "model_generators": [ "get_bar" ],
        "modes": [ "Obscure" ]
      },
      {
        "callable": "test.barfooo",
        "sources": [
          {
            "port": "result",
            "taint": [
              { "kinds": [ { "kind": "Test" } ], "declaration": null }
            ]
          }
        ],
        "model_generators": [ "get_bar", "get_foo", "get_fooo" ],
        "modes": [ "Obscure" ]
      }
    ]
  },
  {
    "get_foo": [
      {
        "callable": "test.barfooo",
        "sources": [
          {
            "port": "result",
            "taint": [
              { "kinds": [ { "kind": "Test" } ], "declaration": null }
            ]
          }
        ],
        "model_generators": [ "get_bar", "get_foo", "get_fooo" ],
        "modes": [ "Obscure" ]
      },
      {
        "callable": "test.foo1",
        "sources": [
          {
            "port": "result",
            "taint": [
              { "kinds": [ { "kind": "Test" } ], "declaration": null }
            ]
          }
        ],
        "model_generators": [ "get_foo" ],
        "modes": [ "Obscure" ]
      },
      {
        "callable": "test.foo2",
        "sources": [
          {
            "port": "result",
            "taint": [
              { "kinds": [ { "kind": "Test" } ], "declaration": null }
            ]
          }
        ],
        "model_generators": [ "get_foo" ],
        "modes": [ "Obscure" ]
      }
    ]
  },
  {
    "get_fooo": [
      {
        "callable": "test.barfooo",
        "sources": [
          {
            "port": "result",
            "taint": [
              { "kinds": [ { "kind": "Test" } ], "declaration": null }
            ]
          }
        ],
        "model_generators": [ "get_bar", "get_foo", "get_fooo" ],
        "modes": [ "Obscure" ]
      }
    ]
  }
]|}
    ();
  (* Test methods *)
  assert_model_query_results
    ~context
    ~models_source:
      {|
        ModelQuery(
          name = "get_Base_child_sources",
          find = "methods",
          where = [cls.name.matches("Base")],
          model = [
            Parameters(TaintSource[Test], where=index.equals(0)),
          ]
        )
      |}
    ~source:
      {|
      class Base:
          def foo(self, x):
              return 0
          def baz(self, y):
              return 0
      |}
    ~expected:
      {|[
  {
    "get_Base_child_sources": [
      {
        "callable": "test.Base.baz",
        "parameter_sources": [
          {
            "port": "formal(self, position=0)",
            "taint": [
              { "kinds": [ { "kind": "Test" } ], "declaration": null }
            ]
          }
        ],
        "model_generators": [ "get_Base_child_sources" ]
      },
      {
        "callable": "test.Base.foo",
        "parameter_sources": [
          {
            "port": "formal(self, position=0)",
            "taint": [
              { "kinds": [ { "kind": "Test" } ], "declaration": null }
            ]
          }
        ],
        "model_generators": [ "get_Base_child_sources" ]
      }
    ]
  }
]|}
    ();
  (* Test correct ModelQuery<->sources for same callable *)
  (* TODO(T213691068): Since we write models to shared memory during model generation,
   * we join models from different queries. Therefore, when dumping models, we are dumping
   * the joined model, which includes results from multiple queries. We should improve that,
   * but this is challenging to do efficiently. *)
  assert_model_query_results
    ~context
    ~models_source:
      {|
      ModelQuery(
        name = "ModelQueryA",
        find = "functions",
        where = name.matches("foo"),
        model = Returns(TaintSource[Test])
      )
      ModelQuery(
        name = "ModelQueryB",
        find = "functions",
        where = name.matches("foo"),
        model = Returns(TaintSource[UserControlled])
      )
    |}
    ~source:{|
      def foo(x): ...
      |}
    ~expected:
      {|[
  {
    "ModelQueryA": [
      {
        "callable": "test.foo",
        "sources": [
          {
            "port": "result",
            "taint": [
              {
                "kinds": [ { "kind": "Test" }, { "kind": "UserControlled" } ],
                "declaration": null
              }
            ]
          }
        ],
        "model_generators": [ "ModelQueryA", "ModelQueryB" ],
        "modes": [ "Obscure" ]
      }
    ]
  },
  {
    "ModelQueryB": [
      {
        "callable": "test.foo",
        "sources": [
          {
            "port": "result",
            "taint": [
              {
                "kinds": [ { "kind": "Test" }, { "kind": "UserControlled" } ],
                "declaration": null
              }
            ]
          }
        ],
        "model_generators": [ "ModelQueryA", "ModelQueryB" ],
        "modes": [ "Obscure" ]
      }
    ]
  }
]|}
    ();
  (* TODO(T123305678) Add test for attributes *)
  (* Test correct ModelQuery<->sources for same callable *)
  assert_model_query_results
    ~context
    ~models_source:
      {|
        ModelQuery(
          name = "get_parent_of_baz_class_sources",
          find = "methods",
          where = [
            cls.any_child(
              cls.name.matches("Baz"),
              is_transitive=False
            ),
            name.matches("init")
          ],
          model = [
            Parameters(TaintSource[Test], where=[
                Not(name.equals("self")),
                Not(name.equals("a"))
            ])
          ]
        )
        |}
    ~source:
      {|
      class Foo:
        def __init__(self, a, b):
          ...
      class Bar(Foo):
        def __init__(self, a, b):
          ...
      class Baz(Bar):
        def __init__(self, a, b):
          ...
      |}
    ~expected:
      {|[
  {
    "get_parent_of_baz_class_sources": [
      {
        "callable": "test.Bar.__init__",
        "parameter_sources": [
          {
            "port": "formal(b, position=2)",
            "taint": [
              { "kinds": [ { "kind": "Test" } ], "declaration": null }
            ]
          }
        ],
        "model_generators": [ "get_parent_of_baz_class_sources" ],
        "modes": [ "Obscure" ]
      },
      {
        "callable": "test.Baz.__init__",
        "parameter_sources": [
          {
            "port": "formal(b, position=2)",
            "taint": [
              { "kinds": [ { "kind": "Test" } ], "declaration": null }
            ]
          }
        ],
        "model_generators": [ "get_parent_of_baz_class_sources" ],
        "modes": [ "Obscure" ]
      }
    ]
  }
]|}
    ();
  assert_model_query_results
    ~context
    ~models_source:
      {|
        ModelQuery(
          name = "get_parent_of_baz_class_transitive_sources",
          find = "methods",
          where = [
            cls.any_child(
              AnyOf(
                cls.decorator(
                  name.matches("anything")
                ),
                AllOf(
                  Not(cls.name.matches("Foo")),
                  Not(cls.name.matches("Bar")),
                )
              ),
              is_transitive=True
            ),
            name.matches("init")
          ],
          model = [
            Parameters(TaintSource[Test], where=[
                Not(name.equals("self")),
                Not(name.equals("a"))
            ])
          ]
        )
        |}
    ~source:
      {|
      class Foo:
        def __init__(self, a, b):
          ...
      class Bar(Foo):
        def __init__(self, a, b):
          ...
      class Baz(Bar):
        def __init__(self, a, b):
          ...
      |}
    ~expected:
      {|[
  {
    "get_parent_of_baz_class_transitive_sources": [
      {
        "callable": "test.Bar.__init__",
        "parameter_sources": [
          {
            "port": "formal(b, position=2)",
            "taint": [
              { "kinds": [ { "kind": "Test" } ], "declaration": null }
            ]
          }
        ],
        "model_generators": [ "get_parent_of_baz_class_transitive_sources" ],
        "modes": [ "Obscure" ]
      },
      {
        "callable": "test.Baz.__init__",
        "parameter_sources": [
          {
            "port": "formal(b, position=2)",
            "taint": [
              { "kinds": [ { "kind": "Test" } ], "declaration": null }
            ]
          }
        ],
        "model_generators": [ "get_parent_of_baz_class_transitive_sources" ],
        "modes": [ "Obscure" ]
      },
      {
        "callable": "test.Foo.__init__",
        "parameter_sources": [
          {
            "port": "formal(b, position=2)",
            "taint": [
              { "kinds": [ { "kind": "Test" } ], "declaration": null }
            ]
          }
        ],
        "model_generators": [ "get_parent_of_baz_class_transitive_sources" ],
        "modes": [ "Obscure" ]
      }
    ]
  }
]|}
    ();
  (* Test functions *)
  assert_model_query_results
    ~context
    ~models_source:
      {|
      ModelQuery(
        name = "get_foo",
        find = "functions",
        where = name.matches("foo"),
        model = Returns(TaintSource[Test])
      )
      ModelQuery(
        name = "get_bar",
        find = "functions",
        where = name.matches("bar"),
        model = Returns(TaintSource[Test])
      )
      ModelQuery(
        name = "get_fooo",
        find = "functions",
        where = name.matches("fooo"),
        model = Returns(TaintSource[Test])
      )
    |}
    ~source:
      {|
      def foo1(): ...
      def foo2(): ...
      def bar(): ...
      def barfooo(): ...
      |}
    ~model_path:(PyrePath.create_absolute "/a/b.pysa")
    ~expected:
      {|[
  {
    "/a/b.pysa/get_bar": [
      {
        "callable": "test.bar",
        "sources": [
          {
            "port": "result",
            "taint": [
              { "kinds": [ { "kind": "Test" } ], "declaration": null }
            ]
          }
        ],
        "model_generators": [ "/a/b.pysa/get_bar" ],
        "modes": [ "Obscure" ]
      },
      {
        "callable": "test.barfooo",
        "sources": [
          {
            "port": "result",
            "taint": [
              { "kinds": [ { "kind": "Test" } ], "declaration": null }
            ]
          }
        ],
        "model_generators": [
          "/a/b.pysa/get_bar", "/a/b.pysa/get_foo", "/a/b.pysa/get_fooo"
        ],
        "modes": [ "Obscure" ]
      }
    ]
  },
  {
    "/a/b.pysa/get_foo": [
      {
        "callable": "test.barfooo",
        "sources": [
          {
            "port": "result",
            "taint": [
              { "kinds": [ { "kind": "Test" } ], "declaration": null }
            ]
          }
        ],
        "model_generators": [
          "/a/b.pysa/get_bar", "/a/b.pysa/get_foo", "/a/b.pysa/get_fooo"
        ],
        "modes": [ "Obscure" ]
      },
      {
        "callable": "test.foo1",
        "sources": [
          {
            "port": "result",
            "taint": [
              { "kinds": [ { "kind": "Test" } ], "declaration": null }
            ]
          }
        ],
        "model_generators": [ "/a/b.pysa/get_foo" ],
        "modes": [ "Obscure" ]
      },
      {
        "callable": "test.foo2",
        "sources": [
          {
            "port": "result",
            "taint": [
              { "kinds": [ { "kind": "Test" } ], "declaration": null }
            ]
          }
        ],
        "model_generators": [ "/a/b.pysa/get_foo" ],
        "modes": [ "Obscure" ]
      }
    ]
  },
  {
    "/a/b.pysa/get_fooo": [
      {
        "callable": "test.barfooo",
        "sources": [
          {
            "port": "result",
            "taint": [
              { "kinds": [ { "kind": "Test" } ], "declaration": null }
            ]
          }
        ],
        "model_generators": [
          "/a/b.pysa/get_bar", "/a/b.pysa/get_foo", "/a/b.pysa/get_fooo"
        ],
        "modes": [ "Obscure" ]
      }
    ]
  }
]|}
    ();
  ()


let () =
  "dump_model_query_results"
  >::: ["dump_model_query_results" >:: test_dump_model_query_results]
  |> Test.run
