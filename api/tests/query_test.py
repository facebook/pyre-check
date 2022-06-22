# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import call, MagicMock, patch

from .. import connection, query


class QueryAPITest(unittest.TestCase):
    def test_defines(self) -> None:
        pyre_connection = MagicMock()
        pyre_connection.query_server.return_value = {
            "response": [
                {
                    "name": "a.foo",
                    "parameters": [{"name": "x", "annotation": "int"}],
                    "return_annotation": "int",
                }
            ]
        }
        self.assertEqual(
            query.defines(pyre_connection, ["a"]),
            [
                query.Define(
                    name="a.foo",
                    parameters=[query.DefineParameter(name="x", annotation="int")],
                    return_annotation="int",
                )
            ],
        )
        pyre_connection.query_server.side_effect = [
            {
                "response": [
                    {
                        "name": "a.foo",
                        "parameters": [{"name": "x", "annotation": "int"}],
                        "return_annotation": "int",
                    }
                ]
            },
            {
                "response": [
                    {
                        "name": "b.bar",
                        "parameters": [{"name": "y", "annotation": "str"}],
                        "return_annotation": "int",
                    }
                ]
            },
        ]
        self.assertEqual(
            query.defines(pyre_connection, ["a", "b"], batch_size=1),
            [
                query.Define(
                    name="a.foo",
                    parameters=[query.DefineParameter(name="x", annotation="int")],
                    return_annotation="int",
                ),
                query.Define(
                    name="b.bar",
                    parameters=[query.DefineParameter(name="y", annotation="str")],
                    return_annotation="int",
                ),
            ],
        )
        with patch(f"{query.__name__}._defines") as defines_implementation:
            defines_implementation.return_value = []
            query.defines(pyre_connection, ["a", "b", "c", "d"], batch_size=2)
            defines_implementation.assert_has_calls(
                [call(pyre_connection, ["a", "b"]), call(pyre_connection, ["c", "d"])]
            )
            defines_implementation.reset_calls()
            query.defines(
                pyre_connection, ["a", "b", "c", "d", "e", "f", "g"], batch_size=2
            )
            defines_implementation.assert_has_calls(
                [
                    call(pyre_connection, ["a", "b"]),
                    call(pyre_connection, ["c", "d"]),
                    call(pyre_connection, ["e", "f"]),
                    call(pyre_connection, ["g"]),
                ]
            )
        with self.assertRaises(ValueError):
            query.defines(pyre_connection, ["a", "b"], batch_size=0)

        with self.assertRaises(ValueError):
            query.defines(pyre_connection, ["a", "b"], batch_size=-1)

    def test_get_class_hierarchy(self) -> None:
        pyre_connection = MagicMock()
        pyre_connection.query_server.return_value = {
            "response": [{"Foo": ["object"]}, {"object": []}]
        }
        hierarchy = query.get_class_hierarchy(pyre_connection)
        assert hierarchy is not None
        self.assertEqual(hierarchy.hierarchy, {"Foo": ["object"], "object": []})
        # Reverse hierarchy.
        self.assertEqual(hierarchy.reverse_hierarchy, {"object": ["Foo"], "Foo": []})
        # Superclasses.
        self.assertEqual(hierarchy.superclasses("Foo"), ["object"])
        self.assertEqual(hierarchy.superclasses("object"), [])
        self.assertEqual(hierarchy.superclasses("Nonexistent"), [])
        # Subclasses.
        self.assertEqual(hierarchy.subclasses("object"), ["Foo"])
        self.assertEqual(hierarchy.subclasses("Foo"), [])
        self.assertEqual(hierarchy.subclasses("Nonexistent"), [])

        pyre_connection.query_server.return_value = {
            "response": [
                {"Foo": ["object"]},
                {"object": []},
                # This should never happen in practice, but unfortunately is something
                # to consider due to the type of the JSON returned. The last entry wins.
                {"Foo": ["Bar", "Baz"]},
                {"Bar": ["object"]},
            ]
        }
        class_hierarchy = query.get_class_hierarchy(pyre_connection)
        assert class_hierarchy is not None
        self.assertEqual(
            class_hierarchy.hierarchy,
            {"Foo": ["Bar", "Baz"], "Bar": ["object"], "object": []},
        )
        self.assertEqual(class_hierarchy.superclasses("Foo"), ["Bar", "Baz"])

    def test_annotations_per_file(self) -> None:
        test_data: connection.PyreQueryResult = {
            "response": [
                {
                    "response": [
                        {
                            "path": "tensor.py",
                            "types": [
                                {
                                    "location": {
                                        "start": {"line": 1, "column": 0},
                                        "stop": {"line": 1, "column": 5},
                                    },
                                    "annotation": "int",
                                },
                                {
                                    "location": {
                                        "start": {"line": 1, "column": 4},
                                        "stop": {"line": 1, "column": 5},
                                    },
                                    "annotation": "typing_extensions.Literal[1]",
                                },
                            ],
                        }
                    ]
                },
                {
                    "response": [
                        {
                            "path": "test.py",
                            "types": [
                                {
                                    "location": {
                                        "start": {"line": 1, "column": 0},
                                        "stop": {"line": 1, "column": 1},
                                    },
                                    "annotation": "typing_extensions.Literal[2]",
                                }
                            ],
                        }
                    ]
                },
            ]
        }
        self.assertEqual(
            query._annotations_per_file(test_data),
            {
                "tensor.py": [
                    query.Annotation(
                        type_name="int",
                        start=query.Position(line=1, column=0),
                        stop=query.Position(line=1, column=5),
                    ),
                    query.Annotation(
                        type_name="typing_extensions.Literal[1]",
                        start=query.Position(line=1, column=4),
                        stop=query.Position(line=1, column=5),
                    ),
                ],
                "test.py": [
                    query.Annotation(
                        type_name="typing_extensions.Literal[2]",
                        start=query.Position(line=1, column=0),
                        stop=query.Position(line=1, column=1),
                    )
                ],
            },
        )

    def test_annotations_per_file_file_not_found(self) -> None:
        test_data: connection.PyreQueryResult = {
            "response": [
                {
                    "response": [
                        {
                            "path": "tensor.py",
                            "types": [
                                {
                                    "location": {
                                        "start": {"line": 1, "column": 0},
                                        "stop": {"line": 1, "column": 5},
                                    },
                                    "annotation": "int",
                                },
                            ],
                        }
                    ]
                },
                {"error": "Some error"},
            ]
        }
        self.assertEqual(
            query._annotations_per_file(test_data),
            {
                "tensor.py": [
                    query.Annotation(
                        type_name="int",
                        start=query.Position(line=1, column=0),
                        stop=query.Position(line=1, column=5),
                    ),
                ],
            },
        )

    def test_get_superclasses(self) -> None:
        pyre_connection = MagicMock()
        pyre_connection.query_server.return_value = {
            "response": [{"Scooter": ["Bike", "Vehicle", "object"]}]
        }
        self.assertEqual(
            query.get_superclasses(pyre_connection, "Scooter"),
            ["Bike", "Vehicle", "object"],
        )

    def test_get_attributes(self) -> None:
        pyre_connection = MagicMock()
        pyre_connection.query_server.return_value = {
            "response": [
                {
                    "response": {
                        "attributes": [
                            {
                                "annotation": "int",
                                "name": "a",
                                "kind": "regular",
                                "final": False,
                            },
                            {
                                "annotation": "typing.Callable(a.C.foo)[[], str]",
                                "name": "foo",
                                "kind": "property",
                                "final": False,
                            },
                        ]
                    }
                }
            ]
        }
        self.assertEqual(
            query.get_attributes(pyre_connection, ["a.C"]),
            {
                "a.C": [
                    query.Attributes(
                        name="a", annotation="int", kind="regular", final=False
                    ),
                    query.Attributes(
                        name="foo",
                        annotation="typing.Callable(a.C.foo)[[], str]",
                        kind="property",
                        final=False,
                    ),
                ]
            },
        )

    def test_get_attributes_batch(self) -> None:
        pyre_connection = MagicMock()
        pyre_connection.query_server.return_value = {
            "response": [
                {
                    "response": {
                        "attributes": [
                            {
                                "annotation": "int",
                                "name": "a",
                                "kind": "regular",
                                "final": False,
                            },
                            {
                                "annotation": "typing.Callable(a.C.foo)[[], str]",
                                "name": "foo",
                                "kind": "property",
                                "final": False,
                            },
                        ]
                    }
                },
                {
                    "response": {
                        "attributes": [
                            {
                                "annotation": "str",
                                "name": "b",
                                "kind": "regular",
                                "final": False,
                            },
                            {
                                "annotation": None,
                                "name": "c",
                                "kind": "property",
                                "final": False,
                            },
                        ]
                    }
                },
            ]
        }
        self.assertEqual(
            query.get_attributes(
                pyre_connection,
                [
                    "TestClassA",
                    "TestClassB",
                ],
                batch_size=100,
            ),
            {
                "TestClassA": [
                    query.Attributes(
                        name="a", annotation="int", kind="regular", final=False
                    ),
                    query.Attributes(
                        name="foo",
                        annotation="typing.Callable(a.C.foo)[[], str]",
                        kind="property",
                        final=False,
                    ),
                ],
                "TestClassB": [
                    query.Attributes(
                        name="b", annotation="str", kind="regular", final=False
                    ),
                    query.Attributes(
                        name="c", annotation=None, kind="property", final=False
                    ),
                ],
            },
        )

    def test_get_attributes_batch_no_size(self) -> None:
        pyre_connection = MagicMock()
        pyre_connection.query_server.return_value = {
            "response": [
                {
                    "response": {
                        "attributes": [
                            {
                                "annotation": "int",
                                "name": "a",
                                "kind": "regular",
                                "final": False,
                            },
                            {
                                "annotation": "typing.Callable(a.C.foo)[[], str]",
                                "name": "foo",
                                "kind": "property",
                                "final": False,
                            },
                        ]
                    }
                },
                {
                    "response": {
                        "attributes": [
                            {
                                "annotation": "str",
                                "name": "b",
                                "kind": "regular",
                                "final": False,
                            },
                            {
                                "annotation": None,
                                "name": "c",
                                "kind": "property",
                                "final": False,
                            },
                        ]
                    }
                },
            ]
        }
        self.assertEqual(
            query.get_attributes(
                pyre_connection,
                [
                    "TestClassA",
                    "TestClassB",
                ],
                batch_size=None,
            ),
            {
                "TestClassA": [
                    query.Attributes(
                        name="a", annotation="int", kind="regular", final=False
                    ),
                    query.Attributes(
                        name="foo",
                        annotation="typing.Callable(a.C.foo)[[], str]",
                        kind="property",
                        final=False,
                    ),
                ],
                "TestClassB": [
                    query.Attributes(
                        name="b", annotation="str", kind="regular", final=False
                    ),
                    query.Attributes(
                        name="c", annotation=None, kind="property", final=False
                    ),
                ],
            },
        )

    def test_get_call_graph(self) -> None:
        pyre_connection = MagicMock()
        pyre_connection.query_server.return_value = {
            "response": {
                "async_test.foo": [],
                "async_test.bar": [
                    {
                        "locations": [
                            {
                                "path": "async_test.py",
                                "start": {"line": 6, "column": 4},
                                "stop": {"line": 6, "column": 7},
                            }
                        ],
                        "kind": "function",
                        "target": "async_test.foo",
                    }
                ],
                "async_test.C.method": [
                    {
                        "locations": [
                            {
                                "path": "async_test.py",
                                "start": {"line": 10, "column": 4},
                                "stop": {"line": 10, "column": 7},
                            }
                        ],
                        "kind": "method",
                        "is_optional_class_attribute": False,
                        "direct_target": "async_test.C.method",
                        "class_name": "async_test.C",
                        "dispatch": "dynamic",
                    }
                ],
            }
        }

        self.assertEqual(
            query.get_call_graph(pyre_connection),
            {
                "async_test.foo": [],
                "async_test.bar": [
                    query.CallGraphTarget(
                        {
                            "target": "async_test.foo",
                            "kind": "function",
                            "locations": [
                                {
                                    "path": "async_test.py",
                                    "start": {"line": 6, "column": 4},
                                    "stop": {"line": 6, "column": 7},
                                }
                            ],
                        }
                    )
                ],
                "async_test.C.method": [
                    query.CallGraphTarget(
                        {
                            "target": "async_test.C.method",
                            "kind": "method",
                            "locations": [
                                {
                                    "path": "async_test.py",
                                    "start": {"line": 10, "column": 4},
                                    "stop": {"line": 10, "column": 7},
                                }
                            ],
                        }
                    )
                ],
            },
        )

    def test_get_invalid_taint_models(self) -> None:
        pyre_connection = MagicMock()
        pyre_connection.query_server.side_effect = connection.PyreQueryError(
            "This is an invalid error message"
        )
        with self.assertRaises(connection.PyreQueryError):
            query.get_invalid_taint_models(pyre_connection)
        pyre_connection = MagicMock()
        pyre_connection.query_server.return_value = {
            "response": {
                "errors": [
                    {
                        "description": "Invalid model for `first.f`: Unrecognized taint annotation `NotAnAnnotation`",  # noqa: B950
                        "path": "/path/to/first.py",
                        "line": 2,
                        "column": 0,
                        "stop_line": 4,
                        "stop_column": 1,
                    }
                ]
            }
        }
        self.assertEqual(
            query.get_invalid_taint_models(pyre_connection),
            [
                query.InvalidModel(
                    fully_qualified_name="",
                    path="/path/to/first.py",
                    line=2,
                    column=0,
                    stop_line=4,
                    stop_column=1,
                    full_error_message="Invalid model for `first.f`: Unrecognized taint annotation `NotAnAnnotation`",  # noqa: B950
                ),
            ],
        )

    def test_get_attributes_query_error(self) -> None:
        test_data: connection.PyreQueryResult = {
            "response": [
                {
                    "response": {
                        "attributes": [
                            {
                                "name": "a",
                                "annotation": "typing.Any",
                                "kind": "property",
                                "final": False,
                            }
                        ]
                    }
                },
                {"error": "Type `B` has the wrong number of parameters."},
                {"response": {"attributes": []}},
            ]
        }
        pyre_connection = MagicMock()
        pyre_connection.query_server.return_value = test_data

        self.assertEqual(
            query.get_attributes(
                pyre_connection,
                [
                    "TestClassA",
                    "TestClassB",
                    "TestClassC",
                ],
            ),
            {
                "TestClassA": [
                    query.Attributes(
                        name="a", annotation="typing.Any", kind="property", final=False
                    )
                ],
                "TestClassB": [],
                "TestClassC": [],
            },
        )
