# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import MagicMock

from .. import query


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
        self.assertEqual(hierarchy.superclasses("Nonexistent"), None)
        # Subclasses.
        self.assertEqual(hierarchy.subclasses("object"), ["Foo"])
        self.assertEqual(hierarchy.subclasses("Foo"), [])
        self.assertEqual(hierarchy.subclasses("Nonexistent"), None)

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
        pyre_connection.query_server.return_value = {"error": "Found an issue"}
        self.assertEqual(query.get_class_hierarchy(pyre_connection), None)

    def test_get_superclasses(self) -> None:
        pyre_connection = MagicMock()
        pyre_connection.query_server.return_value = {
            "response": {"superclasses": ["Bike", "Vehicle", "object"]}
        }
        self.assertEqual(
            query.get_superclasses(pyre_connection, "Scooter"),
            ["Bike", "Vehicle", "object"],
        )
        pyre_connection.query_server.return_value = {
            "error": "Type `Foo` was not found in the type order."
        }
        self.assertEqual(query.get_superclasses(pyre_connection, "Foo"), [])

    def test_get_attributes(self) -> None:
        pyre_connection = MagicMock()
        pyre_connection.query_server.return_value = {
            "response": {
                "attributes": [
                    {"annotation": "int", "name": "a"},
                    {"annotation": "typing.Callable(a.C.foo)[[], str]", "name": "foo"},
                ]
            }
        }
        self.assertEqual(query.get_attributes(pyre_connection, "a.C"), ["a", "foo"])
        pyre_connection.query_server.return_value = {
            "error": "Type `Foo` was not found in the type order."
        }
        self.assertEqual(query.get_superclasses(pyre_connection, "Foo"), [])

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
            }
        }

        self.assertEqual(
            query.get_call_graph(pyre_connection),
            {
                "async_test.foo": [],
                "async_test.bar": [
                    query.CallGraphTarget(
                        target="async_test.foo",
                        kind="function",
                        locations=[
                            query.Location(
                                path="async_test.py",
                                start=query.Position(line=6, column=4),
                                stop=query.Position(line=6, column=7),
                            )
                        ],
                    )
                ],
            },
        )
