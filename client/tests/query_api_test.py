# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import unittest
from unittest.mock import MagicMock

from .. import query_api


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
            query_api.defines(pyre_connection, ["a"]),
            [
                query_api.Define(
                    name="a.foo",
                    parameters=[query_api.DefineParameter(name="x", annotation="int")],
                    return_annotation="int",
                )
            ],
        )

    def test_get_class_hierarchy(self) -> None:
        pyre_connection = MagicMock()
        pyre_connection.query_server.return_value = {
            "response": [{"Foo": ["object"]}, {"object": []}]
        }
        self.assertEqual(
            query_api.get_class_hierarchy(pyre_connection),
            {"Foo": ["object"], "object": []},
        )
        pyre_connection.query_server.return_value = {
            "response": [
                {"Foo": ["object"]},
                {"object": []},
                # This should never happen in practice, but unfortunately is something
                # to consider due to the type of the JSON returned. The last entry wins.
                {"Foo": ["Bar"]},
                {"Bar": ["object"]},
            ]
        }
        self.assertEqual(
            query_api.get_class_hierarchy(pyre_connection),
            {"Foo": ["Bar"], "Bar": ["object"], "object": []},
        )
        pyre_connection.query_server.return_value = {"error": "Found an issue"}
        self.assertEqual(query_api.get_class_hierarchy(pyre_connection), None)

    def test_get_superclasses(self) -> None:
        pyre_connection = MagicMock()
        pyre_connection.query_server.return_value = {
            "response": {"superclasses": ["Bike", "Vehicle", "object"]}
        }
        self.assertEqual(
            query_api.get_superclasses(pyre_connection, "Scooter"),
            ["Bike", "Vehicle", "object"],
        )
        pyre_connection.query_server.return_value = {
            "error": "Type `Foo` was not found in the type order."
        }
        self.assertEqual(query_api.get_superclasses(pyre_connection, "Foo"), [])

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
            query_api.get_call_graph(pyre_connection),
            {
                "async_test.foo": [],
                "async_test.bar": [
                    query_api.CallGraphTarget(
                        target="async_test.foo",
                        kind="function",
                        locations=[
                            query_api.Location(
                                path="async_test.py",
                                start=query_api.Position(line=6, column=4),
                                stop=query_api.Position(line=6, column=7),
                            )
                        ],
                    )
                ],
            },
        )
