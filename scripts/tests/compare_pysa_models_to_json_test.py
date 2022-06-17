# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import unittest
from unittest.mock import mock_open, patch

from ..compare_pysa_models_to_json import (
    get_models_from_json_file,
    get_models_from_pysa_file,
    json_to_parsed_model,
    make_default_taint_model,
    make_default_target_model,
    parse_kinds,
    TargetModel,
)


class MakeDefaultTargetModelTest(unittest.TestCase):
    EMPTY_CALLABLE_MODEL: TargetModel = make_default_target_model()

    def test_defaultdict(self) -> None:
        self.assertEqual(
            self.EMPTY_CALLABLE_MODEL["parameters"]["foo"], make_default_taint_model()
        )


class ParseKindTest(unittest.TestCase):
    def test_sinks(self) -> None:
        taints = json.loads(
            """
            [
                {
                    "kinds": [ { "kind": "ReturnedToUser" } ],
                    "decl": null
                }
            ]
            """
        )
        self.assertEqual(parse_kinds(taints), {"ReturnedToUser"})

    def test_sources(self) -> None:
        taints = json.loads(
            """
            [
                { "decl": null, "kinds": [ { "kind": "UserControlled" } ] },
                {
                    "decl": null,
                    "kinds": [ { "kind": "UserControlled_Parameter" } ]
                }
            ]
            """
        )
        self.assertEqual(
            parse_kinds(taints), {"UserControlled", "UserControlled_Parameter"}
        )


class JsonToParsedModelTest(unittest.TestCase):
    def test_parameters_sources(self) -> None:
        models = json.loads(
            """
            [
                {
                    "kind": "model",
                    "data": {
                        "callable": "foo.bar.websocket.WebSocketHandler.get",
                        "sources": [
                            {
                                "port": "formal(**kw)",
                                "taint": [
                                    { "decl": null, "kinds": [ { "kind": "UserControlled" } ] },
                                    {
                                        "decl": null,
                                        "kinds": [ { "kind": "UserControlled_Parameter" } ]
                                    }
                                ]
                            },
                            {
                                "port": "formal(*rest1)",
                                "taint": [
                                    { "decl": null, "kinds": [ { "kind": "UserControlled" } ] },
                                    {
                                        "decl": null,
                                        "kinds": [ { "kind": "UserControlled_Parameter" } ]
                                    }
                                ]
                            }
                        ]
                    }
                }
            ]
            """
        )
        self.assertEqual(
            json_to_parsed_model(models),
            {
                "parameters": {
                    "**kw": {
                        "sources": {"UserControlled", "UserControlled_Parameter"},
                        "sinks": set(),
                        "tito": set(),
                    },
                    "*rest1": {
                        "sources": {"UserControlled", "UserControlled_Parameter"},
                        "sinks": set(),
                        "tito": set(),
                    },
                },
                "return_model": {"sources": set(), "sinks": set(), "tito": set()},
            },
        )

    def test_return_model(self) -> None:
        models = json.loads(
            """
            [
                {
                    "kind": "model",
                    "data": {
                        "callable":
                        "foo.bar.baz.lorem.LoremIpsum.fake_render_function",
                        "sinks": [
                            {
                                "port": "result",
                                "taint": [
                                    { "decl": null, "kinds": [ { "kind": "ReturnedToUser" } ] }
                                ]
                            }
                        ]
                    }
                }
            ]
            """
        )

        self.assertEqual(
            json_to_parsed_model(models),
            {
                "parameters": {},
                "return_model": {
                    "sources": set(),
                    "sinks": {"ReturnedToUser"},
                    "tito": set(),
                },
            },
        )


TEST_JSON_FILE = """
[
    {
        "query_name_1":
        [
            {
                "callable":
                    "foo.bar.baz.loremipsum.FakeClass1.fake_render_function",
                "model": [
                    {
                        "kind": "model",
                        "data": {
                            "callable":
                            "foo.bar.baz.loremipsum.FakeClass1.fake_render_function",
                            "sinks": [
                                {
                                    "port": "result",
                                    "taint": [
                                        { "decl": null, "kinds": [ { "kind": "ReturnedToUser" } ] }
                                    ]
                                }
                            ]
                        }
                    }
                ]
            },
            {
                "callable":
                    "foo.bar.baz.loremipsum.FakeClass2.fake_render_function",
                "model": [
                    {
                        "kind": "model",
                        "data": {
                            "callable":
                            "foo.bar.baz.loremipsum.FakeClass2.fake_render_function",
                            "sinks": [
                                {
                                    "port": "result",
                                    "taint": [
                                        { "decl": null, "kinds": [ { "kind": "ReturnedToUser" } ] }
                                    ]
                                }
                            ]
                        }
                    }
                ]
            },
            {
                "callable":
                    "foo.bar.baz.loremipsum.FakeClass3.fake_render_function",
                "model": [
                    {
                        "kind": "model",
                        "data": {
                            "callable":
                            "foo.bar.baz.loremipsum.FakeClass3.fake_render_function",
                            "sinks": [
                                {
                                    "port": "result",
                                    "taint": [
                                        { "decl": null, "kinds": [ { "kind": "ReturnedToUser" } ] }
                                    ]
                                }
                            ]
                        }
                    }
                ]
            },
            {
                "callable":
                    "foo.bar.baz.loremipsum.FakeClass4.fake_render_function",
                "model": [
                    {
                        "kind": "model",
                        "data": {
                            "callable":
                            "foo.bar.baz.loremipsum.FakeClass4.fake_render_function",
                            "sinks": [
                                {
                                    "port": "result",
                                    "taint": [
                                        { "decl": null, "kinds": [ { "kind": "ReturnedToUser" } ] }
                                    ]
                                }
                            ]
                        }
                    }
                ]
            },
            {
                "callable":
                    "foo.bar.baz.loremipsum.FakeClass5.fake_render_function",
                "model": [
                    {
                        "kind": "model",
                        "data": {
                                "callable":
                                "foo.bar.baz.loremipsum.FakeClass5.fake_render_function",
                                "sinks": [
                                {
                                    "port": "result",
                                    "taint": [
                                        { "decl": null, "kinds": [ { "kind": "ReturnedToUser" } ] }
                                    ]
                                }
                            ]
                        }
                    }
                ]
            },
            {
                "callable":
                    "foo.bar.baz.loremipsum.FakeClass6.Download.get",
                "model": [
                    {
                        "kind": "model",
                        "data": {
                            "callable":
                            "foo.bar.baz.loremipsum.FakeClass6.Download.get",
                            "sources": [
                                {
                                    "port": "formal(fmt)",
                                    "taint": [
                                        { "decl": null, "kinds": [ { "kind": "UserControlled" } ] },
                                        {
                                            "decl": null,
                                            "kinds": [ { "kind": "UserControlled_Parameter" } ]
                                        }
                                    ]
                                },
                                {
                                    "port": "formal(fignum)",
                                    "taint": [
                                        { "decl": null, "kinds": [ { "kind": "UserControlled" } ] },
                                        {
                                            "decl": null,
                                            "kinds": [ { "kind": "UserControlled_Parameter" } ]
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                ]
            }
        ],
        "query_name_2":
        [
            {
                "callable":
                    "Obj{foo.bar.baz.FakeClass1.a}",
                "model": [
                    {
                        "kind": "model",
                        "data": {
                            "callable":
                            "Obj{foo.bar.baz.FakeClass1.a}",
                            "sources": [
                                {
                                    "port": "result",
                                    "taint": [
                                        {
                                            "kinds": [ { "kind": "UserControlled" } ],
                                            "decl": null
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                ]
            },
            {
                "callable":
                    "Obj{foo.bar.baz.FakeClass1.b}",
                "model": [
                    {
                        "kind": "model",
                        "data": {
                            "callable":
                            "Obj{foo.bar.baz.FakeClass1.b}",
                            "sources": [
                                {
                                    "port": "result",
                                    "taint": [
                                        { "decl": null, "kinds": [ { "kind": "UserControlled" } ] },
                                        {
                                            "decl": null,
                                            "kinds": [ { "kind": "UserControlled_Parameter" } ]
                                        }
                                    ]
                                }
                            ]
                        }
                    }
                ]
            }
        ]
    }
]
"""


class GetModelsFromJsonFileTest(unittest.TestCase):
    def test_json_parameters_sources(self) -> None:
        with patch("pathlib.Path.open", mock_open(read_data=TEST_JSON_FILE)):
            self.assertEqual(
                get_models_from_json_file("fakepath"),
                {
                    "foo.bar.baz.loremipsum.FakeClass1.fake_render_function": {
                        "parameters": {},
                        "return_model": {
                            "sources": set(),
                            "sinks": {"ReturnedToUser"},
                            "tito": set(),
                        },
                    },
                    "foo.bar.baz.loremipsum.FakeClass2.fake_render_function": {
                        "parameters": {},
                        "return_model": {
                            "sources": set(),
                            "sinks": {"ReturnedToUser"},
                            "tito": set(),
                        },
                    },
                    "foo.bar.baz.loremipsum.FakeClass3.fake_render_function": {
                        "parameters": {},
                        "return_model": {
                            "sources": set(),
                            "sinks": {"ReturnedToUser"},
                            "tito": set(),
                        },
                    },
                    "foo.bar.baz.loremipsum.FakeClass4.fake_render_function": {
                        "parameters": {},
                        "return_model": {
                            "sources": set(),
                            "sinks": {"ReturnedToUser"},
                            "tito": set(),
                        },
                    },
                    "foo.bar.baz.loremipsum.FakeClass5.fake_render_function": {
                        "parameters": {},
                        "return_model": {
                            "sources": set(),
                            "sinks": {"ReturnedToUser"},
                            "tito": set(),
                        },
                    },
                    "foo.bar.baz.loremipsum.FakeClass6.Download.get": {
                        "parameters": {
                            "fmt": {
                                "sources": {
                                    "UserControlled_Parameter",
                                    "UserControlled",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "fignum": {
                                "sources": {
                                    "UserControlled_Parameter",
                                    "UserControlled",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                        },
                        "return_model": {
                            "sources": set(),
                            "sinks": set(),
                            "tito": set(),
                        },
                    },
                    "Obj{foo.bar.baz.FakeClass1.a}": {
                        "parameters": {},
                        "return_model": {
                            "sources": {
                                "UserControlled",
                            },
                            "sinks": set(),
                            "tito": set(),
                        },
                    },
                    "Obj{foo.bar.baz.FakeClass1.b}": {
                        "parameters": {},
                        "return_model": {
                            "sources": {
                                "UserControlled_Parameter",
                                "UserControlled",
                            },
                            "sinks": set(),
                            "tito": set(),
                        },
                    },
                },
            )


TEST_PYSA_FILE = """def foo.bar.baz.fake_method1(a, b, c: TaintSource[UserControlled, UserControlled_Parameter]) -> TaintSink[ReturnedToUser]: ...
def foo.bar.baz.fake_method2(a, b: TaintSource[UserControlled, UserControlled_Parameter]) -> TaintSink[ReturnedToUser]: ...
def foo.bar.baz.fake_method3(a, b: TaintSource[UserControlled, UserControlled_Parameter], c: TaintSource[UserControlled, UserControlled_Parameter]) -> TaintSink[ReturnedToUser]: ...
def foo.bar.baz.fake_method4(a, b: TaintSource[UserControlled, UserControlled_Parameter]) -> TaintSink[ReturnedToUser]: ...
foo.bar.baz.FakeClass1.a: TaintSource[UserControlled]
foo.bar.baz.FakeClass1.b: TaintSource[UserControlled, UserControlled_Parameter] = ...
def foo.bar.baz.fake_method5(a, b, c: TaintSource[UserControlled, UserControlled_Parameter], d: TaintSource[UserControlled, UserControlled_Parameter], _e: TaintSource[UserControlled, UserControlled_Parameter], _f: TaintSource[UserControlled, UserControlled_Parameter]) -> TaintSink[ReturnedToUser]: ...
def foo.bar.baz.fake_method6(_a) -> TaintSink[ReturnedToUser]: ...
def foo.bar.baz.fake_method7(a, b, c: TaintSource[UserControlled, UserControlled_Parameter], d: TaintSource[UserControlled, UserControlled_Parameter], e: TaintSource[UserControlled, UserControlled_Parameter], f: TaintSource[UserControlled, UserControlled_Parameter], g: TaintSource[UserControlled, UserControlled_Parameter], h: TaintSource[UserControlled, UserControlled_Parameter], i: TaintSource[UserControlled, UserControlled_Parameter], j: TaintSource[UserControlled, UserControlled_Parameter], k: TaintSource[UserControlled, UserControlled_Parameter], l: TaintSource[UserControlled, UserControlled_Parameter], m: TaintSource[UserControlled, UserControlled_Parameter]) -> TaintSink[ReturnedToUser]: ...
def foo.bar.baz.fake_method8(a, b, c: TaintSource[UserControlled, UserControlled_Parameter], d: TaintSource[UserControlled, UserControlled_Parameter], e: TaintSource[UserControlled, UserControlled_Parameter], f: TaintSource[UserControlled, UserControlled_Parameter], g: TaintSource[UserControlled, UserControlled_Parameter], h: TaintSource[UserControlled, UserControlled_Parameter], i: TaintSource[UserControlled, UserControlled_Parameter], j: TaintSource[UserControlled, UserControlled_Parameter], k: TaintSource[UserControlled, UserControlled_Parameter], l: TaintSource[UserControlled, UserControlled_Parameter], m: TaintSource[UserControlled, UserControlled_Parameter], n: TaintSource[UserControlled, UserControlled_Parameter], o: TaintSource[UserControlled, UserControlled_Parameter], p: TaintSource[UserControlled, UserControlled_Parameter], q: TaintSource[UserControlled, UserControlled_Parameter], r: TaintSource[UserControlled, UserControlled_Parameter], s: TaintSource[UserControlled, UserControlled_Parameter], t: TaintSource[UserControlled, UserControlled_Parameter]) -> TaintSink[ReturnedToUser]: ...
def foo.bar.baz.fake_method9(a, b, c, d) -> TaintSink[ResponseAfterPOSTRateLimit]: ...
def foo.bar.baz.fake_method9(a: TaintSource[UserControlled], b, c: TaintSource[UserControlled], d): ...
"""


class GetModelsFromPysaFileTest(unittest.TestCase):
    def test_pysa_parameters_sources(self) -> None:
        with patch("pathlib.Path.open", mock_open(read_data=TEST_PYSA_FILE)):
            self.assertEqual(
                get_models_from_pysa_file("fakepath"),
                {
                    "foo.bar.baz.fake_method1": {
                        "parameters": {
                            "c": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            }
                        },
                        "return_model": {
                            "sources": set(),
                            "sinks": {"ReturnedToUser"},
                            "tito": set(),
                        },
                    },
                    "foo.bar.baz.fake_method2": {
                        "parameters": {
                            "b": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            }
                        },
                        "return_model": {
                            "sources": set(),
                            "sinks": {"ReturnedToUser"},
                            "tito": set(),
                        },
                    },
                    "foo.bar.baz.fake_method3": {
                        "parameters": {
                            "b": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "c": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                        },
                        "return_model": {
                            "sources": set(),
                            "sinks": {"ReturnedToUser"},
                            "tito": set(),
                        },
                    },
                    "foo.bar.baz.fake_method4": {
                        "parameters": {
                            "b": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            }
                        },
                        "return_model": {
                            "sources": set(),
                            "sinks": {"ReturnedToUser"},
                            "tito": set(),
                        },
                    },
                    "Obj{foo.bar.baz.FakeClass1.a}": {
                        "parameters": {},
                        "return_model": {
                            "sources": {
                                "UserControlled",
                            },
                            "sinks": set(),
                            "tito": set(),
                        },
                    },
                    "Obj{foo.bar.baz.FakeClass1.b}": {
                        "parameters": {},
                        "return_model": {
                            "sources": {
                                "UserControlled_Parameter",
                                "UserControlled",
                            },
                            "sinks": set(),
                            "tito": set(),
                        },
                    },
                    "foo.bar.baz.fake_method5": {
                        "parameters": {
                            "c": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "d": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "_e": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "_f": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                        },
                        "return_model": {
                            "sources": set(),
                            "sinks": {"ReturnedToUser"},
                            "tito": set(),
                        },
                    },
                    "foo.bar.baz.fake_method6": {
                        "parameters": {},
                        "return_model": {
                            "sources": set(),
                            "sinks": {"ReturnedToUser"},
                            "tito": set(),
                        },
                    },
                    "foo.bar.baz.fake_method7": {
                        "parameters": {
                            "c": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "d": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "e": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "f": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "g": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "h": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "i": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "j": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "k": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "l": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "m": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                        },
                        "return_model": {
                            "sources": set(),
                            "sinks": {"ReturnedToUser"},
                            "tito": set(),
                        },
                    },
                    "foo.bar.baz.fake_method8": {
                        "parameters": {
                            "c": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "d": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "e": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "f": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "g": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "h": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "i": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "j": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "k": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "l": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "m": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "n": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "o": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "p": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "q": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "r": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "s": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "t": {
                                "sources": {
                                    "UserControlled",
                                    "UserControlled_Parameter",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                        },
                        "return_model": {
                            "sources": set(),
                            "sinks": {"ReturnedToUser"},
                            "tito": set(),
                        },
                    },
                    "foo.bar.baz.fake_method9": {
                        "parameters": {
                            "a": {
                                "sources": {
                                    "UserControlled",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                            "c": {
                                "sources": {
                                    "UserControlled",
                                },
                                "sinks": set(),
                                "tito": set(),
                            },
                        },
                        "return_model": {
                            "sources": set(),
                            "sinks": {"ResponseAfterPOSTRateLimit"},
                            "tito": set(),
                        },
                    },
                },
            )
