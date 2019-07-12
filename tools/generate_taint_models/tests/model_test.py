# pyre-strict

import unittest

from .. import model


def test_function(argument: str, *variable: str, **keyword: str) -> None:
    pass


class ModelTest(unittest.TestCase):
    def test_generate_model(self) -> None:
        name = f"{__name__}.test_function"
        self.assertEqual(
            model.CallableModel(
                callable=test_function, arg="TaintSource[tainted]"
            ).generate(),
            f"def {name}(argument: TaintSource[tainted], *variable, **keyword): ...",
        )
        self.assertEqual(
            model.CallableModel(
                callable=test_function,
                arg="TaintSource[tainted]",
                whitelisted_parameters=["str"],
            ).generate(),
            f"def {name}(argument, *variable, **keyword): ...",
        )
        self.assertEqual(
            model.CallableModel(
                callable=test_function, vararg="TaintSource[tainted]"
            ).generate(),
            f"def {name}(argument, *variable: TaintSource[tainted], **keyword): ...",
        )
        self.assertEqual(
            model.CallableModel(
                callable=test_function, kwarg="TaintSource[tainted]"
            ).generate(),
            f"def {name}(argument, *variable, **keyword: TaintSource[tainted]): ...",
        )
        self.assertEqual(
            model.CallableModel(
                callable=test_function, returns="TaintSink[returned]"
            ).generate(),
            f"def {name}(argument, *variable, **keyword) -> TaintSink[returned]: ...",
        )

        # We don't generate models for local functions.
        def local_function(x: int, *args: str) -> None:
            ...

        self.assertEqual(
            model.CallableModel(
                callable=local_function, returns="TaintSink[returned]"
            ).generate(),
            None,
        )

        # Ensure that we don't choke on malformed types of functions.
        class CallMe:
            def __call__(self) -> None:
                pass

        self.assertEqual(model.CallableModel(callable=CallMe).generate(), None)

    def test_assignment_model(self) -> None:
        self.assertEqual(
            model.AssignmentModel(
                annotation="TaintSink[Test]", target="name"
            ).generate(),
            "name: TaintSink[Test] = ...",
        )
