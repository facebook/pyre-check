# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from unittest.mock import MagicMock

from ..generator_specifications import (
    AllParametersAnnotation,
    AnnotationSpecification,
    default_entrypoint_taint,
    WhitelistSpecification,
)
from ..get_REST_api_sources import RESTApiSourceGenerator
from .test_functions import __name__ as qualifier, all_functions


class GetRESTApiSourcesTest(unittest.TestCase):
    def test_compute_models(self) -> None:
        # Test with default arguments
        source = "TaintSource[UserControlled]"
        sink = default_entrypoint_taint.returns
        self.assertEqual(
            [
                *map(
                    str,
                    RESTApiSourceGenerator(django_urls=MagicMock()).compute_models(
                        all_functions
                    ),
                )
            ],
            [
                f"def {qualifier}.TestClass.methodA(self, x: {source})"
                f" -> {sink}: ...",
                f"def {qualifier}.TestClass.methodB(self, *args: {source})"
                f" -> {sink}: ...",
                f"def {qualifier}.testA() -> {sink}: ...",
                f"def {qualifier}.testB(x: {source}) -> {sink}: ...",
                f"def {qualifier}.testC(x: {source}) -> {sink}: ...",
                f"def {qualifier}.testD(x: {source}, *args: {source})"
                f" -> {sink}: ...",
                f"def {qualifier}.testE(x: {source}, **kwargs: {source})"
                f" -> {sink}: ...",
            ],
        )

        # Test with view whitelisting
        self.assertEqual(
            [
                *map(
                    str,
                    RESTApiSourceGenerator(
                        django_urls=MagicMock(),
                        whitelisted_views=[f"{qualifier}.testA"],
                    ).compute_models(all_functions),
                )
            ],
            [
                f"def {qualifier}.TestClass.methodA(self, x: {source})"
                f" -> {sink}: ...",
                f"def {qualifier}.TestClass.methodB(self, *args: {source})"
                f" -> {sink}: ...",
                f"def {qualifier}.testB(x: {source}) -> {sink}: ...",
                f"def {qualifier}.testC(x: {source}) -> {sink}: ...",
                f"def {qualifier}.testD(x: {source}, *args: {source})"
                f" -> {sink}: ...",
                f"def {qualifier}.testE(x: {source}, **kwargs: {source})"
                f" -> {sink}: ...",
            ],
        )

        # Test with AnnotationSpecification
        self.assertEqual(
            [
                *map(
                    str,
                    RESTApiSourceGenerator(
                        django_urls=MagicMock(),
                        annotations=AnnotationSpecification(
                            parameter_annotation=AllParametersAnnotation(
                                arg="Arg", vararg="VarArg", kwarg="KWArg"
                            ),
                            returns="Returns",
                        ),
                    ).compute_models(all_functions),
                )
            ],
            [
                f"def {qualifier}.TestClass.methodA(self, x: Arg) -> Returns: ...",
                f"def {qualifier}.TestClass.methodB(self, *args: VarArg)"
                " -> Returns: ...",
                f"def {qualifier}.testA() -> Returns: ...",
                f"def {qualifier}.testB(x: Arg) -> Returns: ...",
                f"def {qualifier}.testC(x: Arg) -> Returns: ...",
                f"def {qualifier}.testD(x: Arg, *args: VarArg) -> Returns: ...",
                f"def {qualifier}.testE(x: Arg, **kwargs: KWArg) -> Returns: ...",
            ],
        )

        # Test with WhitelistSpecification
        self.assertEqual(
            [
                *map(
                    str,
                    RESTApiSourceGenerator(
                        django_urls=MagicMock(),
                        whitelisted_parameters=WhitelistSpecification(
                            parameter_name={"self"}, parameter_type={"int"}
                        ),
                    ).compute_models(all_functions),
                )
            ],
            [
                f"def {qualifier}.TestClass.methodA(self, x) -> {sink}: ...",
                f"def {qualifier}.TestClass.methodB(self, *args: {source})"
                f" -> {sink}: ...",
                f"def {qualifier}.testA() -> {sink}: ...",
                f"def {qualifier}.testB(x: {source}) -> {sink}: ...",
                f"def {qualifier}.testC(x) -> {sink}: ...",
                f"def {qualifier}.testD(x, *args) -> {sink}: ...",
                f"def {qualifier}.testE(x, **kwargs: {source}) -> {sink}: ...",
            ],
        )
