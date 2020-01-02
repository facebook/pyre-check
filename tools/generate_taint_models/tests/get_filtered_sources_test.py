# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import ast
import unittest
from unittest.mock import patch

from ..get_annotated_free_functions_with_decorator import (
    AnnotatedFreeFunctionWithDecoratorGenerator,
)
from ..get_filtered_sources import FilteredSourceGenerator
from ..get_REST_api_sources import RESTApiSourceGenerator
from ..model import CallableModel, FunctionDefinitionModel
from .test_functions import all_functions, testB, testC


class GetFilteredSourcesTest(unittest.TestCase):
    @patch.object(RESTApiSourceGenerator, "generate_models")
    @patch.object(AnnotatedFreeFunctionWithDecoratorGenerator, "generate_models")
    def test_compute_models_with_no_intersection(
        self,
        mock_annotated_decorator_generate_models,
        mock_RESTapi_decorator_generate_models,
    ) -> None:
        function_definition_str = "def testA(x): pass"
        function_definition = ast.parse(function_definition_str).body[0]
        mock_RESTapi_decorator_generate_models.return_value = {
            CallableModel(
                testB,
                arg="TaintSource[UserControlled]",
                vararg="TaintSource[UserControlled]",
                kwarg="TaintSource[UserControlled]",
            )
        }
        mock_annotated_decorator_generate_models.return_value = {
            FunctionDefinitionModel(
                # pyre-ignore: Incompatible parameter type [6]
                function_definition,
                arg="TaintSource[UserControlled]",
                vararg="TaintSource[UserControlled]",
                kwarg="TaintSource[UserControlled]",
                qualifier="tools.pyre.tools.generate_taint_models.tests.test_functions",
            )
        }

        # Functions from RESTApiSourceGenerator should appear
        self.assertEqual(
            [*map(str, FilteredSourceGenerator().compute_models(all_functions))],
            [
                "def tools.pyre.tools.generate_taint_models.tests.test_functions."
                "testB(x: TaintSource[UserControlled]): ..."
            ],
        )

    @patch.object(RESTApiSourceGenerator, "generate_models")
    @patch.object(AnnotatedFreeFunctionWithDecoratorGenerator, "generate_models")
    def test_compute_models_with_some_intersection(
        self,
        mock_annotated_decorator_generate_models,
        mock_RESTapi_decorator_generate_models,
    ) -> None:
        function_definition_str = "def testB(x): pass"
        function_definition = ast.parse(function_definition_str).body[0]
        mock_RESTapi_decorator_generate_models.return_value = {
            CallableModel(
                testC,
                arg="TaintSource[UserControlled]",
                vararg="TaintSource[UserControlled]",
                kwarg="TaintSource[UserControlled]",
            ),
            CallableModel(
                testB,
                arg="TaintSource[UserControlled]",
                vararg="TaintSource[UserControlled]",
                kwarg="TaintSource[UserControlled]",
            ),
        }
        mock_annotated_decorator_generate_models.return_value = {
            FunctionDefinitionModel(
                # pyre-ignore: Incompatible parameter type [6]
                function_definition,
                arg="TaintSource[UserControlled]",
                vararg="TaintSource[UserControlled]",
                kwarg="TaintSource[UserControlled]",
                qualifier="tools.pyre.tools.generate_taint_models.tests.test_functions",
            )
        }

        # Functions that are in RESTApiSourceGenerator but not in
        # AnnotatedFreeFunctionWithDecoratorGenerator should appear
        self.assertEqual(
            [*map(str, FilteredSourceGenerator().compute_models(all_functions))],
            [
                "def tools.pyre.tools.generate_taint_models.tests.test_functions."
                "testC(x: TaintSource[UserControlled]): ..."
            ],
        )

        @patch.object(RESTApiSourceGenerator, "generate_models")
        @patch.object(AnnotatedFreeFunctionWithDecoratorGenerator, "generate_models")
        def test_compute_models_with_complete_intersection(
            self,
            mock_annotated_decorator_generate_models,
            mock_RESTapi_decorator_generate_models,
        ) -> None:
            function_definition_str = "def testB(x): pass"
            function_definition = ast.parse(function_definition_str).body[0]
            mock_RESTapi_decorator_generate_models.return_value = {
                CallableModel(
                    testB,
                    arg="TaintSource[UserControlled]",
                    vararg="TaintSource[UserControlled]",
                    kwarg="TaintSource[UserControlled]",
                )
            }
            mock_annotated_decorator_generate_models.return_value = {
                FunctionDefinitionModel(
                    # pyre-ignore: Incompatible parameter type [6]
                    function_definition,
                    "TaintSource[UserControlled]",
                    "TaintSource[UserControlled]",
                    "TaintSource[UserControlled]",
                )
            }

            self.assertEqual(
                [*map(str, FilteredSourceGenerator().compute_models(all_functions))], []
            )
