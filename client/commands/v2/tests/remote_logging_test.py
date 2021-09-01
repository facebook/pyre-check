# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pathlib import Path
from typing import Optional, Dict, Callable, TypeVar

import testslide

from .... import statistics_logger, commands, configuration as configuration_module
from ..remote_logging import (
    log_usage,
    log_usage_with_additional_info,
    ExitCodeWithAdditionalLogging,
)

# Type signature of `statistics_logger.log`
LoggingApiTypeSignature = Callable[
    [
        statistics_logger.LoggerCategory,
        str,
        Optional[Dict[str, int]],
        Optional[Dict[str, Optional[str]]],
    ],
    None,
]
K = TypeVar("K")
V = TypeVar("V")


class TestException(Exception):
    pass


class RemoteLoggingTest(testslide.TestCase):
    def assert_dict_contains_subset(
        self, actual: Dict[K, V], expected_subset: Dict[K, V]
    ) -> None:
        actual_subset = {
            key: value for key, value in actual.items() if key in expected_subset
        }
        self.assertDictEqual(actual_subset, expected_subset)

    def _create_logging_api(
        self,
        expected_integers_subset: Optional[Dict[str, int]] = None,
        expected_normals_subset: Optional[Dict[str, Optional[str]]] = None,
    ) -> LoggingApiTypeSignature:
        """
        Call this method to create a mock `statistics_logger.log` function
         intended to be used like this:

        ```
        self.mock_callable(statistics_logger, 'log')\
        .with_implementation(
            self._create_logging_api(
                expected_integers_subset=...,
                expected_normals_subset=...,
            )
        )
        ```
        If `expected_integers_subset`/`expected_normals_subset` is not `None`, the
        mocked logging API will try to assert that those expected integers/normals
        are included in the actual integers/normals being logged.
        """

        def mock_logging_api(
            category: statistics_logger.LoggerCategory,
            logger: str,
            integers: Optional[Dict[str, int]],
            normals: Optional[Dict[str, Optional[str]]],
        ) -> None:
            actual_integers = integers or {}
            actual_normals = normals or {}
            if expected_integers_subset is not None:
                self.assert_dict_contains_subset(
                    actual=actual_integers,
                    expected_subset=expected_integers_subset,
                )
            if expected_normals_subset is not None:
                self.assert_dict_contains_subset(
                    actual=actual_normals,
                    expected_subset=expected_normals_subset,
                )

        return mock_logging_api

    def test_log_success(self) -> None:
        command_name: str = "test"
        self.mock_callable(statistics_logger, "log").with_implementation(
            self._create_logging_api(
                expected_integers_subset={"exit_code": commands.ExitCode.SUCCESS},
                expected_normals_subset={"command": command_name},
            )
        ).and_assert_called_once()

        @log_usage(command_name)
        def test_command(
            configuration: configuration_module.Configuration,
        ) -> commands.ExitCode:
            return commands.ExitCode.SUCCESS

        test_command(
            configuration_module.Configuration(
                project_root="/some/root",
                dot_pyre_directory=Path("/some/directory"),
                logger="/some/logger",
            )
        )

    def test_log_success_with_additional_info(self) -> None:
        command_name: str = "test"
        self.mock_callable(statistics_logger, "log").with_implementation(
            self._create_logging_api(
                expected_integers_subset={"exit_code": commands.ExitCode.SUCCESS},
                expected_normals_subset={"command": command_name, "foo": "bar"},
            )
        ).and_assert_called_once()

        @log_usage_with_additional_info(command_name)
        def test_command(
            configuration: configuration_module.Configuration,
        ) -> ExitCodeWithAdditionalLogging:
            return ExitCodeWithAdditionalLogging(
                exit_code=commands.ExitCode.SUCCESS, additional_logging={"foo": "bar"}
            )

        test_command(
            configuration_module.Configuration(
                project_root="/some/root",
                dot_pyre_directory=Path("/some/directory"),
                logger="/some/logger",
            )
        )

    def test_log_failure(self) -> None:
        command_name: str = "test"
        exception: TestException = TestException()
        self.mock_callable(statistics_logger, "log").with_implementation(
            self._create_logging_api(
                expected_integers_subset={"exit_code": commands.ExitCode.FAILURE},
                expected_normals_subset={"client_exception": str(exception)},
            )
        ).and_assert_called_once()

        @log_usage(command_name)
        def test_command(
            configuration: configuration_module.Configuration,
        ) -> commands.ExitCode:
            raise exception

        with self.assertRaises(TestException):
            test_command(
                configuration_module.Configuration(
                    project_root="/some/root",
                    dot_pyre_directory=Path("/some/directory"),
                    logger="/some/logger",
                )
            )
