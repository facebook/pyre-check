# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import os
import sys
import time
from typing import Callable, Dict, Optional

from pyre_extensions import ParameterSpecification
from pyre_extensions.type_variable_operators import Concatenate
from typing_extensions import Protocol

from .. import (
    configuration as configuration_module,
    statistics_logger,
    version,
)
from . import commands

TParams = ParameterSpecification("TParams")


@dataclasses.dataclass(frozen=True)
class ExitCodeWithAdditionalLogging:
    exit_code: commands.ExitCode
    additional_logging: Dict[str, Optional[str]] = dataclasses.field(
        default_factory=dict
    )


class _DecoratorWithDynamicLogging(Protocol):
    def __call__(
        self,
        __f: "Callable[Concatenate[configuration_module.Configuration, TParams], ExitCodeWithAdditionalLogging]",  # noqa: B950
    ) -> "Callable[Concatenate[configuration_module.Configuration, TParams], commands.ExitCode]":  # noqa: B950
        ...


class _DecoratorWithoutDynamicLogging(Protocol):
    def __call__(
        self,
        __f: "Callable[Concatenate[configuration_module.Configuration, TParams], commands.ExitCode]",  # noqa: B950
    ) -> "Callable[Concatenate[configuration_module.Configuration, TParams], commands.ExitCode]":  # noqa: B950
        ...


def log_usage_with_additional_info(command_name: str) -> _DecoratorWithDynamicLogging:
    """
    This decorator is a variant of `log_usage`.

    With `log_usage`, what to log needs to be determined prior to running the
    decorated command. In contrast, `log_usage_with_additional_info` collects what
    to log after the decorated command is done executing. Logging content can
    therefore depend on the execution result after-the-fact.

    In exchange for the flexibility, `log_usage_with_additional_info` requires slightly
    more sophisticated setup: the decorated function should not only return a
    `commands.ExitCode`, but also attach the dynamically-computed logging dictionary
    alongside with the return code. The decorator itself will make sure that the
    additional info will be faithfully forwarded when invoking the underlying logging
    API.
    """
    auxiliary_info: Dict[str, Optional[str]] = {
        "cwd": os.getcwd(),
        "client_version": version.__version__,
        "command_line": " ".join(sys.argv),
        "command": command_name,
    }

    def decorator(
        __command: "Callable[Concatenate[configuration_module.Configuration, TParams], ExitCodeWithAdditionalLogging]",  # noqa: B950
    ) -> "Callable[Concatenate[configuration_module.Configuration, TParams], commands.ExitCode]":  # noqa: B950
        def decorated(
            configuration: configuration_module.Configuration,
            *args: TParams.args,
            **kwargs: TParams.kwargs
        ) -> commands.ExitCode:
            start_time: float = time.time()

            def log_success(
                exit_code: int, additional_logging: Dict[str, Optional[str]]
            ) -> None:
                statistics_logger.log_with_configuration(
                    category=statistics_logger.LoggerCategory.USAGE,
                    configuration=configuration,
                    integers={
                        "exit_code": exit_code,
                        "runtime": int((time.time() - start_time) * 1000),
                    },
                    normals={**auxiliary_info, **additional_logging},
                )

            def log_failure(
                error: Exception, exit_code: int = commands.ExitCode.FAILURE
            ) -> None:
                statistics_logger.log_with_configuration(
                    category=statistics_logger.LoggerCategory.USAGE,
                    configuration=configuration,
                    integers={
                        "exit_code": exit_code,
                        "runtime": int((time.time() - start_time) * 1000),
                    },
                    normals={
                        **auxiliary_info,
                        "client_exception": str(error),
                    },
                )

            try:
                result = __command(configuration, *args, **kwargs)
                exit_code = result.exit_code
                log_success(exit_code, result.additional_logging)
                return exit_code
            except configuration_module.InvalidConfiguration as error:
                log_failure(error, exit_code=commands.ExitCode.CONFIGURATION_ERROR)
                raise
            except Exception as error:
                log_failure(error)
                raise

        return decorated

    # pyre-ignore[7]: (T84575843) This should be fine.
    return decorator


def log_usage(command_name: str) -> _DecoratorWithoutDynamicLogging:
    """
    This decorator is intended to be used on command-like functions that take
    `Configuration` as the first argument and use `commands.ExitCode` as the return
    type. It adds remote logging to the decorated function. What's included in the
    logging records: exit code and running time of the given command, plus the
    exception info if the command raises.
    """

    def decorator(
        __command: "Callable[Concatenate[configuration_module.Configuration, TParams], commands.ExitCode]",  # noqa: B950
    ) -> "Callable[Concatenate[configuration_module.Configuration, TParams], commands.ExitCode]":  # noqa: B950
        def transformed_command(
            configuration: configuration_module.Configuration,
            *args: TParams.args,
            **kwargs: TParams.kwargs
        ) -> ExitCodeWithAdditionalLogging:
            return ExitCodeWithAdditionalLogging(
                exit_code=__command(configuration, *args, **kwargs),
                additional_logging={},
            )

        return log_usage_with_additional_info(command_name)(transformed_command)

    # pyre-ignore[7]: (T84575843) This should be fine.
    return decorator
