# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import sys
import time
from typing import Callable, Dict, Optional

from pyre_extensions import ParameterSpecification
from pyre_extensions.type_variable_operators import Concatenate
from typing_extensions import Protocol

from ... import commands, configuration as configuration_module, statistics, version

TParams = ParameterSpecification("TParams")


class _Decorator(Protocol):
    def __call__(
        self,
        __f: "Callable[Concatenate[configuration_module.Configuration, TParams], commands.ExitCode]",  # noqa: B950
    ) -> "Callable[Concatenate[configuration_module.Configuration, TParams], commands.ExitCode]":  # noqa: B950
        ...


def log_usage(command_name: str) -> _Decorator:
    """
    This decorator is intended to be used on command-like functions that take
    `Configuration` as the first argument and use `commands.ExitCode` as the return
    type. It adds remote logging to the decorated function.
    """
    auxiliary_info: Dict[str, Optional[str]] = {
        "cwd": os.getcwd(),
        "client_version": version.__version__,
        "command_line": " ".join(sys.argv),
        "command": command_name,
    }

    def decorator(
        __command: "Callable[Concatenate[configuration_module.Configuration, TParams], commands.ExitCode]",  # noqa: B950
    ) -> "Callable[Concatenate[configuration_module.Configuration, TParams], commands.ExitCode]":  # noqa: B950
        def decorated(
            configuration: configuration_module.Configuration,
            *args: TParams.args,
            **kwargs: TParams.kwargs
        ) -> commands.ExitCode:
            start_time: float = time.time()

            def log_success(exit_code: int) -> None:
                statistics.log_with_configuration(
                    category=statistics.LoggerCategory.USAGE,
                    configuration=configuration,
                    integers={
                        "exit_code": exit_code,
                        "runtime": int((time.time() - start_time) * 1000),
                    },
                    normals=auxiliary_info,
                )

            def log_failure(
                error: Exception, exit_code: int = commands.ExitCode.FAILURE
            ) -> None:
                statistics.log_with_configuration(
                    category=statistics.LoggerCategory.USAGE,
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
                exit_code = __command(configuration, *args, **kwargs)
                log_success(exit_code)
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
