# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import json
import os
from typing import Sequence, Dict, Any

from .. import command_arguments
from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
from ..error import ModelVerificationError, print_errors
from .command import ClientException, Result
from .query import Query


class ValidateModels(Query):
    def __init__(
        self,
        command_arguments: command_arguments.CommandArguments,
        *,
        original_directory: str,
        configuration: Configuration,
    ) -> None:
        super(ValidateModels, self).__init__(
            command_arguments=command_arguments,
            original_directory=original_directory,
            configuration=configuration,
            query="validate_taint_models()",
        )

    @staticmethod
    def _relativize_error(
        configuration: Configuration,
        relative_root: str,
        error: ModelVerificationError,
        original_directory: str,
    ) -> ModelVerificationError:
        if error.path is None:
            return error

        path = os.path.realpath(os.path.join(relative_root, error.path))
        # If relative paths don't make sense, keep the absolute path around.
        if not path.startswith(configuration.project_root) or not os.path.exists(path):
            return error

        # Relativize path to user's cwd.
        relative_path = os.path.relpath(path, original_directory)
        return dataclasses.replace(error, path=relative_path)

    @staticmethod
    def parse_errors(
        json_result: Dict[str, Any],
        configuration: Configuration,
        analysis_directory: AnalysisDirectory,
        original_directory: str,
    ) -> Sequence[ModelVerificationError]:
        if "errors" not in json_result:
            return []
        analysis_root = os.path.realpath(analysis_directory.get_root())
        return sorted(
            (
                ValidateModels._relativize_error(
                    configuration,
                    analysis_root,
                    ModelVerificationError.from_json(error_json),
                    original_directory,
                )
                for error_json in json_result["errors"]
            ),
            key=lambda error: (error.path or "?", error.line, error.code),
        )

    def _socket_result_handler(self, result: Result) -> None:
        try:
            json_result = json.loads(result.output)
        except json.JSONDecodeError:
            raise ClientException(f"Invalid JSON output: `{result.output}`.")

        if "response" not in json_result:
            raise ClientException(f"Invalid JSON output: `{json_result}`")
        errors = ValidateModels.parse_errors(
            json_result["response"],
            self._configuration,
            self._analysis_directory,
            self._original_directory,
        )
        print_errors(errors, output=self._output, error_kind="model verification")
