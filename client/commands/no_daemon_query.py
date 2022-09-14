# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


from __future__ import annotations

import contextlib

import dataclasses
import subprocess
from typing import Any, Dict, Iterator, Optional

from .. import configuration as configuration_module
from . import backend_arguments, frontend_configuration, query_response


@dataclasses.dataclass(frozen=True)
class Arguments:
    """
    Data structure for configuration options the backend check command can recognize.
    Need to keep in sync with `source/command/checkCommand.ml`
    """

    base_arguments: backend_arguments.BaseArguments

    query: str
    no_validation_on_class_lookup_failure: bool

    def serialize(self) -> Dict[str, Any]:
        return {
            **self.base_arguments.serialize(),
            "query": self.query,
            "no_validation_on_class_lookup_failure": self.no_validation_on_class_lookup_failure,
        }


# TODO:T131533391 - Factor this function out as it is duplicated.
def _create_no_daemon_query_arguments(
    configuration: frontend_configuration.Base,
    query: str,
    no_validation_on_class_lookup_failure: bool,
) -> Arguments:
    """
    Translate client configurations to backend query configurations.
    """
    source_paths = backend_arguments.get_source_path_for_check(configuration)
    return Arguments(
        base_arguments=backend_arguments.BaseArguments(
            log_path=str(configuration.get_log_directory()),
            global_root=str(configuration.get_global_root()),
            source_paths=source_paths,
            checked_directory_allowlist=backend_arguments.get_checked_directory_allowlist(
                configuration, source_paths
            ),
            checked_directory_blocklist=(configuration.get_ignore_all_errors()),
            excludes=configuration.get_excludes(),
            extensions=configuration.get_valid_extension_suffixes(),
            relative_local_root=configuration.get_relative_local_root(),
            number_of_workers=configuration.get_number_of_workers(),
            python_version=configuration.get_python_version(),
            shared_memory=configuration.get_shared_memory(),
            search_paths=configuration.get_existent_search_paths(),
        ),
        query=query,
        no_validation_on_class_lookup_failure=no_validation_on_class_lookup_failure,
    )


@contextlib.contextmanager
def create_no_daemon_arguments_and_cleanup(
    configuration: frontend_configuration.Base,
    query_str: str,
    no_validation_on_class_lookup_failure: bool,
) -> Iterator[Arguments]:
    arguments = _create_no_daemon_query_arguments(
        configuration, query_str, no_validation_on_class_lookup_failure
    )
    try:
        yield arguments
    finally:
        # It is safe to clean up source paths after check command since
        # any created artifact directory won't be reused by other commands.
        arguments.base_arguments.source_paths.cleanup()


def execute_query(
    configuration: frontend_configuration.Base,
    query_text: str,
    no_validation_on_class_lookup_failure: bool,
) -> Optional[query_response.Response]:

    binary_location = configuration.get_binary_location(download_if_needed=True)
    if binary_location is None:
        raise configuration_module.InvalidConfiguration(
            "Cannot locate a Pyre binary to run."
        )

    with create_no_daemon_arguments_and_cleanup(
        configuration, query_text, no_validation_on_class_lookup_failure
    ) as arguments, backend_arguments.temporary_argument_file(
        arguments
    ) as argument_file_path, backend_arguments.backend_log_file(
        prefix="pyre_query"
    ) as log_file:
        query_command = [
            str(binary_location),
            "no-daemon-query",
            str(argument_file_path),
        ]
        # lint-ignore: NoUnsafeExecRule
        result = subprocess.run(
            query_command,
            stdout=subprocess.PIPE,
            stderr=log_file.file,
        )
        return_code = result.returncode
        # Interpretation of the return code needs to be kept in sync with
        # `source/command/noDaemonQueryCommand.ml`.
        if return_code == 0:
            raw_response = result.stdout.decode("utf-8")
            return query_response.Response.parse(raw_response)
