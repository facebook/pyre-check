# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
import subprocess
from typing import Set, Sequence, Tuple

# pyre-fixme[21]: Could not find a module corresponding to import `re2`
import re2  # Python wrapper for Google's RE2 used by the server

LOG: logging.Logger = logging.getLogger(__name__)


class InvalidTaintConfigError(Exception):
    def __init__(self, message: str) -> None:
        self.message = f"Invalid taint.config encountered:\n{message}"
        super().__init__(self.message)


def valid_regex(regexp: str) -> bool:
    try:
        re2.compile(regexp)
        return True
    except re2.error:
        return False


def verify_configs_in(taint_models_paths: Sequence[str]) -> None:
    global_warning_codes: Set[int] = set()
    global_source_names: Set[str] = set()
    global_sink_names: Set[str] = set()
    invalid_warning_codes: Set[Tuple[int, str]] = set()
    invalid_sources: Set[Tuple[str, str]] = set()
    invalid_sinks: Set[Tuple[str, str]] = set()
    invalid_implicit_sinks: Set[Tuple[str, str]] = set()
    invalid_implicit_sources: Set[Tuple[str, str]] = set()

    LOG.debug("Verifying the taint.config files")
    for taint_models_path in taint_models_paths:
        find_config_files = (
            subprocess.check_output(
                [
                    "find",
                    taint_models_path,
                    "-name",
                    "taint.config",
                    "-type",
                    "f",
                    "-print",
                ],
                stderr=subprocess.DEVNULL,
            )
            .decode("utf-8")
            .strip()
        )

        config_files = find_config_files.split("\n") if find_config_files else []
        for config_file in config_files:
            config_file_object = open(config_file, "r")
            config_file_contents = json.load(config_file_object)
            config_file_object.close()
            rules = config_file_contents.get("rules", [])
            source_names_in_config = {
                source.get("name", None)
                for source in config_file_contents.get("sources", [])
            }
            global_source_names.update(source_names_in_config)
            sink_names_in_config = {
                sink.get("name", None) for sink in config_file_contents.get("sinks", [])
            }
            global_sink_names.update(sink_names_in_config)

            for rule in rules:
                # Check for duplicate warning codes
                local_warning_code = rule.get("code", None)
                if local_warning_code is not None:
                    if local_warning_code in global_warning_codes:
                        invalid_warning_codes.add((local_warning_code, config_file))
                    else:
                        global_warning_codes.add(local_warning_code)

                # Check whether sources and sinks in rules exist,
                # if not found, put for later check as they may be in other configs
                local_sources = rule.get("sources", [])
                local_sinks = rule.get("sinks", [])
                for local_source in local_sources:
                    if local_source not in global_source_names:
                        invalid_sources.add((local_source, config_file))
                for local_sink in local_sinks:
                    if local_sink not in global_sink_names:
                        invalid_sinks.add((local_sink, config_file))

            # Validate regex expression of implicit sources and sinks
            for implicit_source in config_file_contents.get(
                "implicit_sources", dict()
            ).get("literal_strings", []):
                if not valid_regex(implicit_source.get("regexp", "")):
                    invalid_implicit_sources.add(
                        (implicit_source.get("kind", "Undefined kind"), config_file)
                    )
            for implicit_sink in config_file_contents.get(
                "implicit_sinkss", dict()
            ).get("literal_strings", []):
                if not valid_regex(implicit_sink.get("regexp", "")):
                    invalid_implicit_sinks.add(
                        (implicit_sink.get("kind", "Undefined kind"), config_file)
                    )

    # Are the sources and sinks put for check later, if any, defined somewhere?
    for invalid_source in invalid_sources:
        if invalid_source[0] in global_source_names:
            invalid_sources.remove(invalid_source)
    for invalid_sink in invalid_sinks:
        if invalid_sink[0] in global_sink_names:
            invalid_sinks.remove(invalid_sink)

    # Check and raise exception if there is an error
    exception_message = ""
    if any(invalid_warning_codes):
        for invalid_warning_code, config_file in invalid_warning_codes:
            exception_message += f"{config_file}: `{invalid_warning_code}` is a duplicate warning code.\n"
    if any(invalid_sources):
        for source_name, config_file in invalid_sources:
            exception_message += f"{config_file}: `{source_name}` is not defined as a source in any taint.config file.\n"
    if any(invalid_sinks):
        for sink_name, config_file in invalid_sinks:
            exception_message += f"{config_file}: `{sink_name}` is not defined as a sink in any taint.config file.\n"
    if any(invalid_implicit_sources):
        for source, config_file in invalid_implicit_sources:
            exception_message += f"{config_file}: Implicit source of kind `{source}` defined in has an invalid re2 regex expression.\n"
    if any(invalid_implicit_sinks):
        for sink, config_file in invalid_implicit_sinks:
            exception_message += f"{config_file}: Implicit sink of kind `{sink}` defined in has an invalid re2 regex expression.\n"
    if exception_message:
        raise InvalidTaintConfigError(exception_message)
    LOG.debug("Validated all taint.config files")
