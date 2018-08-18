# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import functools
import json
import logging
import os
import shutil
from typing import List

from . import (
    BINARY_NAME,
    CONFIGURATION_FILE,
    EnvironmentException,
    assert_readable_directory,
    find_typeshed,
    number_of_workers,
)


LOG = logging.getLogger(__name__)


class InvalidConfiguration(Exception):
    pass


class _ConfigurationFile:
    def __init__(self, file):
        self._configuration = json.load(file)

    def consume(self, key, default=None, current=None, print_on_success=False):
        """
        Consume a key from the configuration. When a key is consumed, it
        is removed from the configuration.

        If not found, the default is returned. If the current value is not
        None, it will be returned instead, but the key will still be
        considered consumed.
        """

        value = self._configuration.pop(key, default)
        if current:
            return current
        if value and print_on_success:
            LOG.debug("Found %s: `%s`", key, ", ".join(value))
        return value

    def unused_keys(self):
        """
        Return all keys not consumed yet. Some keys are explicitly whitelisted.
        """

        return self._configuration.keys() - {
            "continuous",
            "coverage",
            "differential",
            "push_blocking",
        }


class Configuration:
    _disabled = False  # type: bool

    def __init__(
        self,
        original_directory=None,
        local_configuration=None,
        search_path=None,
        typeshed=None,
        preserve_pythonpath=False,
    ) -> None:
        self.analysis_directories = []
        self.targets = []
        self.logger = None
        self.do_not_check = []
        self.number_of_workers = None

        self._version_hash = None
        self._binary = None
        self._typeshed = None
        self._local_configuration = None

        # Handle search path from multiple sources
        self._search_directories = []
        pythonpath = os.getenv("PYTHONPATH")
        if preserve_pythonpath and pythonpath:
            for path in pythonpath.split(":"):
                if os.path.isdir(path):
                    self._search_directories.append(path)
                else:
                    LOG.warning(
                        "`{}` is not a valid directory, dropping it "
                        "from PYTHONPATH".format(path)
                    )
        if search_path:
            self._search_directories.extend(search_path)
        # We will extend the search path further, with the config file
        # items, inside _read().

        if typeshed:
            self._typeshed = typeshed

        if local_configuration:
            # Handle local configuration explicitly configured on the
            # commandline.
            self._check_read_local_configuration(
                local_configuration, fail_on_error=True
            )
        elif original_directory and original_directory != os.getcwd():
            # If `pyre` was run from a directory below the project
            # root, and no local configuration was explictly provided
            # on the commandline, look for a local configuration from
            # the original directory, but don't fail if it does not
            # exist.
            assert_readable_directory(original_directory)
            self._check_read_local_configuration(
                original_directory, fail_on_error=False
            )

        # Order matters. The values will only be updated if a field is None.
        self._read(CONFIGURATION_FILE + ".local", path_from_root="")
        self._read(CONFIGURATION_FILE, path_from_root="")
        self._resolve_versioned_paths()
        self._apply_defaults()

    def validate(self) -> None:
        try:

            def is_list_of_strings(list):
                if len(list) == 0:
                    return True
                return not isinstance(list, str) and all(
                    isinstance(element, str) for element in list
                )

            if not is_list_of_strings(
                self.analysis_directories
            ) or not is_list_of_strings(self.targets):
                raise InvalidConfiguration(
                    "`target` and `analysis_directories` fields must be lists of "
                    "strings."
                )

            if not is_list_of_strings(self.do_not_check):
                raise InvalidConfiguration(
                    "`do_not_check` field must be a list of strings."
                )

            if not self._binary:
                raise InvalidConfiguration("`binary` location must be defined.")
            if not os.path.exists(self.get_binary()):
                raise InvalidConfiguration(
                    "Binary at `{}` does not exist.".format(self._binary)
                )

            if self.number_of_workers < 1:
                raise InvalidConfiguration("Number of workers must be greater than 0.")

            # Validate typeshed path and sub-elements.
            if not self._typeshed:
                raise InvalidConfiguration("`typeshed` location must be defined.")
            assert_readable_directory(self._typeshed)

            # A courtesy warning since we have changed default behaviour.
            if self._typeshed_has_obsolete_value():
                LOG.warning(
                    "It appears that `{}` points at a `stdlib` directory. "
                    "Please note that the `typeshed` configuration must point at "
                    "the root of the `typeshed` directory.".format(self._typeshed)
                )

            typeshed_subdirectories = os.listdir(self._typeshed)
            if "stdlib" not in typeshed_subdirectories:
                raise InvalidConfiguration(
                    "`typeshed` location must contain a `stdlib` directory."
                )

            for typeshed_subdirectory_name in typeshed_subdirectories:
                typeshed_subdirectory = os.path.join(
                    self._typeshed, typeshed_subdirectory_name
                )
                if (
                    not os.path.isdir(typeshed_subdirectory)
                    or typeshed_subdirectory_name == "tests"
                    or typeshed_subdirectory_name[0] == "."
                ):
                    # Ignore some well-known directories we do not care about.
                    continue

                assert_readable_directory(typeshed_subdirectory)
                for typeshed_version_directory_name in os.listdir(
                    typeshed_subdirectory
                ):
                    if not typeshed_version_directory_name[0].isdigit():
                        raise InvalidConfiguration(
                            "Directories inside `typeshed` must only contain "
                            "second-level subdirectories starting with "
                            "a version number."
                        )
                    typeshed_version_directory = os.path.join(
                        typeshed_subdirectory, typeshed_version_directory_name
                    )
                    assert_readable_directory(typeshed_version_directory)

            # Validate elements of the search path.
            for path in self.get_search_path():
                assert_readable_directory(path)
        except InvalidConfiguration as error:
            raise EnvironmentException("Invalid configuration: {}".format(str(error)))

    def get_version_hash(self):
        return self._version_hash

    @functools.lru_cache(1)
    def get_binary(self) -> str:
        if not self._binary:
            raise InvalidConfiguration("Configuration was not validated")

        return self._binary

    @functools.lru_cache(1)
    def get_typeshed(self) -> str:
        if not self._typeshed:
            raise InvalidConfiguration("Configuration was not validated")

        return self._typeshed

    @functools.lru_cache(1)
    def get_search_path(self) -> List[str]:
        return self._search_directories

    def get_local_configuration(self) -> str:
        return self._local_configuration

    def disabled(self) -> bool:
        return self._disabled

    def _check_read_local_configuration(self, path: str, fail_on_error: bool) -> None:
        if fail_on_error and not os.path.exists(path):
            raise EnvironmentException(
                "Local configuration path `{}` does not exist.".format(path)
            )

        if os.path.isdir(path):
            path_from_root = path
            local_configuration = os.path.join(path, CONFIGURATION_FILE + ".local")

            if not os.path.exists(local_configuration):
                if fail_on_error:
                    raise EnvironmentException(
                        "Local configuration directory `{}` does not contain "
                        "a `{}` file.".format(
                            path_from_root, CONFIGURATION_FILE + ".local"
                        )
                    )
                else:
                    LOG.debug(
                        "Configuration will be read from the project root: "
                        "`{}`".format(os.getcwd())
                    )
            else:
                self._local_configuration = local_configuration
        else:
            path_from_root = os.path.dirname(path)
            local_configuration = path
            self._local_configuration = local_configuration
        self._read(local_configuration, path_from_root=path_from_root)

    def _read(self, path, path_from_root) -> None:
        try:
            with open(path) as file:
                LOG.debug("Reading configuration `%s`...", path)

                configuration = _ConfigurationFile(file)

                self.analysis_directories = configuration.consume(
                    "analysis_directories",
                    default=[],
                    current=self.analysis_directories,
                    print_on_success=True,
                )

                self.analysis_directories = configuration.consume(
                    "source_directories",
                    default=[],
                    current=self.analysis_directories,
                    print_on_success=True,
                )

                self.targets = configuration.consume(
                    "targets", default=[], current=self.targets, print_on_success=True
                )

                if configuration.consume("disabled", default=False):
                    self._disabled = True

                self.logger = configuration.consume("logger", current=self.logger)

                do_not_check = configuration.consume("do_not_check", default=[])
                self.do_not_check.extend(
                    map(lambda path: os.path.join(path_from_root, path), do_not_check)
                )

                self.number_of_workers = int(
                    configuration.consume(
                        "workers", default=0, current=self.number_of_workers
                    )
                )

                self._binary = configuration.consume("binary", current=self._binary)

                additional_search_path = configuration.consume(
                    "search_path", default=[]
                )
                if isinstance(additional_search_path, list):
                    self._search_directories.extend(additional_search_path)
                else:
                    self._search_directories.append(additional_search_path)

                self._version_hash = configuration.consume(
                    "version", current=self._version_hash
                )

                self._typeshed = configuration.consume(
                    "typeshed", current=self._typeshed
                )

                unused_keys = configuration.unused_keys()
                if unused_keys:
                    LOG.warning(
                        "Some configuration items were not recognized in {}: {}".format(
                            path, ", ".join(unused_keys)
                        )
                    )
        except IOError:
            LOG.debug("No configuration found at `{}`.".format(path))
        except json.JSONDecodeError as error:
            raise EnvironmentException(
                "Configuration file at `{}` is invalid: {}.".format(path, str(error))
            )

    def _resolve_versioned_paths(self) -> None:
        version_hash = self.get_version_hash()
        binary = self._binary
        if version_hash and binary:
            self._binary = binary.replace("%V", version_hash)
        if version_hash and self._typeshed:
            self._typeshed = self._typeshed.replace("%V", version_hash)

    def _apply_defaults(self) -> None:
        overriding_binary = os.getenv("PYRE_BINARY")
        if overriding_binary:
            self._binary = overriding_binary
            LOG.warning("Binary overridden with `%s`", self._binary)
        if not self._binary:
            LOG.info(
                "No binary specified, looking for `{}` in PATH".format(BINARY_NAME)
            )
            self._binary = shutil.which(BINARY_NAME)
            if not self._binary:
                LOG.warning("Could not find `{}` in PATH".format(BINARY_NAME))
            else:
                LOG.info("Found: `%s`", self._binary)

        overriding_version_hash = os.getenv("PYRE_VERSION_HASH")
        if overriding_version_hash:
            self._version_hash = overriding_version_hash
            LOG.warning("Version hash overridden with `%s`", self._version_hash)

        if not self.number_of_workers:
            self.number_of_workers = number_of_workers()

        if not self._typeshed:
            LOG.info("No typeshed specified, looking for it")
            self._typeshed = find_typeshed()
            if not self._typeshed:
                LOG.warning("Could not find a suitable typeshed")
            else:
                LOG.info("Found: `%s`", self._typeshed)

    def _typeshed_has_obsolete_value(self) -> bool:
        (head, tail) = os.path.split(self._typeshed)
        if tail == "stdlib":
            return True
        if tail != "":
            return False
        # If `path` ends in a slash, tail will be empty.
        (head, tail) = os.path.split(head)
        return tail == "stdlib"
