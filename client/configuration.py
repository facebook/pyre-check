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
        self.source_directories = []
        self.targets = []
        self.logger = None
        self.do_not_check = []
        self.number_of_workers = None

        self._version_hash = None
        self._binary = None
        self._typeshed = None

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
                self.source_directories
            ) or not is_list_of_strings(self.targets):
                raise InvalidConfiguration(
                    "`target` and `source_directories` fields must be lists of "
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
                    LOG.info(
                        "Configuration will be read from the project root: "
                        "`{}`".format(os.getcwd())
                    )
        else:
            path_from_root = os.path.dirname(path)
            local_configuration = path
        self._read(local_configuration, path_from_root=path_from_root)

    def _read(self, path, path_from_root) -> None:
        try:
            with open(path) as file:
                LOG.debug("Reading configuration `%s`...", path)

                configuration = json.load(file)

                if not self.source_directories:
                    self.source_directories = configuration.get(
                        "source_directories", []
                    )
                    if self.source_directories:
                        LOG.debug(
                            "Found source directories `%s`",
                            ", ".join(self.source_directories),
                        )

                if not self.targets:
                    self.targets = configuration.get("targets", [])
                    if self.targets:
                        LOG.debug("Found targets `%s`", ", ".join(self.targets))

                if "disabled" in configuration:
                    self._disabled = True

                if not self.logger:
                    self.logger = configuration.get("logger")

                self.do_not_check.extend(
                    map(
                        lambda path: os.path.join(path_from_root, path),
                        configuration.get("do_not_check", []),
                    )
                )
                # Temporarily support former name "autogenerated"
                self.do_not_check.extend(
                    map(
                        lambda path: os.path.join(path_from_root, path),
                        configuration.get("autogenerated", []),
                    )
                )

                if not self.number_of_workers:
                    self.number_of_workers = int(configuration.get("workers", 0))

                if not self._binary:
                    self._binary = configuration.get("binary")

                additional_search_path = configuration.get("search_path", [])
                if isinstance(additional_search_path, list):
                    self._search_directories.extend(additional_search_path)
                else:
                    self._search_directories.append(additional_search_path)

                if not self._version_hash:
                    self._version_hash = configuration.get("version")

                if not self._typeshed:
                    self._typeshed = configuration.get("typeshed")
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
