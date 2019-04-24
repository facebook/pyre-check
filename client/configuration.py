# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import hashlib
import json
import logging
import os
import shutil
import sys
from typing import Dict, List, Optional, Union

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


class SearchPathElement:
    def __init__(self, element: Union[Dict[str, str], str]) -> None:
        if isinstance(element, str):
            self.root = os.path.abspath(element)
            self.subdirectory = None
        else:
            if "root" not in element or "subdirectory" not in element:
                raise InvalidConfiguration(
                    "Search path elements must have `root` and `subdirectory` specified."
                )
            self.root = os.path.abspath(element["root"])
            self.subdirectory = element["subdirectory"]

    def path(self) -> str:
        subdirectory = self.subdirectory
        if subdirectory is not None:
            return os.path.join(self.root, subdirectory)
        else:
            return self.root

    def command_line_argument(self) -> str:
        subdirectory = self.subdirectory
        if subdirectory is not None:
            return self.root + "$" + subdirectory
        else:
            return self.root

    def __eq__(self, other) -> bool:
        # We support this for testing.
        if isinstance(other, str):
            return self.path() == other
        else:
            return self.root == other.root and self.subdirectory == other.subdirectory


class _ConfigurationFile:
    def __init__(self, file):
        self._deprecated = {"do_not_check": "ignore_all_errors"}
        contents = file.read()
        self.file_hash = hashlib.sha1(contents.encode("utf-8")).hexdigest()
        self._configuration = json.loads(contents)

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
        if value and key in self._deprecated:
            LOG.warning(
                "Configuration file uses deprecated item `%s`: "
                "please migrate to its replacement `%s`",
                key,
                self._deprecated[key],
            )
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
            "saved_state",
            "taint_models_path",
        }


class Configuration:
    disabled = False  # type: bool

    def __init__(
        self,
        local_configuration: Optional[str] = None,
        search_path: Optional[List[str]] = None,
        binary: Optional[str] = None,
        typeshed: Optional[str] = None,
        preserve_pythonpath=False,
        excludes: Optional[List[str]] = None,
    ) -> None:
        self.source_directories = []
        self.targets = []
        self.logger = None
        self.ignore_all_errors = []
        self.number_of_workers = None
        self.local_configuration = None  # type: Optional[str]
        self.taint_models_path = None
        self.file_hash = None  # type: Optional[str]
        self.extensions = []  # type: List[str]

        self._version_hash = None  # type: Optional[str]
        self._binary = None  # type: Optional[str]
        self._typeshed = None  # type: Optional[str]
        self.strict = False  # type: bool

        self.use_buck_builder = False  # type: bool
        self.ignore_unbuilt_dependencies = False  # type: bool

        # Handle search path from multiple sources
        self._search_path = []
        if preserve_pythonpath:
            for path in os.getenv("PYTHONPATH", default="").split(":"):
                if path != "":
                    if os.path.isdir(path):
                        self._search_path.append(SearchPathElement(path))
                    else:
                        LOG.warning(
                            "`{}` is not a valid directory, dropping it "
                            "from PYTHONPATH".format(path)
                        )
            # sys.path often includes '' and a zipped python version, so
            # we don't log warnings for non-dir entries
            sys_path = [
                SearchPathElement(path) for path in sys.path if os.path.isdir(path)
            ]
            self._search_path.extend(sys_path)
        if search_path:
            search_path_elements = [SearchPathElement(path) for path in search_path]
            self._search_path.extend(search_path_elements)
        # We will extend the search path further, with the config file
        # items, inside _read().

        if binary:
            self._binary = binary

        if typeshed:
            self._typeshed = typeshed

        self.excludes = []  # type: List[str]
        if excludes:
            self.excludes.extend(excludes)

        if local_configuration:
            # Handle local configuration explicitly configured on the
            # commandline.
            self._check_read_local_configuration(
                local_configuration, fail_on_error=True
            )

        # Order matters. The values will only be updated if a field is None.
        self._read(CONFIGURATION_FILE)
        self._override_version_hash()
        self._resolve_versioned_paths()
        self._apply_defaults()
        self._validate()

    def _validate(self) -> None:
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

            if not is_list_of_strings(self.ignore_all_errors):
                raise InvalidConfiguration(
                    "`ignore_all_errors` field must be a list of strings."
                )

            if not is_list_of_strings(self.extensions):
                raise InvalidConfiguration(
                    "`extensions` field must be a list of strings."
                )
            if not all(
                extension.startswith(".") or not extension
                for extension in self.extensions
            ):
                raise InvalidConfiguration(
                    "`extensions` must only contain strings formatted as `.EXT`"
                )

            if not os.path.exists(self.binary):
                raise InvalidConfiguration(
                    "Binary at `{}` does not exist.".format(self.binary)
                )

            number_of_workers = self.number_of_workers
            if number_of_workers and number_of_workers < 1:
                raise InvalidConfiguration("Number of workers must be greater than 0.")

            # Validate typeshed path and sub-elements.
            assert_readable_directory(self.typeshed)

            # A courtesy warning since we have changed default behaviour.
            if self._typeshed_has_obsolete_value():
                LOG.warning(
                    "It appears that `{}` points at a `stdlib` directory. "
                    "Please note that the `typeshed` configuration must point at "
                    "the root of the `typeshed` directory.".format(self.typeshed)
                )

            typeshed_subdirectories = os.listdir(self.typeshed)
            if "stdlib" not in typeshed_subdirectories:
                raise InvalidConfiguration(
                    "`typeshed` location must contain a `stdlib` directory."
                )

            for typeshed_subdirectory_name in typeshed_subdirectories:
                typeshed_subdirectory = os.path.join(
                    self.typeshed, typeshed_subdirectory_name
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
            for element in self._search_path:
                assert_readable_directory(element.path())
        except InvalidConfiguration as error:
            raise EnvironmentException("Invalid configuration: {}".format(str(error)))

    @property
    def version_hash(self) -> str:
        return self._version_hash or "unversioned"

    @property
    def binary(self) -> str:
        if not self._binary:
            raise InvalidConfiguration("Configuration was not validated")
        return self._binary

    @property
    def typeshed(self) -> str:
        if not self._typeshed:
            raise InvalidConfiguration("Configuration invalid: no typeshed specified")
        return self._typeshed

    @property
    def search_path(self) -> List[str]:
        if not self._search_path:
            return []
        return [element.command_line_argument() for element in self._search_path]

    @property
    def local_configuration_root(self) -> Optional[str]:
        if self.local_configuration:
            if os.path.isdir(self.local_configuration):
                return self.local_configuration
            else:
                return os.path.dirname(self.local_configuration)

    def _check_read_local_configuration(self, path: str, fail_on_error: bool) -> None:
        if fail_on_error and not os.path.exists(path):
            raise EnvironmentException(
                "Local configuration path `{}` does not exist.".format(path)
            )

        if os.path.isdir(path):
            local_configuration = os.path.join(path, CONFIGURATION_FILE + ".local")

            if not os.path.exists(local_configuration):
                if fail_on_error:
                    raise EnvironmentException(
                        "Local configuration directory `{}` does not contain "
                        "a `{}` file.".format(path, CONFIGURATION_FILE + ".local")
                    )
                else:
                    LOG.debug(
                        "Configuration will be read from the project root: "
                        "`{}`".format(os.getcwd())
                    )
            else:
                self.local_configuration = local_configuration
        else:
            local_configuration = path
            self.local_configuration = local_configuration
        self._read(local_configuration)

    def _read(self, path: str) -> None:
        try:
            with open(path) as file:
                LOG.debug("Reading configuration `%s`...", path)

                configuration = _ConfigurationFile(file)

                source_directories = configuration.consume(
                    "source_directories",
                    default=[],
                    current=self.source_directories,
                    print_on_success=True,
                )
                configuration_directory = os.path.dirname(path)
                if configuration_directory:
                    self.source_directories = [
                        os.path.join(configuration_directory, directory)
                        for directory in source_directories
                    ]
                else:
                    self.source_directories = source_directories

                self.targets = configuration.consume(
                    "targets", default=[], current=self.targets, print_on_success=True
                )

                if configuration.consume("disabled", default=False):
                    self.disabled = True

                self.logger = configuration.consume("logger", current=self.logger)

                self.strict = configuration.consume("strict", default=self.strict)
                ignore_all_errors = configuration.consume(
                    "ignore_all_errors", default=[]
                )
                # Deprecated.
                ignore_all_errors += configuration.consume("do_not_check", default=[])
                configuration_path = os.path.dirname(os.path.realpath(path))
                self.ignore_all_errors.extend(
                    map(
                        lambda do_not_check_relative_to_configuration: os.path.realpath(
                            os.path.join(
                                configuration_path,
                                do_not_check_relative_to_configuration,
                            )
                        ),
                        ignore_all_errors,
                    )
                )

                self.number_of_workers = int(
                    configuration.consume(
                        "workers", default=0, current=self.number_of_workers
                    )
                )

                binary = configuration.consume("binary", current=self._binary)
                assert binary is None or isinstance(binary, str)
                self._binary = binary

                additional_search_path = configuration.consume(
                    "search_path", default=[]
                )

                if isinstance(additional_search_path, list):
                    self._search_path.extend(
                        [
                            SearchPathElement(element)
                            for element in additional_search_path
                        ]
                    )
                else:
                    self._search_path.append(SearchPathElement(additional_search_path))

                version_hash = configuration.consume(
                    "version", current=self._version_hash
                )
                assert version_hash is None or isinstance(version_hash, str)
                self._version_hash = version_hash

                typeshed = configuration.consume("typeshed", current=self._typeshed)
                assert typeshed is None or isinstance(typeshed, str)
                self._typeshed = typeshed

                taint_models_path = configuration.consume(
                    "taint_models_path", current=self.taint_models_path
                )
                assert taint_models_path is None or isinstance(taint_models_path, str)
                self.taint_models_path = taint_models_path

                excludes = configuration.consume("exclude", default=[])
                if isinstance(excludes, list):
                    self.excludes.extend(excludes)
                else:
                    self.excludes.append(excludes)

                extensions = configuration.consume("extensions", default=[])
                self.extensions.extend(extensions)

                # We rely on the configuration SHA1 to make
                if configuration.consume("saved_state"):
                    self.file_hash = configuration.file_hash

                if configuration.consume("use_buck_builder", default=False):
                    self.use_buck_builder = True

                if configuration.consume("ignore_unbuilt_dependencies", default=False):
                    self.ignore_unbuilt_dependencies = True

                # This block should be at the bottom to be effective.
                unused_keys = configuration.unused_keys()
                if unused_keys:
                    LOG.warning(
                        "Some configuration items were not recognized in "
                        "`{}`: {}".format(path, ", ".join(unused_keys))
                    )
        except IOError:
            # To avoid TOCTTOU bugs, handle IOErrors here silently.
            # We error elsewhere if there weren't enough parameters passed into pyre.
            pass
        except json.JSONDecodeError as error:
            raise EnvironmentException(
                "Configuration file at `{}` is invalid: {}.".format(path, str(error))
            )

    def _resolve_versioned_paths(self) -> None:
        version_hash = self.version_hash
        if not version_hash:
            return

        binary = self._binary
        if binary:
            self._binary = binary.replace("%V", version_hash)
        typeshed = self._typeshed
        if typeshed:
            self._typeshed = typeshed.replace("%V", version_hash)

    def _override_version_hash(self) -> None:
        overriding_version_hash = os.getenv("PYRE_VERSION_HASH")
        if overriding_version_hash:
            self._version_hash = overriding_version_hash
            LOG.warning("Version hash overridden with `%s`", self._version_hash)

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
        (head, tail) = os.path.split(self.typeshed)
        if tail == "stdlib":
            return True
        if tail != "":
            return False
        # If `path` ends in a slash, tail will be empty.
        (head, tail) = os.path.split(head)
        return tail == "stdlib"
