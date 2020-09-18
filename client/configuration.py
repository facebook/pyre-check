# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import glob
import hashlib
import json
import logging
import multiprocessing
import os
import shutil
import site
import subprocess
import sys
from logging import Logger
from pathlib import Path
from typing import Dict, List, Optional, Union

from . import command_arguments, find_directories
from .exceptions import EnvironmentException
from .filesystem import assert_readable_directory, expand_relative_path
from .find_directories import (
    BINARY_NAME,
    CONFIGURATION_FILE,
    LOCAL_CONFIGURATION_FILE,
    find_parent_directory_containing_file,
    find_typeshed,
    get_relative_local_root,
)
from .resources import LOG_DIRECTORY


LOG: Logger = logging.getLogger(__name__)


def _relativize_root(root: str, project_root: str, relative_root: Optional[str]) -> str:
    if root.startswith("//"):
        return expand_relative_path(project_root, root[2:])
    else:
        return expand_relative_path(relative_root or "", root)


class InvalidConfiguration(Exception):
    def __init__(self, message: str) -> None:
        self.message = f"Invalid configuration: {message}"
        super().__init__(self.message)


class SearchPathElement:
    def __init__(self, root: str, subdirectory: Optional[str]) -> None:
        self.root = root
        self.subdirectory = subdirectory

    @staticmethod
    def expand(
        path: Union[Dict[str, str], str],
        project_root: str,
        path_relative_to: Optional[str] = None,
    ) -> "SearchPathElement":
        if isinstance(path, str):
            return SearchPathElement(
                _relativize_root(path, project_root, path_relative_to),
                subdirectory=None,
            )
        else:
            if "root" in path and "subdirectory" in path:
                root = path["root"]
                subdirectory = path["subdirectory"]
                return SearchPathElement(
                    _relativize_root(root, project_root, path_relative_to),
                    subdirectory=subdirectory,
                )
            elif "site-package" in path:
                site_root = site.getsitepackages()
                subdirectory = path["site-package"]

                found_element = None
                for root in site_root:
                    site_package_element = SearchPathElement(
                        _relativize_root(root, project_root, None),
                        subdirectory=subdirectory,
                    )
                    if os.path.isdir(site_package_element.path()):
                        found_element = site_package_element
                if found_element is None:
                    raise InvalidConfiguration(
                        f"Cannot find site package '{subdirectory}'"
                    )
                return found_element
            else:
                raise InvalidConfiguration(
                    "Search path elements must have `root` and `subdirectory` "
                    "specified."
                )

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

    def __eq__(self, other: str) -> bool:
        # We support this for testing.
        if isinstance(other, str):
            return self.path() == other
        else:
            return self.root == other.root and self.subdirectory == other.subdirectory


def assert_readable_directory_in_configuration(
    directory: str, field_name: str = ""
) -> None:
    try:
        assert_readable_directory(directory, error_message_prefix=f"{field_name} ")
    except EnvironmentException as error:
        raise InvalidConfiguration(str(error))


class _ConfigurationFile:
    def __init__(self, file) -> None:
        self._deprecated = {"do_not_check": "ignore_all_errors"}
        contents = file.read()
        self.file_hash: str = hashlib.sha1(contents.encode("utf-8")).hexdigest()
        self._configuration = json.loads(contents)

    def consume(
        self,
        key,
        default=None,
        current=None,
        print_on_success=False,
        raise_on_override=False,
    ):
        """
        Consume a key from the configuration. When a key is consumed, it
        is removed from the configuration.

        If not found, the default is returned. If the current value is not
        None, it will be returned instead, but the key will still be
        considered consumed.
        """

        value = self._configuration.pop(key, default)
        if raise_on_override and current and value:
            raise InvalidConfiguration(
                f"Configuration file may not override `{key}` field."
            )
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
            "buck_mode",
            "differential",
            "stable_client",
            "unstable_client",
            "saved_state",
            "taint_models_path",
        }


class Configuration:
    disabled: bool = False

    def __init__(
        self,
        project_root: str,
        dot_pyre_directory: Path,
        local_root: Optional[str] = None,
        search_path: Optional[List[str]] = None,
        binary: Optional[str] = None,
        typeshed: Optional[str] = None,
        use_buck_builder: Optional[bool] = None,
        buck_builder_binary: Optional[str] = None,
        excludes: Optional[List[str]] = None,
        strict: bool = False,
        formatter: Optional[str] = None,
        logger: Optional[str] = None,
        use_buck_source_database: Optional[bool] = None,
    ) -> None:
        self.source_directories = []
        self.targets = []
        self.logger = logger
        self.formatter = formatter
        self._ignore_all_errors = []
        self.number_of_workers: int = 0
        self.project_root: str = project_root
        self._local_root: Optional[str] = local_root
        self.taint_models_path: List[str] = []
        self.file_hash: Optional[str] = None
        self.extensions: List[str] = []
        self.dot_pyre_directory: Path = dot_pyre_directory

        relative_local_root = get_relative_local_root(
            Path(project_root), Path(local_root) if local_root is not None else None
        )
        self.relative_local_root: Optional[str] = relative_local_root
        self.log_directory: str = str(
            self.dot_pyre_directory
            if relative_local_root is None
            else self.dot_pyre_directory / relative_local_root
        )

        self._version_hash: Optional[str] = None
        self._binary: Optional[str] = None
        self._typeshed: Optional[str] = None
        self._buck_builder_binary: Optional[str] = None
        self.strict: bool = strict
        self._use_buck_builder: Optional[bool] = use_buck_builder
        self._use_buck_source_database: Optional[bool] = use_buck_source_database
        self.ignore_infer: List[str] = []
        self._do_not_ignore_errors_in: List[str] = []

        self._search_path: List[SearchPathElement] = []
        if search_path:
            search_path_elements = [
                SearchPathElement.expand(path, project_root=project_root)
                for path in search_path
            ]
            self._search_path.extend(search_path_elements)
        # We will extend the search path further, with the config file
        # items, inside _read().

        if binary:
            self._binary = binary

        if typeshed:
            self._typeshed = typeshed

        if buck_builder_binary:
            self._buck_builder_binary = buck_builder_binary

        self.excludes: List[str] = []
        if excludes:
            self.excludes.extend(excludes)

        if local_root:
            self._check_read_local_configuration(local_root)

        self.autocomplete = False

        self.other_critical_files: List[str] = []

        project_configuration = os.path.join(project_root, CONFIGURATION_FILE)

        # Order matters. The values will only be updated if a field is None.
        self._read(project_configuration)
        self._override_version_hash()
        self._resolve_versioned_paths()
        self._apply_defaults()
        self._validate()

    @staticmethod
    def from_arguments(
        arguments: command_arguments.CommandArguments, base_directory: Path
    ) -> "Optional[Configuration]":
        local_root_argument = arguments.local_configuration
        found_root = find_directories.find_global_and_local_root(
            base_directory
            if local_root_argument is None
            else base_directory / local_root_argument
        )
        if found_root is None:
            return None

        global_root = found_root.global_root
        local_root = found_root.local_root
        return Configuration(
            project_root=str(global_root),
            local_root=str(local_root) if local_root is not None else None,
            search_path=arguments.search_path,
            binary=arguments.binary,
            typeshed=arguments.typeshed,
            use_buck_builder=arguments.use_buck_builder,
            buck_builder_binary=arguments.buck_builder_binary,
            excludes=arguments.exclude,
            strict=arguments.strict,
            logger=arguments.logger,
            formatter=arguments.formatter,
            dot_pyre_directory=(
                arguments.dot_pyre_directory or global_root / LOG_DIRECTORY
            ),
            use_buck_source_database=arguments.use_buck_source_database,
        )

    def _validate(self) -> None:
        def is_list_of_strings(list):
            if len(list) == 0:
                return True
            return not isinstance(list, str) and all(
                isinstance(element, str) for element in list
            )

        if not is_list_of_strings(self.source_directories) or not is_list_of_strings(
            self.targets
        ):
            raise InvalidConfiguration(
                "`target` and `source_directories` fields must be lists of " "strings."
            )

        if not is_list_of_strings(self._ignore_all_errors):
            raise InvalidConfiguration(
                "`ignore_all_errors` field must be a list of strings."
            )

        if not is_list_of_strings(self.ignore_infer):
            raise InvalidConfiguration(
                "`ignore_infer` field must be a list of strings."
            )

        if not is_list_of_strings(self.extensions):
            raise InvalidConfiguration("`extensions` field must be a list of strings.")
        if not all(
            extension.startswith(".") or not extension for extension in self.extensions
        ):
            raise InvalidConfiguration(
                "`extensions` must only contain strings formatted as `.EXT`"
            )

        if not os.path.exists(self.binary):
            raise InvalidConfiguration(f"Binary at `{self.binary}` does not exist.")

        if self.number_of_workers < 1:
            raise InvalidConfiguration("Number of workers must be greater than 0.")

        # Validate typeshed path and sub-elements.
        assert_readable_directory_in_configuration(self.typeshed, field_name="typeshed")

        # A courtesy warning since we have changed default behaviour.
        if self._typeshed_has_obsolete_value():
            LOG.warning(
                f"It appears that `{self.typeshed}` points at a `stdlib` "
                "directory. Please note that the `typeshed` configuration must "
                "point to the root of the `typeshed` directory."
            )

        non_existent_infer_paths = [
            path for path in self.ignore_infer if not os.path.exists(path)
        ]
        if non_existent_infer_paths:
            LOG.warning(
                "Nonexistent paths passed in to `ignore_infer`: "
                f"`{non_existent_infer_paths}`"
            )
            self.ignore_infer = [
                path
                for path in self.ignore_infer
                if path not in non_existent_infer_paths
            ]

        for typeshed_subdirectory_name in ["stdlib", "third_party"]:
            typeshed_subdirectory = os.path.join(
                self.typeshed, typeshed_subdirectory_name
            )
            assert_readable_directory_in_configuration(typeshed_subdirectory)
            for typeshed_version_directory_name in os.listdir(typeshed_subdirectory):
                if not typeshed_version_directory_name[0].isdigit():
                    raise InvalidConfiguration(
                        "Directories inside `typeshed` must only contain "
                        "second-level subdirectories starting with "
                        "a version number."
                    )
                typeshed_version_directory = os.path.join(
                    typeshed_subdirectory, typeshed_version_directory_name
                )
                assert_readable_directory_in_configuration(typeshed_version_directory)

        # Validate elements of the search path.
        for element in self._search_path:
            assert_readable_directory_in_configuration(
                element.path(), field_name="search_path"
            )

        if not is_list_of_strings(self.other_critical_files):
            raise InvalidConfiguration(
                "`critical_files` field must be a list of strings."
            )

        if not is_list_of_strings(self._do_not_ignore_errors_in):
            raise InvalidConfiguration(
                "`do_not_ignore_errors_in` field must be a list of strings."
            )

    @property
    def version_hash(self) -> str:
        return self._version_hash or "unversioned"

    @property
    def binary(self) -> str:
        binary = self._binary
        if not binary:
            raise InvalidConfiguration("No binary specified.")
        return binary

    @property
    def typeshed(self) -> str:
        typeshed = self._typeshed
        if not typeshed:
            raise InvalidConfiguration("No typeshed specified.")
        return typeshed

    @property
    def buck_builder_binary(self) -> Optional[str]:
        return self._buck_builder_binary

    @property
    def use_buck_builder(self) -> bool:
        return self._use_buck_builder or False

    @property
    def use_buck_source_database(self) -> bool:
        return self._use_buck_source_database or False

    @property
    def search_path(self) -> List[str]:
        if not self._search_path:
            return []
        return [element.command_line_argument() for element in self._search_path]

    @property
    def local_root(self) -> Optional[str]:
        return self._local_root

    def get_existent_do_not_ignore_errors_in_paths(self) -> List[str]:
        """
        This is a separate method because we want to check for existing files
        at the time this is called, not when the configuration is
        constructed.
        """
        ignore_paths = [
            _relativize_root(path, project_root=self.project_root, relative_root=None)
            for path in self._do_not_ignore_errors_in
        ]
        paths = [path for path in ignore_paths if os.path.exists(path)]
        non_existent_paths = set(ignore_paths) - set(paths)
        if non_existent_paths:
            LOG.debug(
                "Filtering out nonexistent paths in `do_not_ignore_errors_in`: "
                f"{non_existent_paths}"
            )
        return paths

    def get_existent_ignore_all_errors_paths(self) -> List[str]:
        """
        This is a separate method because we want to check for existing files
        at the time this is called, not when the configuration is
        constructed.
        """
        expanded_ignore_paths = []
        for path in self._ignore_all_errors:
            rooted_path = _relativize_root(
                path, project_root=self.project_root, relative_root=None
            )
            expanded = glob.glob(rooted_path)
            if not expanded:
                expanded_ignore_paths.append(path)
            else:
                expanded_ignore_paths += expanded

        non_existent_ignore_paths = [
            path for path in expanded_ignore_paths if not os.path.exists(path)
        ]
        if non_existent_ignore_paths:
            LOG.warning(
                "Nonexistent paths passed in to `ignore_all_errors`: "
                f"`{non_existent_ignore_paths}`"
            )
        expanded_ignore_paths = [
            path
            for path in expanded_ignore_paths
            if path not in non_existent_ignore_paths
        ]
        return expanded_ignore_paths

    def get_binary_version(self) -> Optional[str]:
        status = subprocess.run(
            [self.binary, "-version"], stdout=subprocess.PIPE, universal_newlines=True
        )
        if status.returncode == 0:
            return status.stdout.strip()
        else:
            return None

    def _check_nested_configurations(self, local_root: str) -> None:
        # TODO(T67874463): Handle sanity checks against project configurations
        parent_local_root = find_parent_directory_containing_file(
            Path(local_root.rstrip("/")).parent, LOCAL_CONFIGURATION_FILE
        )
        if not parent_local_root:
            return

        parent_error = None
        excluded_from_parent = False
        local_root: Path = Path(local_root).resolve()
        try:
            parent_local_configuration = Configuration(
                project_root=self.project_root,
                local_root=str(parent_local_root),
                search_path=[search_path.root for search_path in self._search_path],
                binary=self._binary,
                typeshed=self._typeshed,
                buck_builder_binary=self._buck_builder_binary,
                excludes=self.excludes,
                logger=self.logger,
                formatter=self.formatter,
                dot_pyre_directory=self.dot_pyre_directory,
            )
            paths_to_ignore = list(local_root.parents) + [local_root]
            for (
                ignore_element
            ) in parent_local_configuration.get_existent_ignore_all_errors_paths():
                if Path(ignore_element).resolve() in paths_to_ignore:
                    excluded_from_parent = True
        except InvalidConfiguration as error:
            parent_error = error

        if not excluded_from_parent:
            relative_path = str(local_root.relative_to(parent_local_root))
            error_message = (
                "Local configuration is nested under another local configuration at "
                f"`{parent_local_root}`.\nPlease add `{relative_path}` to the "
                "`ignore_all_errors` field of the parent, or combine the sources "
                "into a single configuration, or split the parent configuration to "
                "avoid inconsistent errors."
            )
            raise InvalidConfiguration(error_message)
        elif parent_error:
            raise InvalidConfiguration(str(parent_error))

    def _check_read_local_configuration(self, path: str) -> None:
        if not os.path.exists(path):
            raise InvalidConfiguration(
                f"Local configuration path `{path}` does not exist."
            )

        local_configuration = os.path.join(path, LOCAL_CONFIGURATION_FILE)
        if not os.path.exists(local_configuration):
            raise InvalidConfiguration(
                f"Local configuration directory `{path}` does not contain "
                f"a `{LOCAL_CONFIGURATION_FILE}` file."
            )

        self._check_nested_configurations(path)
        self._read(local_configuration)
        if not self.source_directories and not self.targets:
            raise InvalidConfiguration(
                f"Local configuration `{local_configuration}` does not specify "
                " any sources to type check."
            )

    def _read(self, path: str) -> None:
        try:
            with open(path) as file:
                LOG.debug("Reading configuration `%s`...", path)
                configuration_directory = os.path.dirname(os.path.realpath(path))
                configuration = _ConfigurationFile(file)

                self.source_directories = configuration.consume(
                    "source_directories",
                    default=[],
                    current=self.source_directories,
                    print_on_success=True,
                    raise_on_override=True,
                )

                self.targets = configuration.consume(
                    "targets",
                    default=[],
                    current=self.targets,
                    print_on_success=True,
                    raise_on_override=True,
                )

                if configuration.consume("disabled", default=False):
                    self.disabled = True

                self.logger = configuration.consume("logger", current=self.logger)

                self.formatter = configuration.consume(
                    "formatter", current=self.formatter
                )

                self.strict = configuration.consume("strict", default=self.strict)

                ignore_all_errors = configuration.consume(
                    "ignore_all_errors", default=[]
                )
                self._ignore_all_errors.extend(ignore_all_errors)

                ignore_infer = configuration.consume("ignore_infer", default=[])
                self.ignore_infer.extend(ignore_infer)

                self.number_of_workers = int(
                    configuration.consume(
                        "workers", default=0, current=self.number_of_workers
                    )
                )

                binary = configuration.consume("binary", current=self._binary)
                assert binary is None or isinstance(binary, str)
                self._binary = binary

                self._buck_builder_binary = configuration.consume(
                    "buck_builder_binary", current=self._buck_builder_binary
                )

                additional_search_path = configuration.consume(
                    "search_path", default=[]
                )
                if isinstance(additional_search_path, list):
                    self._search_path.extend(
                        [
                            SearchPathElement.expand(
                                path,
                                project_root=self.project_root,
                                path_relative_to=configuration_directory,
                            )
                            for path in additional_search_path
                        ]
                    )
                else:
                    self._search_path.append(
                        SearchPathElement.expand(
                            additional_search_path,
                            project_root=self.project_root,
                            path_relative_to=configuration_directory,
                        )
                    )

                version_hash = configuration.consume(
                    "version", current=self._version_hash
                )
                assert version_hash is None or isinstance(version_hash, str)
                self._version_hash = version_hash

                typeshed = configuration.consume("typeshed", current=self._typeshed)
                assert typeshed is None or isinstance(typeshed, str)
                self._typeshed = typeshed

                taint_models_path = configuration.consume("taint_models_path")
                assert (
                    taint_models_path is None
                    or isinstance(taint_models_path, str)
                    or isinstance(taint_models_path, list)
                )
                if isinstance(taint_models_path, str):
                    self.taint_models_path.append(taint_models_path)
                elif isinstance(taint_models_path, list):
                    self.taint_models_path.extend(taint_models_path)

                excludes = configuration.consume("exclude", default=[])
                # TODO(T71120969): Deprecate use of regexes for configuration paths.
                if isinstance(excludes, list):
                    self.excludes.extend(excludes)
                else:
                    self.excludes.append(excludes)

                extensions = configuration.consume("extensions", default=[])
                self.extensions.extend(extensions)

                # We rely on the configuration SHA1 to make
                if configuration.consume("saved_state"):
                    self.file_hash = configuration.file_hash

                use_buck_builder = configuration.consume("use_buck_builder")
                if self._use_buck_builder is None:
                    self._use_buck_builder = use_buck_builder

                use_buck_source_database = configuration.consume(
                    "use_buck_source_database"
                )
                if self._use_buck_source_database is None:
                    self._use_buck_source_database = use_buck_source_database

                self.autocomplete = configuration.consume("autocomplete", default=False)

                self.other_critical_files = configuration.consume(
                    "critical_files", default=[]
                )

                do_not_ignore_errors_in = configuration.consume(
                    "do_not_ignore_errors_in", default=[]
                )
                assert isinstance(do_not_ignore_errors_in, list)
                self._do_not_ignore_errors_in.extend(do_not_ignore_errors_in)

                # Warn on deprecated fields.
                for deprecated_field in configuration._deprecated.keys():
                    configuration.consume(deprecated_field)

                # This block should be at the bottom to be effective.
                unused_keys = configuration.unused_keys()
                if unused_keys:
                    LOG.warning(
                        "Some configuration items were not recognized in "
                        "`{}`: {}".format(path, ", ".join(unused_keys))
                    )
            self._expand_relative_paths(configuration_directory)
        except IOError:
            # To avoid TOCTTOU bugs, handle IOErrors here silently.
            # We error elsewhere if there weren't enough parameters passed into pyre.
            self._expand_relative_paths(configuration_directory)
        except json.JSONDecodeError as error:
            raise InvalidConfiguration(
                "Configuration file at `{}` is invalid: {}.".format(path, str(error))
            )

    def _expand_relative_paths(self, root: str) -> None:
        self.source_directories = [
            expand_relative_path(root, directory)
            for directory in self.source_directories
        ]

        logger = self.logger
        if logger:
            self.logger = expand_relative_path(root, logger)

        self._ignore_all_errors = [
            expand_relative_path(root, path) for path in self._ignore_all_errors
        ]

        self.ignore_infer = [
            expand_relative_path(root, path) for path in self.ignore_infer
        ]

        binary = self._binary
        if binary:
            self._binary = expand_relative_path(root, binary)

        buck_builder_binary = self._buck_builder_binary
        if buck_builder_binary is not None:
            self._buck_builder_binary = expand_relative_path(root, buck_builder_binary)

        typeshed = self._typeshed
        if typeshed is not None:
            self._typeshed = expand_relative_path(root, typeshed)

        self.taint_models_path = [
            expand_relative_path(root, path) for path in self.taint_models_path
        ]

        self.other_critical_files = [
            expand_relative_path(root, path) for path in self.other_critical_files
        ]

        self._do_not_ignore_errors_in = [
            expand_relative_path(root, path) for path in self._do_not_ignore_errors_in
        ]

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
            LOG.info(f"No binary specified, looking for `{BINARY_NAME}` in PATH")
            self._binary = shutil.which(BINARY_NAME)
            if not self._binary:
                binary_candidate = os.path.join(
                    os.path.dirname(sys.argv[0]), BINARY_NAME
                )
                self._binary = shutil.which(binary_candidate)
            if not self._binary:
                LOG.warning(f"Could not find `{BINARY_NAME}` in PATH")
            else:
                LOG.info("Found: `%s`", self._binary)

        if self.number_of_workers == 0:
            try:
                self.number_of_workers = max(multiprocessing.cpu_count() - 4, 1)
            except NotImplementedError:
                self.number_of_workers = 4

        if not self._typeshed:
            LOG.info("No typeshed specified, looking for it")
            typeshed_path = find_typeshed()
            if not typeshed_path:
                LOG.warning("Could not find a suitable typeshed")
            else:
                LOG.info(f"Found: `{typeshed_path}`")
            self._typeshed = str(typeshed_path) if typeshed_path is not None else None

    def _typeshed_has_obsolete_value(self) -> bool:
        (head, tail) = os.path.split(self.typeshed)
        if tail == "stdlib":
            return True
        if tail != "":
            return False
        # If `path` ends in a slash, tail will be empty.
        (head, tail) = os.path.split(head)
        return tail == "stdlib"
