# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import abc
import dataclasses
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
from dataclasses import dataclass, field
from logging import Logger
from pathlib import Path
from typing import (
    Any,
    Dict,
    Iterable,
    List,
    Optional,
    Sequence,
    Set,
    Type,
    TypeVar,
    Union,
)

from . import command_arguments, find_directories
from .exceptions import EnvironmentException
from .filesystem import assert_readable_directory, expand_relative_path
from .find_directories import (
    BINARY_NAME,
    CONFIGURATION_FILE,
    LOCAL_CONFIGURATION_FILE,
    get_relative_local_root,
)
from .resources import LOG_DIRECTORY


LOG: Logger = logging.getLogger(__name__)

T = TypeVar("T")


def _expand_global_root(path: str, global_root: str) -> str:
    if path.startswith("//"):
        return expand_relative_path(global_root, path[2:])
    return path


def _expand_relative_root(path: str, relative_root: str) -> str:
    if not path.startswith("//"):
        return expand_relative_path(relative_root, path)
    return path


def _get_optional_value(source: Optional[T], default: T) -> T:
    return source if source is not None else default


def _expand_and_get_existent_ignore_all_errors_path(
    ignore_all_errors: Iterable[str], project_root: str
) -> List[str]:
    expanded_ignore_paths = []
    for path in ignore_all_errors:
        expanded = glob.glob(_expand_global_root(path, global_root=project_root))
        if not expanded:
            expanded_ignore_paths.append(path)
        else:
            expanded_ignore_paths.extend(expanded)

    paths = []
    for path in expanded_ignore_paths:
        if os.path.exists(path):
            paths.append(path)
        else:
            LOG.warning(f"Nonexistent paths passed in to `ignore_all_errors`: `{path}`")
    return paths


class InvalidConfiguration(Exception):
    def __init__(self, message: str) -> None:
        self.message = f"Invalid configuration: {message}"
        super().__init__(self.message)


class SearchPathElement(abc.ABC):
    @abc.abstractmethod
    def path(self) -> str:
        raise NotImplementedError

    @abc.abstractmethod
    def command_line_argument(self) -> str:
        raise NotImplementedError

    @abc.abstractmethod
    def expand_global_root(self, global_root: str) -> "SearchPathElement":
        raise NotImplementedError

    @abc.abstractmethod
    def expand_relative_root(self, relative_root: str) -> "SearchPathElement":
        raise NotImplementedError


@dataclasses.dataclass(frozen=True)
class SimpleSearchPathElement(SearchPathElement):
    root: str

    def path(self) -> str:
        return self.root

    def command_line_argument(self) -> str:
        return self.root

    def expand_global_root(self, global_root: str) -> SearchPathElement:
        return SimpleSearchPathElement(
            _expand_global_root(self.root, global_root=global_root)
        )

    def expand_relative_root(self, relative_root: str) -> SearchPathElement:
        return SimpleSearchPathElement(
            _expand_relative_root(self.root, relative_root=relative_root)
        )


@dataclasses.dataclass(frozen=True)
class SubdirectorySearchPathElement(SearchPathElement):
    root: str
    subdirectory: str

    def path(self) -> str:
        return os.path.join(self.root, self.subdirectory)

    def command_line_argument(self) -> str:
        return self.root + "$" + self.subdirectory

    def expand_global_root(self, global_root: str) -> SearchPathElement:
        return SubdirectorySearchPathElement(
            root=_expand_global_root(self.root, global_root=global_root),
            subdirectory=self.subdirectory,
        )

    def expand_relative_root(self, relative_root: str) -> SearchPathElement:
        return SubdirectorySearchPathElement(
            root=_expand_relative_root(self.root, relative_root=relative_root),
            subdirectory=self.subdirectory,
        )


@dataclasses.dataclass(frozen=True)
class SitePackageSearchPathElement(SearchPathElement):
    site_root: str
    package_name: str

    def path(self) -> str:
        return os.path.join(self.site_root, self.package_name)

    def command_line_argument(self) -> str:
        return self.site_root + "$" + self.package_name

    def expand_global_root(self, global_root: str) -> SearchPathElement:
        # Site package does not participate in root expansion.
        return self

    def expand_relative_root(self, relative_root: str) -> SearchPathElement:
        # Site package does not participate in root expansion.
        return self


@dataclasses.dataclass
class ExtensionElement:
    suffix: str
    include_suffix_in_module_qualifier: bool

    def command_line_argument(self) -> str:
        options = ""
        if self.include_suffix_in_module_qualifier:
            options = "$" + "include_suffix_in_module_qualifier"
        return self.suffix + options

    @staticmethod
    def from_json(json: Union[str, Dict[str, Union[str, bool]]]) -> "ExtensionElement":
        if isinstance(json, str):
            return ExtensionElement(
                suffix=json, include_suffix_in_module_qualifier=False
            )
        elif isinstance(json, dict):
            include_suffix_in_module_qualifier = False
            if "include_suffix_in_module_qualifier" in json:
                value = json["include_suffix_in_module_qualifier"]
                if isinstance(value, bool):
                    include_suffix_in_module_qualifier = value
            if "suffix" in json:
                suffix = json["suffix"]
                if isinstance(suffix, str):
                    return ExtensionElement(
                        suffix=suffix,
                        include_suffix_in_module_qualifier=include_suffix_in_module_qualifier,
                    )

        raise InvalidConfiguration(f"Invalid extension element: {json}")


def _get_site_roots() -> List[str]:
    try:
        return site.getsitepackages()
    except AttributeError:
        # There are a few Python versions that ship with a broken venv,
        # where `getsitepackages` is not available.
        LOG.warning(
            "`site.getsitepackages()` is not available in your virtualenv. "
            "This is a known virtualenv bug and as a workaround please avoid"
            'using `"site-package"` in your search path configuration.'
        )
        return []


def create_search_paths(
    json: Union[str, Dict[str, str]], site_roots: Iterable[str]
) -> List[SearchPathElement]:
    if isinstance(json, str):
        return [SimpleSearchPathElement(json)]
    elif isinstance(json, dict):
        if "root" in json and "subdirectory" in json:
            return [
                SubdirectorySearchPathElement(
                    root=json["root"], subdirectory=json["subdirectory"]
                )
            ]
        elif "site-package" in json:
            return [
                SitePackageSearchPathElement(
                    site_root=root, package_name=json["site-package"]
                )
                for root in site_roots
            ]

    raise InvalidConfiguration(f"Invalid search path element: {json}")


def assert_readable_directory_in_configuration(
    directory: str, field_name: str = ""
) -> None:
    try:
        assert_readable_directory(directory, error_message_prefix=f"{field_name} ")
    except EnvironmentException as error:
        raise InvalidConfiguration(str(error))


def _in_virtual_environment(override: Optional[bool] = None) -> bool:
    if override is not None:
        return override

    return sys.prefix != sys.base_prefix


@dataclass(frozen=True)
class PartialConfiguration:
    autocomplete: Optional[bool] = None
    binary: Optional[str] = None
    buck_builder_binary: Optional[str] = None
    disabled: Optional[bool] = None
    do_not_ignore_all_errors_in: Sequence[str] = field(default_factory=list)
    dot_pyre_directory: Optional[Path] = None
    excludes: Sequence[str] = field(default_factory=list)
    extensions: Sequence[ExtensionElement] = field(default_factory=list)
    file_hash: Optional[str] = None
    formatter: Optional[str] = None
    ignore_all_errors: Sequence[str] = field(default_factory=list)
    ignore_infer: Sequence[str] = field(default_factory=list)
    isolation_prefix: Optional[str] = None
    logger: Optional[str] = None
    number_of_workers: Optional[int] = None
    other_critical_files: Sequence[str] = field(default_factory=list)
    search_path: Sequence[SearchPathElement] = field(default_factory=list)
    source_directories: Optional[Sequence[SearchPathElement]] = None
    strict: Optional[bool] = None
    taint_models_path: Sequence[str] = field(default_factory=list)
    targets: Optional[Sequence[str]] = None
    typeshed: Optional[str] = None
    use_buck_builder: Optional[bool] = None
    use_buck_source_database: Optional[bool] = None
    version_hash: Optional[str] = None

    @staticmethod
    def _get_depreacted_map() -> Dict[str, str]:
        return {"do_not_check": "ignore_all_errors"}

    @staticmethod
    def _get_extra_keys() -> Set[str]:
        return {
            "buck_mode",
            "create_open_source_configuration",
            "differential",
            "oncall",
            "saved_state",
            "stable_client",
            "taint_models_path",
            "unstable_client",
        }

    @staticmethod
    def from_command_arguments(
        arguments: command_arguments.CommandArguments,
    ) -> "PartialConfiguration":
        strict: Optional[bool] = True if arguments.strict else None
        source_directories = [
            SimpleSearchPathElement(element) for element in arguments.source_directories
        ] or None
        targets: Optional[List[str]] = (
            arguments.targets if len(arguments.targets) > 0 else None
        )
        return PartialConfiguration(
            autocomplete=None,
            binary=arguments.binary,
            buck_builder_binary=arguments.buck_builder_binary,
            disabled=None,
            do_not_ignore_all_errors_in=[],
            dot_pyre_directory=arguments.dot_pyre_directory,
            excludes=arguments.exclude,
            extensions=[],
            file_hash=None,
            formatter=arguments.formatter,
            ignore_all_errors=[],
            ignore_infer=[],
            isolation_prefix=arguments.isolation_prefix,
            logger=arguments.logger,
            number_of_workers=None,
            other_critical_files=[],
            search_path=[
                SimpleSearchPathElement(element) for element in arguments.search_path
            ],
            source_directories=source_directories,
            strict=strict,
            taint_models_path=[],
            targets=targets,
            typeshed=arguments.typeshed,
            use_buck_builder=arguments.use_buck_builder,
            use_buck_source_database=arguments.use_buck_source_database,
            version_hash=None,
        )

    @staticmethod
    def from_string(contents: str) -> "PartialConfiguration":
        def is_list_of_string(elements: object) -> bool:
            return isinstance(elements, list) and all(
                isinstance(element, str) for element in elements
            )

        def ensure_option_type(
            json: Dict[str, Any], name: str, expected_type: Type[T]
        ) -> Optional[T]:
            result = json.pop(name, None)
            if result is None:
                return None
            elif isinstance(result, expected_type):
                return result
            raise InvalidConfiguration(
                f"Configuration `{name}` is expected to have type "
                f"{expected_type} but got: `{json}`."
            )

        def ensure_optional_string_list(
            json: Dict[str, Any], name: str
        ) -> Optional[List[str]]:
            result = json.pop(name, None)
            if result is None:
                return None
            elif is_list_of_string(result):
                return result
            raise InvalidConfiguration(
                f"Configuration `{name}` is expected to be a list of "
                f"strings but got `{json}`."
            )

        def ensure_string_list(
            json: Dict[str, Any], name: str, allow_single_string: bool = False
        ) -> List[str]:
            result = json.pop(name, [])
            if allow_single_string and isinstance(result, str):
                result = [result]
            if is_list_of_string(result):
                return result
            raise InvalidConfiguration(
                f"Configuration `{name}` is expected to be a list of "
                f"strings but got `{json}`."
            )

        def ensure_list(json: Dict[str, Any], name: str) -> List[Any]:
            result = json.pop(name, [])
            if isinstance(result, list):
                return result
            raise InvalidConfiguration(
                f"Configuration `{name}` is expected to be a list but got `{json}`."
            )

        try:
            configuration_json = json.loads(contents)

            if configuration_json.pop("saved_state", None) is not None:
                file_hash = hashlib.sha1(contents.encode("utf-8")).hexdigest()
            else:
                file_hash = None

            dot_pyre_directory = ensure_option_type(
                configuration_json, "dot_pyre_directory", str
            )

            search_path_json = configuration_json.pop("search_path", [])
            if isinstance(search_path_json, list):
                search_path = [
                    element
                    for json in search_path_json
                    for element in create_search_paths(
                        json, site_roots=_get_site_roots()
                    )
                ]
            else:
                search_path = create_search_paths(
                    search_path_json, site_roots=_get_site_roots()
                )

            source_directories_json = ensure_option_type(
                configuration_json, "source_directories", list
            )
            if isinstance(source_directories_json, list):
                source_directories = [
                    element
                    for json in source_directories_json
                    for element in create_search_paths(
                        json, site_roots=_get_site_roots()
                    )
                ]
            else:
                source_directories = None

            partial_configuration = PartialConfiguration(
                autocomplete=ensure_option_type(
                    configuration_json, "autocomplete", bool
                ),
                binary=ensure_option_type(configuration_json, "binary", str),
                buck_builder_binary=ensure_option_type(
                    configuration_json, "buck_builder_binary", str
                ),
                disabled=ensure_option_type(configuration_json, "disabled", bool),
                do_not_ignore_all_errors_in=ensure_string_list(
                    configuration_json, "do_not_ignore_errors_in"
                ),
                dot_pyre_directory=Path(dot_pyre_directory)
                if dot_pyre_directory is not None
                else None,
                excludes=ensure_string_list(
                    configuration_json, "exclude", allow_single_string=True
                ),
                extensions=[
                    ExtensionElement.from_json(json)
                    for json in ensure_list(configuration_json, "extensions")
                ],
                file_hash=file_hash,
                formatter=ensure_option_type(configuration_json, "formatter", str),
                ignore_all_errors=ensure_string_list(
                    configuration_json, "ignore_all_errors"
                ),
                ignore_infer=ensure_string_list(configuration_json, "ignore_infer"),
                isolation_prefix=ensure_option_type(
                    configuration_json, "isolation_prefix", str
                ),
                logger=ensure_option_type(configuration_json, "logger", str),
                number_of_workers=ensure_option_type(
                    configuration_json, "workers", int
                ),
                other_critical_files=ensure_string_list(
                    configuration_json, "critical_files"
                ),
                search_path=search_path,
                source_directories=source_directories,
                strict=ensure_option_type(configuration_json, "strict", bool),
                taint_models_path=ensure_string_list(
                    configuration_json, "taint_models_path", allow_single_string=True
                ),
                targets=ensure_optional_string_list(configuration_json, "targets"),
                typeshed=ensure_option_type(configuration_json, "typeshed", str),
                use_buck_builder=ensure_option_type(
                    configuration_json, "use_buck_builder", bool
                ),
                use_buck_source_database=ensure_option_type(
                    configuration_json, "use_buck_source_database", bool
                ),
                version_hash=ensure_option_type(configuration_json, "version", str),
            )

            # Check for deprecated and unused keys
            for (
                deprecated_key,
                replacement_key,
            ) in PartialConfiguration._get_depreacted_map().items():
                if deprecated_key in configuration_json:
                    configuration_json.pop(deprecated_key)
                    LOG.warning(
                        f"Configuration file uses deprecated item `{deprecated_key}`. "
                        f"Please migrate to its replacement `{replacement_key}`"
                    )
            extra_keys = PartialConfiguration._get_extra_keys()
            for unrecognized_key in configuration_json:
                if unrecognized_key not in extra_keys:
                    LOG.warning(f"Unrecognized configuration item: {unrecognized_key}")

            return partial_configuration
        except json.JSONDecodeError as error:
            raise InvalidConfiguration(f"Invalid JSON file: {error}")

    @staticmethod
    def from_file(path: Path) -> "PartialConfiguration":
        try:
            contents = path.read_text(encoding="utf-8")
            return PartialConfiguration.from_string(contents)
        except OSError as error:
            raise InvalidConfiguration(f"Error when reading {path}: {error}")

    def expand_relative_paths(self, root: str) -> "PartialConfiguration":
        binary = self.binary
        if binary is not None:
            binary = expand_relative_path(root, binary)
        buck_builder_binary = self.buck_builder_binary
        if buck_builder_binary is not None:
            buck_builder_binary = expand_relative_path(root, buck_builder_binary)
        formatter = self.formatter
        if formatter is not None:
            formatter = expand_relative_path(root, formatter)
        logger = self.logger
        if logger is not None:
            logger = expand_relative_path(root, logger)
        source_directories = self.source_directories
        if source_directories is not None:
            source_directories = [
                path.expand_relative_root(root) for path in source_directories
            ]
        typeshed = self.typeshed
        if typeshed is not None:
            typeshed = expand_relative_path(root, typeshed)
        return PartialConfiguration(
            autocomplete=self.autocomplete,
            binary=binary,
            buck_builder_binary=buck_builder_binary,
            disabled=self.disabled,
            do_not_ignore_all_errors_in=[
                expand_relative_path(root, path)
                for path in self.do_not_ignore_all_errors_in
            ],
            dot_pyre_directory=self.dot_pyre_directory,
            excludes=self.excludes,
            extensions=self.extensions,
            file_hash=self.file_hash,
            formatter=formatter,
            ignore_all_errors=[
                expand_relative_path(root, path) for path in self.ignore_all_errors
            ],
            ignore_infer=[
                expand_relative_path(root, path) for path in self.ignore_infer
            ],
            isolation_prefix=self.isolation_prefix,
            logger=logger,
            number_of_workers=self.number_of_workers,
            other_critical_files=[
                expand_relative_path(root, path) for path in self.other_critical_files
            ],
            search_path=[path.expand_relative_root(root) for path in self.search_path],
            source_directories=source_directories,
            strict=self.strict,
            taint_models_path=[
                expand_relative_path(root, path) for path in self.taint_models_path
            ],
            targets=self.targets,
            typeshed=typeshed,
            use_buck_builder=self.use_buck_builder,
            use_buck_source_database=self.use_buck_source_database,
            version_hash=self.version_hash,
        )


def merge_partial_configurations(
    base: PartialConfiguration, override: PartialConfiguration
) -> PartialConfiguration:
    def overwrite_base(base: Optional[T], override: Optional[T]) -> Optional[T]:
        return base if override is None else override

    def prepend_base(base: Sequence[T], override: Sequence[T]) -> Sequence[T]:
        return list(override) + list(base)

    def raise_when_overridden(
        base: Optional[T], override: Optional[T], name: str
    ) -> Optional[T]:
        if base is None:
            return override
        elif override is None:
            return base
        else:
            raise InvalidConfiguration(
                f"Configuration option `{name}` cannot be overridden."
            )

    return PartialConfiguration(
        autocomplete=overwrite_base(base.autocomplete, override.autocomplete),
        binary=overwrite_base(base.binary, override.binary),
        buck_builder_binary=overwrite_base(
            base.buck_builder_binary, override.buck_builder_binary
        ),
        disabled=overwrite_base(base.disabled, override.disabled),
        do_not_ignore_all_errors_in=prepend_base(
            base.do_not_ignore_all_errors_in, override.do_not_ignore_all_errors_in
        ),
        dot_pyre_directory=overwrite_base(
            base.dot_pyre_directory, override.dot_pyre_directory
        ),
        excludes=prepend_base(base.excludes, override.excludes),
        extensions=prepend_base(base.extensions, override.extensions),
        file_hash=overwrite_base(base.file_hash, override.file_hash),
        formatter=overwrite_base(base.formatter, override.formatter),
        ignore_all_errors=prepend_base(
            base.ignore_all_errors, override.ignore_all_errors
        ),
        ignore_infer=prepend_base(base.ignore_infer, override=override.ignore_infer),
        isolation_prefix=overwrite_base(
            base.isolation_prefix, override.isolation_prefix
        ),
        logger=overwrite_base(base.logger, override.logger),
        number_of_workers=overwrite_base(
            base.number_of_workers, override.number_of_workers
        ),
        other_critical_files=prepend_base(
            base.other_critical_files, override.other_critical_files
        ),
        search_path=prepend_base(base.search_path, override.search_path),
        source_directories=raise_when_overridden(
            base.source_directories,
            override.source_directories,
            name="source_directories",
        ),
        strict=overwrite_base(base.strict, override.strict),
        taint_models_path=prepend_base(
            base.taint_models_path, override.taint_models_path
        ),
        targets=raise_when_overridden(base.targets, override.targets, name="targets"),
        typeshed=overwrite_base(base.typeshed, override.typeshed),
        use_buck_builder=overwrite_base(
            base.use_buck_builder, override.use_buck_builder
        ),
        use_buck_source_database=overwrite_base(
            base.use_buck_source_database, override.use_buck_source_database
        ),
        version_hash=overwrite_base(base.version_hash, override.version_hash),
    )


@dataclass(frozen=True)
class Configuration:
    project_root: str
    dot_pyre_directory: Path

    autocomplete: bool = False
    binary: Optional[str] = None
    buck_builder_binary: Optional[str] = None
    disabled: bool = False
    do_not_ignore_all_errors_in: Sequence[str] = field(default_factory=list)
    excludes: Sequence[str] = field(default_factory=list)
    extensions: Sequence[ExtensionElement] = field(default_factory=list)
    file_hash: Optional[str] = None
    formatter: Optional[str] = None
    ignore_all_errors: Sequence[str] = field(default_factory=list)
    ignore_infer: Sequence[str] = field(default_factory=list)
    isolation_prefix: Optional[str] = None
    logger: Optional[str] = None
    number_of_workers: Optional[int] = None
    other_critical_files: Sequence[str] = field(default_factory=list)
    relative_local_root: Optional[str] = None
    search_path: Sequence[SearchPathElement] = field(default_factory=list)
    source_directories: Sequence[SearchPathElement] = field(default_factory=list)
    strict: bool = False
    taint_models_path: Sequence[str] = field(default_factory=list)
    targets: Sequence[str] = field(default_factory=list)
    typeshed: Optional[str] = None
    use_buck_builder: bool = False
    use_buck_source_database: bool = False
    version_hash: Optional[str] = None

    @staticmethod
    def from_partial_configuration(
        project_root: Path,
        relative_local_root: Optional[str],
        partial_configuration: PartialConfiguration,
        in_virtual_environment: Optional[bool] = None,
    ) -> "Configuration":
        search_path = partial_configuration.search_path
        if len(search_path) == 0 and _in_virtual_environment(in_virtual_environment):
            LOG.warning("Using virtual environment site-packages in search path...")
            search_path = [SimpleSearchPathElement(root) for root in _get_site_roots()]

        return Configuration(
            project_root=str(project_root),
            dot_pyre_directory=_get_optional_value(
                partial_configuration.dot_pyre_directory, project_root / LOG_DIRECTORY
            ),
            autocomplete=_get_optional_value(
                partial_configuration.autocomplete, default=False
            ),
            binary=partial_configuration.binary,
            buck_builder_binary=partial_configuration.buck_builder_binary,
            disabled=_get_optional_value(partial_configuration.disabled, default=False),
            do_not_ignore_all_errors_in=partial_configuration.do_not_ignore_all_errors_in,
            excludes=partial_configuration.excludes,
            extensions=partial_configuration.extensions,
            file_hash=partial_configuration.file_hash,
            formatter=partial_configuration.formatter,
            ignore_all_errors=partial_configuration.ignore_all_errors,
            ignore_infer=partial_configuration.ignore_infer,
            isolation_prefix=partial_configuration.isolation_prefix,
            logger=partial_configuration.logger,
            number_of_workers=partial_configuration.number_of_workers,
            other_critical_files=partial_configuration.other_critical_files,
            relative_local_root=relative_local_root,
            search_path=[
                path.expand_global_root(str(project_root)) for path in search_path
            ],
            source_directories=_get_optional_value(
                partial_configuration.source_directories, default=[]
            ),
            strict=_get_optional_value(partial_configuration.strict, default=False),
            taint_models_path=partial_configuration.taint_models_path,
            targets=_get_optional_value(partial_configuration.targets, default=[]),
            typeshed=partial_configuration.typeshed,
            use_buck_builder=_get_optional_value(
                partial_configuration.use_buck_builder, default=False
            ),
            use_buck_source_database=_get_optional_value(
                partial_configuration.use_buck_source_database, default=False
            ),
            version_hash=partial_configuration.version_hash,
        )

    @property
    def log_directory(self) -> str:
        if self.relative_local_root is None:
            return str(self.dot_pyre_directory)
        return str(self.dot_pyre_directory / self.relative_local_root)

    @property
    def local_root(self) -> Optional[str]:
        if self.relative_local_root is None:
            return None
        return os.path.join(self.project_root, self.relative_local_root)

    def get_existent_source_directories(self) -> List[SearchPathElement]:
        return self._get_existent_paths(self.source_directories)

    def get_existent_search_paths(self) -> List[SearchPathElement]:
        existent_paths = self._get_existent_paths(self.search_path)

        typeshed_root = self.get_typeshed_respecting_override()
        typeshed_paths = (
            []
            if typeshed_root is None
            else [
                SimpleSearchPathElement(str(element))
                for element in find_directories.find_typeshed_search_paths(
                    Path(typeshed_root)
                )
            ]
        )

        # pyre-ignore: Unsupported operand [58]: `+` is not supported for
        # operand types `List[SearchPathElement]` and `Union[List[typing.Any],
        # List[SimpleSearchPathElement]]`
        return existent_paths + typeshed_paths

    def _get_existent_paths(
        self, paths: Sequence[SearchPathElement]
    ) -> List[SearchPathElement]:
        existent_paths = []
        for search_path_element in paths:
            search_path = search_path_element.path()
            if os.path.exists(search_path):
                existent_paths.append(search_path_element)
            else:
                LOG.debug(f"Filtering out nonexistent search path: {search_path}")
        return existent_paths

    def get_existent_ignore_infer_paths(self) -> List[str]:
        existent_paths = []
        for path in self.ignore_infer:
            if os.path.exists(path):
                existent_paths.append(path)
            else:
                LOG.warn(f"Filtering out nonexistent path in `ignore_infer`: {path}")
        return existent_paths

    def get_existent_do_not_ignore_errors_in_paths(self) -> List[str]:
        """
        This is a separate method because we want to check for existing files
        at the time this is called, not when the configuration is
        constructed.
        """
        ignore_paths = [
            _expand_global_root(path, global_root=self.project_root)
            for path in self.do_not_ignore_all_errors_in
        ]
        paths = []
        for path in ignore_paths:
            if os.path.exists(path):
                paths.append(path)
            else:
                LOG.debug(
                    "Filtering out nonexistent paths in `do_not_ignore_errors_in`: "
                    f"{path}"
                )
        return paths

    def get_existent_ignore_all_errors_paths(self) -> List[str]:
        """
        This is a separate method because we want to check for existing files
        at the time this is called, not when the configuration is
        constructed.
        """
        return _expand_and_get_existent_ignore_all_errors_path(
            self.ignore_all_errors, self.project_root
        )

    def get_binary_respecting_override(self) -> Optional[str]:
        binary = self.binary
        if binary is not None:
            return binary

        LOG.info(f"No binary specified, looking for `{BINARY_NAME}` in PATH")
        binary_candidate = shutil.which(BINARY_NAME)
        if binary_candidate is None:
            binary_candidate_name = os.path.join(
                os.path.dirname(sys.argv[0]), BINARY_NAME
            )
            binary_candidate = shutil.which(binary_candidate_name)
        if binary_candidate is not None:
            return binary_candidate
        return None

    def get_typeshed_respecting_override(self) -> Optional[str]:
        typeshed = self.typeshed
        if typeshed is not None:
            return typeshed

        LOG.info("No typeshed specified, looking for it...")
        auto_determined_typeshed = find_directories.find_typeshed()
        if auto_determined_typeshed is None:
            LOG.warning(
                "Could not find a suitable typeshed. Types for Python builtins "
                "and standard libraries may be missing!"
            )
            return None
        else:
            LOG.info(f"Found: `{auto_determined_typeshed}`")
            return str(auto_determined_typeshed)

    def get_version_hash_respecting_override(self) -> Optional[str]:
        overriding_version_hash = os.getenv("PYRE_VERSION_HASH")
        if overriding_version_hash:
            LOG.warning(f"Version hash overridden with `{overriding_version_hash}`")
            return overriding_version_hash
        return self.version_hash

    def get_binary_version(self) -> Optional[str]:
        binary = self.get_binary_respecting_override()
        if binary is None:
            return None
        status = subprocess.run(
            [binary, "-version"], stdout=subprocess.PIPE, universal_newlines=True
        )
        return status.stdout.strip() if status.returncode == 0 else None

    def get_number_of_workers(self) -> int:
        number_of_workers = self.number_of_workers
        if number_of_workers is not None and number_of_workers > 0:
            return number_of_workers

        try:
            default_number_of_workers = max(multiprocessing.cpu_count() - 4, 1)
        except NotImplementedError:
            default_number_of_workers = 4

        LOG.info(
            "Could not determine the number of Pyre workers from configuration. "
            f"Auto-set the value to {default_number_of_workers}."
        )
        return default_number_of_workers

    def get_valid_extension_suffixes(self) -> List[str]:
        vaild_extensions = []
        for extension in self.extensions:
            if not extension.suffix.startswith("."):
                LOG.warning(
                    "Filtering out extension which does not start with `.`: "
                    f"`{extension.suffix}`"
                )
            else:
                vaild_extensions.append(extension.suffix)
        return vaild_extensions

    def get_isolation_prefix_respecting_override(self) -> Optional[str]:
        """We need this to disable an isolation prefix set in a configuration.
        Merely omitting the CLI flag would not disable the isolation prefix
        because we would just fall back to the configuration value.

        With this, we can pass `--isolation-prefix ''` as a CLI argument or
        override `isolation_prefix` as `""` in a local configuration."""
        return None if self.isolation_prefix == "" else self.isolation_prefix


def create_configuration(
    arguments: command_arguments.CommandArguments, base_directory: Path
) -> Configuration:
    local_root_argument = arguments.local_configuration
    found_root = find_directories.find_global_and_local_root(
        base_directory
        if local_root_argument is None
        else base_directory / local_root_argument
    )

    command_argument_configuration = PartialConfiguration.from_command_arguments(
        arguments
    )
    if found_root is None:
        project_root = Path.cwd()
        relative_local_root = None
        partial_configuration = command_argument_configuration
    else:
        project_root = found_root.global_root
        relative_local_root = None
        partial_configuration = PartialConfiguration.from_file(
            project_root / CONFIGURATION_FILE
        ).expand_relative_paths(str(project_root))
        local_root = found_root.local_root
        if local_root is not None:
            relative_local_root = get_relative_local_root(project_root, local_root)
            partial_configuration = merge_partial_configurations(
                base=partial_configuration,
                override=PartialConfiguration.from_file(
                    local_root / LOCAL_CONFIGURATION_FILE
                ).expand_relative_paths(str(local_root)),
            )
        partial_configuration = merge_partial_configurations(
            base=partial_configuration, override=command_argument_configuration
        )

    return Configuration.from_partial_configuration(
        project_root, relative_local_root, partial_configuration
    )


def check_nested_local_configuration(configuration: Configuration) -> None:
    """
    Raises `InvalidConfiguration` if the check fails.
    """
    local_root = configuration.local_root
    if local_root is None:
        return

    def is_subdirectory(child: Path, parent: Path) -> bool:
        return parent == child or parent in child.parents

    # We search from the parent of the local root, looking for another local
    # configuration file that lives above the current one
    local_root_path = Path(local_root).resolve()
    current_directory = local_root_path.parent
    while True:
        found_root = find_directories.find_global_and_local_root(current_directory)
        if found_root is None:
            break

        nesting_local_root = found_root.local_root
        if nesting_local_root is None:
            break

        nesting_configuration = PartialConfiguration.from_file(
            nesting_local_root / LOCAL_CONFIGURATION_FILE
        ).expand_relative_paths(str(nesting_local_root))
        nesting_ignored_all_errors_path = (
            _expand_and_get_existent_ignore_all_errors_path(
                nesting_configuration.ignore_all_errors, str(found_root.global_root)
            )
        )
        if not any(
            is_subdirectory(child=local_root_path, parent=Path(path))
            for path in nesting_ignored_all_errors_path
        ):
            error_message = (
                "Local configuration is nested under another local configuration at "
                f"`{nesting_local_root}`.\nPlease add `{local_root_path}` to the "
                "`ignore_all_errors` field of the parent, or combine the sources "
                "into a single configuration, or split the parent configuration to "
                "avoid inconsistent errors."
            )
            raise InvalidConfiguration(error_message)
        current_directory = nesting_local_root.parent
