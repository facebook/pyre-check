# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import dataclasses
import glob
import json
import logging
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

import psutil

from .. import command_arguments, dataclasses_merge, find_directories
from ..filesystem import expand_global_root, expand_relative_path
from ..find_directories import (
    BINARY_NAME,
    CONFIGURATION_FILE,
    get_relative_local_root,
    LOCAL_CONFIGURATION_FILE,
    LOG_DIRECTORY,
)
from . import (
    exceptions,
    ide_features as ide_features_module,
    platform_aware,
    python_version as python_version_module,
    search_path as search_path_module,
    shared_memory as shared_memory_module,
    site_packages,
    unwatched,
)


LOG: Logger = logging.getLogger(__name__)
T = TypeVar("T")


def _get_optional_value(source: Optional[T], default: T) -> T:
    return source if source is not None else default


def _expand_and_get_existent_ignore_all_errors_path(
    ignore_all_errors: Iterable[str], project_root: str
) -> List[str]:
    expanded_ignore_paths = []
    for path in ignore_all_errors:
        expanded = glob.glob(expand_global_root(path, global_root=project_root))
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
            if _is_glob(path):
                LOG.warning(
                    f"Within `ignore_all_errors`, no matches found to glob pattern: `{path}`"
                )
            else:
                LOG.warning(
                    f"Nonexistent paths passed in to `ignore_all_errors`: `{path}`"
                )
    return paths


def _is_glob(path: str) -> bool:
    if ("*" in path) or ("?" in path) or (("[" in path) and ("]" in path)):
        return True
    return False


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

        raise exceptions.InvalidConfiguration(f"Invalid extension element: {json}")


def get_default_site_roots() -> List[str]:
    try:
        return [site.getusersitepackages()] + site.getsitepackages()
    except AttributeError:
        # There are a few Python versions that ship with a broken venv,
        # where `getsitepackages` is not available.
        LOG.warning(
            "Either `site.getusersitepackages()` or `site.getsitepackages()` "
            + "is not available in your virtualenv. This is a known virtualenv "
            + 'bug and as a workaround please explicitly specify `"site_root"` '
            + "in your Pyre configuration."
        )
        return []


@dataclasses_merge.dataclass_merge
@dataclass(frozen=True)
class PartialConfiguration:
    binary: Optional[str] = None
    buck_mode: Optional[platform_aware.PlatformAware[str]] = field(
        default=None,
        metadata={"merge_policy": platform_aware.PlatformAware.merge_optional},
    )
    do_not_ignore_errors_in: Sequence[str] = field(
        default_factory=list,
        metadata={"merge_policy": dataclasses_merge.Policy.PREPEND},
    )
    dot_pyre_directory: Optional[Path] = None
    excludes: Sequence[str] = field(
        default_factory=list,
        metadata={"merge_policy": dataclasses_merge.Policy.PREPEND},
    )
    extensions: Sequence[ExtensionElement] = field(
        default_factory=list,
        metadata={"merge_policy": dataclasses_merge.Policy.PREPEND},
    )
    ide_features: Optional[ide_features_module.IdeFeatures] = field(
        default=None,
        metadata={"merge_policy": ide_features_module.IdeFeatures.merge_optional},
    )
    ignore_all_errors: Sequence[str] = field(
        default_factory=list,
        metadata={"merge_policy": dataclasses_merge.Policy.PREPEND},
    )
    isolation_prefix: Optional[str] = None
    logger: Optional[str] = None
    number_of_workers: Optional[int] = None
    oncall: Optional[str] = None
    other_critical_files: Sequence[str] = field(
        default_factory=list,
        metadata={"merge_policy": dataclasses_merge.Policy.PREPEND},
    )
    pysa_version_hash: Optional[str] = None
    python_version: Optional[python_version_module.PythonVersion] = None
    search_path: Sequence[search_path_module.RawElement] = field(
        default_factory=list,
        metadata={"merge_policy": dataclasses_merge.Policy.PREPEND},
    )
    shared_memory: shared_memory_module.SharedMemory = (
        shared_memory_module.SharedMemory()
    )
    site_package_search_strategy: Optional[site_packages.SearchStrategy] = None
    site_roots: Optional[Sequence[str]] = None
    source_directories: Optional[Sequence[search_path_module.RawElement]] = field(
        default=None,
        metadata={"merge_policy": dataclasses_merge.Policy.RAISE_WHEN_OVERWRITTEN},
    )
    strict: Optional[bool] = None
    taint_models_path: Sequence[str] = field(
        default_factory=list,
        metadata={"merge_policy": dataclasses_merge.Policy.PREPEND},
    )
    targets: Optional[Sequence[str]] = field(
        default=None,
        metadata={"merge_policy": dataclasses_merge.Policy.RAISE_WHEN_OVERWRITTEN},
    )
    typeshed: Optional[str] = None
    unwatched_dependency: Optional[unwatched.UnwatchedDependency] = None
    use_buck2: Optional[bool] = None
    version_hash: Optional[str] = None

    @staticmethod
    def _get_depreacted_map() -> Dict[str, str]:
        return {"do_not_check": "ignore_all_errors"}

    @staticmethod
    def _get_extra_keys() -> Set[str]:
        return {
            "create_open_source_configuration",
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
            search_path_module.SimpleRawElement(element)
            for element in arguments.source_directories
        ] or None
        targets: Optional[List[str]] = (
            arguments.targets if len(arguments.targets) > 0 else None
        )
        python_version_string = arguments.python_version
        ide_features = (
            ide_features_module.IdeFeatures(
                hover_enabled=arguments.enable_hover,
                go_to_definition_enabled=arguments.enable_go_to_definition,
                find_symbols_enabled=arguments.enable_find_symbols,
            )
            if arguments.enable_hover is not None
            or arguments.enable_go_to_definition is not None
            else None
        )
        return PartialConfiguration(
            binary=arguments.binary,
            buck_mode=platform_aware.PlatformAware.from_json(
                arguments.buck_mode, "buck_mode"
            ),
            do_not_ignore_errors_in=arguments.do_not_ignore_errors_in,
            dot_pyre_directory=arguments.dot_pyre_directory,
            excludes=arguments.exclude,
            extensions=[],
            ide_features=ide_features,
            ignore_all_errors=[],
            isolation_prefix=arguments.isolation_prefix,
            logger=arguments.logger,
            number_of_workers=arguments.number_of_workers,
            oncall=None,
            other_critical_files=[],
            pysa_version_hash=None,
            python_version=(
                python_version_module.PythonVersion.from_string(python_version_string)
                if python_version_string is not None
                else None
            ),
            search_path=[
                search_path_module.SimpleRawElement(element)
                for element in arguments.search_path
            ],
            shared_memory=shared_memory_module.SharedMemory(
                heap_size=arguments.shared_memory_heap_size,
                dependency_table_power=arguments.shared_memory_dependency_table_power,
                hash_table_power=arguments.shared_memory_hash_table_power,
            ),
            site_package_search_strategy=None,
            site_roots=None,
            source_directories=source_directories,
            strict=strict,
            taint_models_path=[],
            targets=targets,
            typeshed=arguments.typeshed,
            unwatched_dependency=None,
            use_buck2=arguments.use_buck2,
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
            raise exceptions.InvalidConfiguration(
                f"Configuration field `{name}` is expected to have type "
                f"{expected_type} but got: `{result}`."
            )

        def ensure_optional_string_or_string_dict(
            json: Dict[str, Any], name: str
        ) -> Optional[Union[Dict[str, str], str]]:
            result = json.pop(name, None)
            if result is None:
                return None
            elif isinstance(result, str):
                return result
            elif isinstance(result, Dict):
                for value in result.values():
                    if not isinstance(value, str):
                        raise exceptions.InvalidConfiguration(
                            f"Configuration field `{name}` is expected to be a "
                            + f"dict of strings but got `{result}`."
                        )
                return result
            raise exceptions.InvalidConfiguration(
                f"Configuration field `{name}` is expected to be a string or a "
                + f"dict of strings but got `{result}`."
            )

        def ensure_optional_string_list(
            json: Dict[str, Any], name: str
        ) -> Optional[List[str]]:
            result = json.pop(name, None)
            if result is None:
                return None
            elif is_list_of_string(result):
                return result
            raise exceptions.InvalidConfiguration(
                f"Configuration field `{name}` is expected to be a list of "
                + f"strings but got `{result}`."
            )

        def ensure_string_list(
            json: Dict[str, Any], name: str, allow_single_string: bool = False
        ) -> List[str]:
            result = json.pop(name, [])
            if allow_single_string and isinstance(result, str):
                result = [result]
            if is_list_of_string(result):
                return result
            raise exceptions.InvalidConfiguration(
                f"Configuration field `{name}` is expected to be a list of "
                + f"strings but got `{result}`."
            )

        def ensure_list(json: Dict[str, object], name: str) -> List[object]:
            result = json.pop(name, [])
            if isinstance(result, list):
                return result
            raise exceptions.InvalidConfiguration(
                f"Configuration field `{name}` is expected to be a list but got `{result}`."
            )

        try:
            configuration_json = json.loads(contents)

            dot_pyre_directory = ensure_option_type(
                configuration_json, "dot_pyre_directory", str
            )

            search_path_json = configuration_json.pop("search_path", [])
            if isinstance(search_path_json, list):
                search_path = [
                    search_path_module.create_raw_element(json)
                    for json in search_path_json
                ]
            else:
                search_path = [search_path_module.create_raw_element(search_path_json)]

            python_version_json = configuration_json.pop("python_version", None)
            if python_version_json is None:
                python_version = None
            elif isinstance(python_version_json, str):
                python_version = python_version_module.PythonVersion.from_string(
                    python_version_json
                )
            else:
                raise exceptions.InvalidConfiguration(
                    "Expect python version to be a string but got"
                    + f"'{python_version_json}'"
                )

            shared_memory_json = ensure_option_type(
                configuration_json, "shared_memory", dict
            )
            if shared_memory_json is None:
                shared_memory = shared_memory_module.SharedMemory()
            else:
                shared_memory = shared_memory_module.SharedMemory(
                    heap_size=ensure_option_type(shared_memory_json, "heap_size", int),
                    dependency_table_power=ensure_option_type(
                        shared_memory_json, "dependency_table_power", int
                    ),
                    hash_table_power=ensure_option_type(
                        shared_memory_json, "hash_table_power", int
                    ),
                )
                for unrecognized_key in shared_memory_json:
                    LOG.warning(f"Unrecognized configuration item: {unrecognized_key}")

            source_directories_json = ensure_option_type(
                configuration_json, "source_directories", list
            )
            if isinstance(source_directories_json, list):
                source_directories = [
                    search_path_module.create_raw_element(json)
                    for json in source_directories_json
                ]
            else:
                source_directories = None

            site_package_search_strategy_json = ensure_option_type(
                configuration_json, "site_package_search_strategy", str
            )
            if site_package_search_strategy_json is None:
                site_package_search_strategy = None
            else:
                site_package_search_strategy = site_packages.SearchStrategy.from_string(
                    site_package_search_strategy_json
                )
                if site_package_search_strategy is None:
                    raise exceptions.InvalidConfiguration(
                        "Invalid value for `site_package_search_strategy`: "
                        f"{site_package_search_strategy_json}. Available choices: "
                        f"{[str(x) for x in site_packages.SearchStrategy]}."
                    )

            ide_features_json = ensure_option_type(
                configuration_json, "ide_features", dict
            )
            if ide_features_json is None:
                ide_features = None
            else:
                ide_features = ide_features_module.IdeFeatures.create_from_json(
                    ide_features_json
                )

            unwatched_dependency_json = ensure_option_type(
                configuration_json, "unwatched_dependency", dict
            )
            if unwatched_dependency_json is None:
                unwatched_dependency = None
            else:
                unwatched_dependency = unwatched.UnwatchedDependency.from_json(
                    unwatched_dependency_json
                )

            partial_configuration = PartialConfiguration(
                binary=ensure_option_type(configuration_json, "binary", str),
                buck_mode=platform_aware.PlatformAware.from_json(
                    ensure_optional_string_or_string_dict(
                        configuration_json, "buck_mode"
                    ),
                    "buck_mode",
                ),
                do_not_ignore_errors_in=ensure_string_list(
                    configuration_json, "do_not_ignore_errors_in"
                ),
                dot_pyre_directory=Path(dot_pyre_directory)
                if dot_pyre_directory is not None
                else None,
                excludes=ensure_string_list(
                    configuration_json, "exclude", allow_single_string=True
                ),
                extensions=[
                    # pyre-fixme[6]: we did not fully verify the type of `json`
                    ExtensionElement.from_json(json)
                    for json in ensure_list(configuration_json, "extensions")
                ],
                ide_features=ide_features,
                ignore_all_errors=ensure_string_list(
                    configuration_json, "ignore_all_errors"
                ),
                isolation_prefix=ensure_option_type(
                    configuration_json, "isolation_prefix", str
                ),
                logger=ensure_option_type(configuration_json, "logger", str),
                number_of_workers=ensure_option_type(
                    configuration_json, "workers", int
                ),
                oncall=ensure_option_type(configuration_json, "oncall", str),
                other_critical_files=ensure_string_list(
                    configuration_json, "critical_files"
                ),
                pysa_version_hash=ensure_option_type(
                    configuration_json, "pysa_version", str
                ),
                python_version=python_version,
                search_path=search_path,
                shared_memory=shared_memory,
                site_package_search_strategy=site_package_search_strategy,
                site_roots=ensure_optional_string_list(
                    configuration_json, "site_roots"
                ),
                source_directories=source_directories,
                strict=ensure_option_type(configuration_json, "strict", bool),
                taint_models_path=ensure_string_list(
                    configuration_json, "taint_models_path", allow_single_string=True
                ),
                targets=ensure_optional_string_list(configuration_json, "targets"),
                typeshed=ensure_option_type(configuration_json, "typeshed", str),
                unwatched_dependency=unwatched_dependency,
                use_buck2=ensure_option_type(configuration_json, "use_buck2", bool),
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
            raise exceptions.InvalidConfiguration(f"Invalid JSON file: {error}")

    @staticmethod
    def from_file(path: Path) -> "PartialConfiguration":
        try:
            contents = path.read_text(encoding="utf-8")
            return PartialConfiguration.from_string(contents)
        except OSError as error:
            raise exceptions.InvalidConfiguration(f"Error when reading {path}: {error}")

    def expand_relative_paths(self, root: str) -> "PartialConfiguration":
        binary = self.binary
        if binary is not None:
            binary = expand_relative_path(root, binary)
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
        unwatched_dependency = self.unwatched_dependency
        if unwatched_dependency is not None:
            files = unwatched_dependency.files
            unwatched_dependency = unwatched.UnwatchedDependency(
                change_indicator=unwatched_dependency.change_indicator,
                files=unwatched.UnwatchedFiles(
                    root=expand_relative_path(root, files.root),
                    checksum_path=files.checksum_path,
                ),
            )
        return PartialConfiguration(
            binary=binary,
            buck_mode=self.buck_mode,
            do_not_ignore_errors_in=[
                expand_relative_path(root, path)
                for path in self.do_not_ignore_errors_in
            ],
            dot_pyre_directory=self.dot_pyre_directory,
            excludes=self.excludes,
            extensions=self.extensions,
            ide_features=self.ide_features,
            ignore_all_errors=[
                expand_relative_path(root, path) for path in self.ignore_all_errors
            ],
            isolation_prefix=self.isolation_prefix,
            logger=logger,
            number_of_workers=self.number_of_workers,
            oncall=self.oncall,
            other_critical_files=[
                expand_relative_path(root, path) for path in self.other_critical_files
            ],
            pysa_version_hash=self.pysa_version_hash,
            python_version=self.python_version,
            search_path=[path.expand_relative_root(root) for path in self.search_path],
            shared_memory=self.shared_memory,
            site_package_search_strategy=self.site_package_search_strategy,
            site_roots=self.site_roots,
            source_directories=source_directories,
            strict=self.strict,
            taint_models_path=[
                expand_relative_path(root, path) for path in self.taint_models_path
            ],
            targets=self.targets,
            typeshed=typeshed,
            unwatched_dependency=unwatched_dependency,
            use_buck2=self.use_buck2,
            version_hash=self.version_hash,
        )


def merge_partial_configurations(
    base: PartialConfiguration, override: PartialConfiguration
) -> PartialConfiguration:
    try:
        # pyre-ignore[16]: Pyre does not understand `dataclass_merge`
        return PartialConfiguration.merge(base, override)
    except dataclasses_merge.DataclassMergeError as error:
        raise exceptions.InvalidConfiguration(str(error))


@dataclass(frozen=True)
class Configuration:
    project_root: str
    dot_pyre_directory: Path

    binary: Optional[str] = None
    buck_mode: Optional[platform_aware.PlatformAware[str]] = None
    do_not_ignore_errors_in: Sequence[str] = field(default_factory=list)
    excludes: Sequence[str] = field(default_factory=list)
    extensions: Sequence[ExtensionElement] = field(default_factory=list)
    ide_features: Optional[ide_features_module.IdeFeatures] = None
    ignore_all_errors: Sequence[str] = field(default_factory=list)
    isolation_prefix: Optional[str] = None
    logger: Optional[str] = None
    number_of_workers: Optional[int] = None
    oncall: Optional[str] = None
    other_critical_files: Sequence[str] = field(default_factory=list)
    pysa_version_hash: Optional[str] = None
    python_version: Optional[python_version_module.PythonVersion] = None
    relative_local_root: Optional[str] = None
    search_path: Sequence[search_path_module.RawElement] = field(default_factory=list)
    shared_memory: shared_memory_module.SharedMemory = (
        shared_memory_module.SharedMemory()
    )
    site_package_search_strategy: site_packages.SearchStrategy = (
        site_packages.SearchStrategy.NONE
    )
    site_roots: Optional[Sequence[str]] = None
    source_directories: Optional[Sequence[search_path_module.RawElement]] = None
    strict: bool = False
    taint_models_path: Sequence[str] = field(default_factory=list)
    targets: Optional[Sequence[str]] = None
    typeshed: Optional[str] = None
    unwatched_dependency: Optional[unwatched.UnwatchedDependency] = None
    use_buck2: bool = False
    version_hash: Optional[str] = None

    @staticmethod
    def from_partial_configuration(
        project_root: Path,
        relative_local_root: Optional[str],
        partial_configuration: PartialConfiguration,
    ) -> "Configuration":
        search_path = partial_configuration.search_path

        return Configuration(
            project_root=str(project_root),
            dot_pyre_directory=_get_optional_value(
                partial_configuration.dot_pyre_directory, project_root / LOG_DIRECTORY
            ),
            binary=partial_configuration.binary,
            buck_mode=partial_configuration.buck_mode,
            do_not_ignore_errors_in=partial_configuration.do_not_ignore_errors_in,
            excludes=partial_configuration.excludes,
            extensions=partial_configuration.extensions,
            ide_features=partial_configuration.ide_features,
            ignore_all_errors=partial_configuration.ignore_all_errors,
            isolation_prefix=partial_configuration.isolation_prefix,
            logger=partial_configuration.logger,
            number_of_workers=partial_configuration.number_of_workers,
            oncall=partial_configuration.oncall,
            other_critical_files=partial_configuration.other_critical_files,
            pysa_version_hash=partial_configuration.pysa_version_hash,
            python_version=partial_configuration.python_version,
            relative_local_root=relative_local_root,
            search_path=[
                path.expand_global_root(str(project_root)) for path in search_path
            ],
            shared_memory=partial_configuration.shared_memory,
            site_package_search_strategy=partial_configuration.site_package_search_strategy
            or site_packages.SearchStrategy.NONE,
            site_roots=partial_configuration.site_roots,
            source_directories=partial_configuration.source_directories,
            strict=_get_optional_value(partial_configuration.strict, default=False),
            taint_models_path=partial_configuration.taint_models_path,
            targets=partial_configuration.targets,
            typeshed=partial_configuration.typeshed,
            unwatched_dependency=partial_configuration.unwatched_dependency,
            use_buck2=_get_optional_value(
                partial_configuration.use_buck2, default=False
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

    def to_json(self) -> Dict[str, object]:
        """
        This method is for display purpose only. Do *NOT* expect this method
        to produce JSONs that can be de-serialized back into configurations.
        """
        binary = self.binary
        buck_mode = self.buck_mode
        isolation_prefix = self.isolation_prefix
        logger = self.logger
        number_of_workers = self.number_of_workers
        oncall = self.oncall
        pysa_version_hash = self.pysa_version_hash
        python_version = self.python_version
        relative_local_root = self.relative_local_root
        source_directories = self.source_directories
        site_package_search_strategy = self.site_package_search_strategy
        site_roots = self.site_roots
        targets = self.targets
        typeshed = self.typeshed
        unwatched_dependency = self.unwatched_dependency
        version_hash = self.version_hash
        return {
            "global_root": self.project_root,
            "dot_pyre_directory": str(self.dot_pyre_directory),
            **({"binary": binary} if binary is not None else {}),
            **({"buck_mode": buck_mode.to_json()} if buck_mode is not None else {}),
            "do_not_ignore_errors_in": list(self.do_not_ignore_errors_in),
            "excludes": list(self.excludes),
            "extensions": list(self.extensions),
            "ignore_all_errors": list(self.ignore_all_errors),
            **(
                {"isolation_prefix": isolation_prefix}
                if isolation_prefix is not None
                else {}
            ),
            **({"logger": logger} if logger is not None else {}),
            **({"oncall": oncall} if oncall is not None else {}),
            **({"workers": number_of_workers} if number_of_workers is not None else {}),
            "other_critical_files": list(self.other_critical_files),
            **(
                {"pysa_version_hash": pysa_version_hash}
                if pysa_version_hash is not None
                else {}
            ),
            **(
                {"python_version": python_version.to_string()}
                if python_version is not None
                else {}
            ),
            **(
                {"relative_local_root": relative_local_root}
                if relative_local_root is not None
                else {}
            ),
            "search_path": [str(path) for path in self.search_path],
            **(
                {"shared_memory": self.shared_memory.to_json()}
                if self.shared_memory != shared_memory_module.SharedMemory()
                else {}
            ),
            **(
                {"site_package_search_strategy": site_package_search_strategy}
                if site_package_search_strategy is not None
                else {}
            ),
            "site_roots": site_roots if site_roots is not None else [],
            **(
                {"source_directories": [str(path) for path in source_directories]}
                if source_directories is not None
                else {}
            ),
            "strict": self.strict,
            "taint_models_path": list(self.taint_models_path),
            **({"targets": list(targets)} if targets is not None else {}),
            **({"typeshed": typeshed} if typeshed is not None else {}),
            **(
                {"unwatched_dependency": unwatched_dependency.to_json()}
                if unwatched_dependency is not None
                else {}
            ),
            "use_buck2": self.use_buck2,
            **({"version_hash": version_hash} if version_hash is not None else {}),
        }

    def get_existent_unwatched_dependency(
        self,
    ) -> Optional[unwatched.UnwatchedDependency]:
        unwatched_dependency = self.unwatched_dependency
        if unwatched_dependency is None:
            return None
        unwatched_root = Path(unwatched_dependency.files.root)
        try:
            if not unwatched_root.is_dir():
                LOG.warning(
                    "Nonexistent directory passed in to `unwatched_dependency`: "
                    f"`{unwatched_root}`"
                )
                return None
            checksum_path = unwatched_root / unwatched_dependency.files.checksum_path
            if not checksum_path.is_file():
                LOG.warning(
                    "Nonexistent file passed in to `unwatched_dependency`: "
                    f"`{checksum_path}`"
                )
                return None
            return self.unwatched_dependency
        except PermissionError as error:
            LOG.warning(str(error))
            return None

    def get_site_roots(self) -> Sequence[str]:
        site_roots = self.site_roots
        if site_roots is not None:
            return site_roots
        return get_default_site_roots()

    def expand_and_get_existent_search_paths(
        self,
    ) -> List[search_path_module.Element]:
        site_roots = self.get_site_roots()
        existent_paths = search_path_module.process_raw_elements(
            self.search_path, site_roots
        )

        site_packages_paths = site_packages.search_for_paths(
            self.site_package_search_strategy, site_roots
        )

        typeshed_root = self.get_typeshed_respecting_override()
        if typeshed_root is None:
            return existent_paths + site_packages_paths

        typeshed_paths: List[search_path_module.Element] = [
            search_path_module.SimpleElement(str(element))
            for element in find_directories.find_typeshed_search_paths(
                Path(typeshed_root)
            )
        ]

        return existent_paths + site_packages_paths + typeshed_paths

    def expand_and_get_existent_source_directories(
        self,
    ) -> List[search_path_module.Element]:
        source_directories = self.source_directories
        if source_directories is not None:
            return search_path_module.process_raw_elements(
                source_directories, self.get_site_roots()
            )
        else:
            return []

    def get_existent_do_not_ignore_errors_in_paths(self) -> List[str]:
        """
        This is a separate method because we want to check for existing files
        at the time this is called, not when the configuration is
        constructed.
        """
        ignore_paths = [
            expand_global_root(path, global_root=self.project_root)
            for path in self.do_not_ignore_errors_in
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

        number_of_physical_cores = psutil.cpu_count(logical=False)
        if number_of_physical_cores is None:
            default_number_of_workers = 1
        else:
            default_number_of_workers = max(1, number_of_physical_cores - 1)

        LOG.info(
            "Could not determine the number of Pyre workers from configuration. "
            f"Auto-set the value to {default_number_of_workers}."
        )
        if default_number_of_workers <= 1:
            LOG.info(
                "Consider setting the `--sequential` flag instead when the number "
                "of parallel workers is not greater than 1."
            )
        return default_number_of_workers

    def is_hover_enabled(self) -> bool:
        if self.ide_features is None:
            return ide_features_module.IdeFeatures.DEFAULT_HOVER_ENABLED
        return self.ide_features.is_hover_enabled()

    def is_go_to_definition_enabled(self) -> bool:
        if self.ide_features is None:
            return ide_features_module.IdeFeatures.DEFAULT_GO_TO_DEFINITION_ENABLED
        return self.ide_features.is_go_to_definition_enabled()

    def is_find_symbols_enabled(self) -> bool:
        if self.ide_features is None:
            return ide_features_module.IdeFeatures.DEFAULT_FIND_SYMBOLS_ENABLED
        return self.ide_features.is_find_symbols_enabled()

    def get_valid_extension_suffixes(self) -> List[str]:
        vaild_extensions = []
        for extension in self.extensions:
            if not extension.suffix.startswith("."):
                LOG.warning(
                    "Filtering out extension which does not start with `.`: "
                    f"`{extension.suffix}`"
                )
            else:
                vaild_extensions.append(extension.command_line_argument())
        return vaild_extensions

    def get_python_version(self) -> python_version_module.PythonVersion:
        python_version = self.python_version
        if python_version is not None:
            return python_version
        else:
            version_info = sys.version_info
            return python_version_module.PythonVersion(
                major=version_info.major,
                minor=version_info.minor,
                micro=version_info.micro,
            )


def create_configuration(
    arguments: command_arguments.CommandArguments, base_directory: Path
) -> Configuration:
    local_root_argument = arguments.local_configuration
    search_base = (
        base_directory
        if local_root_argument is None
        else base_directory / local_root_argument
    )
    found_root = find_directories.find_global_and_local_root(search_base)

    # If the local root was explicitly specified but does not exist, return an
    # error instead of falling back to current directory.
    if local_root_argument is not None:
        if found_root is None:
            raise exceptions.InvalidConfiguration(
                "A local configuration path was explicitly specified, but no"
                + f" {CONFIGURATION_FILE} file was found in {search_base}"
                + " or its parents."
            )
        elif found_root.local_root is None:
            raise exceptions.InvalidConfiguration(
                "A local configuration path was explicitly specified, but no"
                + f" {LOCAL_CONFIGURATION_FILE} file was found in {search_base}"
                + " or its parents."
            )

    command_argument_configuration = PartialConfiguration.from_command_arguments(
        arguments
    ).expand_relative_paths(str(Path.cwd()))
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
            base=partial_configuration,
            override=command_argument_configuration,
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
            raise exceptions.InvalidConfiguration(error_message)
        current_directory = nesting_local_root.parent
