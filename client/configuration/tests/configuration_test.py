# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import json
import site
import sys
import tempfile
import unittest
from pathlib import Path

import testslide
import tomli as tomllib

from ... import command_arguments
from ...find_directories import TOML_CONFIGURATION_FILE
from ...tests.setup import (
    ensure_directories_exists,
    ensure_files_exist,
    switch_working_directory,
    write_configuration_file,
)
from ..configuration import (
    check_nested_local_configuration,
    Configuration,
    create_configuration,
    get_default_site_roots,
    PartialConfiguration,
)
from ..exceptions import InvalidConfiguration
from ..extension import Element as ExtensionElement
from ..platform_aware import PlatformAware
from ..python_version import PythonVersion
from ..search_path import (
    SimpleElement,
    SimpleRawElement,
    SitePackageRawElement,
    SubdirectoryRawElement,
)
from ..shared_memory import SharedMemory
from ..site_packages import SearchStrategy
from ..unwatched import UnwatchedDependency, UnwatchedFiles


class PartialConfigurationTest(unittest.TestCase):
    def test_create_from_command_arguments(self) -> None:
        configuration = PartialConfiguration.from_command_arguments(
            command_arguments.CommandArguments(
                local_configuration=None,
                logger="logger",
                targets=[],
                source_directories=[],
                search_path=["x", "y"],
                optional_search_path=["z"],
                binary="binary",
                buck_mode="opt",
                exclude=["excludes"],
                typeshed="typeshed",
                dot_pyre_directory=Path(".pyre"),
                python_version="3.6.7",
                system_platform="darwin",
                shared_memory_heap_size=42,
                number_of_workers=43,
                use_buck2=True,
                enable_unawaited_awaitable_analysis=True,
                include_suppressed_errors=True,
            )
        )
        self.assertEqual(configuration.binary, "binary")
        self.assertEqual(
            configuration.buck_mode, PlatformAware.from_json("opt", "buck_mode")
        )
        self.assertEqual(configuration.dot_pyre_directory, Path(".pyre"))
        self.assertListEqual(list(configuration.excludes), ["excludes"])
        self.assertEqual(configuration.logger, "logger")
        self.assertEqual(configuration.oncall, None)
        self.assertListEqual(
            list(configuration.search_path),
            [SimpleRawElement("x"), SimpleRawElement("y")],
        )
        self.assertListEqual(
            list(configuration.optional_search_path), [SimpleRawElement("z")]
        )
        self.assertIsNone(configuration.source_directories)
        self.assertEqual(configuration.strict, None)
        self.assertIsNone(configuration.targets)
        self.assertEqual(configuration.typeshed, "typeshed")
        self.assertEqual(configuration.unwatched_dependency, None)
        self.assertEqual(
            configuration.python_version, PythonVersion(major=3, minor=6, micro=7)
        )
        self.assertEqual(configuration.system_platform, "darwin")
        self.assertEqual(configuration.shared_memory, SharedMemory(heap_size=42))
        self.assertEqual(configuration.site_package_search_strategy, None)
        self.assertEqual(configuration.site_roots, None)
        self.assertEqual(configuration.number_of_workers, 43)
        self.assertEqual(configuration.max_number_of_workers, None)
        self.assertEqual(configuration.use_buck2, True)
        self.assertEqual(configuration.enable_readonly_analysis, None)
        self.assertEqual(configuration.enable_strict_override_check, None)
        self.assertEqual(configuration.enable_strict_any_check, None)
        self.assertEqual(configuration.enable_unawaited_awaitable_analysis, True)
        self.assertEqual(configuration.include_suppressed_errors, True)

    def test_create_from_string_success(self) -> None:
        self.assertEqual(
            PartialConfiguration.from_dict({"binary": "foo"}).binary,
            "foo",
        )

        for mode in [
            "foo",
            {"default": "foo"},
            {"linux": "foo"},
            {"default": "bar", "macos": "foo", "linux": "foo"},
        ]:
            buck_mode = PartialConfiguration.from_dict({"buck_mode": mode}).buck_mode
            expected_value = PlatformAware.from_json("foo", "buck_mode")
            self.assertIsNotNone(buck_mode)
            self.assertIsNotNone(expected_value)
            self.assertEqual(buck_mode.get(), expected_value.get())

        for null_mode in [{}, None]:
            self.assertIsNone(
                PartialConfiguration.from_dict({"buck_mode": null_mode}).buck_mode
            )

        self.assertEqual(
            PartialConfiguration.from_dict({"bxl_builder": "foo"}).bxl_builder,
            "foo",
        )

        self.assertListEqual(
            list(
                PartialConfiguration.from_dict(
                    {"only_check_paths": ["foo", "bar"]}
                ).only_check_paths
            ),
            ["foo", "bar"],
        )
        self.assertEqual(
            PartialConfiguration.from_dict(
                {"dot_pyre_directory": "foo"}
            ).dot_pyre_directory,
            Path("foo"),
        )
        self.assertListEqual(
            list(PartialConfiguration.from_dict({"exclude": "foo"}).excludes),
            ["foo"],
        )
        self.assertListEqual(
            list(PartialConfiguration.from_dict({"exclude": ["foo", "bar"]}).excludes),
            ["foo", "bar"],
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_dict(
                    {"extensions": [".foo", ".bar"]}
                ).extensions
            ),
            [ExtensionElement(".foo", False), ExtensionElement(".bar", False)],
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_dict(
                    {
                        "extensions": [
                            ".foo",
                            {
                                "suffix": ".bar",
                                "include_suffix_in_module_qualifier": True,
                            },
                            {
                                "suffix": ".baz",
                                "include_suffix_in_module_qualifier": False,
                            },
                        ]
                    }
                ).extensions
            ),
            [
                ExtensionElement(".foo", False),
                ExtensionElement(".bar", True),
                ExtensionElement(".baz", False),
            ],
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_dict(
                    {"ignore_all_errors": ["foo", "bar"]}
                ).ignore_all_errors
            ),
            ["foo", "bar"],
        )
        self.assertEqual(
            PartialConfiguration.from_dict({"logger": "foo"}).logger,
            "foo",
        )
        self.assertEqual(
            PartialConfiguration.from_dict({"oncall": "foo"}).oncall,
            "foo",
        )
        self.assertEqual(
            PartialConfiguration.from_dict({"workers": 42}).number_of_workers,
            42,
        )
        self.assertEqual(
            PartialConfiguration.from_dict({"max_workers": 42}).max_number_of_workers,
            42,
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_dict(
                    {"critical_files": ["foo", "bar"]}
                ).other_critical_files
            ),
            ["foo", "bar"],
        )
        self.assertListEqual(
            list(PartialConfiguration.from_dict({"search_path": "foo"}).search_path),
            [SimpleRawElement("foo")],
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_dict(
                    {"search_path": ["foo", {"root": "bar", "subdirectory": "baz"}]}
                ).search_path
            ),
            [
                SimpleRawElement("foo"),
                SubdirectoryRawElement("bar", "baz"),
            ],
        )
        self.assertIsNone(
            PartialConfiguration.from_dict({}).site_package_search_strategy
        )
        self.assertEqual(
            PartialConfiguration.from_dict(
                {"site_package_search_strategy": "pep561"}
            ).site_package_search_strategy,
            SearchStrategy.PEP561,
        )
        self.assertEqual(PartialConfiguration.from_dict({"strict": True}).strict, True)
        self.assertListEqual(
            list(
                PartialConfiguration.from_dict(
                    {"taint_models_path": "foo"}
                ).taint_models_path
            ),
            ["foo"],
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_dict(
                    {"taint_models_path": ["foo", "bar"]}
                ).taint_models_path
            ),
            ["foo", "bar"],
        )
        self.assertEqual(
            PartialConfiguration.from_dict({"typeshed": "foo"}).typeshed,
            "foo",
        )
        self.assertEqual(
            PartialConfiguration.from_dict({"version": "abc"}).version_hash,
            "abc",
        )
        self.assertEqual(
            PartialConfiguration.from_dict({"pysa_version": "abc"}).pysa_version_hash,
            "abc",
        )
        self.assertEqual(
            PartialConfiguration.from_dict({"python_version": "3"}).python_version,
            PythonVersion(major=3, minor=0, micro=0),
        )
        self.assertEqual(
            PartialConfiguration.from_dict({"python_version": "3.6"}).python_version,
            PythonVersion(major=3, minor=6, micro=0),
        )
        self.assertEqual(
            PartialConfiguration.from_dict({"python_version": "3.6.7"}).python_version,
            PythonVersion(major=3, minor=6, micro=7),
        )

        self.assertEqual(
            PartialConfiguration.from_dict(
                {"system_platform": "darwin"}
            ).system_platform,
            "darwin",
        )

        self.assertEqual(
            PartialConfiguration.from_dict(
                {"shared_memory": {"heap_size": 1}}
            ).shared_memory,
            SharedMemory(heap_size=1),
        )
        self.assertEqual(
            PartialConfiguration.from_dict(
                {"shared_memory": {"dependency_table_power": 2}}
            ).shared_memory,
            SharedMemory(dependency_table_power=2),
        )
        self.assertEqual(
            PartialConfiguration.from_dict(
                {"shared_memory": {"hash_table_power": 3}}
            ).shared_memory,
            SharedMemory(hash_table_power=3),
        )

        self.assertEqual(
            PartialConfiguration.from_dict({"use_buck2": False}).use_buck2,
            False,
        )

        self.assertIsNone(PartialConfiguration.from_dict({}).source_directories)
        source_directories = PartialConfiguration.from_dict(
            {"source_directories": ["foo", "bar"]}
        ).source_directories
        self.assertIsNotNone(source_directories)
        self.assertListEqual(
            list(source_directories),
            [SimpleRawElement("foo"), SimpleRawElement("bar")],
        )

        self.assertIsNone(PartialConfiguration.from_dict({}).site_roots)
        site_roots = PartialConfiguration.from_dict(
            {"site_roots": ["foo", "bar"]}
        ).site_roots
        self.assertIsNotNone(site_roots)
        self.assertListEqual(
            list(site_roots),
            ["foo", "bar"],
        )

        source_directories = PartialConfiguration.from_dict(
            {
                "source_directories": [
                    "foo",
                    {"root": "bar", "subdirectory": "baz"},
                ]
            }
        ).source_directories
        self.assertIsNotNone(source_directories)
        self.assertListEqual(
            list(source_directories),
            [
                SimpleRawElement("foo"),
                SubdirectoryRawElement("bar", "baz"),
            ],
        )
        source_directories = PartialConfiguration.from_dict(
            {
                "source_directories": [
                    "foo",
                    {"import_root": "bar", "source": "baz"},
                ]
            }
        ).source_directories
        self.assertIsNotNone(source_directories)
        self.assertListEqual(
            list(source_directories),
            [
                SimpleRawElement("foo"),
                SubdirectoryRawElement("bar", "baz"),
            ],
        )

        self.assertIsNone(PartialConfiguration.from_dict({}).targets)
        targets = PartialConfiguration.from_dict(
            {"targets": ["//foo", "//bar"]}
        ).targets
        self.assertIsNotNone(targets)
        self.assertListEqual(list(targets), ["//foo", "//bar"])

        unwatched_dependency = PartialConfiguration.from_dict(
            {
                "unwatched_dependency": {
                    "change_indicator": "foo",
                    "files": {"root": "bar", "checksum_path": "baz"},
                }
            }
        ).unwatched_dependency
        self.assertIsNotNone(unwatched_dependency)
        self.assertEqual(
            unwatched_dependency,
            UnwatchedDependency(
                change_indicator="foo",
                files=UnwatchedFiles(root="bar", checksum_path="baz"),
            ),
        )
        self.assertEqual(
            PartialConfiguration.from_dict(
                {"enable_readonly_analysis": True}
            ).enable_readonly_analysis,
            True,
        )
        self.assertEqual(
            PartialConfiguration.from_dict({}).enable_readonly_analysis,
            None,
        )
        self.assertEqual(
            PartialConfiguration.from_dict(
                {"enable_strict_override_check": True}
            ).enable_strict_override_check,
            True,
        )
        self.assertEqual(
            PartialConfiguration.from_dict({}).enable_strict_override_check,
            None,
        )
        self.assertEqual(
            PartialConfiguration.from_dict(
                {"enable_strict_any_check": True}
            ).enable_strict_any_check,
            True,
        )
        self.assertEqual(
            PartialConfiguration.from_dict({}).enable_strict_any_check, None
        )
        self.assertEqual(
            PartialConfiguration.from_dict(
                {"enable_unawaited_awaitable_analysis": True}
            ).enable_unawaited_awaitable_analysis,
            True,
        )
        self.assertEqual(
            PartialConfiguration.from_dict({}).enable_unawaited_awaitable_analysis,
            None,
        )
        self.assertEqual(
            PartialConfiguration.from_dict(
                {"include_suppressed_errors": True}
            ).include_suppressed_errors,
            True,
        )
        self.assertEqual(
            PartialConfiguration.from_dict({}).include_suppressed_errors,
            None,
        )

    def test_create_from_string_failure(self) -> None:
        def assert_raises(content: str) -> None:
            with self.assertRaises(InvalidConfiguration):
                PartialConfiguration.from_dict(json.loads(content))

        assert_raises(json.dumps({"binary": True}))
        assert_raises(json.dumps({"buck_mode": {"default": 5}}))
        assert_raises(json.dumps({"buck_mode": {"bad-platform": "mode"}}))
        assert_raises(
            json.dumps(
                {
                    "buck_mode": {
                        "win": "valid",
                        "bad": "valid-also",
                    }
                }
            )
        )
        assert_raises(json.dumps({"bxl_builder": []}))
        assert_raises(json.dumps({"only_check_paths": "abc"}))
        assert_raises(json.dumps({"dot_pyre_directory": {}}))
        assert_raises(json.dumps({"enable_readonly_analysis": 42}))
        assert_raises(json.dumps({"enable_strict_override_check": 42}))
        assert_raises(json.dumps({"enable_strict_any_check": 42}))
        assert_raises(json.dumps({"enable_unawaited_awaitable_analysis": 42}))
        assert_raises(json.dumps({"exclude": 42}))
        assert_raises(json.dumps({"extensions": 42}))
        assert_raises(json.dumps({"ignore_all_errors": [1, 2, 3]}))
        assert_raises(json.dumps({"include_suppressed_errors": 42}))
        assert_raises(json.dumps({"logger": []}))
        assert_raises(json.dumps({"oncall": []}))
        assert_raises(json.dumps({"workers": "abc"}))
        assert_raises(json.dumps({"critical_files": "abc"}))
        assert_raises(json.dumps({"source_directories": "abc"}))
        assert_raises(json.dumps({"strict": 42}))
        assert_raises(json.dumps({"taint_models_path": True}))
        assert_raises(json.dumps({"taint_models_path": ["foo", 42]}))
        assert_raises(json.dumps({"targets": "abc"}))
        assert_raises(json.dumps({"typeshed": ["abc"]}))
        assert_raises(json.dumps({"version": 123}))
        assert_raises(json.dumps({"pysa_version": 123}))
        assert_raises(json.dumps({"python_version": "abc"}))
        assert_raises(json.dumps({"python_version": 42}))
        assert_raises(json.dumps({"system_platform": 42}))
        assert_raises(json.dumps({"shared_memory": "abc"}))
        assert_raises(json.dumps({"shared_memory": {"heap_size": "abc"}}))
        assert_raises(json.dumps({"site_package_search_strategy": False}))
        assert_raises(json.dumps({"site_roots": 42}))
        assert_raises(json.dumps({"unwatched_dependency": {"change_indicator": "abc"}}))
        assert_raises(json.dumps({"use_buck2": {}}))

    def test_expand_relative_paths(self) -> None:
        self.assertEqual(
            PartialConfiguration(binary="foo").expand_relative_paths("bar").binary,
            "bar/foo",
        )
        self.assertEqual(
            PartialConfiguration(binary="~/foo").expand_relative_paths("bar").binary,
            str(Path.home() / "foo"),
        )
        self.assertEqual(
            PartialConfiguration(only_check_paths=["foo", "bar"])
            .expand_relative_paths("baz")
            .only_check_paths,
            ["baz/foo", "baz/bar"],
        )
        self.assertEqual(
            PartialConfiguration(ignore_all_errors=["foo", "bar"])
            .expand_relative_paths("baz")
            .ignore_all_errors,
            ["baz/foo", "baz/bar"],
        )
        self.assertEqual(
            PartialConfiguration(logger="foo").expand_relative_paths("bar").logger,
            "bar/foo",
        )
        self.assertEqual(
            PartialConfiguration(other_critical_files=["foo", "bar"])
            .expand_relative_paths("baz")
            .other_critical_files,
            ["baz/foo", "baz/bar"],
        )
        configuration = PartialConfiguration(
            search_path=[
                SimpleRawElement("foo"),
                SubdirectoryRawElement("bar", "baz"),
                SitePackageRawElement("package"),
            ],
            optional_search_path=[
                SimpleRawElement("optional"),
            ],
        ).expand_relative_paths("root")
        self.assertEqual(
            configuration.search_path,
            [
                SimpleRawElement("root/foo"),
                SubdirectoryRawElement("root/bar", "baz"),
                SitePackageRawElement("package"),
            ],
        )
        self.assertEqual(
            configuration.optional_search_path, [SimpleRawElement("root/optional")]
        )
        self.assertEqual(
            PartialConfiguration(
                source_directories=[
                    SimpleRawElement("foo"),
                    SimpleRawElement("bar"),
                ]
            )
            .expand_relative_paths("baz")
            .source_directories,
            [
                SimpleRawElement("baz/foo"),
                SimpleRawElement("baz/bar"),
            ],
        )
        self.assertEqual(
            PartialConfiguration(taint_models_path=["foo", "bar"])
            .expand_relative_paths("baz")
            .taint_models_path,
            ["baz/foo", "baz/bar"],
        )
        self.assertEqual(
            PartialConfiguration(typeshed="foo").expand_relative_paths("bar").typeshed,
            "bar/foo",
        )
        self.assertEqual(
            PartialConfiguration(enable_readonly_analysis=True)
            .expand_relative_paths("bar")
            .enable_readonly_analysis,
            True,
        )
        self.assertEqual(
            PartialConfiguration(enable_strict_override_check=True)
            .expand_relative_paths("bar")
            .enable_strict_override_check,
            True,
        )
        self.assertEqual(
            PartialConfiguration(enable_strict_any_check=True)
            .expand_relative_paths("bar")
            .enable_strict_any_check,
            True,
        )
        self.assertEqual(
            PartialConfiguration(enable_unawaited_awaitable_analysis=True)
            .expand_relative_paths("bar")
            .enable_unawaited_awaitable_analysis,
            True,
        )

        def assert_expanded_unwatched_root(
            original: str, root: str, expected: str
        ) -> None:
            actual = (
                PartialConfiguration(
                    unwatched_dependency=UnwatchedDependency(
                        change_indicator="indicator",
                        files=UnwatchedFiles(root=original, checksum_path="checksum"),
                    )
                )
                .expand_relative_paths(root)
                .unwatched_dependency
            )
            self.assertIsNotNone(actual)
            self.assertEqual(actual.files.root, expected)

        assert_expanded_unwatched_root(
            original="foo",
            root="bar",
            expected="bar/foo",
        )


class ConfigurationTest(testslide.TestCase):
    def test_from_partial_configuration(self) -> None:
        configuration = Configuration.from_partial_configuration(
            global_root=Path("root"),
            relative_local_root="local",
            partial_configuration=PartialConfiguration(
                binary="binary",
                buck_mode=PlatformAware.from_json("opt", "buck_mode"),
                bxl_builder="//some/bxl:build",
                only_check_paths=["//foo"],
                dot_pyre_directory=None,
                enable_readonly_analysis=True,
                enable_strict_override_check=True,
                enable_strict_any_check=True,
                enable_unawaited_awaitable_analysis=True,
                excludes=["exclude"],
                extensions=[ExtensionElement(".ext", False)],
                ignore_all_errors=["bar"],
                include_suppressed_errors=True,
                logger="logger",
                number_of_workers=3,
                max_number_of_workers=10,
                oncall="oncall",
                other_critical_files=["critical"],
                python_version=PythonVersion(major=3, minor=6, micro=7),
                system_platform="darwin",
                search_path=[SimpleRawElement("search_path")],
                optional_search_path=[SimpleRawElement("optional_search_path")],
                shared_memory=SharedMemory(heap_size=1024),
                site_package_search_strategy=SearchStrategy.NONE,
                site_roots=["site_root"],
                source_directories=None,
                strict=None,
                taint_models_path=["taint"],
                targets=None,
                typeshed="typeshed",
                unwatched_dependency=None,
                use_buck2=None,
                version_hash="abc",
            ),
        )
        self.assertEqual(configuration.global_root, Path("root"))
        self.assertEqual(configuration.relative_local_root, "local")
        self.assertEqual(configuration.binary, "binary")
        self.assertIsNotNone(configuration.buck_mode)
        self.assertEqual(configuration.buck_mode.get(), "opt")
        self.assertEqual(configuration.bxl_builder, "//some/bxl:build")
        self.assertListEqual(list(configuration.only_check_paths), ["root/foo"])
        self.assertEqual(configuration.dot_pyre_directory, None)
        self.assertEqual(configuration.enable_readonly_analysis, True)
        self.assertEqual(configuration.enable_strict_override_check, True)
        self.assertEqual(configuration.enable_strict_any_check, True)
        self.assertEqual(configuration.enable_unawaited_awaitable_analysis, True)
        self.assertListEqual(list(configuration.excludes), ["exclude"])
        self.assertEqual(configuration.extensions, [ExtensionElement(".ext", False)])
        self.assertListEqual(list(configuration.ignore_all_errors), ["bar"])
        self.assertEqual(configuration.include_suppressed_errors, True)
        self.assertEqual(configuration.logger, "logger")
        self.assertEqual(configuration.number_of_workers, 3)
        self.assertEqual(configuration.max_number_of_workers, 10)
        self.assertEqual(configuration.oncall, "oncall")
        self.assertListEqual(list(configuration.other_critical_files), ["critical"])
        self.assertListEqual(
            list(configuration.search_path), [SimpleRawElement("search_path")]
        )
        self.assertListEqual(
            list(configuration.optional_search_path),
            [SimpleRawElement("optional_search_path")],
        )
        self.assertEqual(
            configuration.python_version, PythonVersion(major=3, minor=6, micro=7)
        )
        self.assertEqual(configuration.system_platform, "darwin")
        self.assertEqual(configuration.source_directories, None)
        self.assertEqual(configuration.shared_memory, SharedMemory(heap_size=1024))
        self.assertEqual(
            configuration.site_package_search_strategy, SearchStrategy.NONE
        )
        self.assertEqual(configuration.site_roots, ["site_root"])
        self.assertEqual(configuration.strict, False)
        self.assertEqual(configuration.taint_models_path, ["taint"])
        self.assertEqual(configuration.targets, None)
        self.assertEqual(configuration.typeshed, "typeshed")
        self.assertEqual(configuration.unwatched_dependency, None)
        self.assertEqual(configuration.use_buck2, True)
        self.assertEqual(configuration.version_hash, "abc")

    def test_get_default_site_roots(self) -> None:
        global_site_package = "/venv/lib/pythonX/site-packages"
        user_site_package = "/user/lib/pythonX/site-packages"
        self.mock_callable(site, "getsitepackages").to_return_value(
            [global_site_package]
        ).and_assert_called_once()
        self.mock_callable(site, "getusersitepackages").to_return_value(
            user_site_package
        ).and_assert_called_once()
        self.assertListEqual(
            get_default_site_roots(), [user_site_package, global_site_package]
        )

    def test_existent_unwatched_dependency(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            ensure_files_exist(root_path, ["a/b"])
            self.assertIsNotNone(
                Configuration(
                    global_root=root_path,
                    dot_pyre_directory=Path(".pyre"),
                    unwatched_dependency=UnwatchedDependency(
                        change_indicator="indicator",
                        files=UnwatchedFiles(
                            root=str(root_path / "a"), checksum_path="b"
                        ),
                    ),
                ).get_existent_unwatched_dependency()
            )
            self.assertIsNone(
                Configuration(
                    global_root=root_path,
                    dot_pyre_directory=Path(".pyre"),
                    unwatched_dependency=UnwatchedDependency(
                        change_indicator="indicator",
                        files=UnwatchedFiles(
                            root=str(root_path / "a"), checksum_path="c"
                        ),
                    ),
                ).get_existent_unwatched_dependency()
            )
            self.assertIsNone(
                Configuration(
                    global_root=root_path,
                    dot_pyre_directory=Path(".pyre"),
                    unwatched_dependency=UnwatchedDependency(
                        change_indicator="indicator",
                        files=UnwatchedFiles(
                            root=str(root_path / "c"), checksum_path="b"
                        ),
                    ),
                ).get_existent_unwatched_dependency()
            )

    def test_get_number_of_workers(self) -> None:
        self.assertEqual(
            Configuration(
                global_root=Path("irrelevant"),
                dot_pyre_directory=Path(".pyre"),
                number_of_workers=42,
            ).get_number_of_workers(),
            42,
        )
        # Whatever the default number is, it should be positive
        self.assertGreater(
            Configuration(
                global_root=Path("irrelevant"),
                dot_pyre_directory=Path(".pyre"),
                number_of_workers=None,
            ).get_number_of_workers(),
            0,
        )

        # `max_number_of_workers` only takes effect when `number_of_workers` is None
        self.assertEqual(
            Configuration(
                global_root=Path("irrelevant"),
                dot_pyre_directory=Path(".pyre"),
                number_of_workers=None,
                max_number_of_workers=1,
            ).get_number_of_workers(),
            1,
        )
        self.assertEqual(
            Configuration(
                global_root=Path("irrelevant"),
                dot_pyre_directory=Path(".pyre"),
                number_of_workers=42,
                max_number_of_workers=10,
            ).get_number_of_workers(),
            42,
        )

    def test_get_python_versions(self) -> None:
        self.assertEqual(
            Configuration(
                global_root=Path("irrelevant"),
                dot_pyre_directory=Path(".pyre"),
                python_version=PythonVersion(major=3, minor=6, micro=7),
            ).get_python_version(),
            PythonVersion(major=3, minor=6, micro=7),
        )
        self.assertEqual(
            Configuration(
                global_root=Path("irrelevant"),
                dot_pyre_directory=Path(".pyre"),
                python_version=None,
            ).get_python_version(),
            PythonVersion(
                major=sys.version_info.major,
                minor=sys.version_info.minor,
                micro=sys.version_info.micro,
            ),
        )

    def test_get_valid_extension_suffixes(self) -> None:
        self.assertListEqual(
            Configuration(
                global_root=Path("irrelevant"),
                dot_pyre_directory=Path(".pyre"),
                extensions=[],
            ).get_valid_extension_suffixes(),
            [],
        )
        self.assertListEqual(
            Configuration(
                global_root=Path("irrelevant"),
                dot_pyre_directory=Path(".pyre"),
                extensions=[
                    ExtensionElement(".foo", False),
                    ExtensionElement(".bar", False),
                ],
            ).get_valid_extension_suffixes(),
            [".foo", ".bar"],
        )
        self.assertListEqual(
            Configuration(
                global_root=Path("irrelevant"),
                dot_pyre_directory=Path(".pyre"),
                extensions=[
                    ExtensionElement("foo", False),
                    ExtensionElement(".bar", False),
                    ExtensionElement("baz", False),
                ],
            ).get_valid_extension_suffixes(),
            [".bar"],
        )

    def test_create_from_command_arguments_only(self) -> None:
        # We assume there does not exist a `.pyre_configuration` file that
        # covers this temporary directory.
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            with switch_working_directory(root_path):
                configuration = create_configuration(
                    command_arguments.CommandArguments(
                        source_directories=["."], dot_pyre_directory=None
                    ),
                    base_directory=Path(root),
                )
                self.assertEqual(configuration.global_root, root_path)
                self.assertEqual(configuration.relative_local_root, None)
                self.assertEqual(configuration.dot_pyre_directory, None)
                self.assertListEqual(
                    list(configuration.source_directories or []),
                    [SimpleRawElement(str(root_path))],
                )

    def test_create_from_global_configuration(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            write_configuration_file(root_path, {"strict": False, "logger": "foo"})

            with switch_working_directory(root_path):
                configuration = create_configuration(
                    command_arguments.CommandArguments(
                        strict=True,  # override configuration file
                        no_logger=True,  # override configuration file
                        source_directories=["."],
                        dot_pyre_directory=Path(".pyre"),
                    ),
                    base_directory=Path(root),
                )
                self.assertEqual(configuration.global_root, root_path)
                self.assertEqual(configuration.relative_local_root, None)
                self.assertEqual(configuration.dot_pyre_directory, Path(".pyre"))
                self.assertEqual(configuration.strict, True)
                self.assertEqual(configuration.logger, None)
                self.assertListEqual(
                    list(configuration.source_directories or []),
                    [SimpleRawElement(str(root_path))],
                )

    def test_create_from_local_configuration(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            ensure_directories_exists(root_path, ["foo", "bar", "baz"])
            write_configuration_file(
                root_path,
                {
                    "strict": False,
                    "search_path": ["foo"],
                    "optional_search_path": ["optional"],
                },
            )
            write_configuration_file(
                root_path,
                {"strict": True, "search_path": ["//bar", "baz"]},
                relative="local",
            )

            with switch_working_directory(root_path):
                configuration = create_configuration(
                    command_arguments.CommandArguments(
                        local_configuration="local",
                        source_directories=["."],
                        dot_pyre_directory=Path(".pyre"),
                    ),
                    base_directory=Path(root),
                )
                self.assertEqual(configuration.global_root, root_path)
                self.assertEqual(configuration.relative_local_root, "local")
                self.assertEqual(configuration.dot_pyre_directory, Path(".pyre"))
                self.assertEqual(configuration.strict, True)
                self.assertListEqual(
                    list(configuration.source_directories or []),
                    [SimpleRawElement(str(root_path))],
                )
                self.assertListEqual(
                    list(configuration.search_path),
                    [
                        SimpleRawElement(str(root_path / "bar")),
                        SimpleRawElement(str(root_path / "local/baz")),
                        SimpleRawElement(str(root_path / "foo")),
                    ],
                )
                self.assertListEqual(
                    list(configuration.optional_search_path),
                    [
                        SimpleRawElement(str(root_path / "optional")),
                    ],
                )

    def test_check_nested_local_configuration_no_nesting(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            write_configuration_file(root_path, {})
            write_configuration_file(root_path, {}, relative="local")

            try:
                check_nested_local_configuration(
                    Configuration(
                        global_root=root_path,
                        dot_pyre_directory=Path(".pyre"),
                        relative_local_root="local",
                    )
                )
            except InvalidConfiguration:
                self.fail("Nested local configuration check fails unexpectedly!")

    def test_check_nested_local_configuration_not_excluded(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            write_configuration_file(root_path, {})
            write_configuration_file(root_path, {}, relative="nest")
            write_configuration_file(root_path, {}, relative="nest/local")

            with self.assertRaises(InvalidConfiguration):
                check_nested_local_configuration(
                    Configuration(
                        global_root=root_path,
                        dot_pyre_directory=Path(".pyre"),
                        relative_local_root="nest/local",
                    )
                )

    def test_check_nested_local_configuration_excluded(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            write_configuration_file(root_path, {})
            write_configuration_file(
                root_path,
                {"ignore_all_errors": [str(root_path / "nest/local")]},
                relative="nest",
            )
            write_configuration_file(root_path, {}, relative="nest/local")

            try:
                check_nested_local_configuration(
                    Configuration(
                        global_root=root_path,
                        dot_pyre_directory=Path(".pyre"),
                        relative_local_root="nest/local",
                    )
                )
            except InvalidConfiguration:
                self.fail("Nested local configuration check fails unexpectedly!")

    def test_check_nested_local_configuration_excluded_parent(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            write_configuration_file(root_path, {})
            write_configuration_file(
                root_path,
                {"ignore_all_errors": [str(root_path / "nest")]},
                relative="nest",
            )
            write_configuration_file(root_path, {}, relative="nest/local")

            try:
                check_nested_local_configuration(
                    Configuration(
                        global_root=root_path,
                        dot_pyre_directory=Path(".pyre"),
                        relative_local_root="nest/local",
                    )
                )
            except InvalidConfiguration:
                self.fail("Nested local configuration check fails unexpectedly!")

    def test_check_nested_local_configuration_not_all_nesting_excluded(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            write_configuration_file(root_path, {})
            write_configuration_file(root_path, {}, relative="nest0")
            write_configuration_file(
                root_path,
                {"ignore_all_errors": [str(root_path / "nest0/nest1/local")]},
                relative="nest0/nest1",
            )
            write_configuration_file(root_path, {}, relative="nest0/nest1/local")

            with self.assertRaises(InvalidConfiguration):
                check_nested_local_configuration(
                    Configuration(
                        global_root=root_path,
                        dot_pyre_directory=Path(".pyre"),
                        relative_local_root="nest0/nest1/local",
                    )
                )

    def test_check_nested_local_configuration_all_nesting_excluded(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            write_configuration_file(root_path, {})
            write_configuration_file(
                root_path,
                {"ignore_all_errors": [str(root_path / "nest0/nest1/local")]},
                relative="nest0",
            )
            write_configuration_file(
                root_path,
                {"ignore_all_errors": [str(root_path / "nest0/nest1/local")]},
                relative="nest0/nest1",
            )
            write_configuration_file(root_path, {}, relative="nest0/nest1/local")

            try:
                check_nested_local_configuration(
                    Configuration(
                        global_root=root_path,
                        dot_pyre_directory=Path(".pyre"),
                        relative_local_root="nest0/nest1/local",
                    )
                )
            except InvalidConfiguration:
                self.fail("Nested local configuration check fails unexpectedly!")

    def test_check_nested_local_configuration_expand_global_root(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            write_configuration_file(root_path, {})
            write_configuration_file(
                root_path,
                {"ignore_all_errors": ["//nest0/nest1/local"]},
                relative="nest0",
            )
            write_configuration_file(
                root_path,
                {"ignore_all_errors": [str(root_path / "nest0/**")]},
                relative="nest0/nest1",
            )
            write_configuration_file(root_path, {}, relative="nest0/nest1/local")

            check_nested_local_configuration(
                Configuration(
                    global_root=root_path,
                    dot_pyre_directory=Path(".pyre"),
                    relative_local_root="nest0/nest1/local",
                )
            )

    def test_check_nested_local_configuration_expand_relative_root(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            write_configuration_file(root_path, {})
            write_configuration_file(
                root_path, {"ignore_all_errors": ["nest1/local"]}, relative="nest0"
            )
            write_configuration_file(
                root_path, {"ignore_all_errors": ["*"]}, relative="nest0/nest1"
            )
            write_configuration_file(root_path, {}, relative="nest0/nest1/local")

            check_nested_local_configuration(
                Configuration(
                    global_root=root_path,
                    dot_pyre_directory=Path(".pyre"),
                    relative_local_root="nest0/nest1/local",
                )
            )

    def test_optional_search_path_comes_last(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            ensure_directories_exists(root_path, ["a1", "a2"])
            source_directories = Configuration(
                global_root=Path("irrelevant"),
                dot_pyre_directory=Path(".pyre"),
                search_path=[
                    SimpleRawElement(str(root_path / "a1")),
                ],
                optional_search_path=[
                    SimpleRawElement(str(root_path / "a2")),
                ],
            ).expand_and_get_existent_search_paths()
            self.assertIsNotNone(source_directories)
            self.assertListEqual(
                list(source_directories),
                [
                    SimpleElement(str(root_path / "a1")),
                    SimpleElement(str(root_path / "a2")),
                ],
            )

    def test_source_directories_glob(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            ensure_directories_exists(root_path, ["a1", "a2", "b", "c"])
            source_directories = Configuration(
                global_root=Path("irrelevant"),
                dot_pyre_directory=Path(".pyre"),
                source_directories=[
                    SimpleRawElement(str(root_path / "a*")),
                    SimpleRawElement(str(root_path / "b")),
                ],
            ).expand_and_get_existent_source_directories()
            self.assertIsNotNone(source_directories)
            self.assertListEqual(
                list(source_directories),
                [
                    SimpleElement(str(root_path / "a1")),
                    SimpleElement(str(root_path / "a2")),
                    SimpleElement(str(root_path / "b")),
                ],
            )

    def test_pyproject_dot_toml_configuration_from_string_and_file(self) -> None:
        pyproject_config: str = """[tool.pyre]
exclude = ["sample/exclude1", "sample/exclude2"]
strict = true
"""
        configuration_dict = tomllib.loads(pyproject_config)["tool"]["pyre"]
        config = PartialConfiguration.from_dict(configuration_dict)
        self.assertEqual(config.excludes, ["sample/exclude1", "sample/exclude2"])
        self.assertEqual(config.strict, True)
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            with open(
                root_path / TOML_CONFIGURATION_FILE, "w", encoding="UTF-8"
            ) as configuration_file:
                configuration_file.write(pyproject_config)
            config = PartialConfiguration.from_file(root_path / TOML_CONFIGURATION_FILE)
            self.assertEqual(config.excludes, ["sample/exclude1", "sample/exclude2"])
            self.assertEqual(config.strict, True)

    def test_pyproject_dot_toml_with_no_json_configuration_file(self) -> None:
        pyproject_config: str = """[tool.pyre]
exclude = ["another/sample/exclude1", "another/sample/exclude2"]
strict = false
        """
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            with open(
                root_path / TOML_CONFIGURATION_FILE, "w", encoding="UTF-8"
            ) as configuration_file:
                configuration_file.write(pyproject_config)
            config = create_configuration(
                command_arguments.CommandArguments(), root_path
            )
            self.assertEqual(
                config.excludes, ["another/sample/exclude1", "another/sample/exclude2"]
            )
            self.assertEqual(config.strict, False)
