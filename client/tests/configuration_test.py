# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import dataclasses
import hashlib
import json
import shutil
import site
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from typing import Optional

import testslide

from .. import (
    command_arguments,
    find_directories,
    configuration as configuration_module,
)
from ..configuration import (
    PythonVersion,
    IdeFeatures,
    InvalidPythonVersion,
    SharedMemory,
    Configuration,
    ExtensionElement,
    InvalidConfiguration,
    PartialConfiguration,
    SimpleSearchPathElement,
    SitePackageSearchPathElement,
    SubdirectorySearchPathElement,
    check_nested_local_configuration,
    create_configuration,
    create_search_paths,
    merge_partial_configurations,
    get_site_roots,
)
from ..find_directories import BINARY_NAME
from .setup import (
    ensure_directories_exists,
    switch_environment,
    switch_working_directory,
    write_configuration_file,
)


class PythonVersionTest(unittest.TestCase):
    def test_from_string(self) -> None:
        def assert_parsed(input: str, expected: PythonVersion) -> None:
            self.assertEqual(PythonVersion.from_string(input), expected)

        def assert_not_parsed(input: str) -> None:
            with self.assertRaises(InvalidPythonVersion):
                PythonVersion.from_string(input)

        assert_not_parsed("")
        assert_not_parsed("derp")
        assert_not_parsed("123abc")
        assert_not_parsed("1.a")
        assert_not_parsed("1.2.a")
        assert_not_parsed(".1")
        assert_not_parsed("1.2.3.4")

        assert_parsed("3", PythonVersion(major=3))
        assert_parsed("3.6", PythonVersion(major=3, minor=6))
        assert_parsed("3.6.7", PythonVersion(major=3, minor=6, micro=7))


class PartialConfigurationTest(unittest.TestCase):
    def test_create_from_command_arguments(self) -> None:
        configuration = PartialConfiguration.from_command_arguments(
            command_arguments.CommandArguments(
                local_configuration=None,
                logger="logger",
                targets=[],
                use_buck_builder=False,
                use_buck_source_database=True,
                use_command_v2=True,
                source_directories=[],
                search_path=["x", "y"],
                binary="binary",
                buck_builder_binary="buck_builder_binary",
                buck_mode="opt",
                exclude=["excludes"],
                typeshed="typeshed",
                dot_pyre_directory=Path(".pyre"),
                python_version="3.6.7",
                shared_memory_heap_size=42,
                number_of_workers=43,
            )
        )
        self.assertEqual(configuration.binary, "binary")
        self.assertEqual(configuration.buck_builder_binary, "buck_builder_binary")
        self.assertEqual(configuration.buck_mode, "opt")
        self.assertEqual(configuration.dot_pyre_directory, Path(".pyre"))
        self.assertListEqual(list(configuration.excludes), ["excludes"])
        self.assertEqual(configuration.logger, "logger")
        self.assertEqual(configuration.oncall, None)
        self.assertListEqual(
            list(configuration.search_path),
            [SimpleSearchPathElement("x"), SimpleSearchPathElement("y")],
        )
        self.assertIsNone(configuration.source_directories)
        self.assertEqual(configuration.strict, None)
        self.assertIsNone(configuration.targets)
        self.assertEqual(configuration.typeshed, "typeshed")
        self.assertEqual(configuration.use_buck_builder, False)
        self.assertEqual(configuration.use_buck_source_database, True)
        self.assertEqual(configuration.use_command_v2, True)
        self.assertEqual(
            configuration.python_version, PythonVersion(major=3, minor=6, micro=7)
        )
        self.assertEqual(configuration.shared_memory, SharedMemory(heap_size=42))
        self.assertEqual(configuration.number_of_workers, 43)

    def test_create_from_command_arguments__ide_features(self) -> None:
        configuration = PartialConfiguration.from_command_arguments(
            command_arguments.CommandArguments(
                enable_hover=True,
            )
        )
        assert configuration.ide_features is not None
        self.assertTrue(configuration.ide_features.is_hover_enabled())
        configuration = PartialConfiguration.from_command_arguments(
            command_arguments.CommandArguments(
                enable_hover=False,
            )
        )
        assert configuration.ide_features is not None
        self.assertFalse(configuration.ide_features.is_hover_enabled())
        configuration = PartialConfiguration.from_command_arguments(
            command_arguments.CommandArguments()
        )
        self.assertEqual(configuration.ide_features, None)

    def test_create_from_string_success(self) -> None:
        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"autocomplete": True})
            ).autocomplete,
            True,
        )
        self.assertEqual(
            PartialConfiguration.from_string(json.dumps({"binary": "foo"})).binary,
            "foo",
        )
        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"buck_builder_binary": "foo"})
            ).buck_builder_binary,
            "foo",
        )
        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"buck_mode": "foo"})
            ).buck_mode,
            "foo",
        )
        self.assertEqual(
            PartialConfiguration.from_string(json.dumps({"disabled": True})).disabled,
            True,
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_string(
                    json.dumps({"do_not_ignore_errors_in": ["foo", "bar"]})
                ).do_not_ignore_errors_in
            ),
            ["foo", "bar"],
        )
        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"dot_pyre_directory": "foo"})
            ).dot_pyre_directory,
            Path("foo"),
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_string(
                    json.dumps({"exclude": "foo"})
                ).excludes
            ),
            ["foo"],
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_string(
                    json.dumps({"exclude": ["foo", "bar"]})
                ).excludes
            ),
            ["foo", "bar"],
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_string(
                    json.dumps({"extensions": [".foo", ".bar"]})
                ).extensions
            ),
            [ExtensionElement(".foo", False), ExtensionElement(".bar", False)],
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_string(
                    json.dumps(
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
                    )
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
                PartialConfiguration.from_string(
                    json.dumps({"ignore_all_errors": ["foo", "bar"]})
                ).ignore_all_errors
            ),
            ["foo", "bar"],
        )
        self.assertEqual(
            PartialConfiguration.from_string(json.dumps({"logger": "foo"})).logger,
            "foo",
        )
        self.assertEqual(
            PartialConfiguration.from_string(json.dumps({"oncall": "foo"})).oncall,
            "foo",
        )
        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"workers": 42})
            ).number_of_workers,
            42,
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_string(
                    json.dumps({"critical_files": ["foo", "bar"]})
                ).other_critical_files
            ),
            ["foo", "bar"],
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_string(
                    json.dumps({"search_path": "foo"})
                ).search_path
            ),
            [SimpleSearchPathElement("foo")],
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_string(
                    json.dumps(
                        {"search_path": ["foo", {"root": "bar", "subdirectory": "baz"}]}
                    )
                ).search_path
            ),
            [
                SimpleSearchPathElement("foo"),
                SubdirectorySearchPathElement("bar", "baz"),
            ],
        )
        self.assertEqual(
            PartialConfiguration.from_string(json.dumps({"strict": True})).strict, True
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_string(
                    json.dumps({"taint_models_path": "foo"})
                ).taint_models_path
            ),
            ["foo"],
        )
        self.assertListEqual(
            list(
                PartialConfiguration.from_string(
                    json.dumps({"taint_models_path": ["foo", "bar"]})
                ).taint_models_path
            ),
            ["foo", "bar"],
        )
        self.assertEqual(
            PartialConfiguration.from_string(json.dumps({"typeshed": "foo"})).typeshed,
            "foo",
        )
        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"use_buck_builder": True})
            ).use_buck_builder,
            True,
        )
        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"use_buck_source_database": True})
            ).use_buck_source_database,
            True,
        )
        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"use_command_v2": True})
            ).use_command_v2,
            True,
        )
        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"version": "abc"})
            ).version_hash,
            "abc",
        )
        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"python_version": "3"})
            ).python_version,
            PythonVersion(major=3, minor=0, micro=0),
        )
        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"python_version": "3.6"})
            ).python_version,
            PythonVersion(major=3, minor=6, micro=0),
        )
        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"python_version": "3.6.7"})
            ).python_version,
            PythonVersion(major=3, minor=6, micro=7),
        )

        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"shared_memory": {"heap_size": 1}})
            ).shared_memory,
            SharedMemory(heap_size=1),
        )
        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"shared_memory": {"dependency_table_power": 2}})
            ).shared_memory,
            SharedMemory(dependency_table_power=2),
        )
        self.assertEqual(
            PartialConfiguration.from_string(
                json.dumps({"shared_memory": {"hash_table_power": 3}})
            ).shared_memory,
            SharedMemory(hash_table_power=3),
        )

        self.assertIsNone(PartialConfiguration.from_string("{}").source_directories)
        source_directories = PartialConfiguration.from_string(
            json.dumps({"source_directories": ["foo", "bar"]})
        ).source_directories
        self.assertIsNotNone(source_directories)
        self.assertListEqual(
            list(source_directories),
            [SimpleSearchPathElement("foo"), SimpleSearchPathElement("bar")],
        )

        source_directories = PartialConfiguration.from_string(
            json.dumps({"source_directories": ["foo"]})
        ).source_directories
        self.assertIsNotNone(source_directories)
        self.assertListEqual(
            list(source_directories),
            [SimpleSearchPathElement("foo")],
        )
        source_directories = PartialConfiguration.from_string(
            json.dumps(
                {
                    "source_directories": [
                        "foo",
                        {"root": "bar", "subdirectory": "baz"},
                    ]
                }
            )
        ).source_directories
        self.assertIsNotNone(source_directories)
        self.assertListEqual(
            list(source_directories),
            [
                SimpleSearchPathElement("foo"),
                SubdirectorySearchPathElement("bar", "baz"),
            ],
        )
        source_directories = PartialConfiguration.from_string(
            json.dumps(
                {
                    "source_directories": [
                        "foo",
                        {"import_root": "bar", "source": "baz"},
                    ]
                }
            )
        ).source_directories
        self.assertIsNotNone(source_directories)
        self.assertListEqual(
            list(source_directories),
            [
                SimpleSearchPathElement("foo"),
                SubdirectorySearchPathElement("bar", "baz"),
            ],
        )

        self.assertIsNone(PartialConfiguration.from_string("{}").targets)
        targets = PartialConfiguration.from_string(
            json.dumps({"targets": ["//foo", "//bar"]})
        ).targets
        self.assertIsNotNone(targets)
        self.assertListEqual(list(targets), ["//foo", "//bar"])

        self.assertEqual(
            PartialConfiguration.from_string(json.dumps({"version": "abc"})).file_hash,
            None,
        )
        file_content = json.dumps({"version": "abc", "saved_state": "xyz"})
        self.assertEqual(
            PartialConfiguration.from_string(file_content).file_hash,
            hashlib.sha1(file_content.encode("utf-8")).hexdigest(),
        )

    def test_create_from_string_ide_features(self) -> None:
        def assert_ide_features_equal(input: object, expected: object) -> None:
            self.assertEqual(
                PartialConfiguration.from_string(json.dumps(input)).ide_features,
                expected,
            )

        def assert_ide_features_raises(input: object) -> None:
            with self.assertRaises(InvalidConfiguration):
                PartialConfiguration.from_string(json.dumps(input))

        assert_ide_features_equal({}, None)
        assert_ide_features_equal({"ide_features": {}}, IdeFeatures())
        assert_ide_features_equal(
            {"ide_features": {"hover_enabled": True}}, IdeFeatures(hover_enabled=True)
        )
        assert_ide_features_equal(
            {"ide_features": {"hover_enabled": False}}, IdeFeatures(hover_enabled=False)
        )
        assert_ide_features_raises({"ide_features": {"hover_enabled": 42}})

    def test_create_from_string_failure(self) -> None:
        def assert_raises(content: str) -> None:
            with self.assertRaises(InvalidConfiguration):
                PartialConfiguration.from_string(content)

        assert_raises("")
        assert_raises("{")
        assert_raises(json.dumps({"autocomplete": 42}))
        assert_raises(json.dumps({"binary": True}))
        assert_raises(json.dumps({"buck_builder_binary": ["."]}))
        assert_raises(json.dumps({"buck_mode": {}}))
        assert_raises(json.dumps({"disabled": "False"}))
        assert_raises(json.dumps({"do_not_ignore_errors_in": "abc"}))
        assert_raises(json.dumps({"dot_pyre_directory": {}}))
        assert_raises(json.dumps({"exclude": 42}))
        assert_raises(json.dumps({"extensions": 42}))
        assert_raises(json.dumps({"ignore_all_errors": [1, 2, 3]}))
        assert_raises(json.dumps({"ignore_infer": [False, "bc"]}))
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
        assert_raises(json.dumps({"use_buck_builder": "derp"}))
        assert_raises(json.dumps({"use_buck_source_database": 4.2}))
        assert_raises(json.dumps({"use_command_v2": 42}))
        assert_raises(json.dumps({"version": 123}))
        assert_raises(json.dumps({"python_version": "abc"}))
        assert_raises(json.dumps({"python_version": 42}))
        assert_raises(json.dumps({"shared_memory": "abc"}))
        assert_raises(json.dumps({"shared_memory": {"heap_size": "abc"}}))

    def test_merge(self) -> None:
        # Unsafe features like `getattr` has to be used in this test to reduce boilerplates.

        def create_configuration(name: str, value: object) -> PartialConfiguration:
            return dataclasses.replace(PartialConfiguration(), **{name: value})

        # Overwriting behaves correctly when:
        # - If both base config and overriding config are absent, result is None.
        # - If one of the config is presented, result is the that config.
        # - If both base config and overriding config are present, result is the
        #   overriding config.
        def assert_overwritten(attribute_name: str) -> None:
            # The actual value doesn't really matter. We only care about equalities.
            # This is obviously not type-safe but it does save a significant amount
            # of keystrokes.
            base_value = object()
            override_value = object()
            self.assertIsNone(
                getattr(
                    merge_partial_configurations(
                        base=create_configuration(attribute_name, None),
                        override=create_configuration(attribute_name, None),
                    ),
                    attribute_name,
                )
            )
            self.assertEqual(
                getattr(
                    merge_partial_configurations(
                        base=create_configuration(attribute_name, base_value),
                        override=create_configuration(attribute_name, None),
                    ),
                    attribute_name,
                ),
                base_value,
            )
            self.assertEqual(
                getattr(
                    merge_partial_configurations(
                        base=create_configuration(attribute_name, None),
                        override=create_configuration(attribute_name, override_value),
                    ),
                    attribute_name,
                ),
                override_value,
            )
            self.assertEqual(
                getattr(
                    merge_partial_configurations(
                        base=create_configuration(attribute_name, base_value),
                        override=create_configuration(attribute_name, override_value),
                    ),
                    attribute_name,
                ),
                override_value,
            )

        def assert_prepended(attribute_name: str) -> None:
            # The actual value doesn't really matter. We only care about equalities.
            # This is obviously not type-safe but it does save a significant amount
            # of keystrokes.
            base_value = object()
            override_value = object()
            self.assertListEqual(
                getattr(
                    merge_partial_configurations(
                        base=create_configuration(attribute_name, [base_value]),
                        override=create_configuration(attribute_name, [override_value]),
                    ),
                    attribute_name,
                ),
                [override_value, base_value],
            )

        def assert_raise_when_overridden(attribute_name: str) -> None:
            # The actual value doesn't really matter. We only care about equalities.
            # This is obviously not type-safe but it does save a significant amount
            # of keystrokes.
            base_value = object()
            override_value = object()
            with self.assertRaises(InvalidConfiguration):
                merge_partial_configurations(
                    base=create_configuration(attribute_name, base_value),
                    override=create_configuration(attribute_name, override_value),
                )

        assert_overwritten("autocomplete")
        assert_overwritten("buck_builder_binary")
        assert_overwritten("buck_mode")
        assert_overwritten("disabled")
        assert_prepended("do_not_ignore_errors_in")
        assert_overwritten("dot_pyre_directory")
        assert_prepended("excludes")
        assert_prepended("extensions")
        assert_overwritten("file_hash")
        assert_prepended("ignore_all_errors")
        assert_prepended("ignore_infer")
        assert_overwritten("logger")
        assert_overwritten("number_of_workers")
        assert_overwritten("oncall")
        assert_prepended("other_critical_files")
        assert_overwritten("python_version")
        assert_prepended("search_path")
        assert_raise_when_overridden("source_directories")
        assert_overwritten("strict")
        assert_prepended("taint_models_path")
        assert_raise_when_overridden("targets")
        assert_overwritten("typeshed")
        assert_overwritten("use_buck_builder")
        assert_overwritten("use_buck_source_database")
        assert_overwritten("use_command_v2")
        assert_overwritten("version_hash")

    def test_merge__ide_features(self) -> None:
        def assert_merged(
            base_ide_features: Optional[IdeFeatures],
            override_ide_features: Optional[IdeFeatures],
            expected: Optional[IdeFeatures],
        ) -> None:
            self.assertEqual(
                merge_partial_configurations(
                    base=PartialConfiguration(ide_features=base_ide_features),
                    override=PartialConfiguration(ide_features=override_ide_features),
                ).ide_features,
                expected,
            )

        assert_merged(None, None, None)
        assert_merged(
            IdeFeatures(hover_enabled=True), None, IdeFeatures(hover_enabled=True)
        )
        assert_merged(
            None, IdeFeatures(hover_enabled=True), IdeFeatures(hover_enabled=True)
        )
        assert_merged(
            IdeFeatures(hover_enabled=False),
            IdeFeatures(hover_enabled=True),
            IdeFeatures(hover_enabled=True),
        )
        assert_merged(
            IdeFeatures(hover_enabled=True),
            IdeFeatures(hover_enabled=False),
            IdeFeatures(hover_enabled=False),
        )

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
            PartialConfiguration(buck_builder_binary="foo")
            .expand_relative_paths("bar")
            .buck_builder_binary,
            "bar/foo",
        )
        self.assertEqual(
            PartialConfiguration(do_not_ignore_errors_in=["foo", "bar"])
            .expand_relative_paths("baz")
            .do_not_ignore_errors_in,
            ["baz/foo", "baz/bar"],
        )
        self.assertEqual(
            PartialConfiguration(ignore_all_errors=["foo", "bar"])
            .expand_relative_paths("baz")
            .ignore_all_errors,
            ["baz/foo", "baz/bar"],
        )
        self.assertEqual(
            PartialConfiguration(ignore_infer=["foo", "bar"])
            .expand_relative_paths("baz")
            .ignore_infer,
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
        self.assertEqual(
            PartialConfiguration(
                search_path=[
                    SimpleSearchPathElement("foo"),
                    SubdirectorySearchPathElement("bar", "baz"),
                    SitePackageSearchPathElement("site", "package"),
                ]
            )
            .expand_relative_paths("root")
            .search_path,
            [
                SimpleSearchPathElement("root/foo"),
                SubdirectorySearchPathElement("root/bar", "baz"),
                SitePackageSearchPathElement("site", "package"),
            ],
        )
        self.assertEqual(
            PartialConfiguration(
                source_directories=[
                    SimpleSearchPathElement("foo"),
                    SimpleSearchPathElement("bar"),
                ]
            )
            .expand_relative_paths("baz")
            .source_directories,
            [
                SimpleSearchPathElement("baz/foo"),
                SimpleSearchPathElement("baz/bar"),
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


class ConfigurationTest(testslide.TestCase):
    def test_from_partial_configuration(self) -> None:
        configuration = Configuration.from_partial_configuration(
            project_root=Path("root"),
            relative_local_root="local",
            partial_configuration=PartialConfiguration(
                autocomplete=None,
                binary="binary",
                buck_builder_binary="buck_builder_binary",
                buck_mode="opt",
                disabled=None,
                do_not_ignore_errors_in=["foo"],
                dot_pyre_directory=None,
                excludes=["exclude"],
                extensions=[ExtensionElement(".ext", False)],
                file_hash="abc",
                ide_features=IdeFeatures(hover_enabled=True),
                ignore_all_errors=["bar"],
                ignore_infer=["baz"],
                logger="logger",
                number_of_workers=3,
                oncall="oncall",
                other_critical_files=["critical"],
                python_version=PythonVersion(major=3, minor=6, micro=7),
                shared_memory=SharedMemory(heap_size=1024),
                search_path=[SimpleSearchPathElement("search_path")],
                source_directories=None,
                strict=None,
                taint_models_path=["taint"],
                targets=None,
                typeshed="typeshed",
                use_buck_builder=None,
                use_buck_source_database=None,
                use_command_v2=None,
                version_hash="abc",
            ),
            in_virtual_environment=False,
        )
        self.assertEqual(configuration.project_root, "root")
        self.assertEqual(configuration.relative_local_root, "local")
        self.assertEqual(configuration.autocomplete, False)
        self.assertEqual(configuration.binary, "binary")
        self.assertEqual(configuration.buck_builder_binary, "buck_builder_binary")
        self.assertEqual(configuration.buck_mode, "opt")
        self.assertEqual(configuration.disabled, False)
        self.assertListEqual(list(configuration.do_not_ignore_errors_in), ["foo"])
        self.assertEqual(configuration.dot_pyre_directory, Path("root/.pyre"))
        self.assertListEqual(list(configuration.excludes), ["exclude"])
        self.assertEqual(configuration.extensions, [ExtensionElement(".ext", False)])
        self.assertEqual(configuration.file_hash, "abc")
        self.assertEqual(configuration.ide_features, IdeFeatures(hover_enabled=True))
        self.assertListEqual(list(configuration.ignore_all_errors), ["bar"])
        self.assertListEqual(list(configuration.ignore_infer), ["baz"])
        self.assertEqual(configuration.logger, "logger")
        self.assertEqual(configuration.number_of_workers, 3)
        self.assertEqual(configuration.oncall, "oncall")
        self.assertListEqual(list(configuration.other_critical_files), ["critical"])
        self.assertListEqual(
            list(configuration.search_path), [SimpleSearchPathElement("search_path")]
        )
        self.assertEqual(
            configuration.python_version, PythonVersion(major=3, minor=6, micro=7)
        )
        self.assertEqual(configuration.shared_memory, SharedMemory(heap_size=1024))
        self.assertEqual(configuration.source_directories, None)
        self.assertEqual(configuration.strict, False)
        self.assertEqual(configuration.taint_models_path, ["taint"])
        self.assertEqual(configuration.targets, None)
        self.assertEqual(configuration.typeshed, "typeshed")
        self.assertEqual(configuration.use_buck_builder, False)
        self.assertEqual(configuration.use_buck_source_database, False)
        self.assertEqual(configuration.use_command_v2, True)
        self.assertEqual(configuration.version_hash, "abc")

    def test_get_site_roots(self) -> None:
        global_site_package = "/venv/lib/pythonX/site-packages"
        user_site_package = "/user/lib/pythonX/site-packages"
        self.mock_callable(site, "getsitepackages").to_return_value(
            [global_site_package]
        ).and_assert_called_once()
        self.mock_callable(site, "getusersitepackages").to_return_value(
            user_site_package
        ).and_assert_called_once()
        self.assertListEqual(get_site_roots(), [global_site_package, user_site_package])

    def test_from_partial_configuration_in_virtual_environment(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            ensure_directories_exists(root_path, ["venv/lib/pythonX/site-packages"])

            site_packages = str(root_path / "venv/lib/pythonX/site-packages")
            self.mock_callable(configuration_module, "get_site_roots").to_return_value(
                [site_packages]
            ).and_assert_called_once()

            configuration = Configuration.from_partial_configuration(
                project_root=Path("root"),
                relative_local_root="local",
                partial_configuration=PartialConfiguration(
                    search_path=[],
                ),
                in_virtual_environment=True,
            )
            self.assertListEqual(
                list(configuration.search_path),
                [
                    SimpleSearchPathElement(site_packages),
                ],
            )

    def test_derived_attributes(self) -> None:
        self.assertIsNone(
            Configuration(
                project_root="foo", dot_pyre_directory=Path(".pyre")
            ).local_root
        )
        self.assertEqual(
            Configuration(
                project_root="foo",
                dot_pyre_directory=Path(".pyre"),
                relative_local_root="bar",
            ).local_root,
            "foo/bar",
        )
        self.assertEqual(
            Configuration(
                project_root="foo",
                dot_pyre_directory=Path(".pyre"),
                relative_local_root="bar/baz",
            ).local_root,
            "foo/bar/baz",
        )

        self.assertEqual(
            Configuration(
                project_root="foo", dot_pyre_directory=Path(".pyre")
            ).log_directory,
            ".pyre",
        )
        self.assertEqual(
            Configuration(
                project_root="foo",
                dot_pyre_directory=Path(".pyre"),
                relative_local_root="bar",
            ).log_directory,
            ".pyre/bar",
        )
        self.assertEqual(
            Configuration(
                project_root="foo",
                dot_pyre_directory=Path(".pyre"),
                relative_local_root="bar/baz",
            ).log_directory,
            ".pyre/bar/baz",
        )

    def test_existent_search_path(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            ensure_directories_exists(
                root_path, ["a", "b/c", "d/e/f", "venv/lib/pythonX/site-packages"]
            )

            self.assertListEqual(
                Configuration(
                    project_root="irrelevant",
                    dot_pyre_directory=Path(".pyre"),
                    search_path=[
                        SimpleSearchPathElement(str(root_path / "a")),
                        SimpleSearchPathElement(str(root_path / "x")),
                        SubdirectorySearchPathElement(
                            root=str(root_path / "b"), subdirectory="c"
                        ),
                        SubdirectorySearchPathElement(
                            root=str(root_path / "y"), subdirectory="z"
                        ),
                        SitePackageSearchPathElement(
                            site_root=str(root_path / "d/e"), package_name="f"
                        ),
                        SitePackageSearchPathElement(
                            site_root=str(root_path / "u/v"), package_name="w"
                        ),
                    ],
                ).expand_and_get_existent_search_paths(),
                [
                    SimpleSearchPathElement(str(root_path / "a")),
                    SubdirectorySearchPathElement(
                        root=str(root_path / "b"), subdirectory="c"
                    ),
                    SitePackageSearchPathElement(
                        site_root=str(root_path / "d/e"), package_name="f"
                    ),
                ],
            )

    def test_existent_search_path_glob(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            ensure_directories_exists(root_path, ["a1", "a2", "b"])
            self.assertListEqual(
                Configuration(
                    project_root="irrelevant",
                    dot_pyre_directory=Path(".pyre"),
                    search_path=[SimpleSearchPathElement(str(root_path / "a?"))],
                ).expand_and_get_existent_search_paths(),
                [
                    SimpleSearchPathElement(str(root_path / "a1")),
                    SimpleSearchPathElement(str(root_path / "a2")),
                ],
            )

    def test_existent_search_path_with_typeshed(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            ensure_directories_exists(root_path, ["a"])
            ensure_directories_exists(
                root_path, ["typeshed/stdlib", "typeshed/stubs/foo"]
            )

            self.assertListEqual(
                Configuration(
                    project_root="irrelevant",
                    dot_pyre_directory=Path(".pyre"),
                    search_path=[
                        SimpleSearchPathElement(str(root_path / "a")),
                    ],
                    typeshed=str(root_path / "typeshed"),
                ).expand_and_get_existent_search_paths(),
                [
                    SimpleSearchPathElement(str(root_path / "a")),
                    SimpleSearchPathElement(str(root_path / "typeshed/stdlib")),
                    SimpleSearchPathElement(str(root_path / "typeshed/stubs/foo")),
                ],
            )

    def test_existent_ignore_infer(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            ensure_directories_exists(root_path, ["a", "b/c"])

            self.assertCountEqual(
                Configuration(
                    project_root=str(root_path),
                    dot_pyre_directory=Path(".pyre"),
                    ignore_infer=[
                        str(root_path / "a"),
                        str(root_path / "x"),
                        str(root_path / "b/c"),
                        str(root_path / "y/z"),
                    ],
                ).get_existent_ignore_infer_paths(),
                [str(root_path / "a"), str(root_path / "b/c")],
            )

    def test_existent_do_not_ignore_errors(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            ensure_directories_exists(root_path, ["a", "b/c"])

            self.assertCountEqual(
                Configuration(
                    project_root=str(root_path),
                    dot_pyre_directory=Path(".pyre"),
                    do_not_ignore_errors_in=[
                        str(root_path / "a"),
                        str(root_path / "x"),
                        "//b/c",
                        "//y/z",
                    ],
                ).get_existent_do_not_ignore_errors_in_paths(),
                [str(root_path / "a"), str(root_path / "b/c")],
            )

    def test_existent_ignore_all_errors(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            ensure_directories_exists(root_path, ["a", "b/c", "b/d"])

            self.assertCountEqual(
                Configuration(
                    project_root=str(root_path),
                    dot_pyre_directory=Path(".pyre"),
                    ignore_all_errors=[
                        str(root_path / "a"),
                        str(root_path / "x"),
                        "//b/c",
                        "//y/z",
                        f"{root_path}/b/*",
                    ],
                ).get_existent_ignore_all_errors_paths(),
                [
                    str(root_path / "a"),
                    str(root_path / "b/c"),
                    str(root_path / "b/c"),
                    str(root_path / "b/d"),
                ],
            )

    def test_get_binary_version_ok(self) -> None:
        binary_path = "foo"
        version = "facefacefaceb00"

        self.mock_callable(subprocess, "run").to_return_value(
            subprocess.CompletedProcess(
                args=[binary_path, "-version"], returncode=0, stdout=f"{version}\n"
            )
        ).and_assert_called_once()

        self.assertEqual(
            Configuration(
                project_root="irrelevant",
                dot_pyre_directory=Path(".pyre"),
                binary=binary_path,
            ).get_binary_version(),
            version,
        )

    def test_get_binary_version_fail(self) -> None:
        binary_path = "foo"

        self.mock_callable(subprocess, "run").to_return_value(
            subprocess.CompletedProcess(
                args=[binary_path, "-version"], returncode=1, stdout="derp"
            )
        ).and_assert_called_once()

        self.assertIsNone(
            Configuration(
                project_root="irrelevant",
                dot_pyre_directory=Path(".pyre"),
                binary=binary_path,
            ).get_binary_version()
        )

    def test_get_number_of_workers(self) -> None:
        self.assertEqual(
            Configuration(
                project_root="irrelevant",
                dot_pyre_directory=Path(".pyre"),
                number_of_workers=42,
            ).get_number_of_workers(),
            42,
        )
        # Whatever the default number is, it should be positive
        self.assertGreater(
            Configuration(
                project_root="irrelevant",
                dot_pyre_directory=Path(".pyre"),
                number_of_workers=None,
            ).get_number_of_workers(),
            0,
        )

    def test_get_python_versions(self) -> None:
        self.assertEqual(
            Configuration(
                project_root="irrelevant",
                dot_pyre_directory=Path(".pyre"),
                python_version=PythonVersion(major=3, minor=6, micro=7),
            ).get_python_version(),
            PythonVersion(major=3, minor=6, micro=7),
        )
        self.assertEqual(
            Configuration(
                project_root="irrelevant",
                dot_pyre_directory=Path(".pyre"),
                python_version=None,
            ).get_python_version(),
            PythonVersion(
                major=sys.version_info.major,
                minor=sys.version_info.minor,
                micro=sys.version_info.micro,
            ),
        )

    def test_get_binary_from_configuration(self) -> None:
        with switch_environment({}):
            self.assertEqual(
                Configuration(
                    project_root="irrelevant",
                    dot_pyre_directory=Path(".pyre"),
                    binary="foo",
                ).get_binary_respecting_override(),
                "foo",
            )

    def test_get_binary_auto_determined(self) -> None:
        self.mock_callable(shutil, "which").for_call(BINARY_NAME).to_return_value(
            "foo"
        ).and_assert_called_once()

        with switch_environment({}):
            self.assertEqual(
                Configuration(
                    project_root="irrelevant",
                    dot_pyre_directory=Path(".pyre"),
                    binary=None,
                ).get_binary_respecting_override(),
                "foo",
            )

    def test_get_binary_cannot_auto_determine(self) -> None:
        self.mock_callable(shutil, "which").to_return_value(None).and_assert_called()

        with switch_environment({}):
            self.assertEqual(
                Configuration(
                    project_root="irrelevant",
                    dot_pyre_directory=Path(".pyre"),
                    binary=None,
                ).get_binary_respecting_override(),
                None,
            )

    def test_get_typeshed_from_configuration(self) -> None:
        with switch_environment({}):
            self.assertEqual(
                Configuration(
                    project_root="irrelevant",
                    dot_pyre_directory=Path(".pyre"),
                    typeshed="foo",
                ).get_typeshed_respecting_override(),
                "foo",
            )

    def test_get_typeshed_auto_determined(self) -> None:
        self.mock_callable(
            find_directories, "find_typeshed"
        ).for_call().to_return_value(Path("foo")).and_assert_called_once()

        with switch_environment({}):
            self.assertEqual(
                Configuration(
                    project_root="irrelevant",
                    dot_pyre_directory=Path(".pyre"),
                    typeshed=None,
                ).get_typeshed_respecting_override(),
                "foo",
            )

    def test_get_typeshed_cannot_auto_determine(self) -> None:
        self.mock_callable(
            find_directories, "find_typeshed"
        ).for_call().to_return_value(None).and_assert_called_once()

        with switch_environment({}):
            self.assertEqual(
                Configuration(
                    project_root="irrelevant",
                    dot_pyre_directory=Path(".pyre"),
                    typeshed=None,
                ).get_typeshed_respecting_override(),
                None,
            )

    def test_get_version_hash_from_configuration(self) -> None:
        with switch_environment({}):
            self.assertEqual(
                Configuration(
                    project_root="irrelevant",
                    dot_pyre_directory=Path(".pyre"),
                    version_hash="abc",
                ).get_version_hash_respecting_override(),
                "abc",
            )

    def test_get_version_hash_environment_override(self) -> None:
        with switch_environment({"PYRE_VERSION_HASH": "abc"}):
            self.assertEqual(
                Configuration(
                    project_root="irrelevant",
                    dot_pyre_directory=Path(".pyre"),
                    version_hash=None,
                ).get_version_hash_respecting_override(),
                "abc",
            )
            self.assertEqual(
                Configuration(
                    project_root="irrelevant",
                    dot_pyre_directory=Path(".pyre"),
                    version_hash="def",
                ).get_version_hash_respecting_override(),
                "abc",
            )

    def test_get_valid_extension_suffixes(self) -> None:
        self.assertListEqual(
            Configuration(
                project_root="irrelevant",
                dot_pyre_directory=Path(".pyre"),
                extensions=[],
            ).get_valid_extension_suffixes(),
            [],
        )
        self.assertListEqual(
            Configuration(
                project_root="irrelevant",
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
                project_root="irrelevant",
                dot_pyre_directory=Path(".pyre"),
                extensions=[
                    ExtensionElement("foo", False),
                    ExtensionElement(".bar", False),
                    ExtensionElement("baz", False),
                ],
            ).get_valid_extension_suffixes(),
            [".bar"],
        )

    def test_is_hover_enabled(self) -> None:
        self.assertFalse(
            Configuration(
                project_root="irrelevant",
                dot_pyre_directory=Path(".pyre"),
            ).is_hover_enabled(),
        )
        self.assertTrue(
            Configuration(
                project_root="irrelevant",
                dot_pyre_directory=Path(".pyre"),
                ide_features=IdeFeatures(hover_enabled=True),
            ).is_hover_enabled(),
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
                self.assertEqual(configuration.project_root, str(root_path))
                self.assertEqual(configuration.relative_local_root, None)
                self.assertEqual(configuration.dot_pyre_directory, root_path / ".pyre")
                self.assertListEqual(
                    list(configuration.source_directories or []),
                    [SimpleSearchPathElement(str(root_path))],
                )

    def test_create_from_global_configuration(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            write_configuration_file(root_path, {"strict": False})

            with switch_working_directory(root_path):
                configuration = create_configuration(
                    command_arguments.CommandArguments(
                        strict=True,  # override configuration file
                        source_directories=["."],
                        dot_pyre_directory=Path(".pyre"),
                    ),
                    base_directory=Path(root),
                )
                self.assertEqual(configuration.project_root, str(root_path))
                self.assertEqual(configuration.relative_local_root, None)
                self.assertEqual(configuration.dot_pyre_directory, Path(".pyre"))
                self.assertEqual(configuration.strict, True)
                self.assertListEqual(
                    list(configuration.source_directories or []),
                    [SimpleSearchPathElement(str(root_path))],
                )

    def test_create_from_local_configuration(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root).resolve()
            ensure_directories_exists(root_path, ["foo", "bar", "baz"])
            write_configuration_file(
                root_path, {"strict": False, "search_path": ["foo"]}
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
                self.assertEqual(configuration.project_root, str(root_path))
                self.assertEqual(configuration.relative_local_root, "local")
                self.assertEqual(configuration.dot_pyre_directory, Path(".pyre"))
                self.assertEqual(configuration.strict, True)
                self.assertListEqual(
                    list(configuration.source_directories or []),
                    [SimpleSearchPathElement(str(root_path))],
                )
                self.assertListEqual(
                    list(configuration.search_path),
                    [
                        SimpleSearchPathElement(str(root_path / "bar")),
                        SimpleSearchPathElement(str(root_path / "local/baz")),
                        SimpleSearchPathElement(str(root_path / "foo")),
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
                        project_root=root,
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
                        project_root=root,
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
                        project_root=root,
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
                        project_root=root,
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
                        project_root=root,
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
                        project_root=root,
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

            try:
                check_nested_local_configuration(
                    Configuration(
                        project_root=root,
                        dot_pyre_directory=Path(".pyre"),
                        relative_local_root="nest0/nest1/local",
                    )
                )
            except InvalidConfiguration:
                self.fail("Nested local configuration check fails unexpectedly!")

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

            try:
                check_nested_local_configuration(
                    Configuration(
                        project_root=root,
                        dot_pyre_directory=Path(".pyre"),
                        relative_local_root="nest0/nest1/local",
                    )
                )
            except InvalidConfiguration:
                self.fail("Nested local configuration check fails unexpectedly!")

    def test_source_directories_glob(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            ensure_directories_exists(root_path, ["a1", "a2", "b", "c"])
            source_directories = (
                Configuration(
                    project_root="irrelevant",
                    dot_pyre_directory=Path(".pyre"),
                    source_directories=[
                        SimpleSearchPathElement(str(root_path / "a*")),
                        SimpleSearchPathElement(str(root_path / "b")),
                    ],
                )
                .expand_and_filter_nonexistent_paths()
                .source_directories
            )
            self.assertIsNotNone(source_directories)
            self.assertListEqual(
                list(source_directories),
                [
                    SimpleSearchPathElement(str(root_path / "a1")),
                    SimpleSearchPathElement(str(root_path / "a2")),
                    SimpleSearchPathElement(str(root_path / "b")),
                ],
            )


class SearchPathElementTest(unittest.TestCase):
    def test_create(self) -> None:
        self.assertListEqual(
            create_search_paths("foo", site_roots=[]), [SimpleSearchPathElement("foo")]
        )
        self.assertListEqual(
            create_search_paths({"root": "foo", "subdirectory": "bar"}, site_roots=[]),
            [SubdirectorySearchPathElement("foo", "bar")],
        )
        self.assertListEqual(
            create_search_paths({"import_root": "foo", "source": "bar"}, site_roots=[]),
            [SubdirectorySearchPathElement("foo", "bar")],
        )
        self.assertListEqual(
            create_search_paths({"site-package": "foo"}, site_roots=[]), []
        )
        self.assertListEqual(
            create_search_paths({"site-package": "foo"}, site_roots=["site0"]),
            [SitePackageSearchPathElement("site0", "foo")],
        )
        self.assertListEqual(
            create_search_paths({"site-package": "foo"}, site_roots=["site1"]),
            [SitePackageSearchPathElement("site1", "foo")],
        )
        self.assertListEqual(
            create_search_paths(
                {"site-package": "foo", "is_toplevel_module": "true"},
                site_roots=["site1"],
            ),
            [SitePackageSearchPathElement("site1", "foo", True)],
        )

        with self.assertRaises(InvalidConfiguration):
            create_search_paths({}, site_roots=[])
        with self.assertRaises(InvalidConfiguration):
            create_search_paths({"foo": "bar"}, site_roots=[])
        with self.assertRaises(InvalidConfiguration):
            create_search_paths({"root": "foo"}, site_roots=[])

    def test_path(self) -> None:
        self.assertEqual(SimpleSearchPathElement("foo").path(), "foo")
        self.assertEqual(SubdirectorySearchPathElement("foo", "bar").path(), "foo/bar")
        self.assertEqual(SitePackageSearchPathElement("foo", "bar").path(), "foo/bar")

    def test_get_root(self) -> None:
        self.assertEqual(SimpleSearchPathElement("foo").get_root(), "foo")
        self.assertEqual(SubdirectorySearchPathElement("foo", "bar").get_root(), "foo")
        self.assertEqual(SitePackageSearchPathElement("foo", "bar").get_root(), "foo")

    def test_command_line_argument(self) -> None:
        self.assertEqual(SimpleSearchPathElement("foo").command_line_argument(), "foo")
        self.assertEqual(
            SubdirectorySearchPathElement("foo", "bar").command_line_argument(),
            "foo$bar",
        )
        self.assertEqual(
            SitePackageSearchPathElement("foo", "bar").command_line_argument(),
            "foo$bar",
        )
        self.assertEqual(
            SitePackageSearchPathElement("foo", "bar", True).command_line_argument(),
            "foo$bar.py",
        )

    def test_expand_global_root(self) -> None:
        self.assertEqual(
            SimpleSearchPathElement("//simple/path").expand_global_root("root"),
            SimpleSearchPathElement("root/simple/path"),
        )
        self.assertEqual(
            SubdirectorySearchPathElement("//path", "sub").expand_global_root("root"),
            SubdirectorySearchPathElement("root/path", "sub"),
        )
        self.assertEqual(
            SitePackageSearchPathElement("//site_root", "package").expand_global_root(
                "root"
            ),
            SitePackageSearchPathElement("//site_root", "package"),
        )

    def test_expand_relative_root(self) -> None:
        self.assertEqual(
            SimpleSearchPathElement("simple/path").expand_relative_root(
                "root/local_project"
            ),
            SimpleSearchPathElement("root/local_project/simple/path"),
        )
        self.assertEqual(
            SubdirectorySearchPathElement("path", "sub").expand_relative_root(
                "root/local_project"
            ),
            SubdirectorySearchPathElement("root/local_project/path", "sub"),
        )
        self.assertEqual(
            SitePackageSearchPathElement("site_root", "package").expand_relative_root(
                "root/local_project"
            ),
            SitePackageSearchPathElement("site_root", "package"),
        )

    def test_expand_glob(self) -> None:
        with tempfile.TemporaryDirectory() as root:
            root_path = Path(root)
            ensure_directories_exists(root_path, ["a1", "a2", "b"])

            search_path = SimpleSearchPathElement(str(root_path / "a*"))

            self.assertListEqual(
                search_path.expand_glob(),
                [
                    SimpleSearchPathElement(str(root_path / "a1")),
                    SimpleSearchPathElement(str(root_path / "a2")),
                ],
            )
