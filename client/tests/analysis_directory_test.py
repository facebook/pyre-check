# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path
from typing import Dict
from unittest.mock import MagicMock, call, patch

from .. import (
    analysis_directory,
    buck,
    configuration as configuration_module,
    filesystem,
)
from ..analysis_directory import (
    REBUILD_THRESHOLD_FOR_NEW_OR_DELETED_PATHS,
    REBUILD_THRESHOLD_FOR_UPDATED_PATHS,
    AnalysisDirectory,
    SharedAnalysisDirectory,
    UpdatedPaths,
    __name__ as analysis_directory_name,
    _get_buck_builder,
    _get_project_name,
    _resolve_filter_paths,
    resolve_analysis_directory,
)


class AnalysisDirectoryTest(unittest.TestCase):
    def assertEqualRootAndFilterRoot(
        self, actual: AnalysisDirectory, expected: AnalysisDirectory
    ) -> None:
        self.assertEqual(expected.get_root(), actual.get_root())
        self.assertEqual(expected.get_filter_roots(), actual.get_filter_roots())

    @patch.object(os.path, "isfile")
    # pyre-fixme[56]: Argument `os.path` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(os.path, "abspath", side_effect=lambda path: path)
    def test_process_updated_files(self, abspath: MagicMock, isfile: MagicMock) -> None:
        analysis_directory = AnalysisDirectory(
            path=configuration_module.SimpleSearchPathElement("foo/bar"),
            search_path=["baz$hello"],
        )
        isfile.side_effect = lambda path: path != "foo/bar/deleted.py"
        actual = analysis_directory.process_updated_files(
            [
                "foo/bar/tracked.py",
                "foo/not_tracked.py",
                "baz/hello/also_tracked.py",
                "foo/bar/deleted.py",
            ]
        )
        expected = UpdatedPaths(
            updated_paths=[
                "foo/bar/tracked.py",
                "baz/hello/also_tracked.py",
                "foo/bar/deleted.py",
            ],
            deleted_paths=["foo/bar/deleted.py"],
        )
        self.assertEqual(actual, expected)

    # pyre-fixme[56]: Argument `tools.pyre.client.buck` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(
        buck,
        "generate_source_directories",
        side_effect=lambda targets, build, prompt: targets,
    )
    def test_resolve_analysis_directory(self, buck) -> None:  # pyre-fixme[2]
        original_directory = "/project"
        project_root = "/project"

        configuration = configuration_module.Configuration.from_partial_configuration(
            project_root=Path(project_root),
            relative_local_root=None,
            partial_configuration=configuration_module.PartialConfiguration(),
        )

        expected_analysis_directory = AnalysisDirectory(
            configuration_module.SimpleSearchPathElement("a/b")
        )
        analysis_directory = resolve_analysis_directory(
            source_directories=["a/b"],
            targets=[],
            configuration=configuration,
            original_directory=original_directory,
            filter_directory=None,
        )
        self.assertEqualRootAndFilterRoot(
            analysis_directory, expected_analysis_directory
        )

        expected_analysis_directory = AnalysisDirectory(
            configuration_module.SimpleSearchPathElement("/symlinked/directory"),
            filter_paths={"/real/directory"},
        )
        analysis_directory = resolve_analysis_directory(
            source_directories=["/symlinked/directory"],
            targets=[],
            configuration=configuration,
            original_directory=original_directory,
            filter_directory="/real/directory",
        )
        self.assertEqualRootAndFilterRoot(
            analysis_directory, expected_analysis_directory
        )


class SharedAnalysisDirectoryTest(unittest.TestCase):
    def assertEqualRootAndFilterRoot(
        self, actual: AnalysisDirectory, expected: AnalysisDirectory
    ) -> None:
        self.assertEqual(expected.get_root(), actual.get_root())
        self.assertEqual(expected.get_filter_roots(), actual.get_filter_roots())

    def assertFileIsLinkedBothWays(
        self,
        relative_path: str,
        shared_analysis_directory: SharedAnalysisDirectory,
        scratch_directory: str,
        project_directory: str,
    ) -> None:
        self.assertEqual(
            os.path.realpath(os.path.join(scratch_directory, relative_path)),
            os.path.realpath(os.path.join(project_directory, relative_path)),
        )
        self.assertEqual(
            shared_analysis_directory._symbolic_links[
                os.path.realpath(os.path.join(project_directory, relative_path))
            ],
            os.path.join(scratch_directory, relative_path),
        )

    @patch(f"{analysis_directory_name}.find_buck_root", return_value="/buck_root")
    @patch.object(os.path, "exists", return_value=True)
    def test_filter_root_for_buck(
        self, exists: MagicMock, buck_root: MagicMock
    ) -> None:
        configuration = configuration_module.Configuration.from_partial_configuration(
            project_root=Path("/buck_root/pyre_root"),
            relative_local_root="local",
            partial_configuration=configuration_module.PartialConfiguration(
                targets=["cell//pyre_root/local:target"],
                use_buck_builder=True,
                buck_mode=None,
            ),
        )
        analysis_directory = resolve_analysis_directory(
            source_directories=[],
            targets=[],
            configuration=configuration,
            original_directory="/buck_root/pyre_root/local",
            filter_directory=None,
            isolate=False,
            relative_local_root=None,
        )
        self.assertEqual(
            analysis_directory.get_filter_roots(), {"/buck_root/pyre_root/local"}
        )

    def test_filter_root_for_nonbuck(self) -> None:
        configuration = configuration_module.Configuration.from_partial_configuration(
            project_root=Path("/foo"),
            relative_local_root=None,
            partial_configuration=configuration_module.PartialConfiguration(
                source_directories=[
                    configuration_module.SimpleSearchPathElement("/foo/bar"),
                    configuration_module.SimpleSearchPathElement("/foo/baz"),
                ],
                targets=[],
                use_buck_builder=False,
                buck_mode=None,
            ),
        )
        analysis_directory = resolve_analysis_directory(
            source_directories=[],
            targets=[],
            configuration=configuration,
            original_directory="/foo",
            filter_directory=None,
            isolate=False,
            relative_local_root=None,
        )
        self.assertEqual(analysis_directory.get_filter_roots(), {"/foo"})

    @patch.object(os, "getcwd", return_value="/root")
    @patch.object(os.path, "isfile")
    # pyre-fixme[56]: Argument `os.path` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(os.path, "abspath", side_effect=lambda path: path)
    def test_get_new_deleted_and_tracked_paths_for_local_project(
        self, absolute_path: MagicMock, isfile: MagicMock, get_project_root: MagicMock
    ) -> None:
        def _isfile(path: str) -> bool:
            return "foo/deleted" not in path

        isfile.side_effect = _isfile
        shared_analysis_directory = SharedAnalysisDirectory(
            project_root="/root",
            source_directories=[],
            targets=["target1"],
            search_path=["/scratch$baz$hello", "/root/typeshed"],
        )
        shared_analysis_directory._symbolic_links = {
            "/root/project/tracked.py": "/scratch/bar/tracked.py",
            "/root/project/foo/deleted.py": "/scratch/foo/deleted.py",
        }
        shared_analysis_directory._local_configuration_root = "project"
        (
            new_paths,
            deleted_paths,
            tracked_paths,
        ) = shared_analysis_directory._get_new_deleted_and_tracked_paths(
            [
                "/root/project/foo/deleted.py",
                "/root/other-project/foo/deleted.py",
                "/root/project/new.py",
                "/root/other-project/new.py",
                "/root/project/tracked.py",
                "/scratch/baz/hello/also_tracked.py",
                "/root/typeshed/foo.pyi",
            ]
        )
        self.assertEqual(
            new_paths, ["/root/project/new.py", "/root/other-project/new.py"]
        )
        self.assertEqual(deleted_paths, ["/root/project/foo/deleted.py"])
        self.assertEqual(
            tracked_paths,
            [
                "/root/project/tracked.py",
                "/scratch/baz/hello/also_tracked.py",
                "/root/typeshed/foo.pyi",
            ],
        )

    @patch.object(os, "getcwd", return_value="/root")
    @patch.object(os.path, "isfile")
    # pyre-fixme[56]: Argument `os.path` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(os.path, "abspath", side_effect=lambda path: path)
    def test_get_new_deleted_and_tracked_paths_for_root_project(
        self, absolute_path: MagicMock, isfile: MagicMock, get_project_root: MagicMock
    ) -> None:
        def _isfile(path: str) -> bool:
            return "foo/deleted" not in path

        isfile.side_effect = _isfile
        shared_analysis_directory = SharedAnalysisDirectory(
            project_root="/root",
            source_directories=[],
            targets=["target1"],
            search_path=["/scratch$baz$hello"],
        )
        shared_analysis_directory._symbolic_links = {
            "/root/project/tracked.py": "/scratch/bar/tracked.py",
            "/root/project/foo/deleted.py": "/scratch/foo/deleted.py",
            "/root/other-project/foo/deleted.py": "/scratch/other-foo/deleted.py",
        }
        (
            new_paths,
            deleted_paths,
            tracked_paths,
        ) = shared_analysis_directory._get_new_deleted_and_tracked_paths(
            [
                "/root/project/foo/deleted.py",
                "/root/other-project/foo/deleted.py",
                "/root/project/new.py",
                "/root/other-project/new.py",
                "/root/project/tracked.py",
                "/scratch/baz/hello/also_tracked.py",
            ]
        )
        self.assertEqual(
            new_paths, ["/root/project/new.py", "/root/other-project/new.py"]
        )
        self.assertEqual(
            deleted_paths,
            ["/root/project/foo/deleted.py", "/root/other-project/foo/deleted.py"],
        )
        self.assertEqual(
            tracked_paths,
            ["/root/project/tracked.py", "/scratch/baz/hello/also_tracked.py"],
        )

    def test_should_rebuild(self) -> None:
        self.assertTrue(
            SharedAnalysisDirectory.should_rebuild(
                updated_tracked_paths=[
                    f"a{index}.py"
                    for index in range(REBUILD_THRESHOLD_FOR_UPDATED_PATHS)
                ],
                new_paths=[],
                deleted_paths=[],
            )
        )
        self.assertTrue(
            SharedAnalysisDirectory.should_rebuild(
                updated_tracked_paths=[],
                new_paths=[
                    f"a{index}.py"
                    for index in range(REBUILD_THRESHOLD_FOR_NEW_OR_DELETED_PATHS // 2)
                ],
                deleted_paths=[
                    f"b{index}.py"
                    for index in range(
                        REBUILD_THRESHOLD_FOR_NEW_OR_DELETED_PATHS
                        - REBUILD_THRESHOLD_FOR_NEW_OR_DELETED_PATHS // 2
                    )
                ],
            )
        )
        self.assertFalse(
            SharedAnalysisDirectory.should_rebuild(
                updated_tracked_paths=["a.py"], new_paths=[], deleted_paths=[]
            )
        )
        self.assertFalse(
            SharedAnalysisDirectory.should_rebuild(
                updated_tracked_paths=["a.py", "b/c/not_real_TARGETS"],
                new_paths=[],
                deleted_paths=[],
            )
        )
        self.assertTrue(
            SharedAnalysisDirectory.should_rebuild(
                updated_tracked_paths=["a.py", "b/c/TARGETS"],
                new_paths=[],
                deleted_paths=[],
            )
        )
        self.assertTrue(
            SharedAnalysisDirectory.should_rebuild(
                updated_tracked_paths=[], new_paths=["TARGETS"], deleted_paths=[]
            )
        )
        self.assertTrue(
            SharedAnalysisDirectory.should_rebuild(
                updated_tracked_paths=[], new_paths=[], deleted_paths=["TARGETS"]
            )
        )
        self.assertTrue(
            SharedAnalysisDirectory.should_rebuild(
                updated_tracked_paths=["a.py", "b/c/foo.thrift"],
                new_paths=[],
                deleted_paths=[],
            )
        )

    @patch.object(analysis_directory, "add_symbolic_link")
    @patch.object(SharedAnalysisDirectory, "rebuild")
    @patch.object(SharedAnalysisDirectory, "should_rebuild", return_value=False)
    @patch.object(os, "getcwd", return_value="project")
    @patch.object(os.path, "isfile", return_value=True)
    # pyre-fixme[56]: Argument `os.path` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(os.path, "abspath", side_effect=lambda path: path)
    def test_process_updated_files_only_tracked_files(
        self,
        abspath: MagicMock,
        isfile: MagicMock,
        getcwd: MagicMock,
        should_rebuild: MagicMock,
        rebuild: MagicMock,
        add_symbolic_link: MagicMock,
    ) -> None:
        shared_analysis_directory = SharedAnalysisDirectory(
            project_root="project",
            source_directories=[],
            targets=["target1"],
            search_path=["scratch$baz$hello"],
        )

        old_symbolic_links = {
            "project/tracked.py": "scratch/bar/tracked.py",
            "project/tracked2.py": "scratch/bar/tracked2.py",
        }
        shared_analysis_directory._symbolic_links = old_symbolic_links
        actual = shared_analysis_directory._process_updated_files(
            [
                "project/tracked.py",
                "project/tracked2.py",
                "other_project/not_tracked.py",
                "scratch/baz/hello/new_file_tracked_because_of_search_path.py",
            ]
        )
        # Note, however, that new files in the search_path are shown as
        # updated files. This is the existing behavior and may be a bug.
        expected = UpdatedPaths(
            updated_paths=[
                "scratch/bar/tracked.py",
                "scratch/bar/tracked2.py",
                "scratch/baz/hello/new_file_tracked_because_of_search_path.py",
            ],
            deleted_paths=[],
        )
        self.assertEqual(actual, expected)
        rebuild.assert_not_called()
        add_symbolic_link.assert_not_called()
        self.assertEqual(shared_analysis_directory._symbolic_links, old_symbolic_links)
        self.assertIsNone(shared_analysis_directory._last_singly_deleted_path_and_link)

    @patch.object(analysis_directory, "add_symbolic_link")
    @patch.object(SharedAnalysisDirectory, "get_root", return_value="scratch")
    @patch.object(buck, "query_buck_relative_paths")
    @patch.object(SharedAnalysisDirectory, "rebuild")
    @patch.object(SharedAnalysisDirectory, "should_rebuild", return_value=False)
    @patch.object(os, "getcwd", return_value="project")
    @patch.object(os.path, "isfile", return_value=True)
    # pyre-fixme[56]: Argument `os.path` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(os.path, "abspath", side_effect=lambda path: path)
    def test_process_updated_files_one_new_file(
        self,
        abspath: MagicMock,
        isfile: MagicMock,
        getcwd: MagicMock,
        should_rebuild: MagicMock,
        rebuild: MagicMock,
        query_buck_relative_paths: MagicMock,
        get_root: MagicMock,
        add_symbolic_link: MagicMock,
    ) -> None:
        shared_analysis_directory = SharedAnalysisDirectory(
            project_root="project",
            source_directories=[],
            targets=["target1"],
            search_path=["baz$hello"],
        )

        query_buck_relative_paths.return_value = {
            "project/something/new_file.py": "foo/new_file.py"
        }

        shared_analysis_directory._symbolic_links = {
            "project/tracked.py": "scratch/bar/tracked.py"
        }
        actual = shared_analysis_directory._process_updated_files(
            ["project/tracked.py", "project/something/new_file.py"]
        )
        expected = UpdatedPaths(
            updated_paths=["scratch/bar/tracked.py", "scratch/foo/new_file.py"],
            deleted_paths=[],
        )
        self.assertEqual(actual, expected)
        self.assertEqual(
            shared_analysis_directory._symbolic_links,
            {
                "project/tracked.py": "scratch/bar/tracked.py",
                "project/something/new_file.py": "scratch/foo/new_file.py",
            },
        )
        rebuild.assert_not_called()
        add_symbolic_link.assert_called_once_with(
            "scratch/foo/new_file.py", "project/something/new_file.py"
        )
        self.assertIsNone(shared_analysis_directory._last_singly_deleted_path_and_link)

    @patch.object(analysis_directory, "add_symbolic_link")
    @patch.object(SharedAnalysisDirectory, "get_root", return_value="scratch")
    @patch.object(subprocess, "check_output")
    @patch.object(SharedAnalysisDirectory, "rebuild")
    @patch.object(SharedAnalysisDirectory, "should_rebuild", return_value=False)
    @patch.object(os, "getcwd", return_value="project")
    @patch.object(os.path, "isfile", return_value=True)
    # pyre-fixme[56]: Argument `os.path` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(os.path, "abspath", side_effect=lambda path: path)
    def test_process_updated_files__untracked_new_file(
        self,
        abspath: MagicMock,
        isfile: MagicMock,
        getcwd: MagicMock,
        should_rebuild: MagicMock,
        rebuild: MagicMock,
        check_output: MagicMock,
        get_root: MagicMock,
        add_symbolic_link: MagicMock,
    ) -> None:
        shared_analysis_directory = SharedAnalysisDirectory(
            project_root="project",
            source_directories=[],
            targets=["target1"],
            search_path=["baz$hello"],
        )

        check_output.return_value = b"{}"

        symbolic_links = {}
        shared_analysis_directory._symbolic_links = symbolic_links
        actual = shared_analysis_directory._process_updated_files(
            ["project/something/untracked_new_file.py"]
        )
        expected = UpdatedPaths(updated_paths=[], deleted_paths=[])
        self.assertEqual(actual, expected)
        self.assertEqual(shared_analysis_directory._symbolic_links, symbolic_links)
        rebuild.assert_not_called()
        check_output.assert_called_once()

        # Should not call `buck query` on the untracked file a second time.
        check_output.reset_mock()
        actual = shared_analysis_directory._process_updated_files(
            ["project/something/untracked_new_file.py"]
        )
        expected = UpdatedPaths(updated_paths=[], deleted_paths=[])
        self.assertEqual(actual, expected)
        check_output.assert_not_called()

    @patch.object(analysis_directory, "add_symbolic_link")
    @patch.object(SharedAnalysisDirectory, "get_root", return_value="scratch")
    @patch.object(buck, "query_buck_relative_paths")
    @patch.object(SharedAnalysisDirectory, "rebuild")
    @patch.object(SharedAnalysisDirectory, "should_rebuild", return_value=False)
    @patch.object(os, "getcwd", return_value="project")
    @patch.object(os.path, "isfile", return_value=True)
    # pyre-fixme[56]: Argument `os.path` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(os.path, "abspath", side_effect=lambda path: path)
    def test_process_updated_files_same_file_after_deletion(
        self,
        abspath: MagicMock,
        isfile: MagicMock,
        getcwd: MagicMock,
        should_rebuild: MagicMock,
        rebuild: MagicMock,
        query_buck_relative_paths: MagicMock,
        get_root: MagicMock,
        add_symbolic_link: MagicMock,
    ) -> None:
        shared_analysis_directory = SharedAnalysisDirectory(
            project_root="project",
            source_directories=[],
            targets=["target1"],
            search_path=["baz$hello"],
        )

        shared_analysis_directory._symbolic_links = {
            "project/foo1.py": "scratch/bar/foo1.py"
        }
        shared_analysis_directory._last_singly_deleted_path_and_link = (
            "project/foo2.py",
            "scratch/bar/foo2.py",
        )
        actual = shared_analysis_directory._process_updated_files(["project/foo2.py"])
        expected = UpdatedPaths(updated_paths=["scratch/bar/foo2.py"], deleted_paths=[])
        self.assertEqual(actual, expected)
        query_buck_relative_paths.assert_not_called()
        self.assertEqual(
            shared_analysis_directory._symbolic_links,
            {
                "project/foo1.py": "scratch/bar/foo1.py",
                "project/foo2.py": "scratch/bar/foo2.py",
            },
        )

        add_symbolic_link.assert_called_once_with(
            "scratch/bar/foo2.py", "project/foo2.py"
        )
        rebuild.assert_not_called()
        self.assertIsNone(shared_analysis_directory._last_singly_deleted_path_and_link)

    @patch.object(analysis_directory, "_delete_symbolic_link")
    @patch.object(SharedAnalysisDirectory, "get_root", return_value="scratch")
    @patch.object(SharedAnalysisDirectory, "rebuild")
    @patch.object(SharedAnalysisDirectory, "should_rebuild", return_value=False)
    @patch.object(os, "getcwd", return_value="project")
    @patch.object(os.path, "isfile")
    # pyre-fixme[56]: Argument `os.path` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(os.path, "abspath", side_effect=lambda path: path)
    def test_process_updated_files_one_deleted_file(
        self,
        abspath: MagicMock,
        isfile: MagicMock,
        getcwd: MagicMock,
        should_rebuild: MagicMock,
        rebuild: MagicMock,
        get_root: MagicMock,
        delete_symbolic_link: MagicMock,
    ) -> None:
        shared_analysis_directory = SharedAnalysisDirectory(
            project_root="project",
            source_directories=[],
            targets=["target1"],
            search_path=["baz$hello"],
        )
        isfile.side_effect = lambda path: path != "project/deleted.py"
        shared_analysis_directory._symbolic_links = {
            "project/deleted.py": "scratch/bar/deleted.py"
        }
        actual = shared_analysis_directory._process_updated_files(
            ["project/deleted.py"]
        )
        expected = UpdatedPaths(
            updated_paths=["scratch/bar/deleted.py"],
            deleted_paths=["scratch/bar/deleted.py"],
        )
        self.assertEqual(actual, expected)
        self.assertEqual(shared_analysis_directory._symbolic_links, {})
        rebuild.assert_not_called()
        delete_symbolic_link.assert_called_once_with("scratch/bar/deleted.py")
        self.assertEqual(
            shared_analysis_directory._last_singly_deleted_path_and_link,
            ("project/deleted.py", "scratch/bar/deleted.py"),
        )

    @patch.object(SharedAnalysisDirectory, "should_rebuild", return_value=True)
    @patch.object(SharedAnalysisDirectory, "_get_new_deleted_and_tracked_paths")
    @patch.object(SharedAnalysisDirectory, "_process_rebuilt_files")
    def test_process_updated_files__rebuild(
        self,
        process_rebuilt_files: MagicMock,
        get_new_deleted_and_tracked_paths: MagicMock,
        should_rebuild: MagicMock,
    ) -> None:
        shared_analysis_directory = SharedAnalysisDirectory(
            project_root="project",
            source_directories=[],
            targets=["target1"],
            search_path=["baz$hello"],
        )
        shared_analysis_directory._symbolic_links = {
            "project/tracked.py": "scratch/bar/tracked.py"
        }

        previously_tracked = [
            "project/tracked.py",
            "baz/hello/new_file_tracked_because_of_search_path.py",
        ]
        get_new_deleted_and_tracked_paths.return_value = (
            ["project/something/new_file_not_tracked.py"],
            previously_tracked,
            [],
        )

        new_paths_from_the_rebuild = ["scratch/new_file_from_rebuild.py"]
        deleted_scratch_paths_from_the_rebuild = ["scratch/deleted_by_rebuild.py"]
        process_rebuilt_files.return_value = (
            previously_tracked + new_paths_from_the_rebuild,
            deleted_scratch_paths_from_the_rebuild,
        )

        actual = shared_analysis_directory._process_updated_files(
            previously_tracked
            + [
                "other_project/not_tracked.py",
                "project/something/new_file_not_tracked.py",
            ]
        )
        expected = UpdatedPaths(
            updated_paths=[
                # Tracked files.
                "scratch/bar/tracked.py",
                "baz/hello/new_file_tracked_because_of_search_path.py",
                # New file from the rebuild.
                "scratch/new_file_from_rebuild.py",
                "scratch/deleted_by_rebuild.py",
            ],
            deleted_paths=["scratch/deleted_by_rebuild.py"],
        )
        self.assertEqual(actual, expected)
        self.assertIsNone(shared_analysis_directory._last_singly_deleted_path_and_link)

    @patch.object(analysis_directory, "time")
    @patch.object(os.path, "getmtime")
    # pyre-fixme[56]: Argument `os.path` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(buck, "clear_buck_query_cache")
    @patch.object(SharedAnalysisDirectory, "_notify_about_rebuild")
    @patch.object(SharedAnalysisDirectory, "rebuild")
    def test_process_rebuilt_files(
        self,
        rebuild: MagicMock,
        notify_about_rebuild: MagicMock,
        clear_buck_query_cache: MagicMock,
        get_modified_time: MagicMock,
        time: MagicMock,
    ) -> None:
        shared_analysis_directory: SharedAnalysisDirectory = SharedAnalysisDirectory(
            project_root="project",
            source_directories=[],
            targets=["target1"],
            search_path=["baz$hello"],
        )

        def _update_paths_for_rebuild() -> None:
            shared_analysis_directory._symbolic_links = {
                "project/tracked.py": "scratch/tracked.py",
                "project/updated_by_thrift.pyi": "scratch/updated_by_thrift.pyi",
                "project/something/new_file_from_rebuild.py": (
                    "scratch/new_file_from_rebuild.py"
                ),
            }

        rebuild.side_effect = _update_paths_for_rebuild

        TIME_VALUE: float = 42
        time.return_value = TIME_VALUE

        def _get_modified_time(filename: str) -> float:
            if filename == "scratch/updated_by_thrift.pyi":
                return TIME_VALUE + 1
            return TIME_VALUE - 1

        get_modified_time.side_effect = _get_modified_time

        shared_analysis_directory._symbolic_links = {
            "project/tracked.py": "scratch/tracked.py",
            "project/updated_by_thrift.pyi": "scratch/updated_by_thrift.pyi",
            "project/deleted_by_rebuild.py": "scratch/deleted_by_rebuild.py",
        }
        actual = shared_analysis_directory._process_rebuilt_files(
            [
                "project/tracked.py",
                "baz/hello/new_file_tracked_because_of_search_path.py",
            ],
            [],
            [],
        )
        expected = (
            [
                # Tracked files.
                "project/tracked.py",
                "baz/hello/new_file_tracked_because_of_search_path.py",
                # New file from the rebuild.
                "project/something/new_file_from_rebuild.py",
                "project/updated_by_thrift.pyi",
            ],
            ["scratch/deleted_by_rebuild.py"],
        )
        self.assertEqual(actual, expected)
        clear_buck_query_cache.assert_called_once()

    @patch.object(SharedAnalysisDirectory, "get_root", return_value="/scratch/foo")
    # pyre-ignore[56]: Argument `tools.pyre.client.analysis_directory` to
    # decorator factory `unittest.mock.patch.object` could not be resolved in a
    # global scope.
    @patch.object(analysis_directory, "acquire_lock")
    @patch.object(SharedAnalysisDirectory, "_process_updated_files")
    def test_process_updated_files__acquire_lock(
        self,
        private_process_updated_files: MagicMock,
        acquire_lock: MagicMock,
        get_root: MagicMock,
    ) -> None:
        shared_analysis_directory = SharedAnalysisDirectory(
            project_root="/",
            source_directories=[],
            targets=["target1"],
            search_path=["baz$hello"],
        )
        shared_analysis_directory.process_updated_files(["foo.py"])
        acquire_lock.assert_called_once_with(
            "/scratch/foo/analysis_directory_reader_writer.lock",
            blocking=True,
            is_shared_reader=False,
        )
        private_process_updated_files.assert_called_once()

    def test_cache_last_deleted_link(self) -> None:
        shared_analysis_directory = SharedAnalysisDirectory(
            project_root="/",
            source_directories=[],
            targets=["target1"],
            search_path=["baz$hello"],
        )
        self.assertIsNone(shared_analysis_directory._last_singly_deleted_path_and_link)
        shared_analysis_directory._cache_last_deleted_link(
            ["foo.py"], ["scratch/foo.py"], ["scratch/foo.py", "scratch/bar.py"]
        )
        self.assertIsNone(shared_analysis_directory._last_singly_deleted_path_and_link)
        shared_analysis_directory._cache_last_deleted_link(
            [], [], ["scratch/foo.py", "scratch/bar.py"]
        )
        self.assertIsNone(shared_analysis_directory._last_singly_deleted_path_and_link)

        # Note: The deleted path won't be in the symbolic links map.
        shared_analysis_directory._symbolic_links = {}
        shared_analysis_directory._cache_last_deleted_link(
            ["foo.py"], ["scratch/foo.py"], ["scratch/foo.py"]
        )
        self.assertEqual(
            shared_analysis_directory._last_singly_deleted_path_and_link,
            ("foo.py", "scratch/foo.py"),
        )

    def test_fetch_cached_absolute_link_map(self) -> None:
        shared_analysis_directory = SharedAnalysisDirectory(
            project_root="/",
            source_directories=[],
            targets=["target1"],
            search_path=["baz$hello"],
        )
        self.assertIsNone(shared_analysis_directory._last_singly_deleted_path_and_link)
        self.assertIsNone(
            shared_analysis_directory._fetch_cached_absolute_link_map(["foo.py"], [])
        )

        shared_analysis_directory._last_singly_deleted_path_and_link = (
            "foo.py",
            "scratch/foo.py",
        )
        self.assertEqual(
            shared_analysis_directory._fetch_cached_absolute_link_map(["foo.py"], []),
            {"foo.py": "scratch/foo.py"},
        )
        self.assertIsNone(
            shared_analysis_directory._fetch_cached_absolute_link_map(
                ["foo.py", "bar.py"], []
            )
        )

    def test_get_buck_builder__simple_buck_builder(self) -> None:
        actual = _get_buck_builder(
            configuration=configuration_module.Configuration.from_partial_configuration(
                project_root=Path("root"),
                relative_local_root=None,
                partial_configuration=configuration_module.PartialConfiguration(
                    buck_mode=None, use_buck_builder=False
                ),
            ),
            relative_local_root=None,
            isolate=False,
        )
        self.assertEqual(actual, (buck.SimpleBuckBuilder(), []))

    @patch.object(tempfile, "mkdtemp")
    @patch.object(analysis_directory, "find_buck_root")
    # pyre-fixme[56]: Pyre was not able to infer the type of argument
    #  `tools.pyre.client.analysis_directory` to decorator factory
    #  `unittest.mock.patch.object`.
    @patch.object(analysis_directory, "_get_project_name")
    def test_get_buck_builder__fast_buck_builder(
        self,
        get_project_name: MagicMock,
        find_buck_root: MagicMock,
        make_temporary_directory: MagicMock,
    ) -> None:
        configuration = configuration_module.Configuration.from_partial_configuration(
            project_root=Path("root"),
            relative_local_root="local",
            partial_configuration=configuration_module.PartialConfiguration(
                use_buck_builder=True, use_buck_source_database=False
            ),
        )
        actual = _get_buck_builder(
            configuration=configuration,
            relative_local_root=None,
            isolate=False,
        )
        self.assertEqual(
            actual,
            (
                buck.FastBuckBuilder(
                    buck_root=find_buck_root(),
                    buck_builder_binary=configuration.buck_builder_binary,
                    buck_mode=None,
                    project_name=get_project_name(),
                    output_directory=make_temporary_directory(),
                    isolation_prefix=None,
                ),
                [make_temporary_directory()],
            ),
        )

    @patch.object(tempfile, "mkdtemp")
    # pyre-fixme[56]: Pyre was not able to infer the type of argument
    #  `tools.pyre.client.analysis_directory` to decorator factory
    #  `unittest.mock.patch.object`.
    @patch.object(analysis_directory, "find_buck_root")
    def test_get_buck_builder__source_database_buck_builder(
        self, find_buck_root: MagicMock, make_temporary_directory: MagicMock
    ) -> None:
        configuration = configuration_module.Configuration.from_partial_configuration(
            project_root=Path("root"),
            relative_local_root="local",
            partial_configuration=configuration_module.PartialConfiguration(
                use_buck_builder=True, use_buck_source_database=True
            ),
        )
        actual = _get_buck_builder(
            configuration=configuration,
            relative_local_root=None,
            isolate=False,
        )
        self.assertEqual(
            actual,
            (
                buck.SourceDatabaseBuckBuilder(
                    buck_root=find_buck_root(),
                    buck_mode=None,
                    output_directory=make_temporary_directory(),
                    isolation_prefix=None,
                ),
                [make_temporary_directory()],
            ),
        )

    @patch.object(
        buck,
        "generate_source_directories",
        side_effect=lambda targets, build, prompt: targets,
    )
    # pyre-fixme[56]: Argument `os.path` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(os.path, "relpath", side_effect=lambda path, relative: path)
    @patch(f"{analysis_directory_name}.find_buck_root", return_value="/buck_root")
    def test_resolve_analysis_directory(
        self, find_buck_root, relpath, buck  # pyre-fixme[2]
    ) -> None:
        original_directory = "/project"
        project_root = "/project"

        configuration = MagicMock()
        configuration.source_directories = []
        configuration.targets = []
        configuration.project_root = project_root
        configuration.local_root = None
        configuration.buck_mode = None

        expected_analysis_directory = SharedAnalysisDirectory(
            [],
            ["//x:y"],
            project_root=project_root,
            original_directory="/project",
            filter_paths={"/real/directory"},
        )
        analysis_directory = resolve_analysis_directory(
            source_directories=[],
            targets=["//x:y"],
            configuration=configuration,
            original_directory=original_directory,
            filter_directory="/real/directory",
        )
        self.assertEqualRootAndFilterRoot(
            analysis_directory, expected_analysis_directory
        )

        expected_analysis_directory = SharedAnalysisDirectory(
            [configuration_module.SimpleSearchPathElement("a/b")],
            ["//x:y", "//y:/..."],
            project_root=project_root,
            original_directory="/project",
            filter_paths={"/filter"},
        )
        analysis_directory = resolve_analysis_directory(
            source_directories=["a/b"],
            targets=["//overridden/..."],
            configuration=configuration,
            original_directory=original_directory,
            filter_directory="/filter",
        )
        self.assertEqualRootAndFilterRoot(
            analysis_directory, expected_analysis_directory
        )

        configuration.source_directories = []
        configuration.targets = ["//not:overridden/..."]
        expected_analysis_directory = SharedAnalysisDirectory(
            [],
            ["//not:overridden/..."],
            project_root=project_root,
            original_directory="/project",
            filter_paths={"/filter"},
        )
        analysis_directory = resolve_analysis_directory(
            source_directories=[],
            targets=[],
            configuration=configuration,
            original_directory=original_directory,
            filter_directory="/filter",
        )
        self.assertEqualRootAndFilterRoot(
            analysis_directory, expected_analysis_directory
        )

        configuration.source_directories = []
        configuration.targets = ["//a/b/..."]
        configuration.local_root = "a"
        expected_analysis_directory = SharedAnalysisDirectory(
            [],
            ["//a/b/..."],
            project_root=project_root,
            local_configuration_root="a",
            original_directory="/project",
            filter_paths={"a"},
        )
        analysis_directory = resolve_analysis_directory(
            source_directories=[],
            targets=[],
            configuration=configuration,
            original_directory=original_directory,
            filter_directory=None,
        )
        self.assertEqualRootAndFilterRoot(
            analysis_directory, expected_analysis_directory
        )

    def test_merge_into_paths(self) -> None:
        directory: str = tempfile.mkdtemp()
        root = os.path.realpath(directory)

        Path(root, "a.py").touch()
        Path(root, "b.pyi").touch()
        Path(root, "c.cpp").touch()
        Path(root, "link1.py").symlink_to(Path(root, "a.py"))
        Path(root, "link2.py").symlink_to(Path(root, "dangling.py"))
        Path(root, "link3.py").symlink_to(Path(root, "c.cpp"))
        Path(root, "link4.cpp").symlink_to(Path(root, "a.py"))
        os.mkdir(os.path.join(root, "mypy"))
        os.mkdir(os.path.join(root, "scipyi"))
        os.mkdir(os.path.join(root, "spy.py"))
        Path(root, "directory_symlink.py").symlink_to(Path(root, "spy.py"))
        Path(root, "mypy/my.py").touch()
        Path(root, "scipyi/sci.pyi").touch()
        Path(root, "mypy/another.pyi").symlink_to(Path(root, "mypy/my.py"))
        Path(root, "scipyi/another.py").symlink_to(Path(root, "scipyi/sci.pyi"))
        shared_analysis_directory = SharedAnalysisDirectory(
            [configuration_module.SimpleSearchPathElement(root)], [], project_root=root
        )
        all_paths: Dict[str, str] = {}
        shared_analysis_directory._merge_into_paths(root, all_paths)
        self.assertEqual(
            all_paths,
            {
                "a.py": os.path.join(root, "a.py"),
                "b.pyi": os.path.join(root, "b.pyi"),
                "link1.py": os.path.join(root, "a.py"),
                "link3.py": os.path.join(root, "c.cpp"),
                "mypy/another.pyi": os.path.join(root, "mypy/my.py"),
                "mypy/my.py": os.path.join(root, "mypy/my.py"),
                "scipyi/another.py": os.path.join(root, "scipyi/sci.pyi"),
                "scipyi/sci.pyi": os.path.join(root, "scipyi/sci.pyi"),
            },
        )

    @patch.object(filesystem, "is_empty", return_value=False)
    @patch.object(os, "symlink")
    @patch.object(subprocess, "check_output")
    # pyre-fixme[56]: Pyre was not able to infer the type of argument `os` to
    #  decorator factory `unittest.mock.patch.object`.
    @patch.object(os, "makedirs")
    @patch.object(os.path, "exists")
    @patch.object(os.path, "realpath")
    def test_merge(
        self,
        os_path_realpath: MagicMock,
        os_path_exists: MagicMock,
        os_makedirs: MagicMock,
        check_output: MagicMock,
        os_symlink: MagicMock,
        is_empty: MagicMock,
    ) -> None:
        os_path_exists.return_value = False
        root = tempfile.mkdtemp()
        os.mkdir(os.path.join(root, "first"))
        os.mkdir(os.path.join(root, "first", "b"))
        os.mkdir(os.path.join(root, "second"))

        Path(root, "first", "x.py").touch()
        Path(root, "first", "y.py").touch()
        Path(root, "first", "b", "z.py").touch()
        Path(root, "second", "a.py").touch()

        # pyre-fixme[53]: Captured variable `root` is not annotated.
        def side_effect(path: str, stderr=None) -> bytes:  # pyre-fixme[2]
            if path[1].endswith("first"):
                serialized = "\n".join(
                    [
                        os.path.join(root, path)
                        for path in ["first/x.py", "first/y.py", "first/b/z.py"]
                    ]
                )
            else:
                serialized = os.path.join(root, "second/a.py")
            return bytes(serialized, "utf-8")

        check_output.side_effect = side_effect
        os_path_realpath.side_effect = lambda x: x
        shared_analysis_directory = SharedAnalysisDirectory(
            [
                configuration_module.SimpleSearchPathElement(
                    os.path.join(root, "first")
                ),
                configuration_module.SimpleSearchPathElement(
                    os.path.join(root, "second")
                ),
            ],
            [],
            project_root=root,
        )
        shared_analysis_directory._merge()
        shared_root = shared_analysis_directory.get_root()
        os_makedirs.assert_has_calls(
            [call(shared_root), call(shared_root + "/b")], any_order=True
        )
        os_symlink.assert_has_calls(
            [
                call(root + "/first/x.py", shared_root + "/x.py"),
                call(root + "/first/y.py", shared_root + "/y.py"),
                call(root + "/first/b/z.py", shared_root + "/b/z.py"),
                call(root + "/second/a.py", shared_root + "/a.py"),
            ],
            any_order=True,
        )

    def test_prepare(self) -> None:
        buck_output_directory: str = tempfile.mkdtemp()
        project_directory: str = tempfile.mkdtemp()
        original_scratch_directory: str = tempfile.mkdtemp()

        with patch.object(buck.FastBuckBuilder, "build") as build, patch.object(
            SharedAnalysisDirectory, "get_root", return_value=original_scratch_directory
        ):
            fast_buck_builder = buck.FastBuckBuilder(
                buck_root="dummy_buck_root",
                output_directory="/tmp/foo",
                isolation_prefix=None,
            )
            shared_analysis_directory = SharedAnalysisDirectory(
                project_root=project_directory,
                source_directories=[],
                targets=["target1"],
                buck_builder=fast_buck_builder,
            )

            Path(project_directory, "existing.py").touch()

            def build_buck_directory(
                argument: buck.BuckBuilder,
            ) -> buck.BuckBuildOutput:
                Path(buck_output_directory, "existing.py").symlink_to(
                    Path(Path(project_directory, "existing.py"))
                )
                return buck.BuckBuildOutput([buck_output_directory], [])

            build.side_effect = build_buck_directory

            Path(original_scratch_directory, "obsolete.py").touch()

            shared_analysis_directory.prepare()

            self.assertFalse(
                os.path.lexists(os.path.join(original_scratch_directory, "obsolete.py"))
            )

            self.assertFileIsLinkedBothWays(
                "existing.py",
                shared_analysis_directory,
                original_scratch_directory,
                project_directory,
            )

    def test_rebuild(self) -> None:
        buck_output_directory: str = tempfile.mkdtemp("_buck_output")
        project_directory: str = tempfile.mkdtemp("_project")
        original_scratch_directory: str = tempfile.mkdtemp("_original_scratch")

        with patch.object(buck.FastBuckBuilder, "build") as build, patch.object(
            SharedAnalysisDirectory, "get_root", return_value=original_scratch_directory
        ):
            fast_buck_builder = buck.FastBuckBuilder(
                buck_root="dummy_buck_root",
                output_directory="/tmp/foo",
                isolation_prefix=None,
            )
            shared_analysis_directory = SharedAnalysisDirectory(
                project_root=project_directory,
                source_directories=[],
                targets=["target1"],
                buck_builder=fast_buck_builder,
            )

            Path(project_directory, "existing.py").touch()
            Path(project_directory, "to_be_deleted.py").touch()

            def build_buck_directory(
                argument: buck.BuckBuilder,
            ) -> buck.BuckBuildOutput:
                Path(buck_output_directory, "existing.py").symlink_to(
                    Path(project_directory, "existing.py")
                )
                Path(buck_output_directory, "to_be_deleted.py").symlink_to(
                    Path(project_directory, "to_be_deleted.py")
                )
                return buck.BuckBuildOutput([buck_output_directory], [])

            build.side_effect = build_buck_directory

            Path(
                original_scratch_directory, "obsolete_file_in_scratch_directory.py"
            ).touch()

            shared_analysis_directory.prepare()

            # Update the project directory.
            Path(project_directory, "new_file.py").touch()
            os.remove(os.path.join(project_directory, "to_be_deleted.py"))

            def update_buck_directory() -> None:
                os.remove(os.path.join(buck_output_directory, "to_be_deleted.py"))
                Path(buck_output_directory, "new_file.py").symlink_to(
                    Path(project_directory, "new_file.py")
                )

            build.side_effect = update_buck_directory()

            shared_analysis_directory.rebuild()

            self.assertFileIsLinkedBothWays(
                "existing.py",
                shared_analysis_directory,
                original_scratch_directory,
                project_directory,
            )
            self.assertFileIsLinkedBothWays(
                "new_file.py",
                shared_analysis_directory,
                original_scratch_directory,
                project_directory,
            )

            self.assertFalse(
                os.path.lexists(
                    os.path.join(original_scratch_directory, "to_be_deleted.py")
                )
            )
            self.assertNotIn(
                os.path.join(project_directory, "to_be_deleted.py"),
                shared_analysis_directory._symbolic_links,
            )

    # pyre-fixme[56]: Argument `tools.pyre.client.analysis_directory` to decorator
    #  factory `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(analysis_directory, "SocketConnection")
    def test_notify_about_rebuild(self, socket_connection_class: MagicMock) -> None:
        fast_buck_builder = buck.FastBuckBuilder(
            buck_root="dummy_buck_root",
            output_directory="/tmp/foo",
            isolation_prefix=None,
        )
        shared_analysis_directory = SharedAnalysisDirectory(
            source_directories=[],
            targets=["target1"],
            buck_builder=fast_buck_builder,
            project_root="/",
        )
        shared_analysis_directory._configuration = None
        shared_analysis_directory._notify_about_rebuild(is_start_message=True)
        socket_connection_class.assert_not_called()

        shared_analysis_directory._configuration = MagicMock()
        shared_analysis_directory._notify_about_rebuild(is_start_message=True)
        socket_connection_class.assert_called_once()

    def test_resolve_filter_paths(self) -> None:
        original_directory = "/project"
        configuration = configuration_module.Configuration(
            project_root="/project",
            dot_pyre_directory=Path("irrlevant"),
            relative_local_root=None,
        )

        filter_paths = _resolve_filter_paths(
            source_directories=[],
            targets=[],
            configuration=configuration,
            original_directory=original_directory,
        )
        self.assertEqual(filter_paths, set())

        filter_paths = _resolve_filter_paths(
            source_directories=["/project/a"],
            targets=[],
            configuration=configuration,
            original_directory=original_directory,
        )
        self.assertEqual(filter_paths, {"/project/a"})

        filter_paths = _resolve_filter_paths(
            source_directories=["/project/a"],
            targets=["//x/y/..."],
            configuration=configuration,
            original_directory=original_directory,
        )
        self.assertEqual(filter_paths, {"/project/a", "x/y"})

        configuration = configuration_module.Configuration(
            project_root="/project",
            dot_pyre_directory=Path("irrlevant"),
            relative_local_root="local",
        )
        filter_paths = _resolve_filter_paths(
            source_directories=["/project/local/a"],
            targets=["//x/y:z"],
            configuration=configuration,
            original_directory=original_directory,
        )
        self.assertEqual(filter_paths, {"/project/local/a", "x/y"})

        filter_paths = _resolve_filter_paths(
            source_directories=[],
            targets=[],
            configuration=configuration,
            original_directory=original_directory,
        )
        self.assertEqual(filter_paths, {"/project/local"})

    # pyre-fixme[56]: Argument `os` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(os, "getpid", return_value=1234)
    def test_get_project_name(self, get_process_id: MagicMock) -> None:
        self.assertEqual(
            _get_project_name(isolate_per_process=True, relative_local_root=None),
            "isolated_1234",
        )
        self.assertEqual(
            _get_project_name(isolate_per_process=False, relative_local_root=None), None
        )
        self.assertEqual(
            _get_project_name(
                isolate_per_process=False, relative_local_root="foo/bar/baz"
            ),
            "foo_bar_baz",
        )

    @patch.object(
        SharedAnalysisDirectory,
        "_directories_to_clean_up",
        return_value=["foo", "bar/baz"],
    )
    # pyre-fixme[56]: Argument `shutil` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(shutil, "rmtree")
    def test_cleanup(
        self, remove_tree: MagicMock, directories_to_clean_up: MagicMock
    ) -> None:
        shared_analysis_directory = SharedAnalysisDirectory(
            source_directories=[], targets=["target1"], project_root="/"
        )
        shared_analysis_directory.cleanup(delete_long_lasting_files=True)
        remove_tree.assert_has_calls([call("foo"), call("bar/baz")])

    @patch.object(
        SharedAnalysisDirectory,
        "_directories_to_clean_up",
        return_value=["foo", "bar/baz"],
    )
    # pyre-fixme[56]: Argument `shutil` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(shutil, "rmtree", side_effect=Exception)
    def test_cleanup__exception(
        self, remove_tree: MagicMock, directories_to_clean_up: MagicMock
    ) -> None:
        shared_analysis_directory = SharedAnalysisDirectory(
            source_directories=[], targets=["target1"], project_root="/"
        )
        shared_analysis_directory.cleanup(delete_long_lasting_files=True)

    @patch.object(tempfile, "mkdtemp")
    # pyre-fixme[56]: Argument `os` to decorator factory
    #  `unittest.mock.patch.object` could not be resolved in a global scope.
    @patch.object(os, "getpid", return_value=42)
    @patch.object(
        SharedAnalysisDirectory, "get_scratch_directory", return_value="/scratch"
    )
    def test_directories_to_clean_up(
        self,
        get_root: MagicMock,
        getpid: MagicMock,
        make_temporary_directory: MagicMock,
    ) -> None:
        shared_analysis_directory = SharedAnalysisDirectory(
            source_directories=[], targets=["target1"], project_root="/", isolate=False
        )
        self.assertEqual(
            shared_analysis_directory._directories_to_clean_up(
                delete_long_lasting_files=False
            ),
            [],
        )

        shared_analysis_directory = SharedAnalysisDirectory(
            source_directories=[],
            targets=["target1"],
            project_root="/",
            isolate=True,
            buck_builder=buck.SimpleBuckBuilder(),
        )
        self.assertEqual(
            shared_analysis_directory._directories_to_clean_up(
                delete_long_lasting_files=True
            ),
            ["/scratch/shared_analysis_directory_42"],
        )

        shared_analysis_directory = SharedAnalysisDirectory(
            source_directories=[],
            targets=["target1"],
            project_root="/",
            isolate=True,
            buck_builder=buck.FastBuckBuilder(
                buck_root="dummy_buck_root",
                output_directory=make_temporary_directory(),
                isolation_prefix=None,
            ),
            temporary_directories=[make_temporary_directory()],
        )
        self.assertEqual(
            shared_analysis_directory._directories_to_clean_up(
                delete_long_lasting_files=True
            ),
            [
                "/scratch/shared_analysis_directory_42",
                "/scratch/.buck_builder_cache_isolated_42",
                make_temporary_directory(),
            ],
        )
