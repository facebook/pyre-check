# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import builtins
import errno
import fcntl
import os
import pathlib  # noqa
import subprocess
import tempfile
import unittest
from contextlib import contextmanager
from unittest.mock import MagicMock, Mock, call, patch

from .. import (
    buck,
    commands,
    filesystem,
    find_directories,
    configuration as configuration_module,
)
from ..analysis_directory import (
    NotWithinLocalConfigurationException,
    SharedAnalysisDirectory,
)
from ..commands.command import __name__ as command_name
from ..filesystem import (
    __name__ as filesystem_name,
    _delete_symbolic_link,
    acquire_lock,
    acquire_lock_if_needed,
    add_symbolic_link,
    expand_relative_path,
    find_python_paths,
    remove_if_exists,
)


class FilesystemTest(unittest.TestCase):
    def test_find_python_paths(self) -> None:
        root = tempfile.mkdtemp()

        # When there are no paths, returns empty list.
        self.assertListEqual(find_python_paths(root), [])

        def create_file(name: str) -> None:
            with open(os.path.join(root, name), "w+"):
                pass

        def create_symlink(target: str, source: str) -> None:
            os.symlink(os.path.join(root, target), os.path.join(root, source))

        create_file("a.py")
        create_file("b.pyi")
        create_file("c.cpp")
        create_symlink("a.py", "link1.py")
        create_symlink("dangling.py", "link2.py")
        create_symlink("c.cpp", "link3.py")
        create_symlink("a.py", "link4.cpp")
        os.mkdir(os.path.join(root, "mypy"))
        os.mkdir(os.path.join(root, "scipyi"))
        os.mkdir(os.path.join(root, "spy.py"))
        create_symlink("spy.py", "directory_symlink.py")
        create_file("mypy/my.py")
        create_file("scipyi/sci.pyi")
        create_symlink("mypy/my.py", "mypy/another.pyi")
        create_symlink("scipyi/sci.pyi", "scipyi/another.py")
        actual_paths = sorted(
            os.path.relpath(path, root) for path in find_python_paths(root)
        )
        self.assertEqual(
            actual_paths,
            [
                "a.py",
                "b.pyi",
                "directory_symlink.py",
                "link1.py",
                "link2.py",
                "link3.py",
                "mypy/another.pyi",
                "mypy/my.py",
                "scipyi/another.py",
                "scipyi/sci.pyi",
            ],
        )

    def test_remove_if_exists(self) -> None:
        # File removal.
        with patch("os.remove") as os_remove, patch("shutil.rmtree") as shutil_rmtree:
            os_remove.side_effect = OSError()
            remove_if_exists("path")
            os_remove.assert_called_once_with("path")
            shutil_rmtree.assert_called_once_with("path")

        # Directory removal.
        with patch("os.remove") as os_remove, patch("shutil.rmtree") as shutil_rmtree:
            shutil_rmtree.side_effect = OSError()
            remove_if_exists("path")
            os_remove.assert_called_once_with("path")
            shutil_rmtree.assert_called_once_with("path")

        # Both throw.
        with patch("os.remove") as os_remove, patch("shutil.rmtree") as shutil_rmtree:
            os_remove.side_effect = FileNotFoundError()
            shutil_rmtree.side_effect = OSError()
            remove_if_exists("path")
            os_remove.assert_called_once_with("path")
            shutil_rmtree.assert_called_once_with("path")

    @patch("fcntl.lockf")
    def test_acquire_lock(self, lock_file: Mock) -> None:
        (_, path) = tempfile.mkstemp()
        lockfile_file_descriptor = None
        with acquire_lock(path, blocking=False) as file_descriptor:
            lockfile_file_descriptor = file_descriptor

        with acquire_lock(path, blocking=True):
            pass
        lock_file.assert_has_calls(
            [
                call(lockfile_file_descriptor, fcntl.LOCK_EX | fcntl.LOCK_NB),
                call(lockfile_file_descriptor, fcntl.LOCK_UN),
                call(lockfile_file_descriptor, fcntl.LOCK_EX),
                call(lockfile_file_descriptor, fcntl.LOCK_UN),
            ]
        )

        def fail_on_exclusive(_, lock_kind):
            if lock_kind == fcntl.LOCK_EX | fcntl.LOCK_NB:
                raise OSError()
            return None

        lock_file.side_effect = fail_on_exclusive
        with self.assertRaises(OSError):
            with acquire_lock(path, blocking=False):
                pass

    @patch.object(builtins, "open")
    @patch.object(fcntl, "lockf")
    def test_acquire_lock__release_even_on_exception(
        self, lock_file: MagicMock, open_file: MagicMock
    ) -> None:
        class SomeException(Exception):
            pass

        with self.assertRaises(SomeException):
            with acquire_lock("foo.txt", blocking=True):
                raise SomeException

        file_descriptor = open_file().__enter__().fileno()
        lock_file.assert_has_calls(
            [call(file_descriptor, fcntl.LOCK_EX), call(file_descriptor, fcntl.LOCK_UN)]
        )

    def test_lock_command(self) -> None:
        self.assertEqual(
            filesystem._lock_command(blocking=True, is_shared_reader=True),
            fcntl.LOCK_SH,
        )
        self.assertEqual(
            filesystem._lock_command(blocking=True, is_shared_reader=False),
            fcntl.LOCK_EX,
        )
        self.assertEqual(
            filesystem._lock_command(blocking=False, is_shared_reader=True),
            fcntl.LOCK_SH | fcntl.LOCK_NB,
        )
        self.assertEqual(
            filesystem._lock_command(blocking=False, is_shared_reader=False),
            fcntl.LOCK_EX | fcntl.LOCK_NB,
        )

    @patch.object(filesystem, "acquire_lock")
    def test_acquire_lock_if_needed(self, acquire_lock: MagicMock) -> None:
        acquire_lock_if_needed("/some/path", blocking=True, needed=True)
        acquire_lock.assert_called_once()

    @patch.object(filesystem, "acquire_lock")
    def test_acquire_lock_if_needed__not_needed(self, acquire_lock: MagicMock) -> None:
        acquire_lock_if_needed("/some/path", blocking=True, needed=False)
        acquire_lock.assert_not_called()

    @patch.object(filesystem, "_compute_symbolic_link_mapping")
    @patch("os.getcwd")
    @patch.object(subprocess, "check_output")
    def test_get_scratch_directory(self, check_output, getcwd, compute_symbolic_links):
        # No scratch, no local configuration
        check_output.side_effect = FileNotFoundError
        getcwd.return_value = "default"
        shared_analysis_directory = SharedAnalysisDirectory(
            ["first", "second"], [], project_root="default"
        )

        directory = shared_analysis_directory.get_scratch_directory()
        self.assertEqual(directory, "default/.pyre")

        root = shared_analysis_directory.get_root()
        self.assertEqual(root, "default/.pyre/shared_analysis_directory")

        # Scratch, no local configuration
        check_output.side_effect = None
        check_output.return_value = "/scratch\n".encode("utf-8")
        shared_analysis_directory = SharedAnalysisDirectory(
            ["first", "second"], [], project_root="default"
        )
        directory = shared_analysis_directory.get_scratch_directory()
        self.assertEqual(directory, "/scratch")

        root = shared_analysis_directory.get_root()
        self.assertEqual(root, "/scratch/shared_analysis_directory")

        # No scratch, using local configuration
        check_output.side_effect = FileNotFoundError
        getcwd.return_value = "default"
        shared_analysis_directory = SharedAnalysisDirectory(
            ["first", "second"],
            [],
            project_root="default",
            filter_paths={"path/to/local"},
            local_configuration_root="path/to/local",
        )

        directory = shared_analysis_directory.get_scratch_directory()
        self.assertEqual(directory, "default/.pyre")

        root = shared_analysis_directory.get_root()
        self.assertEqual(root, "default/.pyre/path/to/local")

        # Scratch, using local configuration
        check_output.side_effect = None
        check_output.return_value = "/scratch\n".encode("utf-8")
        shared_analysis_directory = SharedAnalysisDirectory(
            ["first", "second"],
            [],
            project_root="default",
            filter_paths={"path/to/local"},
            local_configuration_root="path/to/local",
        )
        directory = shared_analysis_directory.get_scratch_directory()
        self.assertEqual(directory, "/scratch")

        root = shared_analysis_directory.get_root()
        self.assertEqual(root, "/scratch/path/to/local")

    @patch.object(tempfile, "mkdtemp", return_value="/tmp/pyre_tmp_xyz")
    @patch.object(buck, "find_buck_root", return_value="/buck_root")
    @patch("os.makedirs")
    @patch(filesystem_name + ".acquire_lock")
    @patch.object(SharedAnalysisDirectory, "get_root", return_value="/analysis_root")
    def test_prepare(
        self,
        get_root,
        acquire_lock,
        makedirs,
        find_parent_directory_containing_file,
        mkdtemp,
    ):
        @contextmanager
        def acquire(*args, **kwargs):
            yield

        with patch.object(SharedAnalysisDirectory, "_clear") as clear, patch.object(
            SharedAnalysisDirectory, "_merge"
        ) as merge:
            shared_analysis_directory = SharedAnalysisDirectory(
                ["first", "second"], [], project_root="/"
            )
            acquire_lock.side_effect = acquire
            shared_analysis_directory.prepare()
            merge.assert_has_calls([call()])
            clear.assert_has_calls([call()])

    @patch(f"{command_name}.Path.mkdir")
    @patch("os.path.realpath", side_effect=lambda path: f"realpath({path})")
    @patch("os.getcwd", return_value="/root")
    @patch("os.path.exists", return_value=True)
    @patch(
        f"{find_directories.__name__}.find_global_and_local_root",
        return_value=find_directories.FoundRoot(pathlib.Path("/root/local")),
    )
    def test_resolve_source_directories(
        self, find_global_and_local_root, exists, cwd, realpath, path_mkdir
    ) -> None:
        arguments = MagicMock()
        arguments.source_directories = []
        arguments.command = commands.Check
        arguments.use_buck_builder = False
        arguments.ignore_unbuilt_dependencies = False
        arguments.local_configuration = None
        arguments.logger = None
        configuration = MagicMock()
        configuration.source_directories = []
        configuration.project_root = "/root"
        configuration.local_root = "/root/local"
        configuration.use_buck_builder = False
        configuration.ignore_unbuilt_dependencies = False

        with self.assertRaises(NotWithinLocalConfigurationException):
            buck_builder = buck.SimpleBuckBuilder()
            analysis_directory = SharedAnalysisDirectory(
                [],
                [],
                project_root="/root",
                original_directory="/root",
                filter_paths=set(),
                buck_builder=buck_builder,
            )
            analysis_directory._resolve_source_directories()

        # Arguments override configuration.
        with patch.object(
            buck, "generate_source_directories", return_value=[]
        ) as buck_source_directories:
            arguments.source_directories = ["arguments_source_directory"]
            configuration.source_directories = ["configuration_source_directory"]

            buck_builder = buck.SimpleBuckBuilder()
            analysis_directory = SharedAnalysisDirectory(
                [configuration_module.SimpleSearchPathElement("some_source_directory")],
                ["configuration_source_directory"],
                project_root="/root",
                original_directory="/root",
                filter_paths=set(),
                buck_builder=buck_builder,
            )
            analysis_directory._resolve_source_directories()
            buck_source_directories.assert_called_with(
                ["configuration_source_directory"]
            )
            self.assertEqual(
                analysis_directory._source_directories,
                {configuration_module.SimpleSearchPathElement("some_source_directory")},
            )

        with patch.object(
            buck, "generate_source_directories", return_value=["arguments_target"]
        ) as buck_source_directories:
            cwd.return_value = "/"
            original_directory = "/root"
            arguments.source_directories = []
            arguments.targets = ["arguments_target"]
            configuration.source_directories = ["configuration_source_directory"]

            command = commands.Check(
                arguments, original_directory, configuration=configuration
            )
            analysis_directory = command._analysis_directory
            assert isinstance(analysis_directory, SharedAnalysisDirectory)
            analysis_directory._resolve_source_directories()
            buck_source_directories.assert_called_with(["arguments_target"])
            self.assertEqual(
                analysis_directory._source_directories,
                {
                    configuration_module.SimpleSearchPathElement(
                        "realpath(root/arguments_target)"
                    )
                },
            )

        with patch.object(
            buck, "generate_source_directories", return_value=["arguments_target"]
        ) as buck_source_directories:
            # same test as above, but Start instead of Check; build should be False
            cwd.return_value = "/"
            original_directory = "/root"
            command = commands.Start(
                arguments,
                original_directory,
                terminal=False,
                store_type_check_resolution=False,
                use_watchman=True,
                incremental_style=commands.command.IncrementalStyle.FINE_GRAINED,
                configuration=configuration,
            )
            analysis_directory = command._analysis_directory
            assert isinstance(analysis_directory, SharedAnalysisDirectory)
            analysis_directory._resolve_source_directories()
            buck_source_directories.assert_called_with(["arguments_target"])
            self.assertEqual(
                analysis_directory._source_directories,
                {
                    configuration_module.SimpleSearchPathElement(
                        "realpath(root/arguments_target)"
                    )
                },
            )

        # Restart and start always rebuild buck targets
        with patch.object(
            buck, "generate_source_directories", return_value=["arguments_target"]
        ) as buck_source_directories:
            cwd.side_effect = ["/", "/", "/"]
            original_directory = "/root"
            command = commands.Start(
                arguments,
                original_directory,
                terminal=False,
                store_type_check_resolution=False,
                use_watchman=True,
                incremental_style=commands.command.IncrementalStyle.FINE_GRAINED,
                configuration=configuration,
            )
            analysis_directory = command._analysis_directory
            assert isinstance(analysis_directory, SharedAnalysisDirectory)
            analysis_directory._resolve_source_directories()
            buck_source_directories.assert_called_with(["arguments_target"])
            command = commands.Restart(
                arguments,
                original_directory,
                configuration=configuration,
                terminal=False,
                incremental_style=commands.command.IncrementalStyle.FINE_GRAINED,
                use_watchman=True,
                store_type_check_resolution=False,
            )
            analysis_directory = command._analysis_directory
            assert isinstance(analysis_directory, SharedAnalysisDirectory)
            analysis_directory._resolve_source_directories()
            buck_source_directories.assert_called_with(["arguments_target"])

        # Configuration is picked up when no arguments provided.
        with patch.object(
            buck,
            "generate_source_directories",
            return_value=["configuration_source_directory"],
        ) as buck_source_directories:
            cwd.return_value = "/"
            original_directory = "/root"
            arguments.source_directories = []
            arguments.targets = []
            arguments.command = commands.Check
            configuration.targets = ["configuration_target"]
            configuration.source_directories = []

            command = commands.Check(
                arguments, original_directory, configuration=configuration
            )
            analysis_directory = command._analysis_directory
            assert isinstance(analysis_directory, SharedAnalysisDirectory)
            analysis_directory._resolve_source_directories()

            buck_source_directories.assert_called_with(["configuration_target"])
            self.assertEqual(
                analysis_directory._source_directories,
                {
                    configuration_module.SimpleSearchPathElement(
                        "realpath(root/configuration_source_directory)"
                    )
                },
            )

        # Files are translated relative to project root
        with patch.object(
            buck, "generate_source_directories", return_value=["."]
        ) as buck_source_directories:
            cwd.side_effect = ["/", "/"]
            original_directory = "/root"
            arguments.source_directories = []
            arguments.targets = []
            configuration.targets = ["."]

            command = commands.Check(
                arguments, original_directory, configuration=configuration
            )
            analysis_directory = command._analysis_directory
            assert isinstance(analysis_directory, SharedAnalysisDirectory)
            analysis_directory._resolve_source_directories()

            self.assertEqual(
                analysis_directory._source_directories,
                {configuration_module.SimpleSearchPathElement("realpath(root/.)")},
            )

    @patch("os.unlink")
    def test_delete_symbolic_link(self, unlink):
        # delete succeeds
        unlink.return_value = None
        _delete_symbolic_link("exists")
        unlink.assert_called_once_with("exists")

        # delete fails
        unlink.reset_mock()
        unlink.side_effect = OSError
        self.assertRaises(OSError, _delete_symbolic_link, "exception_occurs")
        unlink.assert_called_once_with("exception_occurs")

    @patch("os.unlink")
    @patch("os.symlink")
    @patch("os.makedirs")
    def test_add_symbolic_link(self, makedirs, symlink, unlink):
        add_symbolic_link("/a/link", "file.py")
        # standard use-cases
        makedirs.assert_called_once_with("/a")
        symlink.assert_called_once_with("file.py", "/a/link")

        symlink.reset_mock()
        makedirs.reset_mock()
        add_symbolic_link("/a/b/c/d/link", "file.py")
        makedirs.assert_called_once_with("/a/b/c/d")
        symlink.assert_called_once_with("file.py", "/a/b/c/d/link")

        # symlink exists
        symlink.reset_mock()
        makedirs.reset_mock()
        error = OSError()
        error.errno = errno.EEXIST
        symlink.side_effect = [error, None]
        add_symbolic_link("/a/b/link", "file.py")
        makedirs.assert_called_once_with("/a/b")
        symlink.assert_called_with("file.py", "/a/b/link")
        unlink.assert_called_once_with("/a/b/link")

        # symlink fails
        symlink.reset_mock()
        makedirs.reset_mock()
        unlink.reset_mock()
        symlink.side_effect = OSError()
        add_symbolic_link("/a/link", "file.py")
        makedirs.assert_called_once_with("/a")
        symlink.assert_called_once_with("file.py", "/a/link")
        unlink.assert_not_called()

    @patch.object(filesystem, "find_paths_with_extensions")
    @patch.object(
        os.path,
        "realpath",
        side_effect=lambda path: path.replace("ANALYSIS_ROOT", "LOCAL_ROOT"),
    )
    def test_compute_symbolic_link_mapping(self, realpath, find_paths_with_extensions):
        find_paths_with_extensions.return_value = [
            "ANALYSIS_ROOT/a.py",
            "ANALYSIS_ROOT/b.thrift",
            "ANALYSIS_ROOT/subX/d.pyi",
            "ANALYSIS_ROOT/subX/e.py",
            "ANALYSIS_ROOT/subY/subZ/g.pyi",
        ]

        self.assertDictEqual(
            filesystem._compute_symbolic_link_mapping(
                "ANALYSIS_ROOT", ["py", "pyi", "thrift"]
            ),
            {
                "LOCAL_ROOT/a.py": "ANALYSIS_ROOT/a.py",
                "LOCAL_ROOT/b.thrift": "ANALYSIS_ROOT/b.thrift",
                "LOCAL_ROOT/subX/d.pyi": "ANALYSIS_ROOT/subX/d.pyi",
                "LOCAL_ROOT/subX/e.py": "ANALYSIS_ROOT/subX/e.py",
                "LOCAL_ROOT/subY/subZ/g.pyi": "ANALYSIS_ROOT/subY/subZ/g.pyi",
            },
        )

    def test_expand_relative_path__globs_are_unchanged(self) -> None:
        self.assertEqual(expand_relative_path("foo", "bar/*/baz"), "foo/bar/*/baz")
        self.assertEqual(
            expand_relative_path("dontcare", "/absolute/path/*/foo"),
            "/absolute/path/*/foo",
        )
