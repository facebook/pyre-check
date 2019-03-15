# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import errno
import fcntl
import os
import subprocess
import tempfile
import unittest
from contextlib import contextmanager
from typing import Dict  # noqa
from unittest.mock import MagicMock, Mock, call, patch

from .. import buck, commands, filesystem, resolve_analysis_directory
from ..exceptions import EnvironmentException  # noqa
from ..filesystem import (  # noqa
    AnalysisDirectory,
    Filesystem,
    MercurialBackedFilesystem,
    SharedAnalysisDirectory,
    __name__ as filesystem_name,
    _add_symbolic_link,
    _compute_symbolic_link_mapping,
    _delete_symbolic_link,
    _find_python_paths,
    acquire_lock,
    find_root,
    remove_if_exists,
)


class FilesystemTest(unittest.TestCase):
    def test_find_python_paths(self) -> None:
        root = tempfile.mkdtemp()

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
            os.path.relpath(path, root) for path in _find_python_paths(root)
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

    def test_merge_into_paths(self) -> None:
        root = os.path.realpath(tempfile.mkdtemp())

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
        shared_analysis_directory = SharedAnalysisDirectory([root], [])
        all_paths = {}  # type: Dict[str, str]
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

    @patch("{}.is_empty".format(filesystem_name))
    @patch("os.symlink")
    @patch("subprocess.check_output")
    @patch("os.makedirs")
    @patch("os.path.exists")
    @patch("os.path.realpath")
    def test_merge(
        self,
        os_path_realpath,
        os_path_exists,
        os_makedirs,
        check_output,
        os_symlink,
        is_empty,
    ) -> None:
        is_empty.return_value = False
        os_path_exists.return_value = False
        root = tempfile.mkdtemp()
        os.mkdir(os.path.join(root, "first"))
        os.mkdir(os.path.join(root, "first", "b"))
        os.mkdir(os.path.join(root, "second"))

        def create_file(name: str) -> None:
            with open(name, "w+"):
                pass

        create_file(os.path.join(root, "first", "x.py"))
        create_file(os.path.join(root, "first", "y.py"))
        create_file(os.path.join(root, "first", "b", "z.py"))
        create_file(os.path.join(root, "second", "a.py"))

        def side_effect(path, stderr=None):
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
            [os.path.join(root, "first"), os.path.join(root, "second")], []
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

    @patch("shutil.rmtree")
    def test_cleanup(self, rmtree) -> None:
        shared_analysis_directory = SharedAnalysisDirectory(["first", "second"], [])
        shared_analysis_directory.cleanup()
        rmtree.assert_not_called()

        shared_analysis_directory = SharedAnalysisDirectory(
            ["first", "second"], [], isolate=True
        )
        shared_analysis_directory.cleanup()
        rmtree.assert_called_with(shared_analysis_directory.get_root())

    @patch.object(subprocess, "run")
    def test_filesystem_list_bare(self, run):
        filesystem = Filesystem()
        filesystem.list(".", ".pyre_configuration.local")

        def fail_command(arguments, **kwargs):
            return subprocess.CompletedProcess(
                args=[], returncode=1, stdout="".encode("utf-8")
            )

        run.side_effect = fail_command
        self.assertEqual([], filesystem.list(".", ".pyre_configuration.local"))
        run.assert_has_calls(
            [
                call(
                    ["find", ".", "-name", "*.pyre_configuration.local"],
                    stdout=subprocess.PIPE,
                ),
                call().stdout.decode("utf-8"),
                call().stdout.decode().split(),
                call(
                    ["find", ".", "-name", "*.pyre_configuration.local"],
                    stdout=subprocess.PIPE,
                ),
            ]
        )

    @patch.object(subprocess, "run")
    def test_filesystem_list_mercurial(self, run):
        filesystem = MercurialBackedFilesystem()
        filesystem.list(".", ".pyre_configuration.local")

        def fail_command(arguments, **kwargs):
            return subprocess.CompletedProcess(
                args=[], returncode=1, stdout="".encode("utf-8")
            )

        run.side_effect = fail_command
        self.assertEqual([], filesystem.list(".", ".pyre_configuration.local"))
        run.assert_has_calls(
            [
                call(
                    ["hg", "files", "--include", "**.pyre_configuration.local"],
                    stdout=subprocess.PIPE,
                    stderr=subprocess.DEVNULL,
                ),
                call().stdout.decode("utf-8"),
                call().stdout.decode().split(),
                call(
                    ["hg", "files", "--include", "**.pyre_configuration.local"],
                    stdout=subprocess.PIPE,
                    stderr=subprocess.DEVNULL,
                ),
            ]
        )

    @patch.object(filesystem, "_compute_symbolic_link_mapping")
    @patch("os.getcwd")
    @patch.object(subprocess, "check_output")
    def test_get_scratch_directory(self, check_output, getcwd, compute_symbolic_links):
        # No scratch, no local configuration
        check_output.side_effect = FileNotFoundError
        getcwd.return_value = "default"
        shared_analysis_directory = SharedAnalysisDirectory(["first", "second"], [])

        directory = shared_analysis_directory.get_scratch_directory()
        self.assertEqual(directory, "default/.pyre")

        root = shared_analysis_directory.get_root()
        self.assertEqual(root, "default/.pyre/shared_analysis_directory")

        # Scratch, no local configuration
        check_output.side_effect = None
        check_output.return_value = "/scratch\n".encode("utf-8")
        shared_analysis_directory = SharedAnalysisDirectory(["first", "second"], [])
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
            filter_paths=["path/to/local"],
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
            filter_paths=["path/to/local"],
            local_configuration_root="path/to/local",
        )
        directory = shared_analysis_directory.get_scratch_directory()
        self.assertEqual(directory, "/scratch")

        root = shared_analysis_directory.get_root()
        self.assertEqual(root, "/scratch/path/to/local")

    @patch("os.makedirs")
    @patch(filesystem_name + ".acquire_lock")
    @patch.object(SharedAnalysisDirectory, "_clear")
    @patch.object(SharedAnalysisDirectory, "_merge")
    def test_prepare(self, merge, clear, acquire_lock, makedirs):
        @contextmanager
        def acquire(*args, **kwargs):
            yield

        shared_analysis_directory = SharedAnalysisDirectory(["first", "second"], [])
        acquire_lock.side_effect = acquire
        shared_analysis_directory.prepare()
        merge.assert_has_calls([call()])
        clear.assert_has_calls([call()])

    @patch("os.path.realpath", side_effect=lambda path: "realpath({})".format(path))
    @patch("os.getcwd", return_value="/")
    @patch("os.path.exists", return_value=True)
    def test_resolve_source_directories(self, exists, cwd, realpath) -> None:
        arguments = MagicMock()
        arguments.source_directories = []
        arguments.original_directory = "/root"
        arguments.build = False
        arguments.command = commands.Check
        arguments.current_directory = "/root/local"
        configuration = MagicMock()
        configuration.source_directories = []
        configuration.local_configuration_root = "/root/local"

        with self.assertRaises(EnvironmentException):
            analysis_directory = SharedAnalysisDirectory(
                [], [], original_directory="/root", filter_paths=[], build=False
            )
            analysis_directory._resolve_source_directories()

        # Arguments override configuration.
        with patch.object(
            buck, "generate_source_directories", return_value=[]
        ) as buck_source_directories:
            arguments.source_directories = ["arguments_source_directory"]
            configuration.source_directories = ["configuration_source_directory"]

            analysis_directory = SharedAnalysisDirectory(
                ["some_source_directory"],
                ["configuration_source_directory"],
                original_directory="/root",
                filter_paths=[],
                build=False,
                prompt=True,
            )
            analysis_directory._resolve_source_directories()
            buck_source_directories.assert_called_with(
                {"configuration_source_directory"}, build=False, prompt=True
            )
            self.assertEqual(
                analysis_directory._source_directories, {"some_source_directory"}
            )

        with patch.object(
            buck, "generate_source_directories", return_value=["arguments_target"]
        ) as buck_source_directories:
            arguments.source_directories = []
            arguments.targets = ["arguments_target"]
            configuration.source_directories = ["configuration_source_directory"]

            analysis_directory = resolve_analysis_directory(
                arguments, commands, configuration, prompt=True
            )
            assert isinstance(analysis_directory, SharedAnalysisDirectory)
            analysis_directory._resolve_source_directories()
            buck_source_directories.assert_called_with(
                {"arguments_target"}, build=False, prompt=True
            )
            self.assertEqual(
                analysis_directory._source_directories,
                {"realpath(root/arguments_target)"},
            )

        # Restart and start always rebuild buck targets
        with patch.object(
            buck, "generate_source_directories", return_value=["arguments_target"]
        ) as buck_source_directories:
            arguments.command = commands.Start
            analysis_directory = resolve_analysis_directory(
                arguments, commands, configuration, prompt=True
            )
            assert isinstance(analysis_directory, SharedAnalysisDirectory)
            analysis_directory._resolve_source_directories()
            buck_source_directories.assert_called_with(
                {"arguments_target"}, build=False, prompt=True
            )
            arguments.command = commands.Restart
            analysis_directory = resolve_analysis_directory(
                arguments, commands, configuration, prompt=True
            )
            assert isinstance(analysis_directory, SharedAnalysisDirectory)
            analysis_directory._resolve_source_directories()
            buck_source_directories.assert_called_with(
                {"arguments_target"}, build=True, prompt=True
            )

        # Configuration is picked up when no arguments provided.
        with patch.object(
            buck,
            "generate_source_directories",
            return_value=["configuration_source_directory"],
        ) as buck_source_directories:
            arguments.source_directories = []
            arguments.targets = []
            arguments.command = commands.Check
            arguments.build = True
            configuration.targets = ["configuration_target"]
            configuration.source_directories = []

            analysis_directory = resolve_analysis_directory(
                arguments, commands, configuration, prompt=True
            )
            assert isinstance(analysis_directory, SharedAnalysisDirectory)
            analysis_directory._resolve_source_directories()

            buck_source_directories.assert_called_with(
                {"configuration_target"}, build=True, prompt=True
            )
            self.assertEqual(
                analysis_directory._source_directories,
                {"realpath(root/configuration_source_directory)"},
            )

        # Files are translated relative to project root
        with patch.object(
            buck, "generate_source_directories", return_value=["."]
        ) as buck_source_directories:
            arguments.source_directories = []
            arguments.targets = []
            configuration.targets = ["."]

            analysis_directory = resolve_analysis_directory(
                arguments, commands, configuration, prompt=True
            )
            assert isinstance(analysis_directory, SharedAnalysisDirectory)
            analysis_directory._resolve_source_directories()

            self.assertEqual(
                analysis_directory._source_directories, {"realpath(root/.)"}
            )

    @patch("os.path.isfile")
    def test_find_configuration(self, os_mock_isfile) -> None:
        os_mock_isfile.side_effect = [False, False, False, True]
        self.assertEqual(find_root("/a/b/c/d", "configuration"), "/a")
        os_mock_isfile.side_effect = [True]
        self.assertEqual(find_root("/a", "configuration"), "/a")
        os_mock_isfile.side_effect = [False, False]
        self.assertEqual(find_root("/a/b", "configuration"), None)

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
        _add_symbolic_link("/a/link", "file.py")
        # standard use-cases
        makedirs.assert_called_once_with("/a")
        symlink.assert_called_once_with("file.py", "/a/link")

        symlink.reset_mock()
        makedirs.reset_mock()
        _add_symbolic_link("/a/b/c/d/link", "file.py")
        makedirs.assert_called_once_with("/a/b/c/d")
        symlink.assert_called_once_with("file.py", "/a/b/c/d/link")

        # symlink exists
        symlink.reset_mock()
        makedirs.reset_mock()
        error = OSError()
        error.errno = errno.EEXIST
        symlink.side_effect = [error, None]
        _add_symbolic_link("/a/b/link", "file.py")
        makedirs.assert_called_once_with("/a/b")
        symlink.assert_called_with("file.py", "/a/b/link")
        unlink.assert_called_once_with("/a/b/link")

        # symlink fails
        symlink.reset_mock()
        makedirs.reset_mock()
        unlink.reset_mock()
        symlink.side_effect = OSError()
        _add_symbolic_link("/a/link", "file.py")
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

    @patch.object(filesystem, "_compute_symbolic_link_mapping", return_value={})
    @patch.object(os.path, "realpath", side_effect=lambda path: path)
    def test_process_updated_files(self, realpath, compute_symbolic_links):
        search_path = ["/SEARCH_PATH", "/SECOND_SEARCH_PATH$subdir"]
        tracked_files = [
            "/ROOT/a.py",
            "/ROOT/subdir/b.py",
            "/SEARCH_PATH/library.py",
            "/SECOND_SEARCH_PATH/subdir/library2.py",
        ]
        untracked_files = [
            "/UNRELATED/e.py",
            "/SECOND_SEARCH_PATH/f.py",
            "/SECOND_SEARCH_PATH/subdir2/g.py",
        ]

        analysis_directory = AnalysisDirectory("/ROOT", search_path=search_path)
        updated_files = analysis_directory.process_updated_files(
            [*tracked_files, *untracked_files]
        )
        self.assertListEqual(sorted(updated_files), sorted(tracked_files))

        compute_symbolic_links.return_value = {
            "/ROOT/a.py": "/ANALYSIS/a.py",
            "/ROOT/subdir/b.py": "/ANALYSIS/subdir/b.py",
        }
        shared_tracked_files = [
            "/ANALYSIS/a.py",
            "/ANALYSIS/subdir/b.py",
            "/SEARCH_PATH/library.py",
            "/SECOND_SEARCH_PATH/subdir/library2.py",
        ]
        analysis_directory = SharedAnalysisDirectory(
            ["/ROOT"], [], search_path=search_path
        )
        updated_files = analysis_directory.process_updated_files(
            [*tracked_files, *untracked_files]
        )
        self.assertListEqual(sorted(updated_files), sorted(shared_tracked_files))
