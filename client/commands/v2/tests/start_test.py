# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Iterable, Tuple

import testslide

from ..start import (
    Arguments,
    CriticalFile,
    LoadSavedStateFromFile,
    LoadSavedStateFromProject,
    MatchPolicy,
)


class ArgumentTest(testslide.TestCase):
    def test_serialize_critical_file(self) -> None:
        self.assertDictEqual(
            CriticalFile(policy=MatchPolicy.BASE_NAME, path="foo").serialize(),
            {"base_name": "foo"},
        )
        self.assertDictEqual(
            CriticalFile(policy=MatchPolicy.FULL_PATH, path="/foo/bar").serialize(),
            {"full_path": "/foo/bar"},
        )

    def test_serialize_saved_state_action(self) -> None:
        self.assertTupleEqual(
            LoadSavedStateFromFile(shared_memory_path="/foo/bar").serialize(),
            ("load_from_file", {"shared_memory_path": "/foo/bar"}),
        )
        self.assertTupleEqual(
            LoadSavedStateFromFile(
                shared_memory_path="/foo/bar", changed_files_path="derp.txt"
            ).serialize(),
            (
                "load_from_file",
                {"shared_memory_path": "/foo/bar", "changed_files_path": "derp.txt"},
            ),
        )
        self.assertTupleEqual(
            LoadSavedStateFromProject(project_name="my_project").serialize(),
            ("load_from_project", {"project_name": "my_project"}),
        )
        self.assertTupleEqual(
            LoadSavedStateFromProject(
                project_name="my_project", project_metadata="my_metadata"
            ).serialize(),
            (
                "load_from_project",
                {"project_name": "my_project", "project_metadata": "my_metadata"},
            ),
        )

    def test_serialize_arguments(self) -> None:
        def assert_serialized(
            arguments: Arguments, items: Iterable[Tuple[str, object]]
        ) -> None:
            serialized = arguments.serialize()
            for key, value in items:
                if key not in serialized:
                    self.fail(f"Cannot find key `{key}` in serialized arguments")
                else:
                    self.assertEqual(value, serialized[key])

        assert_serialized(
            Arguments(log_path="foo", global_root="bar", source_paths=["source"]),
            [("log_path", "foo"), ("global_root", "bar"), ("source_paths", ["source"])],
        )
        assert_serialized(
            Arguments(
                log_path="/log",
                global_root="/project",
                excludes=["/excludes"],
                checked_directory_allowlist=["/allows"],
                checked_directory_blocklist=["/blocks"],
                extensions=[".typsy"],
                taint_models_path=["/taint/model"],
            ),
            [
                ("excludes", ["/excludes"]),
                ("checked_directory_allowlist", ["/allows"]),
                ("checked_directory_blocklist", ["/blocks"]),
                ("extensions", [".typsy"]),
                ("taint_model_paths", ["/taint/model"]),
            ],
        )
        assert_serialized(
            Arguments(
                log_path="/log",
                global_root="/project",
                debug=True,
                strict=True,
                show_error_traces=True,
                store_type_check_resolution=True,
                critical_files=[
                    CriticalFile(policy=MatchPolicy.BASE_NAME, path="foo.py"),
                    CriticalFile(policy=MatchPolicy.FULL_PATH, path="/home/bar.txt"),
                ],
            ),
            [
                ("debug", True),
                ("strict", True),
                ("show_error_traces", True),
                ("store_type_check_resolution", True),
                (
                    "critical_files",
                    [{"base_name": "foo.py"}, {"full_path": "/home/bar.txt"}],
                ),
            ],
        )
        assert_serialized(
            Arguments(
                log_path="/log",
                global_root="/project",
                parallel=True,
                number_of_workers=20,
            ),
            [("parallel", True), ("number_of_workers", 20)],
        )
        assert_serialized(
            Arguments(
                log_path="/log",
                global_root="/project",
                local_root="/project/local",
                watchman_root="/project",
            ),
            [("local_root", "/project/local"), ("watchman_root", "/project")],
        )
        assert_serialized(
            Arguments(
                log_path="/log",
                global_root="/project",
                saved_state_action=LoadSavedStateFromProject(
                    project_name="my_project", project_metadata="my_metadata"
                ),
            ),
            [
                (
                    "saved_state_action",
                    (
                        "load_from_project",
                        {
                            "project_name": "my_project",
                            "project_metadata": "my_metadata",
                        },
                    ),
                )
            ],
        )
