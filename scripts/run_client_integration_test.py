#!/usr/bin/env python3

import glob
import json
import logging
import os
import re
import shutil
import signal
import subprocess
import tempfile
import textwrap
import time
import unittest
from abc import ABC
from contextlib import contextmanager
from logging import Logger
from pathlib import Path
from typing import Any, Dict, Generator, List, NamedTuple, Optional, Pattern, overload

import libfb.py.pathutils as pathutils


PYRE_CLIENT: str = pathutils.get_build_rule_output_path("//tools/pyre/client:pyre")
PYRE_BUCK_BUILDER: str = pathutils.get_build_rule_output_path(
    "//tools/pyre/facebook/tools/buck_project_builder:fb_buck_project_builder",
    rule_type=pathutils.BuildRuleTypes.JEX_BINARY,
)

LOG: Logger = logging.getLogger(__name__)
CONFIGURATION = ".pyre_configuration"
LOCAL_CONFIGURATION = ".pyre_configuration.local"

BINARY_OVERRIDE = "PYRE_BINARY"
BINARY_VERSION_PATTERN: Pattern[str] = re.compile(r"Binary version: (\w*).*")

VALID_DICT: Pattern[str] = re.compile(r"\{.*\}")
VALID_JSON_LIST: Pattern[str] = re.compile(r"\[(\{.*\})*\]")
VALID_LIST: Pattern[str] = re.compile(r"\[\]")


class FilesystemError(IOError):
    pass


class CommandData(NamedTuple):
    working_directory: str
    command: List[str]


class PyreResult(NamedTuple):
    command: str
    output: Optional[str]
    error_output: Optional[str]
    return_code: int


@contextmanager
def _watch_directory(source_directory: str) -> Generator[None, None, None]:
    try:
        result = json.loads(subprocess.check_output(["watchman", "watch-list"]))
        watched = os.path.abspath(source_directory) in result["roots"]
    except KeyError:
        watched = False

    def call_watchman(command: str) -> None:
        watchman_process = subprocess.run(
            ["watchman", command, source_directory], capture_output=True
        )
        if watchman_process.returncode != 0:
            LOG.error(watchman_process.stderr.decode())

    if not watched:
        call_watchman("watch")
    yield
    if not watched:
        call_watchman("watch-del")


class TestCommand(unittest.TestCase, ABC):
    directory: Path
    typeshed: Path
    command_history: List[CommandData]

    def __init__(self, methodName: str) -> None:
        super(TestCommand, self).__init__(methodName)
        # workaround for initialization type errors
        self.directory = Path(".")
        self.typeshed = Path(".")
        self.command_history = []
        if not os.environ.get("PYRE_CLIENT"):
            os.environ["PYRE_CLIENT"] = PYRE_CLIENT

    def setUp(self) -> None:
        self.directory = Path(tempfile.mkdtemp())
        self.typeshed = Path(self.directory, "fake_typeshed")
        self.buck_config = Path(self.directory, ".buckconfig").touch()
        Path(self.typeshed, "stdlib").mkdir(parents=True)
        self.initial_filesystem()

    def tearDown(self) -> None:
        self.cleanup()
        self.command_history = []
        shutil.rmtree(self.directory)

    def initial_filesystem(self) -> None:
        pass

    def cleanup(self) -> None:
        pass

    def create_project_configuration(
        self, root: Optional[str] = None, contents: Optional[Dict[str, Any]] = None
    ) -> None:
        root: Path = Path(root) if root else self.directory
        configuration_path = root / CONFIGURATION
        if not contents:
            # Use binary override if it is built.
            binary_override = os.environ.get(BINARY_OVERRIDE)
            # TODO(T57341910): Set binary override in buck test.
            if binary_override:
                contents = {"version": "$BINARY_OVERRIDE"}
            else:
                # Default to published binary version.
                output = subprocess.run(["pyre", "--version"], capture_output=True)
                if output.returncode != 0:
                    LOG.error(output.stderr.decode())
                output_match = re.match(BINARY_VERSION_PATTERN, output.stdout.decode())
                version = output_match.group(1) if output_match else None
                if version and version != "No":
                    contents = {"version": version, "use_buck_builder": True}
                else:
                    binary_location = shutil.which("pyre.bin")
                    if binary_location is None:
                        LOG.error(
                            "No project configuration content provided and "
                            "could not find a binary to run."
                        )
                        raise FilesystemError
                    contents = {"binary": binary_location}
        with configuration_path.open("w+") as configuration_file:
            json.dump(contents, configuration_file)

    def create_local_configuration(self, root: str, contents: Dict[str, Any]) -> None:
        root: Path = self.directory / root
        root.mkdir(exist_ok=True)
        with (root / LOCAL_CONFIGURATION).open("w+") as configuration_file:
            json.dump(contents, configuration_file)

    def create_directory(self, relative_path: str) -> None:
        Path(self.directory, relative_path).mkdir(parents=True)

    def create_file(self, relative_path: str, contents: str = "") -> None:
        file_path = self.directory / relative_path
        file_path.parent.mkdir(exist_ok=True, parents=True)
        file_path.write_text(textwrap.dedent(contents))

    def create_file_with_error(self, relative_path: str) -> None:
        contents = """
            def foo(x: int) -> str:
                return x
            """
        self.create_file(relative_path, contents)

    def delete_file(self, relative_path: str) -> None:
        try:
            (self.directory / relative_path).unlink()
        except FileNotFoundError:
            LOG.debug(
                "Deletion of {} skipped; file does not exist.".format(relative_path)
            )

    def run_pyre(
        self,
        command: str,
        *arguments: str,
        working_directory: Optional[str] = None,
        timeout: int = 30,
        prompts: Optional[List[str]] = None,
        interrupt_after_seconds: Optional[int] = None,
    ) -> PyreResult:
        working_directory: Path = (
            self.directory / working_directory if working_directory else self.directory
        )
        prompt_inputs = "\n".join(prompts).encode() if prompts else None
        # TODO(T60769864): Consider building shim if it exists.
        command: List[str] = [
            "pyre",
            "--noninteractive",
            "--output=json",
            "--typeshed",
            str(self.typeshed),
            command,
            *arguments,
        ]
        try:
            self.command_history.append(CommandData(str(working_directory), command))
            process = subprocess.Popen(
                command,
                cwd=working_directory,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            if prompt_inputs:
                process.stdin.write(prompt_inputs)
            if interrupt_after_seconds:
                time.sleep(interrupt_after_seconds)
                process.send_signal(signal.SIGINT)
            output, error_output = process.communicate(timeout=timeout)

            return PyreResult(
                " ".join(command),
                output.decode(),
                error_output.decode(),
                process.returncode,
            )
        except subprocess.TimeoutExpired as error:
            stdout = error.stdout
            stderr = error.stderr
            result = PyreResult(
                " ".join(command),
                stdout.decode() if stdout else "",
                stderr.decode() if stderr else "",
                -1,
            )
            LOG.error(self.get_context(result))
            raise error

    def get_servers(self) -> List[Dict[str, Any]]:
        result = self.run_pyre("--output=json", "servers")
        try:
            running_servers = json.loads(result.output or "")
        except json.JSONDecodeError as json_error:
            LOG.error(self.get_context(result))
            raise json_error
        return running_servers

    def get_context(self, result: Optional[PyreResult] = None) -> str:
        # TODO(T60769864): Avoid printing context twice in buck runs.
        # TODO(T57341910): Log pyre rage / debug when appropriate.
        context = ""

        def format_section(title: str, *contents: str) -> str:
            divider = "=" * 15
            # pyre-ignore[9]: Unable to unpack `str`, expected a tuple.
            contents = "\n\n".join([content.strip() for content in contents])
            section = "\n\n{} {} {}\n\n{}\n".format(divider, title, divider, contents)
            return section

        # Pyre Output
        if result:
            output = result.output or ""
            error_output = result.error_output or ""
            if output:
                output = "Stdout:\n" + output
            if error_output:
                error_output = "Stderr:\n" + error_output
            if result.output or result.error_output:
                context += format_section(
                    "Pyre Output",
                    "Command: `" + result.command + "`",
                    output,
                    error_output,
                )

        # Filesystem Structure
        filesystem_structure = subprocess.run(
            ["tree", self.directory, "-a", "-I", "typeshed"], capture_output=True
        ).stdout.decode()
        context += format_section("Filesystem Structure", filesystem_structure)

        # Version Information
        version_output = subprocess.run(
            ["pyre", "--version"], cwd=self.directory, capture_output=True
        ).stdout.decode()
        configurations = glob.glob(
            str(self.directory / "**/.pyre_configuration*"), recursive=True
        )
        configuration_contents = ""
        for configuration in configurations:
            configuration_contents += configuration + "\n  "
            configuration_contents += Path(configuration).read_text() + "\n\n"
        context += format_section("Versioning", version_output, configuration_contents)

        # Repro Instructions
        instructions = ""
        if self.command_history:
            instructions += "- Create directory structure above and run:\n\t"
            instructions += "\n\t".join(
                [
                    "["
                    + str(command.working_directory).replace(
                        str(self.directory), "$project_root"
                    )
                    + "] "
                    + " ".join(command.command)
                    for command in self.command_history
                ]
            )

        test_id = self.id()
        test_name = test_id.split(".")[-1]
        test_qualifier = r"\.".join(test_id.split(".")[:-1])
        test_target = "//tools/pyre/scripts:pyre_client_integration_test_runner"

        instructions += "\n\n- Re-run only this failing test:"
        instructions += "\n\t(with buck)\n\t"
        instructions += r"[tools/pyre] buck test {} -- '{} \({}\)' {}".format(
            test_target, test_name, test_qualifier, "--run-slow-tests"
        )
        instructions += "\n\t(with client override set)\n\t"
        instructions += "[tools/pyre] python3 {} {}".format(
            "scripts/run_client_integration_test.py", ".".join(test_id.split(".")[-2:])
        )

        instructions += "\n\n- Flaky? Stress test this failing test:\n\t"
        test_target = "//tools/pyre/scripts:pyre_client_integration_test_runner"
        buck_arguments = "--run-slow-tests --jobs 18 --stress-runs 20 --record-results"
        test_name = test_id.split(".")[-1]
        test_qualifier = r"\.".join(test_id.split(".")[:-1])
        instructions += r"[tools/pyre] buck test {} -- '{} \({}\)' {}".format(
            test_target, test_name, test_qualifier, buck_arguments
        )
        context += format_section("Repro Instructions", instructions)
        return context

    def assert_succeeded(self, result: PyreResult) -> None:
        self.assertEqual(result.return_code, 0, self.get_context(result))

    def assert_failed(self, result: PyreResult) -> None:
        self.assertEqual(result.return_code, 2, self.get_context(result))

    def assert_output_matches(
        self, result: PyreResult, expected_pattern: Pattern[str]
    ) -> None:
        output = result.output or ""
        result_match = re.match(expected_pattern, output.strip())
        self.assertTrue(result_match is not None, self.get_context(result))

    def assert_has_errors(self, result: PyreResult) -> None:
        self.assertEqual(result.return_code, 1, self.get_context(result))

    def assert_no_errors(self, result: PyreResult) -> None:
        self.assertEqual(result.return_code, 0, self.get_context(result))

    @overload
    def assert_file_exists(  # pyre-ignore: T62276784
        self, relative_path: str, result: Optional[PyreResult]
    ) -> None:
        ...

    @overload  # noqa
    def assert_file_exists(  # pyre-ignore: T62276784
        self, relative_path: str, *, contents: str, result: Optional[PyreResult] = None
    ) -> None:
        ...

    @overload  # noqa
    def assert_file_exists(  # pyre-ignore: T62276784
        self,
        relative_path: str,
        *,
        json_contents: Dict[str, Any],
        result: Optional[PyreResult] = None,
    ) -> None:
        ...

    def assert_file_exists(  # noqa
        self,
        relative_path: str,
        json_contents: Optional[Dict[str, Any]] = None,
        contents: Optional[str] = None,
        result: Optional[PyreResult] = None,
    ) -> None:
        file_path = self.directory / relative_path
        self.assertTrue(file_path.exists(), self.get_context(result))
        if json_contents or contents:
            file_contents = file_path.read_text()
            if json_contents:
                self.assertEqual(
                    json.loads(file_contents), json_contents, self.get_context(result)
                )
            if contents:
                self.assertEqual(
                    file_contents, textwrap.dedent(contents), self.get_context(result)
                )

    def assert_directory_exists(
        self,
        relative_path: str,
        exists: bool = True,
        result: Optional[PyreResult] = None,
    ) -> None:
        path = self.directory / relative_path
        if exists:
            self.assertTrue(path.is_dir(), self.get_context(result))
        else:
            self.assertFalse(path.is_dir(), self.get_context(result))

    def assert_server_exists(
        self, server_name: str, result: Optional[PyreResult] = None
    ) -> None:
        running_servers = self.get_servers()
        server_exists = any(server["name"] == server_name for server in running_servers)
        self.assertTrue(server_exists, self.get_context(result))

    def assert_server_does_not_exist(
        self, server_name: str, result: Optional[PyreResult] = None
    ) -> None:
        running_servers = self.get_servers()
        server_exists = any(server["name"] == server_name for server in running_servers)
        self.assertFalse(server_exists, self.get_context(result))

    def assert_no_servers_exist(self, result: Optional[PyreResult] = None) -> None:
        self.assertEqual(self.get_servers(), [], self.get_context(result))


class BaseCommandTest(TestCommand):
    def test_pyre_version(self) -> None:
        result = self.run_pyre("--version")
        self.assert_output_matches(
            result, re.compile(r"Binary version: No version set\nClient version:.*")
        )

        self.create_project_configuration(contents={"version": "abc"})
        result = self.run_pyre("--version")
        self.assert_output_matches(
            result, re.compile(r"Binary version: abc\nClient version:.*")
        )

        self.create_local_configuration("local", contents={"version": "def"})
        result = self.run_pyre("-l", "local", "--version")
        self.assert_output_matches(
            result, re.compile(r"Binary version: def\nClient version:.*")
        )


class AnalyzeTest(TestCommand):
    def initial_filesystem(self) -> None:
        self.create_project_configuration()
        self.create_local_configuration("local_project", {"source_directories": ["."]})

    def test_analyze(self) -> None:
        result = self.run_pyre("analyze")
        self.assert_failed(result)

        result = self.run_pyre("-l", "local_project", "analyze")
        self.assert_output_matches(result, VALID_LIST)

    def test_analyze_flags(self) -> None:
        result = self.run_pyre("-l", "local_project", "analyze", "--dump-call-graph")
        self.assert_output_matches(result, VALID_LIST)

        self.create_directory("output")
        result = self.run_pyre(
            "-l", "local_project", "analyze", "--save-results-to", "output"
        )
        self.assert_file_exists("output/taint-metadata.json", result=result)
        self.assert_file_exists("output/taint-output.json", result=result)

        self.create_directory("taint_models_path")
        self.create_file(
            "taint_models_path/taint.config",
            contents="{sources: [], sinks: [], features: [], rules: []}",
        )
        result = self.run_pyre(
            "-l", "local_project", "analyze", "--taint-models-path", "taint_models_path"
        )
        self.assert_succeeded(result)
        self.create_file("taint_models_path/sinks.pysa", contents="def invalid(): ...")
        result = self.run_pyre(
            "-l", "local_project", "analyze", "--taint-models-path", "taint_models_path"
        )
        self.assert_failed(result)
        result = self.run_pyre(
            "-l",
            "local_project",
            "analyze",
            "--taint-models-path",
            "taint_models_path",
            "--no-verify",
        )
        self.assert_succeeded(result)

        self.create_directory("temp")
        result = self.run_pyre(
            "-l", "local_project", "analyze", "--repository-root", "temp"
        )
        self.assert_succeeded(result)
        result = self.run_pyre("-l", "local_project", "analyze", "--rule", "1")
        self.assert_succeeded(result)


class CheckTest(TestCommand):
    def initial_filesystem(self) -> None:
        self.create_project_configuration()
        self.create_file_with_error("local_project/has_type_error.py")

        self.create_file_with_error("buck_project/test.py")
        self.create_file(".buckversion", contents="last")
        self.create_file(
            "buck_project/BUCK",
            contents="""
                python_library(
                    name = "example_library",
                    srcs = ["test.py"],
                )
                python_binary(
                    name = "example",
                    main_module = "test",
                    deps = [
                        ":example_library",
                    ],
                )
                """,
        )

    def test_command_line_source_directory_check(self) -> None:
        result = self.run_pyre("--source-directory", "local_project", "check")
        self.assert_has_errors(result)
        self.assert_no_servers_exist(result)

        result = self.run_pyre("-l", "local_project", "check")
        self.assert_failed(result)

    def test_command_line_targets_check(self) -> None:
        result = self.run_pyre(
            "--target",
            "//buck_project:example",
            "--buck-builder-binary",
            PYRE_BUCK_BUILDER,
            "check",
        )
        self.assert_has_errors(result)
        self.assert_no_servers_exist(result)

    def test_local_configuration_check(self) -> None:
        self.create_local_configuration("local_project", {"source_directories": ["."]})
        result = self.run_pyre("-l", "local_project", "check")
        self.assert_has_errors(result)
        self.assert_no_servers_exist(result)

        self.create_local_configuration(
            "local_project_two", {"targets": ["//buck_project:example"]}
        )
        result = self.run_pyre("-l", "local_project", "check")
        self.assert_has_errors(result)
        self.assert_no_servers_exist(result)


class ColorTest(TestCommand):
    # TODO(T62183021): Add testing when color is fixed.
    pass


class DeobfuscateTest(TestCommand):
    # TODO(T62143503): Add testing when deobfuscate is re-introduced.
    pass


class IncrementalTest(TestCommand):
    def cleanup(self) -> None:
        self.run_pyre("kill")

    def initial_filesystem(self) -> None:
        self.create_project_configuration()
        self.create_file(".watchmanconfig", "{}")
        self.create_directory("local_project")
        self.create_local_configuration("local_project", {"source_directories": ["."]})
        self.create_file(".watchmanconfig", "{}")

    def test_incremental(self) -> None:
        self.create_file_with_error("local_project/has_type_error.py")

        result = self.run_pyre("-l", "local_project")
        self.assert_has_errors(result)
        result = self.run_pyre("-l", "local_project")
        self.assert_has_errors(result)

        self.run_pyre("-l", "local_project", "stop")
        result = self.run_pyre("-l", "local_project")
        self.assert_has_errors(result)

        self.run_pyre("kill")
        result = self.run_pyre("-l", "local_project")
        self.assert_has_errors(result)

        self.run_pyre("kill")
        self.run_pyre("-l", "local_project", "start")
        result = self.run_pyre("-l", "local_project")
        self.assert_has_errors(result)

        self.run_pyre("-l", "local_project", "restart")
        result = self.run_pyre("-l", "local_project")
        self.assert_has_errors(result)

    def test_incremental_with_changes(self) -> None:
        with _watch_directory(self.directory):
            self.create_file("local_project/test.py", contents="def foo(): ...")
            result = self.run_pyre("-l", "local_project")
            self.assert_no_errors(result)

            self.create_file_with_error("local_project/test.py")
            result = self.run_pyre("-l", "local_project")
            self.assert_has_errors(result)

            self.delete_file("local_project/test.py")
            result = self.run_pyre("-l", "local_project")
            self.assert_no_errors(result)

            self.create_file_with_error("local_project/has_type_error.py")
            result = self.run_pyre("-l", "local_project")
            self.assert_has_errors(result)

            self.run_pyre("-l", "local_project", "restart")
            result = self.run_pyre("-l", "local_project")
            self.assert_has_errors(result)
            self.delete_file("local_project/has_type_error.py")
            result = self.run_pyre("-l", "local_project")
            self.assert_no_errors(result)

    def test_command_line_sources(self) -> None:
        # TODO(T60110667): Test that command line sources do not start a server.
        pass


class InferTest(TestCommand):
    contents = """
        def foo():
            return 1
    """
    typed_contents = """
        def foo() -> int:
            return 1
    """

    def initial_filesystem(self) -> None:
        self.create_project_configuration()
        self.create_local_configuration("local_project", {"source_directories": ["."]})

        self.create_file("local_project/missing_annotation.py", self.contents)
        self.create_file("local_project/missing_annotation_two.py", self.contents)

    def test_infer_stubs(self) -> None:
        result = self.run_pyre("-l", "local_project", "infer")
        self.assert_file_exists(
            ".pyre/local_project/types/local_project/missing_annotation.pyi",
            result=result,
        )
        self.assert_file_exists(
            ".pyre/local_project/types/local_project/missing_annotation_two.pyi",
            result=result,
        )
        self.assert_file_exists(
            "local_project/missing_annotation.py", contents=self.contents, result=result
        )
        self.assert_file_exists(
            "local_project/missing_annotation_two.py",
            contents=self.contents,
            result=result,
        )

        # Infer from existing stubs
        result = self.run_pyre(
            "-l", "local_project", "infer", "--annotate-from-existing-stubs"
        )
        # Failing because --in-place flag is required.
        self.assert_failed(result)

        result = self.run_pyre(
            "-l",
            "local_project",
            "infer",
            "--annotate-from-existing-stubs",
            "--in-place",
        )
        self.assert_file_exists(
            "local_project/missing_annotation.py",
            contents=self.typed_contents,
            result=result,
        )
        self.assert_file_exists(
            "local_project/missing_annotation_two.py",
            contents=self.typed_contents,
            result=result,
        )

    def test_infer_in_place(self) -> None:
        result = self.run_pyre("-l", "local_project", "infer", "--in-place")
        self.assert_file_exists(
            ".pyre/local_project/types/local_project/missing_annotation.pyi",
            result=result,
        )
        self.assert_file_exists(
            ".pyre/local_project/types/local_project/missing_annotation_two.pyi",
            result=result,
        )
        self.assert_file_exists(
            "local_project/missing_annotation.py",
            contents=self.typed_contents,
            result=result,
        )
        self.assert_file_exists(
            "local_project/missing_annotation_two.py",
            contents=self.typed_contents,
            result=result,
        )

    def test_infer_specific_in_place(self) -> None:
        result = self.run_pyre(
            "-l",
            "local_project",
            "infer",
            "--in-place",
            "local_project/missing_annotation.py",
        )
        self.assert_file_exists(
            "local_project/missing_annotation.py",
            contents=self.typed_contents,
            result=result,
        )
        self.assert_file_exists(
            "local_project/missing_annotation_two.py",
            contents=self.contents,
            result=result,
        )

    def test_infer_from_json(self) -> None:
        result = self.run_pyre("-l", "local_project", "infer", "--print-only")
        self.assert_output_matches(result, VALID_JSON_LIST)
        self.assert_file_exists(
            "local_project/missing_annotation.py", contents=self.contents, result=result
        )
        self.assert_file_exists(
            "local_project/missing_annotation_two.py",
            contents=self.contents,
            result=result,
        )

        # TODO(T62259082): Fix JSON formatting
        output = result.output
        json_stub = '{"errors": ' + output + "}" if output else ""
        self.assertTrue(json_stub is not None)
        result = self.run_pyre(
            "-l", "local_project", "infer", "--in-place", "--json", prompts=[json_stub]
        )
        self.assert_file_exists(
            "local_project/missing_annotation.py",
            contents=self.typed_contents,
            result=result,
        )
        self.assert_file_exists(
            "local_project/missing_annotation_two.py",
            contents=self.typed_contents,
            result=result,
        )

    def test_infer_recursive(self) -> None:
        result = self.run_pyre("-l", "local_project", "infer", "--recursive")
        self.assert_file_exists(
            ".pyre/local_project/types/local_project/missing_annotation.pyi",
            result=result,
        )
        self.assert_file_exists(
            ".pyre/local_project/types/local_project/missing_annotation_two.pyi",
            result=result,
        )

        result = self.run_pyre(
            "-l", "local_project", "infer", "--recursive", "--in-place"
        )
        self.assert_file_exists(
            "local_project/missing_annotation.py",
            contents=self.typed_contents,
            result=result,
        )
        self.assert_file_exists(
            "local_project/missing_annotation_two.py",
            contents=self.typed_contents,
            result=result,
        )

    def test_infer_full_only(self) -> None:
        partial_contents = """
            def foo(x):
                return 1
        """
        self.create_file("local_project/missing_annotation_three.py", partial_contents)
        result = self.run_pyre(
            "-l", "local_project", "infer", "--full-only", "--in-place"
        )
        self.assert_file_exists(
            "local_project/missing_annotation.py",
            contents=self.typed_contents,
            result=result,
        )
        self.assert_file_exists(
            "local_project/missing_annotation_two.py",
            contents=self.typed_contents,
            result=result,
        )
        self.assert_file_exists(
            "local_project/missing_annotation_three.py",
            contents=partial_contents,
            result=result,
        )


class InitializeTest(TestCommand):
    def initial_filesystem(self) -> None:
        self.create_file("fake_pyre.bin")

    # TODO(T61790851): Make prompting explicit, test conditions that skip prompts.
    def test_initialize_project_configuration(self) -> None:
        with _watch_directory(self.directory):
            result = self.run_pyre(
                "init",
                prompts=["y", "fake_pyre.bin", "fake_typeshed", "//example:target"],
            )
            expected_contents = {
                "binary": str(self.directory / "fake_pyre.bin"),
                "source_directories": ["//example:target"],
                "typeshed": str(self.directory / "fake_typeshed"),
            }
            self.assert_file_exists(
                ".pyre_configuration", json_contents=expected_contents, result=result
            )

    def test_initialize_local_configuration(self) -> None:
        self.create_directory("local_project")
        with _watch_directory(self.directory):
            result = self.run_pyre(
                "init",
                "--local",
                working_directory="local_project",
                prompts=["Y", "//example:target", "Y", "Y", "Y"],
            )
            expected_contents = {
                "differential": False,
                "push_blocking": True,
                "targets": ["//example:target"],
            }
            self.assert_file_exists(
                "local_project/.pyre_configuration.local",
                json_contents=expected_contents,
                result=result,
            )


class KillTest(TestCommand):
    def initial_filesystem(self) -> None:
        self.create_project_configuration()
        self.create_local_configuration("local_one", {"source_directories": ["."]})
        self.create_file_with_error("local_one/has_type_error.py")
        self.create_local_configuration("local_two", {"source_directories": ["."]})
        self.create_file_with_error("local_two/has_type_error.py")

    def test_kill_without_server(self) -> None:
        self.assert_no_servers_exist()
        result = self.run_pyre("kill")
        self.assert_succeeded(result)
        self.assert_no_servers_exist()

    def test_kill(self) -> None:
        self.run_pyre("-l", "local_one", "start")
        self.assert_server_exists("local_one")
        self.run_pyre("kill")
        self.assert_no_servers_exist()

        self.run_pyre("-l", "local_one", "restart")
        self.assert_server_exists("local_one")
        self.run_pyre("kill")
        self.assert_no_servers_exist()

        self.run_pyre("-l", "local_one")
        self.run_pyre("-l", "local_two")
        self.assert_server_exists("local_one")
        self.assert_server_exists("local_two")
        self.run_pyre("kill")
        self.assert_no_servers_exist()

    def test_kill_resources(self) -> None:
        self.run_pyre("-l", "local_one")
        self.assert_directory_exists(".pyre/resource_cache")
        result = self.run_pyre("kill")
        self.assert_directory_exists(
            ".pyre/resource_cache", exists=False, result=result
        )


class PersistentTest(TestCommand):
    def initial_filesystem(self) -> None:
        self.create_project_configuration()
        # TODO(T57341910): Test persistent interaction with -l flag.
        self.create_local_configuration("local", {"source_directories": ["."]})

    def test_persistent(self) -> None:
        result = self.run_pyre("persistent", interrupt_after_seconds=3)
        output = result.output
        output = [line for line in output.split("\r\n") if line][-1] if output else ""
        result = PyreResult(
            result.command, output, result.error_output, result.return_code
        )
        self.assert_output_matches(result, VALID_DICT)


class QueryTest(TestCommand):
    def initial_filesystem(self) -> None:
        self.create_project_configuration()
        self.create_local_configuration("local", {"source_directories": ["."]})
        self.create_file_with_error("local/has_type_error.py")

    def test_query_help(self) -> None:
        # TODO(T62048210): Change this when help works without server
        result = self.run_pyre("query", "help")
        self.assert_failed(result)
        result = self.run_pyre("-l", "local", "query", "help")
        self.assert_failed(result)
        self.run_pyre("-l", "local", "start")
        result = self.run_pyre("-l", "local", "query", "help")
        self.assert_succeeded(result)

    def test_query(self) -> None:
        self.run_pyre("-l", "local", "start")
        result = self.run_pyre("-l", "local", "query", "join(A, B)")
        self.assert_output_matches(result, VALID_DICT)
        result = self.run_pyre("-l", "local", "query", "type(A)")
        self.assert_output_matches(result, VALID_DICT)

        # Invalid query behavior
        result = self.run_pyre("-l", "local", "query", "undefined(A)")
        self.assert_failed(result)
        # TODO(T62066748): Do not stop server when invalid query comes in.
        result = self.run_pyre("-l", "local", "query", "type(A)")
        self.assert_failed(result)


class RageTest(TestCommand):
    def initial_filesystem(self) -> None:
        self.create_project_configuration()

    def test_rage_no_servers(self) -> None:
        result = self.run_pyre("rage")
        self.assert_failed(result)

        # TODO(T61745598): Add testing for proper prompting when implemented.
        self.create_local_configuration("local_one", {"source_directories": ["."]})
        result = self.run_pyre("rage")
        self.assert_failed(result)

        # TODO(T62047832): Better defined behavior for rage after a killed or stopped
        # server, vs. non-existent server or logs
        result = self.run_pyre("-l", "local_one", "rage")
        self.assert_succeeded(result)

    def test_rage(self) -> None:
        self.create_local_configuration("local_one", {"source_directories": ["."]})
        self.create_local_configuration("local_two", {"source_directories": ["."]})
        self.run_pyre("-l", "local_one", "start")
        self.run_pyre("-l", "local_two", "start")
        result = self.run_pyre("-l", "local_one", "rage")
        self.assert_succeeded(result)

        # Invoking rage still prints logs of a stopped server
        self.run_pyre("-l", "local_one", "stop")
        result = self.run_pyre("-l", "local_one", "rage")
        self.assert_succeeded(result)
        result = self.run_pyre("-l", "local_two", "rage")
        self.assert_succeeded(result)


class RestartTest(TestCommand):
    def initial_filesystem(self) -> None:
        self.create_project_configuration()
        self.create_local_configuration("local_one", {"source_directories": ["."]})
        self.create_file_with_error("local_one/has_type_error.py")
        self.create_local_configuration("local_two", {"source_directories": ["."]})
        self.create_file_with_error("local_two/has_type_error.py")

    def test_server_restart_without_sources(self) -> None:
        # TODO(T61745598): Add testing for proper prompting when implemented.
        self.assert_no_servers_exist()
        result = self.run_pyre("restart")
        self.assert_failed(result)

    def test_restart(self) -> None:
        self.assert_no_servers_exist()
        result = self.run_pyre("-l", "local_one", "restart")
        self.assert_has_errors(result)
        self.assert_server_exists("local_one")

        result = self.run_pyre("-l", "local_one", "restart")
        self.assert_has_errors(result)
        self.assert_server_exists("local_one")


class ServersTest(TestCommand):
    def initial_filesystem(self) -> None:
        self.create_project_configuration()
        self.create_local_configuration("local_one", {"source_directories": ["."]})
        self.create_file_with_error("local_one/has_type_error.py")
        self.create_local_configuration("local_two", {"source_directories": ["."]})
        self.create_file_with_error("local_two/has_type_error.py")

    def test_list_servers(self) -> None:
        result = self.run_pyre("--output=json", "servers", "list")
        self.assert_output_matches(result, VALID_LIST)
        self.run_pyre("-l", "local_one")
        result = self.run_pyre("servers", "list")
        self.assert_output_matches(
            result, re.compile(r"\[\{\"pid\": .*, \"name\": \"local_one\"\}\]")
        )
        self.run_pyre("-l", "local_two")
        result = self.run_pyre("servers", "list")
        self.assert_output_matches(
            result,
            re.compile(
                r"\[\{\"pid\": .*, \"name\": \"(local_one|local_two)\"\}, "
                + r"{\"pid\": .*, \"name\": \"(local_one|local_two)\"\}\]"
            ),
        )

        # Test stop servers
        result = self.run_pyre("servers", "stop")
        self.assert_succeeded(result)
        result = self.run_pyre("--output=json", "servers", "list")
        self.assert_output_matches(result, VALID_LIST)


class StartTest(TestCommand):
    def cleanup(self) -> None:
        self.run_pyre("kill")

    def initial_filesystem(self) -> None:
        self.create_project_configuration()
        self.create_file(".watchmanconfig", "{}")
        self.create_local_configuration("local_one", {"source_directories": ["."]})
        self.create_file("local_one/test.py", contents="x = 1")
        self.create_local_configuration("local_two", {"source_directories": ["."]})
        self.create_file("local_two/test.py", contents="x = 1")

    def test_server_start_without_sources(self) -> None:
        # TODO(T61745598): Add testing for proper prompting when implemented.
        result = self.run_pyre("start")
        self.assert_failed(result)

    def test_server_start(self) -> None:
        with _watch_directory(self.directory):
            result = self.run_pyre("-l", "local_one", "start")
            self.assert_no_errors(result)
            self.assert_server_exists("local_one")
            result = self.run_pyre("-l", "local_two", "start")
            self.assert_no_errors(result)
            self.assert_server_exists("local_one")
            self.assert_server_exists("local_two")

            # Start already existing server
            result = self.run_pyre("-l", "local_two", "start")
            self.assert_no_errors(result)
            self.assert_server_exists("local_two")

            # Assert servers are picking up on changes
            result = self.run_pyre("-l", "local_one")
            self.assert_no_errors(result)
            self.create_file_with_error("local_one/test.py")
            result = self.run_pyre("-l", "local_one")
            self.assert_has_errors(result)

            result = self.run_pyre("-l", "local_two")
            self.create_file_with_error("local_two/test_two.py")
            result = self.run_pyre("-l", "local_two")
            self.assert_has_errors(result)

    def test_server_no_watchman(self) -> None:
        with _watch_directory(self.directory):
            result = self.run_pyre("-l", "local_one", "start", "--no-watchman")
            self.assert_no_errors(result)

            # Assert server is not picking up on changes
            self.create_file_with_error("local_one/test.py")
            result = self.run_pyre("-l", "local_one")
            self.assert_no_errors(result)


class StatisticsTest(TestCommand):
    def initial_filesystem(self) -> None:
        self.create_project_configuration()
        self.create_directory("local_project")
        self.create_local_configuration("local_project", {"source_directories": ["."]})
        self.create_file(
            "local_project/test.py",
            contents="""
                def fully_annotated(x: int) -> str:
                    return str(x)

                def return_annotated(x) -> str:
                    return ""
        """,
        )

    def test_statistics_without_sources(self) -> None:
        result = self.run_pyre("statistics")
        self.assert_failed(result)

    def test_statistics(self) -> None:
        result = self.run_pyre("-l", "local_project", "statistics")
        self.assert_output_matches(
            result,
            re.compile(r"\{\"annotations\":.*\"fixmes\":.*\"ignores\".*\"strict\".*\}"),
        )

    def test_collect(self) -> None:
        result = self.run_pyre(
            "-l", "local_project", "statistics", "--collect", "unstrict_files"
        )
        self.assert_output_matches(result, VALID_JSON_LIST)

        result = self.run_pyre(
            "-l", "local_project", "statistics", "--collect", "missing_annotations"
        )
        self.assert_output_matches(result, VALID_JSON_LIST)


class StopTest(TestCommand):
    def initial_filesystem(self) -> None:
        self.create_project_configuration()
        self.create_local_configuration("local_one", {"source_directories": ["."]})
        self.create_file_with_error("local_one/has_type_error.py")
        self.create_local_configuration("local_two", {"source_directories": ["."]})
        self.create_file_with_error("local_two/has_type_error.py")

    def test_stop_without_server(self) -> None:
        # TODO(T61745598): Add testing for selecting the correct server or prompting
        # user when no server is given.
        self.assert_no_servers_exist()
        result = self.run_pyre("stop")
        self.assert_succeeded(result)
        result = self.run_pyre("-l", "local_one", "stop")
        self.assert_succeeded(result)
        self.assert_no_servers_exist()

    def test_stop(self) -> None:
        self.run_pyre("-l", "local_one", "start")
        self.assert_server_exists("local_one")
        self.run_pyre("-l", "local_two", "stop")
        self.assert_server_exists("local_one")
        self.run_pyre("-l", "local_one", "stop")
        self.assert_no_servers_exist()

        self.run_pyre("-l", "local_one", "restart")
        self.assert_server_exists("local_one")
        self.run_pyre("-l", "local_one", "stop")
        self.assert_no_servers_exist()

        self.run_pyre("-l", "local_one", "start")
        self.run_pyre("-l", "local_two", "start")
        self.run_pyre("-l", "local_one", "stop")
        self.assert_server_exists("local_two")
        self.assert_server_does_not_exist("local_one")


if __name__ == "__main__":
    unittest.main()
