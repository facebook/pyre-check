# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import functools
import logging
import os
import re
import shutil
import subprocess
import sys
import time
import traceback
from collections import defaultdict
from pathlib import Path
from typing import Optional, Union  # noqa

from . import (
    FAILURE,
    JSON,
    SUCCESS,
    EnvironmentException,
    buck,
    commands,
    get_version,
    is_capable_terminal,
    log,
    log_statistics,
    merge_source_directories,
    resolve_source_directories,
    switch_root,
)
from .configuration import Configuration


LOG = logging.getLogger(__name__)


def dequalify(annotation):
    return annotation.replace("typing.", "")


def split_imports(types_list):
    typing_imports = set()
    for full_type in types_list:
        if full_type:
            split_type = re.findall(r"[\w]+", full_type)
            if len(split_type) > 1 and split_type[0] == "typing":
                typing_imports.add(split_type[1])
    return typing_imports


def _relativize_access(access, path):
    if not access:
        return []
    path = str(path).split(".", 1)[0].replace("/", ".").replace(".__init__", "")
    return access.replace(path, "", 1).strip(".").split(".")


class FunctionStub:

    def __init__(self, stub) -> None:
        self.name = stub.get("function_name")
        self.actual = stub.get("annotation")
        self.parameters = stub.get("parameters")
        self.decorators = stub.get("decorators")
        self.is_async = stub.get("async")

    @staticmethod
    def is_instance(stub):
        required_fields = ["parameters", "decorators", "async", "function_name"]
        return all(field in stub.keys() for field in required_fields)

    def _get_name(self):
        """ The last part of the access path is the function name """
        return self.name.split(".")[-1] if self.name.split(".") else ""

    def _get_annotation(self):
        return " -> " + dequalify(self.actual) if self.actual else ""

    def _get_parameter_string(self):
        """ Depending on if an argument has a type, the style for default values
        changes. E.g.
           def fun(x=5)
           def fun(x : int = 5)
        """
        parameters = []
        for parameter in self.parameters:
            name = parameter["name"]
            if parameter["type"]:
                name += ": " + dequalify(parameter["type"])
                if parameter["value"]:
                    name += " = " + parameter["value"]
            elif parameter["value"]:
                name += "=" + parameter["value"]
            parameters.append(name)
        return ", ".join(parameters)

    def _get_decorator_string(self) -> str:
        decorator_string = ""
        for decorator in self.decorators:
            decorator_string += "@{}\n".format(decorator)
        return decorator_string

    def _get_async_string(self) -> str:
        return "async " if self.is_async else ""

    def is_complete(self) -> bool:
        """ Determines if a stub completely types a function """
        if not self.actual:
            return False
        for parameter in self.parameters:
            if parameter["name"] != "self" and not parameter["type"]:
                return False
        return True

    def to_string(self):
        return "{}{}def {}({}){}: ...".format(
            self._get_decorator_string(),
            self._get_async_string(),
            self._get_name(),
            self._get_parameter_string(),
            self._get_annotation(),
        )

    @functools.lru_cache(maxsize=1)
    def get_typing_imports(self):
        types_list = re.split("[^\\w.]+", self.actual) if self.actual else []
        for parameter in self.parameters:
            if parameter["type"]:
                types_list += re.split("[^\\w.]+", parameter["type"])
        return split_imports(types_list)

    def join_with(self, other) -> None:
        # pyre-fixme[16]: `FunctionStub` has no attribute `parent`
        if self.name != other.name and self.parent != other.parent:
            raise Exception("Tried to join incompatible stubs")
        if (not self.actual) and other.actual:
            self.actual = other.actual
        for parameter, other_parameter in zip(self.parameters, other.parameters):
            if (not parameter["type"]) and other_parameter["type"]:
                parameter["type"] = other_parameter["type"]


class FieldStub:

    def __init__(self, stub) -> None:
        self.name = stub.get("field_name")
        self.actual = stub.get("annotation")

    @staticmethod
    def is_instance(stub):
        required_fields = ["annotation", "field_name"]
        return all(field in stub.keys() for field in required_fields)

    def _get_name(self):
        """ The last part of the access path is the function name """
        return self.name.split(".")[-1] if self.name.split(".") else ""

    def to_string(self):
        return "{}: {} = ...".format(self._get_name(), dequalify(self.actual))

    @functools.lru_cache(maxsize=1)
    def get_typing_imports(self):
        return split_imports(re.split("[^\\w.]+", self.actual))


class Stub:
    stub = None  # type: Optional[Union[FieldStub, FunctionStub]]

    def __init__(self, error) -> None:
        self.path = Path(error.path)
        self.parent = error.inference.get("parent")
        self.stub = None
        if FunctionStub.is_instance(error.inference):
            self.stub = FunctionStub(error.inference)
        elif FieldStub.is_instance(error.inference):
            self.stub = FieldStub(error.inference)

    def is_function(self):
        return isinstance(self.stub, FunctionStub) and not self.parent

    def is_method(self):
        return isinstance(self.stub, FunctionStub) and self.parent

    def is_field(self):
        return isinstance(self.stub, FieldStub)

    def is_complete(self):
        return isinstance(self.stub, FieldStub) or (
            isinstance(self.stub, FunctionStub) and self.stub.is_complete()
        )

    def to_string(self):
        """ We currently ignore nested functions i.e.:
        def fun():
            [ALL FUNCTIONS AND CLASSES DEFINED IN HERE ARE IGNORED]
        """
        if (
            len(_relativize_access(self.parent, self.path)) >= 2
            or len(_relativize_access(self.stub.name, self.path)) >= 2
        ):
            return ""
        else:
            return self.stub.to_string()

    def get_typing_imports(self):
        return self.stub.get_typing_imports()

    def join_with(self, other) -> None:
        stub = self.stub
        if not self.is_field() and not other.is_field() and stub:
            stub.join_with(other.stub)
        else:
            raise Exception("Tried to join incompatible stubs")


def join_stubs(stubs):
    # Join function stubs if they have the same parent and name
    stub_map = defaultdict(list)
    new_stubs = []
    for stub in stubs:
        if stub.is_field():
            new_stubs.append(stub)
        else:
            stub_map[(stub.parent, stub.stub.name)].append(stub)

    for stubs in stub_map.values():
        new_stub = stubs[0]
        for stub in stubs[1:]:
            new_stub.join_with(stub)
        new_stubs.append(new_stub)
    return new_stubs


class StubFile:

    def __init__(self, errors, full_only: bool = False) -> None:
        stubs = [Stub(error) for error in errors if Stub(error).stub]
        stubs = join_stubs(stubs)
        if full_only:
            stubs = [stub for stub in stubs if stub.is_complete()]
        self._stubs = stubs
        self._fields = [stub for stub in stubs if stub.is_field()]
        self._functions = [stub for stub in stubs if stub.is_function()]
        self._methods = [stub for stub in stubs if stub.is_method()]
        self._path = Path(errors[0].path)

    def to_string(self) -> str:
        """We currently ignore nested classes, i.e.:
          class X:
              class Y:
                  [ALL OF THIS IS IGNORED]
        """
        classes = defaultdict(list)
        typing_imports = set()
        contents = ""

        # import necessary modules from typing
        for stub in self._stubs:
            typing_imports.update(stub.get_typing_imports())
        alphabetical_imports = sorted(list(typing_imports))
        if alphabetical_imports:
            contents += "from typing import {}\n\n".format(
                ", ".join(str(type_import) for type_import in alphabetical_imports)
            )

        for stub in self._fields:
            parent = _relativize_access(stub.parent, stub.path)
            # Ignore nested classes
            if len(parent) == 1:
                classes[parent[0]].append(stub)
            else:
                contents += stub.to_string() + "\n"

        for stub in self._methods:
            parent = _relativize_access(stub.parent, stub.path)
            # Ignore nested classes
            if len(parent) == 1:
                classes[parent[0]].append(stub)

        for stub in self._functions:
            contents += stub.to_string() + "\n"

        for parent, stubs in classes.items():
            contents += "\nclass {}:\n".format(parent)
            for stub in stubs:
                contents += "    {}\n".format(stub.to_string().replace("\n", "\n    "))
        return contents

    def is_empty(self):
        return self._stubs == []

    def path(self, directory):
        return directory / Path("{}i".format(self._path))

    def output_to_file(self, path) -> None:
        contents = self.to_string()
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(contents)


def generate_stub_files(arguments, errors):
    errors = [error for error in errors if error.inference and not error.is_external()]
    files = defaultdict(list)
    errors.sort(key=lambda error: error.line)

    for error in errors:
        files[error.path].append(error)

    stubs = []
    for path, errors in files.items():
        stub = StubFile(errors, full_only=arguments.full_only)
        if not stub.is_empty():
            path = Path(os.getcwd()) / Path(path)
            stubs.append(stub)
    return stubs


def write_stubs_to_disk(arguments, stubs, type_directory) -> None:
    if type_directory.exists():
        LOG.log(log.SUCCESS, "Deleting {}".format(type_directory))
        shutil.rmtree(type_directory)
    type_directory.mkdir(parents=True, exist_ok=True)

    LOG.log(log.SUCCESS, "Outputting inferred stubs to {}".format(type_directory))
    for stub in stubs:
        stub.output_to_file(stub.path(type_directory))


def filter_paths(arguments, stubs, type_directory):
    unused_annotates = [
        path
        for path in arguments.in_place
        if all(not str(stub.path(Path(""))).startswith(str(path)) for stub in stubs)
    ]
    for path in unused_annotates:
        LOG.log(log.SUCCESS, "No annotations for {}".format(path))

    return [
        stub
        for stub in stubs
        if any(
            str(stub.path(Path(""))).startswith(str(path))
            for path in arguments.in_place
        )
    ]


def annotate_paths(arguments, stubs, type_directory) -> None:
    if arguments.in_place != []:
        stubs = filter_paths(arguments, stubs, type_directory)

    current_directory = os.getcwd()

    for stub in stubs:
        try:
            subprocess.check_call(
                [
                    "retype",
                    "--replace-any",
                    "--quiet",
                    "--incremental",
                    "--target-dir",
                    stub.path(current_directory).parent,
                    "--pyi-dir",
                    stub.path(type_directory).parent,
                    str(stub.path(current_directory)).rstrip("i"),
                ]
            )
            LOG.info(
                "Annotated {}".format(str(stub.path(current_directory)).rstrip("i"))
            )
        except (subprocess.CalledProcessError):
            LOG.warning(
                "Failed to annotate {}".format(
                    str(stub.path(current_directory)).rstrip("i")
                )
            )
    with open(os.devnull, "w") as FNULL:
        subprocess.call(
            ["arc", "lint", "--apply-patches", "--only-changed"],
            stdout=FNULL,
            stderr=FNULL,
        )


def file_exists(path):
    if not os.path.exists(path):
        raise argparse.ArgumentTypeError("ERROR: " + str(path) + " does not exist")
    return path


class Infer(commands.command.ErrorHandling):

    def __init__(self, arguments, configuration, source_directory) -> None:
        arguments.show_error_traces = True
        arguments.output = JSON
        super(Infer, self).__init__(arguments, configuration, source_directory)
        self._recursive = arguments.recursive
        self._print_errors = arguments.print_only

    def run(self) -> None:
        flags = self._flags()
        flags.extend(["-infer"])
        if self._recursive:
            flags.append("-recursive-infer")

        result = self._call_client(command=commands.Check.NAME, flags=flags)
        errors = self._get_errors(result, exclude_dependencies=True)
        if self._print_errors:
            self._print(errors)
        else:
            type_directory = Path(os.getcwd()) / Path(".pyre/types")
            stubs = generate_stub_files(self._arguments, errors)
            write_stubs_to_disk(self._arguments, stubs, type_directory)
            if self._arguments.in_place is not None:
                LOG.info("Annotating files")
                annotate_paths(self._arguments, stubs, type_directory)


def main():
    parser = argparse.ArgumentParser(
        epilog="environment variables:  `PYRE_BINARY` overrides the pyre "
        "client binary used."
    )

    parser.add_argument("--debug", action="store_true", help="Run in debug mode")
    parser.add_argument("--sequential", action="store_true", help=argparse.SUPPRESS)
    parser.add_argument("--strict", action="store_true", help=argparse.SUPPRESS)

    parser.add_argument("--verbose", action="store_true", help="Enable verbose logging")
    parser.add_argument(
        "--noninteractive", action="store_true", help="Disable interactive logging"
    )

    parser.add_argument("--logging-sections", help="Enable sectional logging")

    parser.add_argument(
        "--version", action="store_true", help="Print the pyre version to be used"
    )

    parser.add_argument(
        "-p",
        "--print-only",
        action="store_true",
        help="Print raw JSON errors to standard output, "
        + "without converting to stubs or annnotating.",
    )
    parser.add_argument(
        "-f",
        "--full-only",
        action="store_true",
        help="Only output fully annotated functions. Requires infer flag.",
    )
    parser.add_argument(
        "-r",
        "--recursive",
        action="store_true",
        help="Recursively run infer until no new annotations are generated."
        + " Requires infer flag.",
    )
    parser.add_argument(
        "-i",
        "--in-place",
        nargs="*",
        metavar="path",
        type=file_exists,
        help="Add annotations to functions in selected paths."
        + " Takes a set of files and folders to add annotations to."
        + " If no paths are given, all functions are annotated."
        + " WARNING: Modifies original files and requires infer flag and retype",
    )

    buck_arguments = parser.add_argument_group("buck")
    buck_arguments.add_argument(
        "--build", action="store_true", help="Build all the necessary artifacts."
    )
    buck_arguments.add_argument(
        "--target", action="append", help="The buck target to check"
    )

    source_directory = parser.add_argument_group("source-directory")
    source_directory.add_argument(
        "--source-directory",
        action="append",
        help="The source directory to run the inference on.",
    )

    arguments = parser.parse_args()

    start = time.time()
    stubs = []
    error_message = ""
    try:
        exit_code = SUCCESS
        source_directories = []

        arguments.capable_terminal = is_capable_terminal()
        if arguments.debug or not arguments.capable_terminal:
            arguments.noninteractive = True

        log.initialize(arguments)
        switch_root(arguments)

        configuration = Configuration(original_directory=arguments.original_directory)

        if arguments.version:
            sys.stdout.write(get_version(configuration) + "\n")
            return SUCCESS

        configuration.validate()

        source_directories = resolve_source_directories(
            arguments, configuration, prompt=False
        )
        source_directory = merge_source_directories(source_directories)
        Infer(arguments, configuration, source_directory).run()
    except (
        buck.BuckException,
        commands.ClientException,
        EnvironmentException,
    ) as error:
        error_message = str(error)
        LOG.error(error_message)
        LOG.error("For more information, run 'pyre-infer --help'.")
        exit_code = FAILURE
    except Exception as error:
        error_message = str(error)
        LOG.error(error_message)
        LOG.error("For more information, run 'pyre-infer --help'.")
        LOG.info(traceback.format_exc())
        exit_code = FAILURE
    finally:
        if len(source_directories) > 1:
            try:
                shutil.rmtree(source_directory)
            except OSError:
                pass
        log.cleanup(arguments)
        if configuration and configuration.logger:
            log_statistics(
                "perfpipe_pyre_infer_performance",
                arguments,
                configuration,
                ints={
                    "exit_code": exit_code,
                    "runtime": int((time.time() - start) * 1000),  # ms
                    "stubs_generated": len(stubs),
                },
                normals={"error_message": error_message},
            )
        return exit_code


if __name__ == "__main__":
    sys.exit(main())
