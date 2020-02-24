# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import argparse
import functools
import logging
import os
import re
import shutil
import subprocess
import sys
from collections import defaultdict
from logging import Logger
from pathlib import Path
from typing import Any, List, Optional, Set, Union

from .. import apply_annotations, log
from ..analysis_directory import AnalysisDirectory
from ..configuration import Configuration
from .check import Check
from .command import JSON, Command, Result, typeshed_search_path
from .reporting import Reporting


LOG: Logger = logging.getLogger(__name__)


def dequalify(annotation):
    return annotation.replace("typing.", "")


def split_imports(types_list) -> Set[Any]:
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
    def is_instance(stub) -> bool:
        required_fields = ["parameters", "decorators", "async", "function_name"]
        return all(field in stub.keys() for field in required_fields)

    def _get_name(self):
        """ The last part of the access path is the function name """
        return self.name.split(".")[-1] if self.name.split(".") else ""

    def _get_annotation(self) -> str:
        return " -> " + dequalify(self.actual) if self.actual else ""

    def _get_parameter_string(self) -> str:
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

    def to_string(self) -> str:
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
        self.name = stub.get("attribute_name")
        self.actual = stub.get("annotation")

    @staticmethod
    def is_instance(stub) -> bool:
        required_fields = ["annotation", "attribute_name"]
        return all(field in stub.keys() for field in required_fields)

    def _get_name(self):
        """ The last part of the access path is the function name """
        return self.name.split(".")[-1] if self.name.split(".") else ""

    def to_string(self) -> str:
        return "{}: {} = ...".format(self._get_name(), dequalify(self.actual))

    @functools.lru_cache(maxsize=1)
    def get_typing_imports(self):
        return split_imports(re.split("[^\\w.]+", self.actual))


class Stub:
    stub: Optional[Union[FieldStub, FunctionStub]] = None

    def __init__(self, error) -> None:
        self.path = Path(error.path)
        self.parent = error.inference.get("parent")
        self.stub = None
        if FunctionStub.is_instance(error.inference):
            self.stub = FunctionStub(error.inference)
        elif FieldStub.is_instance(error.inference):
            self.stub = FieldStub(error.inference)

    def is_function(self) -> bool:
        return isinstance(self.stub, FunctionStub) and not self.parent

    def is_method(self):
        return isinstance(self.stub, FunctionStub) and self.parent

    def is_field(self) -> bool:
        return isinstance(self.stub, FieldStub)

    def is_complete(self) -> bool:
        return isinstance(self.stub, FieldStub) or (
            isinstance(self.stub, FunctionStub)
            # pyre-fixme[16]: `Optional` has no attribute `is_complete`.
            and self.stub.is_complete()
        )

    def to_string(self):
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
        stubs_in_file = []
        for stub in self._fields:
            parent = _relativize_access(stub.parent, stub.path)
            # Ignore nested classes
            if len(parent) == 1:
                classes[parent[0]].append(stub)
            else:
                stubs_in_file.append(stub)
                contents += stub.to_string() + "\n"

        for stub in self._methods:
            parent = _relativize_access(stub.parent, stub.path)
            # Ignore nested classes
            if len(parent) == 1:
                classes[parent[0]].append(stub)

        for stub in self._functions:
            stubs_in_file.append(stub)
            contents += stub.to_string() + "\n"

        for parent, stubs in classes.items():
            contents += "\nclass {}:\n".format(parent)
            for stub in stubs:
                stubs_in_file.append(stub)
                contents += "    {}\n".format(stub.to_string().replace("\n", "\n    "))

        for stub in stubs_in_file:
            typing_imports.update(stub.get_typing_imports())
            alphabetical_imports = sorted(list(typing_imports))
            if alphabetical_imports and contents != "":
                contents = (
                    "from typing import {}\n\n".format(
                        ", ".join(
                            str(type_import) for type_import in alphabetical_imports
                        )
                    )
                    + contents
                )
        return contents

    def is_empty(self):
        return self._stubs == []

    def path(self, directory):
        return directory / Path("{}i".format(self._path))

    def output_to_file(self, path) -> None:
        contents = self.to_string()
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(contents)


def generate_stub_files(arguments, errors) -> List[StubFile]:
    errors = [
        error
        for error in errors
        if error.inference and not (error.is_external_to_global_root())
    ]
    files = defaultdict(list)
    errors.sort(key=lambda error: error.line)

    for error in errors:
        files[error.path].append(error)

    stubs = []
    for _path, errors in files.items():
        stub = StubFile(errors, full_only=arguments.full_only)
        if not stub.is_empty():
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


def annotate_path(arguments, stub_path: str, file_path: str) -> None:
    try:
        annotated_content = apply_annotations.apply_stub_annotations(
            stub_path, file_path
        )
        with open(file_path, "w") as source_file:
            source_file.write(annotated_content)
        LOG.info("Annotated {}".format(file_path))
    except Exception as error:
        LOG.warning("Failed to annotate {}".format(file_path))
        if arguments.debug_infer:
            LOG.warning("\tError: {}".format(error))


def annotate_paths(
    root, arguments, formatter: Optional[str], stubs, type_directory
) -> None:
    if arguments.in_place != []:
        stubs = filter_paths(arguments, stubs, type_directory)

    for stub in stubs:
        stub_path = stub.path(type_directory)
        if not stub._path.resolve().exists():
            file_path = (root / stub._path).resolve()
        else:
            file_path = stub._path.resolve()
        annotate_path(arguments, stub_path, file_path)
    if formatter:
        subprocess.call(formatter, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)


def annotate_from_existing_stubs(
    root: Path,
    arguments: argparse.Namespace,
    formatter: Optional[str],
    type_directory: Path,
) -> None:
    in_place_paths = [Path(path) for path in arguments.in_place]
    for stub_path in type_directory.rglob("*.pyi"):
        relative_source_path_for_stub = stub_path.relative_to(
            type_directory
        ).with_suffix(".py")

        if in_place_paths == [] or any(
            path
            in (relative_source_path_for_stub, *relative_source_path_for_stub.parents)
            for path in in_place_paths
        ):
            annotate_path(
                arguments, str(stub_path), str(root / relative_source_path_for_stub)
            )
    if formatter:
        subprocess.call(formatter, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)


def file_exists(path):
    if not os.path.exists(path):
        raise argparse.ArgumentTypeError("ERROR: " + str(path) + " does not exist")
    return path


class Infer(Reporting):
    NAME = "infer"

    def __init__(
        self,
        arguments,
        original_directory: str,
        configuration: Optional[Configuration] = None,
        analysis_directory: Optional[AnalysisDirectory] = None,
    ) -> None:
        arguments.show_error_traces = True
        arguments.output = JSON
        super(Infer, self).__init__(
            arguments, original_directory, configuration, analysis_directory
        )
        self._print_errors: bool = arguments.print_only
        self._full_only: bool = arguments.full_only
        self._recursive: bool = arguments.recursive
        self._in_place: bool = arguments.in_place
        self._json: bool = arguments.json
        self._annotate_from_existing_stubs: bool = arguments.annotate_from_existing_stubs
        self._debug_infer: bool = arguments.debug_infer
        self._ignore_infer: List[str] = self._configuration.ignore_infer

    @classmethod
    def add_subparser(cls, parser: argparse._SubParsersAction) -> None:
        infer = parser.add_parser(cls.NAME)
        infer.set_defaults(command=cls)
        infer.add_argument(
            "-p",
            "--print-only",
            action="store_true",
            help="Print raw JSON errors to standard output, "
            + "without converting to stubs or annnotating.",
        )
        infer.add_argument(
            "-f",
            "--full-only",
            action="store_true",
            help="Only output fully annotated functions. Requires infer flag.",
        )
        infer.add_argument(
            "-r",
            "--recursive",
            action="store_true",
            help="Recursively run infer until no new annotations are generated."
            + " Requires infer flag.",
        )
        infer.add_argument(
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
        infer.add_argument(
            "--json",
            action="store_true",
            help="Accept JSON input instead of running full check.",
        )
        infer.add_argument(
            "--annotate-from-existing-stubs",
            action="store_true",
            help="Add annotations from existing stubs.",
        )
        infer.add_argument(
            "--debug-infer",
            action="store_true",
            help="Print error message when file fails to annotate.",
        )

    def run(self) -> Command:
        self._analysis_directory.prepare()
        if self._annotate_from_existing_stubs:
            if self._arguments.in_place is None:
                raise argparse.ArgumentTypeError(
                    "--annotate-from-existing-stubs cannot be used without the \
                    --in-place argument"
                )

            type_directory = Path(os.path.join(self._log_directory, "types"))
            annotate_from_existing_stubs(
                Path(self._original_directory),
                self._arguments,
                self._formatter,
                type_directory,
            )
            return self
        if self._json:
            result = self._errors_from_stdin()
            errors = self._get_errors(result, bypass_filtering=True)
        else:
            result = self._call_client(command=Infer.NAME)
            errors = self._get_errors(result, bypass_filtering=True)
        if self._print_errors:
            self._print(errors)
        else:
            type_directory = Path(os.path.join(self._log_directory, "types"))
            stubs = generate_stub_files(self._arguments, errors)
            write_stubs_to_disk(self._arguments, stubs, type_directory)
            if self._arguments.in_place is not None:
                LOG.info("Annotating files")
                annotate_paths(
                    self._configuration.local_configuration_root,
                    self._arguments,
                    self._formatter,
                    stubs,
                    type_directory,
                )

        return self

    def _flags(self) -> List[str]:
        flags = super()._flags()
        filter_directories = self._get_directories_to_analyze()
        if len(filter_directories):
            flags.extend(["-filter-directories", ";".join(sorted(filter_directories))])
        search_path = self._configuration.search_path + typeshed_search_path(
            self._configuration.typeshed
        )
        if search_path:
            flags.extend(["-search-path", ",".join(search_path)])
        if len(self._ignore_infer) > 0:
            flags.extend(["-ignore-infer", ";".join(self._ignore_infer)])
        return flags

    def _errors_from_stdin(self) -> Result:
        input = sys.stdin.read()
        return Result(0, input)
