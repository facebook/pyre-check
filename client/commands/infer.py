# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import functools
import json
import logging
import multiprocessing
import os
import re
import shutil
import subprocess
import sys
from collections import defaultdict
from dataclasses import dataclass
from logging import Logger
from pathlib import Path
from typing import IO, Dict, List, Optional, Sequence, Set, Union

import libcst
from libcst.codemod import CodemodContext
from libcst.codemod.visitors._apply_type_annotations import ApplyTypeAnnotationsVisitor
from typing_extensions import Final

from .. import command_arguments, log
from ..analysis_directory import AnalysisDirectory
from ..annotation_collector import AnnotationCollector
from ..configuration import Configuration
from ..error import LegacyError
from .command import Command, Result
from .reporting import Reporting
from .statistics import _get_paths, _parse_paths, parse_path_to_module


LOG: Logger = logging.getLogger(__name__)


class AnnotationFixer(libcst.CSTTransformer):
    def leave_Subscript(
        self,
        original_node: libcst.Subscript,
        updated_node: Union[libcst.Subscript, libcst.SimpleString],
    ) -> Union[libcst.Subscript, libcst.SimpleString]:
        if libcst.matchers.matches(
            original_node.value, libcst.matchers.Name("PathLike")
        ):
            name_node = libcst.Attribute(
                value=libcst.Name(
                    value="os",
                    lpar=[],
                    rpar=[],
                ),
                attr=libcst.Name(value="PathLike"),
            )
            node_as_string = libcst.parse_module("").code_for_node(
                updated_node.with_changes(value=name_node)
            )
            updated_node = libcst.SimpleString(f"'{node_as_string}'")
        return updated_node


def dequalify_and_fix_pathlike(annotation: str) -> str:
    if annotation.find("PathLike") >= 0:
        try:
            tree = libcst.parse_module(annotation)
            annotation = tree.visit(AnnotationFixer()).code
        except libcst._exceptions.ParserSyntaxError:
            pass

    return annotation.replace("typing.", "")


def split_imports(types_list: List[str]) -> Set[str]:
    typing_imports = set()
    for full_type in types_list:
        if full_type:
            split_type = re.findall(r"[\w]+", full_type)
            if len(split_type) > 1 and split_type[0] == "typing":
                typing_imports.add(split_type[1])
    return typing_imports


def _relativize_access(access, path) -> List[str]:
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

    def _get_name(self) -> str:
        """The last part of the access path is the function name"""
        return self.name.split(".")[-1] if self.name.split(".") else ""

    def _get_annotation(self) -> str:
        return " -> " + dequalify_and_fix_pathlike(self.actual) if self.actual else ""

    def _get_parameter_string(self) -> str:
        """Depending on if an argument has a type, the style for default values
        changes. E.g.
           def fun(x=5)
           def fun(x : int = 5)
        """
        parameters = []
        for parameter in self.parameters:
            name = parameter["name"]
            if parameter["type"]:
                name += ": " + dequalify_and_fix_pathlike(parameter["type"])
                if parameter["value"]:
                    name += " = " + parameter["value"]
            elif parameter["value"]:
                name += "=" + parameter["value"]
            parameters.append(name)
        return ", ".join(parameters)

    def _get_decorator_string(self) -> str:
        decorator_string = ""
        for decorator in self.decorators:
            decorator_string += f"@{decorator}\n"
        return decorator_string

    def _get_async_string(self) -> str:
        return "async " if self.is_async else ""

    def is_complete(self) -> bool:
        """Determines if a stub completely types a function"""
        if not self.actual:
            return False
        for parameter in self.parameters:
            if parameter["name"] != "self" and not parameter["type"]:
                return False
        return True

    def to_string(self) -> str:
        # lint-fixme: UseFstringRule
        return "{}{}def {}({}){}: ...".format(
            self._get_decorator_string(),
            self._get_async_string(),
            self._get_name(),
            self._get_parameter_string(),
            self._get_annotation(),
        )

    @functools.lru_cache(maxsize=1)
    def get_typing_imports(self) -> Set[str]:
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

    def _get_name(self) -> str:
        """The last part of the access path is the function name"""
        return self.name.split(".")[-1] if self.name.split(".") else ""

    def to_string(self) -> str:
        return f"{self._get_name()}: {dequalify_and_fix_pathlike(self.actual)} = ..."  # noqa

    @functools.lru_cache(maxsize=1)
    def get_typing_imports(self) -> Set[str]:
        return split_imports(re.split("[^\\w.]+", self.actual))


class Stub:
    stub: Optional[Union[FieldStub, FunctionStub]] = None

    def __init__(self, error) -> None:
        self.path = Path(error.error.path)
        self.parent = error.inference.get("parent")
        self.stub = None
        if FunctionStub.is_instance(error.inference):
            self.stub = FunctionStub(error.inference)
        elif FieldStub.is_instance(error.inference):
            self.stub = FieldStub(error.inference)

    def is_function(self) -> bool:
        return isinstance(self.stub, FunctionStub) and not self.parent

    def is_method(self) -> bool:
        return isinstance(self.stub, FunctionStub) and self.parent

    def is_field(self) -> bool:
        return isinstance(self.stub, FieldStub)

    def is_complete(self) -> bool:
        return isinstance(self.stub, FieldStub) or (
            isinstance(self.stub, FunctionStub)
            # pyre-fixme[16]: `Optional` has no attribute `is_complete`.
            and self.stub.is_complete()
        )

    def to_string(self) -> str:
        # pyre-fixme[16]: `Optional` has no attribute `to_string`.
        return self.stub.to_string()

    def get_typing_imports(self) -> Set[str]:
        # pyre-fixme[16]: `Optional` has no attribute `get_typing_imports`.
        return self.stub.get_typing_imports()

    def join_with(self, other) -> None:
        stub = self.stub
        if not self.is_field() and not other.is_field() and stub:
            # pyre-fixme[16]: `None` has no attribute `join_with`.
            stub.join_with(other.stub)
        else:
            raise Exception("Tried to join incompatible stubs")


def join_stubs(stubs) -> List[Stub]:
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
        self._path = Path(errors[0].error.path)

    def to_string(self) -> str:
        """We currently ignore nested classes, i.e.:
        class X:
            class Y:
                [ALL OF THIS IS IGNORED]
        """
        classes = defaultdict(list)
        typing_imports = set()
        alphabetical_imports = []
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
            contents += f"\nclass {parent}:\n"
            for stub in stubs:
                stubs_in_file.append(stub)
                tabbed_stub = stub.to_string().replace("\n", "\n    ")
                contents += f"    {tabbed_stub}\n"

        for stub in stubs_in_file:
            typing_imports.update(stub.get_typing_imports())
            alphabetical_imports = sorted(typing_imports)

        if alphabetical_imports and contents != "":
            typing_imports = ", ".join(
                str(type_import) for type_import in alphabetical_imports
            )
            contents = f"from typing import {typing_imports}\n\n{contents}"
        return contents

    def is_empty(self) -> bool:
        return self._stubs == []

    def path(self, directory) -> Path:
        return directory / Path(f"{self._path}i")

    def output_to_file(self, path) -> None:
        contents = self.to_string()
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(contents)


def generate_stub_files(
    full_only: bool, errors: Sequence[LegacyError]
) -> List[StubFile]:
    errors = [error for error in errors if error.inference]
    files = defaultdict(list)
    errors.sort(key=lambda error: error.error.line)

    for error in errors:
        files[error.error.path].append(error)

    stubs = []
    for _path, errors in files.items():
        stub = StubFile(errors, full_only=full_only)
        if not stub.is_empty():
            stubs.append(stub)
    return stubs


def write_stubs_to_disk(
    stubs,
    type_directory: Path,
) -> None:
    if type_directory.exists():
        LOG.log(log.SUCCESS, f"Deleting {type_directory}")
        shutil.rmtree(type_directory)
    type_directory.mkdir(parents=True, exist_ok=True)

    LOG.log(log.SUCCESS, f"Outputting inferred stubs to {type_directory}")
    for stub in stubs:
        stub.output_to_file(stub.path(type_directory))


def filter_paths(
    stubs: Sequence[StubFile], type_directory: Path, in_place: Sequence[str]
) -> List[StubFile]:
    unused_annotates = [
        path
        for path in in_place
        if all(not str(stub.path(Path(""))).startswith(str(path)) for stub in stubs)
    ]
    for path in unused_annotates:
        LOG.log(log.SUCCESS, f"No annotations for {path}")

    return [
        stub
        for stub in stubs
        if any(str(stub.path(Path(""))).startswith(str(path)) for path in in_place)
    ]


def _parse(file: IO[str]) -> libcst.Module:
    contents = file.read()
    return libcst.parse_module(contents)


def apply_stub_annotations(stub_path: str, file_path: str) -> str:
    with open(stub_path) as stub_file, open(file_path) as source_file:
        stub = _parse(stub_file)
        source = _parse(source_file)
        context = CodemodContext()
        ApplyTypeAnnotationsVisitor.store_stub_in_context(context, stub)
        modified_tree = ApplyTypeAnnotationsVisitor(context).transform_module(source)
        return modified_tree.code


def annotate_path(stub_path: str, file_path: str, debug_infer: bool) -> None:
    try:
        annotated_content = apply_stub_annotations(stub_path, file_path)
        with open(file_path, "w") as source_file:
            source_file.write(annotated_content)
        LOG.info(f"Annotated {file_path}")
    except Exception as error:
        LOG.warning(f"Failed to annotate {file_path}")
        if debug_infer:
            LOG.warning(f"\tError: {error}")


@dataclass
class AnnotatePathArguments:
    stub_path: str
    file_path: str
    debug_infer: bool


def annotate_path_from_arguments(arguments: AnnotatePathArguments) -> None:
    annotate_path(arguments.stub_path, arguments.file_path, arguments.debug_infer)


def _existing_annotations_as_errors(
    modules: Dict[Path, Optional[libcst.Module]], project_root: str
) -> List[LegacyError]:
    errors = []
    for path_name, module in modules.items():
        path_name = str(path_name)
        path = path_name.replace(".py", "").replace(project_root + "/", "")
        collector = AnnotationCollector(path)
        if module is not None:
            module.visit(collector)
        for stub in collector.stubs:
            errors.append(
                LegacyError.create(
                    {
                        "path": path_name.replace(project_root + "/", ""),
                        "inference": stub,
                        "line": 0,
                        "column": 0,
                        "stop_line": 0,
                        "stop_column": 0,
                        "code": 0,
                        "name": "example_string",
                        "description": "example description",
                    }
                )
            )
    return errors


class Infer(Reporting):
    NAME = "infer"

    def __init__(
        self,
        arguments: command_arguments.CommandArguments,
        original_directory: str,
        *,
        configuration: Configuration,
        analysis_directory: Optional[AnalysisDirectory] = None,
        print_errors: bool,
        full_only: bool,
        recursive: bool,
        in_place: Optional[List[str]],
        errors_from_stdin: bool,
        annotate_from_existing_stubs: bool,
        debug_infer: bool,
        full_stub_paths: Optional[List[str]],
    ) -> None:
        super(Infer, self).__init__(
            arguments, original_directory, configuration, analysis_directory
        )
        self._print_errors = print_errors
        self._full_only = full_only
        self._recursive = recursive
        self._in_place: Final[Optional[List[str]]] = in_place
        self._errors_from_stdin = errors_from_stdin
        self._annotate_from_existing_stubs = annotate_from_existing_stubs
        self._debug_infer = debug_infer
        self._ignore_infer: List[
            str
        ] = self._configuration.get_existent_ignore_infer_paths()
        self._full_stub_paths: Final[Optional[List[str]]] = full_stub_paths

        self._show_error_traces = True
        self._output = command_arguments.JSON

    def run(self) -> Command:
        if self._annotate_from_existing_stubs:
            if self._in_place is None:
                raise ValueError(
                    "--annotate-from-existing-stubs cannot be used without the \
                    --in-place argument"
                )

            type_directory = Path(
                os.path.join(self._configuration.log_directory, "types")
            )
            self._annotate_from_stubs(
                Path(self._original_directory),
                self._configuration.formatter,
                type_directory,
                self._in_place,
                self._debug_infer,
            )
            return self
        if self._full_stub_paths is not None:
            if self._full_stub_paths:
                paths = _parse_paths([Path(path) for path in self._full_stub_paths])
            else:
                paths = _get_paths(Path(self._original_directory))
            modules = {path: parse_path_to_module(path) for path in paths}
            errors = _existing_annotations_as_errors(
                modules, self._configuration.project_root
            )
            type_directory = Path(
                os.path.join(self._configuration.log_directory, "types")
            )
            stubs = generate_stub_files(full_only=False, errors=errors)
            write_stubs_to_disk(stubs, type_directory)
            return self
        if self._errors_from_stdin:
            result = self._get_errors_from_stdin()
            from_stdin = True
        else:
            self._analysis_directory.prepare()
            result = self._call_client(command=Infer.NAME)
            from_stdin = False
        errors = self._get_errors(result, bypass_filtering=True, from_stdin=from_stdin)
        if self._print_errors:
            self._print(errors)
        else:
            type_directory = Path(
                os.path.join(self._configuration.log_directory, "types")
            )
            stubs = generate_stub_files(self._full_only, errors)
            write_stubs_to_disk(stubs, type_directory)
            if self._in_place is not None:
                LOG.info("Annotating files")
                self._annotate_paths(
                    self._configuration.local_root,
                    self._configuration.formatter,
                    stubs,
                    type_directory,
                    self._in_place,
                    self._debug_infer,
                )

        return self

    def _flags(self) -> List[str]:
        flags = super()._flags()
        filter_directories = self._get_directories_to_analyze()
        filter_directories.update(
            set(self._configuration.get_existent_do_not_ignore_errors_in_paths())
        )
        if len(filter_directories):
            flags.extend(["-filter-directories", ";".join(sorted(filter_directories))])

        search_path = [
            search_path.command_line_argument()
            for search_path in self._configuration.get_existent_search_paths()
        ]
        if search_path:
            flags.extend(["-search-path", ",".join(search_path)])

        excludes = self._configuration.excludes
        for exclude in excludes:
            flags.extend(["-exclude", exclude])

        if len(self._ignore_infer) > 0:
            flags.extend(["-ignore-infer", ";".join(self._ignore_infer)])
        return flags

    def _get_errors(
        self, result: Result, bypass_filtering: bool = False, from_stdin: bool = False
    ) -> Sequence[LegacyError]:
        analysis_root = os.path.realpath(self._analysis_directory.get_root())
        relative_root = self._original_directory if from_stdin else analysis_root
        errors = self._relativize_errors(relative_root, self._parse_raw_errors(result))

        if bypass_filtering:
            return errors
        else:
            return self._filter_errors(errors)

    def _get_errors_from_stdin(self) -> Result:
        input = sys.stdin.read()
        return Result(0, json.dumps({"errors": json.loads(input)}))

    def _clean_errors(self) -> None:
        result = self._call_client(command="check")
        errors = self._get_errors(result)
        error_json = json.dumps([error.to_json() for error in errors])
        subprocess.run("pyre-upgrade fixme", input=error_json)

    def _annotate_paths(
        self,
        root,
        formatter: Optional[str],
        stubs: Sequence[StubFile],
        type_directory: Path,
        in_place: Sequence[str],
        debug_infer: bool,
    ) -> None:
        if in_place != []:
            stubs = filter_paths(stubs, type_directory, in_place)

        tasks = []
        for stub in stubs:
            stub_path = stub.path(type_directory)
            if not stub._path.resolve().exists():
                file_path = (root / stub._path).resolve()
            else:
                file_path = stub._path.resolve()
            tasks.append(AnnotatePathArguments(str(stub_path), file_path, debug_infer))

        number_workers = self._configuration.get_number_of_workers()
        with multiprocessing.Pool(number_workers) as pool:
            for _ in pool.imap_unordered(annotate_path_from_arguments, tasks):
                pass

        if formatter:
            subprocess.call(
                formatter, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
            )
            self._clean_errors()

    def _annotate_from_stubs(
        self,
        root: Path,
        formatter: Optional[str],
        type_directory: Path,
        in_place: Sequence[str],
        debug_infer: bool,
    ) -> None:
        in_place_paths = [Path(path) for path in in_place]
        for stub_path in type_directory.rglob("*.pyi"):
            relative_source_path_for_stub = stub_path.relative_to(
                type_directory
            ).with_suffix(".py")

            if in_place_paths == [] or any(
                path
                in (
                    relative_source_path_for_stub,
                    *relative_source_path_for_stub.parents,
                )
                for path in in_place_paths
            ):
                annotate_path(
                    str(stub_path),
                    str(root / relative_source_path_for_stub),
                    debug_infer,
                )
        if formatter:
            subprocess.call(
                formatter, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
            )
            self._clean_errors()
