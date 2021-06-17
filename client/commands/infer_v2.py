# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

import json
import logging
import multiprocessing
import os
import re
import shutil
import subprocess
import sys
from abc import ABC
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import cast, Dict, Final, List, Optional, Sequence

from pyre_extensions import none_throws
from typing_extensions import TypeAlias

from .. import command_arguments, log
from ..analysis_directory import AnalysisDirectory, resolve_analysis_directory
from ..configuration import Configuration
from .infer import dequalify_and_fix_pathlike, split_imports, AnnotateModuleInPlace
from .reporting import Reporting

LOG: logging.Logger = logging.getLogger(__name__)


def _sanitize_name(name: str) -> str:
    """The last part of the access path is the function/attribute name"""
    return name.split(".")[-1]


RawInferOutputDict: TypeAlias = Dict[str, List[Dict[str, object]]]


class RawInferOutput:
    """
    Class encapsulating the raw json output from infer.

    This is converted into a list[ModuleAnnotation] to produce stubs.
    """

    def __init__(self, data: RawInferOutputDict) -> None:
        self.data = data

    categories: Sequence[str] = ["globals", "attributes", "defines"]

    @staticmethod
    def empty() -> RawInferOutput:
        return cast(
            RawInferOutput, {category: [] for category in RawInferOutput.categories}
        )

    @staticmethod
    def split_by_path(infer_output: RawInferOutput) -> dict[str, RawInferOutput]:
        infer_output_by_path = defaultdict(RawInferOutput.empty)
        for category in RawInferOutput.categories:
            for annotation in infer_output[category]:
                # pyre-ignore[16]
                path = cast(str, annotation["location"]["path"])
                infer_output_by_path[path][category].append(annotation)
        return dict(infer_output_by_path)

    def __getitem__(self, attribute: str) -> list[Dict[str, object]]:
        return self.data[attribute]


@dataclass(frozen=True)
class TypeAnnotation:
    annotation: str | None

    @staticmethod
    def from_raw(annotation: object) -> TypeAnnotation:
        if annotation is None or isinstance(annotation, str):
            return TypeAnnotation(annotation=annotation)
        else:
            raise ValueError("Expected str | None for annotation")

    def sanitized(self, prefix: str = "") -> str:
        if self.annotation is None:
            return ""
        else:
            return prefix + dequalify_and_fix_pathlike(self.annotation)

    def split(self) -> list[str]:
        """Split an annotation into its tokens"""
        if self.annotation is None or self.annotation == "":
            return []
        else:
            return re.split("[^\\w.]+", self.annotation)

    @property
    def missing(self) -> bool:
        return self.annotation is None


@dataclass(frozen=True)
class Parameter:
    name: str
    annotation: TypeAnnotation
    value: str | None

    def to_stub(self) -> str:
        delimiter = "=" if self.annotation.missing else " = "
        value = f"{delimiter}{self.value}" if self.value else ""
        return f"{self.name}{self.annotation.sanitized(prefix=': ')}{value}"


@dataclass(frozen=True)
class FunctionAnnotation:
    name: str
    return_annotation: TypeAnnotation
    parameters: Sequence[Parameter]
    decorators: Sequence[str]
    is_async: bool

    def to_stub(self) -> str:
        name = _sanitize_name(self.name)
        decorators = "".join(f"@{decorator}\n" for decorator in self.decorators)
        async_ = "async " if self.is_async else ""
        parameters = ", ".join(parameter.to_stub() for parameter in self.parameters)
        return_ = self.return_annotation.sanitized(prefix=" -> ")
        return f"{decorators}{async_}def {name}({parameters}){return_}: ..."

    def typing_imports(self) -> set[str]:
        return split_imports(
            self.return_annotation.split()
            + [
                split
                for splits in (
                    parameter.annotation.split() for parameter in self.parameters
                )
                for split in splits
            ]
        )


@dataclass(frozen=True)
class MethodAnnotation(FunctionAnnotation):
    parent: str


@dataclass(frozen=True)
class FieldAnnotation(ABC):
    name: str
    annotation: TypeAnnotation

    def __post_init__(self) -> None:
        if self.annotation.missing:
            raise RuntimeError(f"Illegal missing FieldAnnotation for {self.name}")

    def to_stub(self) -> str:
        name = _sanitize_name(self.name)
        return f"{name}: {self.annotation.sanitized()} = ..."

    def typing_imports(self) -> set[str]:
        return split_imports(self.annotation.split())


@dataclass(frozen=True)
class GlobalAnnotation(FieldAnnotation):
    ...


@dataclass(frozen=True)
class AttributeAnnotation(FieldAnnotation):
    parent: str


@dataclass
class ModuleAnnotations:
    path: str
    globals_: list[GlobalAnnotation]
    attributes: list[AttributeAnnotation]
    functions: list[FunctionAnnotation]
    methods: list[MethodAnnotation]

    @staticmethod
    def empty(path: str) -> ModuleAnnotations:
        return ModuleAnnotations(
            path=path, globals_=[], attributes=[], functions=[], methods=[]
        )

    @staticmethod
    def from_infer_output(
        path: str,
        infer_output: RawInferOutput,
    ) -> ModuleAnnotations:
        return ModuleAnnotations(
            path=path,
            globals_=[
                GlobalAnnotation(
                    name=cast(str, global_["name"]),
                    annotation=TypeAnnotation.from_raw(global_["annotation"]),
                )
                for global_ in infer_output["globals"]
            ],
            attributes=[
                AttributeAnnotation(
                    parent=cast(str, attribute["parent"]),
                    name=cast(str, attribute["name"]),
                    annotation=TypeAnnotation.from_raw(attribute["annotation"]),
                )
                for attribute in infer_output["attributes"]
            ],
            functions=[
                FunctionAnnotation(
                    name=cast(str, define["name"]),
                    return_annotation=TypeAnnotation.from_raw(define.get("return")),
                    parameters=[
                        Parameter(
                            name=cast(str, parameter["name"]),
                            annotation=TypeAnnotation.from_raw(parameter["annotation"]),
                            value=cast(Optional[str], parameter.get("value")),
                        )
                        # pyre-ignore [16]
                        for parameter in define["parameters"]
                    ],
                    decorators=cast(Sequence[str], define["decorators"]),
                    is_async=cast(bool, define["async"]),
                )
                for define in infer_output["defines"]
                if define.get("parent") is None
            ],
            methods=[
                MethodAnnotation(
                    parent=cast(str, define["parent"]),
                    name=cast(str, define["name"]),
                    return_annotation=TypeAnnotation.from_raw(define.get("return")),
                    parameters=[
                        Parameter(
                            name=cast(str, parameter["name"]),
                            annotation=TypeAnnotation.from_raw(parameter["annotation"]),
                            value=cast(Optional[str], parameter.get("value")),
                        )
                        for parameter in define["parameters"]
                    ],
                    decorators=cast(Sequence[str], define["decorators"]),
                    is_async=cast(bool, define["async"]),
                )
                for define in infer_output["defines"]
                if define.get("parent") is not None
            ],
        )

    @property
    def classes(self) -> dict[str, list[AttributeAnnotation | MethodAnnotation]]:
        """
        Find all classes with attributes or methods to annotate.

        Anything in nested classes is currently ignored, e.g.:
        ```
        class X:
            class Y:
                [ALL OF THIS IS IGNORED]
        ```
        """
        classes = defaultdict(list)
        for attribute in [*self.attributes, *self.methods]:
            parent = self._relativize(attribute.parent)
            if len(parent) != 1:
                LOG.warning("Unexpected parent with length != 1: %r", parent)
            else:
                classes[parent[0]].append(attribute)
        return classes

    def _relativize(self, parent: str) -> Sequence[str]:
        path = (
            str(self.path).split(".", 1)[0].replace("/", ".").replace(".__init__", "")
        )
        return parent.replace(path, "", 1).strip(".").split(".")

    def stubs_path(self, directory: Path) -> Path:
        return directory / Path(f"{self.path}i")

    def is_empty(self) -> bool:
        return (
            len(self.globals_)
            + len(self.attributes)
            + len(self.functions)
            + len(self.methods)
        ) == 0

    def to_stubs(self) -> str:
        """
        Output annotation information as a stub file.
        """
        return "\n".join(
            [
                self._typing_imports(),
                *(global_.to_stub() for global_ in self.globals_),
                *(function.to_stub() for function in self.functions),
                *(
                    self._class_stub(classname, annotations)
                    for classname, annotations in self.classes.items()
                ),
                "",
            ]
        )

    def write_stubs(self, type_directory: Path) -> None:
        path = self.stubs_path(type_directory)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(self.to_stubs())

    def _typing_imports(self) -> str:
        all_imports = (
            typing_import
            for by_category in [
                (global_.typing_imports() for global_ in self.globals_),
                (function.typing_imports() for function in self.functions),
                (
                    attribute.typing_imports()
                    for class_attributes in self.classes.values()
                    for attribute in class_attributes
                ),
            ]
            for by_annotation in by_category
            for typing_import in by_annotation
        )
        imports = sorted(set(all_imports))
        return "" if imports == [] else f"from typing import {', '.join(imports)}\n\n"

    def _class_stub(
        self,
        classname: str,
        annotations: Sequence[AttributeAnnotation | MethodAnnotation],
    ) -> str:
        return f"class {classname}:\n" + "\n".join(
            self._indent(annotation.to_stub()) for annotation in annotations
        )

    @staticmethod
    def _indent(stub: str) -> str:
        return "    " + stub.replace("\n", "\n    ")


def _create_module_annotations(
    infer_output: RawInferOutput,
) -> Sequence[ModuleAnnotations]:
    infer_output_by_path = RawInferOutput.split_by_path(
        infer_output=infer_output,
    )
    return [
        ModuleAnnotations.from_infer_output(
            path=path,
            infer_output=infer_output_for_path,
        )
        for path, infer_output_for_path in infer_output_by_path.items()
    ]


class Infer(Reporting):
    NAME = "infer"

    def __init__(
        self,
        command_arguments: command_arguments.CommandArguments,
        original_directory: str,
        *,
        configuration: Configuration,
        analysis_directory: AnalysisDirectory | None = None,
        print_only: bool,
        in_place_paths: Optional[List[str]],
        annotate_from_existing_stubs: bool,
        debug_infer: bool,
        read_stdin: bool,
    ) -> None:
        if annotate_from_existing_stubs and in_place_paths is None:
            raise ValueError(
                "--annotate-from-existing-stubs cannot be used without the \
                --in-place argument"
            )
        super(Infer, self).__init__(
            command_arguments,
            original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
        )
        self._print_only = print_only
        self._in_place_paths: Final[Optional[List[str]]] = in_place_paths
        self._annotate_from_existing_stubs = annotate_from_existing_stubs
        self._debug_infer = debug_infer
        self._read_stdin = read_stdin
        self._root_type_directory: Path = Path(
            os.path.join(self._configuration.log_directory, "types")
        )
        self._type_directory: Path = (
            self._root_type_directory
            if self._configuration.relative_local_root is None
            else self._root_type_directory
            / none_throws(self._configuration.relative_local_root)
        )

    def generate_analysis_directory(self) -> AnalysisDirectory:
        return resolve_analysis_directory(
            self._command_arguments.source_directories,
            self._command_arguments.targets,
            self._configuration,
            self._original_directory,
            filter_directory=self._command_arguments.filter_directory,
            isolate=True,
        )

    def _flags(self) -> list[str]:
        flags = super()._flags()

        flags.append("-use-v2")

        filter_directories = self._analysis_directory.get_filter_roots()
        filter_directories.update(
            set(self._configuration.get_existent_do_not_ignore_errors_in_paths())
        )
        if len(filter_directories):
            flags.extend(["-filter-directories", ";".join(sorted(filter_directories))])

        search_path = [
            search_path.command_line_argument()
            for search_path in (
                self._configuration.expand_and_get_existent_search_paths()
            )
        ]
        if search_path:
            flags.extend(["-search-path", ",".join(search_path)])

        excludes = self._configuration.excludes
        for exclude in excludes:
            flags.extend(["-exclude", exclude])

        ignore_infer = self._configuration.get_existent_ignore_infer_paths()
        if len(ignore_infer) > 0:
            flags.extend(["-ignore-infer", ";".join(ignore_infer)])

        return flags

    def _run(self, retries: int = 1) -> None:
        if self._annotate_from_existing_stubs:
            return self._annotate_in_place()
        infer_output = RawInferOutput(data=json.loads(self._load_infer_output())[0])
        module_annotations = _create_module_annotations(infer_output=infer_output)
        if self._print_only:
            return self._print_inferences(
                infer_output=infer_output, module_annotations=module_annotations
            )
        self._write_stubs(module_annotations=module_annotations)

    def _load_infer_output(self) -> str:
        if self._read_stdin:
            return sys.stdin.read()
        else:
            self._analysis_directory.prepare()
            result = self._call_client(command=self.NAME)
            result.check()
            return result.output

    @staticmethod
    def _print_inferences(
        infer_output: RawInferOutput, module_annotations: Sequence[ModuleAnnotations]
    ) -> None:
        json.dump(infer_output.data, log.stdout, indent=2)
        LOG.log(
            log.SUCCESS,
            "Generated stubs:\n\n"
            + "\n\n".join(
                f"*{module.path}*\n{module.to_stubs()}" for module in module_annotations
            ),
        )
        return

    def _write_stubs(self, module_annotations: Sequence[ModuleAnnotations]) -> None:
        type_directory = self._type_directory
        if type_directory.exists():
            LOG.log(log.SUCCESS, f"Deleting {type_directory}")
            shutil.rmtree(type_directory)
        type_directory.mkdir(parents=True, exist_ok=True)

        LOG.log(log.SUCCESS, f"Outputting inferred stubs to {type_directory}")
        for module in module_annotations:
            module.write_stubs(type_directory=type_directory)

    def _annotate_in_place(self, stub_paths: Sequence[Path] | None = None) -> None:
        root = self._original_directory
        formatter = self._configuration.formatter
        root_type_directory = self._root_type_directory
        debug_infer = self._debug_infer
        number_workers = self._configuration.get_number_of_workers()
        in_place_paths = none_throws(
            self._in_place_paths,
            "_annotate_in_place called with self._in_place_paths == None",
        )

        tasks: List[AnnotateModuleInPlace] = []
        for stub_path in stub_paths or root_type_directory.rglob("*.pyi"):
            relative_code_path = stub_path.relative_to(root_type_directory).with_suffix(
                ".py"
            )

            annotate_in_place = in_place_paths == [] or any(
                path
                in (
                    relative_code_path,
                    *relative_code_path.parents,
                )
                for path in in_place_paths
            )

            if annotate_in_place:
                code_path = root / relative_code_path
                tasks.append(
                    AnnotateModuleInPlace(
                        stub_path=str(stub_path),
                        code_path=str(code_path),
                        debug_infer=debug_infer,
                    )
                )

        with multiprocessing.Pool(number_workers) as pool:
            for _ in pool.imap_unordered(AnnotateModuleInPlace.run_task, tasks):
                pass

        if formatter:
            subprocess.call(
                formatter, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
            )
            self._suppress_errors()

    def _suppress_errors(self) -> None:
        result = self._call_client(command="check")
        errors = self._get_errors(result)
        error_json = json.dumps([error.to_json() for error in errors])
        subprocess.run("pyre-upgrade fixme", input=error_json)
