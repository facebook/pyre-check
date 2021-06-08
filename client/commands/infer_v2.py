# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

import json
import logging
import re
from abc import ABC
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import cast, Dict, List, Optional, Sequence

import libcst
from typing_extensions import Final
from typing_extensions import TypeAlias

from .. import command_arguments, log
from ..analysis_directory import AnalysisDirectory, resolve_analysis_directory
from ..configuration import Configuration
from .check import Check
from .infer import dequalify_and_fix_pathlike, split_imports

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

    @property
    def complete(self) -> bool:
        missing = self.return_annotation.missing or any(
            parameter.annotation.missing for parameter in self.parameters
        )
        return not missing


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


class AnnotationCollector(libcst.CSTVisitor):
    def __init__(self, file_path: str) -> None:
        self.qualifier: str = file_path.replace(".py", "")
        self.globals_: list[GlobalAnnotation] = []
        self.attributes: list[AttributeAnnotation] = []
        self.functions: list[FunctionAnnotation] = []
        self.methods: list[MethodAnnotation] = []
        self.class_name: list[str] = []

    def visit_ClassDef(self, node: libcst.ClassDef) -> None:
        self.class_name.append(node.name.value)

    def leave_ClassDef(self, original_node: libcst.ClassDef) -> None:
        self.class_name.remove(original_node.name.value)

    def visit_AnnAssign(self, node: libcst.AnnAssign) -> None:
        target_name = self._code_for_node(node.target)
        if target_name is None:
            return
        parent = ".".join(self.class_name) if self.class_name else None
        name = self._qualified_name(
            parent=parent,
            target_name=target_name,
        )
        annotation = TypeAnnotation(self._code_for_node(node.annotation.annotation))
        if parent is None:
            self.globals_.append(
                GlobalAnnotation(
                    name=name,
                    annotation=annotation,
                )
            )
        else:
            self.attributes.append(
                AttributeAnnotation(
                    parent=parent,
                    name=name,
                    annotation=annotation,
                )
            )

    def visit_FunctionDef(self, node: libcst.FunctionDef) -> bool:
        if node.returns is None:
            return False
        parent = ".".join(self.class_name) if self.class_name else None
        name = self._qualified_name(
            parent=parent,
            target_name=node.name.value,
        )
        return_annotation = TypeAnnotation(self._code_for_node(node.returns.annotation))
        parameters = [
            Parameter(
                name=parameter.name.value,
                annotation=TypeAnnotation(
                    self._code_for_node(
                        parameter.annotation.annotation
                        if parameter.annotation
                        else None
                    )
                ),
                value=self._code_for_node(parameter.default),
            )
            for parameter in node.params.params
        ]
        decorators = [
            decorator
            for decorator in (
                self._code_for_node(decorator) for decorator in node.decorators
            )
            if decorator is not None
        ]
        is_async = node.asynchronous is not None
        if parent is None:
            self.functions.append(
                FunctionAnnotation(
                    name=name,
                    return_annotation=return_annotation,
                    parameters=parameters,
                    decorators=decorators,
                    is_async=is_async,
                )
            )
        else:
            self.methods.append(
                MethodAnnotation(
                    parent=parent,
                    name=name,
                    return_annotation=return_annotation,
                    parameters=parameters,
                    decorators=decorators,
                    is_async=is_async,
                )
            )
        return False

    # utility methods

    def _qualified_name(self, parent: str | None, target_name: str) -> str:
        prefix = f"{self.qualifier}."
        if parent is not None:
            prefix += f"{parent}."
        return prefix + target_name

    @staticmethod
    def _code_for_node(node: Optional[libcst.CSTNode]) -> Optional[str]:
        return None if node is None else libcst.parse_module("").code_for_node(node)


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

    @staticmethod
    def from_module(path: str, module: libcst.Module) -> ModuleAnnotations:
        collector = AnnotationCollector(file_path=path)
        module.visit(collector)
        return ModuleAnnotations(
            path=path,
            globals_=collector.globals_,
            attributes=collector.attributes,
            functions=collector.functions,
            methods=collector.methods,
        )

    def filter_for_complete(self) -> ModuleAnnotations:
        return ModuleAnnotations(
            path=self.path,
            globals_=self.globals_,
            attributes=self.attributes,
            functions=[function for function in self.functions if function.complete],
            methods=[method for method in self.methods if method.complete],
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
            ]
        )

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
    complete_only: bool = False,
) -> Sequence[ModuleAnnotations]:
    infer_output_by_path = RawInferOutput.split_by_path(
        infer_output=infer_output,
    )
    modules = [
        ModuleAnnotations.from_infer_output(
            path=path,
            infer_output=infer_output_for_path,
        )
        for path, infer_output_for_path in infer_output_by_path.items()
    ]
    if complete_only:
        return [module.filter_for_complete() for module in modules]
    else:
        return modules


class Infer(Check):
    NAME = "analyze"
    ANALYSIS = "type_inference"

    def __init__(
        self,
        command_arguments: command_arguments.CommandArguments,
        original_directory: str,
        *,
        configuration: Configuration,
        analysis_directory: AnalysisDirectory | None = None,
        dump_call_graph: bool,
        repository_root: str | None,
        use_cache: bool,
    ) -> None:
        super(Infer, self).__init__(
            command_arguments,
            original_directory,
            configuration=configuration,
            analysis_directory=analysis_directory,
        )
        self._dump_call_graph: bool = dump_call_graph
        self._repository_root: Final[str | None] = repository_root
        self._use_cache: bool = use_cache

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
        flags.extend(["-analysis", self.ANALYSIS])
        if self._dump_call_graph:
            flags.append("-dump-call-graph")
        repository_root = self._repository_root
        if repository_root:
            flags.extend(["-repository-root", repository_root])
        if self._use_cache:
            flags.append("-use-cache")
        return flags

    def _run(self, retries: int = 1) -> None:
        self._analysis_directory.prepare()
        result = self._call_client(command=self.NAME)
        result.check()
        LOG.log(
            log.SUCCESS,
            "Raw output from infer:\n" + result.output,
        )
        infer_output = RawInferOutput(data=json.loads(result.output)[0])
        module_annotations = _create_module_annotations(infer_output=infer_output)
        LOG.log(
            log.SUCCESS,
            "Generated stubs:\n\n"
            + "\n\n".join(
                f"*{module.path}*\n{module.to_stubs()}" for module in module_annotations
            ),
        )
