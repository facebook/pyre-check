# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

import json
import logging
import multiprocessing
import re
import shutil
import subprocess
import sys
from abc import ABC
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import cast, Callable, Dict, List, Optional, Sequence

import libcst
from libcst.codemod import CodemodContext
from libcst.codemod.visitors import ApplyTypeAnnotationsVisitor
from typing_extensions import TypeAlias

from .. import command_arguments, log
from ..analysis_directory import AnalysisDirectory, resolve_analysis_directory
from ..configuration import Configuration
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


class PathLikeAnnotationFixer(libcst.CSTTransformer):
    def leave_Subscript(
        self,
        original_node: libcst.Subscript,
        updated_node: libcst.Subscript | libcst.SimpleString,
    ) -> libcst.Subscript | libcst.SimpleString:
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


def sanitize_annotation(
    annotation: str,
    dequalify_all: bool = False,
    dequalify_typing: bool = True,
) -> str:
    """
    Transform raw annotations in an attempt to reduce incorrectly-imported
    annotations in generated code.

    TODO(T93381000): Handle qualification in a principled way and remove
    this: all of these transforms are attempts to hack simple fixes to the
    problem of us not actually understanding qualified types and existing
    imports.

    (1) If `dequalify_typing` is set, then remove all uses of `typing.`, for
    example `typing.Optional[int]` becomes `Optional[int]`. We do this because
    we automatically add a `from typing import ...` if necessary.

    (2) If `dequalify_all` is set, then remove all qualifiers from any
    top-level type (by top-level I mean outside the outermost brackets). For
    example, convert `sqlalchemy.sql.schema.Column[typing.Optional[int]]`
    into `Column[typing.Optional[int]]`.

    (3) Fix PathLike annotations: convert all bare `PathLike` uses to
    `'os.PathLike'`; the ocaml side of pyre currently spits out an unqualified
    type here which is incorrect, and quoting makes the use of os safer
    given that we don't handle imports correctly yet.
    """
    if dequalify_all:
        match = re.fullmatch(r"([^.]*?\.)*?([^.]+)(\[.*\])", annotation)
        if match is not None:
            annotation = f"{match.group(2)}{match.group(3)}"

    if dequalify_typing:
        annotation = annotation.replace("typing.", "")

    if annotation.find("PathLike") >= 0:
        try:
            tree = libcst.parse_module(annotation)
            annotation = tree.visit(PathLikeAnnotationFixer()).code
        except libcst._exceptions.ParserSyntaxError:
            pass

    return annotation


def split_imports(types_list: list[str]) -> set[str]:
    typing_imports = set()
    for full_type in types_list:
        if full_type:
            split_type = re.findall(r"[\w]+", full_type)
            if len(split_type) > 1 and split_type[0] == "typing":
                typing_imports.add(split_type[1])
    return typing_imports


@dataclass(frozen=True)
class TypeAnnotation:
    annotation: str | None

    @staticmethod
    def from_raw(annotation: object) -> TypeAnnotation:
        if annotation is None or isinstance(annotation, str):
            return TypeAnnotation(annotation=annotation)
        else:
            raise ValueError("Expected str | None for annotation")

    def sanitized(self, dequalify: bool, prefix: str = "") -> str:
        if self.annotation is None:
            return ""
        else:
            return prefix + sanitize_annotation(
                self.annotation, dequalify_all=dequalify
            )

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

    def to_stub(self, dequalify: bool) -> str:
        delimiter = "=" if self.annotation.missing else " = "
        value = f"{delimiter}{self.value}" if self.value else ""
        return f"{self.name}{self.annotation.sanitized(dequalify, prefix=': ')}{value}"


@dataclass(frozen=True)
class FunctionAnnotation:
    name: str
    return_annotation: TypeAnnotation
    parameters: Sequence[Parameter]
    decorators: Sequence[str]
    is_async: bool

    def to_stub(self, dequalify: bool) -> str:
        name = _sanitize_name(self.name)
        decorators = "".join(f"@{decorator}\n" for decorator in self.decorators)
        async_ = "async " if self.is_async else ""
        parameters = ", ".join(
            parameter.to_stub(dequalify) for parameter in self.parameters
        )
        return_ = self.return_annotation.sanitized(dequalify, prefix=" -> ")
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

    def to_stub(self, dequalify: bool) -> str:
        name = _sanitize_name(self.name)
        return f"{name}: {self.annotation.sanitized(dequalify)} = ..."

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

    def to_stubs(self, dequalify: bool) -> str:
        """
        Output annotation information as a stub file.
        """
        return "\n".join(
            [
                self._typing_imports(),
                *(global_.to_stub(dequalify) for global_ in self.globals_),
                *(function.to_stub(dequalify) for function in self.functions),
                *(
                    self._class_stub(classname, annotations, dequalify)
                    for classname, annotations in self.classes.items()
                ),
                "",
            ]
        )

    def write_stubs(self, type_directory: Path, dequalify: bool) -> None:
        path = self.stubs_path(type_directory)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(self.to_stubs(dequalify))

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
        dequalify: bool,
    ) -> str:
        body = "\n".join(
            self._indent(annotation.to_stub(dequalify)) for annotation in annotations
        )
        return f"class {classname}:\n{body}\n"

    @staticmethod
    def _indent(stub: str) -> str:
        return "    " + stub.replace("\n", "\n    ")


def _create_module_annotations(
    infer_output: RawInferOutput, sanitize_path: Callable[[str], str | None]
) -> Sequence[ModuleAnnotations]:
    infer_output_by_path = RawInferOutput.split_by_path(
        infer_output=infer_output,
    )
    infer_output_sanitized = {
        path: data
        for raw_path, data in infer_output_by_path.items()
        if (path := sanitize_path(raw_path)) is not None
    }
    return [
        ModuleAnnotations.from_infer_output(
            path=path,
            infer_output=infer_output_for_path,
        )
        for path, infer_output_for_path in infer_output_sanitized.items()
    ]


@dataclass
class AnnotateModuleInPlace:
    full_stub_path: str
    full_code_path: str
    debug_infer: bool

    @staticmethod
    def _annotated_code(stub: str, code: str) -> str:
        """
        Merge inferred annotations from stubs with source code to get
        annotated code.
        """
        context = CodemodContext()
        ApplyTypeAnnotationsVisitor.store_stub_in_context(
            context, libcst.parse_module(stub)
        )
        modified_tree = ApplyTypeAnnotationsVisitor(context).transform_module(
            libcst.parse_module(code)
        )
        return modified_tree.code

    @staticmethod
    def annotate_code(stub_path: str, code_path: str, debug_infer: bool) -> None:
        "Merge a stub file of inferred annotations with a code file inplace."
        try:
            with open(stub_path) as stub_file, open(code_path) as code_file:
                stub = stub_file.read()
                code = code_file.read()
                annotated_code = AnnotateModuleInPlace._annotated_code(stub, code)
            with open(code_path, "w") as code_file:
                code_file.write(annotated_code)
            LOG.info(f"Annotated {code_path}")
        except Exception as error:
            LOG.warning(f"Failed to annotate {code_path}")
            if debug_infer:
                LOG.warning(f"\tError: {error}")

    def run(self) -> None:
        return self.annotate_code(
            stub_path=self.full_stub_path,
            code_path=self.full_code_path,
            debug_infer=self.debug_infer,
        )

    @staticmethod
    def run_task(task: AnnotateModuleInPlace) -> None:
        "Wrap `run` in a static method to use with multiprocessing"
        return task.run()


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
        in_place: bool,
        paths_to_modify: set[Path],
        annotate_from_existing_stubs: bool,
        debug_infer: bool,
        read_stdin: bool,
        dequalify: bool,
        interprocedural: bool,
    ) -> None:
        if annotate_from_existing_stubs and not in_place:
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
        self._in_place = in_place
        self._paths_to_modify = paths_to_modify
        self._annotate_from_existing_stubs = annotate_from_existing_stubs
        self._debug_infer = debug_infer
        self._read_stdin = read_stdin
        self._dequalify = dequalify
        self._interprocedural = interprocedural
        self._type_directory: Path = (
            Path(self._configuration.log_directory) / "types"
        ).absolute()
        self._analysis_root: Path = Path(self._analysis_directory.get_root()).resolve()

    def generate_analysis_directory(self) -> AnalysisDirectory:
        return resolve_analysis_directory(
            self._command_arguments.source_directories,
            self._command_arguments.targets,
            self._configuration,
            self._original_directory,
            filter_directory=self._command_arguments.filter_directory,
            isolate=True,
        )

    @property
    def _mode(self) -> str:
        return "interprocedural" if self._interprocedural else "local"

    def _flags(self) -> list[str]:
        flags = super()._flags()

        flags.extend(["-infer-mode", self._mode])

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
        module_annotations = _create_module_annotations(
            infer_output=infer_output,
            sanitize_path=self.project_relative_path,
        )
        if self._print_only:
            return self._print_inferences(
                infer_output=infer_output, module_annotations=module_annotations
            )
        self._write_stubs(module_annotations=module_annotations)
        if self._in_place:
            self._annotate_in_place()

    def project_relative_path(self, raw_path: str) -> str | None:
        """
        Transform paths from the server into relative paths from the working
        directory. There are two stages to this:
        - concatenate the path returned with the analysis root (which, in
          the case of a buck project, can be a temporary directory of symlinks).
        - follow symlinks (which will be almost everything in a buck project)
          to the real path, and then filter out anything that isn't actually
          part of the project root.

        Filtered paths are transformed to None so that the inference information
        will be discarded.
        """
        full_path = (self._analysis_root / raw_path).resolve()
        in_project = str(full_path).startswith(self._configuration.project_root)
        return (
            str(full_path.relative_to(self._original_directory))
            if in_project and full_path.exists
            else None
        )

    def _load_infer_output(self) -> str:
        if self._read_stdin:
            return sys.stdin.read()
        else:
            self._analysis_directory.prepare()
            result = self._call_client(command=self.NAME)
            result.check()
            return result.output

    def _print_inferences(
        self,
        infer_output: RawInferOutput,
        module_annotations: Sequence[ModuleAnnotations],
    ) -> None:
        json.dump(infer_output.data, log.stdout, indent=2)
        LOG.log(
            log.SUCCESS,
            "Generated stubs:\n\n"
            + "\n\n".join(
                f"*{module.path}*\n{module.to_stubs(self._dequalify)}"
                for module in module_annotations
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
            module.write_stubs(type_directory=type_directory, dequalify=self._dequalify)

    def _annotate_in_place(self) -> None:
        formatter = self._configuration.formatter
        type_directory = self._type_directory
        debug_infer = self._debug_infer
        number_workers = self._configuration.get_number_of_workers()

        tasks: list[AnnotateModuleInPlace] = []
        for full_stub_path in type_directory.rglob("*.pyi"):
            stub_path = full_stub_path.relative_to(type_directory)
            code_path = stub_path.with_suffix(".py")
            full_code_path = self._original_directory / code_path

            if self._should_annotate_in_place(code_path):
                tasks.append(
                    AnnotateModuleInPlace(
                        full_stub_path=str(full_stub_path),
                        full_code_path=str(full_code_path),
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

    def _should_annotate_in_place(self, full_code_path: Path) -> bool:
        if self._paths_to_modify == set():
            return True
        return any(
            path in self._paths_to_modify
            for path in (full_code_path, *full_code_path.parents)
        )

    def _suppress_errors(self) -> None:
        result = self._call_client(command="check")
        errors = self._get_errors(result)
        error_json = json.dumps([error.to_json() for error in errors])
        subprocess.run("pyre-upgrade fixme", input=error_json)
