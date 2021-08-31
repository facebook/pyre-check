# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import contextlib
import dataclasses
import enum
import json
import logging
import multiprocessing
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Any, Dict, Iterator, List, Optional, Sequence, Set, TypeVar, Union

import dataclasses_json
import libcst
from libcst.codemod import CodemodContext
from libcst.codemod.visitors import ApplyTypeAnnotationsVisitor

from ... import commands, command_arguments, configuration as configuration_module, log
from . import remote_logging, backend_arguments, start

LOG: logging.Logger = logging.getLogger(__name__)


class InferMode(enum.Enum):
    LOCAL: str = "Local"
    INTERPROCEDURAL: str = "Interprocedural"

    def serialize(self) -> List[str]:
        return [self.value]


@dataclasses.dataclass(frozen=True)
class Arguments:
    """
    Data structure for configuration options the backend check command can recognize.
    Need to keep in sync with `pyre/command/newInferCommand.ml`
    """

    log_path: str
    global_root: str
    source_paths: backend_arguments.SourcePath

    checked_directory_allowlist: Sequence[str] = dataclasses.field(default_factory=list)
    checked_directory_blocklist: Sequence[str] = dataclasses.field(default_factory=list)
    debug: bool = False
    excludes: Sequence[str] = dataclasses.field(default_factory=list)
    extensions: Sequence[str] = dataclasses.field(default_factory=list)
    memory_profiling_output: Optional[Path] = None
    number_of_workers: int = 1
    parallel: bool = True
    profiling_output: Optional[Path] = None
    python_version: configuration_module.PythonVersion = (
        configuration_module.PythonVersion(major=3)
    )
    relative_local_root: Optional[str] = None
    shared_memory: configuration_module.SharedMemory = (
        configuration_module.SharedMemory()
    )
    remote_logging: Optional[backend_arguments.RemoteLogging] = None
    search_paths: Sequence[configuration_module.SearchPathElement] = dataclasses.field(
        default_factory=list
    )
    # infer-specific fields
    infer_mode: InferMode = InferMode.LOCAL
    ignore_infer: Sequence[str] = dataclasses.field(default_factory=list)
    paths_to_modify: Optional[Set[Path]] = None

    @property
    def local_root(self) -> Optional[str]:
        if self.relative_local_root is None:
            return None
        return os.path.join(self.global_root, self.relative_local_root)

    def serialize(self) -> Dict[str, Any]:
        local_root = self.local_root
        return {
            # base config fields
            "source_paths": self.source_paths.serialize(),
            "search_paths": [
                element.command_line_argument() for element in self.search_paths
            ],
            "excludes": self.excludes,
            "checked_directory_allowlist": self.checked_directory_allowlist,
            "checked_directory_blocklist": self.checked_directory_blocklist,
            "extensions": self.extensions,
            "log_path": self.log_path,
            "global_root": self.global_root,
            **({} if local_root is None else {"local_root": local_root}),
            "debug": self.debug,
            "python_version": {
                "major": self.python_version.major,
                "minor": self.python_version.minor,
                "micro": self.python_version.micro,
            },
            "shared_memory": self.shared_memory.to_json(),
            "parallel": self.parallel,
            "number_of_workers": self.number_of_workers,
            **(
                {}
                if self.remote_logging is None
                else {"remote_logging": self.remote_logging.serialize()}
            ),
            **(
                {}
                if self.profiling_output is None
                else {"profiling_output": str(self.profiling_output)}
            ),
            **(
                {}
                if self.memory_profiling_output is None
                else {"memory_profiling_output": str(self.memory_profiling_output)}
            ),
            "ignore_infer": self.ignore_infer,
            "infer_mode": self.infer_mode.serialize(),
            **(
                {}
                if self.paths_to_modify is None
                else {"paths_to_modify": [str(path) for path in self.paths_to_modify]}
            ),
        }


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class RawAnnotationLocation:
    qualifier: str
    path: str
    line: int


@dataclasses.dataclass(frozen=True)
class RawAnnotation:
    name: str
    location: RawAnnotationLocation


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class RawGlobalAnnotation(RawAnnotation):
    annotation: str


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class RawAttributeAnnotation(RawAnnotation):
    parent: str
    annotation: str


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class RawParameter:
    name: str
    index: int
    annotation: Optional[str] = None
    value: Optional[str] = None


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class RawDefineAnnotation(RawAnnotation):
    parent: Optional[str] = None
    return_: Optional[str] = dataclasses.field(
        metadata=dataclasses_json.config(field_name="return"), default=None
    )
    parameters: List[RawParameter] = dataclasses.field(default_factory=list)
    decorators: List[str] = dataclasses.field(default_factory=list)
    is_async: bool = dataclasses.field(
        metadata=dataclasses_json.config(field_name="async"), default=False
    )


TAnnotation = TypeVar("TAnnotation", bound=RawAnnotation)


@dataclasses_json.dataclass_json(
    letter_case=dataclasses_json.LetterCase.CAMEL,
    undefined=dataclasses_json.Undefined.EXCLUDE,
)
@dataclasses.dataclass(frozen=True)
class RawInferOutput:
    global_annotations: List[RawGlobalAnnotation] = dataclasses.field(
        metadata=dataclasses_json.config(field_name="globals"), default_factory=list
    )
    attribute_annotations: List[RawAttributeAnnotation] = dataclasses.field(
        metadata=dataclasses_json.config(field_name="attributes"), default_factory=list
    )
    define_annotations: List[RawDefineAnnotation] = dataclasses.field(
        metadata=dataclasses_json.config(field_name="defines"), default_factory=list
    )

    @staticmethod
    def create_from_string(input: str) -> "RawInferOutput":
        try:
            # pyre-fixme[16]: Pyre doesn't understand `dataclasses_json`
            return RawInferOutput.schema().loads(input)
        except (
            TypeError,
            KeyError,
            ValueError,
            dataclasses_json.mm.ValidationError,
        ) as error:
            raise RawInferOutputParsingError(str(error)) from error

    @staticmethod
    def create_from_json(input: Dict[str, object]) -> "RawInferOutput":
        return RawInferOutput.create_from_string(json.dumps(input))

    def split_by_path(self) -> "Dict[str, RawInferOutput]":
        def create_index(
            annotations: Sequence[TAnnotation],
        ) -> Dict[str, List[TAnnotation]]:
            result: Dict[str, List[TAnnotation]] = {}
            for annotation in annotations:
                key = annotation.location.path
                result.setdefault(key, []).append(annotation)
            return result

        global_annotation_index = create_index(self.global_annotations)
        attribute_annotation_index = create_index(self.attribute_annotations)
        define_annotation_index = create_index(self.define_annotations)

        return {
            path: RawInferOutput(
                global_annotations=global_annotation_index.get(path, []),
                attribute_annotations=attribute_annotation_index.get(path, []),
                define_annotations=define_annotation_index.get(path, []),
            )
            for path in global_annotation_index.keys()
            | attribute_annotation_index.keys()
            | define_annotation_index.keys()
        }


class RawInferOutputParsingError(Exception):
    pass


def _sanitize_name(name: str) -> str:
    """The last part of the access path is the function/attribute name"""
    return name.split(".")[-1]


class PathLikeAnnotationFixer(libcst.CSTTransformer):
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


@dataclasses.dataclass(frozen=True)
class StubGenerationOptions:
    annotate_attributes: bool = False
    use_future_annotations: bool = False
    quote_annotations: bool = False
    dequalify: bool = False

    def __post__init__(self) -> None:
        if self.quote_annotations and (self.use_future_annotations or self.dequalify):
            raise ValueError(
                "You should not mix the `quote_annotations` option, which causes pyre "
                "to generate quoted and qualified annotations, with the "
                "`use_future_annotations` or `dequalify` options."
            )


@dataclasses.dataclass(frozen=True)
class TypeAnnotation:
    annotation: Optional[str]
    quote: bool
    dequalify: bool

    @staticmethod
    def from_raw(
        annotation: Optional[str], options: StubGenerationOptions
    ) -> "TypeAnnotation":
        return TypeAnnotation(
            annotation=annotation,
            dequalify=options.dequalify,
            quote=options.quote_annotations,
        )

    def sanitized(self, prefix: str = "") -> str:
        if self.annotation is None:
            return ""
        if self.quote:
            return f'{prefix}"{self.annotation}"'
        else:
            return prefix + sanitize_annotation(
                self.annotation, dequalify_all=self.dequalify
            )

    def _simple_types(self) -> List[str]:
        """Split an annotation into its tokens"""
        if self.annotation is None or self.annotation == "":
            return []
        else:
            return re.split("[^\\w.]+", self.annotation)

    def typing_imports(self) -> Set[str]:
        if self.quote:
            return set()
        typing_imports = set()
        for simple_type in self._simple_types():
            if simple_type:
                split_type = re.findall(r"[\w]+", simple_type)
                if len(split_type) > 1 and split_type[0] == "typing":
                    typing_imports.add(split_type[1])
        return typing_imports

    @property
    def missing(self) -> bool:
        return self.annotation is None


@dataclasses.dataclass(frozen=True)
class Parameter:
    name: str
    annotation: TypeAnnotation
    value: Optional[str]

    def to_stub(self) -> str:
        delimiter = "=" if self.annotation.missing else " = "
        value = f"{delimiter}{self.value}" if self.value else ""
        return f"{self.name}{self.annotation.sanitized(prefix=': ')}{value}"


@dataclasses.dataclass(frozen=True)
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

    def typing_imports(self) -> Set[str]:
        typing_imports = self.return_annotation.typing_imports()
        for parameter in self.parameters:
            typing_imports |= parameter.annotation.typing_imports()
        return typing_imports


@dataclasses.dataclass(frozen=True)
class MethodAnnotation(FunctionAnnotation):
    parent: str


@dataclasses.dataclass(frozen=True)
class FieldAnnotation:
    name: str
    annotation: TypeAnnotation

    def __post_init__(self) -> None:
        if self.annotation.missing:
            raise RuntimeError(f"Illegal missing FieldAnnotation for {self.name}")

    def to_stub(self) -> str:
        name = _sanitize_name(self.name)
        return f"{name}: {self.annotation.sanitized()} = ..."

    def typing_imports(self) -> Set[str]:
        return self.annotation.typing_imports()


@dataclasses.dataclass(frozen=True)
class GlobalAnnotation(FieldAnnotation):
    pass


@dataclasses.dataclass(frozen=True)
class AttributeAnnotation(FieldAnnotation):
    parent: str


@dataclasses.dataclass(frozen=True)
class ModuleAnnotations:
    path: str
    options: StubGenerationOptions
    globals_: List[GlobalAnnotation] = dataclasses.field(default_factory=list)
    attributes: List[AttributeAnnotation] = dataclasses.field(default_factory=list)
    functions: List[FunctionAnnotation] = dataclasses.field(default_factory=list)
    methods: List[MethodAnnotation] = dataclasses.field(default_factory=list)

    @staticmethod
    def from_infer_output(
        path: str,
        infer_output: RawInferOutput,
        options: StubGenerationOptions,
    ) -> "ModuleAnnotations":
        def type_annotation(annotation: Optional[str]) -> TypeAnnotation:
            return TypeAnnotation.from_raw(
                annotation,
                options=options,
            )

        return ModuleAnnotations(
            path=path,
            globals_=[
                GlobalAnnotation(
                    name=global_.name, annotation=type_annotation(global_.annotation)
                )
                for global_ in infer_output.global_annotations
            ],
            attributes=[
                AttributeAnnotation(
                    parent=attribute.parent,
                    name=attribute.name,
                    annotation=type_annotation(attribute.annotation),
                )
                for attribute in infer_output.attribute_annotations
            ]
            if options.annotate_attributes
            else [],
            functions=[
                FunctionAnnotation(
                    name=define.name,
                    return_annotation=type_annotation(define.return_),
                    parameters=[
                        Parameter(
                            name=parameter.name,
                            annotation=type_annotation(parameter.annotation),
                            value=parameter.value,
                        )
                        for parameter in define.parameters
                    ],
                    decorators=define.decorators,
                    is_async=define.is_async,
                )
                for define in infer_output.define_annotations
                if define.parent is None
            ],
            methods=[
                MethodAnnotation(
                    parent=define.parent,
                    name=define.name,
                    return_annotation=type_annotation(define.return_),
                    parameters=[
                        Parameter(
                            name=parameter.name,
                            annotation=type_annotation(parameter.annotation),
                            value=parameter.value,
                        )
                        for parameter in define.parameters
                    ],
                    decorators=define.decorators,
                    is_async=define.is_async,
                )
                for define in infer_output.define_annotations
                if define.parent is not None
            ],
            options=options,
        )

    def is_empty(self) -> bool:
        return (
            len(self.globals_)
            + len(self.attributes)
            + len(self.functions)
            + len(self.methods)
        ) == 0

    @staticmethod
    def _indent(stub: str) -> str:
        return "    " + stub.replace("\n", "\n    ")

    def _relativize(self, parent: str) -> Sequence[str]:
        path = (
            str(self.path).split(".", 1)[0].replace("/", ".").replace(".__init__", "")
        )
        return parent.replace(path, "", 1).strip(".").split(".")

    @property
    def classes(self) -> Dict[str, List[Union[AttributeAnnotation, MethodAnnotation]]]:
        """
        Find all classes with attributes or methods to annotate.

        Anything in nested classes is currently ignored, e.g.:
        ```
        class X:
            class Y:
                [ALL OF THIS IS IGNORED]
        ```
        """
        classes: Dict[str, List[Union[AttributeAnnotation, MethodAnnotation]]] = {}
        nested_class_count = 0
        for annotation in [*self.attributes, *self.methods]:
            parent = self._relativize(annotation.parent)
            if len(parent) == 1:
                classes.setdefault(parent[0], []).append(annotation)
            else:
                nested_class_count += 1
        if nested_class_count > 0:
            LOG.warning(
                f"In file {self.path}, ignored {nested_class_count} nested classes"
            )
        return classes

    def _header_imports(self) -> List[str]:
        return (
            ["from __future__ import annotations"]
            if self.options.use_future_annotations
            else []
        )

    def _typing_imports(self) -> List[str]:
        from_typing = sorted(
            {
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
            }
        )
        return (
            []
            if from_typing == []
            else [f"from typing import {', '.join(from_typing)}"]
        )

    def _imports(self) -> str:
        import_statements = self._header_imports() + self._typing_imports()
        imports_str = (
            "" if import_statements == [] else "\n".join(import_statements) + "\n"
        )
        return imports_str

    def _class_stub(
        self,
        classname: str,
        annotations: Sequence[Union[AttributeAnnotation, MethodAnnotation]],
    ) -> str:
        body = "\n".join(
            self._indent(annotation.to_stub()) for annotation in annotations
        )
        return f"class {classname}:\n{body}\n"

    def to_stubs(self) -> str:
        """
        Output annotation information as a stub file.
        """
        return "\n".join(
            [
                self._imports(),
                *(global_.to_stub() for global_ in self.globals_),
                *(function.to_stub() for function in self.functions),
                *(
                    self._class_stub(classname, annotations)
                    for classname, annotations in self.classes.items()
                ),
                "",
            ]
        )

    def stubs_path(self, directory: Path) -> Path:
        return (directory / self.path).with_suffix(".pyi")

    def write_stubs(self, type_directory: Path) -> None:
        path = self.stubs_path(type_directory)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(self.to_stubs())


@dataclasses.dataclass
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
    def run_task(task: "AnnotateModuleInPlace") -> None:
        "Wrap `run` in a static method to use with multiprocessing"
        return task.run()


def create_infer_arguments(
    configuration: configuration_module.Configuration,
    infer_arguments: command_arguments.InferArguments,
) -> Arguments:
    """
    Translate client configurations to backend check configurations.

    This API is not pure since it needs to access filesystem to filter out
    nonexistent directories. It is idempotent though, since it does not alter
    any filesystem state.
    """
    source_paths = backend_arguments.get_source_path_for_check(configuration)

    infer_mode = (
        InferMode.INTERPROCEDURAL
        if infer_arguments.interprocedural
        else InferMode.LOCAL
    )

    profiling_output = (
        backend_arguments.get_profiling_log_path(Path(configuration.log_directory))
        if infer_arguments.enable_profiling
        else None
    )
    memory_profiling_output = (
        backend_arguments.get_profiling_log_path(Path(configuration.log_directory))
        if infer_arguments.enable_memory_profiling
        else None
    )

    logger = configuration.logger
    remote_logging = (
        backend_arguments.RemoteLogging(
            logger=logger, identifier=infer_arguments.log_identifier or ""
        )
        if logger is not None
        else None
    )

    return Arguments(
        log_path=configuration.log_directory,
        global_root=configuration.project_root,
        checked_directory_allowlist=list(
            source_paths.get_checked_directory_allowlist()
        ),
        checked_directory_blocklist=(
            configuration.get_existent_ignore_all_errors_paths()
        ),
        debug=infer_arguments.debug_infer,
        excludes=configuration.excludes,
        extensions=configuration.get_valid_extension_suffixes(),
        ignore_infer=configuration.get_existent_ignore_infer_paths(),
        relative_local_root=configuration.relative_local_root,
        memory_profiling_output=memory_profiling_output,
        number_of_workers=configuration.get_number_of_workers(),
        parallel=not infer_arguments.sequential,
        profiling_output=profiling_output,
        python_version=configuration.get_python_version(),
        shared_memory=configuration.shared_memory,
        remote_logging=remote_logging,
        search_paths=configuration.expand_and_get_existent_search_paths(),
        source_paths=source_paths,
        infer_mode=infer_mode,
        paths_to_modify=infer_arguments.paths_to_modify,
    )


@contextlib.contextmanager
def create_infer_arguments_and_cleanup(
    configuration: configuration_module.Configuration,
    infer_arguments: command_arguments.InferArguments,
) -> Iterator[Arguments]:
    arguments = create_infer_arguments(configuration, infer_arguments)
    try:
        yield arguments
    finally:
        # It is safe to clean up source paths after infer command since
        # any created artifact directory won't be reused by other commands.
        arguments.source_paths.cleanup()


def _check_working_directory(
    working_directory: Path, global_root: Path, relative_local_root: Optional[str]
) -> None:
    candidate_locations: List[str] = []
    if working_directory == global_root:
        return
    candidate_locations.append(f"`{global_root}` with `--local-configuration` set")

    if relative_local_root is not None:
        local_root = global_root / relative_local_root
        if working_directory == local_root:
            return
        candidate_locations.append(f"`{local_root}`")

    valid_locations = " or from ".join(candidate_locations)
    raise ValueError(
        f"Infer must run from {valid_locations}. "
        f"Cannot run from current working directory `{working_directory}`."
    )


def _run_infer_command_get_output(command: Sequence[str]) -> str:
    with backend_arguments.backend_log_file(prefix="pyre_infer") as log_file:
        with start.background_logging(Path(log_file.name)):
            result = subprocess.run(
                command,
                stdout=subprocess.PIPE,
                stderr=log_file.file,
                universal_newlines=True,
            )
            return_code = result.returncode

            # Interpretation of the return code needs to be kept in sync with
            # `command/newInferCommand.ml`.
            if return_code == 0:
                return result.stdout
            elif return_code == 1:
                raise commands.ClientException(
                    message="Pyre encountered an internal failure",
                    exit_code=commands.ExitCode.FAILURE,
                )
            elif return_code == 2:
                raise commands.ClientException(
                    message="Pyre encountered a failure within buck.",
                    exit_code=commands.ExitCode.BUCK_INTERNAL_ERROR,
                )
            elif return_code == 3:
                raise commands.ClientException(
                    message="Pyre encountered an error when building the buck targets.",
                    exit_code=commands.ExitCode.BUCK_USER_ERROR,
                )
            else:
                raise commands.ClientException(
                    message=(
                        "Infer command exited with unexpected return code: "
                        f"{return_code}."
                    ),
                    exit_code=commands.ExitCode.FAILURE,
                )


def _get_infer_command_output(
    configuration: configuration_module.Configuration,
    infer_arguments: command_arguments.InferArguments,
) -> str:
    binary_location = configuration.get_binary_respecting_override()
    if binary_location is None:
        raise configuration_module.InvalidConfiguration(
            "Cannot locate a Pyre binary to run."
        )

    with create_infer_arguments_and_cleanup(
        configuration, infer_arguments
    ) as arguments:
        with backend_arguments.temporary_argument_file(arguments) as argument_file_path:
            infer_command = [binary_location, "newinfer", str(argument_file_path)]
            return _run_infer_command_get_output(command=infer_command)


def _load_output(
    configuration: configuration_module.Configuration,
    infer_arguments: command_arguments.InferArguments,
) -> str:
    if infer_arguments.read_stdin:
        return sys.stdin.read()
    else:
        return _get_infer_command_output(configuration, infer_arguments)


def _relativize_path(path: str, against: Path) -> Optional[str]:
    given_path = Path(path)
    return (
        None
        if against not in given_path.parents
        else str(given_path.relative_to(against))
    )


def create_module_annotations(
    infer_output: RawInferOutput, base_path: Path, options: StubGenerationOptions
) -> List[ModuleAnnotations]:
    infer_output_relativized: Dict[Optional[str], RawInferOutput] = {
        _relativize_path(path, against=base_path): data
        for path, data in infer_output.split_by_path().items()
    }
    return [
        ModuleAnnotations.from_infer_output(
            path=path, infer_output=infer_output_for_path, options=options
        )
        for path, infer_output_for_path in infer_output_relativized.items()
        if path is not None
    ]


def _print_inferences(
    infer_output: RawInferOutput, module_annotations: Sequence[ModuleAnnotations]
) -> None:
    LOG.log(log.SUCCESS, "Raw Infer Outputs:")
    # pyre-ignore[16]: Pyre does not understand `dataclasses_json`
    LOG.log(log.SUCCESS, json.dumps(infer_output.to_dict(), indent=2))
    LOG.log(log.SUCCESS, "Generated Stubs:")
    LOG.log(
        log.SUCCESS,
        "\n\n".join(
            f"*{module.path}*\n{module.to_stubs()}" for module in module_annotations
        ),
    )


def _get_type_directory(log_directory: Path) -> Path:
    return log_directory / "types"


def _write_stubs(
    type_directory: Path, module_annotations: Sequence[ModuleAnnotations]
) -> None:
    if type_directory.exists():
        LOG.log(log.SUCCESS, f"Deleting {type_directory}")
        shutil.rmtree(type_directory)
    type_directory.mkdir(parents=True, exist_ok=True)

    LOG.log(log.SUCCESS, f"Outputting inferred stubs to {type_directory}...")
    for module in module_annotations:
        module.write_stubs(type_directory=type_directory)


def should_annotate_in_place(path: Path, paths_to_modify: Set[Path]) -> bool:
    return (
        True
        if len(paths_to_modify) == 0
        else any(path in paths_to_modify for path in (path, *path.parents))
    )


def _annotate_in_place(
    working_directory: Path,
    type_directory: Path,
    paths_to_modify: Set[Path],
    debug_infer: bool,
    number_of_workers: int,
) -> None:
    tasks: List[AnnotateModuleInPlace] = []
    for full_stub_path in type_directory.rglob("*.pyi"):
        relative_stub_path = full_stub_path.relative_to(type_directory)
        relative_code_path = relative_stub_path.with_suffix(".py")
        full_code_path = working_directory / relative_code_path

        if should_annotate_in_place(full_code_path, paths_to_modify):
            tasks.append(
                AnnotateModuleInPlace(
                    full_stub_path=str(full_stub_path),
                    full_code_path=str(full_code_path),
                    debug_infer=debug_infer,
                )
            )

    with multiprocessing.Pool(number_of_workers) as pool:
        for _ in pool.imap_unordered(AnnotateModuleInPlace.run_task, tasks):
            pass


def run_infer(
    configuration: configuration_module.Configuration,
    infer_arguments: command_arguments.InferArguments,
) -> commands.ExitCode:
    working_directory = infer_arguments.working_directory
    _check_working_directory(
        working_directory=working_directory,
        global_root=Path(configuration.project_root),
        relative_local_root=configuration.relative_local_root,
    )

    type_directory = _get_type_directory(Path(configuration.log_directory))
    paths_to_modify = infer_arguments.paths_to_modify

    if infer_arguments.annotate_from_existing_stubs:
        if paths_to_modify is None:
            raise ValueError(
                "`--annotate-from-existing-stubs` cannot be used without the"
                " `--in-place` flag"
            )
        _annotate_in_place(
            working_directory=working_directory,
            type_directory=type_directory,
            paths_to_modify=paths_to_modify,
            debug_infer=infer_arguments.debug_infer,
            number_of_workers=configuration.get_number_of_workers(),
        )
    else:
        infer_output = RawInferOutput.create_from_json(
            json.loads(_load_output(configuration, infer_arguments))[0]
        )
        module_annotations = create_module_annotations(
            infer_output=infer_output,
            base_path=working_directory,
            options=StubGenerationOptions(
                annotate_attributes=infer_arguments.annotate_attributes,
                use_future_annotations=not infer_arguments.no_future_annotations,
                dequalify=infer_arguments.dequalify,
                quote_annotations=infer_arguments.quote_annotations,
            ),
        )
        if infer_arguments.print_only:
            _print_inferences(infer_output, module_annotations)
        else:
            _write_stubs(type_directory, module_annotations)
            if paths_to_modify is not None:
                _annotate_in_place(
                    working_directory=working_directory,
                    type_directory=type_directory,
                    paths_to_modify=paths_to_modify,
                    debug_infer=infer_arguments.debug_infer,
                    number_of_workers=configuration.get_number_of_workers(),
                )
    return commands.ExitCode.SUCCESS


@remote_logging.log_usage(command_name="infer")
def run(
    configuration: configuration_module.Configuration,
    infer_arguments: command_arguments.InferArguments,
) -> commands.ExitCode:
    try:
        return run_infer(configuration, infer_arguments)
    except commands.ClientException:
        raise
    except Exception as error:
        raise commands.ClientException(
            f"Exception occured during Pyre infer: {error}"
        ) from error
