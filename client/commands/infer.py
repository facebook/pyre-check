# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module contains the logic for `pyre infer`.

The Pyre Infer backend uses an extended version of our type-checking logic to
attempt to infer types of Python variabes, and then in the client we take
those types, write them to a stub file, and use a vendored copy of LibCST's
ApplyTypeAnnotationsVisitor to merge the stub and the original source code.
"""

import contextlib
import dataclasses
import functools
import itertools
import json
import logging
import multiprocessing
import re
import shutil
import subprocess
import sys
import textwrap
from pathlib import Path
from typing import Any, Dict, Iterator, List, Optional, Sequence, Set, TypeVar, Union

import dataclasses_json
import libcst
from libcst.codemod import CodemodContext

from .. import (
    backend_arguments,
    command_arguments,
    configuration as configuration_module,
    dataclasses_json_extensions as json_mixins,
    frontend_configuration,
    log,
)
from ..libcst_vendored_visitors import ApplyTypeAnnotationsVisitor
from . import commands, start

LOG: logging.Logger = logging.getLogger(__name__)
MAX_NESTED_CLASS_STRING_LENGTH: int = 1000


@dataclasses.dataclass(frozen=True)
class Arguments:
    """
    Data structure for configuration options the backend infer command can recognize.
    Need to keep in sync with `source/command/inferCommand.ml`
    """

    base_arguments: backend_arguments.BaseArguments
    paths_to_modify: Optional[Set[Path]] = None

    def serialize(self) -> Dict[str, Any]:
        return {
            **self.base_arguments.serialize(),
            **(
                {}
                if self.paths_to_modify is None
                else {"paths_to_modify": [str(path) for path in self.paths_to_modify]}
            ),
        }


@dataclasses.dataclass(frozen=True)
class RawAnnotationLocation(json_mixins.CamlCaseAndExcludeJsonMixin):
    qualifier: str
    path: str
    line: int


@dataclasses.dataclass(frozen=True)
class RawAnnotation:
    name: str
    location: RawAnnotationLocation


@dataclasses.dataclass(frozen=True)
class RawGlobalAnnotation(json_mixins.CamlCaseAndExcludeJsonMixin, RawAnnotation):
    annotation: str


@dataclasses.dataclass(frozen=True)
class RawAttributeAnnotation(json_mixins.CamlCaseAndExcludeJsonMixin, RawAnnotation):
    parent: str
    annotation: str


@dataclasses.dataclass(frozen=True)
class RawParameter(json_mixins.CamlCaseAndExcludeJsonMixin):
    name: str
    index: int
    annotation: Optional[str] = None
    value: Optional[str] = None


@dataclasses.dataclass(frozen=True)
class RawDefineAnnotation(json_mixins.CamlCaseAndExcludeJsonMixin, RawAnnotation):
    parent: Optional[str] = None
    return_: Optional[str] = dataclasses.field(
        metadata=dataclasses_json.config(field_name="return"), default=None
    )
    parameters: List[RawParameter] = dataclasses.field(default_factory=list)
    is_async: bool = dataclasses.field(
        metadata=dataclasses_json.config(field_name="async"), default=False
    )


TAnnotation = TypeVar("TAnnotation", bound=RawAnnotation)


@dataclasses.dataclass(frozen=True)
class RawInferOutput(json_mixins.CamlCaseAndExcludeJsonMixin):
    global_annotations: List[RawGlobalAnnotation] = dataclasses.field(
        metadata=dataclasses_json.config(field_name="globals"), default_factory=list
    )
    attribute_annotations: List[RawAttributeAnnotation] = dataclasses.field(
        metadata=dataclasses_json.config(field_name="attributes"), default_factory=list
    )
    define_annotations: List[RawDefineAnnotation] = dataclasses.field(
        metadata=dataclasses_json.config(field_name="defines"), default_factory=list
    )

    class ParsingError(Exception):
        pass

    @staticmethod
    def create_from_string(input: str) -> "RawInferOutput":
        try:
            # pyre-fixme[7]: Imprecise return type of `loads()`
            return RawInferOutput.cached_schema().loads(input)
        except (
            TypeError,
            KeyError,
            ValueError,
            dataclasses_json.mm.ValidationError,
        ) as error:
            raise RawInferOutput.ParsingError(str(error)) from error

    @staticmethod
    def create_from_json(input: Dict[str, object]) -> "RawInferOutput":
        return RawInferOutput.create_from_string(json.dumps(input))

    def qualifiers_by_path(self) -> Dict[str, str]:
        return {
            annotation.location.path: annotation.location.qualifier
            for annotation in itertools.chain(
                self.global_annotations,
                self.attribute_annotations,
                self.define_annotations,
            )
        }

    def split_by_path(self) -> "Dict[str, RawInferOutputForPath]":
        def create_index(
            annotations: Sequence[TAnnotation],
        ) -> Dict[str, List[TAnnotation]]:
            result: Dict[str, List[TAnnotation]] = {}
            for annotation in annotations:
                key = annotation.location.path
                result.setdefault(key, []).append(annotation)
            return result

        qualifiers_by_path = self.qualifiers_by_path()
        global_annotation_index = create_index(self.global_annotations)
        attribute_annotation_index = create_index(self.attribute_annotations)
        define_annotation_index = create_index(self.define_annotations)

        return {
            path: RawInferOutputForPath(
                global_annotations=global_annotation_index.get(path, []),
                attribute_annotations=attribute_annotation_index.get(path, []),
                define_annotations=define_annotation_index.get(path, []),
                qualifier=qualifiers_by_path[path],
            )
            for path in global_annotation_index.keys()
            | attribute_annotation_index.keys()
            | define_annotation_index.keys()
        }


@dataclasses.dataclass(frozen=True)
class RawInferOutputForPath(json_mixins.CamlCaseAndExcludeJsonMixin):
    qualifier: str
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
    def create_from_json(input: Dict[str, object]) -> "RawInferOutputForPath":
        # pyre-fixme[7]: Imprecise return type of `loads()`
        return RawInferOutputForPath.cached_schema().loads(json.dumps(input))


def _sanitize_name(name: str) -> str:
    """The last part of the access path is the function/attribute name"""
    return name.split(".")[-1]


@functools.lru_cache()
def empty_module() -> libcst.Module:
    return libcst.parse_module("")


def code_for_node(node: libcst.CSTNode) -> str:
    return empty_module().code_for_node(node)


class AnnotationFixer(libcst.CSTTransformer):
    def __init__(self, qualifier: str) -> None:
        """
        AnnotationFixer sanitizes annotations.

        There are two transformations we apply:

        (1) Strip any prefix matching `prefix` from names. This is because
        the pyre backend always uses fully-qualified names, but in our stubs
        we should not include the prefix for names coming from this module.

        (2) Convert `Pathlike` annotations, which come from pyre in a
        special-cased form that isn't a correct annotation, to a quoted
        `"os.Pathlike[_]"`.

        Note: we eventually will need to either have a proper protocol in the
        backend for generating python-readable types, or extend (2) to handle
        various other special cases where pyre outputs types that are invalid
        in python.

        """
        super().__init__()
        self.qualifier = qualifier

    def leave_Attribute(
        self,
        original_node: libcst.Attribute,
        updated_node: libcst.Attribute,
    ) -> Union[libcst.Attribute, libcst.Name]:
        """
        Note: in order to avoid complex reverse-name-matching, we're
        effectively operating at the top level of attributes, by using only
        the `original_node`. This means the transform we're performing cannot
        be done concurrently with a transform that has to be done
        incrementally.
        """
        value = code_for_node(original_node.value)
        if value == self.qualifier and libcst.matchers.matches(
            original_node.attr, libcst.matchers.Name()
        ):
            return libcst.ensure_type(original_node.attr, libcst.Name)
        return original_node

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
            node_as_string = code_for_node(updated_node.with_changes(value=name_node))
            updated_node = libcst.SimpleString(f"'{node_as_string}'")

        return updated_node

    @staticmethod
    def sanitize(
        annotation: str,
        qualifier: str,
        quote_annotations: bool = False,
        dequalify_all: bool = False,
        runtime_defined: bool = True,
    ) -> str:
        """
        Transform raw annotations in an attempt to reduce incorrectly-imported
        annotations in generated code.

        TODO(T93381000): Handle qualification in a principled way and remove
        this: all of these transforms are attempts to hack simple fixes to the
        problem of us not actually understanding qualified types and existing
        imports.

        (1) If `quote_annotations` is set to True, then spit out a quoted
        raw annotation (with fully-qualified names). The resulting generated
        code will not technically be PEP 484 compliant because it will use
        fully qualified type names without import, but pyre can understand
        this and it's useful for pysa to avoid adding import lines that
        change traces.

        (2) If `dequalify_all` is set, then remove all qualifiers from any
        top-level type (by top-level I mean outside the outermost brackets). For
        example, convert `sqlalchemy.sql.schema.Column[typing.Optional[int]]`
        into `Column[typing.Optional[int]]`.

        (3) Fix PathLike annotations: convert all bare `PathLike` uses to
        `'os.PathLike'`; the ocaml side of pyre currently spits out an unqualified
        type here which is incorrect, and quoting makes the use of os safer
        given that we don't handle imports correctly yet.
        """
        if quote_annotations:
            return f'"{annotation}"'
        if dequalify_all:
            match = re.fullmatch(r"([^.]*?\.)*?([^.]+)(\[.*\])", annotation)
            if match is not None:
                annotation = f"{match.group(2)}{match.group(3)}"

        try:
            tree = libcst.parse_module(annotation)
            annotation = tree.visit(
                AnnotationFixer(
                    qualifier=qualifier,
                )
            ).code
        except libcst._exceptions.ParserSyntaxError:
            pass

        if not runtime_defined:
            return f'"{annotation}"'
        return annotation


@dataclasses.dataclass(frozen=True)
class StubGenerationOptions:
    annotate_attributes: bool = False
    use_future_annotations: bool = False
    quote_annotations: bool = False
    simple_annotations: bool = False
    dequalify: bool = False
    debug_infer: bool = False

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
    qualifier: str
    options: StubGenerationOptions
    runtime_defined: bool

    @staticmethod
    def from_raw(
        annotation: Optional[str],
        options: StubGenerationOptions,
        qualifier: str,
        runtime_defined: bool = True,
    ) -> "TypeAnnotation":
        return TypeAnnotation(
            annotation=annotation,
            qualifier=qualifier,
            options=options,
            runtime_defined=runtime_defined,
        )

    @staticmethod
    def is_simple(sanitized_annotation: str) -> bool:
        """
        This is a flexible definition that should expand if/when our ability to
        handle annotations or imports without manual adjustment improves.
        """
        return len(sanitized_annotation.split(".")) == 1

    def to_stub(self, prefix: str = "") -> str:
        if self.annotation is None:
            return ""
        else:
            sanitized = AnnotationFixer.sanitize(
                annotation=self.annotation,
                qualifier=self.qualifier,
                quote_annotations=self.options.quote_annotations,
                dequalify_all=self.options.dequalify,
                runtime_defined=self.runtime_defined,
            )
            if self.options.simple_annotations and not TypeAnnotation.is_simple(
                sanitized
            ):
                return ""
            return prefix + sanitized

    @property
    def missing(self) -> bool:
        return self.annotation is None


@dataclasses.dataclass(frozen=True)
class Parameter:
    name: str
    annotation: TypeAnnotation
    has_default: bool

    def to_stub(self) -> str:
        delimiter = "=" if self.annotation.missing else " = "
        value = f"{delimiter}..." if self.has_default else ""
        return f"{self.name}{self.annotation.to_stub(prefix=': ')}{value}"


@dataclasses.dataclass(frozen=True)
class FunctionAnnotation:
    name: str
    return_annotation: TypeAnnotation
    parameters: Sequence[Parameter]
    is_async: bool

    def to_stub(self) -> str:
        name = _sanitize_name(self.name)
        async_ = "async " if self.is_async else ""
        parameters = ", ".join(parameter.to_stub() for parameter in self.parameters)
        return_ = self.return_annotation.to_stub(prefix=" -> ")
        return f"{async_}def {name}({parameters}){return_}: ..."


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
        return f"{name}: {self.annotation.to_stub()} = ..."


@dataclasses.dataclass(frozen=True)
class GlobalAnnotation(FieldAnnotation):
    pass


@dataclasses.dataclass(frozen=True)
class AttributeAnnotation(FieldAnnotation):
    parent: str


@dataclasses.dataclass(frozen=True)
class ModuleAnnotations:
    qualifier: str
    path: str
    options: StubGenerationOptions
    globals_: List[GlobalAnnotation] = dataclasses.field(default_factory=list)
    attributes: List[AttributeAnnotation] = dataclasses.field(default_factory=list)
    functions: List[FunctionAnnotation] = dataclasses.field(default_factory=list)
    methods: List[MethodAnnotation] = dataclasses.field(default_factory=list)

    @staticmethod
    def from_infer_output(
        path: str,
        infer_output: RawInferOutputForPath,
        options: StubGenerationOptions,
    ) -> "ModuleAnnotations":
        def type_annotation(
            annotation: Optional[str], parent_class: Optional[str] = None
        ) -> TypeAnnotation:
            return TypeAnnotation.from_raw(
                annotation,
                qualifier=infer_output.qualifier,
                options=options,
                runtime_defined=parent_class != annotation if parent_class else True,
            )

        return ModuleAnnotations(
            qualifier=infer_output.qualifier,
            path=path,
            globals_=[
                GlobalAnnotation(
                    name=global_.name, annotation=type_annotation(global_.annotation)
                )
                for global_ in infer_output.global_annotations
            ],
            attributes=(
                [
                    AttributeAnnotation(
                        parent=attribute.parent,
                        name=attribute.name,
                        annotation=type_annotation(
                            attribute.annotation, attribute.parent
                        ),
                    )
                    for attribute in infer_output.attribute_annotations
                ]
                if options.annotate_attributes
                else []
            ),
            functions=[
                FunctionAnnotation(
                    name=define.name,
                    return_annotation=type_annotation(define.return_),
                    parameters=[
                        Parameter(
                            name=parameter.name,
                            annotation=type_annotation(parameter.annotation),
                            has_default=parameter.value is not None,
                        )
                        for parameter in define.parameters
                    ],
                    is_async=define.is_async,
                )
                for define in infer_output.define_annotations
                if define.parent is None
            ],
            methods=[
                MethodAnnotation(
                    parent=define.parent,
                    name=define.name,
                    return_annotation=type_annotation(define.return_, define.parent),
                    parameters=[
                        Parameter(
                            name=parameter.name,
                            annotation=type_annotation(
                                parameter.annotation, define.parent
                            ),
                            has_default=parameter.value is not None,
                        )
                        for parameter in define.parameters
                    ],
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
        nested_classes = set()
        for annotation in [*self.attributes, *self.methods]:
            relative_parent_class = _remove_prefix(
                text=annotation.parent, prefix=self.qualifier + "."
            )
            if "." in relative_parent_class:
                nested_classes.add(relative_parent_class)
            else:
                classes.setdefault(relative_parent_class, []).append(annotation)
        if len(nested_classes) > 0:
            nested_classes_string = textwrap.shorten(
                ", ".join(sorted(nested_classes)),
                width=MAX_NESTED_CLASS_STRING_LENGTH,
                placeholder="...",
            )
            LOG.warning(
                f"In file {self.path}, ignored {len(nested_classes)} nested classes: {nested_classes_string}"
            )
        return classes

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
    options: StubGenerationOptions

    @staticmethod
    def _annotated_code(
        code_path: str,
        stub: str,
        code: str,
        options: StubGenerationOptions,
    ) -> Optional[str]:
        """
        Merge inferred annotations from stubs with source code to get
        annotated code.
        """
        if "@" "generated" in code:
            LOG.warning(f"Skipping generated file {code_path}")
            return
        context = CodemodContext()
        ApplyTypeAnnotationsVisitor.store_stub_in_context(
            context=context,
            stub=libcst.parse_module(stub),
            use_future_annotations=options.use_future_annotations,
        )
        modified_tree = ApplyTypeAnnotationsVisitor(context).transform_module(
            libcst.parse_module(code)
        )
        return modified_tree.code

    @staticmethod
    def annotate_code(
        stub_path: str,
        code_path: str,
        options: StubGenerationOptions,
    ) -> None:
        "Merge a stub file of inferred annotations with a code file in place."
        try:
            stub = Path(stub_path).read_text()
            code = Path(code_path).read_text()
            annotated_code = AnnotateModuleInPlace._annotated_code(
                code_path=code_path,
                stub=stub,
                code=code,
                options=options,
            )
            if annotated_code is not None:
                Path(code_path).write_text(annotated_code)
            LOG.info(f"Annotated {code_path}")
        except Exception as error:
            LOG.warning(f"Failed to annotate {code_path}")
            if options.debug_infer:
                LOG.warning(f"\tError: {error}")

    def run(self) -> None:
        return self.annotate_code(
            stub_path=self.full_stub_path,
            code_path=self.full_code_path,
            options=self.options,
        )

    @staticmethod
    def run_task(task: "AnnotateModuleInPlace") -> None:
        "Wrap `run` in a static method to use with multiprocessing"
        return task.run()


# For Python<3.9 compatibility
def _remove_prefix(text: str, prefix: str) -> str:
    if text.startswith(prefix):
        return text[len(prefix) :]
    return text


def create_infer_arguments(
    configuration: frontend_configuration.Base,
    infer_arguments: command_arguments.InferArguments,
) -> Arguments:
    """
    Translate client configurations to backend check configurations.

    This API is not pure since it needs to access filesystem to filter out
    nonexistent directories. It is idempotent though, since it does not alter
    any filesystem state.
    """
    source_paths = backend_arguments.get_source_path_for_check(configuration)

    log_directory = configuration.get_log_directory()
    profiling_output = (
        backend_arguments.get_profiling_log_path(log_directory)
        if infer_arguments.enable_profiling
        else None
    )
    memory_profiling_output = (
        backend_arguments.get_profiling_log_path(log_directory)
        if infer_arguments.enable_memory_profiling
        else None
    )

    logger = configuration.get_remote_logger()
    remote_logging = (
        backend_arguments.RemoteLogging(
            logger=logger, identifier=infer_arguments.log_identifier or ""
        )
        if logger is not None
        else None
    )

    return Arguments(
        base_arguments=backend_arguments.BaseArguments(
            log_path=str(log_directory),
            global_root=str(configuration.get_global_root()),
            checked_directory_allowlist=backend_arguments.get_checked_directory_allowlist(
                configuration, source_paths
            ),
            checked_directory_blocklist=(configuration.get_ignore_all_errors()),
            debug=infer_arguments.debug_infer,
            excludes=configuration.get_excludes(),
            extensions=configuration.get_valid_extension_suffixes(),
            relative_local_root=configuration.get_relative_local_root(),
            memory_profiling_output=memory_profiling_output,
            number_of_workers=configuration.get_number_of_workers(),
            parallel=not infer_arguments.sequential,
            profiling_output=profiling_output,
            python_version=configuration.get_python_version(),
            shared_memory=configuration.get_shared_memory(),
            remote_logging=remote_logging,
            search_paths=configuration.get_existent_search_paths(),
            source_paths=source_paths,
        ),
        paths_to_modify=infer_arguments.paths_to_modify,
    )


@contextlib.contextmanager
def create_infer_arguments_and_cleanup(
    configuration: frontend_configuration.Base,
    infer_arguments: command_arguments.InferArguments,
) -> Iterator[Arguments]:
    arguments = create_infer_arguments(configuration, infer_arguments)
    try:
        yield arguments
    finally:
        # It is safe to clean up source paths after infer command since
        # any created artifact directory won't be reused by other commands.
        arguments.base_arguments.source_paths.cleanup()


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
            # lint-ignore: NoUnsafeExecRule
            result = subprocess.run(
                command,
                stdout=subprocess.PIPE,
                stderr=log_file.file,
                universal_newlines=True,
                errors="replace",
            )
            return_code = result.returncode

            # Interpretation of the return code needs to be kept in sync with
            # `source/command/inferCommand.ml`.
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
    configuration: frontend_configuration.Base,
    infer_arguments: command_arguments.InferArguments,
) -> str:
    start_command = configuration.get_server_start_command(download_if_needed=True)
    if start_command is None:
        raise configuration_module.InvalidConfiguration(
            "Cannot locate a Pyre binary to run."
        )
    LOG.info(f"Pyre binary is located at `{start_command.get_pyre_binary_location()}`")

    with create_infer_arguments_and_cleanup(
        configuration, infer_arguments
    ) as arguments:
        with backend_arguments.temporary_argument_file(arguments) as argument_file_path:
            infer_command = [
                str(start_command.get_pyre_binary_location()),
                "infer",
                str(argument_file_path),
            ]
            return _run_infer_command_get_output(command=infer_command)


def _load_output(
    configuration: frontend_configuration.Base,
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
    infer_output_relativized: Dict[Optional[str], RawInferOutputForPath] = {
        _relativize_path(path, against=base_path): data
        for path, data in infer_output.split_by_path().items()
    }
    return [
        ModuleAnnotations.from_infer_output(
            path=path,
            infer_output=infer_output_for_path,
            options=options,
        )
        for path, infer_output_for_path in infer_output_relativized.items()
        if path is not None
    ]


def _print_inferences(
    infer_output: RawInferOutput, module_annotations: Sequence[ModuleAnnotations]
) -> None:
    LOG.log(log.SUCCESS, "Raw Infer Outputs:")
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


def should_annotate_in_place(
    path: Path,
    paths_to_modify: Optional[Set[Path]],
) -> bool:
    return (
        True
        if paths_to_modify is None
        else any(path in paths_to_modify for path in (path, *path.parents))
    )


def _annotate_in_place(
    working_directory: Path,
    type_directory: Path,
    paths_to_modify: Optional[Set[Path]],
    options: StubGenerationOptions,
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
                    options=options,
                )
            )

    with multiprocessing.Pool(number_of_workers) as pool:
        for _ in pool.imap_unordered(AnnotateModuleInPlace.run_task, tasks):
            pass


def run(
    configuration: frontend_configuration.Base,
    infer_arguments: command_arguments.InferArguments,
) -> commands.ExitCode:
    working_directory = infer_arguments.working_directory
    _check_working_directory(
        working_directory=working_directory,
        global_root=configuration.get_global_root(),
        relative_local_root=configuration.get_relative_local_root(),
    )

    type_directory = _get_type_directory(configuration.get_log_directory())
    in_place = infer_arguments.in_place

    options = StubGenerationOptions(
        annotate_attributes=infer_arguments.annotate_attributes,
        use_future_annotations=infer_arguments.use_future_annotations,
        dequalify=infer_arguments.dequalify,
        quote_annotations=infer_arguments.quote_annotations,
        simple_annotations=infer_arguments.simple_annotations,
        debug_infer=infer_arguments.debug_infer,
    )

    if infer_arguments.annotate_from_existing_stubs:
        if not in_place:
            raise ValueError(
                "`--annotate-from-existing-stubs` cannot be used without the"
                " `--in-place` flag"
            )
        _annotate_in_place(
            working_directory=working_directory,
            type_directory=type_directory,
            paths_to_modify=infer_arguments.paths_to_modify,
            options=options,
            number_of_workers=configuration.get_number_of_workers(),
        )
    else:
        infer_output = RawInferOutput.create_from_json(
            json.loads(_load_output(configuration, infer_arguments))[0]
        )
        module_annotations = create_module_annotations(
            infer_output=infer_output,
            base_path=working_directory,
            options=options,
        )
        if infer_arguments.print_only:
            _print_inferences(infer_output, module_annotations)
        else:
            _write_stubs(type_directory, module_annotations)
            if in_place:
                _annotate_in_place(
                    working_directory=working_directory,
                    type_directory=type_directory,
                    paths_to_modify=infer_arguments.paths_to_modify,
                    options=options,
                    number_of_workers=configuration.get_number_of_workers(),
                )
    return commands.ExitCode.SUCCESS
