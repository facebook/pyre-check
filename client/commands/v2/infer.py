# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import contextlib
import dataclasses
import enum
import logging
import os
import subprocess
import sys
from pathlib import Path
from typing import Any, Dict, Iterator, List, Optional, Sequence, TypeVar

import dataclasses_json

from ... import commands, command_arguments, configuration as configuration_module
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
    ignore_infer: Sequence[str] = dataclasses.field(default_factory=list)
    infer_mode: InferMode = InferMode.LOCAL
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

    @property
    def local_root(self) -> Optional[str]:
        if self.relative_local_root is None:
            return None
        return os.path.join(self.global_root, self.relative_local_root)

    def serialize(self) -> Dict[str, Any]:
        local_root = self.local_root
        return {
            "source_paths": self.source_paths.serialize(),
            "search_paths": [
                element.command_line_argument() for element in self.search_paths
            ],
            "excludes": self.excludes,
            "checked_directory_allowlist": self.checked_directory_allowlist,
            "checked_directory_blocklist": self.checked_directory_blocklist,
            "extensions": self.extensions,
            "ignore_infer": self.ignore_infer,
            "infer_mode": self.infer_mode.serialize(),
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
    def create(input: str) -> "RawInferOutput":
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


@dataclasses.dataclass(frozen=True)
class StubGenerationOptions:
    annotate_attributes: bool
    use_future_annotations: bool
    dequalify: bool


@dataclasses.dataclass(frozen=True)
class TypeAnnotation:
    annotation: Optional[str]
    dequalify: bool

    @staticmethod
    def from_raw(
        annotation: Optional[str], options: StubGenerationOptions
    ) -> "TypeAnnotation":
        return TypeAnnotation(annotation=annotation, dequalify=options.dequalify)

    @property
    def missing(self) -> bool:
        return self.annotation is None


@dataclasses.dataclass(frozen=True)
class Parameter:
    name: str
    annotation: TypeAnnotation
    value: Optional[str]


@dataclasses.dataclass(frozen=True)
class FunctionAnnotation:
    name: str
    return_annotation: TypeAnnotation
    parameters: Sequence[Parameter]
    decorators: Sequence[str]
    is_async: bool


@dataclasses.dataclass(frozen=True)
class MethodAnnotation(FunctionAnnotation):
    parent: str


@dataclasses.dataclass(frozen=True)
class FieldAnnotation:
    name: str
    annotation: TypeAnnotation


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
        infer_mode=infer_mode,
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


def _check_arguments(infer_arguments: command_arguments.InferArguments) -> None:
    if (
        infer_arguments.annotate_from_existing_stubs
        and infer_arguments.paths_to_modify is None
    ):
        raise ValueError(
            "`--annotate-from-existing-stubs` cannot be used without the"
            " `--in-place` flag"
        )


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


def run_infer(
    configuration: configuration_module.Configuration,
    infer_arguments: command_arguments.InferArguments,
) -> commands.ExitCode:
    working_directory = Path.cwd()
    _check_arguments(infer_arguments)
    _check_working_directory(
        working_directory=working_directory,
        global_root=Path(configuration.project_root),
        relative_local_root=configuration.relative_local_root,
    )

    raw_output = _load_output(configuration, infer_arguments)
    LOG.warning(f"{raw_output}")
    LOG.warning("WORK IN PROGRESS...")
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
