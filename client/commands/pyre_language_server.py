# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module is responsible for handling requests from the VScode language server and generating an appropriate response.

The response typically will be generated through the Pyre daemon, and the name PyreLanguageServer was chosen for this module
because it illustrates that this is the intermediary between the Language server and the Pyre daemon.
"""

from __future__ import annotations

import abc
import asyncio
import dataclasses
import enum
import functools
import json
import logging
import os
import random
import subprocess
import tempfile
import traceback
from collections import defaultdict
from pathlib import Path
from typing import (
    Callable,
    Collection,
    Coroutine,
    DefaultDict,
    Dict,
    Generic,
    List,
    Mapping,
    Optional,
    Protocol,
    Set,
    TypeVar,
    Union,
)

from pyre_extensions import ParameterSpecification
from pyre_extensions.type_variable_operators import Concatenate

from .. import (
    background_tasks,
    dataclasses_json_extensions as json_mixins,
    error,
    identifiers,
    json_rpc,
    log,
    timer,
)
from ..language_server import (
    connections,
    daemon_connection,
    features,
    protocol as lsp,
)
from . import (
    commands,
    daemon_querier,
    find_symbols,
    libcst_util,
    server_state as state,
    type_error_handler,
)
from .daemon_querier import DaemonQueryFailure, GetDefinitionLocationsResponse
from .document_formatter import AbstractDocumentFormatter
from .pyre_language_server_error import (
    get_language_server_empty_reason,
    get_language_server_error,
    PyreLanguageServerError,
)
from .server_state import OpenedDocumentState
from .source_code_context import SourceCodeContext

LOG: logging.Logger = logging.getLogger(__name__)
CONSECUTIVE_START_ATTEMPT_THRESHOLD: int = 5
PTT_PREEMPTED_RETRIES: int = 3
PYAUTOTARGETS_ENABLED_SUFFIXES: Set[str] = {
    ".py",
}
PTT_ISOLATION_PREFIX: str = ".pyrelsp"


@dataclasses.dataclass(frozen=True)
class FilesForTypeChecker:
    daemon_files: Set[Path]
    daemon_files_use_buck: Optional[bool]
    buck_files: Set[Path]
    error_message: Optional[str]
    duration: float
    did_run: bool

    def to_json(self) -> Dict[str, object]:
        return {
            "daemon_files": [str(path) for path in self.daemon_files],
            "buck_files": [str(path) for path in self.buck_files],
            "error_message": self.error_message,
            "duration": self.duration,
            "did_run": self.did_run,
            "daemon_files_use_buck": self.daemon_files_use_buck,
        }


class PyreDaemonTypeCheckStatus(enum.Enum):
    OK = "ok"
    NO_FILES = "no_files"
    NOT_ENABLED = "not_enabled"


@dataclasses.dataclass(frozen=True)
class PyreDaemonTypeErrors:
    type_errors: Dict[Path, List[error.Error]]
    error_message: Optional[str]
    duration: float
    reason: PyreDaemonTypeCheckStatus

    def type_errors_to_json(self) -> Dict[str, object]:
        return {
            str(path): [document_error.to_json() for document_error in errors]
            for path, errors in self.type_errors.items()
        }

    def to_json(self) -> Dict[str, object]:
        return {
            "type_errors": self.type_errors_to_json(),
            "error_message": self.error_message,
            "duration": self.duration,
            "reason": self.reason.value,
        }


@dataclasses.dataclass(frozen=True)
class BuckTargetTypeErrors(json_mixins.CamlCaseAndExcludeJsonMixin):
    errors: List[error.Error]


@dataclasses.dataclass(frozen=True)
class BuckTypeErrorsResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    artifacts: Dict[str, List[str]]
    root: str


class BuckTypeCheckStatus(enum.Enum):
    SUCCESS = "success"
    BUCK_ERROR = "buck_error"
    USER_ERROR = "user_error"
    PARSE_ERROR = "parse_error"
    NOT_RUN = "not_run"
    PREEMPTED = "preempted"
    PREEMPTED_SHOULD_RETRY = "preempted_should_retry"

    @classmethod
    def from_returncode(
        cls, returncode: Optional[int], remaining_tasks: Optional[int] = None
    ) -> BuckTypeCheckStatus:
        if returncode == 0:
            return BuckTypeCheckStatus.SUCCESS
        elif returncode == 3:
            return BuckTypeCheckStatus.USER_ERROR
        elif returncode == 5 and remaining_tasks == 0:
            return BuckTypeCheckStatus.PREEMPTED_SHOULD_RETRY
        elif returncode == 5:
            return BuckTypeCheckStatus.PREEMPTED
        else:
            return BuckTypeCheckStatus.BUCK_ERROR


@dataclasses.dataclass(frozen=True)
class PyreBuckTypeErrorMetadata:
    status: BuckTypeCheckStatus
    duration: float
    type_errors: Dict[Path, Set[error.Error]]
    build_id: Optional[str] = None
    error_message: Optional[str] = None
    retries: int = 0

    def type_errors_to_json(self) -> Dict[str, object]:
        result: Dict[str, object] = {
            str(path): [type_error.to_json() for type_error in type_errors]
            for path, type_errors in self.type_errors.items()
        }
        return result

    def to_json(self) -> Dict[str, object]:
        return {
            "status": self.status.value,
            "duration": self.duration,
            "type_errors": self.type_errors_to_json(),
            "build_id": self.build_id,
            "error_message": self.error_message,
            "retries": self.retries,
        }


@dataclasses.dataclass(frozen=True)
class PythonAutoTargetsMetadata:
    duration: Optional[float]
    error_message: Optional[str]


@functools.lru_cache(maxsize=1)
def get_buck_root() -> str:
    buck_root_query_parameters = ["buck2", "root", "--kind", "project"]
    LOG.debug(f"Getting Buck root: `{' '.join(buck_root_query_parameters)}`")
    buck2_root_query = subprocess.run(
        buck_root_query_parameters,
        capture_output=True,
        check=True,
    )

    return buck2_root_query.stdout.decode("utf-8").strip()


async def read_lsp_request(
    input_channel: connections.AsyncTextReader,
    output_channel: connections.AsyncTextWriter,
) -> json_rpc.Request:
    while True:
        try:
            message = await lsp.read_json_rpc(input_channel)
            return message
        except json_rpc.JSONRPCException as json_rpc_error:
            LOG.error(f"Exception occurred while reading JSON RPC: {json_rpc_error}")
            await lsp.write_json_rpc_ignore_connection_error(
                output_channel,
                json_rpc.ErrorResponse(
                    id=None,
                    code=json_rpc_error.error_code(),
                    message=str(json_rpc_error),
                ),
            )


async def _wait_for_exit(
    input_channel: connections.AsyncTextReader,
    output_channel: connections.AsyncTextWriter,
) -> None:
    """
    Wait for an LSP "exit" request from the `input_channel`. This is mostly useful
    when the LSP server has received a "shutdown" request, in which case the LSP
    specification dictates that only "exit" can be sent from the client side.

    If a non-exit LSP request is received, drop it and keep waiting on another
    "exit" request.
    """
    while True:
        request = await read_lsp_request(input_channel, output_channel)
        if request.method != "exit":
            LOG.debug(f"Non-exit request received after shutdown: {request}")
            continue
        # Got an exit request. Stop the wait.
        return


def daemon_failure_string(operation: str, type_string: str, error_message: str) -> str:
    return f"For {operation} request, encountered failure response of type: {type_string}, error_message: {error_message}"


QueryResultType = TypeVar("QueryResultType")


@dataclasses.dataclass(frozen=True)
class QueryResultWithDurations(Generic[QueryResultType]):
    result: Union[QueryResultType, DaemonQueryFailure]
    overlay_update_duration: float
    query_duration: float
    overall_duration: float


@dataclasses.dataclass(frozen=True)
class DefinitionTimingData:
    dispatch_request_duration: Optional[float]
    process_request_duration: float
    marshalling_response_duration: float
    write_response_duration: float


class PyreLanguageServerApi(abc.ABC):
    @abc.abstractmethod
    async def write_telemetry(
        self,
        parameters: Dict[str, object],
        activity_key: Optional[Dict[str, object]],
    ) -> None:
        raise NotImplementedError()

    @abc.abstractmethod
    def get_language_server_features(self) -> features.LanguageServerFeatures:
        raise NotImplementedError()

    @abc.abstractmethod
    async def update_overlay_if_needed(self, document_path: Path) -> float:
        raise NotImplementedError()

    @abc.abstractmethod
    def sample_source_code(
        self,
        document_path: Path,
        position: lsp.LspPosition,
    ) -> Optional[str]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def process_open_request(
        self,
        parameters: lsp.DidOpenTextDocumentParameters,
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    @abc.abstractmethod
    async def process_close_request(
        self, parameters: lsp.DidCloseTextDocumentParameters
    ) -> None:
        raise NotImplementedError()

    @abc.abstractmethod
    async def process_did_change_request(
        self,
        parameters: lsp.DidChangeTextDocumentParameters,
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    @abc.abstractmethod
    async def process_did_save_request(
        self,
        parameters: lsp.DidSaveTextDocumentParameters,
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    @abc.abstractmethod
    async def process_type_coverage_request(
        self,
        parameters: lsp.TypeCoverageParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    @abc.abstractmethod
    async def process_hover_request(
        self,
        parameters: lsp.HoverParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    @abc.abstractmethod
    async def process_definition_request(
        self,
        parameters: lsp.DefinitionParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
        # TODO(T184349028) Remove hacky funnel logging
        dispatch_request_duration: Optional[float] = None,
    ) -> Optional[Exception]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def process_completion_request(
        self,
        parameters: lsp.CompletionParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> Optional[Exception]:
        raise NotImplementedError()

    @abc.abstractmethod
    async def process_document_symbols_request(
        self,
        parameters: lsp.DocumentSymbolsParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    @abc.abstractmethod
    async def process_find_all_references_request(
        self,
        parameters: lsp.ReferencesParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    @abc.abstractmethod
    async def process_call_hierarchy_request(
        self,
        parameters: lsp.CallHierarchyParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    @abc.abstractmethod
    async def process_call_hierarchy_incoming_call(
        self,
        parameters: lsp.CallHierarchyIncomingCallParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    @abc.abstractmethod
    async def process_call_hierarchy_outgoing_call(
        self,
        parameters: lsp.CallHierarchyOutgoingCallParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    @abc.abstractmethod
    async def process_symbol_search_request(
        self,
        parameters: lsp.WorkspaceSymbolParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    @abc.abstractmethod
    async def process_document_formatting_request(
        self,
        parameters: lsp.DocumentFormattingParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    @abc.abstractmethod
    async def process_rename_request(
        self,
        parameters: lsp.RenameParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        raise NotImplementedError()

    @abc.abstractmethod
    async def process_shutdown_request(self, request_id: Union[int, str, None]) -> None:
        raise NotImplementedError()


P = ParameterSpecification("P")
T = TypeVar("T")


class LanguageServerDecorator(Protocol):
    def __call__(
        self,
        f: Callable[Concatenate[PyreLanguageServer, P], Coroutine[None, None, T]],
        /,
    ) -> Callable[Concatenate[PyreLanguageServer, P], Coroutine[None, None, T]]: ...


# Decorator factory for catching exceptions and logging as telemetry events.
def log_exceptions_factory(
    operation: str,
) -> LanguageServerDecorator:
    def log_exceptions(
        func: Callable[Concatenate[PyreLanguageServer, P], Coroutine[None, None, T]],
    ) -> Callable[Concatenate[PyreLanguageServer, P], Coroutine[None, None, T]]:
        async def new_func(
            self_: PyreLanguageServer, *args: P.args, **kwargs: P.kwargs
        ) -> T:
            try:
                return await func(self_, *args, **kwargs)
            except json_rpc.InvalidRequestError as exception:
                await self_.write_telemetry(
                    {
                        "type": "LSP",
                        "operation": operation,
                        "server_state_open_documents_count": len(
                            self_.server_state.opened_documents
                        ),
                        "error_message": f"exception occurred in handling request: {traceback.format_exception(exception)}",
                        **self_.server_state.status_tracker.get_status().as_telemetry_dict(),
                        "error_type": PyreLanguageServerError.DOCUMENT_PATH_IS_NULL,
                    },
                    activity_key=None,
                )
                raise exception

            except Exception as exception:
                await self_.write_telemetry(
                    {
                        "type": "LSP",
                        "operation": operation,
                        "server_state_open_documents_count": len(
                            self_.server_state.opened_documents
                        ),
                        "error_message": f"exception occurred in handling request: {traceback.format_exception(exception)}",
                        **self_.server_state.status_tracker.get_status().as_telemetry_dict(),
                    },
                    activity_key=None,
                )
                raise exception

        return new_func

    return log_exceptions


@dataclasses.dataclass(frozen=True)
class PyreLanguageServer(PyreLanguageServerApi):
    # Channel to send responses to the editor
    output_channel: connections.AsyncTextWriter

    # NOTE: The fields inside `server_state` are mutable and can be changed by the background
    # task.
    server_state: state.ServerState

    querier: daemon_querier.AbstractDaemonQuerier
    index_querier: daemon_querier.AbstractDaemonQuerier
    document_formatter: Optional[AbstractDocumentFormatter]
    client_type_error_handler: type_error_handler.ClientTypeErrorHandler

    async def write_telemetry(
        self,
        parameters: Dict[str, object],
        activity_key: Optional[Dict[str, object]],
    ) -> None:
        should_write_telemetry = self.server_state.server_options.language_server_features.telemetry.is_enabled()
        if should_write_telemetry:
            parameters = dict(parameters)
            parameters["project_identifier"] = (
                self.server_state.server_options.project_identifier
            )
            # TODO:(T165048078): remove this if we decide against pursuing global lazy type check
            parameters["enabled_features"] = {
                "type_errors_enabled": self.get_language_server_features().type_errors.is_enabled()
            }
            await lsp.write_json_rpc_ignore_connection_error(
                self.output_channel,
                json_rpc.Request(
                    activity_key=activity_key,
                    method="telemetry/event",
                    parameters=json_rpc.ByNameParameters(parameters),
                ),
            )

    def get_language_server_features(self) -> features.LanguageServerFeatures:
        return self.server_state.server_options.language_server_features

    async def update_overlay_if_needed(self, document_path: Path) -> float:
        """
        Send an overlay update to the daemon if three conditions are met:
        - unsaved changes support is enabled
        - a document is listed in `server_state.opened_documents`
        - the OpenedDocumentState says the overlay overlay may be stale

        Returns the time taken to run the update.
        """
        update_timer = timer.Timer()
        if (
            self.get_language_server_features().unsaved_changes.is_enabled()
            and document_path in self.server_state.opened_documents
        ):
            opened_document_state = self.server_state.opened_documents[document_path]
            code_changes = opened_document_state.code
            current_is_dirty_state = opened_document_state.is_dirty
            if not opened_document_state.pyre_code_updated:
                result = await self.querier.update_overlay(
                    path=document_path, code=code_changes
                )
                if isinstance(result, daemon_connection.DaemonConnectionFailure):
                    LOG.info(
                        daemon_failure_string(
                            "didChange", str(type(result)), result.error_message
                        )
                    )
                    LOG.info(result.error_message)
                else:
                    self.server_state.opened_documents[document_path] = (
                        OpenedDocumentState(
                            code=code_changes,
                            is_dirty=current_is_dirty_state,
                            pyre_code_updated=True,
                        )
                    )
        return update_timer.stop_in_millisecond()

    def sample_source_code(
        self,
        document_path: Path,
        position: lsp.LspPosition,
    ) -> Optional[str]:
        sample_percent = 10
        if random.randrange(0, 100) >= sample_percent:
            LOG.debug("Skipping file content sampling.")
            return None
        if document_path not in self.server_state.opened_documents:
            source_code_context = f"Error: Document path: {document_path} could not be found in opened documents structure"
        else:
            source_code_context = SourceCodeContext.from_source_and_position(
                self.server_state.opened_documents[document_path].code,
                position,
            )
        if source_code_context is None:
            source_code_context = f"""
            ERROR: Position specified by parameters: {position} is an illegal position.
            Check if the position contains negative numbers or if it is
            larger than the bounds of the file path: {document_path}
            """
            LOG.warning(source_code_context)
        LOG.debug(
            f"Logging file contents to scuba near requested line"
            f" for definition request position: {position}"
        )
        return source_code_context

    async def process_open_request(
        self,
        parameters: lsp.DidOpenTextDocumentParameters,
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )
        document_path = document_path.resolve()
        self.server_state.opened_documents[document_path] = OpenedDocumentState(
            code=parameters.text_document.text,
            is_dirty=False,
            pyre_code_updated=True,
        )
        LOG.info(f"File opened: {document_path}")
        await self.querier.handle_file_opened(
            document_path, parameters.text_document.text
        )

        if (
            self.get_language_server_features().type_errors.is_enabled()
            # TODO (T165048078): hack to get this working only for codenav server
            and self.server_state.server_options.flavor
            == identifiers.PyreFlavor.CODE_NAVIGATION
        ):
            await self.handle_overlay_type_errors(
                document_path=document_path,
                new_file_loaded=True,
                activity_key=activity_key,
            )

    async def process_close_request(
        self, parameters: lsp.DidCloseTextDocumentParameters
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )
        document_path = document_path.resolve()
        try:
            del self.server_state.opened_documents[document_path]
            LOG.info(f"File closed: {document_path}")
            await self.querier.handle_file_closed(document_path)
        except KeyError:
            LOG.warning(f"Trying to close an un-opened file: {document_path}")

    async def _publish_type_errors_for_files(
        self,
        type_errors: Mapping[Path, Collection[error.Error]],
        type_checked_files: Set[Path],
        set_unused_as_warning: bool = False,
    ) -> None:
        for file in type_checked_files:
            document_errors = type_errors.get(file, set())
            await self.client_type_error_handler.show_overlay_type_errors(
                path=file,
                type_errors=list(document_errors),
                set_unused_as_warning=set_unused_as_warning,
            )

    @staticmethod
    def _process_buck_target_type_errors(
        buck_root: Path,
        file_path: str,
    ) -> Dict[Path, Set[error.Error]]:
        error_result = BuckTargetTypeErrors.from_json(
            (buck_root / file_path).read_text()
        )
        result: defaultdict[Path, Set[error.Error]] = defaultdict(set)
        for type_error in error_result.errors:
            type_error = type_error.with_path(buck_root / type_error.path)
            result[type_error.path].add(type_error)
        return result

    def _process_buck_type_errors_from_stdout(
        self,
        stdout: str,
    ) -> Dict[Path, Set[error.Error]]:
        buck_result = BuckTypeErrorsResponse.from_json(stdout)
        buck_root = Path(buck_result.root)
        errors: defaultdict[Path, Set[error.Error]] = defaultdict(set)
        for target_error_artifacts in buck_result.artifacts.values():
            for target_errors in target_error_artifacts:
                for path, type_errors in self._process_buck_target_type_errors(
                    buck_root, target_errors
                ).items():
                    errors[path] |= type_errors
        return dict(errors)

    async def _query_buck_for_type_errors(
        self,
        type_checkable_files: Set[Path],
        buck_query_timer: timer.Timer,
        retries: int,
    ) -> PyreBuckTypeErrorMetadata:
        open_documents = set(self.server_state.opened_documents.keys())
        # filter out documents that may have been closed in between preempts
        type_checkable_files &= open_documents
        with tempfile.NamedTemporaryFile(
            mode="rb"
        ) as build_id_file, tempfile.NamedTemporaryFile(mode="w") as argfile:
            print("--", file=argfile)
            print(
                *[
                    argument
                    for file in type_checkable_files
                    for argument in ["--source", str(file)]
                ],
                sep="\n",
                file=argfile,
            )
            argfile.flush()

            type_check_parameters = [
                "buck2",
                f"--isolation-dir={PTT_ISOLATION_PREFIX}",
                "bxl",
                "--reuse-current-config",
                "--preemptible=ondifferentstate",
                "--oncall=pyre",
                "--client-metadata=id=pyre.ide",
                f"--write-build-id={build_id_file.name}",
                "prelude//python/typecheck/batch_files.bxl:run",
                f"@{argfile.name}",
            ]
            LOG.debug(
                f"Querying Buck for type errors: `{' '.join(type_check_parameters)}`"
            )

            self.server_state.ongoing_type_checks += 1
            type_check = await asyncio.create_subprocess_exec(
                *type_check_parameters,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
            )
            stdout_data, stderr_data = await type_check.communicate()
            self.server_state.ongoing_type_checks -= 1

            stdout_text = stdout_data.decode("utf-8")
            stderr_text = stderr_data.decode("utf-8")
            LOG.debug(f"Buck type check results:\n{stdout_text}\n{stderr_text}")

            build_id = build_id_file.read().decode("utf-8").strip()
            LOG.debug(f"Buck type check build ID: {build_id}")

        error_message = None
        # return now if buck build was unsuccessful or not preempted
        result_status = BuckTypeCheckStatus.from_returncode(
            type_check.returncode, remaining_tasks=self.server_state.ongoing_type_checks
        )
        if result_status != BuckTypeCheckStatus.SUCCESS:
            if result_status not in (
                BuckTypeCheckStatus.PREEMPTED,
                BuckTypeCheckStatus.PREEMPTED_SHOULD_RETRY,
            ):
                error_message = (
                    f"Got error exit code from Buck type check: {stderr_text}"
                )
                LOG.error(error_message)
            return PyreBuckTypeErrorMetadata(
                status=result_status,
                duration=buck_query_timer.stop_in_millisecond(),
                type_errors={},
                build_id=build_id,
                error_message=error_message,
                retries=retries,
            )
        type_errors: Dict[Path, Set[error.Error]] = {}
        try:
            type_errors = self._process_buck_type_errors_from_stdout(stdout_text)
            LOG.debug(f"Received type errors from Buck: {type_errors}")
            await self._publish_type_errors_for_files(
                type_errors, type_checkable_files, set_unused_as_warning=True
            )
        except (
            json.JSONDecodeError,
            AssertionError,
            error.ErrorParsingFailure,
        ) as buck_error:
            error_message = f"Error parsing Buck type errors: {repr(buck_error)}"
            LOG.error(error_message)

        return PyreBuckTypeErrorMetadata(
            status=(
                BuckTypeCheckStatus.SUCCESS
                if error_message is None
                else BuckTypeCheckStatus.PARSE_ERROR
            ),
            duration=buck_query_timer.stop_in_millisecond(),
            type_errors=type_errors,
            build_id=build_id,
            error_message=error_message,
            retries=retries,
        )

    async def _get_buck_type_errors(
        self, type_checkable_files: Set[Path]
    ) -> PyreBuckTypeErrorMetadata:
        result = PyreBuckTypeErrorMetadata(
            status=BuckTypeCheckStatus.NOT_RUN,
            duration=0,
            type_errors={},
        )

        if len(type_checkable_files) == 0:
            LOG.debug("No Buck type checkable files found")
            return result

        buck_query_timer = timer.Timer()
        for retries in range(PTT_PREEMPTED_RETRIES):
            if retries != 0:
                LOG.warning("Rerunning preempted Buck type check")
            result = await self._query_buck_for_type_errors(
                type_checkable_files, buck_query_timer, retries=retries
            )
            if result.status != BuckTypeCheckStatus.PREEMPTED_SHOULD_RETRY:
                return result

        LOG.warning(
            f"Giving up on type check after being preempted {PTT_PREEMPTED_RETRIES} times"
        )
        return result

    async def _query_pyre_daemon_type_errors(
        self, document_path: Path, type_checkable_files: Set[Path]
    ) -> PyreDaemonTypeErrors:
        if len(type_checkable_files) == 0:
            LOG.debug("No daemon type checkable files found")
            return PyreDaemonTypeErrors(
                type_errors={},
                error_message=None,
                duration=0,
                reason=PyreDaemonTypeCheckStatus.NO_FILES,
            )
        type_errors_timer = timer.Timer()
        # TODO(connernilsen): we should get rid of this
        await self.update_overlay_if_needed(document_path)
        result = await self.querier.get_type_errors(
            type_checkable_files,
        )
        error_message: Optional[str] = None
        type_errors: Dict[Path, List[error.Error]] = {}
        if isinstance(result, DaemonQueryFailure):
            error_message = result.error_message
        else:
            type_errors = result
            await self._publish_type_errors_for_files(type_errors, type_checkable_files)

        return PyreDaemonTypeErrors(
            type_errors=type_errors,
            error_message=error_message,
            duration=type_errors_timer.stop_in_millisecond(),
            reason=PyreDaemonTypeCheckStatus.OK,
        )

    async def _get_files_for_type_checker(
        self, open_documents: Set[Path]
    ) -> FilesForTypeChecker:
        buck_query_timer = timer.Timer()
        with tempfile.NamedTemporaryFile(mode="w") as argfile:
            print("--", file=argfile)
            print(
                *[
                    argument
                    for file in open_documents
                    for argument in ["--source", str(file)]
                ],
                sep="\n",
                file=argfile,
            )
            argfile.flush()
            query_parameters = [
                "buck2",
                f"--isolation-dir={PTT_ISOLATION_PREFIX}",
                "bxl",
                "--reuse-current-config",
                "--oncall=pyre",
                "--client-metadata=id=pyre.ide",
                "prelude//python/sourcedb/typing_query.bxl:typing_query",
                f"@{argfile.name}",
            ]
            LOG.debug(
                f"Getting files for type checker from Buck: `{' '.join(query_parameters)}`"
            )

            self.server_state.ongoing_type_checks += 1
            type_checked_files_query = await asyncio.create_subprocess_exec(
                *query_parameters,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
            )
            stdout_data, stderr_data = await type_checked_files_query.communicate()
            self.server_state.ongoing_type_checks -= 1

        if type_checked_files_query.returncode != 0:
            stderr = stderr_data.decode("utf-8")
            message = f"Typing query ended in error: {stderr}"
            LOG.error(message)
            return FilesForTypeChecker(
                buck_files=set(),
                daemon_files_use_buck=None,
                daemon_files=open_documents,
                error_message=message,
                duration=buck_query_timer.stop_in_millisecond(),
                did_run=True,
            )

        stdout_decoded = stdout_data.decode("utf-8")
        query_data = json.loads(stdout_decoded)
        buck_type_checkable_files: Set[Path] = {
            Path(file)
            for file, query_result in query_data.items()
            if (
                (
                    isinstance(query_result, dict)
                    and query_result.get("typing_enabled", False)
                )
                # TODO(connernilsen): remove, this is for API compatibility
                or query_result is True
            )
        }

        daemon_files_use_buck = any(
            isinstance(query_result, dict)
            and query_result.get("has_owning_targets", False)
            for query_result in query_data.values()
        )
        return FilesForTypeChecker(
            buck_files=buck_type_checkable_files,
            daemon_files_use_buck=daemon_files_use_buck,
            daemon_files=open_documents - buck_type_checkable_files,
            error_message=None,
            duration=buck_query_timer.stop_in_millisecond(),
            did_run=True,
        )

    async def handle_overlay_type_errors(
        self,
        document_path: Path,
        new_file_loaded: bool,
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        client_register_event = self.server_state.client_register_event
        if client_register_event is not None and not client_register_event.is_set():
            return
        daemon_status_before = self.server_state.status_tracker.get_status()
        type_errors_timer = timer.Timer()
        open_documents = set(self.server_state.opened_documents.keys())

        async def async_identity(value: T) -> T:
            return value

        type_checked_files = await self._get_files_for_type_checker(open_documents)

        if self.get_language_server_features().per_target_type_errors.is_enabled():
            unawaited_buck_type_errors = self._get_buck_type_errors(
                type_checked_files.buck_files
            )
        else:
            unawaited_buck_type_errors = async_identity(
                PyreBuckTypeErrorMetadata(
                    status=BuckTypeCheckStatus.NOT_RUN,
                    duration=0,
                    type_errors={},
                    error_message=None,
                )
            )

        if self.get_language_server_features().global_lazy_type_errors.is_enabled():
            unawaited_daemon_type_errors = self._query_pyre_daemon_type_errors(
                document_path, type_checked_files.daemon_files
            )
        else:
            unawaited_daemon_type_errors = async_identity(
                PyreDaemonTypeErrors(
                    type_errors={},
                    error_message=None,
                    duration=0,
                    reason=PyreDaemonTypeCheckStatus.NOT_ENABLED,
                )
            )

        daemon_type_errors, pyre_buck_metadata = await asyncio.gather(
            unawaited_daemon_type_errors, unawaited_buck_type_errors
        )

        error_message = (daemon_type_errors.error_message or "") + (
            pyre_buck_metadata.error_message or ""
        )

        await self.write_telemetry(
            {
                "type": "LSP",
                "operation": "typeErrors",
                "filePath": str(document_path),
                "server_state_open_documents_count": len(
                    self.server_state.opened_documents
                ),
                "duration_ms": type_errors_timer.stop_in_millisecond(),
                "error_message": error_message
                if len(error_message.strip()) != 0
                else None,
                "type_check_metadata": {
                    "buck_type_errors": pyre_buck_metadata.to_json(),
                    "daemon_type_errors": daemon_type_errors.to_json(),
                    "typing_query": type_checked_files.to_json(),
                    "telemetry_version": 4,
                    "new_file_loaded": new_file_loaded,
                },
                **daemon_status_before.as_telemetry_dict(),
            },
            activity_key,
        )

    async def process_did_change_request(
        self,
        parameters: lsp.DidChangeTextDocumentParameters,
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )
        document_path = document_path.resolve()
        if document_path not in self.server_state.opened_documents:
            return

        daemon_status_before = self.server_state.status_tracker.get_status()
        did_change_timer = timer.Timer()

        process_unsaved_changes = self.server_state.server_options.language_server_features.unsaved_changes.is_enabled()
        error_message = None
        code_changes = str(
            "".join(
                [content_change.text for content_change in parameters.content_changes]
            )
        )
        self.server_state.opened_documents[document_path] = OpenedDocumentState(
            code=code_changes,
            is_dirty=True,
            pyre_code_updated=False,
        )
        await self.write_telemetry(
            {
                "type": "LSP",
                "operation": "didChange",
                "filePath": str(document_path),
                "server_state_open_documents_count": len(
                    self.server_state.opened_documents
                ),
                "duration_ms": did_change_timer.stop_in_millisecond(),
                "error_message": error_message,
                "overlays_enabled": process_unsaved_changes,
                **daemon_status_before.as_telemetry_dict(),
            },
            activity_key,
        )
        if (
            process_unsaved_changes
            and self.get_language_server_features().type_errors.is_enabled()
            # TODO(T165048078): temporary change to prevent tons of didChange requests
            # from queueing up and severely slowing down daemon
            and self.server_state.status_tracker.get_status().connection_status
            == state.ConnectionStatus.READY
            # TODO(T165048078): hack to turn this off for codenav server, since it
            # will be incompatible with early per-target implementations
            and self.server_state.server_options.flavor
            != identifiers.PyreFlavor.CODE_NAVIGATION
        ):
            await self.handle_overlay_type_errors(
                document_path=document_path,
                new_file_loaded=False,
                activity_key=activity_key,
            )

    async def _run_python_auto_targets(
        self,
        changed_file: Path,
        removals_enabled: bool,
    ) -> PythonAutoTargetsMetadata:
        try:
            buck2_root = get_buck_root()
        except subprocess.CalledProcessError as error:
            message = f"Error occurred while querying buck root:\n\tCWD: {os.getcwd()}\n\t{str(error)}"
            LOG.error(message)
            return PythonAutoTargetsMetadata(
                duration=None,
                error_message=message,
            )

        pyautotargets_timer = timer.Timer()
        system_pyautotarget = (
            Path(r"C:\ProgramData\chocolatey\bin\pyautotargets.exe")
            if os.name == "nt"
            else Path("/usr/local/bin/pyautotargets")
        )
        pyautotargets_bin = (
            system_pyautotarget
            if (
                self.get_language_server_features().use_system_pyautotargets.is_enabled()
                and system_pyautotarget.exists()
            )
            else Path(f"{buck2_root}/xplat/tools/pyautotargets")
        )
        pyautodeps_parameters = [str(pyautotargets_bin)]
        if removals_enabled:
            pyautodeps_parameters.append("--remove-deps")
        pyautodeps_parameters.extend(
            [
                "--verbose",
                "--buck-root",
                buck2_root,
                "--isolation-dir=.pyautotargets",
                str(changed_file),
            ]
        )
        LOG.debug(f"Running Pyautotargets: `{' '.join(pyautodeps_parameters)}`")
        pyautodeps_run = await asyncio.create_subprocess_exec(
            *pyautodeps_parameters,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
            cwd=buck2_root,
        )
        stdout_data, stderr_data = await pyautodeps_run.communicate()

        if pyautodeps_run.returncode != 0:
            stderr = stderr_data.decode("utf-8")
            message = f"Pyautodeps run failed:\n{stderr}"
            LOG.error(message)
            return PythonAutoTargetsMetadata(duration=None, error_message=message)

        stdout = stdout_data.decode("utf-8")
        LOG.debug(f"Pyautodeps result:\n{stdout}")
        return PythonAutoTargetsMetadata(
            duration=pyautotargets_timer.stop_in_millisecond(),
            error_message=None,
        )

    async def process_did_save_request(
        self,
        parameters: lsp.DidSaveTextDocumentParameters,
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )
        document_path = document_path.resolve()

        if document_path not in self.server_state.opened_documents:
            return

        daemon_status_before = self.server_state.status_tracker.get_status()

        code_changes = self.server_state.opened_documents[document_path].code

        self.server_state.opened_documents[document_path] = OpenedDocumentState(
            code=code_changes,
            is_dirty=False,
            # False here because even though a didSave event means the base environment
            # will be up-to-date (after an incremental push), it is not necessarily
            # the case that the overlay environment is up to date.
            pyre_code_updated=False,
        )

        if (
            self.get_language_server_features().python_auto_targets.is_enabled()
            and document_path.suffix in PYAUTOTARGETS_ENABLED_SUFFIXES
        ):
            async with self.server_state.pyautotargets_lock:
                auto_targets_metadata = await self._run_python_auto_targets(
                    document_path,
                    self.get_language_server_features().python_auto_targets_removal.is_enabled(),
                )
        else:
            auto_targets_metadata = PythonAutoTargetsMetadata(
                duration=None, error_message=None
            )
        await self.write_telemetry(
            {
                "type": "LSP",
                "operation": "didSave",
                "filePath": str(document_path),
                "server_state_open_documents_count": len(
                    self.server_state.opened_documents
                ),
                # We don't do any blocking work on didSave, but analytics are easier if
                # we avoid needlessly introducing NULL values.
                "duration_ms": 0,
                "python_auto_targets_metadata": {
                    "duration": auto_targets_metadata.duration,
                    "error_message": auto_targets_metadata.error_message,
                },
                **daemon_status_before.as_telemetry_dict(),
            },
            activity_key,
        )

        if (
            self.get_language_server_features().type_errors.is_enabled()
            # TODO (T165048078): hack to get this working only for codenav server
            and self.server_state.server_options.flavor
            == identifiers.PyreFlavor.CODE_NAVIGATION
        ):
            await self.handle_overlay_type_errors(
                document_path=document_path,
                new_file_loaded=False,
                activity_key=activity_key,
            )

    async def process_type_coverage_request(
        self,
        parameters: lsp.TypeCoverageParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )
        document_path = document_path.resolve()

        daemon_status_before = self.server_state.status_tracker.get_status()
        type_coverage_timer = timer.Timer()

        response = await self.querier.get_type_coverage(path=document_path)
        if response is not None:
            await lsp.write_json_rpc(
                self.output_channel,
                json_rpc.SuccessResponse(
                    id=request_id,
                    activity_key=activity_key,
                    result=response.to_dict(),
                ),
            )
        await self.write_telemetry(
            {
                "type": "LSP",
                "operation": "typeCoverage",
                "filePath": str(document_path),
                "duration_ms": type_coverage_timer.stop_in_millisecond(),
                "coverage_type": self.get_language_server_features().type_coverage.value,
                **daemon_status_before.as_telemetry_dict(),
            },
            activity_key,
        )

    async def process_hover_request(
        self,
        parameters: lsp.HoverParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        """Always respond to a hover request even for non-tracked paths.

        Otherwise, VS Code hover will wait for Pyre until it times out, meaning
        that messages from other hover providers will be delayed."""

        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )
        document_path = document_path.resolve()

        if document_path not in self.server_state.opened_documents:
            return await lsp.write_json_rpc(
                self.output_channel,
                json_rpc.SuccessResponse(
                    id=request_id,
                    activity_key=activity_key,
                    result=None,
                ),
            )
        daemon_status_before = self.server_state.status_tracker.get_status()
        hover_timer = timer.Timer()

        await self.update_overlay_if_needed(document_path)
        # TODO Bring out the state logic and use regular querier
        result = await self.index_querier.get_hover(
            path=document_path,
            position=parameters.position.to_pyre_position(),
        )
        error_message = None
        if isinstance(result, DaemonQueryFailure):
            LOG.info(
                daemon_failure_string("hover", str(type(result)), result.error_message)
            )
            error_message = result.error_message
            raw_result = None
            empty = True
        else:
            empty = result.data is None
            raw_result = (
                None
                if empty
                else lsp.LspHoverResponse.cached_schema().dump(
                    result.data,
                )
            )

        await lsp.write_json_rpc(
            self.output_channel,
            json_rpc.SuccessResponse(
                id=request_id,
                activity_key=activity_key,
                result=raw_result,
            ),
        )
        await self.write_telemetry(
            {
                "type": "LSP",
                "operation": "hover",
                "filePath": str(document_path),
                "nonEmpty": not empty,
                "response": raw_result,
                "duration_ms": hover_timer.stop_in_millisecond(),
                "query_source": (
                    result.source
                    if not isinstance(result, DaemonQueryFailure)
                    else None
                ),
                "server_state_open_documents_count": len(
                    self.server_state.opened_documents
                ),
                "error_message": error_message,
                "overlays_enabled": self.server_state.server_options.language_server_features.unsaved_changes.is_enabled(),
                "position": parameters.position.to_dict(),
                **daemon_status_before.as_telemetry_dict(),
            },
            activity_key,
        )

    async def _get_definition_result_from_daemon_querier(
        self,
        document_path: Path,
        position: lsp.LspPosition,
    ) -> QueryResultWithDurations[GetDefinitionLocationsResponse]:
        """
        Helper function to call the querier. Exists only to reduce code duplication
        due to shadow mode, please don't make more of these - we already have enough
        layers of handling.
        """
        overall_timer = timer.Timer()
        overlay_update_duration = await self.update_overlay_if_needed(document_path)
        query_timer = timer.Timer()
        # TODO Bring out the state logic and use regular querier
        result = await self.index_querier.get_definition_locations(
            path=document_path,
            position=position.to_pyre_position(),
        )
        query_duration = query_timer.stop_in_millisecond()
        if isinstance(result, DaemonQueryFailure):
            LOG.info(
                "%s",
                daemon_failure_string(
                    "definition", str(type(result)), result.error_message
                ),
            )
        return QueryResultWithDurations(
            result=result,
            overlay_update_duration=overlay_update_duration,
            query_duration=query_duration,
            overall_duration=overall_timer.stop_in_millisecond(),
        )

    @log_exceptions_factory("definition")
    async def process_definition_request(
        self,
        parameters: lsp.DefinitionParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
        dispatch_request_duration: Optional[float] = None,
    ) -> Optional[Exception]:
        process_request_timer = timer.Timer()
        document_path: Optional[Path] = (
            parameters.text_document.document_uri().to_file_path()
        )
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )
        document_path = document_path.resolve()
        if document_path not in self.server_state.opened_documents:
            await lsp.write_json_rpc(
                self.output_channel,
                json_rpc.SuccessResponse(
                    id=request_id,
                    activity_key=activity_key,
                    result=lsp.LspLocation.cached_schema().dump([], many=True),
                ),
            )
            await self.write_telemetry(
                {
                    "type": "LSP",
                    "operation": "definition",
                    "filePath": str(document_path),
                    "server_state_open_documents_count": len(
                        self.server_state.opened_documents
                    ),
                    "overlays_enabled": self.server_state.server_options.language_server_features.unsaved_changes.is_enabled(),
                    "using_errpy_parser": self.server_state.server_options.using_errpy_parser,
                    "error_type": PyreLanguageServerError.DOCUMENT_PATH_MISSING_IN_SERVER_STATE,
                    "dispatch_request_duration": dispatch_request_duration,
                    "process_request_duration": process_request_timer.stop_in_millisecond(),
                    "overlay_update_duration": 0.0,
                    "query_duration": 0.0,
                    "marshalling_response_duration": 0.0,
                    "write_response_duration": 0.0,
                    "sample_source_code_duration": 0.0,
                },
                activity_key,
            )
            return None

        shadow_mode = self.get_language_server_features().definition.is_shadow()

        # In shadow mode, we need to return an empty response immediately
        if shadow_mode:
            return await self._process_definitions_for_shadow_mode(
                document_path,
                parameters,
                request_id,
                activity_key,
            )
        else:
            return await self._process_definitions_for_codenav(
                document_path,
                parameters,
                request_id,
                activity_key,
                process_request_timer,
                dispatch_request_duration,
            )

    async def _process_definitions_for_codenav(
        self,
        document_path: Path,
        parameters: lsp.DefinitionParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]],
        process_request_timer: timer.Timer,
        dispatch_request_duration: Optional[float],
    ) -> Optional[Exception]:
        daemon_status_before = self.server_state.status_tracker.get_status()
        process_request_duration = process_request_timer.stop_in_millisecond()
        result_with_durations = await self._get_definition_result_from_daemon_querier(
            document_path=document_path,
            position=parameters.position,
        )
        marshalling_response_timer = timer.Timer()
        output_result_json = self._get_output_result_json_from_result_with_durations(
            result_with_durations
        )
        marshalling_response_duration = marshalling_response_timer.stop_in_millisecond()

        # send response as output
        write_response_timer = timer.Timer()
        await lsp.write_json_rpc(
            self.output_channel,
            json_rpc.SuccessResponse(
                id=request_id,
                activity_key=activity_key,
                result=output_result_json,
            ),
        )
        write_response_duration = write_response_timer.stop_in_millisecond()

        await self._log_telemetry_for_definitions(
            document_path,
            parameters,
            activity_key,
            daemon_status_before,
            result_with_durations,
            DefinitionTimingData(
                dispatch_request_duration=dispatch_request_duration,
                process_request_duration=process_request_duration,
                marshalling_response_duration=marshalling_response_duration,
                write_response_duration=write_response_duration,
            ),
        )

        return self._get_error_source_from_result_with_duration(result_with_durations)

    async def _process_definitions_for_shadow_mode(
        self,
        document_path: Path,
        parameters: lsp.DefinitionParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]],
    ) -> Optional[Exception]:
        # In shadow mode, we need to return an empty response immediately
        await lsp.write_json_rpc(
            self.output_channel,
            json_rpc.SuccessResponse(
                id=request_id,
                activity_key=activity_key,
                result=lsp.LspLocation.cached_schema().dump([], many=True),
            ),
        )

        daemon_status_before = self.server_state.status_tracker.get_status()
        result_with_durations = await self._get_definition_result_from_daemon_querier(
            document_path=document_path,
            position=parameters.position,
        )

        await self._log_telemetry_for_definitions(
            document_path,
            parameters,
            activity_key,
            daemon_status_before,
            result_with_durations,
            None,
        )

        return self._get_error_source_from_result_with_duration(result_with_durations)

    def _get_error_source_from_result_with_duration(
        self,
        result_with_durations: QueryResultWithDurations[GetDefinitionLocationsResponse],
    ) -> Optional[Exception]:
        result = result_with_durations.result
        return result.error_source if isinstance(result, DaemonQueryFailure) else None

    def _get_output_result_json_from_result_with_durations(
        self,
        result_with_durations: QueryResultWithDurations[GetDefinitionLocationsResponse],
    ) -> List[Dict[str, object]]:
        result = result_with_durations.result
        if isinstance(result, DaemonQueryFailure):
            output_result = (
                result.fallback_result.data
                if result.fallback_result is not None
                else []
            )
        else:
            output_result = result.data

        return lsp.LspLocation.cached_schema().dump(output_result, many=True)

    async def _log_telemetry_for_definitions(
        self,
        document_path: Path,
        parameters: lsp.DefinitionParameters,
        activity_key: Optional[Dict[str, object]],
        daemon_status_before: state.DaemonStatus,
        result_with_durations: QueryResultWithDurations[GetDefinitionLocationsResponse],
        definition_timing_data: Optional[DefinitionTimingData],
    ) -> None:
        output_result_json = self._get_output_result_json_from_result_with_durations(
            result_with_durations
        )
        is_empty_result = len(output_result_json) == 0

        # Only sample if response is empty
        source_code_if_sampled = (
            self.sample_source_code(
                document_path,
                parameters.position,
            )
            if is_empty_result
            else None
        )

        result = result_with_durations.result
        if isinstance(result, DaemonQueryFailure):
            error_message = result.error_message
            daemon_duration = result.duration
            daemon_inner_duration = 0
            if result.fallback_result is None:
                query_source = None
                empty_reason = None
                glean_duration = 0
            else:
                query_source = result.fallback_result.source
                empty_reason = result.fallback_result.empty_reason
                glean_duration = result.fallback_result.glean_duration
        else:
            error_message = None
            query_source = result.source
            empty_reason = result.empty_reason
            daemon_duration = result.daemon_duration
            daemon_inner_duration = result.daemon_inner_duration
            glean_duration = result.glean_duration

        error_type_to_log = get_language_server_error(error_message, query_source)
        empty_reason_to_log = (
            get_language_server_empty_reason(empty_reason, error_message, query_source)
            if is_empty_result
            else None
        )
        sample_source_code_timer = timer.Timer()
        character_at_position = SourceCodeContext.character_at_position(
            self.server_state.opened_documents[document_path].code, parameters.position
        )
        sample_source_code_duration = sample_source_code_timer.stop_in_millisecond()

        if definition_timing_data is None:
            definition_timing_data = DefinitionTimingData(
                dispatch_request_duration=0.0,
                process_request_duration=0.0,
                marshalling_response_duration=0.0,
                write_response_duration=0.0,
            )

        await self.write_telemetry(
            {
                "type": "LSP",
                "operation": "definition",
                "filePath": str(document_path),
                "count": len(output_result_json),
                "response": output_result_json,
                "query_source": query_source,
                "duration_ms": result_with_durations.overall_duration,
                "dispatch_request_duration": (
                    definition_timing_data.dispatch_request_duration
                ),
                "process_request_duration": (
                    definition_timing_data.process_request_duration
                ),
                "overlay_update_duration": result_with_durations.overlay_update_duration,
                "query_duration": result_with_durations.query_duration,
                "query_daemon_duration": daemon_duration,
                "query_daemon_inner_duration": daemon_inner_duration,
                "query_glean_duration": glean_duration,
                "marshalling_response_duration": (
                    definition_timing_data.marshalling_response_duration
                ),
                "write_response_duration": (
                    definition_timing_data.write_response_duration
                ),
                "sample_source_code_duration": sample_source_code_duration,
                "server_state_open_documents_count": len(
                    self.server_state.opened_documents
                ),
                "overlays_enabled": self.server_state.server_options.language_server_features.unsaved_changes.is_enabled(),
                "error_message": error_message,
                "is_dirty": self.server_state.opened_documents[document_path].is_dirty,
                "truncated_file_contents": source_code_if_sampled,
                "position": parameters.position.to_dict(),
                "using_errpy_parser": self.server_state.server_options.using_errpy_parser,
                "character_at_position": character_at_position,
                **daemon_status_before.as_telemetry_dict(),
                "error_type": error_type_to_log,
                "empty_reason": empty_reason_to_log,
            },
            activity_key,
        )

    async def process_symbol_search_request(
        self,
        parameters: lsp.WorkspaceSymbolParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        completion_timer = timer.Timer()
        query = parameters.query
        symbol_search_response = await self.index_querier.get_symbol_search(query)
        len_res = 0

        if isinstance(symbol_search_response, DaemonQueryFailure):
            error_message = symbol_search_response.error_message
            raw_results = (
                symbol_search_response.fallback_result.data
                if symbol_search_response.fallback_result is not None
                else []
            )
            error_source = symbol_search_response.error_source
        else:
            error_message = None
            error_source = None
            raw_results = []

            if symbol_search_response.data is not None:
                len_res = len(symbol_search_response.data.workspace_symbols)
                raw_results = lsp.WorkspaceSymbolResponse.cached_schema().dump(
                    symbol_search_response.data
                )["workspaceSymbols"]

        (
            await lsp.write_json_rpc(
                self.output_channel,
                json_rpc.SuccessResponse(
                    id=request_id,
                    activity_key=activity_key,
                    result=raw_results,
                ),
            ),
        )

        await self.write_telemetry(
            {
                "type": "LSP",
                "operation": "symbol_search",
                "query": (
                    query
                    if not isinstance(symbol_search_response, DaemonQueryFailure)
                    else None
                ),
                "response": raw_results,
                "response length": len_res,
                "duration_ms": completion_timer.stop_in_millisecond(),
                "error_message": error_message,
                "error_source": error_source,
            },
            activity_key,
        )

    async def process_document_formatting_request(
        self,
        parameters: lsp.DocumentFormattingParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        document_formatter_timer = 0

        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )
        if document_path not in self.server_state.opened_documents:
            raise json_rpc.InvalidRequestError(
                f"Document URI has not been opened: {parameters.text_document.uri}"
            )

        try:
            document_formatter = self.document_formatter
            if document_formatter is None:
                raise json_rpc.InternalError("Formatter was not initialized correctly.")
            else:
                document_formatter_timer = timer.Timer()
                document_formatting_response = await document_formatter.format_document(
                    document_path
                )

                raw_results = lsp.DocumentFormattingResponse.cached_schema().dump(
                    document_formatting_response
                )["listOfEdits"]

                (
                    await lsp.write_json_rpc(
                        self.output_channel,
                        json_rpc.SuccessResponse(
                            id=request_id,
                            activity_key=activity_key,
                            result=raw_results,
                        ),
                    ),
                )

            await self.write_telemetry(
                {
                    "type": "LSP",
                    "operation": "document_formatting",
                    "duration_ms": document_formatter_timer.stop_in_millisecond(),
                },
                activity_key,
            )

        except OSError as error:
            raise lsp.RequestFailedError(
                f"Document URI is not a readable file: {parameters.text_document.uri}"
            ) from error

    async def process_completion_request(
        self,
        parameters: lsp.CompletionParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> Optional[Exception]:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )
        document_path = document_path.resolve()

        if document_path not in self.server_state.opened_documents:
            await lsp.write_json_rpc(
                self.output_channel,
                json_rpc.SuccessResponse(
                    id=request_id,
                    activity_key=activity_key,
                    result=[],
                ),
            )
            return None
        daemon_status_before = self.server_state.status_tracker.get_status()
        completion_timer = timer.Timer()

        await self.update_overlay_if_needed(document_path)
        result = await self.querier.get_completions(
            path=document_path,
            position=parameters.position.to_pyre_position(),
        )

        error_message = None
        if isinstance(result, DaemonQueryFailure):
            LOG.info(
                daemon_failure_string(
                    "completion", str(type(result)), result.error_message
                )
            )
            error_message = result.error_message
            error_source = result.error_source
            result = []
        else:
            error_source = None

        raw_result = [completion_item.to_dict() for completion_item in result]

        LOG.debug(f"raw_result: {raw_result}")

        await lsp.write_json_rpc(
            self.output_channel,
            json_rpc.SuccessResponse(
                id=request_id,
                activity_key=activity_key,
                result=raw_result,
            ),
        )
        await self.write_telemetry(
            {
                "type": "LSP",
                "operation": "completion",
                "filePath": str(document_path),
                "nonEmpty": len(result) > 0,
                "response": raw_result,
                "duration_ms": completion_timer.stop_in_millisecond(),
                "server_state_open_documents_count": len(
                    self.server_state.opened_documents
                ),
                "error_message": error_message,
                "position": parameters.position.to_dict(),
                **daemon_status_before.as_telemetry_dict(),
            },
            activity_key,
        )

        return error_source

    async def process_document_symbols_request(
        self,
        parameters: lsp.DocumentSymbolsParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        request_timer = timer.Timer()
        algorithm_duration = None
        error = None
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )
        if document_path not in self.server_state.opened_documents:
            raise json_rpc.InvalidRequestError(
                f"Document URI has not been opened: {parameters.text_document.uri}"
            )
        try:
            algorithm_timer = timer.Timer()
            source = document_path.read_text()
            symbols = find_symbols.parse_source_and_collect_symbols(source)
            algorithm_duration = algorithm_timer.stop_in_millisecond()

            await lsp.write_json_rpc(
                self.output_channel,
                json_rpc.SuccessResponse(
                    id=request_id,
                    activity_key=activity_key,
                    result=[symbol.to_dict() for symbol in symbols],
                ),
            )
        except find_symbols.UnparseableError as error:
            raise lsp.RequestFailedError(
                f"Document URI is not parsable: {parameters.text_document.uri}"
            ) from error
        except OSError as error:
            raise lsp.RequestFailedError(
                f"Document URI is not a readable file: {parameters.text_document.uri}"
            ) from error

        await self.write_telemetry(
            {
                "type": "LSP",
                "operation": "document_symbols",
                "filepath": str(document_path),
                "duration_ms": request_timer.stop_in_millisecond(),
                "duration_document_symbol_ms": algorithm_duration,
                "server_state_open_documents_count": len(
                    self.server_state.opened_documents
                ),
                "error": str(error),
            },
            activity_key,
        )

    async def process_find_all_references_request(
        self,
        parameters: lsp.ReferencesParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )

        if document_path not in self.server_state.opened_documents:
            await lsp.write_json_rpc(
                self.output_channel,
                json_rpc.SuccessResponse(
                    id=request_id,
                    activity_key=activity_key,
                    result=lsp.LspLocation.cached_schema().dump([], many=True),
                ),
            )
            return

        reference_locations = await self.index_querier.get_reference_locations(
            path=document_path,
            position=parameters.position.to_pyre_position(),
        )
        await lsp.write_json_rpc(
            self.output_channel,
            json_rpc.SuccessResponse(
                id=request_id,
                activity_key=activity_key,
                result=lsp.LspLocation.cached_schema().dump(
                    reference_locations,
                    many=True,
                ),
            ),
        )

    async def process_call_hierarchy_request(
        self,
        parameters: lsp.CallHierarchyParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        LOG.info(f"Processing call hierarchy request for {parameters}")
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )
        daemon_status_before = self.server_state.status_tracker.get_status()
        request_timer = timer.Timer()

        call_hierarchy_response = await self.index_querier.get_init_call_hierarchy(
            path=document_path,
            position=parameters.position.to_pyre_position(),
            relation_direction=lsp.PyreCallHierarchyRelationDirection.PARENT,
        )

        error_message = None
        if isinstance(call_hierarchy_response, DaemonQueryFailure):
            LOG.info(
                daemon_failure_string(
                    "call_hierarchy",
                    str(type(call_hierarchy_response)),
                    call_hierarchy_response.error_message,
                )
            )
            error_message = call_hierarchy_response.error_message
            call_hierarchy_response = []

        if error_message:
            LOG.error(
                f"Error at `process_call_hierarchy_request` message: {error_message}"
            )

        raw_result = [c.to_dict() for c in call_hierarchy_response]

        await lsp.write_json_rpc(
            self.output_channel,
            json_rpc.SuccessResponse(
                id=request_id,
                activity_key=activity_key,
                result=raw_result,
            ),
        )
        await self.write_telemetry(
            {
                "type": "LSP",
                "operation": "prepare_call_hierarchy",
                "filepath": str(document_path),
                "non_empty": len(call_hierarchy_response) > 0,
                "response": raw_result,
                "duration_ms": request_timer.stop_in_millisecond(),
                "server_state_open_documents_count": len(
                    self.server_state.opened_documents
                ),
                "error_message": error_message,
                "position": parameters.position.to_dict(),
                **daemon_status_before.as_telemetry_dict(),
            },
            activity_key,
        )

    async def process_call_hierarchy_incoming_call(
        self,
        parameters: lsp.CallHierarchyIncomingCallParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        LOG.info(f"Processing incoming call hierarchy call for {parameters}")

        document_path = parameters.item.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.item.document_uri()}"
            )
        daemon_status_before = self.server_state.status_tracker.get_status()
        request_timer = timer.Timer()

        call_hierarchy_items = await self.index_querier.get_call_hierarchy_from_item(
            path=document_path,
            call_hierarchy_item=parameters.item,
            relation_direction=lsp.PyreCallHierarchyRelationDirection.PARENT,
        )

        error_message = None
        if isinstance(call_hierarchy_items, DaemonQueryFailure):
            LOG.info(
                daemon_failure_string(
                    "call_hierarchy, incoming call",
                    str(type(call_hierarchy_items)),
                    call_hierarchy_items.error_message,
                )
            )
            error_message = call_hierarchy_items.error_message
            call_hierarchy_items = []

        if error_message:
            LOG.error(
                f"Error at `process_call_hierarchy_incoming_call` message: {error_message}"
            )

        raw_result = [
            lsp.CallHierarchyIncomingCall.cached_schema().dump(
                lsp.CallHierarchyIncomingCall(
                    from_=call_hierarchy_item, from_ranges=[call_hierarchy_item.range]
                )
            )
            for call_hierarchy_item in call_hierarchy_items
        ]
        LOG.info(f"Call hierarchy incoming call response: {raw_result}")

        await lsp.write_json_rpc(
            self.output_channel,
            json_rpc.SuccessResponse(
                id=request_id,
                activity_key=activity_key,
                result=raw_result,
            ),
        )
        await self.write_telemetry(
            {
                "type": "LSP",
                "operation": "call_hierarchy_incoming_call",
                "filepath": str(document_path),
                "non_empty": len(raw_result) > 0,
                "response": raw_result,
                "duration_ms": request_timer.stop_in_millisecond(),
                "server_state_open_documents_count": len(
                    self.server_state.opened_documents
                ),
                "error_message": error_message,
                "call_hierarchy_items": parameters.item.to_dict(),
                **daemon_status_before.as_telemetry_dict(),
            },
            activity_key,
        )

    async def process_call_hierarchy_outgoing_call(
        self,
        parameters: lsp.CallHierarchyOutgoingCallParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        LOG.info(f"Processing outgoing call hierarchy call for {parameters}")

        document_path = parameters.item.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.item.document_uri()}"
            )
        daemon_status_before = self.server_state.status_tracker.get_status()
        request_timer = timer.Timer()

        call_hierarchy_items = await self.index_querier.get_call_hierarchy_from_item(
            path=document_path,
            call_hierarchy_item=parameters.item,
            relation_direction=lsp.PyreCallHierarchyRelationDirection.CHILD,
        )

        error_message = None
        if isinstance(call_hierarchy_items, DaemonQueryFailure):
            LOG.info(
                daemon_failure_string(
                    "call_hierarchy, outgoing call",
                    str(type(call_hierarchy_items)),
                    call_hierarchy_items.error_message,
                )
            )
            error_message = call_hierarchy_items.error_message
            call_hierarchy_items = []

        if error_message:
            LOG.error(
                f"Error at `process_call_hierarchy_outgoing_call` message: {error_message}"
            )

        raw_result = [
            lsp.CallHierarchyOutgoingCall(
                to=call_hierarchy_item, from_ranges=[call_hierarchy_item.range]
            ).to_dict()
            for call_hierarchy_item in call_hierarchy_items
        ]
        LOG.info(f"Call hierarchy outgoing call response: {raw_result}")

        await lsp.write_json_rpc(
            self.output_channel,
            json_rpc.SuccessResponse(
                id=request_id,
                activity_key=activity_key,
                result=raw_result,
            ),
        )
        await self.write_telemetry(
            {
                "type": "LSP",
                "operation": "call_hierarchy_outgoing_call",
                "filepath": str(document_path),
                "non_empty": len(raw_result) > 0,
                "response": raw_result,
                "duration_ms": request_timer.stop_in_millisecond(),
                "server_state_open_documents_count": len(
                    self.server_state.opened_documents
                ),
                "error_message": error_message,
                "call_hierarchy_items": parameters.item.to_dict(),
                **daemon_status_before.as_telemetry_dict(),
            },
            activity_key,
        )

    def filter_references(
        self,
        document_path: Path,
        position: lsp.PyrePosition,
        references: List[lsp.LspLocation],
    ) -> List[lsp.LspLocation]:
        """
        Removes extra/erroneous references by:
        1. Deduping references with identical LspRange
            a. References from an index will overlap with references locally calculated.
        2. Filtering out references that don't point to the same symbol
            a. References from an index can be stale and mispoint.
        """
        # Why `dict.fromkeys`? This is a trick to get ordered-set behavior in Python 3.7+
        deduped_references = list(dict.fromkeys(references))
        code_text = self.server_state.opened_documents[document_path].code
        global_root = (
            self.server_state.server_options.start_arguments.base_arguments.global_root
        )
        symbol_range = libcst_util.find_symbol_range(
            document_path,
            Path(global_root),
            code_text,
            position,
        )
        LOG.debug(f"symbol_range: {symbol_range}")
        text_at_range = SourceCodeContext.text_at_range(code_text, symbol_range)
        LOG.debug(f"text_at_range: {text_at_range}")
        filtered_references = []
        for reference in deduped_references:
            destination_filepath = Path(lsp.DocumentUri.parse(reference.uri).path)
            if destination_filepath in self.server_state.opened_documents:
                code_text = self.server_state.opened_documents[
                    destination_filepath
                ].code
            else:
                # Annoying workaround to handle files that are not opened yet.
                # Daemon fetches it by reading, whereas the LS reads in code from
                # didChange/didOpen requests. For files we didn't receive requests
                # for, we need to open the file and read it ourselves.
                # @lint-ignore FIXIT1
                with open(destination_filepath, "r") as f:
                    code_text = f.read()
            text_to_replace = SourceCodeContext.text_at_range(
                code_text, reference.range
            )
            if text_to_replace == text_at_range:
                filtered_references.append(reference)
            else:
                LOG.info(
                    f"Filtering out reference {reference} because it doesn't match text at symbol range: {text_to_replace}"
                )
        return filtered_references

    async def process_rename_request(
        self,
        parameters: lsp.RenameParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        document_path: Optional[Path] = (
            parameters.text_document.document_uri().to_file_path()
        )
        daemon_status_before = self.server_state.status_tracker.get_status()
        request_timer = timer.Timer()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )

        references: List[lsp.LspLocation] = []
        references_responses = await asyncio.gather(
            self.querier.get_reference_locations(
                document_path, parameters.position.to_pyre_position()
            ),
            self.index_querier.get_reference_locations(
                document_path, parameters.position.to_pyre_position()
            ),
        )

        local_references = references_responses[0]
        local_references_error_message = None
        local_references_count = 0
        if isinstance(local_references, DaemonQueryFailure):
            LOG.info(
                daemon_failure_string(
                    "querier.get_reference_locations",
                    str(type(local_references)),
                    local_references.error_message,
                )
            )
            local_references_error_message = local_references.error_message
        else:
            LOG.info(f"Local references: {local_references}")
            references.extend(local_references)
            local_references_count = len(local_references)

        global_references = references_responses[1]
        global_references_error_message = None
        global_references_count = 0
        if isinstance(global_references, DaemonQueryFailure):
            LOG.info(
                daemon_failure_string(
                    "index_querier.get_reference_locations",
                    str(type(global_references)),
                    global_references.error_message,
                )
            )
            global_references_error_message = global_references.error_message
        else:
            LOG.info(f"Global references: {global_references}")
            references.extend(global_references)
            global_references_count = len(global_references)

        deduped_references = self.filter_references(
            document_path, parameters.position.to_pyre_position(), references
        )

        changes: DefaultDict[str, List[lsp.TextEdit]] = defaultdict(list)
        for reference in deduped_references:
            changes[reference.uri].append(
                lsp.TextEdit(reference.range, parameters.new_name)
            )

        rename_edits = lsp.WorkspaceEdit(changes=changes)

        raw_response = lsp.WorkspaceEdit.cached_schema().dump(rename_edits)
        LOG.info(f"Rename response: {raw_response}")

        await lsp.write_json_rpc(
            self.output_channel,
            json_rpc.SuccessResponse(
                id=request_id, activity_key=activity_key, result=raw_response
            ),
        )
        await self.write_telemetry(
            {
                "type": "LSP",
                "operation": "rename",
                "filepath": str(document_path),
                "non_empty": rename_edits is not None,
                "local_references_count": local_references_count,
                "global_references_count": global_references_count,
                "response": raw_response,
                "duration_ms": request_timer.stop_in_millisecond(),
                "server_state_open_documents_count": len(
                    self.server_state.opened_documents
                ),
                "error_message": f"local:{local_references_error_message}, global:{global_references_error_message}",
                **daemon_status_before.as_telemetry_dict(),
            },
            activity_key,
        )

    async def process_shutdown_request(self, request_id: Union[int, str, None]) -> None:
        await lsp.write_json_rpc_ignore_connection_error(
            self.output_channel,
            json_rpc.SuccessResponse(id=request_id, activity_key=None, result=None),
        )


@dataclasses.dataclass(frozen=True)
class CodeNavigationServerApi(PyreLanguageServer):
    pass


class PyreLanguageServerDispatcher:
    """
    The dispatcher provides the top-level, "foreground" logic for a Pyre
    language server. Its only job is to read requests from standard input,
    parse them, and dispatch to the appropriate lower-level logic.

    There are two compontents to which we might dispatch:
    - We'll dispatch to the PyreLanguageServer for all request handling,
      which includes querying the daemon, sending responses to the client,
      and reporting telemetry.
    - We also may check that the background task used to start/restart the
      daemon and get type error notifications over subscriptions is alive.
      The daemon can go down, for example if a critical file change occurs,
      so it is important for us to periodically check whether it is up.
    """

    # I/O channels. Output channel is used *exclusively* to report parse errors.
    input_channel: connections.AsyncTextReader
    output_channel: connections.AsyncTextWriter

    # State: used *exclusively* to track restart failures.
    server_state: state.ServerState

    daemon_manager: background_tasks.TaskManager
    api: PyreLanguageServerApi

    # A set of outstanding (not "done") asyncio tasks (like requests being processed). This is necessary to retain strong references to those tasks
    # to avoid them being collected mid-execution by gc. See https://docs.python.org/3/library/asyncio-task.html#asyncio.create_task
    outstanding_tasks: Set[asyncio.Task[None]]

    def __init__(
        self,
        input_channel: connections.AsyncTextReader,
        output_channel: connections.AsyncTextWriter,
        server_state: state.ServerState,
        daemon_manager: background_tasks.TaskManager,
        api: PyreLanguageServerApi,
    ) -> None:
        self.input_channel = input_channel
        self.output_channel = output_channel
        self.server_state = server_state
        self.daemon_manager = daemon_manager
        self.api = api
        self.outstanding_tasks = set()

    async def wait_for_exit(self) -> commands.ExitCode:
        await _wait_for_exit(self.input_channel, self.output_channel)
        return commands.ExitCode.SUCCESS

    async def _restart_if_needed(
        self, error_source: Optional[Exception] = None
    ) -> None:
        if (
            self.server_state.consecutive_start_failure
            >= CONSECUTIVE_START_ATTEMPT_THRESHOLD
        ):
            LOG.info(
                "Not restarting Pyre since failed consecutive start attempt limit"
                " has been reached."
            )
            return

        if isinstance(
            error_source,
            (
                connections.ConnectionFailure,
                asyncio.IncompleteReadError,
                ConnectionError,
            ),
        ):  # do we think the daemon is probably down at this point?
            # Terminate any existing daemon processes before starting a new one
            LOG.info("Forcing pyre daemon restart...")
            await self.daemon_manager.ensure_task_stop()  # make sure it's down

        # restart if needed
        if not self.daemon_manager.is_task_running():
            # Just check to ensure that the daemon is running and restart if needed
            await self.daemon_manager.ensure_task_running()

    async def dispatch_nonblocking_request(self, request: json_rpc.Request) -> None:
        if request.method == "exit" or request.method == "shutdown":
            raise Exception("Exit and shutdown requests should be blocking")

        dispatch_request_timer = timer.Timer()
        await self._restart_if_needed()
        dispatch_request_duration = dispatch_request_timer.stop_in_millisecond()
        if self.server_state.client_register_event is not None:
            await self.server_state.client_register_event.wait()
        if request.method == "textDocument/definition":
            error_source = await self.api.process_definition_request(
                lsp.DefinitionParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.id,
                request.activity_key,
                dispatch_request_duration,
            )
            await self._restart_if_needed(error_source)
        elif request.method == "textDocument/completion":
            LOG.debug("Received 'textDocument/completion' request.")
            error_source = await self.api.process_completion_request(
                lsp.CompletionParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.id,
                request.activity_key,
            )
            await self._restart_if_needed(error_source)
        elif request.method == "textDocument/didOpen":
            await self.api.process_open_request(
                lsp.DidOpenTextDocumentParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.activity_key,
            )
            await self._restart_if_needed()
        elif request.method == "textDocument/didChange":
            await self.api.process_did_change_request(
                lsp.DidChangeTextDocumentParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                )
            )
            await self._restart_if_needed()
        elif request.method == "textDocument/didClose":
            await self.api.process_close_request(
                lsp.DidCloseTextDocumentParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                )
            )
        elif request.method == "textDocument/didSave":
            await self.api.process_did_save_request(
                lsp.DidSaveTextDocumentParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.activity_key,
            )
            await self._restart_if_needed()
        elif request.method == "textDocument/hover":
            await self.api.process_hover_request(
                lsp.HoverParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.id,
                request.activity_key,
            )
        elif request.method == "textDocument/typeCoverage":
            await self.api.process_type_coverage_request(
                lsp.TypeCoverageParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.id,
                request.activity_key,
            )
        elif request.method == "textDocument/documentSymbol":
            await self.api.process_document_symbols_request(
                lsp.DocumentSymbolsParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.id,
                request.activity_key,
            )
        elif request.method == "textDocument/formatting":
            await self.api.process_document_formatting_request(
                lsp.DocumentFormattingParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.id,
                request.activity_key,
            )
        elif request.method == "textDocument/references":
            await self.api.process_find_all_references_request(
                lsp.ReferencesParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.id,
                request.activity_key,
            )
        elif request.method == "textDocument/prepareCallHierarchy":
            await self.api.process_call_hierarchy_request(
                lsp.CallHierarchyParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.id,
                request.activity_key,
            )
        elif request.method == "callHierarchy/incomingCalls":
            await self.api.process_call_hierarchy_incoming_call(
                lsp.CallHierarchyIncomingCallParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.id,
                request.activity_key,
            )
        elif request.method == "callHierarchy/outgoingCalls":
            await self.api.process_call_hierarchy_outgoing_call(
                lsp.CallHierarchyOutgoingCallParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.id,
                request.activity_key,
            )

        elif request.method == "workspace/symbol":
            await self.api.process_symbol_search_request(
                lsp.WorkspaceSymbolParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.id,
                request.activity_key,
            )

        elif request.method == "textDocument/rename":
            await self.api.process_rename_request(
                lsp.RenameParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.id,
                request.activity_key,
            )
        elif request.id is not None:
            raise lsp.RequestCancelledError(
                f"{request.method} Request not supported yet"
            )

    async def dispatch_request(
        self, request: json_rpc.Request
    ) -> Optional[commands.ExitCode]:
        """
        The top-level request dispatcher has two parts:
        - Forward the request to the appropriate handler method
        - For some types of requests, check that the background task is running; this
          is how we ensure the daemon connection is live (the background task will
          crash if the daemon goes down and closes the socket).

        """
        if request.method == "exit":
            LOG.info(
                "Received exit request without a shutdown request, exiting as FAILURE."
            )
            return commands.ExitCode.LANGUAGE_SERVER_EXIT
        elif request.method == "shutdown":
            await self.api.process_shutdown_request(request.id)
            return await self.wait_for_exit()
        else:
            request_task = asyncio.create_task(
                self.dispatch_nonblocking_request(request)
            )
            self.outstanding_tasks.add(request_task)
            request_task.add_done_callback(self.outstanding_tasks.discard)

    async def serve_requests(self) -> int:
        while True:
            request = await read_lsp_request(self.input_channel, self.output_channel)
            LOG.debug(f"Received LSP request: {log.truncate(str(request), 400)}")

            try:
                return_code = await self.dispatch_request(request)
                if return_code is not None:
                    return return_code
            except json_rpc.JSONRPCException as json_rpc_error:
                LOG.error(
                    f"Exception occurred while processing request: {json_rpc_error}"
                )
                await lsp.write_json_rpc_ignore_connection_error(
                    self.output_channel,
                    json_rpc.ErrorResponse(
                        id=request.id,
                        activity_key=request.activity_key,
                        code=json_rpc_error.error_code(),
                        message=str(json_rpc_error),
                    ),
                )

    async def run(self) -> int:
        """
        Launch the background tasks that deal with starting and subscribing
        to a pyre server and managing a queue of requests, then run the
        language server itself.
        """
        try:
            await self.daemon_manager.ensure_task_running()
            return await self.serve_requests()
        except lsp.ReadChannelClosedError:
            # This error can happen when the connection gets closed unilaterally
            # from the language client, which causes issue when we try to access the
            # input channel. This usually signals that the language client has exited,
            # which implies that the language server should do that as well.
            LOG.info("Connection closed by LSP client.")
            return commands.ExitCode.SUCCESS
        finally:
            await self.daemon_manager.ensure_task_stop()
