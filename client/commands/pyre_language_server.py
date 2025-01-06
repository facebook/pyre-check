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
    Tuple,
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
from ..language_server import connections, daemon_connection, features, protocol as lsp
from . import (
    commands,
    daemon_querier,
    libcst_util,
    server_state as state,
    type_error_handler,
)
from .daemon_querier import DaemonQueryFailure
from .server_state import OpenedDocumentState
from .source_code_context import SourceCodeContext

LOG: logging.Logger = logging.getLogger(__name__)
CONSECUTIVE_START_ATTEMPT_THRESHOLD: int = 5


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
    async def process_shutdown_request(self, request_id: Union[int, str, None]) -> None:
        raise NotImplementedError()


@dataclasses.dataclass(frozen=True)
class PyreLanguageServer(PyreLanguageServerApi):
    # Channel to send responses to the editor
    output_channel: connections.AsyncTextWriter

    # NOTE: The fields inside `server_state` are mutable and can be changed by the background
    # task.
    server_state: state.ServerState

    querier: daemon_querier.AbstractDaemonQuerier
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

    async def _query_pyre_daemon_type_errors(
        self, document_path: Path, type_checkable_files: Set[Path]
    ) -> Tuple[Dict[Path, List[error.Error]], Optional[str]]:
        if len(type_checkable_files) == 0:
            LOG.debug("No daemon type checkable files found")
            return {}, None
        await self.update_overlay_if_needed(document_path)
        result = await self.querier.get_type_errors(
            type_checkable_files,
        )
        type_errors: Dict[Path, List[error.Error]] = {}
        if isinstance(result, DaemonQueryFailure):
            return type_errors, result.error_message
        else:
            type_errors = result
            await self._publish_type_errors_for_files(type_errors, type_checkable_files)
            return type_errors, None

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
        type_errors, error_message = await self._query_pyre_daemon_type_errors(
            document_path, open_documents
        )

        json_type_errors = {
            str(path): [type_error.to_json() for type_error in errors]
            for path, errors in type_errors.items()
        }
        await self.write_telemetry(
            {
                "type": "LSP",
                "operation": "typeErrors",
                "filePath": str(document_path),
                "server_state_open_documents_count": len(
                    self.server_state.opened_documents
                ),
                "duration_ms": type_errors_timer.stop_in_millisecond(),
                "error_message": error_message,
                "type_errors": json_type_errors,
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
        ):
            await self.handle_overlay_type_errors(
                document_path=document_path,
                new_file_loaded=False,
                activity_key=activity_key,
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
                **daemon_status_before.as_telemetry_dict(),
            },
            activity_key,
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

    async def process_shutdown_request(self, request_id: Union[int, str, None]) -> None:
        await lsp.write_json_rpc_ignore_connection_error(
            self.output_channel,
            json_rpc.SuccessResponse(id=request_id, activity_key=None, result=None),
        )


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
        dispatch_request_duration = (  # noqa: F841
            dispatch_request_timer.stop_in_millisecond()
        )
        if self.server_state.client_register_event is not None:
            await self.server_state.client_register_event.wait()
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
        elif request.method == "textDocument/typeCoverage":
            await self.api.process_type_coverage_request(
                lsp.TypeCoverageParameters.from_json_rpc_parameters(
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
