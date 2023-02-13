# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module is responsible for handling requests from the VScode language server and generating an appropriate response.

The response typically will be generated through the Pyre daemon, and the name PyreLanguageServer was chosen for this module
because it illustrates that this is the intermediary between the Language server and the Pyre daemon.
"""

from __future__ import annotations

import dataclasses
import logging
import random
from pathlib import Path
from typing import (
    ClassVar,
    Dict,
    Generic,
    List,
    Optional,
    TYPE_CHECKING,
    TypeVar,
    Union,
)

from .. import json_rpc, log, timer

from ..language_server import connections, daemon_connection, features, protocol as lsp
from . import background, commands, daemon_querier, find_symbols, server_state as state

from .daemon_query import DaemonQueryFailure

from .server_state import OpenedDocumentState

if TYPE_CHECKING:
    from . import persistent

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
            LOG.debug(f"Exception occurred while reading JSON RPC: {json_rpc_error}")
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


@dataclasses.dataclass(frozen=True)
class SourceCodeContext:

    MAX_LINES_BEFORE_OR_AFTER: ClassVar[int] = 2500

    @staticmethod
    def from_source_and_position(
        source: str,
        position: lsp.LspPosition,
        max_lines_before_or_after: int = MAX_LINES_BEFORE_OR_AFTER,
    ) -> Optional[str]:
        lines = source.splitlines()
        line_number = position.line

        if line_number >= len(lines):
            return None

        full_document_contents = source.splitlines()
        lower_line_number = max(position.line - max_lines_before_or_after, 0)
        higher_line_number = min(
            position.line + max_lines_before_or_after + 1,
            len(full_document_contents),
        )
        return "\n".join(full_document_contents[lower_line_number:higher_line_number])


QueryResultType = TypeVar("QueryResultType")


@dataclasses.dataclass(frozen=True)
class QueryResultWithDurations(Generic[QueryResultType]):
    result: Union[QueryResultType, DaemonQueryFailure]
    overlay_update_duration: float
    query_duration: float
    overall_duration: float


@dataclasses.dataclass(frozen=True)
class PyreLanguageServerApi:
    # Channel to send responses to the editor
    output_channel: connections.AsyncTextWriter

    # NOTE: The fields inside `server_state` are mutable and can be changed by the background
    # task.
    server_state: state.ServerState

    querier: daemon_querier.AbstractDaemonQuerier
    client_type_error_handler: persistent.ClientTypeErrorHandler

    async def write_telemetry(
        self,
        parameters: Dict[str, object],
        activity_key: Optional[Dict[str, object]],
    ) -> None:
        should_write_telemetry = (
            self.server_state.server_options.language_server_features.telemetry.is_enabled()
        )
        if should_write_telemetry:
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
                    self.server_state.opened_documents[
                        document_path
                    ] = OpenedDocumentState(
                        code=code_changes,
                        is_dirty=current_is_dirty_state,
                        pyre_code_updated=True,
                    )
        return update_timer.stop_in_millisecond()

    def sample_source_code(
        self,
        document_path: Path,
        position: lsp.LspPosition,
    ) -> Optional[str]:
        downsample_rate = 100
        if random.randrange(0, downsample_rate) != 0:
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
            f"Logging file contents to scuba near requested line: {source_code_context}"
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

    async def send_overlay_type_errors(
        self,
        document_path: Path,
    ) -> None:
        await self.update_overlay_if_needed(document_path)
        result = await self.querier.get_type_errors(document_path)
        if isinstance(result, DaemonQueryFailure):
            return
        await self.client_type_error_handler.show_overlay_type_errors(
            path=document_path,
            type_errors=result,
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

        process_unsaved_changes = (
            self.server_state.server_options.language_server_features.unsaved_changes.is_enabled()
        )
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
        if (
            process_unsaved_changes
            and self.get_language_server_features().type_errors.is_enabled()
        ):
            await self.send_overlay_type_errors(
                document_path=document_path,
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
            await lsp.write_json_rpc(
                self.output_channel,
                json_rpc.SuccessResponse(
                    id=request_id,
                    activity_key=activity_key,
                    result=lsp.LspHoverResponse.empty().to_dict(),
                ),
            )
        else:
            daemon_status_before = self.server_state.status_tracker.get_status()
            hover_timer = timer.Timer()

            await self.update_overlay_if_needed(document_path)
            result = await self.querier.get_hover(
                path=document_path,
                position=parameters.position.to_pyre_position(),
            )
            error_message = None
            if isinstance(result, DaemonQueryFailure):
                LOG.info(
                    daemon_failure_string(
                        "hover", str(type(result)), result.error_message
                    )
                )
                error_message = result.error_message
                result = lsp.LspHoverResponse.empty()
            raw_result = lsp.LspHoverResponse.cached_schema().dump(
                result,
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
                    "nonEmpty": len(result.contents) > 0,
                    "response": raw_result,
                    "duration_ms": hover_timer.stop_in_millisecond(),
                    "server_state_open_documents_count": len(
                        self.server_state.opened_documents
                    ),
                    "error_message": error_message,
                    "overlays_enabled": self.server_state.server_options.language_server_features.unsaved_changes.is_enabled(),
                    **daemon_status_before.as_telemetry_dict(),
                },
                activity_key,
            )

    async def _get_definition_result(
        self, document_path: Path, position: lsp.LspPosition
    ) -> QueryResultWithDurations[List[Dict[str, object]]]:
        """
        Helper function to call the querier. Exists only to reduce code duplication
        due to shadow mode, please don't make more of these - we already have enough
        layers of handling.
        """
        overall_timer = timer.Timer()
        overlay_update_duration = await self.update_overlay_if_needed(document_path)
        query_timer = timer.Timer()
        raw_result = await self.querier.get_definition_locations(
            path=document_path,
            position=position.to_pyre_position(),
        )
        query_duration = query_timer.stop_in_millisecond()
        if isinstance(raw_result, DaemonQueryFailure):
            LOG.info(
                "%s",
                daemon_failure_string(
                    "definition", str(type(raw_result)), raw_result.error_message
                ),
            )
            result = raw_result
        else:
            result = lsp.LspLocation.cached_schema().dump(
                raw_result,
                many=True,
            )
        return QueryResultWithDurations(
            result=result,
            overlay_update_duration=overlay_update_duration,
            query_duration=query_duration,
            overall_duration=overall_timer.stop_in_millisecond(),
        )

    async def process_definition_request(
        self,
        parameters: lsp.DefinitionParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        document_path: Optional[
            Path
        ] = parameters.text_document.document_uri().to_file_path()
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
                    result=lsp.LspLocation.cached_schema().dump([], many=True),
                ),
            )
        daemon_status_before = self.server_state.status_tracker.get_status()
        shadow_mode = self.get_language_server_features().definition.is_shadow()
        # In shadow mode, we need to return an empty response immediately
        if shadow_mode:
            await lsp.write_json_rpc(
                self.output_channel,
                json_rpc.SuccessResponse(
                    id=request_id,
                    activity_key=activity_key,
                    result=lsp.LspLocation.cached_schema().dump([], many=True),
                ),
            )
        # Regardless of the mode, we want to get the actual result
        result_with_durations = await self._get_definition_result(
            document_path=document_path,
            position=parameters.position,
        )
        result = result_with_durations.result
        if isinstance(result, DaemonQueryFailure):
            error_message = result.error_message
            output_result = []
        else:
            error_message = None
            output_result = result
        # Unless we are in shadow mode, we send the response as output
        if not shadow_mode:
            await lsp.write_json_rpc(
                self.output_channel,
                json_rpc.SuccessResponse(
                    id=request_id,
                    activity_key=activity_key,
                    result=output_result,
                ),
            )
        source_code_if_sampled = self.sample_source_code(
            document_path,
            parameters.position,
        )
        await self.write_telemetry(
            {
                "type": "LSP",
                "operation": "definition",
                "filePath": str(document_path),
                "count": len(output_result),
                "response": output_result,
                "duration_ms": result_with_durations.overall_duration,
                "overlay_update_duration": result_with_durations.overlay_update_duration,
                "query_duration": result_with_durations.query_duration,
                "server_state_open_documents_count": len(
                    self.server_state.opened_documents
                ),
                "overlays_enabled": self.server_state.server_options.language_server_features.unsaved_changes.is_enabled(),
                "error_message": error_message,
                "is_dirty": self.server_state.opened_documents[document_path].is_dirty,
                "truncated_file_contents": source_code_if_sampled,
                **daemon_status_before.as_telemetry_dict(),
            },
            activity_key,
        )

    async def process_document_symbols_request(
        self,
        parameters: lsp.DocumentSymbolsParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )
        if document_path not in self.server_state.opened_documents:
            raise json_rpc.InvalidRequestError(
                f"Document URI has not been opened: {parameters.text_document.uri}"
            )
        try:
            source = document_path.read_text()
            symbols = find_symbols.parse_source_and_collect_symbols(source)
            await lsp.write_json_rpc(
                self.output_channel,
                json_rpc.SuccessResponse(
                    id=request_id,
                    activity_key=activity_key,
                    result=[s.to_dict() for s in symbols],
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

        reference_locations = await self.querier.get_reference_locations(
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

    async def process_shutdown_request(self, request_id: Union[int, str, None]) -> None:
        await lsp.write_json_rpc_ignore_connection_error(
            self.output_channel,
            json_rpc.SuccessResponse(id=request_id, activity_key=None, result=None),
        )


class CodeNavigationServerApi(PyreLanguageServerApi):
    pass


@dataclasses.dataclass(frozen=True)
class PyreLanguageServerDispatcher:
    """
    The dispatcher provides the top-level, "foreground" logic for a Pyre
    language server. Its only job is to read requests from standard input,
    parse them, and dispatch to the appropriate lower-level logic.

    There are two compontents to which we might dispatch:
    - We'll dispatch to the PyreLanguageServerApi for all request handling,
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

    daemon_manager: background.TaskManager
    api: PyreLanguageServerApi

    async def wait_for_exit(self) -> commands.ExitCode:
        await _wait_for_exit(self.input_channel, self.output_channel)
        return commands.ExitCode.SUCCESS

    async def _try_restart_pyre_daemon(self) -> None:
        if (
            self.server_state.consecutive_start_failure
            < CONSECUTIVE_START_ATTEMPT_THRESHOLD
        ):
            await self.daemon_manager.ensure_task_running()
        else:
            LOG.info(
                "Not restarting Pyre since failed consecutive start attempt limit"
                " has been reached."
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
                "Recieved exit request without a shutdown request, exiting as FAILURE."
            )
            return commands.ExitCode.FAILURE
        elif request.method == "shutdown":
            await self.api.process_shutdown_request(request.id)
            return await self.wait_for_exit()
        elif request.method == "textDocument/definition":
            await self.api.process_definition_request(
                lsp.DefinitionParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.id,
                request.activity_key,
            )
            if not self.daemon_manager.is_task_running():
                await self._try_restart_pyre_daemon()
        elif request.method == "textDocument/didOpen":
            await self.api.process_open_request(
                lsp.DidOpenTextDocumentParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.activity_key,
            )
            if not self.daemon_manager.is_task_running():
                await self._try_restart_pyre_daemon()
        elif request.method == "textDocument/didChange":
            await self.api.process_did_change_request(
                lsp.DidChangeTextDocumentParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                )
            )
            if not self.daemon_manager.is_task_running():
                await self._try_restart_pyre_daemon()
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
            if not self.daemon_manager.is_task_running():
                await self._try_restart_pyre_daemon()
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
        elif request.method == "textDocument/references":
            await self.api.process_find_all_references_request(
                lsp.ReferencesParameters.from_json_rpc_parameters(
                    request.extract_parameters()
                ),
                request.id,
                request.activity_key,
            )
        elif request.id is not None:
            raise lsp.RequestCancelledError("Request not supported yet")

    async def serve_requests(self) -> int:
        while True:
            request = await read_lsp_request(self.input_channel, self.output_channel)
            LOG.debug(f"Received LSP request: {log.truncate(str(request), 400)}")

            try:
                return_code = await self.dispatch_request(request)
                if return_code is not None:
                    return return_code
            except json_rpc.JSONRPCException as json_rpc_error:
                LOG.debug(
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
