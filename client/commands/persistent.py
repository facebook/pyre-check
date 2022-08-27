# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

import asyncio
import dataclasses
import enum
import json
import logging
import os
import subprocess
import tempfile
import traceback
from pathlib import Path
from typing import Callable, Dict, List, Optional, Sequence, Set, Union

from libcst.metadata import CodeRange

from .. import (
    command_arguments,
    configuration as configuration_module,
    dataclasses_json_extensions as json_mixins,
    error,
    json_rpc,
    log,
    statistics_logger,
    timer,
    version,
)
from ..coverage_collector import coverage_collector_for_module, CoveredAndUncoveredLines
from . import (
    backend_arguments,
    background,
    commands,
    connections,
    daemon_connection,
    daemon_query,
    daemon_socket,
    expression_level_coverage,
    find_symbols,
    frontend_configuration,
    incremental,
    language_server_protocol as lsp,
    server_event,
    start,
    statistics,
    subscription,
)

LOG: logging.Logger = logging.getLogger(__name__)

COMMAND_NAME = "persistent"

CONSECUTIVE_START_ATTEMPT_THRESHOLD: int = 5


class LSPEvent(enum.Enum):
    INITIALIZED = "initialized"
    NOT_INITIALIZED = "not initialized"
    CONNECTED = "connected"
    NOT_CONNECTED = "not connected"
    NOT_CONFIGURED = "not configured"
    DISCONNECTED = "disconnected"
    SUSPENDED = "suspended"
    STOPPED = "stopped"
    COVERED = "covered"


def _log_lsp_event(
    remote_logging: Optional[backend_arguments.RemoteLogging],
    event: LSPEvent,
    integers: Optional[Dict[str, int]] = None,
    normals: Optional[Dict[str, Optional[str]]] = None,
) -> None:
    if remote_logging is not None:
        logger = remote_logging.logger
        if logger is not None:
            log_identifier = remote_logging.identifier
            statistics_logger.log(
                category=statistics_logger.LoggerCategory.LSP_EVENTS,
                logger=logger,
                integers=integers,
                normals={
                    **(normals or {}),
                    "event": event.value,
                    "pyre client version": version.__version__,
                    **(
                        {"identifier": log_identifier}
                        if log_identifier is not None
                        else {}
                    ),
                },
            )


PyreDaemonStartOptionsReader = Callable[[], "PyreDaemonStartOptions"]
FrontendConfigurationReader = Callable[[], frontend_configuration.Base]


@dataclasses.dataclass(frozen=True)
class PyreDaemonStartOptions:
    binary: str
    project_identifier: str
    start_arguments: start.Arguments
    ide_features: Optional[configuration_module.IdeFeatures]
    strict_default: bool
    excludes: Sequence[str]
    enabled_telemetry_event: bool = False

    @staticmethod
    def create(
        start_command_argument: command_arguments.StartArguments,
        configuration: frontend_configuration.Base,
        enabled_telemetry_event: bool,
    ) -> PyreDaemonStartOptions:
        binary_location = configuration.get_binary_location(download_if_needed=True)
        if binary_location is None:
            raise configuration_module.InvalidConfiguration(
                "Cannot locate a Pyre binary to run."
            )

        start_arguments = start.create_server_arguments(
            configuration,
            start_command_argument,
        )
        if start_arguments.watchman_root is None:
            raise commands.ClientException(
                "Cannot locate a `watchman` root. Pyre's server will not function "
                "properly."
            )

        return PyreDaemonStartOptions(
            binary=str(binary_location),
            project_identifier=configuration.get_project_identifier(),
            start_arguments=start_arguments,
            ide_features=configuration.get_ide_features(),
            strict_default=configuration.is_strict(),
            excludes=configuration.get_excludes(),
            enabled_telemetry_event=enabled_telemetry_event,
        )

    @staticmethod
    def create_reader(
        command_argument: command_arguments.CommandArguments,
        start_command_argument: command_arguments.StartArguments,
        read_frontend_configuration: FrontendConfigurationReader,
        enabled_telemetry_event: bool,
    ) -> PyreDaemonStartOptionsReader:
        def read() -> PyreDaemonStartOptions:
            return PyreDaemonStartOptions.create(
                start_command_argument=start_command_argument,
                configuration=read_frontend_configuration(),
                enabled_telemetry_event=enabled_telemetry_event,
            )

        return read


def read_server_start_options(
    server_start_options_reader: PyreDaemonStartOptionsReader,
    remote_logging: Optional[backend_arguments.RemoteLogging],
) -> "PyreDaemonStartOptions":
    try:
        LOG.info("Reading Pyre server configurations...")
        return server_start_options_reader()
    except Exception:
        _log_lsp_event(
            remote_logging=remote_logging,
            event=LSPEvent.NOT_CONFIGURED,
            normals={
                "exception": traceback.format_exc(),
            },
        )
        raise


def process_initialize_request(
    parameters: lsp.InitializeParameters,
    ide_features: Optional[configuration_module.IdeFeatures] = None,
) -> lsp.InitializeResult:
    LOG.info(
        f"Received initialization request from {parameters.client_info} "
        f" (pid = {parameters.process_id})"
    )

    server_info = lsp.Info(name="pyre", version=version.__version__)
    did_change_result = (
        lsp.TextDocumentSyncKind.FULL
        if ide_features is not None
        and ide_features.is_consume_unsaved_changes_enabled()
        else lsp.TextDocumentSyncKind.NONE
    )
    server_capabilities = lsp.ServerCapabilities(
        text_document_sync=lsp.TextDocumentSyncOptions(
            open_close=True,
            change=did_change_result,
            save=lsp.SaveOptions(include_text=False),
        ),
        **(
            {
                "hover_provider": ide_features.is_hover_enabled(),
                "definition_provider": ide_features.is_go_to_definition_enabled(),
                "document_symbol_provider": ide_features.is_find_symbols_enabled(),
                "references_provider": ide_features.is_find_all_references_enabled(),
            }
            if ide_features is not None
            else {}
        ),
    )
    return lsp.InitializeResult(
        capabilities=server_capabilities, server_info=server_info
    )


@dataclasses.dataclass(frozen=True)
class InitializationSuccess:
    client_capabilities: lsp.ClientCapabilities
    client_info: Optional[lsp.Info] = None
    initialization_options: Optional[lsp.InitializationOptions] = None


@dataclasses.dataclass(frozen=True)
class InitializationFailure:
    exception: Optional[json_rpc.JSONRPCException] = None


@dataclasses.dataclass(frozen=True)
class InitializationExit:
    pass


async def try_initialize(
    input_channel: connections.AsyncTextReader,
    output_channel: connections.AsyncTextWriter,
    server_start_options_reader: PyreDaemonStartOptionsReader,
) -> Union[InitializationSuccess, InitializationFailure, InitializationExit]:
    """
    Read an LSP message from the input channel and try to initialize an LSP
    server. Also write to the output channel with proper response if the input
    message is a request instead of a notification.

    The function can return one of three possibilities:
    - If the initialization succeeds, return `InitializationSuccess`.
    - If the initialization fails, return `InitializationFailure`. There could
      be many reasons for the failure: The incoming LSP message may not be an
      initiailization request. The incoming LSP request may be malformed. Or the
      client may not complete the handshake by sending back an `initialized` request.
    - If an exit notification is received, return `InitializationExit`. The LSP
      spec allows exiting a server without a preceding initialize request.
    """
    request = None
    try:
        request = await lsp.read_json_rpc(input_channel)
        LOG.debug(f"Received pre-initialization LSP request: {request}")

        request_id = request.id
        if request_id is None:
            return (
                InitializationExit()
                if request.method == "exit"
                else InitializationFailure()
            )
        if request.method != "initialize":
            raise lsp.ServerNotInitializedError("An initialize request is needed.")
        request_parameters = request.parameters
        if request_parameters is None:
            raise lsp.ServerNotInitializedError(
                "Missing parameters for initialize request."
            )
        initialize_parameters = lsp.InitializeParameters.from_json_rpc_parameters(
            request_parameters
        )

        try:
            server_start_options = read_server_start_options(
                server_start_options_reader, remote_logging=None
            )
        except configuration_module.InvalidConfiguration as e:
            raise lsp.ServerNotInitializedError(str(e)) from None

        result = process_initialize_request(
            initialize_parameters, server_start_options.ide_features
        )
        await lsp.write_json_rpc_ignore_connection_error(
            output_channel,
            json_rpc.SuccessResponse(
                id=request_id,
                activity_key=request.activity_key,
                result=result.to_dict(),
            ),
        )

        initialized_notification = await lsp.read_json_rpc(input_channel)
        if initialized_notification.method == "shutdown":
            try:
                await _wait_for_exit(input_channel, output_channel)
            except lsp.ReadChannelClosedError:
                # This error can happen when the connection gets closed unilaterally
                # from the language client, which causes issue when we try to access
                # the input channel. This usually signals that the language client
                # has exited, which implies that the language server should do that
                # as well.
                LOG.info("Initialization connection closed by LSP client")
            return InitializationExit()
        elif initialized_notification.method != "initialized":
            actual_message = json.dumps(initialized_notification.json())
            raise lsp.ServerNotInitializedError(
                "Failed to receive an `initialized` request from client. "
                + f"Got {log.truncate(actual_message, 100)}"
            )

        return InitializationSuccess(
            client_capabilities=initialize_parameters.capabilities,
            client_info=initialize_parameters.client_info,
            initialization_options=initialize_parameters.initialization_options,
        )
    except json_rpc.JSONRPCException as json_rpc_error:
        await lsp.write_json_rpc_ignore_connection_error(
            output_channel,
            json_rpc.ErrorResponse(
                id=request.id if request is not None else None,
                activity_key=request.activity_key if request is not None else None,
                code=json_rpc_error.error_code(),
                message=str(json_rpc_error),
                data={"retry": False},
            ),
        )
        return InitializationFailure(exception=json_rpc_error)
    except lsp.ReadChannelClosedError:
        return InitializationExit()


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


async def _publish_diagnostics(
    output_channel: connections.AsyncTextWriter,
    path: Path,
    diagnostics: Sequence[lsp.Diagnostic],
) -> None:
    LOG.debug(f"Publish diagnostics for {path}: {diagnostics}")
    await lsp.write_json_rpc(
        output_channel,
        json_rpc.Request(
            method="textDocument/publishDiagnostics",
            parameters=json_rpc.ByNameParameters(
                {
                    "uri": lsp.DocumentUri.from_file_path(path).unparse(),
                    "diagnostics": [diagnostic.to_dict() for diagnostic in diagnostics],
                }
            ),
        ),
    )


async def _read_server_response(
    server_input_channel: connections.AsyncTextReader,
) -> str:
    return await server_input_channel.read_until(separator="\n")


@dataclasses.dataclass(frozen=True)
class TypeCoverageQuery:
    id: Union[int, str, None]
    path: Path
    activity_key: Optional[Dict[str, object]] = None


@dataclasses.dataclass(frozen=True)
class OverlayUpdate:
    # TODO: T126924773 Consider making the overlay id also contain a GUID or PID
    overlay_id: str
    source_path: Path
    code_update: str


@dataclasses.dataclass(frozen=True)
class HoverQuery:
    id: Union[int, str, None]
    path: Path
    position: lsp.Position
    activity_key: Optional[Dict[str, object]] = None


@dataclasses.dataclass(frozen=True)
class HoverResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    response: lsp.LspHoverResponse


@dataclasses.dataclass(frozen=True)
class DefinitionLocationQuery:
    id: Union[int, str, None]
    path: Path
    position: lsp.Position
    activity_key: Optional[Dict[str, object]] = None


@dataclasses.dataclass(frozen=True)
class DefinitionLocationResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    response: List[lsp.PyreDefinitionResponse]


@dataclasses.dataclass(frozen=True)
class ReferencesQuery:
    id: Union[int, str, None]
    path: Path
    position: lsp.Position
    activity_key: Optional[Dict[str, object]] = None


@dataclasses.dataclass(frozen=True)
class ReferencesResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    response: List[lsp.ReferencesResponse]


RequestTypes = Union[
    TypeCoverageQuery,
    HoverQuery,
    DefinitionLocationQuery,
    ReferencesQuery,
    OverlayUpdate,
]


@dataclasses.dataclass
class PyreQueryState:
    # Queue of queries.
    queries: "asyncio.Queue[RequestTypes]" = dataclasses.field(
        default_factory=asyncio.Queue
    )


@dataclasses.dataclass(frozen=True)
class LineColumn(json_mixins.CamlCaseAndExcludeJsonMixin):
    line: int
    column: int

    def to_position(self) -> lsp.Position:
        return lsp.Position(line=self.line, character=self.column)


@dataclasses.dataclass(frozen=True)
class LocationInfo(json_mixins.CamlCaseAndExcludeJsonMixin):
    start: LineColumn
    stop: LineColumn


@dataclasses.dataclass(frozen=True)
class LocationAnnotation(json_mixins.CamlCaseAndExcludeJsonMixin):
    location: LocationInfo
    annotation: str


async def _send_request(
    output_channel: connections.AsyncTextWriter, request: str
) -> None:
    LOG.debug(f"Sending `{log.truncate(request, 400)}`")
    await output_channel.write(f"{request}\n")


async def _receive_response(
    input_channel: connections.AsyncTextReader,
) -> Optional[daemon_query.Response]:
    raw_response = await _read_server_response(input_channel)
    LOG.info(f"Received `{log.truncate(raw_response, 400)}`")
    try:
        return daemon_query.Response.parse(raw_response)
    except daemon_query.InvalidQueryResponse as exception:
        LOG.info(f"Failed to parse json {raw_response} due to exception: {exception}")
        return None


async def _consume_and_drop_response(
    input_channel: connections.AsyncTextReader,
) -> None:
    raw_response = await _read_server_response(input_channel)
    LOG.info(f"Received and will drop response: `{log.truncate(raw_response, 400)}`")
    return None


@dataclasses.dataclass(frozen=True)
class QueryModulesOfPathResponse(json_mixins.CamlCaseAndExcludeJsonMixin):
    response: List[str]


@dataclasses.dataclass
class ServerState:
    # Immutable States
    client_capabilities: lsp.ClientCapabilities = lsp.ClientCapabilities()

    # Mutable States
    consecutive_start_failure: int = 0
    is_user_notified_on_buck_failure: bool = False
    opened_documents: Set[Path] = dataclasses.field(default_factory=set)
    diagnostics: Dict[Path, List[lsp.Diagnostic]] = dataclasses.field(
        default_factory=dict
    )
    last_diagnostic_update_timer: timer.Timer = dataclasses.field(
        default_factory=timer.Timer
    )
    query_state: PyreQueryState = dataclasses.field(default_factory=PyreQueryState)


@dataclasses.dataclass(frozen=True)
class StartSuccess:
    pass


@dataclasses.dataclass(frozen=True)
class BuckStartFailure:
    message: str


@dataclasses.dataclass(frozen=True)
class OtherStartFailure:
    message: str
    detail: str


async def _start_pyre_server(
    binary_location: str, pyre_arguments: start.Arguments
) -> Union[StartSuccess, BuckStartFailure, OtherStartFailure]:
    try:
        with backend_arguments.temporary_argument_file(
            pyre_arguments
        ) as argument_file_path:
            server_environment = {
                **os.environ,
                # This is to make sure that backend server shares the socket root
                # directory with the client.
                # TODO(T77556312): It might be cleaner to turn this into a
                # configuration option instead.
                "TMPDIR": tempfile.gettempdir(),
            }

            with start.background_server_log_file(
                Path(pyre_arguments.base_arguments.log_path)
            ) as server_stderr:
                server_process = await asyncio.create_subprocess_exec(
                    binary_location,
                    "newserver",
                    str(argument_file_path),
                    stdout=subprocess.PIPE,
                    stderr=server_stderr,
                    env=server_environment,
                    start_new_session=True,
                )

            server_stdout = server_process.stdout
            if server_stdout is None:
                raise RuntimeError(
                    "asyncio.create_subprocess_exec failed to set up a pipe for "
                    "server stdout"
                )

            await server_event.Waiter(wait_on_initialization=True).async_wait_on(
                connections.AsyncTextReader(
                    connections.StreamBytesReader(server_stdout)
                )
            )

        return StartSuccess()
    except server_event.ServerStartException as error:
        message = str(error)
        LOG.error(message)
        if error.kind == server_event.ErrorKind.BUCK_USER:
            return BuckStartFailure(message)
        else:
            # We know where the exception come from. Let's keep the error details
            # succinct.
            return OtherStartFailure(message=message, detail=message)
    except Exception as error:
        # These exceptions are unexpected. Let's keep verbose stack traces to
        # help with post-mortem analyses.
        message = str(error)
        detail = traceback.format_exc()
        LOG.error(f"{detail}")
        return OtherStartFailure(message=message, detail=detail)


def type_error_to_diagnostic(type_error: error.Error) -> lsp.Diagnostic:
    return lsp.Diagnostic(
        range=lsp.Range(
            start=lsp.Position(line=type_error.line - 1, character=type_error.column),
            end=lsp.Position(
                line=type_error.stop_line - 1, character=type_error.stop_column
            ),
        ),
        message=type_error.description,
        severity=lsp.DiagnosticSeverity.ERROR,
        code=None,
        source="Pyre",
    )


def type_errors_to_diagnostics(
    type_errors: Sequence[error.Error],
) -> Dict[Path, List[lsp.Diagnostic]]:
    result: Dict[Path, List[lsp.Diagnostic]] = {}
    for type_error in type_errors:
        result.setdefault(type_error.path, []).append(
            type_error_to_diagnostic(type_error)
        )
    return result


def uncovered_range_to_diagnostic(uncovered_range: CodeRange) -> lsp.Diagnostic:
    return lsp.Diagnostic(
        range=lsp.Range(
            start=lsp.Position(
                line=uncovered_range.start.line - 1,
                character=uncovered_range.start.column,
            ),
            end=lsp.Position(
                line=uncovered_range.end.line - 1, character=uncovered_range.end.column
            ),
        ),
        message=(
            "This function is not type checked. "
            "Consider adding parameter or return type annotations."
        ),
    )


def to_coverage_result(
    covered_and_uncovered_lines: CoveredAndUncoveredLines,
    uncovered_ranges: List[CodeRange],
) -> lsp.TypeCoverageResponse:
    num_covered = len(covered_and_uncovered_lines.covered_lines)
    num_uncovered = len(covered_and_uncovered_lines.uncovered_lines)
    num_total = num_covered + num_uncovered
    if num_total == 0:
        return lsp.TypeCoverageResponse(
            covered_percent=100.0, uncovered_ranges=[], default_message=""
        )
    else:
        return lsp.TypeCoverageResponse(
            covered_percent=100.0 * num_covered / num_total,
            uncovered_ranges=[
                uncovered_range_to_diagnostic(uncovered_range)
                for uncovered_range in uncovered_ranges
            ],
            default_message="Consider adding type annotations.",
        )


def file_not_typechecked_coverage_result() -> lsp.TypeCoverageResponse:
    return lsp.TypeCoverageResponse(
        covered_percent=0.0,
        uncovered_ranges=[
            lsp.Diagnostic(
                range=lsp.Range(
                    start=lsp.Position(
                        line=0,
                        character=0,
                    ),
                    end=lsp.Position(line=1, character=0),
                ),
                message="This file is not type checked by Pyre.",
            )
        ],
        default_message="",
    )


def path_to_coverage_response(
    path: Path, strict_default: bool
) -> Optional[lsp.TypeCoverageResponse]:
    module = statistics.parse_path_to_module(path)
    if module is None:
        return None

    coverage_collector = coverage_collector_for_module(
        str(path), module, strict_default
    )
    covered_and_uncovered_lines = coverage_collector.covered_and_uncovered_lines()
    uncovered_ranges = [f.code_range for f in coverage_collector.uncovered_functions()]
    return to_coverage_result(covered_and_uncovered_lines, uncovered_ranges)


def path_to_expression_coverage_response(
    strict_default: bool,
    expression_coverage: expression_level_coverage.ExpressionLevelCoverageResponse,
) -> lsp.TypeCoverageResponse:
    path_coverage = expression_coverage.response[0]
    if isinstance(path_coverage, expression_level_coverage.ErrorAtPathResponse):
        uncovered_expressions_diagnostics = []
        covered_percent = 0
    else:
        uncovered_expressions_diagnostics = (
            expression_level_coverage.get_uncovered_expression_diagnostics(
                expression_coverage
            )
        )
        covered_percent = expression_level_coverage.get_percent_covered_per_path(
            path_coverage
        )
    return lsp.TypeCoverageResponse(
        covered_percent=covered_percent,
        uncovered_ranges=uncovered_expressions_diagnostics,
        default_message="Consider adding type annotations.",
    )


class PyreQueryHandler(background.Task):
    def __init__(
        self,
        query_state: PyreQueryState,
        server_start_options_reader: PyreDaemonStartOptionsReader,
        client_output_channel: connections.AsyncTextWriter,
    ) -> None:
        self.query_state = query_state
        self.server_start_options_reader = server_start_options_reader
        self.client_output_channel = client_output_channel

    async def _query_modules_of_path(
        self,
        path: Path,
        socket_path: Path,
        consume_unsaved_changes_enabled: bool,
    ) -> Optional[QueryModulesOfPathResponse]:
        overlay_id = str(path) if consume_unsaved_changes_enabled else None
        return await daemon_query.attempt_typed_async_query(
            response_type=QueryModulesOfPathResponse,
            socket_path=socket_path,
            query_text=f"modules_of_path('{path}')",
            overlay_id=overlay_id,
        )

    async def _query_is_typechecked(
        self, path: Path, socket_path: Path, consume_unsaved_changes_enabled: bool
    ) -> Optional[bool]:
        response = await self._query_modules_of_path(
            path, socket_path, consume_unsaved_changes_enabled
        )
        if response is None:
            return None
        else:
            return len(response.response) > 0

    async def _query_type_coverage(
        self,
        path: Path,
        strict_default: bool,
        socket_path: Path,
        expression_level_coverage_enabled: bool,
        consume_unsaved_changes_enabled: bool,
    ) -> Optional[lsp.TypeCoverageResponse]:
        is_typechecked = await self._query_is_typechecked(
            path, socket_path, consume_unsaved_changes_enabled
        )
        if is_typechecked is None:
            return None
        elif expression_level_coverage_enabled:
            response = await daemon_query.attempt_async_query(
                socket_path=socket_path,
                query_text=f"expression_level_coverage('{path}')",
            )
            if response is None:
                return None
            expression_coverage = (
                expression_level_coverage._make_expression_level_coverage_response(
                    response.payload
                )
            )
            if expression_coverage is None:
                return file_not_typechecked_coverage_result()
            return path_to_expression_coverage_response(
                strict_default, expression_coverage
            )
        elif is_typechecked:
            return path_to_coverage_response(path, strict_default)
        else:
            return file_not_typechecked_coverage_result()

    async def _handle_type_coverage_query(
        self,
        query: TypeCoverageQuery,
        strict_default: bool,
        socket_path: Path,
        expression_level_coverage_enabled: bool,
        consume_unsaved_changes_enabled: bool,
    ) -> None:
        type_coverage_result = await self._query_type_coverage(
            query.path,
            strict_default,
            socket_path,
            expression_level_coverage_enabled,
            consume_unsaved_changes_enabled,
        )
        if type_coverage_result is not None:
            await lsp.write_json_rpc(
                self.client_output_channel,
                json_rpc.SuccessResponse(
                    id=query.id,
                    activity_key=query.activity_key,
                    result=type_coverage_result.to_dict(),
                ),
            )

    async def _query_and_send_hover_contents(
        self,
        query: HoverQuery,
        socket_path: Path,
        enabled_telemetry_event: bool,
        consume_unsaved_changes_enabled: bool,
    ) -> None:
        path_string = f"'{query.path}'"
        query_text = (
            f"hover_info_for_position(path={path_string},"
            f" line={query.position.line}, column={query.position.character})"
        )
        overlay_id = str(query.path) if consume_unsaved_changes_enabled else None
        daemon_response = await daemon_query.attempt_typed_async_query(
            response_type=HoverResponse,
            socket_path=socket_path,
            query_text=query_text,
            overlay_id=overlay_id,
        )
        response = (
            daemon_response.response
            if daemon_response
            else lsp.LspHoverResponse.empty()
        )
        result = lsp.LspHoverResponse.cached_schema().dump(
            response,
        )
        await lsp.write_json_rpc(
            self.client_output_channel,
            json_rpc.SuccessResponse(
                id=query.id,
                activity_key=query.activity_key,
                result=result,
            ),
        )
        await _write_telemetry(
            enabled_telemetry_event,
            self.client_output_channel,
            {
                "type": "LSP",
                "operation": "hover",
                "filePath": str(query.path),
                "nonEmpty": len(response.contents) > 0,
                "response": result,
            },
            query.activity_key,
        )

    async def _query_and_send_definition_location(
        self,
        query: DefinitionLocationQuery,
        socket_path: Path,
        enabled_telemetry_event: bool,
        consume_unsaved_changes_enabled: bool,
    ) -> None:
        path_string = f"'{query.path}'"
        query_text = (
            f"location_of_definition(path={path_string},"
            f" line={query.position.line}, column={query.position.character})"
        )
        overlay_id = str(query.path) if consume_unsaved_changes_enabled else None
        daemon_response = await daemon_query.attempt_typed_async_query(
            response_type=DefinitionLocationResponse,
            socket_path=socket_path,
            query_text=query_text,
            overlay_id=overlay_id,
        )
        definitions = (
            [
                response.to_lsp_definition_response()
                for response in daemon_response.response
            ]
            if daemon_response is not None
            else []
        )
        result = lsp.LspDefinitionResponse.cached_schema().dump(
            definitions,
            many=True,
        )
        await lsp.write_json_rpc(
            self.client_output_channel,
            json_rpc.SuccessResponse(
                id=query.id,
                activity_key=query.activity_key,
                result=result,
            ),
        )
        await _write_telemetry(
            enabled_telemetry_event,
            self.client_output_channel,
            {
                "type": "LSP",
                "operation": "definition",
                "filePath": str(query.path),
                "count": len(definitions),
                "response": result,
            },
            query.activity_key,
        )

    async def _handle_find_all_references_query(
        self,
        query: ReferencesQuery,
        socket_path: Path,
        consume_unsaved_changes_enabled: bool,
    ) -> None:
        path_string = f"'{query.path}'"
        query_text = (
            f"find_references(path={path_string},"
            f" line={query.position.line}, column={query.position.character})"
        )
        overlay_id = str(query.path) if consume_unsaved_changes_enabled else None
        daemon_response = await daemon_query.attempt_typed_async_query(
            response_type=ReferencesResponse,
            socket_path=socket_path,
            query_text=query_text,
            overlay_id=overlay_id,
        )
        reference_locations = (
            [
                response.to_lsp_definition_response()
                for response in daemon_response.response
            ]
            if daemon_response is not None
            else []
        )
        await lsp.write_json_rpc(
            self.client_output_channel,
            json_rpc.SuccessResponse(
                id=query.id,
                activity_key=query.activity_key,
                result=lsp.LspDefinitionResponse.cached_schema().dump(
                    reference_locations,
                    many=True,
                ),
            ),
        )

    async def _handle_overlay_update_request(
        self, request: OverlayUpdate, socket_path: Path
    ) -> None:
        source_path = f"{request.source_path}"
        overlay_update_dict = {
            "overlay_id": request.overlay_id,
            "source_path": source_path,
            "code_update": ["NewCode", request.code_update],
        }
        # Drop the response (the daemon code will log it for us)
        await daemon_connection.attempt_send_async_raw_request(
            socket_path=socket_path,
            request=json.dumps(overlay_update_dict),
        )

    async def run_request_handler(
        self, server_start_options: "PyreDaemonStartOptions"
    ) -> None:
        start_arguments = server_start_options.start_arguments
        socket_path = daemon_socket.get_default_socket_path(
            project_root=Path(start_arguments.base_arguments.global_root),
            relative_local_root=start_arguments.base_arguments.relative_local_root,
        )
        strict_default = server_start_options.strict_default
        expression_level_coverage_enabled = (
            server_start_options.ide_features is not None
            and server_start_options.ide_features.is_expression_level_coverage_enabled()
        )
        enabled_telemetry_event = server_start_options.enabled_telemetry_event
        consume_unsaved_changes_enabled = (
            server_start_options.ide_features is not None
            and server_start_options.ide_features.is_consume_unsaved_changes_enabled()
        )
        while True:
            query = await self.query_state.queries.get()
            if isinstance(query, TypeCoverageQuery):
                await self._handle_type_coverage_query(
                    query,
                    strict_default,
                    socket_path,
                    expression_level_coverage_enabled,
                    consume_unsaved_changes_enabled,
                )
            elif isinstance(query, HoverQuery):
                await self._query_and_send_hover_contents(
                    query,
                    socket_path,
                    enabled_telemetry_event,
                    consume_unsaved_changes_enabled,
                )
            elif isinstance(query, DefinitionLocationQuery):
                await self._query_and_send_definition_location(
                    query,
                    socket_path,
                    enabled_telemetry_event,
                    consume_unsaved_changes_enabled,
                )
            elif isinstance(query, ReferencesQuery):
                await self._handle_find_all_references_query(
                    query, socket_path, consume_unsaved_changes_enabled
                )
            elif isinstance(query, OverlayUpdate):
                await self._handle_overlay_update_request(query, socket_path)

    def read_server_start_options(self) -> "PyreDaemonStartOptions":
        try:
            LOG.info("Reading Pyre server configurations...")
            return self.server_start_options_reader()
        except Exception:
            LOG.error("Pyre query handler failed to read server configuration")
            raise

    async def run(self) -> None:
        """
        Reread the server start options, which can change due to configuration
        reloading, and run with error logging.
        """
        server_start_options = self.read_server_start_options()

        try:
            LOG.info(
                "Running Pyre query manager using"
                f" configuration: {server_start_options}"
            )
            await self.run_request_handler(server_start_options)
        except Exception:
            LOG.error("Failed to run the Pyre query handler")
            raise


def _client_has_status_bar_support(
    client_capabilities: lsp.ClientCapabilities,
) -> bool:
    window_capabilities = client_capabilities.window
    if window_capabilities is not None:
        return window_capabilities.status is not None
    else:
        return False


async def _write_telemetry(
    enabled: bool,
    output_channel: connections.AsyncTextWriter,
    parameters: Dict[str, object],
    activity_key: Optional[Dict[str, object]],
) -> None:
    if enabled:
        await lsp.write_json_rpc_ignore_connection_error(
            output_channel,
            json_rpc.Request(
                activity_key=activity_key,
                method="telemetry/event",
                parameters=json_rpc.ByNameParameters(parameters),
            ),
        )


async def _write_status(
    output_channel: connections.AsyncTextWriter,
    message: str,
    short_message: Optional[str] = None,
    level: lsp.MessageType = lsp.MessageType.INFO,
) -> None:
    await lsp.write_json_rpc(
        output_channel,
        json_rpc.Request(
            id=0,  # the value doesn't matter but the existence does
            method="window/showStatus",
            parameters=json_rpc.ByNameParameters(
                {
                    "type": int(level),
                    "message": message,
                    **(
                        {} if short_message is None else {"shortMessage": short_message}
                    ),
                }
            ),
        ),
    )


async def _write_notification(
    output_channel: connections.AsyncTextWriter,
    message: str,
    short_message: Optional[str] = None,
    level: lsp.MessageType = lsp.MessageType.INFO,
) -> None:
    await lsp.write_json_rpc(
        output_channel,
        json_rpc.Request(
            method="window/showMessage",
            parameters=json_rpc.ByNameParameters(
                {
                    "type": int(level),
                    "message": (
                        message
                        if short_message is None
                        else f"{short_message}: {message}"
                    ),
                }
            ),
        ),
    )


class PyreDaemonShutdown(Exception):
    pass


class PyreDaemonLaunchAndSubscribeHandler(background.Task):
    server_start_options_reader: PyreDaemonStartOptionsReader
    remote_logging: Optional[backend_arguments.RemoteLogging]
    client_output_channel: connections.AsyncTextWriter
    server_state: ServerState

    def __init__(
        self,
        server_start_options_reader: PyreDaemonStartOptionsReader,
        client_output_channel: connections.AsyncTextWriter,
        server_state: ServerState,
        remote_logging: Optional[backend_arguments.RemoteLogging] = None,
    ) -> None:
        self.server_start_options_reader = server_start_options_reader
        self.remote_logging = remote_logging
        self.client_output_channel = client_output_channel
        self.server_state = server_state

    async def show_notification_message_to_client(
        self,
        message: str,
        level: lsp.MessageType = lsp.MessageType.INFO,
    ) -> None:
        await _write_notification(self.client_output_channel, message, level=level)

    async def show_status_message_to_client(
        self,
        message: str,
        short_message: Optional[str] = None,
        level: lsp.MessageType = lsp.MessageType.INFO,
        fallback_to_notification: bool = False,
    ) -> None:
        if _client_has_status_bar_support(self.server_state.client_capabilities):
            await _write_status(
                self.client_output_channel, message, short_message, level
            )
        elif fallback_to_notification:
            await _write_notification(
                self.client_output_channel, message, short_message, level
            )

    async def log_and_show_status_message_to_client(
        self,
        message: str,
        short_message: Optional[str] = None,
        level: lsp.MessageType = lsp.MessageType.INFO,
        fallback_to_notification: bool = False,
    ) -> None:
        log_message = (
            message if short_message is None else f"[{short_message}] {message}"
        )
        if level == lsp.MessageType.ERROR:
            LOG.error(log_message)
        elif level == lsp.MessageType.WARNING:
            LOG.warning(log_message)
        elif level == lsp.MessageType.INFO:
            LOG.info(log_message)
        else:
            LOG.debug(log_message)
        await self.show_status_message_to_client(
            message, short_message, level, fallback_to_notification
        )

    def update_type_errors(self, type_errors: Sequence[error.Error]) -> None:
        LOG.info(
            "Refreshing type errors received from Pyre server. "
            f"Total number of type errors is {len(type_errors)}."
        )
        incremental.log_error_statistics(
            remote_logging=self.remote_logging,
            type_errors=type_errors,
            command_name=COMMAND_NAME,
        )
        self.server_state.diagnostics = type_errors_to_diagnostics(type_errors)

    async def clear_type_errors_for_client(self) -> None:
        for path in self.server_state.diagnostics:
            await _publish_diagnostics(self.client_output_channel, path, [])
        last_update_timer = self.server_state.last_diagnostic_update_timer
        _log_lsp_event(
            self.remote_logging,
            LSPEvent.COVERED,
            integers={"duration": int(last_update_timer.stop_in_millisecond())},
        )
        # Reset the timestamp to avoid duplicate counting
        last_update_timer.reset()

    async def show_type_errors_to_client(self) -> None:
        for path, diagnostics in self.server_state.diagnostics.items():
            await _publish_diagnostics(self.client_output_channel, path, diagnostics)
        self.server_state.last_diagnostic_update_timer.reset()

    async def handle_type_error_subscription(
        self, type_error_subscription: subscription.TypeErrors
    ) -> None:
        await self.clear_type_errors_for_client()
        self.update_type_errors(type_error_subscription.errors)
        await self.show_type_errors_to_client()
        await self.log_and_show_status_message_to_client(
            "Pyre has completed an incremental check and is currently "
            "watching on further source changes.",
            short_message="Pyre Ready",
            level=lsp.MessageType.INFO,
        )

    async def handle_status_update_subscription(
        self, status_update_subscription: subscription.StatusUpdate
    ) -> None:
        await self.clear_type_errors_for_client()
        if status_update_subscription.kind == "Rebuilding":
            await self.log_and_show_status_message_to_client(
                "Pyre is busy rebuilding the project for type checking...",
                short_message="Pyre (waiting for Buck)",
                level=lsp.MessageType.WARNING,
            )
        elif status_update_subscription.kind == "Rechecking":
            await self.log_and_show_status_message_to_client(
                "Pyre is busy re-type-checking the project...",
                short_message="Pyre (checking)",
                level=lsp.MessageType.WARNING,
            )

    async def handle_error_subscription(
        self, error_subscription: subscription.Error
    ) -> None:
        message = error_subscription.message
        LOG.info(f"Received error from subscription channel: {message}")
        raise PyreDaemonShutdown(message)

    async def _handle_subscription_body(
        self, subscription_body: subscription.Body
    ) -> None:
        if isinstance(subscription_body, subscription.TypeErrors):
            await self.handle_type_error_subscription(subscription_body)
        elif isinstance(subscription_body, subscription.StatusUpdate):
            await self.handle_status_update_subscription(subscription_body)
        elif isinstance(subscription_body, subscription.Error):
            await self.handle_error_subscription(subscription_body)

    async def _subscribe_to_type_error(
        self,
        server_input_channel: connections.AsyncTextReader,
        server_output_channel: connections.AsyncTextWriter,
    ) -> None:
        subscription_name = f"persistent_{os.getpid()}"
        await server_output_channel.write(
            f'["SubscribeToTypeErrors", "{subscription_name}"]\n'
        )

        first_response = await _read_server_response(server_input_channel)
        initial_type_errors = incremental.parse_type_error_response(first_response)
        self.update_type_errors(initial_type_errors)
        await self.show_type_errors_to_client()

        while True:
            raw_subscription_response = await _read_server_response(
                server_input_channel
            )
            subscription_response = subscription.Response.parse(
                raw_subscription_response
            )
            if subscription_name == subscription_response.name:
                await self._handle_subscription_body(subscription_response.body)

    async def subscribe_to_type_error(
        self,
        server_input_channel: connections.AsyncTextReader,
        server_output_channel: connections.AsyncTextWriter,
    ) -> None:
        try:
            await self._subscribe_to_type_error(
                server_input_channel, server_output_channel
            )
        finally:
            await self.show_status_message_to_client(
                "Lost connection to the background Pyre Server. "
                "This usually happens when Pyre detect changes in project which "
                "it was not able to handle incrementally. "
                "A new Pyre server will be started next time you open or save "
                "a .py file",
                short_message="Pyre Stopped",
                level=lsp.MessageType.ERROR,
                fallback_to_notification=True,
            )
            await self.clear_type_errors_for_client()
            self.server_state.diagnostics = {}

    @staticmethod
    def _auxiliary_logging_info(
        server_start_options: PyreDaemonStartOptions,
    ) -> Dict[str, Optional[str]]:
        relative_local_root = (
            server_start_options.start_arguments.base_arguments.relative_local_root
        )
        return {
            "binary": server_start_options.binary,
            "log_path": server_start_options.start_arguments.base_arguments.log_path,
            "global_root": (
                server_start_options.start_arguments.base_arguments.global_root
            ),
            **(
                {}
                if relative_local_root is None
                else {"local_root": relative_local_root}
            ),
        }

    async def _try_connect_and_subscribe(
        self,
        server_start_options: PyreDaemonStartOptions,
        socket_path: Path,
        connection_timer: timer.Timer,
        is_preexisting: bool,
    ) -> None:
        project_identifier = server_start_options.project_identifier
        async with connections.connect_async(socket_path) as (
            input_channel,
            output_channel,
        ):
            if is_preexisting:
                await self.log_and_show_status_message_to_client(
                    "Established connection with existing Pyre server at "
                    f"`{project_identifier}`.",
                    short_message="Pyre Ready",
                    level=lsp.MessageType.INFO,
                    fallback_to_notification=True,
                )
            else:
                await self.log_and_show_status_message_to_client(
                    f"Pyre server at `{project_identifier}` has been initialized.",
                    short_message="Pyre Ready",
                    level=lsp.MessageType.INFO,
                    fallback_to_notification=True,
                )
            self.server_state.consecutive_start_failure = 0
            self.server_state.is_user_notified_on_buck_failure = False
            _log_lsp_event(
                remote_logging=self.remote_logging,
                event=LSPEvent.CONNECTED,
                integers={"duration": int(connection_timer.stop_in_millisecond())},
                normals={
                    "connected_to": (
                        "already_running_server"
                        if is_preexisting
                        else "newly_started_server"
                    ),
                    **self._auxiliary_logging_info(server_start_options),
                },
            )
            await self.subscribe_to_type_error(input_channel, output_channel)

    async def launch_and_subscribe(
        self, server_start_options: PyreDaemonStartOptions
    ) -> None:
        project_identifier = server_start_options.project_identifier
        start_arguments = server_start_options.start_arguments
        socket_path = daemon_socket.get_default_socket_path(
            project_root=Path(start_arguments.base_arguments.global_root),
            relative_local_root=start_arguments.base_arguments.relative_local_root,
        )

        connection_timer = timer.Timer()
        try:
            return await self._try_connect_and_subscribe(
                server_start_options,
                socket_path,
                connection_timer,
                is_preexisting=True,
            )
        except connections.ConnectionFailure:
            pass

        await self.log_and_show_status_message_to_client(
            f"Starting a new Pyre server at `{project_identifier}` in "
            "the background.",
            short_message="Starting Pyre...",
            level=lsp.MessageType.WARNING,
            fallback_to_notification=True,
        )
        start_status = await _start_pyre_server(
            server_start_options.binary, start_arguments
        )
        if isinstance(start_status, StartSuccess):
            await self._try_connect_and_subscribe(
                server_start_options,
                socket_path,
                connection_timer,
                is_preexisting=False,
            )
        elif isinstance(start_status, BuckStartFailure):
            # Buck start failures are intentionally not counted towards
            # `consecutive_start_failure` -- they happen far too often in practice
            # so we do not want them to trigger suspensions.
            _log_lsp_event(
                remote_logging=self.remote_logging,
                event=LSPEvent.NOT_CONNECTED,
                integers={"duration": int(connection_timer.stop_in_millisecond())},
                normals={
                    **self._auxiliary_logging_info(server_start_options),
                    "exception": str(start_status.message),
                },
            )
            if not self.server_state.is_user_notified_on_buck_failure:
                await self.show_notification_message_to_client(
                    f"Cannot start a new Pyre server at `{project_identifier}` "
                    "due to Buck failure. If you added or changed a target, "
                    "make sure the target file is parsable and the owning "
                    "targets are buildable by Buck. If you removed a target, "
                    "make sure that target is not explicitly referenced from the "
                    "Pyre configuration file of the containing project.",
                    level=lsp.MessageType.ERROR,
                )
                self.server_state.is_user_notified_on_buck_failure = True
            await self.show_status_message_to_client(
                f"Cannot start a new Pyre server at `{project_identifier}`. "
                f"{start_status.message}",
                short_message="Pyre Stopped",
                level=lsp.MessageType.INFO,
                fallback_to_notification=False,
            )
        elif isinstance(start_status, OtherStartFailure):
            self.server_state.consecutive_start_failure += 1
            if (
                self.server_state.consecutive_start_failure
                < CONSECUTIVE_START_ATTEMPT_THRESHOLD
            ):
                _log_lsp_event(
                    remote_logging=self.remote_logging,
                    event=LSPEvent.NOT_CONNECTED,
                    integers={"duration": int(connection_timer.stop_in_millisecond())},
                    normals={
                        **self._auxiliary_logging_info(server_start_options),
                        "exception": str(start_status.detail),
                    },
                )
                await self.show_status_message_to_client(
                    f"Cannot start a new Pyre server at `{project_identifier}`. "
                    f"{start_status.message}",
                    short_message="Pyre Stopped",
                    level=lsp.MessageType.INFO,
                    fallback_to_notification=True,
                )
            else:
                _log_lsp_event(
                    remote_logging=self.remote_logging,
                    event=LSPEvent.SUSPENDED,
                    integers={"duration": int(connection_timer.stop_in_millisecond())},
                    normals={
                        **self._auxiliary_logging_info(server_start_options),
                        "exception": str(start_status.detail),
                    },
                )
                await self.show_status_message_to_client(
                    f"Pyre server restart at `{project_identifier}` has been "
                    "failing repeatedly. Disabling The Pyre plugin for now.",
                    short_message="Pyre Disabled",
                    level=lsp.MessageType.ERROR,
                    fallback_to_notification=True,
                )
        else:
            raise RuntimeError("Impossible type for `start_status`")

    async def run(self) -> None:
        """
        Reread the server start options, which can change due to configuration
        reloading, and run with error logging.
        """
        server_start_options = read_server_start_options(
            self.server_start_options_reader, self.remote_logging
        )
        session_timer = timer.Timer()
        error_message: Optional[str] = None
        try:
            LOG.info(f"Starting Pyre server from configuration: {server_start_options}")
            await self.launch_and_subscribe(server_start_options)
        except asyncio.CancelledError:
            error_message = "Explicit termination request"
            raise
        except PyreDaemonShutdown as error:
            error_message = f"Pyre server shutdown: {error}"
        except BaseException:
            error_message = traceback.format_exc()
            raise
        finally:
            _log_lsp_event(
                remote_logging=self.remote_logging,
                event=LSPEvent.DISCONNECTED,
                integers={"duration": int(session_timer.stop_in_millisecond())},
                normals={
                    **self._auxiliary_logging_info(server_start_options),
                    **(
                        {"exception": error_message}
                        if error_message is not None
                        else {}
                    ),
                },
            )


@dataclasses.dataclass(frozen=True)
class PyreServer:
    # I/O Channels
    input_channel: connections.AsyncTextReader
    output_channel: connections.AsyncTextWriter

    # inside this task manager is a PyreDaemonLaunchAndSubscribeHandler
    pyre_manager: background.TaskManager
    # inside this task manager is a PyreQueryHandler
    pyre_query_manager: background.TaskManager
    # NOTE: The fields inside `server_state` are mutable and can be changed by `pyre_manager`
    server_state: ServerState

    async def wait_for_exit(self) -> int:
        await _wait_for_exit(self.input_channel, self.output_channel)
        return 0

    async def _try_restart_pyre_server(self) -> None:
        if (
            self.server_state.consecutive_start_failure
            < CONSECUTIVE_START_ATTEMPT_THRESHOLD
        ):
            await self.pyre_manager.ensure_task_running()
        else:
            LOG.info(
                "Not restarting Pyre since failed consecutive start attempt limit"
                " has been reached."
            )

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
        self.server_state.opened_documents.add(document_path)
        LOG.info(f"File opened: {document_path}")

        # Attempt to trigger a background Pyre server start on each file open
        if not self.pyre_manager.is_task_running():
            await self._try_restart_pyre_server()

    async def process_close_request(
        self, parameters: lsp.DidCloseTextDocumentParameters
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )
        try:
            self.server_state.opened_documents.remove(document_path)
            LOG.info(f"File closed: {document_path}")
        except KeyError:
            LOG.warning(f"Trying to close an un-opened file: {document_path}")

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

        if document_path not in self.server_state.opened_documents:
            return

        overlay_update = OverlayUpdate(
            str(document_path.resolve()),
            document_path.resolve(),
            str(
                "".join(
                    [
                        content_change.text
                        for content_change in parameters.content_changes
                    ]
                )
            ),
        )

        self.server_state.query_state.queries.put_nowait(overlay_update)

        # Attempt to trigger a background Pyre server start on each file change
        if not self.pyre_manager.is_task_running():
            await self._try_restart_pyre_server()

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

        if document_path not in self.server_state.opened_documents:
            return

        # Attempt to trigger a background Pyre server start on each file save
        if not self.pyre_manager.is_task_running():
            await self._try_restart_pyre_server()

    async def process_type_coverage_request(
        self,
        parameters: lsp.TypeCoverageTextDocumentParameters,
        request_id: Union[int, str, None],
        activity_key: Optional[Dict[str, object]] = None,
    ) -> None:
        document_path = parameters.text_document.document_uri().to_file_path()
        if document_path is None:
            raise json_rpc.InvalidRequestError(
                f"Document URI is not a file: {parameters.text_document.uri}"
            )
        await self.server_state.query_state.queries.put(
            TypeCoverageQuery(
                id=request_id, activity_key=activity_key, path=document_path
            )
        )

    async def process_hover_request(
        self,
        parameters: lsp.HoverTextDocumentParameters,
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
            self.server_state.query_state.queries.put_nowait(
                HoverQuery(
                    id=request_id,
                    activity_key=activity_key,
                    path=document_path,
                    position=parameters.position.to_pyre_position(),
                )
            )

    async def process_definition_request(
        self,
        parameters: lsp.DefinitionTextDocumentParameters,
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
                    result=lsp.LspDefinitionResponse.cached_schema().dump(
                        [], many=True
                    ),
                ),
            )
            return

        self.server_state.query_state.queries.put_nowait(
            DefinitionLocationQuery(
                id=request_id,
                activity_key=activity_key,
                path=document_path,
                position=parameters.position.to_pyre_position(),
            )
        )

    async def process_document_symbols_request(
        self,
        parameters: lsp.DocumentSymbolsTextDocumentParameters,
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
        parameters: lsp.ReferencesTextDocumentParameters,
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
                    result=lsp.LspDefinitionResponse.cached_schema().dump(
                        [], many=True
                    ),
                ),
            )
            return

        self.server_state.query_state.queries.put_nowait(
            ReferencesQuery(
                id=request_id,
                activity_key=activity_key,
                path=document_path,
                position=parameters.position.to_pyre_position(),
            )
        )

    async def process_shutdown_request(self, request_id: Union[int, str, None]) -> int:
        await lsp.write_json_rpc_ignore_connection_error(
            self.output_channel,
            json_rpc.SuccessResponse(id=request_id, activity_key=None, result=None),
        )
        return await self.wait_for_exit()

    async def run_request_handler(self) -> int:
        while True:
            request = await read_lsp_request(self.input_channel, self.output_channel)
            LOG.debug(f"Received LSP request: {log.truncate(str(request), 400)}")

            try:
                if request.method == "exit":
                    return commands.ExitCode.FAILURE
                elif request.method == "shutdown":
                    return await self.process_shutdown_request(request.id)
                elif request.method == "textDocument/definition":
                    parameters = request.parameters
                    if parameters is None:
                        raise json_rpc.InvalidRequestError(
                            "Missing parameters for definition method"
                        )
                    await self.process_definition_request(
                        lsp.DefinitionTextDocumentParameters.from_json_rpc_parameters(
                            parameters
                        ),
                        request.id,
                        request.activity_key,
                    )
                elif request.method == "textDocument/didOpen":
                    parameters = request.parameters
                    if parameters is None:
                        raise json_rpc.InvalidRequestError(
                            "Missing parameters for didOpen method"
                        )
                    await self.process_open_request(
                        lsp.DidOpenTextDocumentParameters.from_json_rpc_parameters(
                            parameters
                        ),
                        request.activity_key,
                    )
                elif request.method == "textDocument/didChange":
                    parameters = request.parameters
                    if parameters is None:
                        raise json_rpc.InvalidRequestError(
                            "Missing parameters for didChange method"
                        )
                    await self.process_did_change_request(
                        lsp.DidChangeTextDocumentParameters.from_json_rpc_parameters(
                            parameters
                        )
                    )
                elif request.method == "textDocument/didClose":
                    parameters = request.parameters
                    if parameters is None:
                        raise json_rpc.InvalidRequestError(
                            "Missing parameters for didClose method"
                        )
                    await self.process_close_request(
                        lsp.DidCloseTextDocumentParameters.from_json_rpc_parameters(
                            parameters
                        )
                    )
                elif request.method == "textDocument/didSave":
                    parameters = request.parameters
                    if parameters is None:
                        raise json_rpc.InvalidRequestError(
                            "Missing parameters for didSave method"
                        )
                    await self.process_did_save_request(
                        lsp.DidSaveTextDocumentParameters.from_json_rpc_parameters(
                            parameters
                        ),
                        request.activity_key,
                    )
                elif request.method == "textDocument/hover":
                    parameters = request.parameters
                    if parameters is None:
                        raise json_rpc.InvalidRequestError(
                            "Missing parameters for hover method"
                        )
                    await self.process_hover_request(
                        lsp.HoverTextDocumentParameters.from_json_rpc_parameters(
                            parameters
                        ),
                        request.id,
                        request.activity_key,
                    )
                elif request.method == "textDocument/typeCoverage":
                    parameters = request.parameters
                    if parameters is None:
                        raise json_rpc.InvalidRequestError(
                            "Missing parameters for typeCoverage method"
                        )
                    await self.process_type_coverage_request(
                        lsp.TypeCoverageTextDocumentParameters.from_json_rpc_parameters(
                            parameters
                        ),
                        request.id,
                        request.activity_key,
                    )
                elif request.method == "textDocument/documentSymbol":
                    parameters = request.parameters
                    if parameters is None:
                        raise json_rpc.InvalidRequestError(
                            "Mising Parameters for document symbols"
                        )
                    await self.process_document_symbols_request(
                        lsp.DocumentSymbolsTextDocumentParameters.from_json_rpc_parameters(
                            parameters
                        ),
                        request.id,
                        request.activity_key,
                    )
                elif request.method == "textDocument/references":
                    parameters = request.parameters
                    if parameters is None:
                        raise json_rpc.InvalidRequestError(
                            "Missing parameters for find all references"
                        )
                    await self.process_find_all_references_request(
                        lsp.ReferencesTextDocumentParameters.from_json_rpc_parameters(
                            parameters
                        ),
                        request.id,
                        request.activity_key,
                    )
                elif request.id is not None:
                    raise lsp.RequestCancelledError("Request not supported yet")
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
            await self.pyre_manager.ensure_task_running()
            await self.pyre_query_manager.ensure_task_running()
            return await self.run_request_handler()
        except lsp.ReadChannelClosedError:
            # This error can happen when the connection gets closed unilaterally
            # from the language client, which causes issue when we try to access the
            # input channel. This usually signals that the language client has exited,
            # which implies that the language server should do that as well.
            LOG.info("Connection closed by LSP client.")
            return commands.ExitCode.SUCCESS
        finally:
            await self.pyre_manager.ensure_task_stop()
            await self.pyre_query_manager.ensure_task_stop()


async def run_persistent(
    server_start_options_reader: PyreDaemonStartOptionsReader,
    remote_logging: Optional[backend_arguments.RemoteLogging],
) -> int:
    stdin, stdout = await connections.create_async_stdin_stdout()
    while True:
        initialize_result = await try_initialize(
            stdin, stdout, server_start_options_reader
        )
        if isinstance(initialize_result, InitializationExit):
            LOG.info("Received exit request before initialization.")
            return 0
        elif isinstance(initialize_result, InitializationSuccess):
            LOG.info("Initialization successful.")
            client_info = initialize_result.client_info
            _log_lsp_event(
                remote_logging=remote_logging,
                event=LSPEvent.INITIALIZED,
                normals=(
                    {}
                    if client_info is None
                    else {
                        "lsp client name": client_info.name,
                        "lsp client version": client_info.version,
                    }
                ),
            )

            client_capabilities = initialize_result.client_capabilities
            LOG.debug(f"Client capabilities: {client_capabilities}")
            server_state = ServerState(client_capabilities=client_capabilities)
            server = PyreServer(
                input_channel=stdin,
                output_channel=stdout,
                server_state=server_state,
                pyre_manager=background.TaskManager(
                    PyreDaemonLaunchAndSubscribeHandler(
                        server_start_options_reader=server_start_options_reader,
                        remote_logging=remote_logging,
                        client_output_channel=stdout,
                        server_state=server_state,
                    )
                ),
                pyre_query_manager=background.TaskManager(
                    PyreQueryHandler(
                        query_state=server_state.query_state,
                        server_start_options_reader=server_start_options_reader,
                        client_output_channel=stdout,
                    )
                ),
            )
            return await server.run()
        elif isinstance(initialize_result, InitializationFailure):
            exception = initialize_result.exception
            message = (
                str(exception) if exception is not None else "ignoring notification"
            )
            LOG.info(f"Initialization failed: {message}")
            _log_lsp_event(
                remote_logging=remote_logging,
                event=LSPEvent.NOT_INITIALIZED,
                normals=(
                    {
                        "exception": message,
                    }
                ),
            )
            # Loop until we get either InitializeExit or InitializeSuccess
        else:
            raise RuntimeError("Cannot determine the type of initialize_result")


def run(
    read_server_start_options: PyreDaemonStartOptionsReader,
    remote_logging: Optional[backend_arguments.RemoteLogging],
) -> int:
    command_timer = timer.Timer()
    error_message: Optional[str] = None
    try:
        return asyncio.get_event_loop().run_until_complete(
            run_persistent(
                read_server_start_options,
                remote_logging,
            )
        )
    except Exception:
        error_message = traceback.format_exc()
        return 1
    finally:
        _log_lsp_event(
            remote_logging,
            LSPEvent.STOPPED,
            integers={"duration": int(command_timer.stop_in_millisecond())},
            normals={
                **({"exception": error_message} if error_message is not None else {})
            },
        )
