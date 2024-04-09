# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import enum
from typing import Optional

from .daemon_querier import DaemonQuerierSource


class PyreLanguageServerError(str, enum.Enum):
    COULD_NOT_ESTABLISH_CONNECTION = "COULD_NOT_ESTABLISH_CONNECTION"
    DAEMON_QUERY_FAILURE = "DAEMON_QUERY_FAILURE"
    DOCUMENT_PATH_MISSING_IN_SERVER_STATE = "DOCUMENT_PATH_MISSING_IN_SERVER_STATE"
    DOCUMENT_PATH_IS_NULL = "DOCUMENT_PATH_IS_NULL"
    PYRE_DAEMON_UNAVAILABLE_AND_FELLBACK_TO_GLEAN = (
        "PYRE_DAEMON_UNAVAILABLE_AND_FELLBACK_TO_GLEAN"
    )
    DID_NOT_FALLBACK_TO_GLEAN_AFTER_PYRE_FAILURE = (
        "DID_NOT_FALLBACK_TO_GLEAN_AFTER_PYRE_FAILURE"
    )
    PYRE_DAEMON_UNAVAILABLE_AND_FAILED_TO_FALLBACK_TO_GLEAN = (
        "PYRE_DAEMON_UNAVAILABLE_AND_FAILED_TO_FALLBACK_TO_GLEAN"
    )

    # Daemon Errors
    ATTRIBUTE_DEFINITION_NOT_FOUND = "ATTRIBUTE_DEFINITION_NOT_FOUND"
    CLIENT_ALREADY_REGISTERED = "CLIENT_ALREADY_REGISTERED"
    CLIENT_NOT_REGISTERED = "CLIENT_NOT_REGISTERED"
    FILE_NOT_OPENED = "FILE_NOT_OPENED"
    IDENTIFIER_DEFINITION_NOT_FOUND = "IDENTIFIER_DEFINITION_NOT_FOUND"
    MODULE_NOT_TRACKED = "MODULE_NOT_TRACKED"
    SOURCE_PATH_NOT_FOUND = "SOURCE_PATH_NOT_FOUND"
    SYMBOL_NOT_FOUND = "SYMBOL_NOT_FOUND"
    UNSUPPORTED_EXPRESSION = "UNSUPPORTED_EXPRESSION"
    GENERIC_DAEMON_QUERY_FAILURE = "GENERIC_DAEMON_QUERY_FAILURE"


# Capturing the most common errors returned from Daemon:
# https://fburl.com/scuba/vscode_chronicle_language_server_events/e6efk79s
def getLanguageServerError(
    error_message: Optional[str],
    is_empty_result: bool,
    empty_reason: Optional[object],
    query_source: Optional[DaemonQuerierSource],
) -> Optional[PyreLanguageServerError]:
    if error_message is not None:
        # By default, glean should handle result if pyre has an error
        if query_source != DaemonQuerierSource.GLEAN_INDEXER:
            return PyreLanguageServerError.DID_NOT_FALLBACK_TO_GLEAN_AFTER_PYRE_FAILURE
        # TOOD (T184657347): use predefined error and empty enum instead of performing a substring match
        if "Could not establish connection" in error_message:
            return PyreLanguageServerError.COULD_NOT_ESTABLISH_CONNECTION
        elif "ClientAlreadyRegistered" in error_message:
            return PyreLanguageServerError.CLIENT_ALREADY_REGISTERED
        elif "ClientNotRegistered" in error_message:
            return PyreLanguageServerError.CLIENT_NOT_REGISTERED
        elif "FileNotOpened" in error_message:
            return PyreLanguageServerError.FILE_NOT_OPENED
        elif "ModuleNotTracked" in error_message:
            return PyreLanguageServerError.MODULE_NOT_TRACKED
        # TODO (T184656101): Remove check for error message using empty reasons after 4/12
        elif "SourcePathNotFound" in error_message:
            return PyreLanguageServerError.SOURCE_PATH_NOT_FOUND
        elif "SymbolNotFound" in error_message:
            return PyreLanguageServerError.SYMBOL_NOT_FOUND
        elif "UnsupportedExpression" in error_message:
            return PyreLanguageServerError.UNSUPPORTED_EXPRESSION
        elif "AttributeDefinitionNotFound" in error_message:
            return PyreLanguageServerError.ATTRIBUTE_DEFINITION_NOT_FOUND
        elif "IdentifierDefinitionNotFound" in error_message:
            return PyreLanguageServerError.IDENTIFIER_DEFINITION_NOT_FOUND
        else:
            return PyreLanguageServerError.GENERIC_DAEMON_QUERY_FAILURE

    if not is_empty_result:
        return None

    # TOOD (T184657347): use predefined error and empty enum instead of performing a substring match
    if empty_reason is not None:
        empty_reason_str = str(empty_reason)
        if "SourcePathNotFound" in empty_reason_str:
            return PyreLanguageServerError.SOURCE_PATH_NOT_FOUND
        elif "SymbolNotFound" in empty_reason_str:
            return PyreLanguageServerError.SYMBOL_NOT_FOUND
        elif "UnsupportedExpression" in empty_reason_str:
            return PyreLanguageServerError.UNSUPPORTED_EXPRESSION
        elif "AttributeDefinitionNotFound" in empty_reason_str:
            return PyreLanguageServerError.ATTRIBUTE_DEFINITION_NOT_FOUND
        elif "IdentifierDefinitionNotFound" in empty_reason_str:
            return PyreLanguageServerError.IDENTIFIER_DEFINITION_NOT_FOUND

    # We assume that Pyre daemon always returns an empty reason or error when it returns an empty response
    # This case would only cover the case when pyre daemon is unavailable: https://fburl.com/code/a2ilz0iw
    elif query_source == DaemonQuerierSource.GLEAN_INDEXER:
        return PyreLanguageServerError.PYRE_DAEMON_UNAVAILABLE_AND_FELLBACK_TO_GLEAN
    else:
        return (
            PyreLanguageServerError.PYRE_DAEMON_UNAVAILABLE_AND_FAILED_TO_FALLBACK_TO_GLEAN
        )
