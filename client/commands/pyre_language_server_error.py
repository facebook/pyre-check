# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import enum
from typing import Optional


class PyreLanguageServerError(str, enum.Enum):
    COULD_NOT_ESTABLISH_CONNECTION = "COULD_NOT_ESTABLISH_CONNECTION"
    DAEMON_QUERY_FAILURE = "DAEMON_QUERY_FAILURE"
    DOCUMENT_PATH_MISSING_IN_SERVER_STATE = "DOCUMENT_PATH_MISSING_IN_SERVER_STATE"
    DOCUMENT_PATH_IS_NULL = "DOCUMENT_PATH_IS_NULL"

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
def getLanguageServerErrorFromDaemonError(
    error: Optional[str],
) -> Optional[PyreLanguageServerError]:
    if error is None:
        return None
    elif "Could not establish connection" in error:
        return PyreLanguageServerError.COULD_NOT_ESTABLISH_CONNECTION
    elif "ClientAlreadyRegistered" in error:
        return PyreLanguageServerError.CLIENT_ALREADY_REGISTERED
    elif "ClientNotRegistered" in error:
        return PyreLanguageServerError.CLIENT_NOT_REGISTERED
    elif "FileNotOpened":
        return PyreLanguageServerError.FILE_NOT_OPENED
    elif "ModuleNotTracked" in error:
        return PyreLanguageServerError.MODULE_NOT_TRACKED
    elif "SourcePathNotFound" in error:
        return PyreLanguageServerError.SOURCE_PATH_NOT_FOUND
    elif "SymbolNotFound" in error:
        return PyreLanguageServerError.SYMBOL_NOT_FOUND
    elif "UnsupportedExpression" in error:
        return PyreLanguageServerError.UNSUPPORTED_EXPRESSION
    elif "AttributeDefinitionNotFound" in error:
        return PyreLanguageServerError.ATTRIBUTE_DEFINITION_NOT_FOUND
    elif "IdentifierDefinitionNotFound" in error:
        return PyreLanguageServerError.IDENTIFIER_DEFINITION_NOT_FOUND
    else:
        return PyreLanguageServerError.GENERIC_DAEMON_QUERY_FAILURE
