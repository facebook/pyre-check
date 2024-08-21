# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import enum
from typing import Optional

from .daemon_querier import DaemonQuerierSource


class PyreLanguageServerError(str, enum.Enum):
    # Client Errors
    COULD_NOT_ESTABLISH_CONNECTION = "COULD_NOT_ESTABLISH_CONNECTION"
    DAEMON_QUERY_FAILURE = "DAEMON_QUERY_FAILURE"
    DOCUMENT_PATH_MISSING_IN_SERVER_STATE = "DOCUMENT_PATH_MISSING_IN_SERVER_STATE"
    DOCUMENT_PATH_IS_NULL = "DOCUMENT_PATH_IS_NULL"
    DID_NOT_FALLBACK_TO_GLEAN_AFTER_PYRE_FAILURE = (
        "DID_NOT_FALLBACK_TO_GLEAN_AFTER_PYRE_FAILURE"
    )

    # Daemon Errors
    CLIENT_ALREADY_REGISTERED = "CLIENT_ALREADY_REGISTERED"
    CLIENT_NOT_REGISTERED = "CLIENT_NOT_REGISTERED"
    FILE_NOT_OPENED = "FILE_NOT_OPENED"
    MODULE_NOT_TRACKED = "MODULE_NOT_TRACKED"
    GENERIC_DAEMON_QUERY_FAILURE = "GENERIC_DAEMON_QUERY_FAILURE"


class PyreLanguageServerEmptyReason(str, enum.Enum):
    PYRE_DAEMON_UNAVAILABLE_AND_FELLBACK_TO_GLEAN = (
        "PYRE_DAEMON_UNAVAILABLE_AND_FELLBACK_TO_GLEAN"
    )
    PYRE_DAEMON_UNAVAILABLE_AND_FAILED_TO_FALLBACK_TO_GLEAN = (
        "PYRE_DAEMON_UNAVAILABLE_AND_FAILED_TO_FALLBACK_TO_GLEAN"
    )
    PYRE_DAEMON_ERROR_AND_FELLBACK_TO_GLEAN = "PYRE_DAEMON_ERROR_AND_FELLBACK_TO_GLEAN"
    PYRE_DAEMON_ERROR_AND_FAILED_TO_FALLBACK_TO_GLEAN = (
        "PYRE_DAEMON_ERROR_AND_FAILED_TO_FALLBACK_TO_GLEAN"
    )


# Capturing the most common errors returned from Daemon:
# https://fburl.com/scuba/vscode_chronicle_language_server_events/e6efk79s
def get_language_server_error(
    error_message: Optional[str],
    query_source: Optional[DaemonQuerierSource],
) -> Optional[PyreLanguageServerError]:
    if error_message is None:
        return None

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


def get_language_server_empty_reason(
    empty_reason: Optional[object],
    error_message: Optional[str],
    query_source: Optional[DaemonQuerierSource],
) -> object:
    if empty_reason is not None:
        return empty_reason

    if error_message is not None:
        if query_source == DaemonQuerierSource.GLEAN_INDEXER:
            return PyreLanguageServerEmptyReason.PYRE_DAEMON_ERROR_AND_FELLBACK_TO_GLEAN
        else:
            return PyreLanguageServerEmptyReason.PYRE_DAEMON_ERROR_AND_FAILED_TO_FALLBACK_TO_GLEAN
    else:
        # We assume that Pyre daemon always returns an empty reason or error when it returns an empty response, this means that
        # if Pyre daemon doesn't return an empty, then it means it's unavailable: https://fburl.com/code/a2ilz0iw
        if query_source == DaemonQuerierSource.GLEAN_INDEXER:
            return PyreLanguageServerEmptyReason.PYRE_DAEMON_UNAVAILABLE_AND_FELLBACK_TO_GLEAN
        else:
            return PyreLanguageServerEmptyReason.PYRE_DAEMON_UNAVAILABLE_AND_FAILED_TO_FALLBACK_TO_GLEAN
