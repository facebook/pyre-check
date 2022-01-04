# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from typing import Any

NON_FIELD_ERRORS: str

class PermissionDenied(Exception):
    pass

class ViewDoesNotExist(Exception):
    pass

class MiddlewareNotUsed(Exception):
    pass

class ValidationError(Exception):
    def __init__(self, message, code=None, params=None): ...
    error_dict: Any
    error_list: Any
    def update_error_dict(self, error_dict: Any) -> Any: ...
    message: Any
    code: Any

class SuspiciousOperation(Exception):
    pass

class ObjectDoesNotExist(Exception):
    pass

class MultipleObjectsReturned(Exception):
    pass
