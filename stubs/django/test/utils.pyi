# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from typing import Any, ContextManager

class override_settings(ContextManager[None]):
    def __call__(self, test_func: Any) -> Any: ...
    def __enter__(self) -> None: ...
    def __exit__(self, exc_type: Any, exc_value: Any, traceback: Any) -> None: ...
