# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

class StreamingBuffer:
    def read(self) -> bytes: ...

def compress_string(s: bytes) -> bytes: ...
def slugify(s: str) -> str: ...
