# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Any, List, Optional

# Lazy hack to not have to stub those out.
from lxml.unstubbed import (
    Element as Element,
    SubElement as SubElement,
    _Element as _Element,
)

def parse(source, parser=..., base_url=...): ...
def fromstring(input: str, parser=..., *, base_url=...) -> Any: ...
def fromstringlist(strings: List[str], parser=...) -> Any: ...
def tostring(
    element_or_tree: Any,
    encoding: Optional[str] = ...,
    method: str = ...,
    xml_declaration: Optional[bool] = ...,
    pretty_print: bool = ...,
    with_tail: bool = ...,
    standalone: Optional[bool] = ...,
    doctype: Optional[str] = ...,
    exclusive: bool = ...,
    inclusive_ns_prefixes: Optional[List[str]] = ...,
    with_comments: bool = ...,
    strip_text: bool = ...,
) -> bytes: ...
