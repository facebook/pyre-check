from typing import Any, List, Optional

# Lazy hack to not have to stub those out.
from lxml.unstubbed import Element, SubElement, _Element

def parse(source, parser=..., base_url=...): ...
def fromstring(input: str) -> Any: ...
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
