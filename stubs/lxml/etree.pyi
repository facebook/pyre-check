from typing import Any

# Lazy hack to not have to stub those out.
from lxml.unstubbed import Element, SubElement, _Element, parse

def fromstring(input: str) -> Any: ...
def tostring(input: Any) -> str: ...
