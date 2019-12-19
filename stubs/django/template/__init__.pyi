from typing import Any, Mapping

class TemplateSyntaxError(Exception):
    pass

class Template: ...

class Context:
    def __init__(self, dict_: Mapping[str, Any]) -> None: ...

class Engine:
    def from_string(self, template_code: str) -> Engine: ...
