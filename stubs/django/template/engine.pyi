from .base import Template

class Engine:
    def from_string(self, source: str) -> Template: ...
