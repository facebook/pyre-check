from builtins import __test_sink, __test_source
from typing import Any, Dict, List, Optional, Tuple


class C:
    def __init__(
        self, id: int, params: Dict[str, Any], kind: str, request: str
    ) -> None:
        self.id = id
        self.timestamp = params.get("timestamp") or 0
        self.app_id = params.get("app_id")
        self.kind = kind
        self.request = request

    @classmethod
    async def async_create(
        cls, id: int, params: Dict, request: Optional[str] = None
    ) -> "C":
        kind = str(params)
        if kind == "special_kind":
            request = "get_current_request()"
        else:
            if not request:
                request = str(params)

        return cls(id, params, kind, request)


def test():
    obj = C.async_create(1, {__test_source(): __test_source()}, "")
    __test_sink(obj.id)
