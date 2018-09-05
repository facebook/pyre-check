from typing import Optional


class GEOSBase:
    pass


class ListMixin:
    pass


class GEOSGeometry(GEOSBase, ListMixin):
    @property
    def empty() -> bool:
        ...


class Point(GEOSGeometry):
    @property
    def x() -> int:
        ...

    @property
    def y() -> int:
        ...

    @property
    def z() -> Optional[int]:
        ...


class Polygon(GEOSGeometry):
    pass
