import os
import typing as t

from aiofiles.base import AsyncBase

PathLike = t.Union[str, bytes, os.PathLike]

def open(
    file: t.Union[PathLike, int],
    mode: str = ...,
    buffering: int = ...,
    encoding: t.Optional[str] = ...,
    errors: t.Optional[str] = ...,
    newline: t.Optional[str] = ...,
    closefd: bool = ...,
    opener: t.Optional[t.Callable[[str, int], int]] = ...,
    *,
    loop: t.Optional[t.Any] = ...,
    executor: t.Optional[t.Any] = ...,
) -> t.AsyncContextManager[AsyncBase]: ...
def wrap(file, *, loop=None, executor=None) -> AsyncBase: ...
