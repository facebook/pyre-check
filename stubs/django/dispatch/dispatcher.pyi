# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

from typing import (
    Any,
    Callable,
    Generator,
    Iterable,
    List,
    Optional,
    Tuple,
    TypeVar,
    Union,
)

_CallableT = TypeVar("_CallableT", bound=Callable)

class Signal:
    send: Any = ...
    receivers: Any
    sender_receivers_cache: Any
    def __init__(
        self, providing_args: Optional[Iterable[Any]] = None, use_caching: bool = False
    ) -> None: ...
    def has_listeners(self, sender: Optional[Any] = None) -> bool: ...
    async def async_send(
        self, sender: Any, **named: Any
    ) -> List[Tuple[Callable[..., Any], Callable[..., Any]]]: ...
    def send(
        self, sender: Any, **named: Any
    ) -> List[Tuple[Callable[..., Any], Callable[..., Any]]]: ...
    def connect(
        self,
        receiver: Callable[..., None],
        sender: Optional[Signal] = None,
        weak: bool = True,
        dispatch_uid: Optional[str] = None,
    ) -> None: ...
    def _live_receivers(self, sender: Optional[Any] = None) -> Any: ...

def receiver(
    signal: Union[Signal, List[Signal], Tuple[Signal, ...]], **kwargs: Any
) -> Callable[[_CallableT], _CallableT]: ...
