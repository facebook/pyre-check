# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

import abc
from typing import Generic, List, Type, TypeVar


EPInputType = TypeVar("EPInputType")
EPOutputType = TypeVar("EPOutputType")


class AbstractEventProcessor(Generic[EPInputType, EPOutputType]):
    def __init__(self, tainted: str, benign: str) -> None:
        self.tainted = tainted
        self.benign = benign

    async def async_run(self) -> None:
        pass

    async def async_call_tainted(self) -> None:
        pass


class ConcreteEventProcessor(AbstractEventProcessor[EPInputType, EPOutputType]):
    async def async_run(self) -> None:
        _test_sink(self.benign)

    async def async_call_tainted(self) -> None:
        _test_sink(self.tainted)


PIInputType = TypeVar("PIInputType")
PIOutputType = TypeVar("PIOutputType")


class ProcessorInfo(Generic[PIInputType, PIOutputType]):
    def __init__(
        self,
        processor_type: Type[AbstractEventProcessor[PIInputType, PIOutputType]],
    ) -> None:
        self.processor_type: Type[
            AbstractEventProcessor[PIInputType, PIOutputType]
        ] = processor_type


def get_event_processors() -> List[ProcessorInfo[int, int]]:
    ...


async def async_execute_event_processor() -> None:
    for p_info in get_event_processors():
        processor = p_info.processor_type(_test_source(), "benign")
        await processor.async_run()
        await processor.async_call_tainted()
