# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module defines a background task base class, that can be used
to start an async background task (which is basically a forked async
green thread, which exposes hooks to start, stop, and query whether
it is running but is otherwise opaque to the rest of the program).
"""


import abc
import asyncio
import logging
from typing import Optional

LOG: logging.Logger = logging.getLogger(__name__)


class Task(abc.ABC):
    @abc.abstractmethod
    async def run(self) -> None:
        raise NotImplementedError()


class TaskManager:
    """
    This class manages the lifetime of a given background task.

    It maintains one piece of internal state: the existence of an ongoing
    task, represented as an attribute of type `Optional[Future]`. When the
    attribute is not `None`, it means that the task is actively running in the
    background.
    """

    _task: Task
    _ongoing: "Optional[asyncio.Future[None]]"

    def __init__(self, task: Task) -> None:
        """
        Initialize a background task manager. The `task` parameter is expected
        to be a coroutine which will be executed when `ensure_task_running()`
        method is invoked.

        It is expected that the provided task does not internally swallow asyncio
        `CancelledError`. Otherwise, task shutdown may not work properly.
        """
        self._task = task
        self._ongoing = None

    async def _run_task(self) -> None:
        try:
            await self._task.run()
        except asyncio.CancelledError:
            LOG.info("Terminate background task on explicit cancelling request.")
        except Exception:
            LOG.exception("Background task unexpectedly quit")
        finally:
            self._ongoing = None

    def is_task_running(self) -> bool:
        return self._ongoing is not None

    async def ensure_task_running(self) -> None:
        """
        If the background task is not currently running, schedule it to run
        in the future by adding the task to the event loop. Note that the
        scheduled task won't get a chance to execute unless control is somehow
        yield to the event loop from the current task (e.g. via an `await` on
        something).
        """
        if self._ongoing is None:
            self._ongoing = asyncio.create_task(self._run_task())

    async def ensure_task_stop(self) -> None:
        """
        If the background task is running actively, make sure it gets stopped.
        """
        ongoing = self._ongoing
        if ongoing is not None:
            try:
                ongoing.cancel()
                await ongoing
            except asyncio.CancelledError:
                # This catch is needed when `ongoing.cancel` is called before
                # `_run_task` gets a chance to execute.
                LOG.info("Terminate background task on explicit cancelling request.")
            finally:
                self._ongoing = None
