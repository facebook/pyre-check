# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import asyncio

import testslide

from .. import background_tasks

from ..tests import setup


class WaitForeverTask(background_tasks.Task):
    async def run(self) -> None:
        await asyncio.Event().wait()


class WaitForEventTask(background_tasks.Task):
    event: asyncio.Event

    def __init__(self) -> None:
        self.event = asyncio.Event()

    async def run(self) -> None:
        # Wait until the event is set. Then quit.
        await self.event.wait()


class TaskTest(testslide.TestCase):
    @setup.async_test
    async def test_background_task_manager(self) -> None:
        task = WaitForEventTask()
        manager = background_tasks.TaskManager(task)
        self.assertFalse(manager.is_task_running())

        await manager.ensure_task_running()
        # Yield control to the event loop, allowing the background task to start
        await asyncio.sleep(0)
        self.assertTrue(manager.is_task_running())

        # Terminate the server handler by setting the event
        task.event.set()
        # Yield control to the event loop, allowing the server handler to stop
        await asyncio.sleep(0)
        self.assertFalse(manager.is_task_running())

        # Restart the background task
        task.event.clear()
        await manager.ensure_task_running()
        # Yield control to the event loop, allowing the background task to start
        await asyncio.sleep(0)
        self.assertTrue(manager.is_task_running())

        await manager.ensure_task_stop()
        self.assertFalse(manager.is_task_running())

    @setup.async_test
    async def test_background_task_manager_shutdown_before_start(self) -> None:
        manager = background_tasks.TaskManager(WaitForeverTask())
        self.assertFalse(manager.is_task_running())

        await manager.ensure_task_running()
        await manager.ensure_task_stop()
        self.assertFalse(manager.is_task_running())
