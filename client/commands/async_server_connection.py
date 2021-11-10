# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import abc
import asyncio
import logging
import sys
from pathlib import Path
from typing import AsyncIterator, Tuple, Optional, List

if sys.version_info >= (3, 7):
    from contextlib import asynccontextmanager
else:
    from async_generator import asynccontextmanager

LOG: logging.Logger = logging.getLogger(__name__)


class ConnectionFailure(Exception):
    pass


class BytesReader(abc.ABC):
    """
    This class defines the basic interface for async I/O input channel.
    """

    @abc.abstractmethod
    async def read_until(self, separator: bytes = b"\n") -> bytes:
        """
        Read data from the stream until `separator` is found.
        If EOF is reached before the complete separator is found, raise
        `asyncio.IncompleteReadError`.
        """
        raise NotImplementedError

    @abc.abstractmethod
    async def read_exactly(self, count: int) -> bytes:
        """
        Read exactly `count` bytes.
        If EOF is reached before the complete separator is found, raise
        `asyncio.IncompleteReadError`.
        """
        raise NotImplementedError

    async def readline(self) -> bytes:
        """
        Read one line, where "line" is a sequence of bytes ending with '\n'.
        If EOF is received and '\n' was not found, the method returns partially
        read data.
        """
        try:
            return await self.read_until(b"\n")
        except asyncio.IncompleteReadError as error:
            return error.partial


class BytesWriter(abc.ABC):
    """
    This class defines the basic interface for async I/O output channel.
    """

    @abc.abstractmethod
    async def write(self, data: bytes) -> None:
        """
        The method attempts to write the data to the underlying channel and
        flushes immediately.
        """
        raise NotImplementedError

    @abc.abstractmethod
    async def close(self) -> None:
        """
        The method closes the underlying channel and wait until the channel is
        fully closed.
        """
        raise NotImplementedError


class TextReader:
    """
    An adapter for `BytesReader` that decodes everything it reads immediately
    from bytes to string. In other words, it tries to expose the same interfaces
    as `BytesReader` except it operates on strings rather than bytestrings.
    """

    bytes_reader: BytesReader
    encoding: str

    def __init__(self, bytes_reader: BytesReader, encoding: str = "utf-8") -> None:
        self.bytes_reader = bytes_reader
        self.encoding = encoding

    async def read_until(self, separator: str = "\n") -> str:
        separator_bytes = separator.encode(self.encoding)
        result_bytes = await self.bytes_reader.read_until(separator_bytes)
        return result_bytes.decode(self.encoding)

    async def read_exactly(self, count: int) -> str:
        result_bytes = await self.bytes_reader.read_exactly(count)
        return result_bytes.decode(self.encoding)

    async def readline(self) -> str:
        result_bytes = await self.bytes_reader.readline()
        return result_bytes.decode(self.encoding)


class TextWriter:
    """
    An adapter for `BytesWriter` that encodes everything it writes immediately
    from string to bytes. In other words, it tries to expose the same interfaces
    as `BytesWriter` except it operates on strings rather than bytestrings.
    """

    bytes_writer: BytesWriter
    encoding: str

    def __init__(self, bytes_writer: BytesWriter, encoding: str = "utf-8") -> None:
        self.bytes_writer = bytes_writer
        self.encoding = encoding

    async def write(self, data: str) -> None:
        data_bytes = data.encode(self.encoding)
        await self.bytes_writer.write(data_bytes)


class MemoryBytesReader(BytesReader):
    """
    An implementation of `BytesReader` based on a given in-memory byte string.
    """

    _data: bytes
    _cursor: int

    def __init__(self, data: bytes) -> None:
        self._data = data
        self._cursor = 0

    async def read_until(self, separator: bytes = b"\n") -> bytes:
        result = bytearray()
        start_index = self._cursor
        end_index = len(self._data)
        for index in range(start_index, end_index):
            byte = self._data[index]
            result.append(byte)
            if result.endswith(separator):
                self._cursor = index + 1
                return bytes(result)

        self._cursor = end_index
        raise asyncio.IncompleteReadError(bytes(result), None)

    async def read_exactly(self, count: int) -> bytes:
        old_cursor = self._cursor
        new_cursor = self._cursor + count
        data_size = len(self._data)
        if new_cursor <= data_size:
            self._cursor = new_cursor
            return self._data[old_cursor:new_cursor]
        else:
            self._cursor = data_size
            raise asyncio.IncompleteReadError(self._data[old_cursor:], count)

    def reset(self) -> None:
        self._cursor = 0


def create_memory_text_reader(data: str, encoding: str = "utf-8") -> TextReader:
    return TextReader(MemoryBytesReader(data.encode(encoding)), encoding)


class MemoryBytesWriter(BytesWriter):
    _items: List[bytes]

    def __init__(self) -> None:
        self._items = []

    async def write(self, data: bytes) -> None:
        self._items.append(data)

    async def close(self) -> None:
        pass

    def items(self) -> List[bytes]:
        return self._items


def create_memory_text_writer(encoding: str = "utf-8") -> TextWriter:
    return TextWriter(MemoryBytesWriter())


class StreamBytesReader(BytesReader):
    """
    An implementation of `BytesReader` based on `asyncio.StreamReader`.
    """

    stream_reader: asyncio.StreamReader

    def __init__(self, stream_reader: asyncio.StreamReader) -> None:
        self.stream_reader = stream_reader

    async def read_until(self, separator: bytes = b"\n") -> bytes:
        # StreamReader.readuntil() may raise when its internal buffer cannot hold
        # all the input data. We need to explicitly handle the raised exceptions
        # by "parking" all partial-read results in memory.
        chunks = []
        while True:
            try:
                chunk = await self.stream_reader.readuntil(separator)
                chunks.append(chunk)
                break
            except asyncio.LimitOverrunError as error:
                chunk = await self.stream_reader.readexactly(error.consumed)
                chunks.append(chunk)
        return b"".join(chunks)

    async def read_exactly(self, count: int) -> bytes:
        return await self.stream_reader.readexactly(count)


class StreamBytesWriter(BytesWriter):
    """
    An implementation of `BytesWriter` based on `asyncio.StreamWriter`.
    """

    stream_writer: asyncio.StreamWriter

    def __init__(self, stream_writer: asyncio.StreamWriter) -> None:
        self.stream_writer = stream_writer

    async def write(self, data: bytes) -> None:
        self.stream_writer.write(data)
        await self.stream_writer.drain()

    async def close(self) -> None:
        self.stream_writer.close()
        await self._stream_writer_wait_closed()

    async def _stream_writer_wait_closed(self) -> None:
        """
        StreamWriter does not have a `wait_closed` method prior to python
        3.7. For 3.6 compatibility we have to hack it with an async busy
        loop that waits
        - first for the transport to be aware that it is closing
        - then for the socket to become unmapped

        This approach is inspired by the solution in qemu.aqmp.util.
        """
        if sys.version_info >= (3, 7):
            return await self.stream_writer.wait_closed()

        while not self.stream_writer.transport.is_closing():
            await asyncio.sleep(0)

        transport_socket: sys.IO = self.stream_writer.transport.get_extra_info("socket")
        if transport_socket is not None:
            while transport_socket.fileno() != -1:
                await asyncio.sleep(0)


@asynccontextmanager
async def connect(
    socket_path: Path, buffer_size: Optional[int] = None
) -> AsyncIterator[Tuple[BytesReader, BytesWriter]]:
    """
    Connect to the socket at given path. Once connected, create an input and
    an output stream from the socket. Both the input stream and the output
    stream are in raw binary mode. The API is intended to be used like this:

    ```
    async with connect(socket_path) as (input_stream, output_stream):
        # Read from input_stream and write into output_stream here
        ...
    ```

    The optional `buffer_size` argument determines the size of the input buffer
    used by the returned reader instance. If not specified, a default value of
    64kb will be used.

    Socket creation, connection, and closure will be automatically handled
    inside this context manager. If any of the socket operations fail, raise
    `ConnectionFailure`.
    """
    writer: Optional[BytesWriter] = None
    try:
        limit = buffer_size if buffer_size is not None else 2 ** 16
        stream_reader, stream_writer = await asyncio.open_unix_connection(
            str(socket_path), limit=limit
        )
        reader = StreamBytesReader(stream_reader)
        writer = StreamBytesWriter(stream_writer)
        yield reader, writer
    except OSError as error:
        raise ConnectionFailure() from error
    finally:
        if writer is not None:
            await writer.close()


@asynccontextmanager
async def connect_in_text_mode(
    socket_path: Path, buffer_size: Optional[int] = None
) -> AsyncIterator[Tuple[TextReader, TextWriter]]:
    """
    This is a line-oriented higher-level API than `connect`. It can be used
    when the caller does not want to deal with the complexity of binary I/O.

    The behavior is the same as `connect`, except the streams that are created
    operates in text mode. Read/write APIs of the streams uses UTF-8 encoded
    `str` instead of `bytes`.
    """
    async with connect(socket_path, buffer_size) as (bytes_reader, bytes_writer):
        yield (
            TextReader(bytes_reader, encoding="utf-8"),
            TextWriter(bytes_writer, encoding="utf-8"),
        )


async def create_async_stdin_stdout() -> Tuple[TextReader, TextWriter]:
    """
    By default, `sys.stdin` and `sys.stdout` are synchronous channels: reading
    from `sys.stdin` or writing to `sys.stdout` will block until the read/write
    succeed, which is very different from the async socket channels created via
    `connect` or `connect_in_text_mode`.

    This function creates wrappers around `sys.stdin` and `sys.stdout` and makes
    them behave in the same way as other async socket channels. This makes it
    easier to write low-level-I/O-agonstic code, where the high-level logic does
    not need to worry about whether the underlying async I/O channel comes from
    sockets or from stdin/stdout.
    """
    loop = asyncio.get_event_loop()
    stream_reader = asyncio.StreamReader(loop=loop)
    await loop.connect_read_pipe(
        lambda: asyncio.StreamReaderProtocol(stream_reader), sys.stdin
    )
    w_transport, w_protocol = await loop.connect_write_pipe(
        asyncio.streams.FlowControlMixin, sys.stdout
    )
    stream_writer = asyncio.StreamWriter(w_transport, w_protocol, stream_reader, loop)
    return (
        TextReader(StreamBytesReader(stream_reader)),
        TextWriter(StreamBytesWriter(stream_writer)),
    )


class BackgroundTask(abc.ABC):
    @abc.abstractmethod
    async def run(self) -> None:
        raise NotImplementedError


class BackgroundTaskManager:
    """
    This class manages the lifetime of a given background task.

    It maintains one piece of internal state: the existence of an ongoing
    task, represented as an attribute of type `Optional[Future]`. When the
    attribute is not `None`, it means that the task is actively running in the
    background.
    """

    _task: BackgroundTask
    _ongoing: "Optional[asyncio.Future[None]]"

    def __init__(self, task: BackgroundTask) -> None:
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
        except Exception as error:
            LOG.error(f"Background task unexpectedly quited: {error}")
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
            self._ongoing = asyncio.ensure_future(self._run_task())

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
