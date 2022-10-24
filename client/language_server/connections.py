# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
TODO(T132414938) Add a module-level docstring
"""


import abc
import asyncio
import contextlib
import io
import logging
import socket
import sys
from pathlib import Path
from typing import AsyncIterator, BinaryIO, Iterator, List, Optional, TextIO, Tuple

LOG: logging.Logger = logging.getLogger(__name__)


class ConnectionFailure(Exception):
    pass


@contextlib.contextmanager
def _connect_bytes(
    socket_path: Path,
) -> Iterator[Tuple[BinaryIO, BinaryIO]]:
    """
    Connect to the socket at given path. Once connected, create an input and
    an output stream from the socket. Both the input stream and the output
    stream are in raw binary mode: read/write APIs of the streams need to use
    `bytes` rather than `str`. The API is intended to be used like this:

    ```
    with connect(socket_path) as (input_stream, output_stream):
        # Read from input_stream and write into output_stream here
        ...
    ```

    Socket creation, connection, and closure will be automatically handled
    inside this context manager. If any of the socket operations fail, raise
    `ConnectionFailure`.
    """
    try:
        with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as client_socket:
            client_socket.connect(str(socket_path))
            with client_socket.makefile(
                mode="rb"
            ) as input_channel, client_socket.makefile(mode="wb") as output_channel:
                yield (input_channel, output_channel)
    except OSError as error:
        raise ConnectionFailure() from error


@contextlib.contextmanager
def connect(
    socket_path: Path,
) -> Iterator[Tuple[TextIO, TextIO]]:
    """
    This is a line-oriented higher-level API than `connect`. It can be used
    when the caller does not want to deal with the complexity of binary I/O.

    The behavior is the same as `connect`, except the streams that are created
    operates in text mode. Read/write APIs of the streams uses UTF-8 encoded
    `str` instead of `bytes`. Those operations are also line-buffered, meaning
    that the streams will automatically be flushed once the newline character
    is encountered.
    """
    with _connect_bytes(socket_path) as (input_channel, output_channel):
        yield (
            io.TextIOWrapper(
                input_channel,
                line_buffering=True,
                errors="replace",
            ),
            io.TextIOWrapper(
                output_channel,
                line_buffering=True,
                errors="replace",
            ),
        )


class AsyncBytesReader(abc.ABC):
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
        raise NotImplementedError()

    @abc.abstractmethod
    async def read_exactly(self, count: int) -> bytes:
        """
        Read exactly `count` bytes.
        If EOF is reached before the complete separator is found, raise
        `asyncio.IncompleteReadError`.
        """
        raise NotImplementedError()

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


class AsyncBytesWriter(abc.ABC):
    """
    This class defines the basic interface for async I/O output channel.
    """

    @abc.abstractmethod
    async def write(self, data: bytes) -> None:
        """
        The method attempts to write the data to the underlying channel and
        flushes immediately.
        """
        raise NotImplementedError()

    @abc.abstractmethod
    async def close(self) -> None:
        """
        The method closes the underlying channel and wait until the channel is
        fully closed.
        """
        raise NotImplementedError()


class AsyncTextReader:
    """
    An adapter for `AsyncBytesReader` that decodes everything it reads immediately
    from bytes to string. In other words, it tries to expose the same interfaces
    as `AsyncBytesReader` except it operates on strings rather than bytestrings.
    """

    bytes_reader: AsyncBytesReader
    encoding: str
    errors: str

    def __init__(
        self,
        bytes_reader: AsyncBytesReader,
        encoding: str = "utf-8",
        errors: str = "replace",
    ) -> None:
        self.bytes_reader = bytes_reader
        self.encoding = encoding
        self.errors = errors

    async def read_until(self, separator: str = "\n") -> str:
        separator_bytes = separator.encode(self.encoding)
        result_bytes = await self.bytes_reader.read_until(separator_bytes)
        return result_bytes.decode(self.encoding, errors=self.errors)

    async def read_exactly(self, count: int) -> str:
        result_bytes = await self.bytes_reader.read_exactly(count)
        return result_bytes.decode(self.encoding, errors=self.errors)

    async def readline(self) -> str:
        result_bytes = await self.bytes_reader.readline()
        return result_bytes.decode(self.encoding, errors=self.errors)


class AsyncTextWriter:
    """
    An adapter for `AsyncBytesWriter` that encodes everything it writes immediately
    from string to bytes. In other words, it tries to expose the same interfaces
    as `AsyncBytesWriter` except it operates on strings rather than bytestrings.
    """

    bytes_writer: AsyncBytesWriter
    encoding: str

    def __init__(self, bytes_writer: AsyncBytesWriter, encoding: str = "utf-8") -> None:
        self.bytes_writer = bytes_writer
        self.encoding = encoding

    async def write(self, data: str) -> None:
        data_bytes = data.encode(self.encoding)
        await self.bytes_writer.write(data_bytes)


class MemoryBytesReader(AsyncBytesReader):
    """
    An implementation of `AsyncBytesReader` based on a given in-memory byte string.
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


def create_memory_text_reader(data: str, encoding: str = "utf-8") -> AsyncTextReader:
    return AsyncTextReader(MemoryBytesReader(data.encode(encoding)), encoding)


class MemoryBytesWriter(AsyncBytesWriter):
    _items: List[bytes]

    def __init__(self) -> None:
        self._items = []

    async def write(self, data: bytes) -> None:
        self._items.append(data)

    async def close(self) -> None:
        pass

    def items(self) -> List[bytes]:
        return self._items


def create_memory_text_writer(encoding: str = "utf-8") -> AsyncTextWriter:
    return AsyncTextWriter(MemoryBytesWriter())


class StreamBytesReader(AsyncBytesReader):
    """
    An implementation of `AsyncBytesReader` based on `asyncio.StreamReader`.
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


class StreamBytesWriter(AsyncBytesWriter):
    """
    An implementation of `AsyncBytesWriter` based on `asyncio.StreamWriter`.
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


@contextlib.asynccontextmanager
async def _connect_async_bytes(
    socket_path: Path, buffer_size: Optional[int] = None
) -> AsyncIterator[Tuple[AsyncBytesReader, AsyncBytesWriter]]:
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
    writer: Optional[AsyncBytesWriter] = None
    try:
        limit = buffer_size if buffer_size is not None else 2**16
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


@contextlib.asynccontextmanager
async def connect_async(
    socket_path: Path, buffer_size: Optional[int] = None
) -> AsyncIterator[Tuple[AsyncTextReader, AsyncTextWriter]]:
    """
    This is a line-oriented higher-level API than `connect`. It can be used
    when the caller does not want to deal with the complexity of binary I/O.

    The behavior is the same as `connect`, except the streams that are created
    operates in text mode. Read/write APIs of the streams uses UTF-8 encoded
    `str` instead of `bytes`.
    """
    async with _connect_async_bytes(socket_path, buffer_size) as (
        bytes_reader,
        bytes_writer,
    ):
        yield (
            AsyncTextReader(bytes_reader, encoding="utf-8"),
            AsyncTextWriter(bytes_writer, encoding="utf-8"),
        )


async def create_async_stdin_stdout() -> Tuple[AsyncTextReader, AsyncTextWriter]:
    """
    By default, `sys.stdin` and `sys.stdout` are synchronous channels: reading
    from `sys.stdin` or writing to `sys.stdout` will block until the read/write
    succeed, which is very different from the async socket channels created via
    `connect_async`.

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
        AsyncTextReader(StreamBytesReader(stream_reader)),
        AsyncTextWriter(StreamBytesWriter(stream_writer)),
    )
