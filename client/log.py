# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import argparse
import copy
import io
import logging
import os
import re
import sys
import threading
import time
from typing import List, Optional, Sequence  # noqa

from .filesystem import make_pyre_directory


LOG = logging.getLogger(__name__)  # type: logging.Logger
PERFORMANCE = 15  # type: int
PROMPT = 50  # type: int
SUCCESS = 60  # type: int


stdout = io.StringIO(newline="")  # type: io.StringIO


class Color:
    YELLOW = "\033[33m"  # type: str
    RED = "\033[31m"  # type: str
    GREEN = "\033[32m"  # type: str


class Format:
    BOLD = "\033[1m"  # type: str

    CLEAR_LINE = "\x1b[0G\x1b[K"  # type: str
    CLEAR = "\033[0m"  # type: str
    TRUNCATE_OVERFLOW = "\033[?7l"  # type: str
    WRAP_OVERFLOW = "\033[?7h"  # type: str
    NEWLINE = "\n"  # type: str

    CURSOR_UP_LINE = "\x1b[1A"  # type: str
    HIDE_CURSOR = "\x1b[?25l"  # type: str
    SHOW_CURSOR = "\x1b[?25h"  # type: str


class Character:
    LAMBDA = "ƛ"  # type: str


class SectionFormatter(logging.Formatter):
    def __init__(self) -> None:
        super(SectionFormatter, self).__init__("%(asctime)s %(levelname)s %(message)s")

    def format(self, record: logging.LogRecord) -> str:
        formatted = super(SectionFormatter, self).format(record)
        return re.sub(r"DEBUG \[(.*)\]", r"\1", formatted)


class TimedStreamHandler(logging.StreamHandler):
    THRESHOLD = 0.5  # type: float
    LINE_BREAKING_LEVELS = ["ERROR", "WARNING", "SUCCESS"]  # type: Sequence[str]

    _terminate = False  # type: bool
    _last_update = 0.0  # type: float

    def __init__(self) -> None:
        super(TimedStreamHandler, self).__init__()
        self.setFormatter(logging.Formatter("%(message)s"))
        self.terminator = ""  # type: str
        self.setLevel(logging.INFO)

        self._record = None  # type: Optional[logging.LogRecord]
        self._last_record = None  # type: Optional[logging.LogRecord]
        self._active_lines = 0  # type: int

        # Preamble preparing terminal.
        sys.stderr.write(
            Format.NEWLINE
            + Format.TRUNCATE_OVERFLOW
            + Format.CLEAR_LINE
            + Format.CURSOR_UP_LINE
            + Format.HIDE_CURSOR
        )

        thread = threading.Thread(target=self._thread)
        thread.daemon = True
        thread.start()

    def clear_lines(self) -> str:
        if self._active_lines == 0:
            return ""
        return Format.CLEAR_LINE + "".join(
            [
                Format.CURSOR_UP_LINE + Format.CLEAR_LINE
                for n in range(self._active_lines - 1)
            ]
        )

    def emit(self, record: logging.LogRecord, age: Optional[float] = None) -> None:
        self._last_record = record
        suffix = ""
        color = ""
        active_lines = record.msg.count("\n") + 1
        if record.levelname in self.LINE_BREAKING_LEVELS:
            record.msg += "\n"

        if record.levelname == "ERROR":
            color = Color.RED
            self._record = None
            active_lines = 0
        elif record.levelname == "WARNING":
            color = Color.YELLOW
            self._record = None
            active_lines = 0
        elif record.levelname == "PROMPT":
            color = Color.YELLOW
            self._record = None
            active_lines = 0
        elif record.levelname == "SUCCESS":
            self._record = None
            active_lines = 0
        elif age:
            if age > 10:
                color = Color.YELLOW
            if age > 30:
                color = Color.RED
            suffix = " {}[{:.1f}s]{}".format(
                color if color else "", age, Format.CLEAR if color else ""
            )
        else:
            self._record = record
            self._last_update = time.time()

        timed_record = copy.copy(record)
        timed_record.msg = (
            "{clear_line}{color} {cursor}{clear} " "{message}{suffix}"
        ).format(
            clear_line=self.clear_lines(),
            color=color,
            cursor=Character.LAMBDA,
            clear=Format.CLEAR,
            message=record.msg,
            suffix=suffix,
        )
        self._active_lines = active_lines
        super(TimedStreamHandler, self).emit(timed_record)

    def _thread(self) -> None:
        while not self._terminate:
            if self._record:
                age = time.time() - self._last_update
                if age > self.THRESHOLD:
                    self.emit(self._record, age)
            time.sleep(0.1)

    def terminate(self) -> None:
        last_record = self._last_record
        if last_record and last_record.levelname not in self.LINE_BREAKING_LEVELS:
            sys.stderr.write("\n")

        # Reset terminal.
        sys.stderr.write(Format.WRAP_OVERFLOW + Format.SHOW_CURSOR)
        sys.stderr.flush()
        self._terminate = True


def initialize(arguments: argparse.Namespace) -> None:
    if arguments.noninteractive:
        stream_handler = logging.StreamHandler()
        stream_handler.setFormatter(SectionFormatter())
        stream_handler.setLevel(logging.DEBUG)
        arguments.timed_stream_handler = None
    else:
        stream_handler = TimedStreamHandler()
        arguments.timed_stream_handler = stream_handler

    handlers = [stream_handler]  # type: List[logging.Handler]

    if not arguments.noninteractive:
        pyre_directory = make_pyre_directory()
        file_handler = logging.FileHandler(os.path.join(pyre_directory, "pyre.stderr"))
        file_handler.setFormatter(SectionFormatter())
        file_handler.setLevel(logging.DEBUG)
        handlers.append(file_handler)
    logging.addLevelName(PERFORMANCE, "PERFORMANCE")
    logging.addLevelName(PROMPT, "PROMPT")
    logging.addLevelName(SUCCESS, "SUCCESS")
    logging.basicConfig(level=logging.DEBUG, handlers=handlers)


def cleanup(arguments: argparse.Namespace) -> None:
    if arguments.timed_stream_handler:
        arguments.timed_stream_handler.terminate()

    output = stdout.getvalue()
    if output:
        sys.stdout.write(output + "\n")


class Buffer:
    THRESHOLD = 0.1  # type: float

    _flushed = False  # type: bool

    def __init__(self, section: str, data: List[str]) -> None:
        self._section = section  # type: str
        self._data = data  # type: List[str]
        self._lock = threading.RLock()  # type: threading.RLock
        thread = threading.Thread(target=self._thread)
        thread.daemon = True
        thread.start()

    def append(self, line: str) -> None:
        self._data.append(line)

    def flush(self) -> None:
        with self._lock:
            if self._flushed is True:
                return
            self._flushed = True
        message = "\n".join(self._data)
        if self._section == "ERROR":
            LOG.error(message)
        elif self._section == "INFO":
            LOG.info(message)
        elif self._section == "DUMP":
            LOG.warning(message)
        elif self._section == "WARNING":
            LOG.warning(message)
        elif self._section == "PROGRESS":
            LOG.info(message)
        elif self._section == "PARSER":
            LOG.error(message)
        else:
            LOG.debug("[%s] %s", self._section, message)

    def _thread(self) -> None:
        time.sleep(self.THRESHOLD)
        with self._lock:
            if not self._flushed:
                self.flush()


def get_yes_no_input(prompt: str) -> bool:
    choice = get_input(prompt, suffix=" [Y/n] ")
    return choice.lower() in ["", "y", "ye", "yes"]


def get_optional_input(prompt: str, default: str) -> str:
    result = get_input(prompt, suffix=" (Default: `{}`): ".format(default))
    if result == "":
        return default
    return result


def get_input(prompt: str, suffix: str = "") -> str:
    LOG.log(PROMPT, prompt + suffix)
    return input().strip()
