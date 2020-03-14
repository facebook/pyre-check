# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import copy
import io
import logging
import os
import re
import sys
import threading
import time
from types import TracebackType
from typing import Iterable, List, Optional, Pattern, Sequence


PERFORMANCE: int = 15
PROMPT: int = 50
SUCCESS: int = 60


LOG: logging.Logger = logging.getLogger(__name__)


stdout: io.StringIO = io.StringIO(newline="")
__handler: Optional["TimedStreamHandler"] = None


class Color:
    YELLOW: str = "\033[33m"
    RED: str = "\033[31m"
    GREEN: str = "\033[32m"


class Format:
    BOLD: str = "\033[1m"

    CLEAR_LINE: str = "\x1b[0G\x1b[K"
    CLEAR: str = "\033[0m"
    TRUNCATE_OVERFLOW: str = "\033[?7l"
    WRAP_OVERFLOW: str = "\033[?7h"
    NEWLINE: str = "\n"

    CURSOR_UP_LINE: str = "\x1b[1A"
    HIDE_CURSOR: str = "\x1b[?25l"
    SHOW_CURSOR: str = "\x1b[?25h"


class Character:
    LAMBDA: str = "ƛ"


class SectionFormatter(logging.Formatter):
    def __init__(self) -> None:
        super(SectionFormatter, self).__init__("%(asctime)s %(levelname)s %(message)s")

    def format(self, record: logging.LogRecord) -> str:
        formatted = super(SectionFormatter, self).format(record)
        return re.sub(r"DEBUG \[(.*)\]", r"\1", formatted)


class TimedStreamHandler(logging.StreamHandler):
    THRESHOLD: float = 0.5
    LINE_BREAKING_LEVELS: Sequence[str] = ["ERROR", "WARNING", "SUCCESS"]

    _terminate: bool = False
    _last_update: float = 0.0

    def __init__(self) -> None:
        super(TimedStreamHandler, self).__init__()
        self.setFormatter(logging.Formatter("%(message)s"))
        self.terminator: str = ""
        self.setLevel(logging.INFO)

        self._record: Optional[logging.LogRecord] = None
        self._last_record: Optional[logging.LogRecord] = None
        self._active_lines: int = 0

        # Preamble preparing terminal.
        sys.stderr.write(
            Format.NEWLINE
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
            "{clear_line}{color} {cursor}{clear} " "{truncate}{message}{suffix}"
        ).format(
            clear_line=self.clear_lines(),
            color=color,
            cursor=Character.LAMBDA,
            clear=Format.CLEAR,
            truncate=Format.TRUNCATE_OVERFLOW,
            message=record.msg,
            suffix=suffix,
        )
        self._active_lines = active_lines
        super(TimedStreamHandler, self).emit(timed_record)

    def _thread(self) -> None:
        while not self._terminate:
            record = self._record
            if record:
                age = time.time() - self._last_update
                if age > self.THRESHOLD:
                    self.emit(record, age)
            time.sleep(0.1)

    def terminate(self) -> None:
        self._terminate = True

        if self._active_lines > 0:
            sys.stderr.write(self.clear_lines())
            self._active_lines = 0

        # Reset terminal.
        sys.stderr.write(Format.WRAP_OVERFLOW + Format.SHOW_CURSOR)
        sys.stderr.flush()


def initialize(
    noninteractive: bool,
    log_directory: str = "/tmp/.pyre",
    disable_file_logging: bool = False,
) -> None:
    global __handler
    if noninteractive:
        stream_handler = logging.StreamHandler()
        stream_handler.setFormatter(SectionFormatter())
        stream_handler.setLevel(logging.DEBUG)
        __handler = None
    else:
        stream_handler = TimedStreamHandler()
        __handler = stream_handler

    handlers: List[logging.Handler] = [stream_handler]

    if not noninteractive and not disable_file_logging:
        if not os.path.exists(log_directory):
            os.makedirs(log_directory)
        file_handler = logging.FileHandler(os.path.join(log_directory, "pyre.stderr"))
        file_handler.setFormatter(SectionFormatter())
        file_handler.setLevel(logging.DEBUG)
        handlers.append(file_handler)
    logging.addLevelName(PERFORMANCE, "PERFORMANCE")
    logging.addLevelName(PROMPT, "PROMPT")
    logging.addLevelName(SUCCESS, "SUCCESS")
    logging.basicConfig(level=logging.DEBUG, handlers=handlers)


def cleanup() -> None:
    handler = __handler
    if handler:
        handler.terminate()

    output = stdout.getvalue()
    if output:
        sys.stdout.write(output + "\n")


class StreamLogger:
    _should_stop_reading_stream = False

    _server_log_pattern: Pattern[str] = re.compile(
        r"\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2} (\w+) (.*)"
    )

    def __init__(self, stream: Iterable[str]) -> None:
        self._reader = threading.Thread(target=self._read_stream, args=(stream,))
        self._reader.daemon = True

    def join(self) -> None:
        self._reader.join()

    def _log_server_stderr_message(self, server_message: str) -> None:
        line = server_message.rstrip()
        match = self._server_log_pattern.match(line)
        if match:
            section = match.groups()[0]
            message = match.groups()[1]
            if section == "ERROR":
                LOG.error(message)
            elif section == "INFO":
                LOG.info(message)
            elif section == "DUMP":
                LOG.warning(message)
            elif section == "WARNING":
                LOG.warning(message)
            elif section == "PROGRESS":
                LOG.info(message)
            elif section == "PARSER":
                LOG.error(message)
            else:
                LOG.debug("[%s] %s", section, message)
        else:
            LOG.debug(line)

    def _read_stream(self, stream: Iterable[str]) -> None:
        try:
            for line in stream:
                if self._should_stop_reading_stream:
                    return
                self._log_server_stderr_message(line)
        except Exception:
            pass

    def __enter__(self) -> "StreamLogger":
        self._should_stop_reading_stream = False
        self._reader.start()
        return self

    def __exit__(
        self,
        _type: Optional[BaseException],
        _value: Optional[BaseException],
        _traceback: Optional[TracebackType],
    ) -> None:
        self._should_stop_reading_stream = True


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
