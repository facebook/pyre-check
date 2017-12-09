# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import copy
import logging
import os
import re
import sys
import threading
import time


LOG = logging.getLogger(__name__)


class Color:
    GREEN = '\033[33m'  # I'm colorblind, YMMV
    RED = '\033[31m'


class Format:
    BOLD = '\033[1m'
    CLEAR_LINE = '\x1b[0G\x1b[K'
    CLEAR = '\033[0m'
    CURSOR_UP_LINE = '\x1b[1A'


class Character:
    LAMBDA = 'ƛ'


class SectionFormatter(logging.Formatter):
    def __init__(self):
        super(SectionFormatter, self).__init__(
            '%(asctime)s %(levelname)s %(message)s')

    def format(self, record):
        formatted = super(SectionFormatter, self).format(record)
        return re.sub(r'DEBUG \[(.*)\]', r'\1', formatted)


class TimedStreamHandler(logging.StreamHandler):

    THRESHOLD = 0.5

    def __init__(self):
        super(TimedStreamHandler, self).__init__()
        self.setFormatter(logging.Formatter('%(message)s'))
        self.terminator = ""
        self.setLevel(logging.INFO)

        self._record = None
        self._active_lines = 0

        self._terminate = False
        self._thread = threading.Thread(target=self._thread)
        self._thread.daemon = True
        self._thread.start()

    def clear_lines(self):
        if self._active_lines == 0:
            return ''
        return Format.CLEAR_LINE + ''.join(
            [Format.CURSOR_UP_LINE + Format.CLEAR_LINE
                for n in range(self._active_lines - 1)])

    def emit(self, record, age=None):
        suffix = ''
        color = ''
        active_lines = record.msg.count('\n') + 1
        if record.levelname == 'ERROR':
            color = Color.RED
            record.msg += '\n'
            self._record = None
            active_lines = 0
        elif record.levelname == 'WARNING':
            color = Color.GREEN
            record.msg += '\n'
            self._record = None
            active_lines = 0
        elif age:
            if age > 10:
                color = Color.GREEN
            if age > 30:
                color = Color.RED
            suffix = ' {}[{:.1f}s]{}'.format(
                color if color else '',
                age,
                Format.CLEAR if color else '')
        else:
            self._record = record
            self._last_update = time.time()

        timed_record = copy.copy(record)
        timed_record.msg = (
            '{clear_line}{color} {cursor}{clear} '
            '{message}{suffix}').format(
                clear_line=self.clear_lines(),
                color=color,
                cursor=Character.LAMBDA,
                clear=Format.CLEAR,
                message=record.msg,
                suffix=suffix)
        self._active_lines = active_lines
        super(TimedStreamHandler, self).emit(timed_record)

    def _thread(self):
        while not self._terminate:
            if self._record:
                age = time.time() - self._last_update
                if age > self.THRESHOLD:
                    self.emit(self._record, age)
            time.sleep(0.1)

    def terminate(self):
        self._terminate = True


def initialize(arguments):
    # Hide cursor.
    sys.stderr.write('\x1b[?25l')

    if arguments.noninteractive:
        stream_handler = logging.StreamHandler()
        stream_handler.setFormatter(SectionFormatter())
        stream_handler.setLevel(logging.DEBUG)
        arguments.timed_stream_handler = None
    else:
        stream_handler = TimedStreamHandler()
        arguments.timed_stream_handler = stream_handler

    handlers = [stream_handler]

    if not arguments.noninteractive:
        try:
            os.mkdir('.pyre')
        except FileExistsError:
            pass

        file_handler = logging.FileHandler('.pyre/pyre.stderr')
        file_handler.setFormatter(SectionFormatter())
        file_handler.setLevel(logging.DEBUG)
        handlers.append(file_handler)

    logging.basicConfig(level=logging.DEBUG, handlers=handlers)


def cleanup(arguments):
    # Show cursor.
    sys.stderr.write("\x1b[?25h\n")

    if arguments.timed_stream_handler:
        arguments.timed_stream_handler.terminate()


class Buffer:

    THRESHOLD = 0.1

    def __init__(self, section, data):
        self._section = section
        self._data = data
        self._flushed = False
        self._lock = threading.RLock()
        thread = threading.Thread(target=self._thread)
        thread.daemon = True
        thread.start()

    def append(self, line):
        self._data.append(line)

    def flush(self):
        with self._lock:
            self._flushed = True
        message = '\n'.join(self._data)
        if self._section == 'ERROR':
            LOG.error(message)
        elif self._section == 'INFO':
            LOG.info(message)
        elif self._section == 'WARNING':
            LOG.warning(message)
        else:
            LOG.debug('[%s] %s', self._section, message)

    def _thread(self):
        time.sleep(self.THRESHOLD)
        with self._lock:
            if not self._flushed:
                self.flush()
