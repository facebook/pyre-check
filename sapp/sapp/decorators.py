#!/usr/bin/env python3

import datetime
import gc
import logging
import sys
import time
from contextlib import contextmanager
from functools import wraps
from typing import Any, Callable, List, Optional


log = logging.getLogger("sapp")


class retryable(object):
    def __init__(
        self, num_tries: int = 1, retryable_exs: Optional[List[Any]] = None
    ) -> None:
        self.num_tries = num_tries
        self.retryable_exs = retryable_exs

    def __call__(self, func):
        @wraps(func)
        def new_func(*args, **kwargs):
            try_num = 1
            while True:
                try:
                    return func(*args, **kwargs)
                except Exception as e:
                    if self.retryable_exs and type(e) not in self.retryable_exs:
                        raise
                    try_num += 1
                    if try_num > self.num_tries:
                        raise

        new_func.__wrapped__ = func
        return new_func


def log_time(func: Callable[..., Any]) -> Callable[..., Any]:
    """Log the time it takes to run a function. It's sort of like timeit, but
    prettier.
    """

    def wrapper(*args, **kwargs):
        start_time = time.time()
        log.info("%s starting...", func.__name__.title())
        ret = func(*args, **kwargs)
        log.info(
            "%s finished (%s)",
            func.__name__.title(),
            datetime.timedelta(seconds=int(time.time() - start_time)),
        )
        return ret

    return wrapper


class UserError(Exception):
    pass


@contextmanager
def catch_user_error():
    try:
        yield
    except UserError as error:
        print(str(error), file=sys.stderr)


@contextmanager
def catch_keyboard_interrupt():
    try:
        yield
    except KeyboardInterrupt:
        print("\nOperation aborted.", file=sys.stderr)
