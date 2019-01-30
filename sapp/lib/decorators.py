#!/usr/bin/env python3

from functools import wraps
from typing import Any, List, Optional


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
