# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module defines a timer class we can use to track and report performance.
"""


import sys
import time
from typing import Callable, Optional


class Timer:
    """
    A simple utility class to facilitate clock duration computation.

    A ``Timer`` object tracks one single state: the timestamp at which the timer
    starts. It provides various ``stop`` methods which returns the duration from
    the moment of the stop method is invoked to the moment when the timer starts.

    The benefit of using this class over using the ``time`` module in the standard
    library is that this module provides a more readable interfaces with respect to
    the resolution of the duration.
    """

    get_current_time_in_nanosecond: Callable[[], int]
    start_time: int

    def __init__(
        self,
        get_current_time_in_nanosecond: Optional[Callable[[], int]] = None,
    ) -> None:
        """
        Initialize a Timer object. Starting time will be automatically reset to
        the time when this method is invoked.

        :param get_current_time_in_nanosecond:
            The timer object will invoke this function to obtain the current
            timestamp. By default, :func:`time.perf_counter_ns` is used, which
            provides a high-precision system-wide monotonic clock that is not
            affected by NTP adjustment. The default clock should be sufficient
            for wall-clock time tracking. For other purpose such as profiling,
            the recommendation is to use alternative clocks like
            :func:`time.process_time_ns` or :func:`time.thread_time_ns`.
        """
        if get_current_time_in_nanosecond is not None:
            self.get_current_time_in_nanosecond = get_current_time_in_nanosecond
        else:
            if (sys.version_info.major, sys.version_info.minor) >= (3, 7):
                self.get_current_time_in_nanosecond = time.perf_counter_ns
            else:
                self.get_current_time_in_nanosecond = lambda: int(
                    time.perf_counter() * 1e9
                )
        self.reset()

    def reset(self) -> None:
        """
        Set the starting time of the containing timer to the current time.
        """
        self.start_time = self.get_current_time_in_nanosecond()

    def stop_in_nanosecond(self) -> int:
        """
        Return how many nanoseconds have elapsed since the start time.
        """
        return self.get_current_time_in_nanosecond() - self.start_time

    def stop_in_microsecond(self) -> float:
        """
        Return how many microseconds have elapsed since the start time.
        """
        return float(self.stop_in_nanosecond()) / 1000

    def stop_in_millisecond(self) -> float:
        """
        Return how many milliseconds have elapsed since the start time.
        """
        return float(self.stop_in_nanosecond()) / 1000000

    def stop_in_second(self) -> float:
        """
        Return how many seconds have elapsed since the start time.
        """
        return float(self.stop_in_nanosecond()) / 1000000000
