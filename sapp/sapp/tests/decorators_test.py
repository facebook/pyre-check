#!/usr/bin/env python3

import gc
from unittest import TestCase, mock

from sapp.decorators import (
    UserError,
    catch_keyboard_interrupt,
    catch_user_error,
    disable_gc,
    log_time,
    retryable,
)


class RetryableTest(TestCase):
    def setUp(self) -> None:
        self.times_through = 0

    @retryable(num_tries=5)
    def semiRaiseException(self):
        self.times_through += 1
        if self.times_through < 3:
            raise Exception("You are killing me.")
        else:
            return True

    @retryable(num_tries=5, retryable_exs=[ImportError])
    def raiseRetryableException(self):
        self.times_through += 1
        if self.times_through < 3:
            raise ImportError
        else:
            raise Exception("You are killing me.")

    def testRetries(self):
        self.assertTrue(self.semiRaiseException())
        self.assertTrue(
            self.times_through == 3, "times_through = %d" % (self.times_through)
        )
        self.assertTrue(self.semiRaiseException())
        self.assertTrue(
            self.times_through == 4, "times_through = %d" % (self.times_through)
        )

    def testRetryableExceptions(self):
        self.assertRaises(Exception, lambda: self.raiseRetryableException())
        self.assertEqual(3, self.times_through)


def mocked_time_generator():
    """
    Returns time in 10s increments
    """
    start = 578854800.0
    while True:
        yield start
        start += 10


@mock.patch("time.time", side_effect=mocked_time_generator())
class LogTimeTest(TestCase):
    @log_time
    def takes_some_time(self):
        pass

    def testBasic(self, mocked_time_generator):
        with self.assertLogs() as cm:
            self.takes_some_time()
        self.assertEqual(
            cm.output,
            [
                "INFO:root:Takes_Some_Time starting...",
                "INFO:root:Takes_Some_Time finished (0:00:20)",
            ],
        )


class DisableGCTest(TestCase):
    @disable_gc
    def gcShouldBeDisabled(self):
        self.assertFalse(gc.isenabled())

    def testDisableGCWhenEnabled(self):
        self.assertTrue(gc.isenabled())
        self.gcShouldBeDisabled()
        self.assertTrue(gc.isenabled())  # make sure we enable afterwards

    def testDoesNotEnableGCIfDisabled(self):
        gc.disable()
        self.assertFalse(gc.isenabled())
        self.gcShouldBeDisabled()
        self.assertFalse(gc.isenabled())
        gc.enable()


class CatchUserErrorTest(TestCase):
    @catch_user_error()
    def throwsUserError(self):
        raise UserError

    def testCatchesUserError(self):
        try:
            self.throwsUserError()
        except UserError:
            self.fail("Unexpected UserError")

    @catch_user_error()
    def throwsException(self):
        raise Exception

    def testDoesNotCatchOtherExceptions(self):
        with self.assertRaises(Exception):
            self.throwsException()


class CatchKeyboardInterruptTest(TestCase):
    @catch_keyboard_interrupt()
    def throwsKeyboardInterrupt(self):
        raise KeyboardInterrupt

    def testCatchesKeyboardInterrupt(self):
        try:
            self.throwsKeyboardInterrupt()
        except KeyboardInterrupt:
            self.fail("Unexpected KeyboardInterrupt")

    @catch_keyboard_interrupt()
    def throwsException(self):
        raise Exception

    def testDoesNotCatchOtherExceptions(self):
        with self.assertRaises(Exception):
            self.throwsException()
