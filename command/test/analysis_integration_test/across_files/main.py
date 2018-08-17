#!/usr/bin/env python3
from taint_sink import send_to_sink


def __main__():
    taint = taint()
    send_to_sink(taint)
