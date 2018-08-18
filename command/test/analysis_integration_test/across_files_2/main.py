#!/usr/bin/env python3

from taint_sink import send_to_sink
from taint_source import get_source


def main():
    taint = get_source()
    send_to_sink(taint)
