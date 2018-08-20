#!/usr/bin/env python3


class Sink:
    def taint_sink(argument):
        eval(argument)


def send_to_sink(argument):
    sink = Sink()
    sink.taint_sink(argument)
