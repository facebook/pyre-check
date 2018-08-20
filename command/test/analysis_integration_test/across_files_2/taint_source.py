#!/usr/bin/env python3


class Source:
    def return_taint(self):
        return taint()


def get_source():
    source = Source()
    return source.return_taint()
