#!/usr/bin/env python3

import itertools


def split_every(n, iterable):
    """Yields batches of size 'n' from an iterable:

    list(split_every(2, range(10))) => [[0, 1], [2, 3], [4, 5], [6, 7], [8, 9]]
    """
    i = iter(iterable)
    piece = list(itertools.islice(i, n))
    while piece:
        yield piece
        piece = list(itertools.islice(i, n))
