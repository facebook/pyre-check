# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import itertools


# pyre-fixme[3]: Return type must be annotated.
# pyre-fixme[2]: Parameter must be annotated.
# pyre-fixme[2]: Parameter must be annotated.
def split_every(n, iterable):
    """Yields batches of size 'n' from an iterable:

    list(split_every(2, range(10))) => [[0, 1], [2, 3], [4, 5], [6, 7], [8, 9]]
    """
    i = iter(iterable)
    piece = list(itertools.islice(i, n))
    while piece:
        yield piece
        piece = list(itertools.islice(i, n))
