# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging

from libfb.py.log import set_simple_logging

LOG: logging.Logger = logging.getLogger(__name__)


def main() -> None:
    set_simple_logging(logging.INFO)

    LOG.info("This is a script!")


if __name__ == "__main__":
    main()
