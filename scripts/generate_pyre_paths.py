#!/usr/bin/env python

from __future__ import absolute_import, division, print_function, unicode_literals

import os
from argparse import ArgumentParser, Namespace


parser = ArgumentParser()
parser.add_argument("--install_dir")
parser.add_argument("--fbcode_dir")
parser.add_argument("--pyre_client")
args: Namespace = parser.parse_args()
with open(os.path.join(args.install_dir, "pyre_paths.py"), "w") as f:
    f.write('pyre_client = "%s"\n' % args.pyre_client)
