# Copyright 2004-present Facebook.  All rights reserved.

import argparse
import itertools
import json
import logging
import pathlib
import re
import sys
import traceback

import tools.pyre.scripts as pyre

from collections import defaultdict


LOG = logging.getLogger(__name__)


class PostprocessError(Exception):
    pass


def run_fixme(arguments):
    try:
        def error_path(error):
            return error['path']
        result = itertools.groupby(
            sorted(json.load(sys.stdin), key=error_path),
            error_path)

        for path, errors in result:
            LOG.info('Processing `%s`', path)

            # Build map from line to error codes.
            codes = defaultdict(lambda: set())
            for error in errors:
                code = re.search(r'\[(\d+)\]', error['description'])
                if code:
                    codes[error['line']].add(code.group(1))

            # Replace lines in file.
            path = pathlib.Path(path)
            lines = path.read_text().split('\n')

            new_lines = []
            for number, line in enumerate(lines):
                if number + 1 in codes:
                    sorted_codes = sorted(list(codes[number + 1]))
                    comment = '{}# pyre-fixme[{}]{}'.format(
                        line[:(len(line) - len(line.lstrip(' ')))],  # indent
                        ', '.join([str(code) for code in sorted_codes]),
                        '' if not arguments.comment \
                        else ': ' + arguments.comment)
                    LOG.info('Adding `%s` on line %d', comment, number + 1)

                    new_lines.extend([comment, line])
                else:
                    new_lines.append(line)

            path.write_text('\n'.join(new_lines))
    except (json.JSONDecodeError, OSError) as error:
        raise PostprocessError(str(error))


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '--verbose',
        action='store_true',
        help='Enable verbose logging')

    # Subcommands.
    commands = parser.add_subparsers()

    fixme = commands.add_parser("fixme")
    fixme.set_defaults(function=run_fixme)
    fixme.add_argument(
        '--comment',
        help='Comment after fixme comments')

    # Initialize default values.
    arguments = parser.parse_args()
    if not hasattr(arguments, 'function'):
        arguments.function = run_fixme

    logging.basicConfig(
        format='%(asctime)s %(levelname)s %(message)s',
        datefmt='%Y-%m-%d %H:%M:%S',
        level=logging.DEBUG if arguments.verbose else logging.INFO)

    try:
        exit_code = pyre.SUCCESS
        arguments.function(arguments)
    except PostprocessError as error:
        LOG.error(str(error))
        LOG.debug(traceback.format_exc())
        exit_code = pyre.FAILURE
    except Exception as error:
        LOG.error(str(error))
        LOG.info(traceback.format_exc())
        exit_code = pyre.FAILURE

    sys.exit(exit_code)
