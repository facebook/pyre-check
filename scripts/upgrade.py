# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


import argparse
import itertools
import json
import logging
import os
import pathlib
import re
import subprocess
import sys
import traceback

from collections import defaultdict

from . import (
    FAILURE,
    SUCCESS,
)


LOG = logging.getLogger(__name__)


class PostprocessError(Exception):
    pass


def run_fixme(arguments) -> None:
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


def run_unused_ignores(arguments) -> None:
    expected_error_lines = defaultdict(lambda: set())

    status = subprocess.check_output(['/usr/bin/hg', 'status'])
    dirty = len(status.strip()) > 0

    if dirty:
        LOG.info("Saving repository state")
        subprocess.check_call(['/usr/bin/hg', 'shelve'])

    def _remove_ignores(path):
        modified = False
        with open(path, 'r') as file:
            lines = list(file)
            for number, line in enumerate(lines):
                match = re.match(r'(.*)# pyre-(ignore|fixme).*', line)
                if match:
                    modified = True
                    lines[number] = match.groups()[0] + '\n'
                    expected_error_lines[path].add(number + 1)
                else:
                    lines[number] = line

        if modified:
            LOG.info("Removed ignores in `%s`", path)
            with open(path, 'w') as file:
                file.write(''.join(lines))

    for directory, _, filenames in os.walk('.'):
        directory = directory[2:]  # strip './' prefix
        for filename in filenames:
            path = os.path.join(directory, filename)
            if not path.endswith('.py'):
                continue
            _remove_ignores(path)

    LOG.info("Rechecking...")
    errors = json.loads(
        subprocess.check_output(
            ['/usr/local/bin/pyre', '--output=json', 'check']))

    # Compare result to expectations.
    unused_ignores = defaultdict(lambda: set())
    for path, expected_error_lines in expected_error_lines.items():
        for line in expected_error_lines:
            matches = [path == error['path'] and
                        (line == error['line'] or line + 1 == error['line'])
                        for error
                        in errors]
            if not any(matches):
                unused_ignores[path].add(line)

    LOG.info("Restoring repository state")
    subprocess.check_call(['/usr/bin/hg', 'revert', '--all'])
    if dirty:
        subprocess.check_call(['/usr/bin/hg', 'unshelve'])

    for path, lines in unused_ignores.items():
        LOG.info(
            "Found unused ignores in `%s`: %s",
            path,
            ', '.join([str(line) for line in lines]))


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

    unused_ignores = commands.add_parser("unused-ignores")
    unused_ignores.set_defaults(function=run_unused_ignores)

    # Initialize default values.
    arguments = parser.parse_args()
    if not hasattr(arguments, 'function'):
        arguments.function = run_fixme

    logging.basicConfig(
        format='%(asctime)s %(levelname)s %(message)s',
        datefmt='%Y-%m-%d %H:%M:%S',
        level=logging.DEBUG if arguments.verbose else logging.INFO)

    try:
        exit_code = SUCCESS
        arguments.function(arguments)
    except PostprocessError as error:
        LOG.error(str(error))
        LOG.debug(traceback.format_exc())
        exit_code = FAILURE
    except Exception as error:
        LOG.error(str(error))
        LOG.info(traceback.format_exc())
        exit_code = FAILURE

    sys.exit(exit_code)
