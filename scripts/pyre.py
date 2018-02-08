# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import logging
import os
import sys
import time
import traceback


from . import (
    buck,
    commands,
    EnvironmentException,
    FAILURE,
    get_version,
    JSON,
    log,
    log_statistics,
    resolve_source_directories,
    SUCCESS,
    switch_root,
    TEXT,
)
from .configuration import Configuration


LOG = logging.getLogger(__name__)


def main() -> int:
    parser = argparse.ArgumentParser(
        epilog='environment variables:  `PYRE_BINARY` overrides the pyre '
        'client binary used.',
        allow_abbrev=False)

    parser.add_argument(
        '-l',
        '--local-configuration',
        type=str,
        help='Use a local configuration')

    parser.add_argument(
        '--debug',
        action='store_true',
        help='Run in debug mode')

    parser.add_argument(
        '--show-error-traces',
        action='store_true',
        help='Display errors trace information')

    # Logging.
    parser.add_argument(
        '--output',
        choices=[TEXT, JSON],
        default=TEXT,
        help='How to format output')
    parser.add_argument(
        '--verbose',
        action='store_true',
        help='Enable verbose logging')
    parser.add_argument(
        '--noninteractive',
        action='store_true',
        help='Disable interactive logging')
    parser.add_argument(
        '--logging-sections',
        help='Enable sectional logging')
    parser.add_argument(
        '--log-identifier',
        action='store',
        default='',
        help='Add given identifier to logged samples.')

    parser.add_argument(
        '--version',
        action='store_true',
        help='Print the pyre version to be used')

    # Link tree determination.
    buck_arguments = parser.add_argument_group('buck')
    buck_arguments.add_argument(
        '--build',
        action='store_true',
        help='Build all the necessary artifacts.')
    buck_arguments.add_argument(
        '--target',
        action='append',
        help='The buck target to check')

    source_directory = parser.add_argument_group('source-directory')
    source_directory.add_argument(
        '--source-directory',
        action='append',
        help='The source directory to check')

    # Subcommands.
    parsed_commands = parser.add_subparsers()

    incremental = parsed_commands.add_parser(commands.INCREMENTAL)
    incremental.set_defaults(command=commands.Incremental)

    rage = parsed_commands.add_parser(commands.RAGE)
    rage.set_defaults(command=commands.Rage)

    check = parsed_commands.add_parser(commands.CHECK)
    check.set_defaults(command=commands.Check)

    persistent = parsed_commands.add_parser(commands.PERSISTENT)
    persistent.add_argument(
        '--no-watchman',
        action='store_true',
        help='Do not spawn a watchman client in the background.'
        ' control changes')
    persistent.set_defaults(command=commands.Persistent, noninteractive=True)

    start = parsed_commands.add_parser(commands.START)
    start.add_argument(
        '--terminal',
        action='store_true',
        help='Run the server in the terminal.')
    start.add_argument(
        '--no-watchman',
        action='store_true',
        help='Do not spawn a watchman client in the background.'
        ' control changes')
    start.set_defaults(command=commands.Start)

    stop = parsed_commands.add_parser(commands.STOP)
    stop.set_defaults(command=commands.Stop)

    restart = parsed_commands.add_parser(commands.RESTART)
    restart.add_argument(
        '--terminal',
        action='store_true',
        help='Run the server in the terminal.')
    restart.add_argument(
        '--no-watchman',
        action='store_true',
        help='Do not spawn a watchman client in the background.'
        ' control changes')
    restart.set_defaults(command=commands.Restart)

    kill = parsed_commands.add_parser(commands.KILL)
    kill.add_argument(
        '--with-fire',
        action='store_true',
        help='Adds emphasis to the command.')
    kill.set_defaults(command=commands.Kill)

    arguments = parser.parse_args()

    if not hasattr(arguments, 'command'):
        arguments.command = commands.Incremental

    configuration = None
    try:
        start = time.time()
        exit_code = SUCCESS

        if arguments.debug or not os.isatty(sys.stderr.fileno()):
            arguments.noninteractive = True

        log.initialize(arguments)
        switch_root(arguments)

        configuration = Configuration(
            original_directory=arguments.original_directory,
            local_configuration=arguments.local_configuration)
        source_directories = []
        if configuration.disabled():
            LOG.log(
                log.SUCCESS,
                "Pyre will not run due to being explicitly disabled")
            return SUCCESS

        if arguments.version:
            log.stdout.write(get_version(configuration))
            return SUCCESS

        configuration.validate()

        source_directories = resolve_source_directories(
            arguments,
            configuration)
        arguments.command(arguments, configuration, source_directories).run()
    except (
        buck.BuckException,
        commands.ClientException,
        EnvironmentException
    ) as error:
        LOG.error(str(error))
        arguments.command(
            arguments,
            configuration,
            source_directories).on_client_exception()
        exit_code = FAILURE
    except Exception as error:
        LOG.error(str(error))
        LOG.info(traceback.format_exc())
        exit_code = FAILURE
    except KeyboardInterrupt:
        LOG.warning("Interrupted by user")
        LOG.debug(traceback.format_exc())
        exit_code = SUCCESS
    finally:
        log.cleanup(arguments)
        if configuration and configuration.logger:
            sample = {
                'int': {
                    'exit_code': exit_code,
                    'runtime': int((time.time() - start) * 1000),  # ms
                },
                'normal': {
                    'arguments': str(arguments),
                    'host': os.getenv('HOSTNAME'),
                    'source_directory': str(arguments.source_directory or []),
                    'target': str(arguments.target or []),
                    'user': os.getenv('USER'),

                },
            }
            log_statistics('pyre_usage', configuration.logger, sample)

    return exit_code


if __name__ == '__main__':
    sys.exit(main())
