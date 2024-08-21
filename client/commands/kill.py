# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module provides the logic for `pyre kill`. Normally one would cleanly
shut down a pyre daemon using `pyre stop`, but if the daemon is not
responsive `pyre kill` will kill it right away.
"""

import logging
import os
import shutil
import signal

import psutil

from .. import daemon_socket, find_directories, frontend_configuration, identifiers
from . import commands, stop

LOG: logging.Logger = logging.getLogger(__name__)

PYRE_FIRE = """
                                                    ',
                                                   ,c:
                                ',                ;cc:
                              ,:l;             ';:lll:
                            'cllc'            'clllll:
                           ;loooc'           'cllllllc'
                          ;looool,           :llllllll,       :,
                         'looooool'         ;lollllllo:      ;ll;
                         ;ooooooool,       'loooooloool;    ,clll:
                         cdoddoooooo;      ;oooooooooool:  ;loooll:
                         cddddddddodo;     cooooooooooooolloooooool;
                         ;ddddddddddo;    'loooooooooooooooooooooooc'
                          cdddddddddc     'ldddddooooooooooooooooooo,
                           ,coodolc;       cddddddddddoooooooooooooo;
                               '           ,oddddddddddddddddodooooo;
                          ,::::::::::::,    :ddddddddddddddddddddddl'
                          'lddddddddddxd:    :ddddddddddddddddddddd:
                            ;odddddddddddl,   ;oxdddddddddddddddddl'
                             'ldddddddddddo:   ,:ldxddxddddddddddl'
                               :ddddddddddddl'    cdxxxxxxxdddddl'
                                ,ldddddddddddo;    ,oxxxxxxxxxdc
                                  :ddddddddddddc;'  'cdxxxxxxo;
                                   ,ldddddddddddxo;   ;dxxxo:
                                     cdddddddddddddc   'lo:
                                      ;oddddddddddddo,
                                       'cddddddddddddd:
                                        ;odddddddddddddl,
                                       :ddddddddddddddddd:
                                     ,ldddddddddddddddddddl,
                                    :odddddddddddddddddddddo:
                                  'ldddddddddddddddddddddddddl'
                                 ;odddddddddddl, ,ldddddddddddo;
                               'cdddddddddddd:     :ddddddddddddc'
                              ;odddddddddddo,       ,odddddddddddo;
                             cddddddddddddc           cddddddddddddc
                           ;oxddxddxddddo;             ;odxxxxddddxxo,
                           ;:::::::::::;'               ';:::::::::::;
"""


def _kill_processes_by_name(name: str) -> None:
    for process in psutil.process_iter(attrs=["name"]):
        if process.name() != name:
            continue
        # Do not kill the `pyre kill` command itself.
        pid_to_kill = process.pid
        if pid_to_kill == os.getpgid(os.getpid()):
            continue
        try:
            LOG.info(f"Killing process {name} with pid {pid_to_kill}.")
            os.kill(pid_to_kill, signal.SIGKILL)
        except (ProcessLookupError, PermissionError) as exception:
            LOG.error(
                f"Failed to kill process {name} with pid {pid_to_kill} "
                + f"due to exception {exception}"
            )


def _kill_binary_processes(configuration: frontend_configuration.Base) -> None:
    LOG.warning("Force-killing all running pyre servers.")
    LOG.warning(
        "Use `pyre servers stop` if you want to gracefully stop all running servers."
    )
    start_command = configuration.get_server_start_command(download_if_needed=False)
    if start_command is not None:
        _kill_processes_by_name(str(start_command.get_pyre_binary_location()))


def _kill_client_processes(configuration: frontend_configuration.Base) -> None:
    _kill_processes_by_name(find_directories.CLIENT_NAME)
    # TODO (T85602687): Run `buck kill` once buck is supported by the server


def _delete_server_files(
    configuration: frontend_configuration.Base, flavor: identifiers.PyreFlavor
) -> None:
    socket_root = daemon_socket.get_default_socket_root()
    LOG.info(f"Deleting socket files and lock files under {socket_root}")
    for socket_path in daemon_socket.find_socket_files(socket_root):
        stop.remove_socket_if_exists(socket_path)

    log_directory = configuration.get_log_directory() / flavor.server_log_subdirectory()
    LOG.info(f"Deleting server logs under {log_directory}")
    try:
        shutil.rmtree(str(log_directory), ignore_errors=True)
    except OSError:
        pass

    # TODO(T92826668): Delete files under artifact root


def _delete_caches(configuration: frontend_configuration.Base) -> None:
    dot_pyre_directory = configuration.get_dot_pyre_directory()
    resource_cache_directory = dot_pyre_directory / "resource_cache"
    LOG.info(
        f"Deleting local binary and typeshed cache under {resource_cache_directory}"
    )
    try:
        shutil.rmtree(str(resource_cache_directory), ignore_errors=True)
    except OSError:
        pass
    # TODO (T85602687): Try to remove buck builder cache as well once buck is
    # supported by the server


def run(
    configuration: frontend_configuration.Base, with_fire: bool
) -> commands.ExitCode:
    _kill_binary_processes(configuration)
    _kill_client_processes(configuration)
    # TODO (T85602550): Store a rage log before this happens.
    # TODO (T85614630): Delete client logs as well.
    for flavor in [
        identifiers.PyreFlavor.CLASSIC,
        identifiers.PyreFlavor.CODE_NAVIGATION,
    ]:
        _delete_server_files(configuration, flavor)
    _delete_caches(configuration)
    if with_fire:
        LOG.warning(
            (
                "Note that `--with-fire` adds emphasis to `pyre kill` but does"
                + f" not affect its behavior.\n{PYRE_FIRE}"
            )
        )
    LOG.info("Done\n")
    return commands.ExitCode.SUCCESS
