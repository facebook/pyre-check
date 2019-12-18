# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import functools
import logging
import subprocess
from pathlib import Path
from typing import List

from mypy_extensions import TypedDict

from .. import LOG_DIRECTORY, find_project_root
from .command import Command


LOG = logging.getLogger(__name__)  # type: logging.Logger


ROOT_PLACEHOLDER_NAME = "<root>"
PID_MAXIMUM_WIDTH = 10


class ServerDetails(TypedDict):
    local_root: str
    pid: int
    path: Path


class Servers(Command):
    NAME = "servers"

    @classmethod
    def add_subparser(cls, parser: argparse._SubParsersAction) -> None:
        servers_parser = parser.add_parser(
            cls.NAME,
            epilog="""
            Command to manipulate multiple Pyre servers.
            """,
        )
        servers_parser.set_defaults(command=cls, noninteractive=True)
        servers_parser.add_argument("list", help="List running servers.")

    @staticmethod
    @functools.lru_cache()
    def _dot_pyre_root() -> Path:
        return Path(find_project_root(str(Path.cwd())), LOG_DIRECTORY)

    @staticmethod
    def _print_server_details(all_server_details: List[ServerDetails]) -> None:
        print("Pyre servers: <pid> <server-path>")
        for details in all_server_details:
            print(
                "{:<{column_one_width}} {}".format(
                    details["pid"],
                    details["local_root"],
                    column_one_width=PID_MAXIMUM_WIDTH,
                )
            )

    @staticmethod
    def _find_servers() -> List[Path]:
        find_command = [
            "find",
            str(Servers._dot_pyre_root()),
            "-type",
            "f",
            "-name",
            "server.pid",
        ]
        LOG.info("Running command: `{}`".format(" ".join(find_command)))
        try:
            server_paths = subprocess.check_output(find_command).decode().splitlines()
        except subprocess.CalledProcessError as exception:
            LOG.error(f"`find` failed with exception `{exception}`")
            raise exception
        return [Path(path) for path in server_paths]

    @staticmethod
    def _fetch_server_details(server_path: Path, dot_pyre_root: Path) -> ServerDetails:
        local_root = str(server_path.relative_to(dot_pyre_root).parent.parent)
        return {
            "pid": int(server_path.read_text()),
            "path": server_path,
            "local_root": ROOT_PLACEHOLDER_NAME if local_root == "." else local_root,
        }

    def _run(self) -> None:
        server_paths = self._find_servers()
        all_server_details = sorted(
            (
                self._fetch_server_details(server_path, Servers._dot_pyre_root())
                for server_path in server_paths
            ),
            key=lambda details: details["local_root"],
        )
        self._print_server_details(all_server_details)
