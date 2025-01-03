# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This module handles logic related to naming pyre projects and
servers (which in turn impacts log locations and socket paths).
"""

import enum
from pathlib import Path
from typing import List, Optional


def get_project_identifier(
    global_root: Path,
    relative_local_root: Optional[str],
) -> str:
    project_identifier = str(global_root)
    if relative_local_root is not None:
        project_identifier = project_identifier + "//" + relative_local_root
    return project_identifier


class IllegalFlavorException(Exception):
    pass


class PyreFlavor(enum.Enum):
    """
    The pyre flavor acts as a name of a particular language-server + daemon
    pair. Its value is a factor in determining socket and log paths, which
    have to be kept separate if we are running multiple language servers
    in parallel, as well as tagging telemetry data.

    On the client side, the enum value is only really important for language
    servers, which are long-lived and in most cases need custom daemons.

    All standard pyre commands use the CLASSIC daemon.

    The flavor is restricted to be one of a few known options because we rely
    on the values for metrics and also because we depend on the names being
    short enough not to exceed socket path limits.

    """

    CLASSIC = "classic"
    SHADOW = "shadow"

    def path_suffix(self) -> str:
        return "" if self == PyreFlavor.CLASSIC else f"__{self.value}"

    @staticmethod
    def persistent_choices() -> List[str]:
        """
        Valid flavors to use for the `pyre persistent` command.
        """
        return [
            PyreFlavor.CLASSIC.value,
            PyreFlavor.SHADOW.value,
        ]

    @staticmethod
    def server_flavor_choices() -> List[str]:
        return [
            PyreFlavor.CLASSIC.value,
        ]

    def server_log_subdirectory(self) -> str:
        return self.value

    def simple_name(self) -> str:
        if self == PyreFlavor.CLASSIC:
            return "Type Checking"
        else:
            raise IllegalFlavorException(f"No simple name defined for flavor {self}")
