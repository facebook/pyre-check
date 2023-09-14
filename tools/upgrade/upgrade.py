# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
The pyre-upgrade command is composed of many subcommands related to upgrading a project or performing codemods. See the module-level documentation for each subcommand for more details.
"""


import argparse
import enum
import logging
import sys
import traceback
from logging import Logger

from . import UserError
from .ast import UnstableAST
from .commands.codemods import (
    MissingGlobalAnnotations,
    MissingOverrideReturnAnnotations,
    SetUseBuck1,
)
from .commands.consolidate_nested_configurations import ConsolidateNestedConfigurations
from .commands.expand_target_coverage import ExpandTargetCoverage
from .commands.fix_configuration import FixConfiguration
from .commands.fixme import Fixme
from .commands.fixme_all import FixmeAll
from .commands.fixme_single import FixmeSingle
from .commands.global_strictness import GlobalStrictness
from .commands.global_version_update import GlobalVersionUpdate
from .commands.pysa_version_update import PysaVersionUpdate
from .commands.strict_default import StrictDefault
from .commands.support_sqlalchemy import SupportSqlalchemy
from .commands.targets_to_configuration import TargetsToConfiguration
from .repository import Repository


LOG: Logger = logging.getLogger(__name__)


class ExitCode(enum.IntEnum):
    SUCCESS = 0
    FOUND_ERRORS = 1
    FAILURE = 2


def run(repository: Repository) -> None:
    parser = argparse.ArgumentParser(fromfile_prefix_chars="@")
    parser.add_argument("--verbose", action="store_true", help="Enable verbose logging")

    commands = parser.add_subparsers()

    # Subcommands: Codemods
    missing_overridden_return_annotations = commands.add_parser(
        "missing-overridden-return-annotations",
        help="Add annotations according to errors inputted through stdin.",
    )
    MissingOverrideReturnAnnotations.add_arguments(
        missing_overridden_return_annotations
    )

    missing_global_annotations = commands.add_parser(
        "missing-global-annotations",
        help="Add annotations according to errors inputted through stdin.",
    )
    MissingGlobalAnnotations.add_arguments(missing_global_annotations)

    # Subcommand: Change default pyre mode to strict and adjust module headers.
    strict_default = commands.add_parser("strict-default")
    StrictDefault.add_arguments(strict_default)

    # Subcommand: Changes .pyre_configuration strictness and adds overrides to maintain current strictness for sub-configurations.
    update_global_strictness = commands.add_parser("update-global-strictness")
    GlobalStrictness.add_arguments(update_global_strictness)

    # Subcommand: Set global configuration to given hash, and add version override
    # to all local configurations to run previous version.
    update_global_version = commands.add_parser("update-global-version")
    GlobalVersionUpdate.add_arguments(update_global_version)

    # Subcommand: Set global configuration `pysa_version` to given hash
    update_pysa_version = commands.add_parser("update-pysa-version")
    PysaVersionUpdate.add_arguments(update_pysa_version)

    # Subcommand: Fixme all errors inputted through stdin.
    fixme = commands.add_parser("fixme")
    Fixme.add_arguments(fixme)

    # Subcommand: Fixme all errors for a single project.
    fixme_single = commands.add_parser("fixme-single")
    FixmeSingle.add_arguments(fixme_single)

    # Subcommand: Fixme all errors in all projects with local configurations.
    fixme_all = commands.add_parser("fixme-all")
    FixmeAll.add_arguments(fixme_all)

    # Subcommand: Remove targets integration and replace with configuration
    targets_to_configuration = commands.add_parser("targets-to-configuration")
    TargetsToConfiguration.add_arguments(targets_to_configuration)

    # Subcommand: Expand target coverage in configuration up to given error limit
    expand_target_coverage = commands.add_parser("expand-target-coverage")
    ExpandTargetCoverage.add_arguments(expand_target_coverage)

    # Subcommand: Consolidate nested local configurations
    consolidate_nested_configurations = commands.add_parser("consolidate-nested")
    ConsolidateNestedConfigurations.add_arguments(consolidate_nested_configurations)

    # Subcommand: Attempt remediation on broken configuration.
    fix_configuration = commands.add_parser("fix-configuration")
    FixConfiguration.add_arguments(fix_configuration)

    set_use_buck1 = commands.add_parser("set-use-buck1")
    SetUseBuck1.add_arguments(set_use_buck1)

    support_sqlalchemy = commands.add_parser("support-sqlalchemy")
    SupportSqlalchemy.add_arguments(support_sqlalchemy)

    # Initialize default values.
    arguments = parser.parse_args()
    # All commands should have the argument `command` set to their `from_arguments``
    if not hasattr(arguments, "command"):
        # Reparsing with `fixme` as default subcommand.
        arguments = parser.parse_args(sys.argv[1:] + ["fixme"])

    logging.basicConfig(
        format="%(asctime)s %(levelname)s %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
        level=logging.DEBUG if arguments.verbose else logging.INFO,
    )

    try:
        exit_code = ExitCode.SUCCESS
        arguments.command(arguments, repository).run()
    except UnstableAST as error:
        LOG.error(str(error))
        exit_code = ExitCode.FOUND_ERRORS
    except UserError as error:
        LOG.error(str(error))
        exit_code = ExitCode.FAILURE
    except Exception as error:
        LOG.error(str(error))
        LOG.debug(traceback.format_exc())
        exit_code = ExitCode.FAILURE

    sys.exit(exit_code)


def main() -> None:
    run(Repository())


if __name__ == "__main__":
    main()
