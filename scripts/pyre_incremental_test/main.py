# pyre-strict

import argparse
import json
import logging
import sys
from pathlib import Path
from typing import Optional

from .environment import SubprocessEnvironment
from .runner import compare_server_to_full
from .specification import InvalidSpecificationException, Specification


LOG: logging.Logger = logging.getLogger(__name__)


def _setup_logging(verbosity: int) -> None:
    logging.basicConfig(
        level=max(3 - verbosity, 0) * 10,
        format=" >>> %(asctime)s %(levelname)s %(message)s",
    )


def main(arguments: argparse.Namespace) -> int:
    specification_path: Optional[Path] = arguments.specification
    try:
        LOG.info("Reading input JSON...")
        if specification_path is None:
            file_content = sys.stdin.read()
        else:
            with open(specification_path, "r") as specification_file:
                file_content = specification_file.read()

        LOG.info("Parsing JSON into test specification...")
        specification = Specification.from_json(json.loads(file_content))

        LOG.info("Start testing...")
        result = compare_server_to_full(
            environment=SubprocessEnvironment(), specification=specification
        )
        LOG.info("Done testing.")
        print(json.dumps(result.to_json()))
    except FileNotFoundError:
        LOG.exception(f"Specification file at {specification_path} does not exist")
        return 1
    except json.JSONDecodeError:
        LOG.exception(f"Cannot parse JSON at {specification_path}")
        return 1
    except InvalidSpecificationException:
        LOG.exception(f"Invalid specification JSON")
        return 1
    except Exception:
        LOG.exception("Exception occurs in the check")
        return 1
    return 0


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Run incremental test for Pyre according to given specification"
    )
    parser.add_argument(
        "specification",
        metavar="SPECIFICATION",
        type=Path,
        nargs="?",
        help="A JSON file containing a list of testing specifications",
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action="count",
        default=0,
        help="Logging verbosity: 0 = only warnings, 1 = info, 2 = debug",
    )
    arguments = parser.parse_args()
    _setup_logging(arguments.verbose)

    sys.exit(main(arguments))
