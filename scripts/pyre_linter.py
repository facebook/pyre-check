# pyre-strict

import argparse
import json
import logging
import os
import subprocess
import sys
from collections import defaultdict
from enum import Enum
from typing import Dict, Iterable, List, Mapping, NamedTuple, Optional


LOG: logging.Logger = logging.getLogger(__name__)


class LintSeverity(str, Enum):
    ERROR = "error"
    WARNING = "warning"
    ADVICE = "advice"
    DISABLED = "disabled"


class LintMessage(NamedTuple):
    path: str
    line: Optional[int]
    char: Optional[int]
    code: str
    severity: LintSeverity
    name: str
    original: Optional[str]
    replacement: Optional[str]
    description: Optional[str]
    bypassChangedLineFiltering: Optional[bool]


def _lint_paths(
    checks: List[str],
    directory: str,
    paths: Iterable[str],
    timeout: Optional[int] = None,
    start_server: bool = False,
) -> List[LintMessage]:
    # Chdir to the server's directory.
    messages = []
    try:
        os.chdir(directory)
        if start_server:
            # Start a server if necessary, and block until it gives results.
            subprocess.run(["pyre"], check=True, stdout=subprocess.DEVNULL)
        paths = [f"'{path}'" for path in paths]
        for check in checks:
            result = subprocess.run(
                ["pyre", "query", f"run_check('{check}', {','.join(paths)})"],
                check=True,
                stdout=subprocess.PIPE,
                timeout=timeout,
            )
            response = json.loads(result.stdout.decode())
            if "response" in response:
                for error in response["response"]["errors"]:
                    messages.append(
                        LintMessage(
                            path=error["path"],
                            line=error["line"],
                            char=error["column"],
                            code="PYRELINT",
                            severity=LintSeverity.WARNING,
                            name=check,
                            original=None,
                            replacement=None,
                            description=error["description"],
                            bypassChangedLineFiltering=None,
                        )
                    )
        if start_server:
            # This will not be invoked via the regular lint flow, only on CI checks, so
            # stopping the server is appropriate.
            subprocess.run(["pyre", "stop"], check=True, stdout=subprocess.DEVNULL)
    except subprocess.CalledProcessError as exception:
        LOG.error(str(exception))
    except json.decoder.JSONDecodeError:
        LOG.warning("Unable to decode server response.")
    except subprocess.TimeoutExpired as exception:
        LOG.warning(
            f"Pyre query timed out after {str(exception.timeout)} seconds, \
            skipping {directory}."
        )
    return messages


def _get_local_pyre_project(path: str) -> Optional[str]:
    while path != "/":
        if os.path.isfile(f"{path}/.pyre_configuration.local") or os.path.isfile(
            f"{path}/.pyre_configuration"
        ):
            return path
        path = os.path.dirname(path)
    return None


def _group_by_pyre_server(paths: Iterable[str]) -> Mapping[str, List[str]]:
    """
    Given a set of files, evaluates to a mapping of project directory -> list of files.
    Each file will appear in at most one project directory (if no configuration is
    found, the file won't belong to a project).
    """
    file_mapping: Dict[str, List[str]] = defaultdict(list)
    for path in paths:
        pyre_configuration = _get_local_pyre_project(path)
        if pyre_configuration is not None:
            file_mapping[pyre_configuration].append(path)
    return dict(file_mapping)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="A linter that runs static analyses on top of Pyre.",
        fromfile_prefix_chars="@",
    )
    parser.add_argument("--verbose", action="store_true", help="verbose logging")
    parser.add_argument(
        "--check", action="append", help="List of static analyses that should be run."
    )
    parser.add_argument("--timeout", type=int, help="timeout (in seconds) per server")
    parser.add_argument(
        "--start-server",
        action="store_true",
        help="Start a Pyre server if necessary and block on its results",
    )
    parser.add_argument("filenames", nargs="+", help="paths to lint")
    arguments = parser.parse_args()

    logging.basicConfig(
        format="[pid=%(process)d %(threadName)s] <%(levelname)s> %(message)s",
        level=logging.NOTSET if arguments.verbose else logging.WARNING,
        stream=sys.stderr,
    )

    paths = [os.path.abspath(filename) for filename in arguments.filenames]
    to_lint = _group_by_pyre_server(paths)
    for directory in to_lint:
        for result in _lint_paths(
            checks=arguments.check,
            directory=directory,
            paths=to_lint[directory],
            timeout=arguments.timeout,
            start_server=arguments.start_server,
        ):
            print(json.dumps(result._asdict()))


if __name__ == "__main__":
    main()
