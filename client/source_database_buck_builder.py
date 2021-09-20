# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import itertools
import json
import logging
import re
import shutil
import subprocess
import sys
import tempfile
from itertools import chain
from pathlib import Path
from typing import Dict, List, Optional

from typing_extensions import TypedDict


LOG: logging.Logger = logging.getLogger(__name__)


class SourceDatabase(TypedDict):
    sources: Dict[str, str]
    dependencies: Dict[str, str]


def _buck(
    arguments: List[str],
    isolation_prefix: Optional[str],
    buck_root: Path,
) -> str:
    isolation_prefix_arguments = (
        ["--isolation_prefix", isolation_prefix]
        if isolation_prefix is not None and len(isolation_prefix) > 0
        else []
    )
    command = (
        ["buck"]
        + isolation_prefix_arguments
        + arguments
        + ["--config", "client.id=pyre"]
    )
    LOG.debug("Running `%s`", " ".join(command))
    return subprocess.check_output(
        command, stderr=subprocess.PIPE, cwd=str(buck_root)
    ).decode("utf-8")


def _get_buck_query_arguments(
    specifications: List[str], mode: Optional[str]
) -> List[str]:
    mode_sublist = ["@mode/" + mode] if mode is not None else []
    return [
        "query",
        "--json",
        *mode_sublist,
        'kind("python_binary|python_library|python_test", %s)'
        # Don't check generated rules.
        + " - attrfilter(labels, generated, %s)"
        # `python_unittest()` sources are separated into a macro-generated
        # library, so make sure we include those.
        + " + attrfilter(labels, unittest-library, %s)"
        # Provide an opt-out label so that rules can avoid type-checking (e.g.
        # some libraries wrap generated sources which are expensive to build
        # and therefore typecheck).
        + " - attrfilter(labels, no_pyre, %s)",
        *specifications,
    ]


def _normalize_specification(specification: str) -> str:
    return specification if "//" in specification else "//" + specification


def _ignore_target(target: str) -> bool:
    suffixes_for_ignored_targets = ("-mypy_ini", "-testmodules-lib")
    return target.endswith(suffixes_for_ignored_targets)


def _load_json_ignoring_extra_data(source: str) -> Dict[str, str]:
    try:
        return json.loads(source)
    except json.JSONDecodeError as exception:
        LOG.debug(f"JSON output: {source}")
        LOG.warning("Failed to parse JSON. Retrying by ignoring extra data...")

        match = re.search(r"Extra data: line ([0-9]+) column", exception.args[0])
        if match is None:
            raise exception

        line_number = int(match.group(1))
        source_without_extra_data = "\n".join(source.splitlines()[: line_number - 1])
        return json.loads(source_without_extra_data)


def _query_targets(
    target_specifications: List[str],
    mode: Optional[str],
    isolation_prefix: Optional[str],
    buck_root: Path,
) -> List[str]:
    normalized_target_specifications = [
        _normalize_specification(specification)
        for specification in target_specifications
    ]
    query_arguments = _get_buck_query_arguments(normalized_target_specifications, mode)
    LOG.info("Running `buck query`...")
    specification_targets_dictionary = _load_json_ignoring_extra_data(
        _buck(query_arguments, isolation_prefix, buck_root)
    )
    targets = list(chain(*specification_targets_dictionary.values()))
    return [target for target in targets if not _ignore_target(target)]


def _get_buck_build_arguments(mode: Optional[str], targets: List[str]) -> List[str]:
    # NOTE(agallagher): We could potentially use flags like
    # `-c fbcode.py_version=3 -c fbcode.platform=platform007` to force everything
    # onto a consistent set of platforms, but this has a cost of invalidating the
    # parser cache, which may not be worth it.
    mode_sublist = ["@mode/" + mode] if mode is not None else []
    return [
        *mode_sublist,
        "--show-full-json-output",
        *(f"{target}#source-db" for target in targets),
    ]


def _build_targets(
    targets: List[str],
    mode: Optional[str],
    isolation_prefix: Optional[str],
    buck_root: Path,
) -> Dict[str, str]:
    build_arguments = _get_buck_build_arguments(mode, targets)
    LOG.info("Running `buck build`...")
    with tempfile.NamedTemporaryFile(
        "w+", prefix="pyre_buck_build_arguments"
    ) as arguments_file:
        build_args_contents = "\n".join(build_arguments)
        arguments_file.write(build_args_contents)
        arguments_file.flush()  # Ensure the contents get to file
        output = _buck(
            ["build", f"@{arguments_file.name}"], isolation_prefix, buck_root
        )
        return _load_json_ignoring_extra_data(output)


def _load_source_databases(
    target_path_dictionary: Dict[str, str]
) -> Dict[str, SourceDatabase]:
    return {
        target: json.loads(Path(path).read_text())
        for target, path in target_path_dictionary.items()
    }


def _merge_source_databases(databases: Dict[str, SourceDatabase]) -> Dict[str, str]:
    link_map = {}
    for _target, database in sorted(databases.items(), key=lambda pair: pair[0]):
        for destination, source in itertools.chain(
            database["sources"].items(), database["dependencies"].items()
        ):
            # Ignore non-Python sources.
            if Path(destination).suffix not in (".py", ".pyi"):
                continue

            # These auto-generated modules are duplicated between test/binary
            # rules and so conflict when merging.  In practice, they're probably
            # not important for type checking, so just ignore them.
            if destination in [
                "__manifest__.py",
                "__test_modules__.py",
                "__test_main__.py",
            ]:
                continue

            # If we've already created the link, skip.
            link_map.setdefault(destination, source)
    return link_map


def _build_link_tree(
    link_map: Dict[str, str], output_directory: Path, buck_root: Path
) -> None:
    """
    Create a symlink tree where we merge the transitive dependency modules for all
    Python rules.
    """
    shutil.rmtree(output_directory, ignore_errors=True)
    output_directory.mkdir(parents=True)
    for destination, source in link_map.items():
        source_path = buck_root / source
        assert source_path.exists(), source_path
        destination_path = output_directory / destination
        destination_path.parent.mkdir(parents=True, exist_ok=True)
        destination_path.symlink_to(source_path)


def build(
    target_specifications: List[str],
    output_directory: Path,
    buck_root: Path,
    mode: Optional[str],
    isolation_prefix: Optional[str],
) -> None:
    targets = _query_targets(target_specifications, mode, isolation_prefix, buck_root)
    target_path_dictionary = _build_targets(targets, mode, isolation_prefix, buck_root)
    source_databases = _load_source_databases(target_path_dictionary)
    link_map = _merge_source_databases(source_databases)
    _build_link_tree(link_map, output_directory, buck_root)


def main(argv: List[str]) -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("-J")
    parser.add_argument("--debug", action="store_true")
    parser.add_argument("--mode")
    parser.add_argument("--project_name")
    parser.add_argument("--isolation_prefix")
    parser.add_argument("--output_directory", required=True, type=Path)
    parser.add_argument("--buck_root", dest="buck_root", required=True, type=Path)
    parser.add_argument("target_specifications", nargs="*")
    arguments = parser.parse_args(argv[1:])

    build(
        arguments.target_specifications,
        arguments.output_directory,
        arguments.buck_root,
        arguments.mode,
        arguments.isolation_prefix,
    )


if __name__ == "__main__":
    logging.basicConfig(
        level=logging.INFO, format="[%(asctime)s] [%(levelname)s] %(message)s"
    )
    sys.exit(main(sys.argv))
