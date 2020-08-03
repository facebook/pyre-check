import argparse
import itertools
import json
import logging
import shutil
import subprocess
import sys
from itertools import chain
from pathlib import Path
from typing import Dict, List, Optional

from typing_extensions import TypedDict


LOG: logging.Logger = logging.getLogger(__name__)


class SourceDatabase(TypedDict):
    sources: Dict[str, str]
    dependencies: Dict[str, str]


def _buck(args: List[str]) -> str:
    return subprocess.check_output(["buck"] + args).decode("utf-8")


def _get_buck_query_arguments(
    specifications: List[str], mode: Optional[str]
) -> List[str]:
    mode_sublist = ["@mode/" + mode] if mode is not None else []
    return [
        "query",
        "--json",
        *mode_sublist,
        'kind("python_binary|python_library|python_test", "%s")'
        # Don't check generated rules.
        " - attrfilter(labels, generated, %s)"
        # `python_unittest()` sources are separated into a macro-generated
        # library, so make sure we include those.
        " + attrfilter(labels, unittest-library, %s)"
        # Provide an opt-out label so that rules can avoid type-checking (e.g.
        # some libraries wrap generated sources which are expensive to build
        # and therefore typecheck).
        " - attrfilter(labels, no_pyre, %s)",
        *specifications,
    ]


def _normalize_specification(specification: str) -> str:
    return specification if specification.startswith("//") else "//" + specification


def _query_targets(target_specifications: List[str], mode: Optional[str]) -> List[str]:
    normalized_target_specifications = [
        _normalize_specification(specification)
        for specification in target_specifications
    ]
    query_arguments = _get_buck_query_arguments(normalized_target_specifications, mode)
    specification_targets_dictionary = json.loads(_buck(query_arguments))
    return list(chain(*specification_targets_dictionary.values()))


def _get_buck_build_arguments(targets: List[str]) -> List[str]:
    # NOTE(agallagher): We could potentially use flags like
    # `-c fbcode.py_version=3 -c fbcode.platform=platform007` to force everything
    # onto a consistent set of platforms, but this has a cost of invalidating the
    # parser cache, which may not be worth it.
    return [
        "build",
        "--show-full-json-output",
        *(f"{target}#source-db" for target in targets),
    ]


def _build_targets(targets: List[str]) -> Dict[str, str]:
    build_arguments = _get_buck_build_arguments(targets)
    return json.loads(_buck(build_arguments))


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


def _build(
    target_specifications: List[str],
    output_directory: Path,
    buck_root: Path,
    mode: Optional[str],
) -> None:
    targets = _query_targets(target_specifications, mode)
    target_path_dictionary = _build_targets(targets)
    source_databases = _load_source_databases(target_path_dictionary)
    link_map = _merge_source_databases(source_databases)
    _build_link_tree(link_map, output_directory, buck_root)


def main(argv: List[str]) -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("-J")
    parser.add_argument("--debug", action="store_true")
    parser.add_argument("--mode")
    parser.add_argument("--project_name")
    parser.add_argument("--output_directory", required=True, type=Path)
    parser.add_argument("--buck_root", dest="buck_root", required=True, type=Path)
    parser.add_argument("target_specifications", nargs="*")
    arguments = parser.parse_args(argv[1:])

    _build(
        arguments.target_specifications,
        arguments.output_directory,
        arguments.buck_root,
        arguments.mode,
    )


if __name__ == "__main__":
    logging.basicConfig(
        level=logging.INFO, format="[%(asctime)s] [%(levelname)s] %(message)s"
    )
    sys.exit(main(sys.argv))
