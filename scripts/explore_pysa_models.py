#!/usr/bin/env python3
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import collections
import io
import json
import re
import subprocess
from pathlib import Path
from typing import Optional, Any, Dict, List, Tuple, Iterable, NamedTuple


class FilePosition(NamedTuple):
    offset: int
    length: int


__handle: Optional[io.BufferedReader] = None
__model_index: Dict[str, FilePosition] = {}
__issue_index: Dict[str, List[FilePosition]] = collections.defaultdict(list)
__warned_missing_jq: bool = False


def _iter_with_offset(lines: Iterable[bytes]) -> Iterable[Tuple[bytes, int]]:
    offset = 0
    for line in lines:
        yield (line, offset)
        offset += len(line)


def _resolve_taint_output_path(taint_output_path: str) -> Path:
    taint_output_path = Path(taint_output_path)
    if taint_output_path.is_dir():
        taint_output_path = taint_output_path / "taint-output.json"
    return taint_output_path


def index(taint_output_path: str = "taint-output.json") -> None:
    """Index all available models in the given taint output file or directory."""
    global __handle, __model_index, __issue_index

    print(f"Indexing `{taint_output_path}`")
    taint_output_path = _resolve_taint_output_path(taint_output_path)
    __handle = open(taint_output_path, "rb")
    __model_index = {}
    __issue_index = collections.defaultdict(list)

    count_models = 0
    count_issues = 0
    for line, offset in _iter_with_offset(__handle):
        message = json.loads(line)
        if "kind" not in message:
            continue

        if message["kind"] == "model":
            callable = message["data"]["callable"]
            assert callable not in __model_index
            __model_index[callable] = FilePosition(offset=offset, length=len(line))
            count_models += 1
        elif message["kind"] == "issue":
            callable = message["data"]["callable"]
            __issue_index[callable].append(
                FilePosition(offset=offset, length=len(line))
            )
            count_issues += 1

    print(f"Indexed {count_models} models and {count_issues} issues")


def _assert_loaded() -> None:
    if __handle is None or len(__model_index) == 0:
        raise AssertionError("call index() first")


def callables_containing(string: str) -> List[str]:
    """Find all callables containing the given string."""
    _assert_loaded()
    return sorted(filter(lambda name: string in name, __model_index.keys()))


def callables_matching(pattern: str) -> List[str]:
    """Find all callables matching the given regular expression."""
    _assert_loaded()
    regex = re.compile(pattern)
    return sorted(filter(lambda name: re.search(regex, name), __model_index.keys()))


def _read(position: FilePosition) -> bytes:
    _assert_loaded()
    __handle.seek(position.offset)
    return __handle.read(position.length)


def get_model(callable: str) -> Dict[str, Any]:
    """Get the model for the given callable."""
    _assert_loaded()

    if callable not in __model_index:
        raise AssertionError(f"no model for callable `{callable}`.")

    message = json.loads(_read(__model_index[callable]))
    assert message["kind"] == "model"
    return message["data"]


def _print_json(data: object) -> None:
    try:
        subprocess.run(["jq", "-C"], input=json.dumps(data).encode(), check=True)
    except FileNotFoundError:
        print(json.dumps(data, indent="  "))

        global __warned_missing_jq
        if not __warned_missing_jq:
            print(
                "[HINT] Install `jq` to use syntax highlighting, https://stedolan.github.io/jq/"
            )
            __warned_missing_jq = True


def print_model(callable: str) -> None:
    """Pretty print the model for the given callable."""
    _print_json(get_model(callable))


def get_issues(callable: str) -> List[Dict[str, Any]]:
    """Get all issues within the given callable."""
    _assert_loaded()

    issues = []
    for position in __issue_index[callable]:
        message = json.loads(_read(position))
        assert message["kind"] == "issue"
        issues.append(message["data"])

    return issues


def print_issues(callable: str) -> None:
    """Pretty print the issues within the given callable."""
    _print_json(get_issues(callable))


def print_help() -> None:
    print("# Pysa Model Explorer")
    print("Available commands:")
    commands = [
        (index, "index('taint-output.json')"),
        (callables_containing, "callables_containing('foo.bar')"),
        (callables_matching, "callables_matching(r'foo\\..*')"),
        (get_model, "get_model('foo.bar')"),
        (print_model, "print_model('foo.bar')"),
        (get_issues, "get_issues('foo.bar')"),
        (print_issues, "print_issues('foo.bar')"),
    ]
    max_width = max(len(command[1]) for command in commands)
    for command, example in commands:
        print(f"  {example:<{max_width}} {command.__doc__}")


if __name__ == "__main__":
    print_help()
