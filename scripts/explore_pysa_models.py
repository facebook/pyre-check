#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import collections
import io
import json
import re
import subprocess
import textwrap
from pathlib import Path
from typing import Any, Callable, Dict, Iterable, List, NamedTuple, Optional, Tuple


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


def _resolve_taint_output_path(taint_output_filename: str) -> Path:
    taint_output_path = Path(taint_output_filename)
    if taint_output_path.is_dir():
        taint_output_path = taint_output_path / "taint-output.json"
    return taint_output_path


def index(taint_output_filename: str = "taint-output.json") -> None:
    """Index all available models in the given taint output file or directory."""
    global __handle, __model_index, __issue_index

    print(f"Indexing `{taint_output_filename}`")
    taint_output_path = _resolve_taint_output_path(taint_output_filename)
    handle = open(taint_output_path, "rb")
    __handle = handle
    __model_index = {}
    __issue_index = collections.defaultdict(list)

    count_models = 0
    count_issues = 0
    for line, offset in _iter_with_offset(handle):
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


def _assert_loaded() -> io.BufferedReader:
    handle = __handle
    if handle is None or len(__model_index) == 0:
        raise AssertionError("call index() first")
    return handle


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
    handle = _assert_loaded()
    handle.seek(position.offset)
    return handle.read(position.length)


def _filter_taint_tree(
    taint_tree: List[Dict[str, Any]],
    frame_predicate: Callable[[str, Dict[str, Any]], bool],
) -> List[Dict[str, Any]]:
    new_taint_tree = []
    for taint in taint_tree:
        caller_port = taint["port"]
        new_local_taints = []
        for local_taint in taint["taint"]:
            new_kinds = [
                frame
                for frame in local_taint["kinds"]
                if frame_predicate(caller_port, frame)
            ]
            if len(new_kinds) > 0:
                new_local_taint = local_taint.copy()
                new_local_taint["kinds"] = new_kinds
                new_local_taints.append(new_local_taint)

        if len(new_local_taints) > 0:
            new_taint = taint.copy()
            new_taint["taint"] = new_local_taints
            new_taint_tree.append(new_taint)

    return new_taint_tree


def filter_model(
    model: Dict[str, Any], frame_predicate: Callable[[str, Dict[str, Any]], bool]
) -> Dict[str, Any]:
    model = model.copy()
    model["sources"] = _filter_taint_tree(model.get("sources", []), frame_predicate)
    model["sinks"] = _filter_taint_tree(model.get("sinks", []), frame_predicate)
    model["tito"] = _filter_taint_tree(model.get("tito", []), frame_predicate)
    return model


def filter_model_caller_port(model: Dict[str, Any], port: str) -> Dict[str, Any]:
    def predicate(caller_port: str, frame: Dict[str, Any]) -> bool:
        return port == caller_port

    return filter_model(model, predicate)


def filter_model_kind(model: Dict[str, Any], kind: str) -> Dict[str, Any]:
    def predicate(caller_port: str, frame: Dict[str, Any]) -> bool:
        return frame["kind"] == kind

    return filter_model(model, predicate)


def _map_taint_tree(
    taint_tree: List[Dict[str, Any]],
    frame_map: Callable[[str, Dict[str, Any]], None],
    local_taint_map: Callable[[str, Dict[str, Any]], None],
) -> List[Dict[str, Any]]:
    new_taint_tree = []
    for taint in taint_tree:
        caller_port = taint["port"]
        new_local_taints = []
        for local_taint in taint["taint"]:
            new_kinds = []
            for frame in local_taint["kinds"]:
                new_frame = frame.copy()
                frame_map(caller_port, new_frame)
                new_kinds.append(new_frame)

            new_local_taint = local_taint.copy()
            new_local_taint["kinds"] = new_kinds
            local_taint_map(caller_port, new_local_taint)
            new_local_taints.append(new_local_taint)

        new_taint = taint.copy()
        new_taint["taint"] = new_local_taints
        new_taint_tree.append(new_taint)

    return new_taint_tree


def map_model(
    model: Dict[str, Any],
    frame_map: Optional[Callable[[str, Dict[str, Any]], None]] = None,
    local_taint_map: Optional[Callable[[str, Dict[str, Any]], None]] = None,
) -> Dict[str, Any]:
    frame_map = frame_map if frame_map is not None else lambda x, y: None
    local_taint_map = (
        local_taint_map if local_taint_map is not None else lambda x, y: None
    )

    model = model.copy()
    model["sources"] = _map_taint_tree(
        model.get("sources", []), frame_map, local_taint_map
    )
    model["sinks"] = _map_taint_tree(model.get("sinks", []), frame_map, local_taint_map)
    model["tito"] = _map_taint_tree(model.get("tito", []), frame_map, local_taint_map)
    return model


def model_remove_tito_positions(model: Dict[str, Any]) -> Dict[str, Any]:
    def local_taint_map(caller_port: str, local_taint: Dict[str, Any]) -> None:
        if "tito" in local_taint:
            del local_taint["tito"]

    return map_model(model, local_taint_map=local_taint_map)


def model_remove_features(model: Dict[str, Any]) -> Dict[str, Any]:
    def frame_map(caller_port: str, frame: Dict[str, Any]) -> None:
        if "features" in frame:
            del frame["features"]

    def local_taint_map(caller_port: str, local_taint: Dict[str, Any]) -> None:
        if "local_features" in local_taint:
            del local_taint["local_features"]

    return map_model(model, frame_map=frame_map, local_taint_map=local_taint_map)


def model_remove_leaf_names(model: Dict[str, Any]) -> Dict[str, Any]:
    def frame_map(caller_port: str, frame: Dict[str, Any]) -> None:
        if "leaves" in frame:
            del frame["leaves"]

    return map_model(model, frame_map=frame_map)


def get_model(
    callable: str,
    *,
    kind: Optional[str] = None,
    caller_port: Optional[str] = None,
    remove_sources: bool = False,
    remove_sinks: bool = False,
    remove_tito: bool = False,
    remove_tito_positions: bool = False,
    remove_features: bool = False,
    remove_leaf_names: bool = False,
) -> Dict[str, Any]:
    """Get the model for the given callable."""
    _assert_loaded()

    if callable not in __model_index:
        raise AssertionError(f"no model for callable `{callable}`.")

    message = json.loads(_read(__model_index[callable]))
    assert message["kind"] == "model"

    model = message["data"]
    if remove_sources and "sources" in model:
        del model["sources"]
    if remove_sinks and "sinks" in model:
        del model["sinks"]
    if remove_tito and "tito" in model:
        del model["tito"]
    if kind is not None:
        model = filter_model_kind(model, kind)
    if caller_port is not None:
        model = filter_model_caller_port(model, caller_port)
    if remove_tito_positions:
        model = model_remove_tito_positions(model)
    if remove_features:
        model = model_remove_features(model)
    if remove_leaf_names:
        model = model_remove_leaf_names(model)
    return model


def print_json(data: object) -> None:
    """Pretty print json objects with syntax highlighting."""
    if isinstance(data, str):
        data = json.loads(data)

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


def print_model(
    callable: str,
    *,
    kind: Optional[str] = None,
    caller_port: Optional[str] = None,
    remove_sources: bool = False,
    remove_sinks: bool = False,
    remove_tito: bool = False,
    remove_tito_positions: bool = False,
    remove_features: bool = False,
    remove_leaf_names: bool = False,
) -> None:
    """
    Pretty print the model for the given callable.
    Optional parameters:
      kind='UserControlled'      Filter by taint kind.
      caller_port='result'       Filter by caller port.
      remove_sources=False
      remove_sinks=False
      remove_tito=False
      remove_tito_positions=True
      remove_features=True
      remove_leaf_names=True
    """
    print_json(
        get_model(
            callable,
            kind=kind,
            caller_port=caller_port,
            remove_sources=remove_sources,
            remove_sinks=remove_sinks,
            remove_tito=remove_tito,
            remove_tito_positions=remove_tito_positions,
            remove_features=remove_features,
            remove_leaf_names=remove_leaf_names,
        )
    )


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
    print_json(get_issues(callable))


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
        (print_json, "print_json({'a': 'b'})"),
    ]
    max_width = max(len(command[1]) for command in commands)
    for command, example in commands:
        doc = textwrap.dedent(command.__doc__ or "")
        doc = textwrap.indent(doc, prefix=" " * (max_width + 3)).strip()
        print(f"  {example:<{max_width}} {doc}")


if __name__ == "__main__":
    print_help()
