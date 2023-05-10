#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
Explore taint models interactively.

This script can be used to debug false positives and false negatives in the
taint analysis. See https://pyre-check.org/docs/pysa-explore/ for the documentation.
"""


import io
import json
import multiprocessing
import re
import subprocess
import textwrap
from pathlib import Path
from typing import Any, Callable, Dict, Iterable, List, NamedTuple, Optional, Tuple


class FilePosition(NamedTuple):
    file_index: int
    offset: int
    length: int


class TaintOutputIndex(NamedTuple):
    models: Dict[str, FilePosition] = {}
    issues: Dict[str, List[FilePosition]] = {}


class TaintOutputDirectory(NamedTuple):
    files: List[Path]
    handles: List[io.BufferedReader]
    index_: TaintOutputIndex


__current_directory: Optional[TaintOutputDirectory] = None
__warned_missing_jq: bool = False


def _iter_with_offset(lines: Iterable[bytes]) -> Iterable[Tuple[bytes, int]]:
    offset = 0
    for line in lines:
        yield (line, offset)
        offset += len(line)


def index_taint_output_file(arguments: Tuple[int, Path]) -> TaintOutputIndex:
    file_index, file_path = arguments
    index = TaintOutputIndex()

    print(f"Indexing {file_path}")
    with open(file_path, "rb") as handle:
        for line, offset in _iter_with_offset(handle):
            message = json.loads(line)
            if "kind" not in message:
                continue

            kind = message["kind"]
            if kind == "model":
                callable = message["data"]["callable"]
                assert callable not in index.models
                index.models[callable] = FilePosition(
                    file_index=file_index, offset=offset, length=len(line)
                )
            elif kind == "issue":
                callable = message["data"]["callable"]
                if callable not in index.issues:
                    index.issues[callable] = []
                index.issues[callable].append(
                    FilePosition(file_index=file_index, offset=offset, length=len(line))
                )
            else:
                raise AssertionError("Unexpected kind `{kind}` in `{file_path}`")

    return index


def index(path: str = ".") -> None:
    """Index all available models in the given taint output directory."""

    taint_output_directory = Path(path)
    if not taint_output_directory.is_dir():
        raise AssertionError(f"No such directory `{path}`")

    taint_output_files: List[Path] = []
    for filepath in taint_output_directory.iterdir():
        if (
            filepath.is_file()
            and filepath.name.startswith("taint-output")
            and filepath.suffix == ".json"
        ):
            taint_output_files.append(filepath)

    if len(taint_output_files) == 0:
        raise AssertionError(f"Could not find taint output files in `{path}`")

    with multiprocessing.Pool() as pool:
        index = TaintOutputIndex()
        for new_index in pool.imap_unordered(
            index_taint_output_file, enumerate(taint_output_files), chunksize=1
        ):
            index.models.update(new_index.models)
            index.issues.update(new_index.issues)

    print(f"Indexed {len(index.models)} models")

    global __current_directory
    __current_directory = TaintOutputDirectory(
        files=taint_output_files,
        handles=[open(path, "rb") for path in taint_output_files],
        index_=index,
    )


def _assert_loaded() -> TaintOutputDirectory:
    current_directory = __current_directory
    if current_directory is None:
        raise AssertionError("call index() first")
    return current_directory


def callables_containing(string: str) -> List[str]:
    """Find all callables containing the given string."""
    directory = _assert_loaded()
    return sorted(filter(lambda name: string in name, directory.index_.models.keys()))


def callables_matching(pattern: str) -> List[str]:
    """Find all callables matching the given regular expression."""
    directory = _assert_loaded()
    regex = re.compile(pattern)
    return sorted(
        filter(lambda name: re.search(regex, name), directory.index_.models.keys())
    )


def _read(position: FilePosition) -> bytes:
    directory = _assert_loaded()
    handle = directory.handles[position.file_index]
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
        if "tito_positions" in local_taint:
            del local_taint["tito_positions"]

    return map_model(model, local_taint_map=local_taint_map)


def model_remove_class_intervals(model: Dict[str, Any]) -> Dict[str, Any]:
    def local_taint_map(caller_port: str, local_taint: Dict[str, Any]) -> None:
        if "receiver_interval" in local_taint:
            del local_taint["receiver_interval"]
        if "caller_interval" in local_taint:
            del local_taint["caller_interval"]
        if "is_self_call" in local_taint:
            del local_taint["is_self_call"]

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
    remove_class_intervals: bool = False,
    remove_features: bool = False,
    remove_leaf_names: bool = False,
) -> Dict[str, Any]:
    """Get the model for the given callable."""
    directory = _assert_loaded()

    if callable not in directory.index_.models:
        raise AssertionError(f"no model for callable `{callable}`.")

    message = json.loads(_read(directory.index_.models[callable]))
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
    if remove_class_intervals:
        model = model_remove_class_intervals(model)
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
    remove_class_intervals: bool = False,
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
      remove_class_intervals=True
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
            remove_class_intervals=remove_class_intervals,
            remove_features=remove_features,
            remove_leaf_names=remove_leaf_names,
        )
    )


def get_issues(callable: Optional[str] = None) -> List[Dict[str, Any]]:
    """
    Get all issues.
    If a callable is provided, only return issues within it.
    """
    directory = _assert_loaded()

    if callable is None:
        callables = directory.index_.issues.items()
    else:
        positions = directory.index_.issues.get(callable, [])
        callables = [(callable, positions)]

    issues = []
    for _, issue_positions in callables:
        for issue_position in issue_positions:
            message = json.loads(_read(issue_position))
            assert message["kind"] == "issue"
            issues.append(message["data"])

    return issues


def print_issues(callable: str) -> None:
    """Pretty print the issues within the given callable."""
    print_json(get_issues(callable))


def print_help() -> None:
    """Print this help message."""
    print("# Pysa Model Explorer")
    print("Available commands:")
    commands = [
        (index, "index('/path/to/results-directory')"),
        (callables_containing, "callables_containing('foo.bar')"),
        (callables_matching, "callables_matching(r'foo\\..*')"),
        (get_model, "get_model('foo.bar')"),
        (print_model, "print_model('foo.bar')"),
        (get_issues, "get_issues('foo.bar')"),
        (print_issues, "print_issues('foo.bar')"),
        (print_json, "print_json({'a': 'b'})"),
        (print_help, "print_help()"),
    ]
    max_width = max(len(command[1]) for command in commands)
    for command, example in commands:
        doc = textwrap.dedent(command.__doc__ or "")
        doc = textwrap.indent(doc, prefix=" " * (max_width + 3)).strip()
        print(f"  {example:<{max_width}} {doc}")


if __name__ == "__main__":
    print_help()
