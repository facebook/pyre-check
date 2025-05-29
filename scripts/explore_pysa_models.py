#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
Explore taint models interactively.

This script can be used to debug false positives and false negatives in the
taint analysis. See https://pyre-check.org/docs/pysa-explore/ for the documentation.
"""

import collections
import copy
import enum
import io
import itertools
import json
import multiprocessing
import pickle
import re
import subprocess
import textwrap
import time
from dataclasses import dataclass
from pathlib import Path
from typing import (
    Any,
    Callable,
    Dict,
    Iterable,
    List,
    NamedTuple,
    Optional,
    Set,
    Tuple,
    TypedDict,
    Union,
)


class FilePosition(NamedTuple):
    file_index: int
    offset: int
    length: int


class AnalysisOutputIndex(NamedTuple):
    models: Dict[str, FilePosition] = {}
    issues: Dict[str, List[FilePosition]] = {}
    call_graphs: Dict[str, FilePosition] = {}

    def update(self, index: "AnalysisOutputIndex") -> None:
        self.models.update(index.models)
        self.issues.update(index.issues)
        self.call_graphs.update(index.call_graphs)


class AnalysisOutputDirectory(NamedTuple):
    files: List[Path]
    handles: List[io.BufferedReader]
    index_: AnalysisOutputIndex


__current_directory: Optional[AnalysisOutputDirectory] = None
__warned_missing_jq: bool = False


def _iter_with_offset(lines: Iterable[bytes]) -> Iterable[Tuple[bytes, int]]:
    offset = 0
    for line in lines:
        yield (line, offset)
        offset += len(line)


def index_json_output_file(arguments: Tuple[int, Path]) -> AnalysisOutputIndex:
    start_time = time.time()
    file_index, file_path = arguments
    index = AnalysisOutputIndex()

    print(f"Indexing {file_path}")
    with open(file_path, "rb") as handle:
        for line, offset in _iter_with_offset(handle):
            try:
                message = json.loads(line)
            except UnicodeDecodeError:
                print(f"ERROR: Unicode Decode Error when parsing: {line}")
                continue

            if "kind" not in message:
                continue

            file_position = FilePosition(
                file_index=file_index, offset=offset, length=len(line)
            )
            kind = message["kind"]
            if kind == "model":
                callable = message["data"]["callable"]
                assert callable not in index.models
                index.models[callable] = file_position
            elif kind == "issue":
                callable = message["data"]["callable"]
                if callable not in index.issues:
                    index.issues[callable] = []
                index.issues[callable].append(file_position)
            elif kind == "call_graph":
                callable = message["data"]["callable"]
                index.call_graphs[callable] = file_position
            else:
                raise AssertionError(f"Unexpected kind `{kind}` in `{file_path}`")

    duration = time.time() - start_time
    print(f"Indexed {file_path} in {duration:.2f}s")
    return index


def index(path: str = ".") -> None:
    """Index all available results in the given analysis output directory."""

    taint_output_directory = Path(path)
    if not taint_output_directory.is_dir():
        raise AssertionError(f"No such directory `{path}`")

    json_output_files: List[Path] = []
    for filepath in taint_output_directory.iterdir():
        if (
            filepath.is_file()
            and filepath.suffix == ".json"
            and (
                filepath.name.startswith("taint-output")
                or filepath.name.startswith("call-graph")
            )
        ):
            json_output_files.append(filepath)

    if len(json_output_files) == 0:
        raise AssertionError(f"Could not find taint output files in `{path}`")

    with multiprocessing.Pool() as pool:
        index = AnalysisOutputIndex()
        for new_index in pool.imap_unordered(
            index_json_output_file, enumerate(json_output_files), chunksize=1
        ):
            index.update(new_index)

    print(f"Indexed {len(index.models)} models")

    global __current_directory
    __current_directory = AnalysisOutputDirectory(
        files=json_output_files,
        handles=[open(path, "rb") for path in json_output_files],
        index_=index,
    )


def _assert_loaded() -> AnalysisOutputDirectory:
    current_directory = __current_directory
    if current_directory is None:
        raise AssertionError("call index() first")
    return current_directory


def save_index(path: Optional[str] = None) -> None:
    directory = _assert_loaded()

    if path is None:
        path = "pysa-model-explorer-index.pickle"

    print(f"Saving index to `{path}`")
    with open(path, "wb") as f:
        pickle.dump(
            AnalysisOutputDirectory(
                files=directory.files, handles=[], index_=directory.index_
            ),
            f,
        )


def load_index(path: Optional[str] = None) -> None:
    if path is None:
        path = "pysa-model-explorer-index.pickle"

    print(f"Loading index from `{path}`")
    with open(path, "rb") as f:
        directory = pickle.load(f)

    global __current_directory
    __current_directory = AnalysisOutputDirectory(
        files=directory.files,
        handles=[open(path, "rb") for path in directory.files],
        index_=directory.index_,
    )


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


def _filter_taint_conditions(
    taint_conditions: List[Dict[str, Any]],
    frame_predicate: Callable[[str, Dict[str, Any]], bool],
) -> List[Dict[str, Any]]:
    new_taint_conditions = []
    for condition in taint_conditions:
        caller_port = condition["port"]
        new_local_taints = []
        for local_taint in condition["taint"]:
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
            new_condition = condition.copy()
            new_condition["taint"] = new_local_taints
            new_taint_conditions.append(new_condition)

    return new_taint_conditions


def filter_model(
    model: Dict[str, Any], frame_predicate: Callable[[str, Dict[str, Any]], bool]
) -> Dict[str, Any]:
    model = model.copy()
    model["sources"] = _filter_taint_conditions(
        model.get("sources", []), frame_predicate
    )
    model["sinks"] = _filter_taint_conditions(model.get("sinks", []), frame_predicate)
    model["tito"] = _filter_taint_conditions(model.get("tito", []), frame_predicate)
    return model


def filter_model_caller_port(model: Dict[str, Any], port: str) -> Dict[str, Any]:
    def predicate(caller_port: str, frame: Dict[str, Any]) -> bool:
        return port == caller_port

    return filter_model(model, predicate)


def filter_model_kind(model: Dict[str, Any], kind: str) -> Dict[str, Any]:
    def predicate(caller_port: str, frame: Dict[str, Any]) -> bool:
        return frame["kind"] == kind

    return filter_model(model, predicate)


def _map_taint_conditions(
    taint_conditions: List[Dict[str, Any]],
    frame_map: Callable[[str, Dict[str, Any]], None],
    local_taint_map: Callable[[str, Dict[str, Any]], None],
) -> List[Dict[str, Any]]:
    taint_conditions = copy.deepcopy(taint_conditions)

    for condition in taint_conditions:
        caller_port = condition["port"]
        for local_taint in condition["taint"]:
            local_taint_map(caller_port, local_taint)
            for frame in local_taint["kinds"]:
                frame_map(caller_port, frame)

    return taint_conditions


def map_model(
    model: Dict[str, Any],
    frame_map: Callable[[str, Dict[str, Any]], None] = lambda x, y: None,
    local_taint_map: Callable[[str, Dict[str, Any]], None] = lambda x, y: None,
) -> Dict[str, Any]:
    model = model.copy()
    model["sources"] = _map_taint_conditions(
        model.get("sources", []), frame_map, local_taint_map
    )
    model["sinks"] = _map_taint_conditions(
        model.get("sinks", []), frame_map, local_taint_map
    )
    model["tito"] = _map_taint_conditions(
        model.get("tito", []), frame_map, local_taint_map
    )
    return model


def map_issue_traces(
    issue: Dict[str, Any],
    frame_map: Callable[[str, Dict[str, Any]], None] = lambda x, y: None,
    local_taint_map: Callable[[str, Dict[str, Any]], None] = lambda x, y: None,
) -> Dict[str, Any]:
    issue = copy.deepcopy(issue)

    for trace in issue["traces"]:
        condition = trace["name"]
        for root in trace["roots"]:
            local_taint_map(condition, root)
            for frame in root["kinds"]:
                frame_map(condition, frame)

    return issue


def model_remove_tito_positions(model: Dict[str, Any]) -> Dict[str, Any]:
    def local_taint_map(caller_port: str, local_taint: Dict[str, Any]) -> None:
        if "tito_positions" in local_taint:
            del local_taint["tito_positions"]

    return map_model(model, local_taint_map=local_taint_map)


def issue_remove_tito_positions(issue: Dict[str, Any]) -> Dict[str, Any]:
    def local_taint_map(condition: str, local_taint: Dict[str, Any]) -> None:
        if "tito_positions" in local_taint:
            del local_taint["tito_positions"]

    return map_issue_traces(issue, local_taint_map=local_taint_map)


def model_remove_class_intervals(model: Dict[str, Any]) -> Dict[str, Any]:
    def local_taint_map(caller_port: str, local_taint: Dict[str, Any]) -> None:
        if "receiver_interval" in local_taint:
            del local_taint["receiver_interval"]
        if "caller_interval" in local_taint:
            del local_taint["caller_interval"]
        if "is_self_call" in local_taint:
            del local_taint["is_self_call"]

    return map_model(model, local_taint_map=local_taint_map)


def issue_remove_class_intervals(issue: Dict[str, Any]) -> Dict[str, Any]:
    def local_taint_map(condition: str, local_taint: Dict[str, Any]) -> None:
        if "receiver_interval" in local_taint:
            del local_taint["receiver_interval"]
        if "caller_interval" in local_taint:
            del local_taint["caller_interval"]
        if "is_self_call" in local_taint:
            del local_taint["is_self_call"]

    return map_issue_traces(issue, local_taint_map=local_taint_map)


def model_remove_features(model: Dict[str, Any]) -> Dict[str, Any]:
    def frame_map(caller_port: str, frame: Dict[str, Any]) -> None:
        if "features" in frame:
            del frame["features"]

    def local_taint_map(caller_port: str, local_taint: Dict[str, Any]) -> None:
        if "local_features" in local_taint:
            del local_taint["local_features"]

    return map_model(model, frame_map=frame_map, local_taint_map=local_taint_map)


def issue_remove_features(issue: Dict[str, Any]) -> Dict[str, Any]:
    def frame_map(condition: str, frame: Dict[str, Any]) -> None:
        if "features" in frame:
            del frame["features"]

    def local_taint_map(condition: str, local_taint: Dict[str, Any]) -> None:
        if "local_features" in local_taint:
            del local_taint["local_features"]

    return map_issue_traces(issue, frame_map=frame_map, local_taint_map=local_taint_map)


def model_remove_leaf_names(model: Dict[str, Any]) -> Dict[str, Any]:
    def frame_map(caller_port: str, frame: Dict[str, Any]) -> None:
        if "leaves" in frame:
            del frame["leaves"]

    return map_model(model, frame_map=frame_map)


def issue_remove_leaf_names(issue: Dict[str, Any]) -> Dict[str, Any]:
    def frame_map(condition: str, frame: Dict[str, Any]) -> None:
        if "leaves" in frame:
            del frame["leaves"]

    return map_issue_traces(issue, frame_map=frame_map)


@dataclass
class FormattingOptions:
    format: str = "json"
    show_sources: bool = True
    show_sinks: bool = True
    show_tito: bool = True
    show_tito_positions: bool = True
    show_class_intervals: bool = True
    show_features: bool = True
    show_leaf_names: bool = True
    kind: Optional[str] = None
    caller_port: Optional[str] = None

    def apply_options(self, **kwargs: Union[bool, str]) -> "FormattingOptions":
        options = copy.copy(self)
        for name, value in kwargs.items():
            if not hasattr(options, name):
                raise AssertionError(f"Unknown formatting option `{name}`")
            setattr(options, name, value)
        return options


__default_formatting_options: FormattingOptions = FormattingOptions(
    show_tito_positions=False,
    show_class_intervals=False,
    show_features=False,
    show_leaf_names=False,
)


def set_formatting(**kwargs: Union[str, bool]) -> None:
    """
    Set default formatting options.
    Available options with their default values:
      format = 'json'            Display format ('json' or 'text')
      kind = None                Filter by taint kind.
      caller_port = None         Filter by caller port.
      show_sources = True
      show_sinks = True
      show_tito = True
      show_tito_positions = False
      show_class_intervals = False
      show_features = False
      show_leaf_names = False
    Most functions accept formatting options as optional arguments.
    """
    global __default_formatting_options
    __default_formatting_options = __default_formatting_options.apply_options(**kwargs)


def show_formatting() -> None:
    """Show default formatting options."""
    print(__default_formatting_options)


def get_raw_model(
    callable: str, cache: Optional[Dict[str, Dict[str, Any]]] = None
) -> Dict[str, Any]:
    """Get the model for the given callable."""
    if cache is not None and callable in cache:
        return cache[callable]

    directory = _assert_loaded()

    if callable not in directory.index_.models:
        raise AssertionError(f"no model for callable `{callable}`.")

    message = json.loads(_read(directory.index_.models[callable]))
    assert message["kind"] == "model"
    model = message["data"]

    if cache is not None:
        cache[callable] = model

    return model


def get_model(
    callable: str,
    **kwargs: Union[str, bool],
) -> Dict[str, Any]:
    """Get the model for the given callable (with formatting options)"""
    model = get_raw_model(callable)

    options = __default_formatting_options.apply_options(**kwargs)
    if not options.show_sources and "sources" in model:
        del model["sources"]
    if not options.show_sinks and "sinks" in model:
        del model["sinks"]
    if not options.show_tito and "tito" in model:
        del model["tito"]
    if options.kind is not None:
        model = filter_model_kind(model, options.kind)
    if options.caller_port is not None:
        model = filter_model_caller_port(model, options.caller_port)
    if not options.show_tito_positions:
        model = model_remove_tito_positions(model)
    if not options.show_class_intervals:
        model = model_remove_class_intervals(model)
    if not options.show_features:
        model = model_remove_features(model)
    if not options.show_leaf_names:
        model = model_remove_leaf_names(model)
    return model


def print_json(data: object) -> None:
    """Pretty print json objects with syntax highlighting."""
    if isinstance(data, str):
        data = json.loads(data)

    try:
        subprocess.run(["jq", "-C"], input=json.dumps(data).encode(), check=True)
    except FileNotFoundError:
        print(json.dumps(data, indent=" " * 2))

        global __warned_missing_jq
        if not __warned_missing_jq:
            print(
                "[HINT] Install `jq` to use syntax highlighting, https://stedolan.github.io/jq/"
            )
            __warned_missing_jq = True


def green(text: str | int) -> str:
    return f"\033[32m{text}\033[0m"


def blue(text: str | int) -> str:
    return f"\033[34m{text}\033[0m"


def feature_to_string(feature: Union[str, Dict[str, str]]) -> str:
    if isinstance(feature, str):
        return feature
    elif isinstance(feature, dict):
        if len(feature) == 1:
            key, value = next(iter(feature.items()))
            return f"{key}:{value}"
        else:
            raise AssertionError(f"unexpected feature: {feature}")
    else:
        raise AssertionError(f"unexpected feature: {feature}")


def leaf_name_to_string(leaf: Dict[str, str]) -> str:
    name = leaf["name"]
    if "port" in leaf:
        name += f':{leaf["port"]}'
    return name


class SourceLocationWithFilename(NamedTuple):
    filename: str
    path: Optional[str]
    line: int
    start: int
    end: int

    def print(self, prefix: str, indent: str) -> None:
        filename = self.filename
        path = self.path
        if filename == "*" and path is not None:
            filename = path
        print(f"{indent}{prefix}{blue(filename)}:{blue(self.line)}:{blue(self.start)}")


class SourceLocation(NamedTuple):
    line: int
    start: int
    end: int

    @staticmethod
    def from_json(json: Dict[str, int]) -> "SourceLocation":
        return SourceLocation(line=json["line"], start=json["start"], end=json["end"])

    def add_filename(
        self, filename: str, path: Optional[str]
    ) -> SourceLocationWithFilename:
        return SourceLocationWithFilename(
            filename=filename, path=path, line=self.line, start=self.start, end=self.end
        )


def print_filename(json: Dict[str, str], prefix: str, indent: str) -> None:
    filename = json["filename"]
    path = json.get("path")
    if filename == "*" and path is not None:
        print(f"{indent}{prefix}{blue(path)}")
    else:
        print(f"{indent}{prefix}{blue(filename)}")


def print_json_location(
    location: Dict[str, Union[str, int]], prefix: str, indent: str
) -> None:
    filename = location.get("filename")
    path = location.get("path")
    line = location["line"]
    start = location["start"]
    if filename is not None and filename != "*":
        print(f"{indent}{prefix}{blue(filename)}:{blue(line)}:{blue(start)}")
    elif path is not None:
        print(f"{indent}{prefix}{blue(path)}:{blue(line)}:{blue(start)}")
    else:
        print(f"{indent}{prefix}{blue(line)}:{blue(start)}")


def print_call_info(local_taint: Dict[str, Any], indent: str) -> None:
    if "call" in local_taint:
        call = local_taint["call"]
        print(f'{indent}CalleePort: {green(call["port"])}')
        for resolve_to in call["resolves_to"]:
            print(f"{indent}Callee: {blue(resolve_to)}")
        print_json_location(call["position"], prefix="Location: ", indent=indent)
    elif "origin" in local_taint:
        print_json_location(
            local_taint["origin"], prefix="Origin: Location: ", indent=indent
        )
    elif "declaration" in local_taint:
        print(f"{indent}Declaration:")
    elif "tito" in local_taint:
        print(f"{indent}Tito:")
    else:
        raise AssertionError("unexpected call info")


def print_local_taint(local_taint: Dict[str, Any], indent: str) -> None:
    if "receiver_interval" in local_taint:
        print(f'{indent}ReceiverInterval: {local_taint["receiver_interval"]}')
    if "caller_interval" in local_taint:
        print(f'{indent}CallerInterval: {local_taint["caller_interval"]}')
    if "is_self_call" in local_taint:
        print(f'{indent}IsSelfCall: {local_taint["is_self_call"]}')
    if "tito_positions" in local_taint:
        positions = ", ".join(
            f'{position["line"]}:{position["start"]}:{position["end"]}'
            for position in local_taint["tito_positions"]
        )
        print(f"{indent}TitoPositions: {positions}")
    if "local_features" in local_taint:
        features = ", ".join(
            blue(feature_to_string(feature))
            for feature in local_taint["local_features"]
        )
        print(f"{indent}LocalFeatures: {features}")


def print_frame(frame: Dict[str, Any], indent: str) -> None:
    if "return_paths" in frame:
        # Special case for taint-in-taint-out
        for return_path, collapse_depth in frame["return_paths"].items():
            print(
                f'{indent}{green(frame["kind"])}: '
                f"ReturnPath {green(return_path)} "
                f"CollapseDepth {blue(collapse_depth)} "
                f'Distance {blue(frame.get("length", 0))}'
            )
    else:
        print(
            f'{indent}{green(frame["kind"])}: Distance {blue(frame.get("length", 0))}'
        )

    if "features" in frame:
        features = ", ".join(
            blue(feature_to_string(feature)) for feature in frame["features"]
        )
        print(f"{indent}  Features: {features}")

    if "leaves" in frame:
        leaves = ", ".join(blue(leaf_name_to_string(leaf)) for leaf in frame["leaves"])
        print(f"{indent}  Leaves: {leaves}")


def print_taint_conditions(conditions: List[Dict[str, Any]], is_tito: bool) -> None:
    for condition in conditions:
        label = "CallerPort" if not is_tito else "ParameterPath"
        print(f'  {label}: {green(condition["port"])}')
        for local_taint in condition["taint"]:
            print_call_info(local_taint, indent=" " * 4)
            print_local_taint(local_taint, indent=" " * 4)

            for frame in local_taint["kinds"]:
                print_frame(frame, indent=" " * 6)


def print_model(
    callable: str,
    **kwargs: Union[str, bool],
) -> None:
    """Pretty print the model for the given callable."""
    model = get_model(callable, **kwargs)

    options = __default_formatting_options.apply_options(**kwargs)
    if options.format == "json":
        print_json(model)
    elif options.format == "text":
        print(f"Model for {green(model['callable'])}")
        print_filename(model, prefix="Location: ", indent="")
        print("Sources:")
        print_taint_conditions(model.get("sources", []), is_tito=False)
        print("Sinks:")
        print_taint_conditions(model.get("sinks", []), is_tito=False)
        print("Tito:")
        print_taint_conditions(model.get("tito", []), is_tito=True)
        if "global_sanitizer" in model:
            print(f"GlobalSanitizers: {model['global_sanitizer']}")
        if "parameters_sanitizer" in model:
            print(f"ParametersSanitizer: {model['parameters_sanitizer']}")
        if "sanitizers" in model:
            print(f"Sanitizers: {model['sanitizers']}")
        if "modes" in model:
            modes = ", ".join(green(mode) for mode in model["modes"])
            print(f"Modes: {modes}")
    else:
        raise AssertionError(f"Unexpected format `{options.format}`")


class ConditionKind(enum.Enum):
    SOURCE = 0
    SINK = 1

    @staticmethod
    def from_string(s: str) -> Optional["ConditionKind"]:
        if s == "source":
            return ConditionKind.SOURCE
        elif s == "sink":
            return ConditionKind.SINK
        else:
            return None

    def model_key(self) -> str:
        if self == ConditionKind.SOURCE:
            return "sources"
        else:
            return "sinks"


class TaintFrame(NamedTuple):
    condition_kind: ConditionKind
    caller: str
    caller_port: str
    callee: Optional[str]
    callee_port: Optional[str]
    taint_kind: str
    distance: Optional[int]  # None for subtraces.
    location: SourceLocationWithFilename
    shared_local_features: List[Dict[str, str]]
    local_features: List[Dict[str, str]]
    type_interval: Dict[str, Any]

    def key(
        self,
    ) -> Tuple[
        ConditionKind,
        str,
        str,
        Optional[str],
        Optional[str],
        str,
        SourceLocationWithFilename,
        str,
    ]:
        return (
            self.condition_kind,
            self.caller,
            self.caller_port,
            self.callee,
            self.callee_port,
            self.taint_kind,
            self.location,
            str(self.type_interval),
        )


def get_frames_from_extra_traces(
    caller: str, filename: str, path: Optional[str], extra_traces: List[Dict[str, Any]]
) -> Iterable[TaintFrame]:
    for extra_trace in extra_traces:
        if extra_trace["trace_kind"] == "source":
            condition_kind = ConditionKind.SOURCE
        elif extra_trace["trace_kind"] == "sink":
            condition_kind = ConditionKind.SINK
        else:
            raise AssertionError(f'unexpected trace_kind: {extra_trace["trace_kind"]}')

        if "call" in extra_trace:
            call = extra_trace["call"]
            for resolved in call["resolves_to"]:
                yield TaintFrame(
                    condition_kind=condition_kind,
                    caller=caller,
                    caller_port="subtrace",
                    callee=resolved,
                    callee_port=call["port"],
                    taint_kind=extra_trace["leaf_kind"],
                    distance=None,
                    location=SourceLocation.from_json(call["position"]).add_filename(
                        filename, path
                    ),
                    shared_local_features=[],
                    local_features=[],
                    type_interval={},
                )


def get_frames_from_local_taints(
    caller: str,
    filename: str,
    path: Optional[str],
    condition_kind: ConditionKind,
    port: str,
    local_taints: List[Dict[str, Any]],
    include_subtraces: bool = False,
    deduplicate: bool = True,
) -> Iterable[TaintFrame]:
    for local_taint in local_taints:
        if include_subtraces and deduplicate:
            yield from get_frames_from_extra_traces(
                caller, filename, path, local_taint.get("extra_traces", [])
            )

        if "origin" in local_taint:
            for flow_details in local_taint.get("kinds", []):
                if include_subtraces and deduplicate:
                    yield from get_frames_from_extra_traces(
                        caller, filename, path, flow_details.get("extra_traces", [])
                    )

                for leaf in flow_details.get("leaves", [{}]):
                    if include_subtraces and not deduplicate:
                        # subtraces are attached to a taint frame, so those will be duplicated
                        yield from get_frames_from_extra_traces(
                            caller, filename, path, local_taint.get("extra_traces", [])
                        )
                        yield from get_frames_from_extra_traces(
                            caller, filename, path, flow_details.get("extra_traces", [])
                        )
                    yield TaintFrame(
                        condition_kind=condition_kind,
                        caller=caller,
                        caller_port=port,
                        callee=leaf.get("name", None),
                        callee_port=leaf.get("port", None),
                        taint_kind=flow_details["kind"],
                        distance=flow_details.get("length", 0),
                        location=SourceLocation.from_json(
                            local_taint["origin"]
                        ).add_filename(filename, path),
                        shared_local_features=local_taint.get("local_features", []),
                        local_features=flow_details.get("local_features", []),
                        type_interval=local_taint.get("type_interval", {}),
                    )
        elif "call" in local_taint:
            call = local_taint["call"]
            for flow_details in local_taint.get("kinds", []):
                if include_subtraces and deduplicate:
                    yield from get_frames_from_extra_traces(
                        caller, filename, path, flow_details.get("extra_traces", [])
                    )

                for resolved in call.get("resolves_to", []):
                    if include_subtraces and not deduplicate:
                        # subtraces are attached to a taint frame, so those will be duplicated
                        yield from get_frames_from_extra_traces(
                            caller, filename, path, local_taint.get("extra_traces", [])
                        )
                        yield from get_frames_from_extra_traces(
                            caller, filename, path, flow_details.get("extra_traces", [])
                        )
                    yield TaintFrame(
                        condition_kind=condition_kind,
                        caller=caller,
                        caller_port=port,
                        callee=resolved,
                        callee_port=call["port"],
                        taint_kind=flow_details["kind"],
                        distance=flow_details.get("length", 0),
                        location=SourceLocation.from_json(
                            call["position"]
                        ).add_filename(filename, path),
                        shared_local_features=local_taint.get("local_features", []),
                        local_features=flow_details.get("local_features", []),
                        type_interval=local_taint.get("type_interval", {}),
                    )
        elif "declaration" in local_taint:
            pass  # User-declared fragment.
        else:
            raise AssertionError("Unexpected trace fragment.")


def get_frames_from_taint_conditions(
    caller: str,
    filename: str,
    path: Optional[str],
    condition_kind: ConditionKind,
    conditions: List[Dict[str, Any]],
    include_subtraces: bool = False,
    deduplicate: bool = True,
) -> Iterable[TaintFrame]:
    for taint in conditions:
        yield from get_frames_from_local_taints(
            caller,
            filename,
            path,
            condition_kind,
            taint["port"],
            taint["taint"],
            include_subtraces=include_subtraces,
            deduplicate=deduplicate,
        )


def print_model_size_stats(callable: str) -> None:
    """Print statistics about a model size (number of frames, etc.)"""
    model = get_raw_model(callable)
    trace_frames = 0
    trace_frames_for_subtraces = 0
    source_trace_frames = 0
    sink_trace_frames = 0
    trace_frames_per_kind = collections.defaultdict(int)
    trace_frames_per_callee = collections.defaultdict(int)
    for frame in get_frames_from_taint_conditions(
        callable,
        model["filename"],
        model.get("path"),
        ConditionKind.SOURCE,
        model.get("sources", []),
        include_subtraces=True,
        deduplicate=False,
    ):
        trace_frames += 1
        if frame.caller_port == "subtrace":
            trace_frames_for_subtraces += 1
        else:
            source_trace_frames += 1
        trace_frames_per_kind[frame.taint_kind] += 1
        trace_frames_per_callee[frame.callee] += 1
    for frame in get_frames_from_taint_conditions(
        callable,
        model["filename"],
        model.get("path"),
        ConditionKind.SINK,
        model.get("sinks", []),
        include_subtraces=True,
        deduplicate=False,
    ):
        trace_frames += 1
        if frame.caller_port == "subtrace":
            trace_frames_for_subtraces += 1
        else:
            sink_trace_frames += 1
        trace_frames_per_kind[frame.taint_kind] += 1
        trace_frames_per_callee[frame.callee] += 1

    print(f"Statistics of model for callable `{callable}`:")
    print(f"Trace frames: {trace_frames}")
    print(f"Trace frames for subtraces: {trace_frames_for_subtraces}")
    print(f"Source trace frames: {source_trace_frames}")
    print(f"Sink trace frames: {sink_trace_frames}")
    print()
    print("Most common taint kinds:")
    for taint_kind, count in sorted(
        trace_frames_per_kind.items(), key=lambda p: p[1], reverse=True
    )[:20]:
        print(f"{taint_kind}: {count} trace frames ({count/trace_frames*100.0:.2f}%)")
    print()
    print("Most common callees:")
    for callee, count in sorted(
        trace_frames_per_callee.items(), key=lambda p: p[1], reverse=True
    )[:20]:
        print(f"{callee}: {count} trace frames ({count/trace_frames*100.0:.2f}%)")


def get_issues(
    callable: Optional[str] = None, **kwargs: Union[str, bool]
) -> List[Dict[str, Any]]:
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

    options = __default_formatting_options.apply_options(**kwargs)
    for index in range(len(issues)):
        # TODO(T138283233): implement filtering by kind on issues.
        if not options.show_tito_positions:
            issues[index] = issue_remove_tito_positions(issues[index])
        if not options.show_class_intervals:
            issues[index] = issue_remove_class_intervals(issues[index])
        if not options.show_features:
            issues[index] = issue_remove_features(issues[index])
            del issues[index]["features"]
        if not options.show_leaf_names:
            issues[index] = issue_remove_leaf_names(issues[index])

    return issues


def print_issue_trace(trace: Dict[str, Any]) -> None:
    for local_taint in trace["roots"]:
        print_call_info(local_taint, indent=" " * 4)
        print_local_taint(local_taint, indent=" " * 4)

        for frame in local_taint["kinds"]:
            print_frame(frame, indent=" " * 6)


def print_issues(callable: str, **kwargs: Union[str, bool]) -> None:
    """Pretty print the issues within the given callable."""
    issues = get_issues(callable, **kwargs)

    options = __default_formatting_options.apply_options(**kwargs)
    if options.format == "json":
        print_json(issues)
    elif options.format == "text":
        print(f"Issues for {green(callable)}")
        for issue in issues:
            print("Issue:")
            print(f'  Code: {issue["code"]}')
            # pyre-ignore: issue contains a location
            print_json_location(issue, "Location: ", indent=" " * 2)
            print(f'  Message: {blue(issue["message"])}')
            print(f'  Handle: {green(issue["master_handle"])}')
            for trace in issue["traces"]:
                print(f'  {trace["name"].capitalize()}:')
                print_issue_trace(trace)
    else:
        raise AssertionError(f"Unexpected format `{options.format}`")


def get_call_graph(callable: str, **kwargs: Union[str, bool]) -> Dict[str, Any]:
    """Get the call graph for the given callable."""
    directory = _assert_loaded()

    if callable not in directory.index_.call_graphs:
        raise AssertionError(f"no call graph for callable `{callable}`.")

    message = json.loads(_read(directory.index_.call_graphs[callable]))
    assert message["kind"] == "call_graph"

    return message["data"]


def print_call_graph(callable: str, **kwargs: Union[str, bool]) -> None:
    """Pretty print the call graph for the given callable."""
    call_graph = get_call_graph(callable, **kwargs)

    # TODO(T138283233): Support format=text
    print_json(call_graph)


def taint_kind_match(a: str, b: str) -> bool:
    return len(a) == len(b) and a.replace("@", ":") == b.replace("@", ":")


def taint_kind_next_hop(kind: str) -> str:
    parts = kind.split("@", 1)
    if len(parts) == 1:
        return kind
    else:
        return parts[1]


def get_closest_next_frame(
    condition_kind: ConditionKind,
    callee: str,
    port: str,
    taint_kind: str,
    seen: Set[TaintFrame] = set(),
) -> Optional[TaintFrame]:
    model = get_raw_model(callee)

    shortest_frame = None
    for frame in get_frames_from_taint_conditions(
        caller=callee,
        filename=model["filename"],
        path=model.get("path"),
        condition_kind=condition_kind,
        conditions=model.get(condition_kind.model_key(), []),
        include_subtraces=False,
        deduplicate=True,
    ):
        if frame.caller_port != port:
            continue
        if not taint_kind_match(frame.taint_kind, taint_kind):
            continue
        if frame.key() in seen:
            continue
        # TODO: match on type interval
        # pyre-ignore: distance is not None
        if shortest_frame is None or (shortest_frame.distance > frame.distance):
            shortest_frame = frame

    return shortest_frame


def print_shortest_trace(
    condition_kind_string: str, callee: str, port: str, taint_kind: str
) -> None:
    """Print the shortest trace starting from the given callable, port, kind"""
    condition_kind = ConditionKind.from_string(condition_kind_string)
    if condition_kind is None:
        print(f"error: expected source or sink as condition kind")
        return

    seen = set()
    while True:
        frame = get_closest_next_frame(condition_kind, callee, port, taint_kind, seen)

        if frame is None:
            print(
                f"error: could not find next frame for callee `{callee}` port `{port}` kind `{taint_kind}`"
            )
            return

        print()
        print(
            f"Callee: {blue(frame.callee or '')} Port: {blue(frame.callee_port or '')} Distance: {frame.distance}"
        )
        frame.location.print(prefix="Location: ", indent="")

        if frame.distance == 0:  # leaf
            return

        seen.add(frame)
        callee = frame.callee or ""
        port = frame.callee_port or ""
        taint_kind = taint_kind_next_hop(frame.taint_kind)


def print_reachable_leaves(
    condition_kind_string: str,
    callable: str,
    taint_kind: str,
    include_subtraces: bool = False,
) -> None:
    condition_kind = ConditionKind.from_string(condition_kind_string)
    if condition_kind is None:
        print(f"error: expected source or sink as condition kind")
        return

    # Find all initial frames
    cache = {}
    stack = []
    model = get_raw_model(callable, cache=cache)

    # we need to iterate on both sources and sinks if `include_subtraces=True`
    for frame in itertools.chain(
        get_frames_from_taint_conditions(
            caller=callable,
            filename=model["filename"],
            path=model.get("path"),
            condition_kind=ConditionKind.SOURCE,
            conditions=model.get("sources", []),
            include_subtraces=include_subtraces,
            deduplicate=True,
        ),
        get_frames_from_taint_conditions(
            caller=callable,
            filename=model["filename"],
            path=model.get("path"),
            condition_kind=ConditionKind.SINK,
            conditions=model.get("sinks", []),
            include_subtraces=include_subtraces,
            deduplicate=True,
        ),
    ):
        if frame.condition_kind != condition_kind:
            continue
        if not taint_kind_match(frame.taint_kind, taint_kind):
            continue
        stack.append(frame)

    seen = set()
    while len(stack) > 0:
        frame = stack.pop()
        if frame.key() in seen:
            continue
        seen.add(frame.key())

        if frame.distance == 0:  # leaf
            print()
            print(
                f"Caller: {blue(frame.caller or '')} Port: {blue(frame.caller_port or '')}"
            )
            print(
                f"Leaf: {blue(frame.callee or '')} Port: {blue(frame.callee_port or '')}"
            )
            frame.location.print(prefix="Location: ", indent="")
            continue

        model = get_raw_model(frame.callee, cache=cache)
        for next_frame in get_frames_from_taint_conditions(
            caller=frame.callee,
            filename=model["filename"],
            path=model.get("path"),
            condition_kind=condition_kind,
            conditions=model.get(condition_kind.model_key(), []),
            include_subtraces=False,
            deduplicate=True,
        ):
            if next_frame.caller_port != frame.callee_port:
                continue
            if not taint_kind_match(
                next_frame.taint_kind, taint_kind_next_hop(frame.taint_kind)
            ):
                continue
            # TODO: match on type interval
            if next_frame.key() in seen:
                continue
            stack.append(next_frame)


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
        (get_call_graph, "get_call_graph('foo.bar')"),
        (print_call_graph, "print_call_graph('foo.bar')"),
        (print_model_size_stats, "print_model_size_stats('foo.bar')"),
        (
            print_shortest_trace,
            "print_shortest_trace('source', 'foo.bar', 'result', 'UserControlled')",
        ),
        (
            print_reachable_leaves,
            "print_reachable_leaves('source', 'foo.bar', 'UserControlled')",
        ),
        (set_formatting, "set_formatting(show_sources=False)"),
        (show_formatting, "show_formatting()"),
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
