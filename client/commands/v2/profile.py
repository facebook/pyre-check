# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
import subprocess
from collections import defaultdict
from pathlib import Path
from typing import Dict, List, Tuple

from typing_extensions import Final

from ... import commands, configuration as configuration_module
from . import remote_logging


LOG: logging.Logger = logging.getLogger(__name__)


class StatisticsOverTime:
    _data: List[Tuple[str, int]] = []

    def add(self, line: str) -> None:
        dividers = [
            " MEMORY Shared memory size (size: ",
            " MEMORY Shared memory size post-typecheck (size: ",
        ]
        for divider in dividers:
            if divider in line:
                time, size_component = line.split(divider)
                size_in_megabytes = int(size_component[:-2])
                size_in_bytes = size_in_megabytes * (10 ** 6)
                self._data.append((time, size_in_bytes))

    def graph_total_shared_memory_size_over_time(self) -> None:
        try:
            gnuplot = subprocess.Popen(["gnuplot"], stdin=subprocess.PIPE)
            # pyre-fixme[16]: `Optional` has no attribute `write`.
            gnuplot.stdin.write(b"set term dumb 140 25\n")
            gnuplot.stdin.write(b"plot '-' using 1:2 title '' with linespoints \n")
            for (i, (_time, size)) in enumerate(self._data):
                # This is graphing size against # of updates, not time
                gnuplot.stdin.write(b"%f %f\n" % (i, size))
            gnuplot.stdin.write(b"e\n")
            # pyre-fixme[16]: `Optional` has no attribute `flush`.
            gnuplot.stdin.flush()
        except FileNotFoundError:
            LOG.error("gnuplot is not installed")

    def to_json(self) -> str:
        return json.dumps(self._data)


class TableStatistics:
    # category -> aggregation -> table name -> value
    # pyre-ignore: T62493941
    _data: Dict[str, Dict[str, Dict[str, str]]] = defaultdict(lambda: defaultdict(dict))
    _shared_heap_category: Final = "bytes serialized into shared heap"

    @staticmethod
    def sort_by_value(items: List[Tuple[str, str]]) -> None:
        def parse(number: str) -> float:
            if number[-1] == "G":
                return float(number[:-1]) * (10 ** 9)
            if number[-1] == "M":
                return float(number[:-1]) * (10 ** 6)
            if number[-1] == "K":
                return float(number[:-1]) * (10 ** 3)
            return float(number)

        items.sort(key=lambda x: parse(x[1]), reverse=True)

    def add(self, line: str) -> None:
        divider = "stats -- "
        if divider in line:
            header, data = line.split(divider)
            cells = data[:-2].split(", ")
            collected = [cell.split(": ") for cell in cells]
            tag_and_category = header[:-2].split(" (")
            if len(tag_and_category) == 2:
                tag, category = tag_and_category
            elif header[:3] == "ALL":
                tag = "ALL"
                category = header[4:-1]
            elif header[:4] == "(ALL":
                tag = "ALL"
                category = header[5:-2]
            else:
                return
            if len(tag) > 0:
                for key, value in collected:
                    self._data[category][key][tag] = value

    def is_empty(self) -> bool:
        return len(self._data) == 0

    def get_totals(self) -> List[Tuple[str, str]]:
        totals = list(self._data[self._shared_heap_category]["total"].items())
        TableStatistics.sort_by_value(totals)
        return totals

    def get_counts(self) -> List[Tuple[str, str]]:
        counts = list(self._data[self._shared_heap_category]["samples"].items())
        TableStatistics.sort_by_value(counts)
        return counts


def _get_server_log(log_directory: Path) -> Path:
    server_stderr_path = log_directory / "new_server" / "server.stderr"
    if not server_stderr_path.is_file():
        raise RuntimeError(f"Cannot find server output at `{server_stderr_path}`.")
    return server_stderr_path


def _collect_memory_statistics_over_time(log_directory: Path) -> StatisticsOverTime:
    server_log = _get_server_log(log_directory)
    extracted = StatisticsOverTime()
    with open(server_log) as server_log_file:
        for line in server_log_file.readlines():
            extracted.add(line)
    return extracted


def print_individual_table_sizes(
    configuration: configuration_module.Configuration,
) -> None:
    server_log = _get_server_log(Path(configuration.log_directory))
    extracted = TableStatistics()
    with open(str(server_log)) as server_log_file:
        for line in server_log_file.readlines():
            extracted.add(line)
    if extracted.is_empty():
        raise RuntimeError(
            "Cannot find table size data in "
            + f"`{server_log.as_posix()}`. "
            + "Please run Pyre with `--debug` option first."
        )
    sizes = json.dumps(extracted.get_totals())
    counts = json.dumps(extracted.get_counts())
    # I manually put together this json in order to be
    # simultaneously machine and human readable
    combined = (
        "{\n"
        + f'  "total_table_sizes": {sizes},\n'
        + f'  "table_key_counts": {counts}\n'
        + "}"
    )
    print(combined)


def print_total_shared_memory_size_over_time(
    configuration: configuration_module.Configuration,
) -> None:
    memory_over_time = _collect_memory_statistics_over_time(
        Path(configuration.log_directory)
    ).to_json()
    print(memory_over_time)


def print_total_shared_memory_size_over_time_graph(
    configuration: configuration_module.Configuration,
) -> None:
    statistics = _collect_memory_statistics_over_time(Path(configuration.log_directory))
    statistics.graph_total_shared_memory_size_over_time()


def run_profile(
    configuration: configuration_module.Configuration, output: commands.ProfileOutput
) -> commands.ExitCode:
    if output == commands.ProfileOutput.INDIVIDUAL_TABLE_SIZES:
        print_individual_table_sizes(configuration)
    elif output == commands.ProfileOutput.TOTAL_SHARED_MEMORY_SIZE_OVER_TIME:
        print_total_shared_memory_size_over_time(configuration)
    elif output == commands.ProfileOutput.TOTAL_SHARED_MEMORY_SIZE_OVER_TIME_GRAPH:
        print_total_shared_memory_size_over_time_graph(configuration)
    else:
        LOG.info("Coming soon...")
    return commands.ExitCode.SUCCESS


@remote_logging.log_usage(command_name="profile")
def run(
    configuration: configuration_module.Configuration, output: commands.ProfileOutput
) -> commands.ExitCode:
    try:
        return run_profile(configuration, output)
    except Exception as error:
        raise commands.ClientException(
            f"Exception occured during profile: {error}"
        ) from error
