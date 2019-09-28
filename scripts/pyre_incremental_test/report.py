# pyre-strict

import json
import logging
import subprocess
import time
from typing import Any, Dict, Optional, Sequence

from .batch import RunnerResult


LOG: logging.Logger = logging.getLogger(__name__)

CONSOLE: str = "console"
SCUBA: str = "scuba"


def to_console(results: Sequence[RunnerResult], dont_show_discrepancy: bool) -> None:
    print(
        json.dumps(
            [result.to_json(dont_show_discrepancy) for result in results], indent=2
        )
    )


def to_scuba(results: Sequence[RunnerResult], identifier: Optional[str]) -> None:
    def to_scuba_sample(result: RunnerResult) -> Dict[str, Any]:
        normals = {
            "input": json.dumps(result.input.to_json()),
            "status": result.get_status(),
        }
        if identifier is not None:
            normals["identifier"] = identifier

        integers = {"time": int(time.time())}
        output = result.output
        if output is not None:
            integers["full_check_time"] = output.full_check_time
            integers["incremental_check_time"] = output.incremental_check_time

        return {"normal": normals, "int": integers}

    LOG.info(f"Sending {len(results)} results to scuba...")
    for result in results:
        sample = to_scuba_sample(result)
        subprocess.run(
            ["scribe_cat", "perfpipe_pyre_incremental_test_result"],
            input=json.dumps(sample),
            universal_newlines=True,
        )
