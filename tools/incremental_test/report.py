# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
import subprocess
import time
from typing import Any, Dict, Optional, Sequence

from .batch import RunnerResult, Sample


LOG: logging.Logger = logging.getLogger(__name__)


def to_console(results: Sequence[RunnerResult], dont_show_discrepancy: bool) -> None:
    print(
        json.dumps(
            [result.to_json(dont_show_discrepancy) for result in results], indent=2
        )
    )


def to_logger(
    logger: str, results: Sequence[RunnerResult], identifier: Optional[str]
) -> None:
    def expand_sample(sample: Sample) -> Dict[str, Any]:
        normals = {**sample.normals}
        if identifier is not None:
            normals["identifier"] = identifier
        integers = {"time": int(time.time()), **sample.integers}
        return {"normal": normals, "int": integers}

    LOG.info(f"Sending {len(results)} results to {logger}...")
    for result in results:
        sample = expand_sample(result.to_logger_sample())
        subprocess.run(
            [logger, "perfpipe_pyre_incremental_test_result"],
            input=json.dumps(sample),
            universal_newlines=True,
        )
