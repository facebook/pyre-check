# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
import logging
import os
import platform
import subprocess
import sys
import time
import traceback
from argparse import Namespace
from enum import Enum
from typing import Dict, Optional

from .configuration import Configuration  # noqa


LOG: logging.Logger = logging.getLogger(__name__)


class LoggerCategory(Enum):
    PERFORMANCE = "perfpipe_pyre_performance"
    USAGE = "perfpipe_pyre_usage"
    ANNOTATION_COUNTS = "perfpipe_pyre_annotation_counts"
    STRICT_ADOPTION = "perfpipe_pyre_strict_adoption"
    FIXME_COUNTS = "perfpipe_pyre_fixme_counts"
    QUALITY_ANALYZER = "perfpipe_pyre_quality_analyzer"
    QUALITY_ANALYZER_ISSUES = "perfpipe_pyre_quality_analyser_issues"


def log(
    category: LoggerCategory,
    arguments: Optional[Namespace] = None,
    configuration: Optional["Configuration"] = None,
    integers: Optional[Dict[str, int]] = None,
    normals: Optional[Dict[str, Optional[str]]] = None,
    logger: Optional[str] = None,
) -> None:
    integers = integers or {}
    if "time" not in integers:
        integers["time"] = int(time.time())
    normals = normals or {}
    if configuration:
        # pyre-fixme[9]: normals has type `Dict[str, str]`; used as `Union[Dict[str,
        #  Optional[str]], Dict[str, str]]`.
        normals: Dict[str, str] = {**normals, "version": configuration.version_hash}
        if not logger:
            logger = configuration.logger
    if not logger:
        raise ValueError("Logger must either be given or in configuration")
    if arguments:
        normals = {**normals, "arguments": str(arguments)}
    try:
        statistics = {
            "int": integers,
            "normal": {
                **normals,
                "command_line": " ".join(sys.argv),
                "host": platform.node() or "",
                "platform": platform.system() or "",
                "user": os.getenv("USER", ""),
            },
        }
        statistics = json.dumps(statistics).encode("ascii", "strict")
        subprocess.run([logger, category.value], input=statistics)
    except Exception:
        LOG.warning("Unable to log using `%s`", logger)
        LOG.info(traceback.format_exc())
