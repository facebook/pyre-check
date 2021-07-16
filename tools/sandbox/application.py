# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import json
import logging
import subprocess
import tempfile
import textwrap
import threading
from pathlib import Path
from typing import IO, List

from flask import Flask, request, jsonify
from flask_cors import CORS

logging.basicConfig(
    format="%(asctime)s %(levelname)s %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
    level=logging.DEBUG,
)

LOG: logging.Logger = logging.getLogger(__name__)


def _consume(stream: IO[str]) -> str:
    buffer: List[str] = []

    def _consume() -> None:
        while True:
            line = stream.readline()
            if line:
                decoded = line.strip()
                LOG.debug(decoded)
                buffer.append(decoded)
            else:
                break

    thread = threading.Thread(target=_consume)
    thread.start()
    thread.join()

    return "\n".join(buffer)


class Pyre:
    def __init__(self) -> None:
        self._directory: Path = Path(tempfile.mkdtemp())

        LOG.debug(f"Starting server in `{self._directory}`...")
        pyre_configuration = textwrap.dedent(
            """
                {{
                    "source_directories": ["."]
                }}
            """
        )
        LOG.debug(f"Writing configuration:\n{pyre_configuration}")
        pyre_configuration_path = self._directory / ".pyre_configuration"
        pyre_configuration_path.write_text(pyre_configuration)

        LOG.debug("Writing watchman configuration")
        watchman_configuration_path = self._directory / ".watchmanconfig"
        watchman_configuration_path.write_text("{}\n")

        LOG.debug("Starting watchman")
        subprocess.check_call(["watchman", "watch", str(self._directory)])

        LOG.debug("Priming the server")
        # TODO(T82114844): incremental is borked on Ubuntu 20.04.
        subprocess.check_call(
            ["pyre", "--noninteractive", "check"], cwd=self._directory
        )

    def check(self, input: str) -> str:
        LOG.debug(f"Writing code:\n{input}")
        code_path = self._directory / "input.py"
        code_path.write_text(input)

        # TODO(T82114844): incremental is borked on Ubuntu 20.04.
        with subprocess.Popen(
            ["pyre", "--output=json", "--noninteractive", "check"],
            stderr=subprocess.PIPE,
            stdout=subprocess.PIPE,
            cwd=self._directory,
            text=True,
        ) as process:
            # pyre-fixme[6]: Expected `IO[bytes]` for 1st param but got
            #  `Optional[IO[typing.Any]]`.
            stderr = _consume(process.stderr)
            # pyre-fixme[6]: Expected `IO[bytes]` for 1st param but got
            #  `Optional[IO[typing.Any]]`.
            stdout = _consume(process.stdout)
            return_code = process.wait()

            if return_code > 1:
                LOG.error(f"Returning error: {stderr}")
                result = jsonify(errors=[stderr])
            else:
                errors = json.loads(stdout)
                result = jsonify(data={"errors": errors, "stderr": stderr})

            return result


pyre = Pyre()
application = Flask(__name__)
CORS(application)


@application.route("/check", methods=["GET", "POST"])
def check() -> str:
    input = (
        request.args.get("input")
        or request.form.get("input")
        or request.json.get("input")
    )
    if input is None:
        return jsonify(errors=["Input not provided"])

    LOG.info(f"Checking `{input}`...")
    return pyre.check(input)


@application.route("/")
def index() -> str:
    return "index"


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--debug", action="store_true")
    arguments: argparse.Namespace = parser.parse_args()

    application.debug = arguments.debug
    application.run()
