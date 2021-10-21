# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import argparse
import json
import logging
import os
import subprocess
import tempfile
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

CUSTOM_PYSA_MODEL_FILE: str = "custom.py"
WATCHMAN_CONFIG_FILE: str = ".watchmanconfig"
PYRE_CONFIG_FILE: str = ".pyre_configuration"
INPUT_FILE: str = "input.py"


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
        pyre_configuration = json.dumps(
            {
                "source_directories": ["."],
            }
        )
        LOG.debug(f"Writing configuration:\n{pyre_configuration}")
        pyre_configuration_path = self._directory / PYRE_CONFIG_FILE
        pyre_configuration_path.write_text(pyre_configuration)

        LOG.debug("Writing watchman configuration")
        watchman_configuration_path = self._directory / WATCHMAN_CONFIG_FILE
        watchman_configuration_path.write_text("{}\n")

        LOG.debug("Starting watchman")
        subprocess.check_call(["watchman", "watch", str(self._directory)])

        LOG.debug("Priming the server")
        # TODO(T82114844): incremental is borked on Ubuntu 20.04.
        subprocess.check_call(
            ["pyre", "--noninteractive", "check"], cwd=self._directory
        )

    def check(self, input: str) -> str:
        LOG.debug("Running pyre check")
        code_path = self._directory / INPUT_FILE
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


class Pysa:
    def __init__(self, model: str = "", use_builtin_pysa_models: bool = False) -> None:
        self._directory: Path = Path(tempfile.mkdtemp())
        self._stubs: Path = Path(tempfile.mkdtemp())

        LOG.debug(f"Intializing Pysa in `{self._directory}`...")
        pyre_configuration = json.dumps(
            {
                "source_directories": ["."],
                "taint_models_path": [
                    str(self._stubs),
                    os.environ["PYSA_PLAYGROUND_STUBS"],
                ]
                if use_builtin_pysa_models
                else str(self._stubs),
                "search_path": [str(self._stubs)],
            }
        )
        LOG.debug(f"Writing configuration:\n{pyre_configuration}")
        pyre_configuration_path = self._directory / PYRE_CONFIG_FILE
        pyre_configuration_path.write_text(pyre_configuration)
        if model:
            LOG.debug("Writing custom model to pysa file")
            model_path = self._stubs / CUSTOM_PYSA_MODEL_FILE
            model_path.write_text(model)

    def analyze(self, input: str) -> str:
        LOG.debug(f"Writing code:\n{input}")
        code_path = self._directory / INPUT_FILE
        code_path.write_text(input)

        LOG.debug("Running pysa")
        with subprocess.Popen(
            ["pyre", "-n", "analyze"],
            stderr=subprocess.PIPE,
            stdout=subprocess.PIPE,
            cwd=self._directory,
            text=True,
        ) as process:
            process_stderr = process.stderr
            stderr = ""
            if process_stderr is not None:
                stderr = _consume(process_stderr)
            process_stdout = process.stdout
            stdout = ""
            if process_stdout is not None:
                stdout = _consume(process_stdout)
            return_code = process.wait()

            if return_code != 0:
                LOG.error(f"Returning error: {stderr}")
                result = jsonify(errors=[stderr])
            else:
                errors = json.loads(stdout)
                result = jsonify(data={"errors": errors, "stderr": stderr})

            return result


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
    pyre = Pyre()
    return pyre.check(input)


@application.route("/analyze", methods=["POST"])
def analyze() -> str:
    input = (
        request.args.get("input")
        or request.form.get("input")
        or request.json.get("input")
    )
    use_builtin_pysa_models = bool(
        request.args.get("use_builtin_pysa_models")
        or request.form.get("use_builtin_pysa_models")
        or request.json.get("use_builtin_pysa_models")
    )
    model = (
        request.args.get("model")
        or request.form.get("model")
        or request.json.get("model")
    )
    if input is None:
        return jsonify(errors=["Input not provided"])
    pysa = Pysa(model, use_builtin_pysa_models)
    LOG.info(f"Checking `{input}`...")
    return pysa.analyze(input)


@application.route("/")
def index() -> str:
    return "index"


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--debug", action="store_true")
    arguments: argparse.Namespace = parser.parse_args()

    application.debug = arguments.debug
    application.run()
