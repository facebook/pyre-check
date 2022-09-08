# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import argparse
import functools
import hashlib
import json
import logging
import os
import subprocess
import tempfile
import threading
from pathlib import Path
from typing import Dict, IO, List

from flask import Flask, jsonify, request, Response
from flask_cors import CORS

# pyre-fixme[21]: pyre cannot seem to find this module
from flask_socketio import emit, SocketIO

logging.basicConfig(
    format="%(asctime)s %(levelname)s %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
    level=logging.DEBUG,
)

LOG: logging.Logger = logging.getLogger(__name__)

CUSTOM_PYSA_MODEL_FILE: str = "custom.pysa"
WATCHMAN_CONFIG_FILE: str = ".watchmanconfig"
PYRE_CONFIG_FILE: str = ".pyre_configuration"
INPUT_FILE: str = "playground_input.py"


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


@functools.lru_cache(maxsize=128)
def _get_cache_contents(file_path: Path) -> str:
    with file_path.open() as cache_file:
        return json.loads(cache_file.read())


@functools.lru_cache(maxsize=128)
def _get_cache_file_path(input: str, model: str) -> Path:
    hashable_object = str(input + model).encode("utf-8")
    sha1_hash_file_name = hashlib.sha1(hashable_object).hexdigest() + ".cache"
    cache_directory = Path(
        os.environ.get("PYSA_PLAYGROUND_CACHE_DIRECTORY", "/var/pysa_cache")
    )
    cache_file_path = cache_directory / sha1_hash_file_name
    return cache_file_path


def _generate_cache_contents(
    return_code: int, lines: List[str], annotations: List[Dict[str, str]]
):
    return json.dumps(
        {"return_code": return_code, "lines": lines, "annotations": annotations}
    )


def _parse_annotations_from_taint_output(
    taint_output_file_path: Path,
) -> List[Dict[str, str]]:
    if not (taint_output_file_path.is_file() and taint_output_file_path.exists()):
        return []
    annotations = []
    with taint_output_file_path.open() as taint_output_file:
        taint_output_contents = taint_output_file.readlines()
        for taint_output_line in taint_output_contents:
            taint_output_line_json = json.loads(taint_output_line)
            if taint_output_line_json.get("kind") != "issue":
                continue
            data = taint_output_line_json.get("data")
            if data is None:
                continue
            message = data.get("message")
            if message is None:
                continue
            traces = data.get("traces")
            if traces is None:
                continue
            for trace in traces:
                trace_roots = trace.get("roots")
                if trace_roots is None:
                    continue
                for trace_root in trace_roots:
                    root = trace_root.get("root")
                    if root is None:
                        continue
                    line = root.get("line")
                    start = root.get("start")
                    end = root.get("end")
                    if line is None or start is None or end is None:
                        continue
                    annotations.append(
                        {
                            "message": message,
                            "line": line,
                            "start": start,
                            "end": end,
                        }
                    )
    return annotations


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

        LOG.debug("Initializing the code")
        code_path = self._directory / INPUT_FILE
        code_path.write_text("x = 0\n")

        LOG.debug("Starting watchman")
        subprocess.check_call(["watchman", "watch", str(self._directory)])

        LOG.debug("Priming the server")
        subprocess.check_call(
            ["pyre", "--noninteractive", "--sequential"],
            cwd=self._directory,
        )

    def check(self, input: str) -> Response:
        LOG.debug("Running pyre check")
        code_path = self._directory / INPUT_FILE
        code_path.write_text(input)

        with subprocess.Popen(
            ["pyre", "--output=json", "--noninteractive", "--sequential"],
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
    def __init__(
        self, input: str, model: str = "", use_builtin_pysa_models: bool = False
    ) -> None:
        self._directory: Path = Path(tempfile.mkdtemp())
        self._stubs: Path = Path(tempfile.mkdtemp())
        self.input: str = input
        self.model: str = model
        self.taint_output_file_path = Path(self._directory / "taint_output.json")

        LOG.debug(f"Intializing Pysa in `{self._directory}`...")
        pyre_configuration = json.dumps(
            {
                "source_directories": ["."],
                "taint_models_path": [
                    str(self._stubs),
                    os.environ["PYSA_PLAYGROUND_TAINT_MODELS"],
                ]
                if use_builtin_pysa_models
                else str(self._stubs),
                "search_path": [str(self._stubs), os.environ["PYSA_PLAYGROUND_STUBS"]],
            }
        )
        LOG.debug(f"Writing configuration:\n{pyre_configuration}")
        pyre_configuration_path = self._directory / PYRE_CONFIG_FILE
        pyre_configuration_path.write_text(pyre_configuration)
        if model:
            LOG.debug("Writing custom model to pysa file")
            model_path = self._stubs / CUSTOM_PYSA_MODEL_FILE
            model_path.write_text(model)
        LOG.debug(f"Writing code:\n{input}")
        code_path = self._directory / INPUT_FILE
        code_path.write_text(input)

    def analyze(self) -> None:
        LOG.debug("Running pysa")
        with subprocess.Popen(
            ["pyre", "-n", "analyze", "--no-verify", "--save-results-to", "./"],
            stderr=subprocess.PIPE,
            stdout=subprocess.PIPE,
            cwd=self._directory,
            text=True,
        ) as process:
            model_verification_errors = []
            cache_lines = []
            # pyre-fixme[16]: process.stderr is marked as Optional
            for line in iter(process.stderr.readline, b""):
                line = line.rstrip()
                if line == "":
                    break
                elif "ERROR" in line and "is not part of the environment" in line:
                    model_verification_errors.append(line)
                elif "INFO" in line or "ERROR" in line:
                    if model_verification_errors:
                        # Emit all model verification lines together to prevent
                        # network overhead.
                        model_verification_error_output = "\n".join(
                            model_verification_errors
                        )
                        emit(
                            "pysa_results_channel",
                            {
                                "type": "output",
                                "line": model_verification_error_output,
                            },
                        )
                        LOG.debug(model_verification_error_output)
                        model_verification_errors = []
                    emit("pysa_results_channel", {"type": "output", "line": line})
                LOG.debug(line)
                cache_lines.append(line)

            return_code = process.wait()
            if return_code != 0:
                result = {"type": "finished", "result": "error"}
            else:
                result = {"type": "finished", "result": "ok"}
            annotations = _parse_annotations_from_taint_output(
                self.taint_output_file_path
            )
            if len(annotations) > 0:
                emit(
                    "pysa_result_channel",
                    {"type": "annotations", "annotations": annotations},
                )
            emit("pysa_results_channel", result)
            # write to cache now:
            with _get_cache_file_path(self.input, self.model).open("w") as cache_file:
                cache_contents = _generate_cache_contents(
                    return_code, cache_lines, annotations
                )
                cache_file.write(cache_contents)


def get_server():
    application = Flask(__name__)

    # You may need to modify the origin to the pyre-check website
    # before deployment.
    CORS(application)
    socketio = SocketIO(application, cors_allowed_origins="*")

    LOG.info("Initializizing the pyre server")
    pyre = Pyre()

    LOG.info("Pyre server is initialized, configuring application routes")

    @application.route("/check", methods=["GET", "POST"])
    def check() -> Response:
        input = (
            # pyre-ignore[16] - Request attributes are missing from stub
            request.args.get("input")
            # pyre-ignore[16] - Request attributes are missing from stub
            or request.form.get("input")
            # pyre-ignore[16] - Request attributes are missing from stub
            or request.json.get("input")
        )
        if input is None:
            return jsonify(errors=["Input not provided"])

        LOG.info(f"Checking `{input}`...")
        return pyre.check(input)

    @socketio.on("analyze", namespace="/analyze")
    def analyze(json) -> None:
        input = json.get("input", None)
        use_builtin_pysa_models = json.get("use_builtin_pysa_models", False)
        model = json.get("model", "")
        if input is None:
            emit(
                "pysa_results_channel",
                {
                    "type": "finished",
                    "result": "error",
                    "reason": "No code given to analyze.",
                },
            )
        else:
            cache_file_path = _get_cache_file_path(input, model)
            if cache_file_path.exists():
                LOG.info(f"Using cache `{cache_file_path}`...")
                cache_contents = _get_cache_contents(cache_file_path)
                run_status = cache_contents["return_code"]
                lines = cache_contents["lines"]
                annotations = cache_contents["annotations"]
                emit(
                    "pysa_results_channel",
                    {
                        "type": "output",
                        "line": "\n".join(lines),
                    },
                )
                if len(annotations) > 0:
                    emit(
                        "pysa_result_channel",
                        {"type": "annotations", "annotations": annotations},
                    )
                if run_status != 0:
                    result = {"type": "finished", "result": "error"}
                else:
                    result = {"type": "finished", "result": "ok"}
                emit("pysa_results_channel", result)
            else:
                pysa = Pysa(input, model, use_builtin_pysa_models)
                LOG.info(f"Checking `{input}`...")
                pysa.analyze()

    @application.route("/")
    def index() -> str:
        return "404"

    return application, socketio


def run_server(debug: bool) -> None:
    application, socketio = get_server()
    socketio.run(application, debug=debug)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--debug", action="store_true")
    arguments: argparse.Namespace = parser.parse_args()
    run_server(debug=arguments.debug)
