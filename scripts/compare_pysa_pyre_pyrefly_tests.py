#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

"""
This script compares end-to-end integration test results between Pyre and Pyrefly
by comparing `.py.models` files (Pyre output) with `.py.pyrefly.models` files
(Pyrefly output).
"""

import argparse
import json
import logging
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Any

LOG: logging.Logger = logging.getLogger(__name__)


def find_integration_tests(
    integration_dir: Path, filter_pattern: str | None
) -> list[Path]:
    """Find all .py test files in the integration directory."""
    test_files = sorted(integration_dir.glob("*.py"))
    if filter_pattern:
        test_files = [f for f in test_files if filter_pattern in f.name]
    return test_files


def parse_models(path: Path) -> list[dict[str, Any]]:
    with open(path) as f:
        models: list[dict[str, Any]] = []
        line_iterator = iter(f.readlines())
        while True:
            try:
                line = next(line_iterator)
            except StopIteration:
                break

            if line == ("@" + "generated\n"):
                continue

            if line == "{\n":
                current_model = [line]
                while True:
                    try:
                        line = next(line_iterator)
                    except StopIteration:
                        raise AssertionError(
                            f"Failed to parse {str(path)}: unexpected end of file"
                        )

                    if line == "}\n":
                        current_model.append(line)
                        models.append(json.loads("\n".join(current_model)))
                        break
                    else:
                        current_model.append(line)
            else:
                raise AssertionError(f"Unexpected line in models file: `{line}`")

    return models


@dataclass
class CallableMapping:
    name: str
    line: int


CALLABLE_MAPPING = {
    "builtins.eval": CallableMapping(name="eval", line=474),
    "builtins.getattr": CallableMapping(name="getattr", line=476),
    "builtins.list.append": CallableMapping(name="list.append", line=422),
    "builtins.dict.__setitem__": CallableMapping(name="dict.__setitem__", line=399),
    "typing.Mapping.__getitem__": CallableMapping(
        name="typing.Mapping.__getitem__", line=131
    ),
    "typing.Mapping.get": CallableMapping(name="typing.Mapping.get", line=135),
    "typing.Mapping.update": CallableMapping(name="typing.Mapping.update", line=155),
}


def strip_builtins(o: object) -> object:
    """Strip `builtins.` from all strings in the given object, in place, recursively."""
    if isinstance(o, str):
        if o.startswith("builtins.") and o != "builtins.pyi":
            return o[len("builtins.") :]
    elif isinstance(o, list):
        for i in range(len(o)):
            o[i] = strip_builtins(o[i])
    elif isinstance(o, dict):
        for key in list(o.keys()):
            o[key] = strip_builtins(o[key])

    return o


def normalize_pyrefly_model(model: dict[str, Any]) -> object:
    if model["kind"] != "model":
        return strip_builtins(model)

    callable_name = model["data"]["callable"]
    # Remove default models for the standard library.
    if callable_name == "ast._Attributes.__init__":
        return None

    # pyre and pyrefly use different typeshed. Update standard library callable
    # names and lines.
    callable_mapping = CALLABLE_MAPPING.get(callable_name)
    if callable_mapping is not None:
        model["data"]["callable"] = callable_mapping.name
        model["data"]["callable_line"] = callable_mapping.line

    return strip_builtins(model)


def normalize_pyre_models(path: Path) -> str:
    lines = []
    for model in parse_models(path):
        lines.append(json.dumps(model, indent=2))
    lines.sort()
    return "\n".join(lines)


def normalize_pyrefly_models(path: Path) -> str:
    lines = []
    for model in parse_models(path):
        new_model = normalize_pyrefly_model(model)
        if new_model is not None:
            lines.append(json.dumps(new_model, indent=2))
    lines.sort()
    return "\n".join(lines)


def compare_models(test_file: Path, temporary_directory: Path) -> None:
    """
    Compare .py.models and .py.pyrefly.models files for a given test file.
    """
    pyre_models_path = test_file.with_suffix(".py.models")
    pyrefly_models_path = test_file.with_suffix(".py.pyrefly.models")

    if not pyre_models_path.exists():
        raise AssertionError(f"Missing pyre models for test {test_file.name}")
    if not pyrefly_models_path.exists():
        raise AssertionError(f"Missing pyrefly models for test {test_file.name}")

    pyre_normalized_models_path = temporary_directory / (
        test_file.name + ".pyre.models"
    )
    pyre_normalized_models_path.write_text(normalize_pyre_models(pyre_models_path))
    pyrefly_normalized_models_path = temporary_directory / (
        test_file.name + ".pyrefly.models"
    )
    pyrefly_normalized_models_path.write_text(
        normalize_pyrefly_models(pyrefly_models_path)
    )

    result = subprocess.run(
        [
            "diff",
            "-u",
            str(pyre_normalized_models_path),
            str(pyrefly_normalized_models_path),
        ],
        capture_output=True,
        text=True,
    )

    if result.returncode != 0:
        print(f"Mismatch for test {test_file.name}:")
        for line in result.stdout.splitlines():
            print(f"  {line}")
    else:
        print(f"Perfect match for test {test_file.name}")


def main() -> int:
    logging.basicConfig(
        level=logging.INFO, format="[%(asctime)s] [%(levelname)s] %(message)s"
    )

    parser = argparse.ArgumentParser(
        description="Compare Pysa integration test results between Pyre and Pyrefly."
    )
    parser.add_argument(
        "--filter",
        type=str,
        default=None,
        help="Optional filter pattern to only compare specific test files "
        "(e.g., --filter dictionary to only compare dictionary.py)",
    )
    parser.add_argument(
        "--interactive",
        action="store_true",
        help="Pause after each comparison",
    )

    parsed = parser.parse_args()

    pyre_directory = Path(__file__).parent.parent.absolute()
    integration_directory = (
        pyre_directory / "source/interprocedural_analyses/taint/test/integration"
    )

    if not integration_directory.exists():
        LOG.error(f"Integration test directory not found: {integration_directory}")
        return 1

    print("Comparing Pysa integration tests between Pyre and Pyrefly...")
    if parsed.filter:
        print(f"Filtering tests matching: {parsed.filter}")
    print()

    test_files = find_integration_tests(integration_directory, parsed.filter)

    if not test_files:
        print("No integration tests found.")
        return 0

    print(f"Found {len(test_files)} integration tests.")
    print()

    with tempfile.TemporaryDirectory() as temporary_directory:
        for test_file in test_files:
            compare_models(test_file, Path(temporary_directory))
            if parsed.interactive:
                print("[Press enter to continue] ", end="")
                input()

    return 0


if __name__ == "__main__":
    sys.exit(main())
