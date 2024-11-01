# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
from pathlib import Path
from typing import Optional
import argparse
import json
import logging
import os
import random
import shutil
import subprocess
import sys

# Import both code generators with the updated filenames
from .forward_code_generator import CodeGenerator as ForwardCodeGenerator
from .mutation_based_code_generator import CodeGenerator as MutationBasedCodeGenerator

logging.basicConfig(level=logging.INFO)


RULE_CODE: int = 9999


def is_in_fbcode() -> bool:
    path = Path(os.getcwd())

    while path.parent != path:
        if path.name == "fbcode":
            return True
        path = path.parent

    return False


def generate_python_files(
    output_dir: Path,
    num_files: int,
    num_statements: int,
    generator_version: int,
    seed: Optional[int] = None,
    enable_known_false_negatives: bool = False,
) -> None:
    if generator_version == 1:
        generator = ForwardCodeGenerator(
            enable_known_false_negatives=enable_known_false_negatives
        )
    elif generator_version == 2:
        generator = MutationBasedCodeGenerator(
            enable_known_false_negatives=enable_known_false_negatives
        )
    else:
        raise AssertionError("invalid generator version")

    test_dir = output_dir / "tests"
    test_dir.mkdir(exist_ok=True)

    filenames = []

    rng = random.Random(seed) if seed is not None else random.Random()

    for i in range(1, num_files + 1):
        if isinstance(generator, MutationBasedCodeGenerator):
            generator.apply_mutation(rng)
            generated_code = generator.generate()
        elif isinstance(generator, ForwardCodeGenerator):
            generator.reset()
            generated_code = generator.generate_statements(
                num_statements, rng
            )
        else:
            raise AssertionError("unknown generator")

        filename = test_dir / f"test_{i}.py"
        with open(filename, "w") as file:
            file.write(generated_code)
        filenames.append(str(filename))

        logging.info(f"Generated {filename}")

    # Save filenames to temporary file
    with open(output_dir / "filenames.json", "w") as tmp_file:
        json.dump(filenames, tmp_file)


def configure_and_analyze(output_dir: Path) -> None:
    with open(output_dir / ".pyre_configuration", "w") as config_file:
        config = {
            "site_package_search_strategy": "pep561",
            "source_directories": ["./tests"],
            "taint_models_path": [".", "../../../stubs/taint"],
            "typeshed": "../../../stubs/typeshed/typeshed",
        }
        if is_in_fbcode():
            config.update(
                {
                    "stable_client": "../../../../../../tools/pyre/stable/pyre_client",
                    "unstable_client": "../../../../../../tools/pyre/stable/pyre_client",
                    "binary": ".overriden",
                }
            )
        json.dump(config, config_file, indent=2)

    with open(output_dir / "sources_sinks.pysa", "w") as pysa_file:
        pysa_file.write("def input() -> TaintSource[TestSource]: ...\n")
        pysa_file.write("def print(*__args: TaintSink[TestSink], **__kwargs): ...\n")

    with open(output_dir / "taint.config", "w") as taint_config_file:
        taint_config = {
            "sources": [
                {
                    "name": "TestSource",
                    "comment": "test source",
                }
            ],
            "sinks": [
                {
                    "name": "TestSink",
                    "comment": "test sink",
                }
            ],
            "features": [],
            "rules": [
                {
                    "name": "Test issue",
                    "code": RULE_CODE,
                    "sources": ["TestSource"],
                    "sinks": ["TestSink"],
                    "message_format": "test source flowing into test sink",
                }
            ],
        }
        json.dump(taint_config, taint_config_file, indent=2)


def run_pysa(output_dir: Path) -> None:
    if is_in_fbcode() and "PYRE_BINARY" not in os.environ:
        logging.error("Required environment variable `PYRE_BINARY` is not set.")
        sys.exit(1)

    logging.info("Please wait a minute or two for Pysa to Run!")
    try:
        result = subprocess.run(
            ["pyre", "-n", "analyze", "--rule", str(RULE_CODE), "--no-verify"],
            stdout=subprocess.PIPE,
            text=True,
            check=True,
            cwd=output_dir,
        )
        with open(output_dir / "analysis_output.json", "w") as file:
            file.write(result.stdout)
    except subprocess.CalledProcessError as e:
        logging.error(f"Command 'pyre -n analyze' failed with error:\n{e.stdout}")
        raise e


def find_undetected_files(output_dir: Path) -> None:
    try:
        with open(output_dir / "filenames.json", "r") as tmp_file:
            filenames = json.load(tmp_file)
    except FileNotFoundError:
        logging.error("Temporary file with filenames not found.")
        return

    try:
        with open(output_dir / "analysis_output.json", "r") as file:
            analysis_output = json.load(file)
    except FileNotFoundError:
        logging.error("Analysis output file not found.")
        return

    detected_files = set()
    for entry in analysis_output:
        detected_files.add(os.path.basename(entry["path"]))

    all_files = {os.path.basename(filename) for filename in filenames}
    undetected_files = all_files - detected_files

    logging.info("Files where the flow has not been detected:")
    for file in sorted(
        undetected_files, key=lambda x: int(x[len("test_") : -len(".py")])
    ):
        logging.info(file)

    total_files = len(all_files)
    undetected_count = len(undetected_files)
    undetected_percentage = (undetected_count / total_files) * 100
    logging.info(
        f"Flow has not been detected in {undetected_percentage:.2f}% of the files"
    )


def clean_up(output_dir: Path) -> None:
    shutil.rmtree(output_dir, ignore_errors=True)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Build script with setup, analysis, and cleanup."
    )
    parser.add_argument(
        "action", choices=["all", "find-undetected", "clean"], help="Action to perform"
    )
    parser.add_argument(
        "--num-files", type=int, default=100, help="Number of files to generate"
    )
    parser.add_argument(
        "--num-statements",
        type=int,
        default=20,
        help="Number of statements to generate in each file",
    )
    parser.add_argument(
        "--generator-version",
        type=int,
        choices=[1, 2],
        default=1,
        help="Select code generator version",
    )
    parser.add_argument("--seed", type=int, help="Seed for random number generation")
    parser.add_argument(
        "--enable-known-false-negatives",
        action="store_true",
        help="Enable the use of known false negatives",
    )

    args = parser.parse_args()

    if Path(os.getcwd()).name != 'pysa_fuzzer':
        logging.error('This script must be ran from the pysa_fuzzer directory.')
        sys.exit(1)

    output_dir = Path("generated_files")
    output_dir.mkdir(exist_ok=True)

    if args.action == "all":
        generate_python_files(
            output_dir,
            args.num_files,
            args.num_statements,
            args.generator_version,
            args.seed,
            args.enable_known_false_negatives,
        )
        configure_and_analyze(output_dir)
        run_pysa(output_dir)
        find_undetected_files(output_dir)
    elif args.action == "find-undetected":
        find_undetected_files(output_dir)
    elif args.action == "clean":
        clean_up(output_dir)


if __name__ == "__main__":
    main()
