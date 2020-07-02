import argparse
import json
import subprocess
import tempfile
from pathlib import Path
from typing import Any
from venv import EnvBuilder


class AssertionError(Exception):
    pass


def production_assert(value: bool, *args: Any) -> None:
    if not value:
        raise AssertionError(*args)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Test wheel or source distribution for basic functionality."
    )
    parser.add_argument("pypi", type=str)
    arguments = parser.parse_args()
    pypi = arguments.pypi
    with tempfile.TemporaryDirectory() as temporary_venv:
        venv = Path(temporary_venv)
        builder = EnvBuilder(system_site_packages=False, clear=True, with_pip=True)
        builder.create(venv)

        pyre_path = venv / "bin" / "pyre"

        # Confirm that pypi package can be successfully installed
        subprocess.run([venv / "bin" / "pip", "install", pypi])
        production_assert(pyre_path.exists(), "Pyre was not installed.")

        # Create test project.
        with tempfile.TemporaryDirectory() as temporary_project:
            temporary_project_path = Path(temporary_project)
            python_file_path = temporary_project_path / "a.py"
            python_file_path.touch()
            python_file_path.write_text("# pyre-strict \ndef foo():\n\treturn 1")
            # Confirm we can run `pyre init` successfully.
            init_process = subprocess.run(
                [str(pyre_path), "init"],
                cwd=temporary_project_path,
                input=b"n\n.\n",
                capture_output=True,
            )
            error_message = init_process.stderr.decode()
            production_assert(
                init_process.returncode == 0,
                f"Failed to run `pyre init` successfully: {error_message}",
            )

            configuration = json.loads(
                (temporary_project_path / ".pyre_configuration").read_text()
            )
            print(json.dumps(configuration, indent=2))

            # Confirm configuration contains actual typeshed and taint files.
            typeshed_path = Path(configuration["typeshed"])
            taint_path = Path(configuration["taint_models_path"])
            binary = Path(configuration["binary"])

            production_assert(taint_path.is_dir(), "Taint path is not a directory.")
            production_assert(
                (taint_path / "taint.config").is_file(), "Taint config is not included."
            )

            production_assert(typeshed_path.is_dir(), "Typeshed was not installed.")
            production_assert(
                (typeshed_path / "third_party").is_dir(),
                "`third_party` was not included in typeshed.",
            )
            production_assert(
                (typeshed_path / "stdlib").is_dir(),
                "`stdlib` was not included in typeshed.",
            )

            production_assert(
                binary.is_file(), "Binary was not included in pypi package."
            )

            # Confirm Pyre reports errors as expected.
            result = subprocess.run(
                [pyre_path, "--output=json", "check"],
                capture_output=True,
                cwd=temporary_project_path,
            )
            errors = json.loads(result.stdout)
            production_assert(
                errors[0]["name"] == "Missing return annotation",
                "Incorrect pyre error returned.",
            )


if __name__ == "__main__":
    main()
