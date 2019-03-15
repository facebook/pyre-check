from sapp import cli_lib
from sapp.diagnostics.cli import diagnostics


def cli():
    cli_lib.cli.add_command(diagnostics)
    cli_lib.cli()


if __name__ == "__main__":
    cli()
