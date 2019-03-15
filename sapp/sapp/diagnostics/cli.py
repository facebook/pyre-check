#!/usr/bin/env python3

import click

from . import models
from ..cli_lib import Context, pass_context


@click.group()
def diagnostics():
    """Commands for diagnosing analysis issues"""
    pass


@diagnostics.command()
@pass_context
def save_models(ctx: Context) -> None:
    models.save()


@diagnostics.command()
@pass_context
def show_models(ctx: Context) -> None:
    models.show()
