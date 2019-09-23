# flake8: noqa

from typing import List

from django.http import HttpRequest


class Sink:
    def run(self, command: str) -> str:
        eval(command)
        return ""


def take_input(request: HttpRequest) -> None:
    sinks: List[Sink] = [Sink()]
    result = [s.run(request.GET["bad"]) for s in sinks]


def inductive_comprehension_sink(arguments: List[str]) -> None:
    command = "  ".join(argument.lower() for argument in arguments)
    eval(command)


def eval_via_comprehension_sink(request: HttpRequest) -> None:
    inductive_comprehension_sink(request.GET["arguments"])
