#!/usr/bin/env python
# Copyright 2004-present Facebook. All Rights Reserved.

import ast
import json
import os
import subprocess
from typing import Mapping  # noqa

from tools.pyre.client import find_configuration_root
from tools.pyre.client.commands.command import ExitCode


class PyreServerException(Exception):
    pass


class Location:
    def __init__(self, line: int, column: int) -> None:
        self.line = line
        self.column = column

    def __hash__(self) -> int:
        return hash((self.line, self.column))

    def __eq__(self, other) -> bool:
        if not isinstance(other, Location):
            return False
        return self.line == other.line and self.column == other.column

    def __str__(self):
        return "Location(%d, %d)" % (self.line, self.column)


class Annotation:
    def __init__(self, start: Location, stop: Location, annotation: str) -> None:
        self.start = start
        self.stop = stop
        self.annotation = annotation

    def __eq__(self, other) -> bool:
        if not isinstance(other, Annotation):
            return False
        return (
            self.start == other.start
            and self.stop == other.stop
            and self.annotation == other.annotation
        )

    def __str__(self):
        return "Annotation(%s, %s, %s)" % (
            str(self.start),
            str(self.stop),
            self.annotation,
        )


class AddTypes(ast.NodeTransformer):
    def __init__(self, annotation_lookup: Mapping[Location, Annotation]) -> None:
        self._annotation_lookup = annotation_lookup

    # pyre-fixme: Overridden generic_visit has return type of `None`
    def generic_visit(self, node: ast.AST) -> ast.AST:
        if hasattr(node, "lineno") and hasattr(node, "col_offset"):
            location = Location(node.lineno, node.col_offset)
            annotation = None
            if location in self._annotation_lookup:
                annotation = self._annotation_lookup[location].annotation
            node.type = annotation
            node._attributes = node._attributes + ("type",)
        super(AddTypes, self).generic_visit(node)
        return node


class PyreAst:
    def __init__(self, configuration_path: str) -> None:
        configuration_path = os.path.abspath(configuration_path)
        if os.path.isfile(os.path.join(configuration_path, ".pyre_configuration")):
            self._project_configuration = configuration_path
            self._local_configuration = configuration_path
            self._initialize_server()
        elif os.path.isfile(
            os.path.join(configuration_path, ".pyre_configuration.local")
        ):
            self._local_configuration = configuration_path
            project_configuration = find_configuration_root(
                os.path.dirname(configuration_path), ".pyre_configuration"
            )
            if not project_configuration:
                raise PyreServerException(
                    "{} is not a subdirectory of a toplevel pyre configuration.".format(
                        configuration_path
                    )
                )
            self._project_configuration = project_configuration
            self._initialize_server()
        else:
            raise PyreServerException(
                "{} does not contain a pyre configuration.".format(configuration_path)
            )

    def _initialize_server(self) -> None:
        # TODO(T37004997): Server call can be async
        return_code = subprocess.call(
            ["pyre", "-l", self._local_configuration],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        killed = False
        while return_code == ExitCode.FAILURE and not killed:
            subprocess.call(["pyre", "kill"])
            return_code = subprocess.call(
                ["pyre", "-l", self._local_configuration],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
            killed = True
        if return_code == ExitCode.SUCCESS or ExitCode.FOUND_ERRORS:
            return
        raise PyreServerException(
            "Could not start pyre server for local configuration {}.".format(
                self._local_configuration
            )
        )

    def _query_file_types(self, filename: str) -> Mapping[Location, Annotation]:
        query_command = [
            "pyre",
            "-l",
            self._local_configuration,
            "query",
            "types_in_file('" + filename + "')",
        ]
        # TODO(T37004997): Server call can be async
        process = subprocess.Popen(
            query_command, stdout=subprocess.PIPE, stderr=subprocess.PIPE
        )
        stdout, stderr = process.communicate()
        annotations = stdout.decode().strip()
        if not annotations:
            error_message = stderr.decode().strip().split(" ")[2:]
            raise PyreServerException(
                "Failed to query types for {}.\n{}".format(
                    filename, " ".join(error_message)
                )
            )
        annotation_locations_list = json.loads(annotations)["response"]["types"]
        type_lookup = {}  # type: Mapping[Location, Annotation]
        for annotation_location in annotation_locations_list:
            start = annotation_location["location"]["start"]
            stop = annotation_location["location"]["stop"]
            start_location = Location(start["line"], start["column"])
            stop_location = Location(stop["line"], stop["column"])
            type_lookup[start_location] = Annotation(
                start_location, stop_location, annotation_location["annotation"]
            )
        return type_lookup

    def typed_ast(self, existing_ast: ast.AST, file_path: str) -> ast.Module:
        file_path = os.path.abspath(file_path)
        types_by_location = self._query_file_types(
            os.path.relpath(file_path, self._local_configuration)
        )
        # TODO(T37004997): ASTs are re-used between flake8 plugins.
        # If types already exist on the provided AST, do nothing.
        return AddTypes(types_by_location).visit(existing_ast)
