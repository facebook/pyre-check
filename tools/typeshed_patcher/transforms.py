# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module provides libcst transform classes for applying various
stub patches to open-source typeshed stubs.
"""
from __future__ import annotations

from typing import Sequence

import libcst
import libcst.codemod

from . import patch


def statements_from_content(content: str) -> Sequence[libcst.BaseStatement]:
    """
    Given a content string (originating from a patch toml file),
    parse statements as a CST so that we can apply them in a
    libcst transform.
    """
    module = libcst.parse_module(content)
    return module.body


class PatchTransform(libcst.codemod.ContextAwareTransformer):

    CONTEXT_KEY = "PatchTransform"

    def __init__(self) -> None:
        super().__init__(libcst.codemod.CodemodContext())


class AddTransform(PatchTransform):

    statements_to_add: Sequence[libcst.BaseStatement]
    add_position: patch.AddPosition
    parent: str
    current_names: list[str]
    found_parent: bool

    def __init__(
        self,
        parent: patch.QualifiedName,
        content: str,
        add_position: patch.AddPosition,
    ) -> None:
        super().__init__()
        # Data determining what to transform
        self.statements_to_add = statements_from_content(content)
        self.add_position = add_position
        self.parent = parent.to_string()
        # Tracking visitor state
        self.current_names: list[str] = []
        self.found_parent = self.parent == ""

    def visit_ClassDef(
        self,
        node: libcst.ClassDef,
    ) -> None:
        self.current_names.append(node.name.value)

    def extend_body(
        self, body: Sequence[libcst.BaseStatement]
    ) -> Sequence[libcst.BaseStatement]:
        if self.add_position == patch.AddPosition.TOP_OF_SCOPE:
            return [
                *self.statements_to_add,
                *body,
            ]
        elif self.add_position == patch.AddPosition.BOTTOM_OF_SCOPE:
            return [
                *body,
                *self.statements_to_add,
            ]
        else:
            raise RuntimeError(f"Unexpected add_position value {self.add_position}")

    def get_current_name_and_pop(self) -> str:
        this_name = ".".join(self.current_names)
        self.current_names.pop()
        return this_name

    def should_modify_class(self, current_name: str) -> bool:
        if current_name == self.parent:
            if self.found_parent:
                raise ValueError(f"Encountered two classes with name {self.parent}")
            else:
                self.found_parent = True
            return True
        else:
            return False

    def leave_ClassDef(
        self,
        original_node: libcst.ClassDef,
        updated_node: libcst.ClassDef,
    ) -> libcst.ClassDef:
        current_name = self.get_current_name_and_pop()
        if self.should_modify_class(current_name):
            # This update code has to cope with LibCST's need to to distinguish
            # between indented vs non-indented bodies. As a result, there are
            # two layers of bodies to deal with, and if the outer body is not
            # indented then we will need to coerce the body to be indented
            # before adding statements.
            outer_body = updated_node.body
            if isinstance(outer_body, libcst.IndentedBlock):
                new_outer_body = outer_body.with_changes(
                    body=self.extend_body(outer_body.body)
                )
            else:
                inner_body_as_base_statements = [
                    statement
                    if isinstance(statement, libcst.BaseStatement)
                    else libcst.SimpleStatementLine(body=[statement])
                    for statement in outer_body.body
                ]
                new_outer_body = libcst.IndentedBlock(
                    body=self.extend_body(inner_body_as_base_statements)
                )
            return updated_node.with_changes(body=new_outer_body)
        else:
            return updated_node

    def leave_Module(
        self,
        original_node: libcst.Module,
        updated_node: libcst.Module,
    ) -> libcst.Module:
        if self.parent == "":
            return updated_node.with_changes(body=self.extend_body(updated_node.body))
        else:
            if not self.found_parent:
                raise ValueError(
                    "Could not find class with name {}".format(self.parent)
                )
            return updated_node


def run_transform(code: str, transform: PatchTransform) -> str:
    original_module = libcst.parse_module(code)
    transformed_module = transform.transform_module(original_module)
    return transformed_module.code
