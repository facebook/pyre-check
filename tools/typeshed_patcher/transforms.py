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

    parent: str
    current_names: list[str]
    found_parent: bool

    def __init__(
        self,
        parent: patch.QualifiedName,
    ) -> None:
        super().__init__(libcst.codemod.CodemodContext())
        # All of the actions take a parent
        self.parent = parent.to_string()
        # Tracking visitor state
        self.current_names = []
        self.found_parent = False

    def visit_ClassDef(
        self,
        node: libcst.ClassDef,
    ) -> None:
        self.current_names.append(node.name.value)

    def get_current_name(self) -> str:
        return ".".join(self.current_names)

    def pop_current_name(self) -> str:
        this_name = self.get_current_name()
        self.current_names.pop()
        return this_name

    def is_parent(self, current_name: str) -> bool:
        if current_name == self.parent:
            if self.found_parent:
                raise ValueError(f"Encountered two classes with name {self.parent}")
            else:
                self.found_parent = True
            return True
        else:
            return False


class AddTransform(PatchTransform):

    statements_to_add: Sequence[libcst.BaseStatement]
    add_position: patch.AddPosition

    def __init__(
        self,
        parent: patch.QualifiedName,
        content: str,
        add_position: patch.AddPosition,
    ) -> None:
        super().__init__(parent=parent)
        # Data determining what to transform
        self.statements_to_add = statements_from_content(content)
        self.add_position = add_position

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

    def transform_parent_class(
        self,
        node: libcst.ClassDef,
    ) -> libcst.ClassDef:
        """
        Add statements to a class body.

        This update code has to cope with LibCST's need to to distinguish
        between indented vs non-indented bodies. As a result, there are two
        layers of bodies to deal with. Moreover, if the outer body is not
        indented then we will need to coerce the body to be indented
        before adding statements.
        """
        outer_body = node.body
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
        return node.with_changes(body=new_outer_body)

    def leave_ClassDef(
        self,
        original_node: libcst.ClassDef,
        updated_node: libcst.ClassDef,
    ) -> libcst.ClassDef:
        current_name = self.pop_current_name()
        if self.is_parent(current_name):
            return self.transform_parent_class(updated_node)
        else:
            return updated_node

    def transform_parent_module(
        self,
        node: libcst.Module,
    ) -> libcst.Module:
        return node.with_changes(body=self.extend_body(node.body))

    def leave_Module(
        self,
        original_node: libcst.Module,
        updated_node: libcst.Module,
    ) -> libcst.Module:
        if self.is_parent(self.get_current_name()):
            out = self.transform_parent_module(updated_node)
        else:
            out = updated_node
        if not self.found_parent:
            raise ValueError(f"Did not find any classes matching {self.parent}")
        return out


def run_transform(code: str, transform: PatchTransform) -> str:
    original_module = libcst.parse_module(code)
    transformed_module = transform.transform_module(original_module)
    return transformed_module.code
