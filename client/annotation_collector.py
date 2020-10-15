# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import Any, Dict, List, Optional

import libcst as cst


class AnnotationCollector(cst.CSTVisitor):
    def __init__(self, path_name: str) -> None:
        self.stubs: List[Dict[str, Any]] = []
        self.class_name: List[str] = []
        self.path_name: str = str(path_name).replace("/", ".")

    def visit_FunctionDef(self, node: cst.FunctionDef) -> bool:
        if node.returns is None:
            return False
        class_name = ".".join(self.class_name)
        name = (
            node.name.value
            if not self.class_name
            else class_name + "." + node.name.value
        )
        parameters = []
        for parameter in node.params.params:
            annotation = (
                None
                if parameter.annotation is None
                else parameter.annotation.annotation
            )
            parameters.append(
                {
                    "name": parameter.name.value,
                    "type": self.code_for_node(annotation),
                    "value": self.code_for_node(parameter.default),
                }
            )
        function_stub = {
            "function_name": self.path_name + "." + name,
            "annotation": self.code_for_node(node.returns.annotation),
            "parameters": parameters,
            "decorators": [
                self.code_for_node(decorator) for decorator in node.decorators
            ],
            "async": node.asynchronous is not None,
            "parent": class_name if self.class_name else None,
        }
        self.stubs.append(function_stub)
        return False

    def visit_ClassDef(self, node: cst.ClassDef) -> None:
        self.class_name.append(node.name.value)

    def leave_ClassDef(self, original_node: cst.ClassDef) -> None:
        self.class_name.remove(original_node.name.value)

    def visit_AnnAssign(self, node: cst.AnnAssign) -> None:
        target_name = self.code_for_node(node.target)
        if target_name is None:
            return
        class_name = ".".join(self.class_name)
        attribute_name = (
            class_name + "." + target_name if self.class_name else target_name
        )
        stub = {
            "attribute_name": self.path_name + "." + attribute_name,
            "annotation": self.code_for_node(node.annotation.annotation),
            "parent": class_name if self.class_name else None,
        }
        self.stubs.append(stub)

    # Class helper methods
    @classmethod
    def code_for_node(cls, node: Optional[cst.CSTNode]) -> Optional[str]:
        return None if node is None else cst.parse_module("").code_for_node(node)
