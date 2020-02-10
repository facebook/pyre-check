# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree


from collections import defaultdict
from typing import IO, Any, Dict, List

import libcst as cst


class NonFinalAttributeCollector(cst.CSTVisitor):
    def __init__(self) -> None:
        # qualifier for storing the canonical name of the current class/function
        self.qualifier: List[str] = []
        # store each class mapped to its non-final attributes
        self.attributes: Dict[str, List[str]] = defaultdict(list)

    def _add_target_to_attributes(self, target: cst.AssignTarget) -> None:
        attribute = target.target
        if (
            isinstance(attribute, cst.Attribute)
            and isinstance(attribute.value, cst.Name)
            # attribute `value`.
            and attribute.value.value == "self"
        ):
            self.attributes[self.qualifier[0]].append(attribute.attr.value)

    def visit_ClassDef(self, node: cst.ClassDef) -> None:
        self.qualifier.append(node.name.value)

    def leave_ClassDef(self, original_node: cst.ClassDef) -> None:
        self.qualifier.pop()

    def visit_FunctionDef(self, node: cst.FunctionDef) -> None:
        self.qualifier.append(node.name.value)

    def leave_FunctionDef(self, original_node: cst.FunctionDef) -> None:
        self.qualifier.pop()

    def visit_Assign(self, node: cst.Assign) -> None:
        # check assigned targets only after initialization
        if self.qualifier[-1] != "__init__":
            for target in node.targets:
                self._add_target_to_attributes(target)


def _parse(file: IO[Any]) -> cst.Module:  # pyre-fixme[2]
    contents = file.read()
    return cst.parse_module(contents)


def _get_attributes(source: cst.Module) -> Dict[str, List[str]]:
    visitor = NonFinalAttributeCollector()
    source.visit(visitor)
    return visitor.attributes
