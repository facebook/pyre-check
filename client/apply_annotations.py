# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree

# pyre-strict

from typing import IO, Any, Dict, List, NamedTuple, Optional

import libcst as cst


class FunctionAnnotation(NamedTuple):
    parameters: cst.Parameters
    returns: Optional[cst.Annotation]


class TypeCollector(cst.CSTVisitor):
    def __init__(self) -> None:
        # Qualifier for storing the canonical name of the current function.
        self.qualifier: List[str] = []
        # Store the annotations.
        self.annotations: Dict[str, FunctionAnnotation] = {}

    def visit_ClassDef(self, node: cst.ClassDef) -> None:
        self.qualifier.append(node.name.value)

    def leave_ClassDef(self, node: cst.ClassDef) -> None:
        self.qualifier.pop()

    def visit_FunctionDef(self, node: cst.FunctionDef) -> bool:
        self.qualifier.append(node.name.value)
        self.annotations[".".join(self.qualifier)] = FunctionAnnotation(
            parameters=node.params, returns=node.returns
        )
        # pyi files don't support inner functions, return False to stop the traversal.
        return False

    def leave_FunctionDef(self, node: cst.FunctionDef) -> None:
        self.qualifier.pop()


class TypeTransformer(cst.CSTTransformer):
    def __init__(self, annotations: Dict[str, FunctionAnnotation]) -> None:
        # Qualifier for storing the canonical name of the current function.
        self.qualifier: List[str] = []
        # Store the annotations.
        self.annotations: Dict[str, FunctionAnnotation] = annotations

    def visit_ClassDef(self, node: cst.ClassDef) -> None:
        self.qualifier.append(node.name.value)

    def leave_ClassDef(
        self, node: cst.ClassDef, updated_node: cst.ClassDef
    ) -> cst.CSTNode:
        self.qualifier.pop()
        return updated_node

    def visit_FunctionDef(self, node: cst.FunctionDef) -> bool:
        self.qualifier.append(node.name.value)
        # pyi files don't support inner functions, return False to stop the traversal.
        return False

    def leave_FunctionDef(
        self, node: cst.FunctionDef, updated_node: cst.FunctionDef
    ) -> cst.CSTNode:
        key = ".".join(self.qualifier)
        self.qualifier.pop()
        if key in self.annotations:
            annotations = self.annotations[key]
            return updated_node.with_changes(
                params=annotations.parameters, returns=annotations.returns
            )
        return updated_node


# pyre-fixme[2]: Parameter annotation cannot contain `Any`.
def _parse(file: IO[Any]) -> cst.Module:
    contents = file.read()
    return cst.parse_module(contents)


def _annotate_functions(stubs: cst.Module, source: cst.Module) -> cst.Module:
    visitor = TypeCollector()
    stubs.visit(visitor)
    transformer = TypeTransformer(visitor.annotations)
    return source.visit(transformer)


def apply_stub_annotations(stub_path: str, file_path: str) -> str:
    with open(stub_path) as stub_file, open(file_path) as source_file:
        stubs = _parse(stub_file)
        source = _parse(source_file)

        modified_tree = _annotate_functions(stubs, source)
        return modified_tree.code
