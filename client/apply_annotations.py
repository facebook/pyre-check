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
        self.function_annotations: Dict[str, FunctionAnnotation] = {}
        self.attribute_annotations: Dict[str, cst.Annotation] = {}

    def visit_ClassDef(self, node: cst.ClassDef) -> None:
        self.qualifier.append(node.name.value)

    def leave_ClassDef(self, node: cst.ClassDef) -> None:
        self.qualifier.pop()

    def visit_FunctionDef(self, node: cst.FunctionDef) -> bool:
        self.qualifier.append(node.name.value)
        self.function_annotations[".".join(self.qualifier)] = FunctionAnnotation(
            parameters=node.params, returns=node.returns
        )
        # pyi files don't support inner functions, return False to stop the traversal.
        return False

    def leave_FunctionDef(self, node: cst.FunctionDef) -> None:
        self.qualifier.pop()

    def visit_AnnAssign(self, node: cst.AnnAssign) -> bool:
        # pyre-fixme[16]: `BaseExpression` has no attribute `value`.
        self.qualifier.append(node.target.value)
        annotation_value = node.annotation
        self.attribute_annotations[".".join(self.qualifier)] = annotation_value
        return True

    def leave_AnnAssign(self, node: cst.AnnAssign) -> None:
        self.qualifier.pop()


class TypeTransformer(cst.CSTTransformer):
    def __init__(
        self,
        function_annotations: Dict[str, FunctionAnnotation],
        attribute_annotations: Dict[str, cst.Annotation],
    ) -> None:
        # Qualifier for storing the canonical name of the current function.
        self.qualifier: List[str] = []
        # Store the annotations.
        self.function_annotations: Dict[str, FunctionAnnotation] = function_annotations
        self.attribute_annotations: Dict[str, cst.Annotation] = attribute_annotations
        self.toplevel_annotations: Dict[str, cst.CSTNode] = {}

    def _qualifier_name(self) -> str:
        return ".".join(self.qualifier)

    # pyre-fixme[2]: Parameter must be annotated.
    def _get_toplevel_index(self, body) -> int:
        for index, node in enumerate(body):
            if isinstance(node, cst.FunctionDef) or isinstance(node, cst.ClassDef):
                return index
        return 0

    def _annotate_single_target(
        self, node: cst.Assign, updated_node: cst.Assign
    ) -> cst.CSTNode:
        if isinstance(node.targets[0].target, cst.Tuple):
            target = node.targets[0].target
            # pyre-fixme[16]: `BaseAssignTargetExpression` has no attribute `elements`.
            for element in target.elements:
                self._add_to_toplevel_annotations(element.value.value)
            return updated_node
        else:
            # pyre-fixme[16]: `BaseAssignTargetExpression` has no attribute `value`.
            name = node.targets[0].target.value
            self.qualifier.append(name)
            if self._qualifier_name() in self.attribute_annotations:
                annotation = self.attribute_annotations[self._qualifier_name()]
                self.qualifier.pop()
                return cst.AnnAssign(cst.Name(name), annotation, node.value)
            else:
                self.qualifier.pop()
                return updated_node

    def _add_to_toplevel_annotations(self, name: str) -> None:
        self.qualifier.append(name)
        if self._qualifier_name() in self.attribute_annotations:
            annotation = self.attribute_annotations[self._qualifier_name()]
            self.toplevel_annotations[name] = annotation
        self.qualifier.pop()

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
        key = self._qualifier_name()
        self.qualifier.pop()
        if key in self.function_annotations:
            annotations = self.function_annotations[key]
            return updated_node.with_changes(
                params=annotations.parameters, returns=annotations.returns
            )
        return updated_node

    def leave_Assign(self, node: cst.Assign, updated_node: cst.Assign) -> cst.CSTNode:

        if len(node.targets) > 1:
            for assign in node.targets:
                # pyre-fixme[16]: `BaseAssignTargetExpression` has no attribute `value`.
                self._add_to_toplevel_annotations(assign.target.value)
            return updated_node
        else:
            return self._annotate_single_target(node, updated_node)

    def leave_Module(self, node: cst.Module, updated_node: cst.Module) -> cst.CSTNode:
        body = list(updated_node.body)
        index = self._get_toplevel_index(body)
        for name, annotation in self.toplevel_annotations.items():
            annotated_assign = cst.AnnAssign(
                cst.Name(name),
                # pyre-fixme[16]: `CSTNode` has no attribute `annotation`.
                cst.Annotation(annotation.annotation),
                None,
            )
            body.insert(index, cst.SimpleStatementLine([annotated_assign]))
        return updated_node.with_changes(body=tuple(body))


# pyre-fixme[2]: Parameter annotation cannot contain `Any`.
def _parse(file: IO[Any]) -> cst.Module:
    contents = file.read()
    return cst.parse_module(contents)


def _annotate_source(stubs: cst.Module, source: cst.Module) -> cst.Module:
    visitor = TypeCollector()
    stubs.visit(visitor)
    transformer = TypeTransformer(
        visitor.function_annotations, visitor.attribute_annotations
    )
    return source.visit(transformer)


def apply_stub_annotations(stub_path: str, file_path: str) -> str:
    with open(stub_path) as stub_file, open(file_path) as source_file:
        stubs = _parse(stub_file)
        source = _parse(source_file)
        modified_tree = _annotate_source(stubs, source)
        return modified_tree.code
