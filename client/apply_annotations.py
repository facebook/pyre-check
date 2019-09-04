# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree

# pyre-strict

from typing import IO, Any, Dict, List, NamedTuple, Optional, Tuple, Union

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
        self.imports: Dict[str, List[cst.ImportFrom]] = {}

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

    def visit_ImportFrom(self, node: cst.ImportFrom) -> None:
        module = node.module
        if module is None:
            return
        if module.value not in self.imports:
            # pyre-fixme[6]: Expected `str` for 1st param but got
            #  `Union[BaseExpression, str]`.
            self.imports[module.value] = node.names
        else:
            # pyre-fixme[6]: Expected `Iterable[ImportFrom]` for 1st param but got
            #  `Union[Sequence[ImportAlias], ImportStar]`.
            self.imports[module.value] += node.names


class TypeTransformer(cst.CSTTransformer):
    def __init__(
        self,
        function_annotations: Dict[str, FunctionAnnotation],
        attribute_annotations: Dict[str, cst.Annotation],
        imports: Dict[str, List[cst.ImportFrom]],
    ) -> None:
        # Qualifier for storing the canonical name of the current function.
        self.qualifier: List[str] = []
        # Store the annotations.
        self.function_annotations = function_annotations
        self.attribute_annotations = attribute_annotations
        self.toplevel_annotations: Dict[str, cst.CSTNode] = {}
        self.imports = imports
        self.import_statements: List[cst.CSTNode] = []

    def _qualifier_name(self) -> str:
        return ".".join(self.qualifier)

    def _get_name_as_string(self, node: Union[cst.CSTNode, str]) -> str:
        if isinstance(node, cst.Name):
            return node.value
        else:
            # pyre-fixme[7]: Expected `str` but got `Union[CSTNode, str]`.
            return node

    def _annotate_single_target(
        self, node: cst.Assign, updated_node: cst.Assign
    ) -> Union[cst.Assign, cst.AnnAssign]:
        if isinstance(node.targets[0].target, cst.Tuple):
            target = node.targets[0].target
            # pyre-fixme[16]: `BaseAssignTargetExpression` has no attribute `elements`.
            for element in target.elements:
                if not isinstance(element.value, cst.Subscript):
                    name = self._get_name_as_string(element.value.value)
                    self._add_to_toplevel_annotations(name)
            return updated_node
        else:
            target = node.targets[0].target
            # pyre-fixme[16]: `BaseAssignTargetExpression` has no attribute `value`.
            name = self._get_name_as_string(target.value)
            self.qualifier.append(name)
            if self._qualifier_name() in self.attribute_annotations and not isinstance(
                target, cst.Subscript
            ):
                annotation = self.attribute_annotations[self._qualifier_name()]
                self.qualifier.pop()
                return cst.AnnAssign(cst.Name(name), annotation, node.value)
            else:
                self.qualifier.pop()
                return updated_node

    def _split_module(
        self, module: cst.Module, updated_module: cst.Module
    ) -> Tuple[
        List[Union[cst.SimpleStatementLine, cst.BaseCompoundStatement]],
        List[Union[cst.SimpleStatementLine, cst.BaseCompoundStatement]],
    ]:
        import_add_location = 0
        # This works under the principle that while we might modify node contents,
        # we have yet to modify the number of statements. So we can match on the
        # original tree but break up the statements of the modified tree. If we
        # change this assumption in this visitor, we will have to change this code.
        for i, statement in enumerate(module.body):
            if isinstance(statement, cst.SimpleStatementLine):
                for possible_import in statement.body:
                    for last_import in self.import_statements:
                        if possible_import is last_import:
                            import_add_location = i + 1
                            break

        return (
            list(updated_module.body[:import_add_location]),
            list(updated_module.body[import_add_location:]),
        )

    def _add_to_toplevel_annotations(self, name: str) -> None:
        self.qualifier.append(name)
        if self._qualifier_name() in self.attribute_annotations:
            annotation = self.attribute_annotations[self._qualifier_name()]
            self.toplevel_annotations[name] = annotation
        self.qualifier.pop()

    def _update_default_parameters(
        self, annotations: FunctionAnnotation, updated_node: cst.FunctionDef
    ) -> cst.Parameters:
        parameter_annotations = {}
        annotated_default_parameters = []
        for parameter in list(annotations.parameters.default_params):
            if parameter.annotation:
                parameter_annotations[parameter.name.value] = parameter.annotation
        for parameter in list(updated_node.params.default_params):
            if parameter.name.value in parameter_annotations:
                annotated_default_parameters.append(
                    parameter.with_changes(
                        annotation=parameter_annotations[parameter.name.value]
                    )
                )
            else:
                annotated_default_parameters.append(parameter)
        return annotations.parameters.with_changes(
            default_params=annotated_default_parameters
        )

    def _insert_empty_line(
        self,
        statements: List[Union[cst.SimpleStatementLine, cst.BaseCompoundStatement]],
    ) -> List[Union[cst.SimpleStatementLine, cst.BaseCompoundStatement]]:
        if len(statements) < 1:
            # No statements, nothing to add to
            return statements
        if len(statements[0].leading_lines) == 0:
            # Statement has no leading lines, add one!
            return [
                statements[0].with_changes(leading_lines=(cst.EmptyLine(),)),
                *statements[1:],
            ]
        if statements[0].leading_lines[0].comment is None:
            # First line is empty, so its safe to leave as-is
            return statements
        # Statement has a comment first line, so lets add one more empty line
        return [
            statements[0].with_changes(
                leading_lines=(cst.EmptyLine(), *statements[0].leading_lines)
            ),
            *statements[1:],
        ]

    def visit_ClassDef(self, node: cst.ClassDef) -> None:
        self.qualifier.append(node.name.value)

    def leave_ClassDef(
        self, original_node: cst.ClassDef, updated_node: cst.ClassDef
    ) -> cst.ClassDef:
        self.qualifier.pop()
        return updated_node

    def visit_FunctionDef(self, node: cst.FunctionDef) -> bool:
        self.qualifier.append(node.name.value)
        # pyi files don't support inner functions, return False to stop the traversal.
        return False

    def leave_FunctionDef(
        self, original_node: cst.FunctionDef, updated_node: cst.FunctionDef
    ) -> cst.FunctionDef:
        key = self._qualifier_name()
        self.qualifier.pop()
        if key in self.function_annotations:
            annotations = self.function_annotations[key]
            # Only add new annotation if one doesn't already exist
            if not updated_node.returns:
                updated_node = updated_node.with_changes(returns=annotations.returns)
            # Don't override default values when annotating functions
            new_parameters = self._update_default_parameters(annotations, updated_node)
            return updated_node.with_changes(params=new_parameters)
        return updated_node

    def leave_Assign(
        self, original_node: cst.Assign, updated_node: cst.Assign
    ) -> Union[cst.Assign, cst.AnnAssign]:

        if len(original_node.targets) > 1:
            for assign in original_node.targets:
                if not isinstance(assign.target, cst.Subscript):
                    # pyre-fixme[16]: `BaseAssignTargetExpression`
                    # has no attribute `value`.
                    value = self._get_name_as_string(assign.target.value)
                    self._add_to_toplevel_annotations(value)
            return updated_node
        else:
            return self._annotate_single_target(original_node, updated_node)

    def leave_ImportFrom(
        self, original_node: cst.ImportFrom, updated_node: cst.ImportFrom
    ) -> cst.ImportFrom:
        self.import_statements.append(original_node)
        if (
            original_node.module is not None
            # pyre-fixme[16]: `Optional` has no attribute `value`.
            and original_node.module.value in self.imports
        ):
            names = list(updated_node.names) + self.imports[original_node.module.value]
            updated_node = updated_node.with_changes(names=tuple(names))
            del self.imports[original_node.module.value]
        return updated_node

    def visit_ImportAlias(self, node: cst.ImportAlias) -> None:
        self.import_statements.append(node)

    def visit_ImportStar(self, node: cst.ImportStar) -> None:
        self.import_statements.append(node)

    def leave_Module(
        self, original_node: cst.Module, updated_node: cst.Module
    ) -> cst.Module:
        if not self.toplevel_annotations and not self.imports:
            return updated_node

        toplevel_statements = []

        # First, find the insertion point for imports
        statements_before_imports, statements_after_imports = self._split_module(
            original_node, updated_node
        )

        # Make sure there's at least one empty line before the first non-import
        statements_after_imports = self._insert_empty_line(statements_after_imports)

        for module_name, aliases in self.imports.items():
            import_statement = cst.ImportFrom(
                module=cst.Name(module_name),
                # pyre-fixme[6]: Expected `Union[Sequence[ImportAlias], ImportStar]`
                #  for 2nd param but got `List[ImportFrom]`.
                names=aliases,
            )
            # Add import statements to module body.
            # Need to assign an Iterable, and the argument to SimpleStatementLine
            # must be subscriptable.
            toplevel_statements = [cst.SimpleStatementLine([import_statement])]

        for name, annotation in self.toplevel_annotations.items():
            annotated_assign = cst.AnnAssign(
                cst.Name(name),
                # pyre-fixme[16]: `CSTNode` has no attribute `annotation`.
                cst.Annotation(annotation.annotation),
                None,
            )
            toplevel_statements.append(cst.SimpleStatementLine([annotated_assign]))

        return updated_node.with_changes(
            body=[
                *statements_before_imports,
                *toplevel_statements,
                *statements_after_imports,
            ]
        )


# pyre-fixme[2]: Parameter annotation cannot contain `Any`.
def _parse(file: IO[Any]) -> cst.Module:
    contents = file.read()
    return cst.parse_module(contents)


def _annotate_source(stubs: cst.Module, source: cst.Module) -> cst.Module:
    visitor = TypeCollector()
    stubs.visit(visitor)
    transformer = TypeTransformer(
        visitor.function_annotations, visitor.attribute_annotations, visitor.imports
    )
    return source.visit(transformer)


def apply_stub_annotations(stub_path: str, file_path: str) -> str:
    with open(stub_path) as stub_file, open(file_path) as source_file:
        stubs = _parse(stub_file)
        source = _parse(source_file)
        modified_tree = _annotate_source(stubs, source)
        return modified_tree.code
