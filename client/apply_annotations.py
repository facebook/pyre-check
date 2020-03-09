# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree


from typing import (
    IO,
    Any,
    Dict,
    List,
    NamedTuple,
    Optional,
    Sequence,
    Set,
    Tuple,
    Union,
    overload,
)

import libcst as cst


def _get_attribute_as_string(attribute: cst.BaseExpression) -> str:
    names = []
    while isinstance(attribute, cst.Attribute):
        # pyre-fixme[16]: `BaseExpression` has no attribute `value`.
        if isinstance(attribute.value.value, cst.Attribute):
            value = _get_attribute_as_string(
                cst.ensure_type(attribute.value, cst.Attribute).value
            )
        else:
            value = _get_name_as_string(attribute.value.value)
        names.append(value)
        attribute = attribute.attr
    if attribute is not None:
        names.append(_get_name_as_string(attribute.value))
    return ".".join(names)


def _get_name_as_string(node: Union[cst.CSTNode, str]) -> str:
    if isinstance(node, cst.Name):
        return node.value
    else:
        # pyre-fixme[7]: Expected `str` but got `Union[CSTNode, str]`.
        return node


class FunctionAnnotation(NamedTuple):
    parameters: cst.Parameters
    returns: Optional[cst.Annotation]


class ImportStatement(NamedTuple):
    module: Union[cst.Name, cst.Attribute]
    names: Set[str]


class ImportCollector(cst.CSTVisitor):
    def __init__(self) -> None:
        self.existing_imports: Set[str] = set()

    def visit_Import(self, node: cst.Import) -> None:
        for imported_name in node.names:
            asname = imported_name.asname
            if asname:
                self.existing_imports.add(_get_attribute_as_string(asname.name))
            else:
                self.existing_imports.add(_get_attribute_as_string(imported_name.name))


class TypeCollector(cst.CSTVisitor):
    def __init__(self, existing_imports: Set[str]) -> None:
        # Qualifier for storing the canonical name of the current function.
        self.qualifier: List[str] = []
        # Store the annotations.
        self.function_annotations: Dict[str, FunctionAnnotation] = {}
        self.attribute_annotations: Dict[str, cst.Annotation] = {}
        self.imports: Dict[str, ImportStatement] = {}
        self.existing_imports: Set[str] = existing_imports
        self.class_definitions: Dict[str, cst.ClassDef] = {}

    def visit_ClassDef(self, node: cst.ClassDef) -> None:
        self.qualifier.append(node.name.value)
        self.class_definitions[node.name.value] = node

    def leave_ClassDef(self, original_node: cst.ClassDef) -> None:
        self.qualifier.pop()

    def visit_FunctionDef(self, node: cst.FunctionDef) -> bool:
        self.qualifier.append(node.name.value)
        returns = node.returns
        if returns is not None:
            return_annotation = self._create_import_from_annotation(returns)
            parameter_annotations = self._import_parameter_annotations(node.params)
            self.function_annotations[".".join(self.qualifier)] = FunctionAnnotation(
                parameters=parameter_annotations, returns=return_annotation
            )
        # pyi files don't support inner functions, return False to stop the traversal.
        return False

    def leave_FunctionDef(self, original_node: cst.FunctionDef) -> None:
        self.qualifier.pop()

    def visit_AnnAssign(self, node: cst.AnnAssign) -> bool:
        # pyre-fixme[16]: `BaseExpression` has no attribute `value`.
        self.qualifier.append(node.target.value)
        annotation_value = self._create_import_from_annotation(node.annotation)
        self.attribute_annotations[".".join(self.qualifier)] = annotation_value
        return True

    def leave_AnnAssign(self, original_node: cst.AnnAssign) -> None:
        self.qualifier.pop()

    def visit_ImportFrom(self, node: cst.ImportFrom) -> None:
        module = node.module
        if module is None or isinstance(node, cst.ImportStar):
            return
        # pyre-fixme[6]: Expected `List[CSTNode]` for 1st param but got
        #  `Union[Sequence[ImportAlias], ImportStar]`.
        # pyre-fixme[6]: Expected `str` for 1st param but got `Union[BaseExpression,
        #  str]`.
        self._add_to_imports(node.names, cst.Name(module.value), module.value)

    def _add_annotation_to_imports(
        self, annotation: cst.Attribute
    ) -> Union[cst.Name, cst.Attribute]:
        key = _get_attribute_as_string(annotation.value)
        # Don't attempt to re-import existing imports.
        if key in self.existing_imports:
            return annotation
        self._add_to_imports(
            [cst.ImportAlias(name=annotation.attr)], annotation.value, key
        )
        return annotation.attr

    @overload
    def _handle_Index(self, slice: cst.Index, node: cst.Subscript) -> cst.Subscript:
        pass

    def _handle_Index(  # noqa
        self, slice: cst.Index, node: cst.BaseExpression
    ) -> cst.BaseExpression:
        value = slice.value
        if isinstance(value, cst.Subscript):
            new_slice = slice.with_changes(value=self._handle_Subscript(value))
            return node.with_changes(slice=new_slice)
        elif isinstance(value, cst.Attribute):
            new_slice = slice.with_changes(value=self._add_annotation_to_imports(value))
            return node.with_changes(slice=new_slice)
        else:
            return node

    def _handle_Subscript(self, node: cst.Subscript) -> cst.Subscript:
        slice = node.slice
        if isinstance(slice, list):
            new_slice = []
            for item in slice:
                value = item.slice.value
                if isinstance(value, cst.Attribute):
                    name = self._add_annotation_to_imports(item.slice.value)
                    new_index = item.slice.with_changes(value=name)
                    new_slice.append(item.with_changes(slice=new_index))
                else:
                    if isinstance(item.slice, cst.Index) and not isinstance(
                        item.slice.value, cst.Name
                    ):
                        new_index = item.slice.with_changes(
                            value=self._handle_Index(item.slice, item)
                        )
                        item = item.with_changes(slice=new_index, comma=None)
                    new_slice.append(item)
            return node.with_changes(slice=new_slice)
        elif isinstance(slice, cst.Index):
            return self._handle_Index(slice, node)
        else:
            return node

    def _create_import_from_annotation(self, returns: cst.Annotation) -> cst.Annotation:
        annotation = returns.annotation
        if isinstance(annotation, cst.Attribute):
            attr = self._add_annotation_to_imports(annotation)
            return cst.Annotation(annotation=attr)
        if isinstance(annotation, cst.Subscript):
            value = annotation.value
            if isinstance(value, cst.Name) and value.value == "Type":
                return returns
            return cst.Annotation(annotation=self._handle_Subscript(annotation))
        else:
            return returns

    def _add_to_imports(
        self, names: List[cst.ImportAlias], module: cst.BaseExpression, key: str
    ) -> None:
        names_as_string = [_get_name_as_string(name.name) for name in names]
        set_names = set(names_as_string)
        if key not in self.imports:
            # pyre-fixme[6]: Expected `Union[Attribute, Name]` for 2nd param but got
            #  `BaseExpression`.
            self.imports[key] = ImportStatement(names=set_names, module=module)
        else:
            import_statement = self.imports[key]
            for name in set_names:
                if name not in import_statement.names:
                    import_statement.names.add(name)

    def _import_parameter_annotations(
        self, parameters: cst.Parameters
    ) -> cst.Parameters:
        def update_annotations(parameters: Sequence[cst.Param]) -> List[cst.Param]:
            updated_parameters = []
            for parameter in list(parameters):
                annotation = parameter.annotation
                if annotation is not None:
                    parameter = parameter.with_changes(
                        annotation=self._create_import_from_annotation(annotation)
                    )
                updated_parameters.append(parameter)
            return updated_parameters

        return parameters.with_changes(params=update_annotations(parameters.params))


class TypeTransformer(cst.CSTTransformer):
    def __init__(
        self,
        function_annotations: Dict[str, FunctionAnnotation],
        attribute_annotations: Dict[str, cst.Annotation],
        imports: Dict[str, ImportStatement],
        class_definitions: Dict[str, cst.ClassDef],
    ) -> None:
        # Qualifier for storing the canonical name of the current function.
        self.qualifier: List[str] = []
        # Store the annotations.
        self.function_annotations = function_annotations
        self.attribute_annotations = attribute_annotations
        self.toplevel_annotations: Dict[str, cst.CSTNode] = {}
        self.class_definitions = class_definitions
        self.visited_classes: Set[str] = set()
        self.imports = imports
        self.import_statements: List[cst.ImportFrom] = []
        self.is_generated: bool = False

    def _qualifier_name(self) -> str:
        return ".".join(self.qualifier)

    def _annotate_single_target(
        self, node: cst.Assign, updated_node: cst.Assign
    ) -> Union[cst.Assign, cst.AnnAssign]:
        if isinstance(node.targets[0].target, cst.Tuple):
            target = node.targets[0].target
            # pyre-fixme[16]: `BaseAssignTargetExpression` has no attribute `elements`.
            for element in target.elements:
                if not isinstance(element.value, cst.Subscript):
                    name = _get_name_as_string(element.value.value)
                    self._add_to_toplevel_annotations(name)
            return updated_node
        else:
            target = node.targets[0].target
            # pyre-fixme[16]: `BaseAssignTargetExpression` has no attribute `value`.
            name = _get_name_as_string(target.value)
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

    def _update_parameters(
        self, annotations: FunctionAnnotation, updated_node: cst.FunctionDef
    ) -> cst.Parameters:
        # Update params and default params with annotations
        # don't override existing annotations or default values
        def update_annotation(
            parameters: Sequence[cst.Param], annotations: Sequence[cst.Param]
        ) -> List[cst.Param]:
            parameter_annotations = {}
            annotated_parameters = []
            for parameter in list(annotations):
                if parameter.annotation:
                    parameter_annotations[parameter.name.value] = parameter.annotation
            for parameter in list(parameters):
                key = parameter.name.value
                if key in parameter_annotations and not parameter.annotation:
                    parameter = parameter.with_changes(
                        annotation=parameter_annotations[key]
                    )
                annotated_parameters.append(parameter)
            return annotated_parameters

        return annotations.parameters.with_changes(
            params=update_annotation(
                updated_node.params.params, annotations.parameters.params
            )
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
        self.visited_classes.add(node.name.value)

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
            new_parameters = self._update_parameters(annotations, updated_node)
            return updated_node.with_changes(params=new_parameters)
        return updated_node

    def leave_Assign(
        self, original_node: cst.Assign, updated_node: cst.Assign
    ) -> Union[cst.Assign, cst.AnnAssign]:

        if len(original_node.targets) > 1:
            for assign in original_node.targets:
                if not isinstance(assign.target, cst.Subscript):
                    self._add_to_toplevel_annotations(
                        # pyre-fixme[16]: `BaseAssignTargetExpression` has no
                        #  attribute `value`.
                        _get_name_as_string(assign.target.value)
                    )
            return updated_node
        else:
            return self._annotate_single_target(original_node, updated_node)

    def leave_ImportFrom(
        self, original_node: cst.ImportFrom, updated_node: cst.ImportFrom
    ) -> cst.ImportFrom:
        self.import_statements.append(original_node)
        # pyre-fixme[6]: Expected `Union[Attribute, Name]` for 1st param but got
        #  `Optional[Union[Attribute, Name]]`.
        key = _get_attribute_as_string(original_node.module)
        import_names = updated_node.names
        module = original_node.module
        if (
            module is not None
            and module.value in self.imports
            and not isinstance(import_names, cst.ImportStar)
        ):
            names_as_string = [_get_name_as_string(name.name) for name in import_names]
            updated_names = self.imports[key].names.union(set(names_as_string))
            names = [cst.ImportAlias(cst.Name(name)) for name in sorted(updated_names)]
            updated_node = updated_node.with_changes(names=tuple(names))
            del self.imports[key]
        return updated_node

    def visit_Comment(self, node: cst.Comment) -> None:
        if "@" "generated" in node.value:
            self.is_generated = True

    def leave_Module(
        self, original_node: cst.Module, updated_node: cst.Module
    ) -> cst.Module:
        fresh_class_definitions = [
            definition
            for name, definition in self.class_definitions.items()
            if name not in self.visited_classes
        ]
        if self.is_generated:
            return original_node
        if (
            not self.toplevel_annotations
            and not self.imports
            and not fresh_class_definitions
        ):
            return updated_node
        toplevel_statements = []
        # First, find the insertion point for imports
        statements_before_imports, statements_after_imports = self._split_module(
            original_node, updated_node
        )

        # Make sure there's at least one empty line before the first non-import
        statements_after_imports = self._insert_empty_line(statements_after_imports)

        imported = set()
        for statement in self.import_statements:
            names = statement.names
            if isinstance(names, cst.ImportStar):
                continue
            for name in names:
                if name.asname:
                    name = name.asname
                if name:
                    imported.add(_get_name_as_string(name.name))

        for _, import_statement in self.imports.items():
            # Filter out anything that has already been imported.
            names = import_statement.names.difference(imported)
            names = [cst.ImportAlias(cst.Name(name)) for name in sorted(names)]
            if not names:
                continue
            import_statement = cst.ImportFrom(
                module=import_statement.module, names=names
            )
            # Add import statements to module body.
            # Need to assign an Iterable, and the argument to SimpleStatementLine
            # must be subscriptable.
            toplevel_statements.append(cst.SimpleStatementLine([import_statement]))

        for name, annotation in self.toplevel_annotations.items():
            annotated_assign = cst.AnnAssign(
                cst.Name(name),
                # pyre-fixme[16]: `CSTNode` has no attribute `annotation`.
                cst.Annotation(annotation.annotation),
                None,
            )
            toplevel_statements.append(cst.SimpleStatementLine([annotated_assign]))

        toplevel_statements.extend(fresh_class_definitions)

        return updated_node.with_changes(
            body=[
                *statements_before_imports,
                *toplevel_statements,
                *statements_after_imports,
            ]
        )


def _parse(file: IO[Any]) -> cst.Module:  # pyre-fixme[2]
    contents = file.read()
    return cst.parse_module(contents)


def _annotate_source(stubs: cst.Module, source: cst.Module) -> cst.Module:
    import_visitor = ImportCollector()
    source.visit(import_visitor)
    visitor = TypeCollector(import_visitor.existing_imports)
    stubs.visit(visitor)
    transformer = TypeTransformer(
        visitor.function_annotations,
        visitor.attribute_annotations,
        visitor.imports,
        visitor.class_definitions,
    )
    return source.visit(transformer)


def apply_stub_annotations(stub_path: str, file_path: str) -> str:
    with open(stub_path) as stub_file, open(file_path) as source_file:
        stubs = _parse(stub_file)
        source = _parse(source_file)
        modified_tree = _annotate_source(stubs, source)
        return modified_tree.code
