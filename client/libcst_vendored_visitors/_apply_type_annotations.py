# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree
#

# VENDORED FROM Instagram/LibCST/libcst/codemod/visitors/_apply_type_annotations.py ON COMMIT 1e0d4841bfcfc2a399a826208c2d1b5419ca4c44
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Sequence, Set, Tuple, Union

import libcst as cst
from libcst import matchers as m
from libcst.codemod._context import CodemodContext
from libcst.codemod._visitor import ContextAwareTransformer
from libcst.codemod.visitors._add_imports import AddImportsVisitor
from libcst.codemod.visitors._gather_imports import GatherImportsVisitor
from libcst.helpers import get_full_name_for_node


def _get_import_alias_names(import_aliases: Sequence[cst.ImportAlias]) -> Set[str]:
    import_names = set()
    for imported_name in import_aliases:
        asname = imported_name.asname
        if asname is not None:
            import_names.add(get_full_name_for_node(asname.name))
        else:
            import_names.add(get_full_name_for_node(imported_name.name))
    return import_names


def _get_import_names(imports: Sequence[Union[cst.Import, cst.ImportFrom]]) -> Set[str]:
    import_names = set()
    for _import in imports:
        if isinstance(_import, cst.Import):
            import_names.update(_get_import_alias_names(_import.names))
        else:
            names = _import.names
            if not isinstance(names, cst.ImportStar):
                import_names.update(_get_import_alias_names(names))
    return import_names


@dataclass(frozen=True)
class FunctionAnnotation:
    parameters: cst.Parameters
    returns: Optional[cst.Annotation]


class TypeCollector(cst.CSTVisitor):
    """
    Collect type annotations from a stub module.
    """

    def __init__(self, existing_imports: Set[str], context: CodemodContext) -> None:
        # Qualifier for storing the canonical name of the current function.
        self.qualifier: List[str] = []
        # Store the annotations.
        self.function_annotations: Dict[str, FunctionAnnotation] = {}
        self.attribute_annotations: Dict[str, cst.Annotation] = {}
        self.existing_imports: Set[str] = existing_imports
        self.class_definitions: Dict[str, cst.ClassDef] = {}
        self.context = context

    def visit_ClassDef(self, node: cst.ClassDef) -> None:
        self.qualifier.append(node.name.value)
        self.class_definitions[node.name.value] = node

    def leave_ClassDef(self, original_node: cst.ClassDef) -> None:
        self.qualifier.pop()

    def visit_FunctionDef(self, node: cst.FunctionDef) -> bool:
        self.qualifier.append(node.name.value)
        returns = node.returns
        return_annotation = (
            self._create_import_from_annotation(returns)
            if returns is not None
            else None
        )
        parameter_annotations = self._import_parameter_annotations(node.params)
        self.function_annotations[".".join(self.qualifier)] = FunctionAnnotation(
            parameters=parameter_annotations, returns=return_annotation
        )

        # pyi files don't support inner functions, return False to stop the traversal.
        return False

    def leave_FunctionDef(self, original_node: cst.FunctionDef) -> None:
        self.qualifier.pop()

    def visit_AnnAssign(self, node: cst.AnnAssign) -> bool:
        name = get_full_name_for_node(node.target)
        if name is not None:
            self.qualifier.append(name)
        annotation_value = self._create_import_from_annotation(node.annotation)
        self.attribute_annotations[".".join(self.qualifier)] = annotation_value
        return True

    def leave_AnnAssign(self, original_node: cst.AnnAssign) -> None:
        self.qualifier.pop()

    def visit_ImportFrom(self, node: cst.ImportFrom) -> None:
        module = node.module
        names = node.names

        # module is None for relative imports like `from .. import foo`.
        # We ignore these for now.
        if module is None or isinstance(names, cst.ImportStar):
            return
        module_name = get_full_name_for_node(module)
        if module_name is not None:
            for import_name in _get_import_alias_names(names):
                AddImportsVisitor.add_needed_import(
                    self.context, module_name, import_name
                )

    def _add_annotation_to_imports(
        self, annotation: cst.Attribute
    ) -> Union[cst.Name, cst.Attribute]:
        key = get_full_name_for_node(annotation.value)
        if key is not None:
            # Don't attempt to re-import existing imports.
            if key in self.existing_imports:
                return annotation
            import_name = get_full_name_for_node(annotation.attr)
            if import_name is not None:
                AddImportsVisitor.add_needed_import(self.context, key, import_name)
        return annotation.attr

    def _handle_Index(self, slice: cst.Index, node: cst.Subscript) -> cst.Subscript:
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
        if m.matches(node.value, m.Name(value="Type")):
            return node
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
            if m.matches(value, m.Name(value="Type")):
                return returns
            return cst.Annotation(annotation=self._handle_Subscript(annotation))
        else:
            return returns

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


@dataclass(frozen=True)
class Annotations:
    function_annotations: Dict[str, FunctionAnnotation] = field(default_factory=dict)
    attribute_annotations: Dict[str, cst.Annotation] = field(default_factory=dict)
    class_definitions: Dict[str, cst.ClassDef] = field(default_factory=dict)


class ApplyTypeAnnotationsVisitor(ContextAwareTransformer):
    """
    Apply type annotations to a source module using the given stub mdules.
    You can also pass in explicit annotations for functions and attributes and
    pass in new class definitions that need to be added to the source module.

    This is one of the transforms that is available automatically to you when
    running a codemod. To use it in this manner, import
    :class:`~libcst.codemod.visitors.ApplyTypeAnnotationsVisitor` and then call the static
    :meth:`~libcst.codemod.visitors.ApplyTypeAnnotationsVisitor.store_stub_in_context` method,
    giving it the current context (found as ``self.context`` for all subclasses of
    :class:`~libcst.codemod.Codemod`), the stub module from which you wish to add annotations.

    For example, you can store the type annotation ``int`` for ``x`` using::

        stub_module = parse_module("x: int = ...")

        ApplyTypeAnnotationsVisitor.store_stub_in_context(self.context, stub_module)

    You can apply the type annotation using::

        source_module = parse_module("x = 1")
        ApplyTypeAnnotationsVisitor.transform_module(source_module)

    This will produce the following code::

        x: int = 1

    If the function or attribute already has a type annotation, it will not be overwritten.

    To overwrite existing annotations when applying annotations from a stub,
    use the keyword argument ``overwrite_existing_annotations=True`` when
    constructing the codemod or when calling ``store_stub_in_context``.
    """

    CONTEXT_KEY = "ApplyTypeAnnotationsVisitor"

    def __init__(
        self,
        context: CodemodContext,
        annotations: Optional[Annotations] = None,
        overwrite_existing_annotations: bool = False,
    ) -> None:
        super().__init__(context)
        # Qualifier for storing the canonical name of the current function.
        self.qualifier: List[str] = []
        self.annotations: Annotations = (
            Annotations() if annotations is None else annotations
        )
        self.toplevel_annotations: Dict[str, cst.Annotation] = {}
        self.visited_classes: Set[str] = set()
        self.overwrite_existing_annotations = overwrite_existing_annotations

        # We use this to determine the end of the import block so that we can
        # insert top-level annotations.
        self.import_statements: List[cst.ImportFrom] = []

    @staticmethod
    def store_stub_in_context(
        context: CodemodContext,
        stub: cst.Module,
        overwrite_existing_annotations: bool = False,
    ) -> None:
        # deprecated, should be removed in 0.4 release.
        ApplyTypeAnnotationsVisitor.store_stub_in_context(
            context, stub, overwrite_existing_annotations
        )

    @staticmethod
    def store_stub_in_context(
        context: CodemodContext,
        stub: cst.Module,
        overwrite_existing_annotations: bool = False,
    ) -> None:
        """
        Store a stub module in the :class:`~libcst.codemod.CodemodContext` so
        that type annotations from the stub can be applied in a later
        invocation of this class.

        If the ``overwrite_existing_annotations`` flag is ``True``, the
        codemod will overwrite any existing annotations.

        If you call this function multiple times, only the last values of
        ``stub`` and ``overwrite_existing_annotations`` will take effect.
        """
        context.scratch[ApplyTypeAnnotationsVisitor.CONTEXT_KEY] = (
            stub,
            overwrite_existing_annotations,
        )

    def transform_module_impl(self, tree: cst.Module) -> cst.Module:
        """
        Collect type annotations from all stubs and apply them to ``tree``.

        Gather existing imports from ``tree`` so that we don't add duplicate imports.
        """
        import_gatherer = GatherImportsVisitor(CodemodContext())
        tree.visit(import_gatherer)
        existing_import_names = _get_import_names(import_gatherer.all_imports)

        context_contents = self.context.scratch.get(
            ApplyTypeAnnotationsVisitor.CONTEXT_KEY
        )
        if context_contents is not None:
            stub, overwrite_existing_annotations = context_contents
            self.overwrite_existing_annotations = (
                self.overwrite_existing_annotations or overwrite_existing_annotations
            )
            visitor = TypeCollector(existing_import_names, self.context)
            stub.visit(visitor)
            self.annotations.function_annotations.update(visitor.function_annotations)
            self.annotations.attribute_annotations.update(visitor.attribute_annotations)
            self.annotations.class_definitions.update(visitor.class_definitions)

        tree_with_imports = AddImportsVisitor(self.context).transform_module(tree)
        return tree_with_imports.visit(self)

    def _qualifier_name(self) -> str:
        return ".".join(self.qualifier)

    def _annotate_single_target(
        self, node: cst.Assign, updated_node: cst.Assign
    ) -> Union[cst.Assign, cst.AnnAssign]:
        only_target = node.targets[0].target
        if isinstance(only_target, (cst.Tuple, cst.List)):
            for element in only_target.elements:
                value = element.value
                name = get_full_name_for_node(value)
                if name:
                    self._add_to_toplevel_annotations(name)
        elif isinstance(only_target, (cst.Subscript)):
            pass
        else:
            name = get_full_name_for_node(only_target)
            if name is not None:
                self.qualifier.append(name)
                if (
                    self._qualifier_name() in self.annotations.attribute_annotations
                    and not isinstance(only_target, cst.Subscript)
                ):
                    annotation = self.annotations.attribute_annotations[
                        self._qualifier_name()
                    ]
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
        if self._qualifier_name() in self.annotations.attribute_annotations:
            annotation = self.annotations.attribute_annotations[self._qualifier_name()]
            self.toplevel_annotations[name] = annotation
        self.qualifier.pop()

    def _update_parameters(
        self, annotations: FunctionAnnotation, updated_node: cst.FunctionDef
    ) -> cst.Parameters:
        # Update params and default params with annotations
        # Don't override existing annotations or default values unless asked
        # to overwrite existing annotations.
        def update_annotation(
            parameters: Sequence[cst.Param], annotations: Sequence[cst.Param]
        ) -> List[cst.Param]:
            parameter_annotations = {}
            annotated_parameters = []
            for parameter in annotations:
                if parameter.annotation:
                    parameter_annotations[parameter.name.value] = parameter.annotation
            for parameter in parameters:
                key = parameter.name.value
                if key in parameter_annotations and (
                    self.overwrite_existing_annotations or not parameter.annotation
                ):
                    parameter = parameter.with_changes(
                        annotation=parameter_annotations[key]
                    )
                annotated_parameters.append(parameter)
            return annotated_parameters

        return annotations.parameters.with_changes(
            params=update_annotation(
                updated_node.params.params, annotations.parameters.params
            ),
            kwonly_params=update_annotation(
                updated_node.params.kwonly_params, annotations.parameters.kwonly_params
            ),
            posonly_params=update_annotation(
                updated_node.params.posonly_params,
                annotations.parameters.posonly_params,
            ),
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
        if key in self.annotations.function_annotations:
            function_annotation = self.annotations.function_annotations[key]
            # Only add new annotation if explicitly told to overwrite existing
            # annotations or if one doesn't already exist.
            set_return_annotation = not updated_node.returns or (
                self.overwrite_existing_annotations and function_annotation.returns
            )
            if set_return_annotation:
                updated_node = updated_node.with_changes(
                    returns=function_annotation.returns
                )
            # Don't override default values when annotating functions
            new_parameters = self._update_parameters(function_annotation, updated_node)
            return updated_node.with_changes(params=new_parameters)
        return updated_node

    def leave_Assign(
        self, original_node: cst.Assign, updated_node: cst.Assign
    ) -> Union[cst.Assign, cst.AnnAssign]:

        if len(original_node.targets) > 1:
            for assign in original_node.targets:
                target = assign.target
                if isinstance(target, (cst.Name, cst.Attribute)):
                    name = get_full_name_for_node(target)
                    if name is not None:
                        # Add separate top-level annotations for `a = b = 1`
                        # as `a: int` and `b: int`.
                        self._add_to_toplevel_annotations(name)
            return updated_node
        else:
            return self._annotate_single_target(original_node, updated_node)

    def leave_ImportFrom(
        self, original_node: cst.ImportFrom, updated_node: cst.ImportFrom
    ) -> cst.ImportFrom:
        self.import_statements.append(original_node)
        return updated_node

    def leave_Module(
        self, original_node: cst.Module, updated_node: cst.Module
    ) -> cst.Module:
        fresh_class_definitions = [
            definition
            for name, definition in self.annotations.class_definitions.items()
            if name not in self.visited_classes
        ]
        if not self.toplevel_annotations and not fresh_class_definitions:
            return updated_node
        toplevel_statements = []
        # First, find the insertion point for imports
        statements_before_imports, statements_after_imports = self._split_module(
            original_node, updated_node
        )

        # Make sure there's at least one empty line before the first non-import
        statements_after_imports = self._insert_empty_line(statements_after_imports)

        for name, annotation in self.toplevel_annotations.items():
            annotated_assign = cst.AnnAssign(cst.Name(name), annotation, None)
            toplevel_statements.append(cst.SimpleStatementLine([annotated_assign]))

        toplevel_statements.extend(fresh_class_definitions)

        return updated_node.with_changes(
            body=[
                *statements_before_imports,
                *toplevel_statements,
                *statements_after_imports,
            ]
        )
