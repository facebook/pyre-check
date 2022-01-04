# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Changes vendored from LibCST. The current commit is this PR:
# https://github.com/Instagram/LibCST/pull/542

from dataclasses import dataclass, field
from typing import Dict, List, Optional, Sequence, Set, Tuple, Union

import libcst as cst
from libcst.codemod._context import CodemodContext
from libcst.codemod._visitor import ContextAwareTransformer
from libcst.codemod.visitors._add_imports import AddImportsVisitor
from libcst.codemod.visitors._gather_imports import GatherImportsVisitor
from libcst.helpers import get_full_name_for_node
from libcst.metadata import QualifiedNameProvider, PositionProvider


NameOrAttribute = Union[cst.Name, cst.Attribute]
NAME_OR_ATTRIBUTE = (cst.Name, cst.Attribute)


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

    METADATA_DEPENDENCIES = (
        PositionProvider,
        QualifiedNameProvider,
    )

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
        new_bases = []
        for base in node.bases:
            value = base.value
            if isinstance(value, NAME_OR_ATTRIBUTE):
                new_value = self._handle_NameOrAttribute(value)
            elif isinstance(value, cst.Subscript):
                new_value = self._handle_Subscript(value)
            else:
                start = self.get_metadata(PositionProvider, node).start
                raise ValueError(
                    "Invalid type used as base class in stub file at "
                    + f"{start.line}:{start.column}. Only subscripts, names, and "
                    + "attributes are valid base classes for static typing."
                )
            new_bases.append(base.with_changes(value=new_value))

        self.class_definitions[node.name.value] = node.with_changes(bases=new_bases)

    def leave_ClassDef(self, original_node: cst.ClassDef) -> None:
        self.qualifier.pop()

    def visit_FunctionDef(self, node: cst.FunctionDef) -> bool:
        self.qualifier.append(node.name.value)
        returns = node.returns
        return_annotation = (
            self._handle_Annotation(annotation=returns) if returns is not None else None
        )
        parameter_annotations = self._handle_Parameters(node.params)
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
        annotation_value = self._handle_Annotation(annotation=node.annotation)
        self.attribute_annotations[".".join(self.qualifier)] = annotation_value
        return True

    def leave_AnnAssign(self, original_node: cst.AnnAssign) -> None:
        self.qualifier.pop()

    def _get_unique_qualified_name(self, node: cst.CSTNode) -> str:
        name = None
        names = [q.name for q in self.get_metadata(QualifiedNameProvider, node)]
        if len(names) == 0:
            # we hit this branch if the stub is directly using a fully
            # qualified name, which is not technically valid python but is
            # convenient to allow.
            name = get_full_name_for_node(node)
        elif len(names) == 1 and isinstance(names[0], str):
            name = names[0]
        if name is None:
            start = self.get_metadata(PositionProvider, node).start
            raise ValueError(
                "Could not resolve a unique qualified name for type "
                + f"{get_full_name_for_node(node)} at {start.line}:{start.column}. "
                + f"Candidate names were: {names!r}"
            )
        return name

    def _get_qualified_name_and_dequalified_node(
        self,
        node: Union[cst.Name, cst.Attribute],
    ) -> Tuple[str, Union[cst.Name, cst.Attribute]]:
        qualified_name = self._get_unique_qualified_name(node)
        dequalified_node = node.attr if isinstance(node, cst.Attribute) else node
        return qualified_name, dequalified_node

    def _handle_qualification_and_should_qualify(self, qualified_name: str) -> bool:
        """
        Basd on a qualified name and the existing module imports, record that
        we need to add an import if necessary and return whether or not we
        should use the qualified name due to a preexisting import.
        """
        split_name = qualified_name.split(".")
        if len(split_name) > 1 and qualified_name not in self.existing_imports:
            module, target = ".".join(split_name[:-1]), split_name[-1]
            if module == "builtins":
                return False
            elif module in self.existing_imports:
                return True
            else:
                AddImportsVisitor.add_needed_import(self.context, module, target)
                return False
        return False

    # Handler functions.
    #
    # Each of these does one of two things, possibly recursively, over some
    # valid CST node for a static type:
    #  - process the qualified name and ensure we will add necessary imports
    #  - dequalify the node

    def _handle_NameOrAttribute(
        self,
        node: NameOrAttribute,
    ) -> Union[cst.Name, cst.Attribute]:
        (
            qualified_name,
            dequalified_node,
        ) = self._get_qualified_name_and_dequalified_node(node)
        should_qualify = self._handle_qualification_and_should_qualify(qualified_name)
        if should_qualify:
            return node
        else:
            return dequalified_node

    def _handle_Index(self, slice: cst.Index, node: cst.Subscript) -> cst.Index:
        value = slice.value
        if isinstance(value, cst.Subscript):
            return slice.with_changes(value=self._handle_Subscript(value))
        elif isinstance(value, cst.Attribute):
            return slice.with_changes(value=self._handle_NameOrAttribute(value))
        else:
            return slice

    def _handle_Subscript(self, node: cst.Subscript) -> cst.Subscript:
        value = node.value
        if isinstance(value, NAME_OR_ATTRIBUTE):
            new_node = node.with_changes(value=self._handle_NameOrAttribute(value))
        else:
            raise ValueError("Expected any indexed type to have")
        if self._get_unique_qualified_name(node) in ("Type", "typing.Type"):
            # Note: we are intentionally not handling qualification of
            # anything inside `Type` because it's common to have nested
            # classes, which we cannot currently distinguish from classes
            # coming from other modules, appear here.
            return new_node
        slice = node.slice
        if isinstance(slice, tuple):
            new_slice = []
            for item in slice:
                value = item.slice.value
                if isinstance(value, NAME_OR_ATTRIBUTE):
                    name = self._handle_NameOrAttribute(item.slice.value)
                    new_index = item.slice.with_changes(value=name)
                    new_slice.append(item.with_changes(slice=new_index))
                else:
                    if isinstance(item.slice, cst.Index):
                        new_index = item.slice.with_changes(
                            value=self._handle_Index(item.slice, item)
                        )
                        item = item.with_changes(slice=new_index)
                    new_slice.append(item)
            return new_node.with_changes(slice=tuple(new_slice))
        elif isinstance(slice, cst.Index):
            new_slice = self._handle_Index(slice)
            return new_node.with_changes(slice=new_slice)
        else:
            return new_node

    def _handle_Annotation(self, annotation: cst.Annotation) -> cst.Annotation:
        node = annotation.annotation
        if isinstance(node, cst.SimpleString):
            return annotation
        elif isinstance(node, cst.Subscript):
            return cst.Annotation(annotation=self._handle_Subscript(node))
        elif isinstance(node, NAME_OR_ATTRIBUTE):
            return cst.Annotation(annotation=self._handle_NameOrAttribute(node))
        else:
            raise ValueError(f"Unexpected annotation node: {node}")

    def _handle_Parameters(self, parameters: cst.Parameters) -> cst.Parameters:
        def update_annotations(parameters: Sequence[cst.Param]) -> List[cst.Param]:
            updated_parameters = []
            for parameter in list(parameters):
                annotation = parameter.annotation
                if annotation is not None:
                    parameter = parameter.with_changes(
                        annotation=self._handle_Annotation(annotation=annotation)
                    )
                updated_parameters.append(parameter)
            return updated_parameters

        return parameters.with_changes(params=update_annotations(parameters.params))


@dataclass(frozen=True)
class Annotations:
    function_annotations: Dict[str, FunctionAnnotation] = field(default_factory=dict)
    attribute_annotations: Dict[str, cst.Annotation] = field(default_factory=dict)
    class_definitions: Dict[str, cst.ClassDef] = field(default_factory=dict)


@dataclass
class AnnotationCounts:
    global_annotations: int = 0
    attribute_annotations: int = 0
    parameter_annotations: int = 0
    return_annotations: int = 0
    classes_added: int = 0

    def any_changes_applied(self) -> bool:
        return (
            self.global_annotations
            + self.attribute_annotations
            + self.parameter_annotations
            + self.return_annotations
            + self.classes_added
        ) > 0


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
        use_future_annotations: bool = False,
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
        self.use_future_annotations = use_future_annotations

        # We use this to determine the end of the import block so that we can
        # insert top-level annotations.
        self.import_statements: List[cst.ImportFrom] = []

        # We use this to report annotations added, as well as to determine
        # whether to abandon the codemod in edge cases where we may have
        # only made changes to the imports.
        self.annotation_counts: AnnotationCounts = AnnotationCounts()

    @staticmethod
    def store_stub_in_context(
        context: CodemodContext,
        stub: cst.Module,
        overwrite_existing_annotations: bool = False,
        use_future_annotations: bool = False,
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
            use_future_annotations,
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
            (
                stub,
                overwrite_existing_annotations,
                use_future_annotations,
            ) = context_contents
            self.overwrite_existing_annotations = (
                self.overwrite_existing_annotations or overwrite_existing_annotations
            )
            self.use_future_annotations = (
                self.use_future_annotations or use_future_annotations
            )
            visitor = TypeCollector(existing_import_names, self.context)
            cst.MetadataWrapper(stub).visit(visitor)
            self.annotations.function_annotations.update(visitor.function_annotations)
            self.annotations.attribute_annotations.update(visitor.attribute_annotations)
            self.annotations.class_definitions.update(visitor.class_definitions)

        tree_with_imports = AddImportsVisitor(
            context=self.context,
            imports=(
                [("__future__", "annotations", None)]
                if self.use_future_annotations
                else ()
            ),
        ).transform_module(tree)
        tree_with_changes = tree_with_imports.visit(self)

        # don't modify the imports if we didn't actually add any type information
        if self.annotation_counts.any_changes_applied():
            return tree_with_changes
        else:
            return tree

    # smart constructors: all applied annotations happen via one of these

    def _apply_annotation_to_attribute_or_global(
        self,
        name: str,
        annotation: cst.Annotation,
        value: Optional[cst.BaseExpression],
    ) -> cst.AnnAssign:
        if len(self.qualifier) == 0:
            self.annotation_counts.global_annotations += 1
        else:
            self.annotation_counts.attribute_annotations += 1
        return cst.AnnAssign(cst.Name(name), annotation, value)

    def _apply_annotation_to_parameter(
        self,
        parameter: cst.Param,
        annotation: cst.Annotation,
    ) -> cst.Param:
        self.annotation_counts.parameter_annotations += 1
        return parameter.with_changes(
            annotation=annotation,
        )

    def _apply_annotation_to_return(
        self,
        function_def: cst.FunctionDef,
        annotation: cst.Annotation,
    ) -> cst.FunctionDef:
        self.annotation_counts.return_annotations += 1
        return function_def.with_changes(returns=annotation)

    # private methods used in the visit and leave methods

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
                    return self._apply_annotation_to_attribute_or_global(
                        name=name,
                        annotation=annotation,
                        value=node.value,
                    )
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
                    parameter = self._apply_annotation_to_parameter(
                        parameter=parameter,
                        annotation=parameter_annotations[key],
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

    # transform API methods

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
            set_return_annotation = (
                self.overwrite_existing_annotations or updated_node.returns is None
            )
            if set_return_annotation and function_annotation.returns is not None:
                updated_node = self._apply_annotation_to_return(
                    function_def=updated_node,
                    annotation=function_annotation.returns,
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
            annotated_assign = self._apply_annotation_to_attribute_or_global(
                name=name,
                annotation=annotation,
                value=None,
            )
            toplevel_statements.append(cst.SimpleStatementLine([annotated_assign]))

        self.annotation_counts.classes_added = len(fresh_class_definitions)
        toplevel_statements.extend(fresh_class_definitions)

        return updated_node.with_changes(
            body=[
                *statements_before_imports,
                *toplevel_statements,
                *statements_after_imports,
            ]
        )
