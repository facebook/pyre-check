# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

# flake8: noqa
# VENDORED FROM Instagram/LibCST/libcst/codemod/visitors/_apply_type_annotations.py ON COMMIT f7417febe71c1fa8581b40dbf456d7d7d6025e62

from dataclasses import dataclass
from typing import Dict, List, Optional, Sequence, Set, Tuple, Union

import libcst as cst
import libcst.matchers as m
from libcst.codemod._context import CodemodContext
from libcst.codemod._visitor import ContextAwareTransformer
from libcst.codemod.visitors._add_imports import AddImportsVisitor
from libcst.codemod.visitors._gather_imports import GatherImportsVisitor
from libcst.helpers import get_full_name_for_node
from libcst.metadata import PositionProvider, QualifiedNameProvider

from ._gather_global_names import GatherGlobalNamesVisitor


NameOrAttribute = Union[cst.Name, cst.Attribute]
NAME_OR_ATTRIBUTE = (cst.Name, cst.Attribute)
# Union type for *args and **args
StarParamType = Union[
    None,
    cst._maybe_sentinel.MaybeSentinel,
    cst._nodes.expression.Param,
    cst._nodes.expression.ParamStar,
]


def _get_import_alias_names(
    import_aliases: Sequence[cst.ImportAlias],
) -> Set[str]:
    import_names = set()
    for imported_name in import_aliases:
        asname = imported_name.asname
        if asname is not None:
            import_names.add(get_full_name_for_node(asname.name))
        else:
            import_names.add(get_full_name_for_node(imported_name.name))
    return import_names


def _get_imported_names(
    imports: Sequence[Union[cst.Import, cst.ImportFrom]],
) -> Set[str]:
    """
    Given a series of import statements (both Import and ImportFrom),
    determine all of the names that have been imported into the current
    scope. For example:
    - ``import foo.bar as bar, foo.baz`` produces ``{'bar', 'foo.baz'}``
    - ``from foo import (Bar, Baz as B)`` produces ``{'Bar', 'B'}``
    - ``from foo import *`` produces ``set()` because we cannot resolve names
    """
    import_names = set()
    for _import in imports:
        if isinstance(_import, cst.Import):
            import_names.update(_get_import_alias_names(_import.names))
        else:
            names = _import.names
            if not isinstance(names, cst.ImportStar):
                import_names.update(_get_import_alias_names(names))
    return import_names


def _is_non_sentinel(
    x: Union[None, cst.CSTNode, cst.MaybeSentinel],
) -> bool:
    return x is not None and x != cst.MaybeSentinel.DEFAULT


def _get_string_value(
    node: cst.SimpleString,
) -> str:
    s = node.value
    c = s[-1]
    return s[s.index(c) : -1]


def _find_generic_base(
    node: cst.ClassDef,
) -> Optional[cst.Arg]:
    for b in node.bases:
        if m.matches(b.value, m.Subscript(value=m.Name("Generic"))):
            return b


@dataclass(frozen=True)
class FunctionKey:
    """
    Class representing a funciton name and signature.

    This exists to ensure we do not attempt to apply stubs to functions whose
    definition is incompatible.
    """

    name: str
    pos: int
    kwonly: str
    posonly: int
    star_arg: bool
    star_kwarg: bool

    @classmethod
    def make(
        cls,
        name: str,
        params: cst.Parameters,
    ) -> "FunctionKey":
        pos = len(params.params)
        kwonly = ",".join(sorted(x.name.value for x in params.kwonly_params))
        posonly = len(params.posonly_params)
        star_arg = _is_non_sentinel(params.star_arg)
        star_kwarg = _is_non_sentinel(params.star_kwarg)
        return cls(
            name,
            pos,
            kwonly,
            posonly,
            star_arg,
            star_kwarg,
        )


@dataclass(frozen=True)
class FunctionAnnotation:
    parameters: cst.Parameters
    returns: Optional[cst.Annotation]


@dataclass
class Annotations:
    """
    Represents all of the annotation information we might add to
    a class:
    - All data is keyed on the qualified name relative to the module root
    - The ``functions`` field also keys on the signature so that we
      do not apply stub types where the signature is incompatible.

    The idea is that
    - ``functions`` contains all function and method type
      information from the stub, and the qualifier for a method includes
      the containing class names (e.g. "Cat.meow")
    - ``attributes`` similarly contains all globals
      and class-level attribute type information.
    - The ``class_definitions`` field contains all of the classes
      defined in the stub. Most of these classes will be ignored in
      downstream logic (it is *not* used to annotate attributes or
      method), but there are some cases like TypedDict where a
      typing-only class needs to be injected.
    - The field ``typevars`` contains the assign statement for all
      type variables in the stub, and ``names`` tracks
      all of the names used in annotations; together these fields
      tell us which typevars should be included in the codemod
      (all typevars that appear in annotations.)
    """

    # TODO: consider simplifying this in a few ways:
    # - We could probably just inject all typevars, used or not.
    #   It doesn't seem to me that our codemod needs to act like
    #   a linter checking for unused names.
    # - We could probably decide which classes are typing-only
    #   in the visitor rather than the codemod, which would make
    #   it easier to reason locally about (and document) how the
    #   class_definitions field works.

    functions: Dict[FunctionKey, FunctionAnnotation]
    attributes: Dict[str, cst.Annotation]
    class_definitions: Dict[str, cst.ClassDef]
    typevars: Dict[str, cst.Assign]
    names: Set[str]

    @classmethod
    def empty(cls) -> "Annotations":
        return Annotations({}, {}, {}, {}, set())

    def update(self, other: "Annotations") -> None:
        self.functions.update(other.functions)
        self.attributes.update(other.attributes)
        self.class_definitions.update(other.class_definitions)
        self.typevars.update(other.typevars)
        self.names.update(other.names)

    def finish(self) -> None:
        self.typevars = {k: v for k, v in self.typevars.items() if k in self.names}


class TypeCollector(m.MatcherDecoratableVisitor):
    """
    Collect type annotations from a stub module.
    """

    METADATA_DEPENDENCIES = (
        PositionProvider,
        QualifiedNameProvider,
    )

    annotations: Annotations

    def __init__(
        self,
        existing_imports: Set[str],
        context: CodemodContext,
    ) -> None:
        super().__init__()
        self.context = context
        # Existing imports, determined by looking at the target module.
        # Used to help us determine when a type in a stub will require new imports.
        #
        # The contents of this are fully-qualified names of types in scope
        # as well as module names, although downstream we effectively ignore
        # the module names as of the current implementation.
        self.existing_imports: Set[str] = existing_imports
        # Fields that help us track temporary state as we recurse
        self.qualifier: List[str] = []
        self.current_assign: Optional[cst.Assign] = None  # used to collect typevars
        # Store the annotations.
        self.annotations = Annotations.empty()

    def visit_ClassDef(
        self,
        node: cst.ClassDef,
    ) -> None:
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

        self.annotations.class_definitions[node.name.value] = node.with_changes(
            bases=new_bases
        )

    def leave_ClassDef(
        self,
        original_node: cst.ClassDef,
    ) -> None:
        self.qualifier.pop()

    def visit_FunctionDef(
        self,
        node: cst.FunctionDef,
    ) -> bool:
        self.qualifier.append(node.name.value)
        returns = node.returns
        return_annotation = (
            self._handle_Annotation(annotation=returns) if returns is not None else None
        )
        parameter_annotations = self._handle_Parameters(node.params)
        name = ".".join(self.qualifier)
        key = FunctionKey.make(name, node.params)
        self.annotations.functions[key] = FunctionAnnotation(
            parameters=parameter_annotations, returns=return_annotation
        )

        # pyi files don't support inner functions, return False to stop the traversal.
        return False

    def leave_FunctionDef(
        self,
        original_node: cst.FunctionDef,
    ) -> None:
        self.qualifier.pop()

    def visit_AnnAssign(
        self,
        node: cst.AnnAssign,
    ) -> bool:
        name = get_full_name_for_node(node.target)
        if name is not None:
            self.qualifier.append(name)
        annotation_value = self._handle_Annotation(annotation=node.annotation)
        self.annotations.attributes[".".join(self.qualifier)] = annotation_value
        return True

    def leave_AnnAssign(
        self,
        original_node: cst.AnnAssign,
    ) -> None:
        self.qualifier.pop()

    def visit_Assign(
        self,
        node: cst.Assign,
    ) -> None:
        self.current_assign = node

    def leave_Assign(
        self,
        original_node: cst.Assign,
    ) -> None:
        self.current_assign = None

    @m.call_if_inside(m.Assign())
    @m.visit(m.Call(func=m.Name("TypeVar")))
    def record_typevar(
        self,
        node: cst.Call,
    ) -> None:
        # pyre-ignore current_assign is never None here
        name = get_full_name_for_node(self.current_assign.targets[0].target)
        if name is not None:
            # pyre-ignore current_assign is never None here
            self.annotations.typevars[name] = self.current_assign
            self._handle_qualification_and_should_qualify("typing.TypeVar")
            self.current_assign = None

    def leave_Module(
        self,
        original_node: cst.Module,
    ) -> None:
        self.annotations.finish()

    def _get_unique_qualified_name(
        self,
        node: cst.CSTNode,
    ) -> str:
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

    def _module_and_target(
        self,
        qualified_name: str,
    ) -> Tuple[str, str]:
        relative_prefix = ""
        while qualified_name.startswith("."):
            relative_prefix += "."
            qualified_name = qualified_name[1:]
        split = qualified_name.rsplit(".", 1)
        if len(split) == 1:
            qualifier, target = "", split[0]
        else:
            qualifier, target = split
        return (relative_prefix + qualifier, target)

    def _handle_qualification_and_should_qualify(
        self,
        qualified_name: str,
    ) -> bool:
        """
        Based on a qualified name and the existing module imports, record that
        we need to add an import if necessary and return whether or not we
        should use the qualified name due to a preexisting import.
        """
        module, target = self._module_and_target(qualified_name)
        if module in ("", "builtins"):
            return False
        elif qualified_name not in self.existing_imports:
            if module in self.existing_imports:
                return True
            else:
                AddImportsVisitor.add_needed_import(
                    self.context,
                    module,
                    target,
                )
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
        self.annotations.names.add(qualified_name)
        if should_qualify:
            return node
        else:
            return dequalified_node

    def _handle_Index(
        self,
        slice: cst.Index,
    ) -> cst.Index:
        value = slice.value
        if isinstance(value, cst.Subscript):
            return slice.with_changes(value=self._handle_Subscript(value))
        elif isinstance(value, cst.Attribute):
            return slice.with_changes(value=self._handle_NameOrAttribute(value))
        else:
            if isinstance(value, cst.SimpleString):
                self.annotations.names.add(_get_string_value(value))
            return slice

    def _handle_Subscript(
        self,
        node: cst.Subscript,
    ) -> cst.Subscript:
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
                            value=self._handle_Index(item.slice)
                        )
                        item = item.with_changes(slice=new_index)
                    new_slice.append(item)
            return new_node.with_changes(slice=tuple(new_slice))
        elif isinstance(slice, cst.Index):
            new_slice = self._handle_Index(slice)
            return new_node.with_changes(slice=new_slice)
        else:
            return new_node

    def _handle_Annotation(
        self,
        annotation: cst.Annotation,
    ) -> cst.Annotation:
        node = annotation.annotation
        if isinstance(node, cst.SimpleString):
            self.annotations.names.add(_get_string_value(node))
            return annotation
        elif isinstance(node, cst.Subscript):
            return cst.Annotation(annotation=self._handle_Subscript(node))
        elif isinstance(node, NAME_OR_ATTRIBUTE):
            return cst.Annotation(annotation=self._handle_NameOrAttribute(node))
        else:
            raise ValueError(f"Unexpected annotation node: {node}")

    def _handle_Parameters(
        self,
        parameters: cst.Parameters,
    ) -> cst.Parameters:
        def update_annotations(
            parameters: Sequence[cst.Param],
        ) -> List[cst.Param]:
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


@dataclass
class AnnotationCounts:
    global_annotations: int = 0
    attribute_annotations: int = 0
    parameter_annotations: int = 0
    return_annotations: int = 0
    classes_added: int = 0
    typevars_and_generics_added: int = 0

    def any_changes_applied(self) -> bool:
        return (
            self.global_annotations
            + self.attribute_annotations
            + self.parameter_annotations
            + self.return_annotations
            + self.classes_added
            + self.typevars_and_generics_added
        ) > 0


class ApplyTypeAnnotationsVisitor(ContextAwareTransformer):
    """
    Apply type annotations to a source module using the given stub mdules.
    You can also pass in explicit annotations for functions and attributes and
    pass in new class definitions that need to be added to the source module.

    This is one of the transforms that is available automatically to you when
    running a codemod. To use it in this manner, import
    :class:`~libcst.codemod.visitors.ApplyTypeAnnotationsVisitor` and then call
    the static
    :meth:`~libcst.codemod.visitors.ApplyTypeAnnotationsVisitor.store_stub_in_context`
    method, giving it the current context (found as ``self.context`` for all
    subclasses of :class:`~libcst.codemod.Codemod`), the stub module from which
    you wish to add annotations.

    For example, you can store the type annotation ``int`` for ``x`` using::

        stub_module = parse_module("x: int = ...")

        ApplyTypeAnnotationsVisitor.store_stub_in_context(self.context, stub_module)

    You can apply the type annotation using::

        source_module = parse_module("x = 1")
        ApplyTypeAnnotationsVisitor.transform_module(source_module)

    This will produce the following code::

        x: int = 1

    If the function or attribute already has a type annotation, it will not be
    overwritten.

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
        strict_posargs_matching: bool = True,
        strict_annotation_matching: bool = False,
    ) -> None:
        super().__init__(context)
        # Qualifier for storing the canonical name of the current function.
        self.qualifier: List[str] = []
        self.annotations: Annotations = (
            Annotations.empty() if annotations is None else annotations
        )
        self.toplevel_annotations: Dict[str, cst.Annotation] = {}
        self.visited_classes: Set[str] = set()
        self.overwrite_existing_annotations = overwrite_existing_annotations
        self.use_future_annotations = use_future_annotations
        self.strict_posargs_matching = strict_posargs_matching
        self.strict_annotation_matching = strict_annotation_matching

        # We use this to determine the end of the import block so that we can
        # insert top-level annotations.
        self.import_statements: List[cst.ImportFrom] = []

        # We use this to report annotations added, as well as to determine
        # whether to abandon the codemod in edge cases where we may have
        # only made changes to the imports.
        self.annotation_counts: AnnotationCounts = AnnotationCounts()

        # We use this to collect typevars, to avoid importing existing ones from the pyi file
        self.current_assign: Optional[cst.Assign] = None
        self.typevars: Dict[str, cst.Assign] = {}

        # Global variables and classes defined on the toplevel of the target module.
        # Used to help determine which names we need to check are in scope, and add
        # quotations to avoid undefined forward references in type annotations.
        self.global_names: Set[str] = set()

    @staticmethod
    def store_stub_in_context(
        context: CodemodContext,
        stub: cst.Module,
        overwrite_existing_annotations: bool = False,
        use_future_annotations: bool = False,
        strict_posargs_matching: bool = True,
        strict_annotation_matching: bool = False,
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
            strict_posargs_matching,
            strict_annotation_matching,
        )

    def transform_module_impl(
        self,
        tree: cst.Module,
    ) -> cst.Module:
        """
        Collect type annotations from all stubs and apply them to ``tree``.

        Gather existing imports from ``tree`` so that we don't add duplicate imports.

        Gather global names from ``tree`` so forward references are quoted.
        """
        import_gatherer = GatherImportsVisitor(CodemodContext())
        tree.visit(import_gatherer)
        existing_import_names = _get_imported_names(import_gatherer.all_imports)

        global_names_gatherer = GatherGlobalNamesVisitor(CodemodContext())
        tree.visit(global_names_gatherer)
        self.global_names = global_names_gatherer.global_names.union(
            global_names_gatherer.class_names
        )

        context_contents = self.context.scratch.get(
            ApplyTypeAnnotationsVisitor.CONTEXT_KEY
        )
        if context_contents is not None:
            (
                stub,
                overwrite_existing_annotations,
                use_future_annotations,
                strict_posargs_matching,
                strict_annotation_matching,
            ) = context_contents
            self.overwrite_existing_annotations = (
                self.overwrite_existing_annotations or overwrite_existing_annotations
            )
            self.use_future_annotations = (
                self.use_future_annotations or use_future_annotations
            )
            self.strict_posargs_matching = (
                self.strict_posargs_matching and strict_posargs_matching
            )
            self.strict_annotation_matching = (
                self.strict_annotation_matching or strict_annotation_matching
            )
            visitor = TypeCollector(existing_import_names, self.context)
            cst.MetadataWrapper(stub).visit(visitor)
            self.annotations.update(visitor.annotations)

            if self.use_future_annotations:
                AddImportsVisitor.add_needed_import(
                    self.context, "__future__", "annotations"
                )
            tree_with_imports = AddImportsVisitor(self.context).transform_module(tree)

        tree_with_changes = tree_with_imports.visit(self)

        # don't modify the imports if we didn't actually add any type information
        if self.annotation_counts.any_changes_applied():
            return tree_with_changes
        else:
            return tree

    # helpers for processing annotation nodes
    def _quote_future_annotations(self, annotation: cst.Annotation) -> cst.Annotation:
        # TODO: We probably want to make sure references to classes defined in the current
        # module come to us fully qualified - so we can do the dequalification here and
        # know to look for what is in-scope without also catching builtins like "None" in the
        # quoting. This should probably also be extended to handle what imports are in scope,
        # as well as subscriptable types.
        # Note: We are collecting all imports and passing this to the type collector grabbing
        # annotations from the stub file; should consolidate import handling somewhere too.
        node = annotation.annotation
        if (
            isinstance(node, cst.Name)
            and (node.value in self.global_names)
            and not (node.value in self.visited_classes)
        ):
            return annotation.with_changes(
                annotation=cst.SimpleString(value=f'"{node.value}"')
            )
        return annotation

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
        return cst.AnnAssign(
            cst.Name(name),
            self._quote_future_annotations(annotation),
            value,
        )

    def _apply_annotation_to_parameter(
        self,
        parameter: cst.Param,
        annotation: cst.Annotation,
    ) -> cst.Param:
        self.annotation_counts.parameter_annotations += 1
        return parameter.with_changes(
            annotation=self._quote_future_annotations(annotation),
        )

    def _apply_annotation_to_return(
        self,
        function_def: cst.FunctionDef,
        annotation: cst.Annotation,
    ) -> cst.FunctionDef:
        self.annotation_counts.return_annotations += 1
        return function_def.with_changes(
            returns=self._quote_future_annotations(annotation),
        )

    # private methods used in the visit and leave methods

    def _qualifier_name(self) -> str:
        return ".".join(self.qualifier)

    def _annotate_single_target(
        self,
        node: cst.Assign,
        updated_node: cst.Assign,
    ) -> Union[cst.Assign, cst.AnnAssign]:
        only_target = node.targets[0].target
        if isinstance(only_target, (cst.Tuple, cst.List)):
            for element in only_target.elements:
                value = element.value
                name = get_full_name_for_node(value)
                if name is not None and name != "_":
                    self._add_to_toplevel_annotations(name)
        elif isinstance(only_target, (cst.Subscript)):
            pass
        else:
            name = get_full_name_for_node(only_target)
            if name is not None:
                self.qualifier.append(name)
                if (
                    self._qualifier_name() in self.annotations.attributes
                    and not isinstance(only_target, cst.Subscript)
                ):
                    annotation = self.annotations.attributes[self._qualifier_name()]
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
        self,
        module: cst.Module,
        updated_module: cst.Module,
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

    def _add_to_toplevel_annotations(
        self,
        name: str,
    ) -> None:
        self.qualifier.append(name)
        if self._qualifier_name() in self.annotations.attributes:
            annotation = self.annotations.attributes[self._qualifier_name()]
            self.toplevel_annotations[name] = annotation
        self.qualifier.pop()

    def _update_parameters(
        self,
        annotations: FunctionAnnotation,
        updated_node: cst.FunctionDef,
    ) -> cst.Parameters:
        # Update params and default params with annotations
        # Don't override existing annotations or default values unless asked
        # to overwrite existing annotations.
        def update_annotation(
            parameters: Sequence[cst.Param],
            annotations: Sequence[cst.Param],
            positional: bool,
        ) -> List[cst.Param]:
            parameter_annotations = {}
            annotated_parameters = []
            positional = positional and not self.strict_posargs_matching
            for i, parameter in enumerate(annotations):
                key = i if positional else parameter.name.value
                if parameter.annotation:
                    parameter_annotations[key] = parameter.annotation.with_changes(
                        whitespace_before_indicator=cst.SimpleWhitespace(value="")
                    )
            for i, parameter in enumerate(parameters):
                key = i if positional else parameter.name.value
                if key in parameter_annotations and (
                    self.overwrite_existing_annotations or not parameter.annotation
                ):
                    parameter = self._apply_annotation_to_parameter(
                        parameter=parameter,
                        annotation=parameter_annotations[key],
                    )
                annotated_parameters.append(parameter)
            return annotated_parameters

        return updated_node.params.with_changes(
            params=update_annotation(
                updated_node.params.params,
                annotations.parameters.params,
                positional=True,
            ),
            kwonly_params=update_annotation(
                updated_node.params.kwonly_params,
                annotations.parameters.kwonly_params,
                positional=False,
            ),
            posonly_params=update_annotation(
                updated_node.params.posonly_params,
                annotations.parameters.posonly_params,
                positional=True,
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

    def _match_signatures(  # noqa: C901: Too complex
        self,
        function: cst.FunctionDef,
        annotations: FunctionAnnotation,
    ) -> bool:
        """Check that function annotations on both signatures are compatible."""

        def compatible(
            p: Optional[cst.Annotation],
            q: Optional[cst.Annotation],
        ) -> bool:
            if (
                self.overwrite_existing_annotations
                or not _is_non_sentinel(p)
                or not _is_non_sentinel(q)
            ):
                return True
            if not self.strict_annotation_matching:
                # We will not overwrite clashing annotations, but the signature as a
                # whole will be marked compatible so that holes can be filled in.
                return True
            return p.annotation.deep_equals(q.annotation)  # pyre-ignore[16]

        def match_posargs(
            ps: Sequence[cst.Param],
            qs: Sequence[cst.Param],
        ) -> bool:
            if len(ps) != len(qs):
                return False
            for p, q in zip(ps, qs):
                if self.strict_posargs_matching and not p.name.value == q.name.value:
                    return False
                if not compatible(p.annotation, q.annotation):
                    return False
            return True

        def match_kwargs(
            ps: Sequence[cst.Param],
            qs: Sequence[cst.Param],
        ) -> bool:
            ps_dict = {x.name.value: x for x in ps}
            qs_dict = {x.name.value: x for x in qs}
            if set(ps_dict.keys()) != set(qs_dict.keys()):
                return False
            for k in ps_dict.keys():
                if not compatible(ps_dict[k].annotation, qs_dict[k].annotation):
                    return False
            return True

        def match_star(
            p: StarParamType,
            q: StarParamType,
        ) -> bool:
            return _is_non_sentinel(p) == _is_non_sentinel(q)

        def match_params(
            f: cst.FunctionDef,
            g: FunctionAnnotation,
        ) -> bool:
            p, q = f.params, g.parameters
            return (
                match_posargs(p.params, q.params)
                and match_posargs(p.posonly_params, q.posonly_params)
                and match_kwargs(p.kwonly_params, q.kwonly_params)
                and match_star(p.star_arg, q.star_arg)
                and match_star(p.star_kwarg, q.star_kwarg)
            )

        def match_return(
            f: cst.FunctionDef,
            g: FunctionAnnotation,
        ) -> bool:
            return compatible(f.returns, g.returns)

        return match_params(function, annotations) and match_return(
            function, annotations
        )

    # transform API methods

    def visit_ClassDef(
        self,
        node: cst.ClassDef,
    ) -> None:
        self.qualifier.append(node.name.value)

    def leave_ClassDef(
        self,
        original_node: cst.ClassDef,
        updated_node: cst.ClassDef,
    ) -> cst.ClassDef:
        self.visited_classes.add(original_node.name.value)
        cls_name = ".".join(self.qualifier)
        self.qualifier.pop()
        definition = self.annotations.class_definitions.get(cls_name)
        if definition:
            b1 = _find_generic_base(definition)
            b2 = _find_generic_base(updated_node)
            if b1 and not b2:
                new_bases = list(updated_node.bases) + [b1]
                self.annotation_counts.typevars_and_generics_added += 1
                return updated_node.with_changes(bases=new_bases)
        return updated_node

    def visit_FunctionDef(
        self,
        node: cst.FunctionDef,
    ) -> bool:
        self.qualifier.append(node.name.value)
        # pyi files don't support inner functions, return False to stop the traversal.
        return False

    def leave_FunctionDef(
        self,
        original_node: cst.FunctionDef,
        updated_node: cst.FunctionDef,
    ) -> cst.FunctionDef:
        key = FunctionKey.make(self._qualifier_name(), updated_node.params)
        self.qualifier.pop()
        if key in self.annotations.functions:
            function_annotation = self.annotations.functions[key]
            # Only add new annotation if:
            # * we have matching function signatures and
            # * we are explicitly told to overwrite existing annotations or
            # * there is no existing annotation
            if not self._match_signatures(updated_node, function_annotation):
                return updated_node
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

    def visit_Assign(
        self,
        node: cst.Assign,
    ) -> None:
        self.current_assign = node

    @m.call_if_inside(m.Assign())
    @m.visit(m.Call(func=m.Name("TypeVar")))
    def record_typevar(
        self,
        node: cst.Call,
    ) -> None:
        # pyre-ignore current_assign is never None here
        name = get_full_name_for_node(self.current_assign.targets[0].target)
        if name is not None:
            # Preserve the whole node, even though we currently just use the
            # name, so that we can match bounds and variance at some point and
            # determine if two typevars with the same name are indeed the same.

            # pyre-ignore current_assign is never None here
            self.typevars[name] = self.current_assign
            self.current_assign = None

    def leave_Assign(
        self,
        original_node: cst.Assign,
        updated_node: cst.Assign,
    ) -> Union[cst.Assign, cst.AnnAssign]:
        self.current_assign = None

        if len(original_node.targets) > 1:
            for assign in original_node.targets:
                target = assign.target
                if isinstance(target, (cst.Name, cst.Attribute)):
                    name = get_full_name_for_node(target)
                    if name is not None and name != "_":
                        # Add separate top-level annotations for `a = b = 1`
                        # as `a: int` and `b: int`.
                        self._add_to_toplevel_annotations(name)
            return updated_node
        else:
            return self._annotate_single_target(original_node, updated_node)

    def leave_ImportFrom(
        self,
        original_node: cst.ImportFrom,
        updated_node: cst.ImportFrom,
    ) -> cst.ImportFrom:
        self.import_statements.append(original_node)
        return updated_node

    def leave_Module(
        self,
        original_node: cst.Module,
        updated_node: cst.Module,
    ) -> cst.Module:
        fresh_class_definitions = [
            definition
            for name, definition in self.annotations.class_definitions.items()
            if name not in self.visited_classes
        ]

        # NOTE: The entire change will also be abandoned if
        # self.annotation_counts is all 0s, so if adding any new category make
        # sure to record it there.
        if not (
            self.toplevel_annotations
            or fresh_class_definitions
            or self.annotations.typevars
        ):
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

        # TypeVar definitions could be scattered through the file, so do not
        # attempt to put new ones with existing ones, just add them at the top.
        typevars = {
            k: v for k, v in self.annotations.typevars.items() if k not in self.typevars
        }
        if typevars:
            for var, stmt in typevars.items():
                toplevel_statements.append(cst.Newline())
                toplevel_statements.append(stmt)
                self.annotation_counts.typevars_and_generics_added += 1
            toplevel_statements.append(cst.Newline())

        self.annotation_counts.classes_added = len(fresh_class_definitions)
        toplevel_statements.extend(fresh_class_definitions)

        return updated_node.with_changes(
            body=[
                *statements_before_imports,
                *toplevel_statements,
                *statements_after_imports,
            ]
        )
