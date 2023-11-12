# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module provides libcst transform classes for applying various
stub patches to open-source typeshed stubs.
"""
from __future__ import annotations

from typing import Callable, Iterable, Sequence

import libcst
import libcst.codemod

from . import patch_specs


def statements_from_content(content: str) -> Sequence[libcst.BaseStatement]:
    """
    Given a content string (originating from a patch toml file),
    parse statements as a CST so that we can apply them in a
    libcst transform.
    """
    try:
        module = libcst.parse_module(content)
        return module.body
    except libcst.ParserSyntaxError as e:
        raise ValueError(f"Failed to parse content:\n---\n{content}\n---\n{e.message}")


def statements_in_indented_block(
    block: libcst.CSTNode,
) -> list[libcst.BaseStatement]:
    if isinstance(block, libcst.IndentedBlock):
        return [statement for statement in block.body]
    else:
        raise ValueError(f"Expected an indented block, got {block}")


def statements_in_if_block(
    block: libcst.If,
) -> list[libcst.BaseStatement]:
    statements = statements_in_indented_block(block.body)
    if block.orelse is None:
        pass
    elif isinstance(block.orelse, libcst.If):
        statements.extend(statements_in_if_block(block.orelse))
    elif isinstance(block.orelse, libcst.Else):
        statements.extend(statements_in_indented_block(block.orelse.body))
    else:
        raise ValueError(f"Unexpected orelse {block.orelse}")
    return statements


def is_matching_if_block(
    block: libcst.If,
    predicate: Callable[[libcst.BaseStatement | libcst.BaseSmallStatement], bool],
) -> bool:
    """
    For the most part stubs do not use control flow. The one common exception
    is the use of if(/else) conditions to gate logic, typically on the Python
    version. This helper allows us to extend statement matchers to support
    if/else blocks in which all the statements in the block match our condition.

    We don't currently handle the case where many different names
    (e.g.  methods on a class) are defined in a single if block. This does
    happen occasionally in stub files, but so far we've never needed to patch
    such a case.

    If we detect that this is likely (the predicate matches on some but not
    all statements), we raise a NotImplementedError rather than failing silently.
    """
    statements = statements_in_if_block(block)
    predicate_evaluations = [predicate(statement) for statement in statements]
    if all(predicate_evaluations):
        return True
    elif any(predicate_evaluations):
        raise NotImplementedError(
            "Typeshed patcher does not yet support complex if statements where "
            "some inner statements match a condition and others don't.\n"
            f"Got this if-statement: {block}"
        )
    else:
        return False


def statement_matches_name(
    name: str,
    statement: libcst.BaseStatement | libcst.BaseSmallStatement,
) -> bool:
    """
    Given a statement in the parent scope, determine whether it
    matches the name (used for delete and replace actions).

    We handle if blocks correctly as long as all the conditions
    are indented blocks consisting statements that themselves
    match the name.
    """
    if isinstance(statement, libcst.SimpleStatementLine):
        if len(statement.body) != 1:
            raise ValueError(
                f"Did not expect compound statement line {statement} "
                "in a stub scope we patch."
            )
        return statement_matches_name(name, statement.body[0])
    if isinstance(statement, libcst.Assign):
        # Regular assignment is unusual in stubs, but happens in type aliases.
        if len(statement.targets) != 1:
            raise ValueError(
                f"Expect only simple assignments in stubs, got {statement}"
            )
        target = statement.targets[0].target
        if not isinstance(target, libcst.Name):
            raise ValueError(
                f"Expect only simple assignments in stubs, got {statement}"
            )
        return target.value == name
    if isinstance(statement, libcst.AnnAssign):
        target = statement.target
        if isinstance(target, libcst.Name):
            return target.value == name
        else:
            raise ValueError(
                "Did not expect non-name target {target} "
                "of AnnAssign in a stub scope we patch."
            )
    if isinstance(statement, libcst.FunctionDef):
        return statement.name.value == name
    if isinstance(statement, libcst.ClassDef):
        return statement.name.value == name
    if isinstance(statement, libcst.If):
        return is_matching_if_block(
            statement,
            predicate=lambda s: statement_matches_name(name, s),
        )
    else:
        return False


def is_import_statement(
    statement: libcst.BaseStatement | libcst.BaseSmallStatement,
) -> bool:
    """
    Given a statement, determine whether it is only performing
    imports.

    We handle if blocks correctly as long as all the conditions
    are indented blocks consisting only of import statements.
    """

    def is_import_indented_block(
        block: libcst.CSTNode,
    ) -> bool:
        return isinstance(block, libcst.IndentedBlock) and all(
            is_import_statement(inner_statement) for inner_statement in block.body
        )

    if isinstance(statement, libcst.ImportFrom) or isinstance(statement, libcst.Import):
        return True
    if isinstance(statement, libcst.SimpleStatementLine) and len(statement.body) == 1:
        return is_import_statement(statement.body[0])
    if isinstance(statement, libcst.If):
        return is_matching_if_block(
            statement,
            predicate=is_import_statement,
        )
    return False


def split_import_header(
    statements: Sequence[libcst.BaseStatement],
) -> tuple[Sequence[libcst.BaseStatement], Sequence[libcst.BaseStatement]]:
    """
    Split a sequence of statements (in our context the existing body of
    some scope) into the leading imports and then everything else.

    Used to handle add actions with position of "top"; we try to add
    at the bottom of the existing import header.

    Note that some complex cases, like an import inside of an if block
    (sometimes used to gate by python version) are not yet supported.
    """
    split_index = 0
    while is_import_statement(statements[split_index]):
        split_index += 1
    return statements[:split_index], statements[split_index:]


def run_add_action(
    action: patch_specs.AddAction,
    existing_body: Sequence[libcst.BaseStatement],
) -> Sequence[libcst.BaseStatement]:
    statements_to_add = statements_from_content(action.content)
    if action.position == patch_specs.AddPosition.TOP_OF_SCOPE:
        import_header, rest_of_body = split_import_header(existing_body)
        return [
            *import_header,
            *statements_to_add,
            *rest_of_body,
        ]
    elif action.position == patch_specs.AddPosition.BOTTOM_OF_SCOPE:
        return [
            *existing_body,
            *statements_to_add,
        ]
    else:
        raise RuntimeError(f"Unknown position {action.position}")


def run_delete_action(
    action: patch_specs.DeleteAction,
    existing_body: Sequence[libcst.BaseStatement],
    parent: str,
) -> Sequence[libcst.BaseStatement]:
    new_body = [
        statement
        for statement in existing_body
        if not statement_matches_name(action.name, statement)
    ]
    # Always make sure we successfully deleted the target. This
    # might fail if the target has disappeared, or if our
    # `matches_name` logic needs to be extended.
    if len(new_body) == len(existing_body):
        raise ValueError(f"Could not find deletion target {action.name} in {parent}")
    # There's an edge case where we delete the entire scope body;
    # we can deal with this by inserting a pass.
    if len(new_body) == 0:
        new_body = [libcst.SimpleStatementLine([libcst.Pass()])]
    return new_body


def run_replace_action(
    action: patch_specs.ReplaceAction,
    existing_body: Sequence[libcst.BaseStatement],
    parent: str,
) -> Sequence[libcst.BaseStatement]:
    statements_to_add = statements_from_content(action.content)
    new_body: list[libcst.BaseStatement] = []
    added_replacements = False
    for statement in existing_body:
        if statement_matches_name(action.name, statement):
            if not added_replacements:
                added_replacements = True
                new_body.extend(statements_to_add)
        else:
            new_body.append(statement)
    if not added_replacements:
        raise ValueError(f"Could not find replacement target {action.name} in {parent}")
    return new_body


def run_action(
    action: patch_specs.Action,
    existing_body: Sequence[libcst.BaseStatement],
    parent: str,
) -> Sequence[libcst.BaseStatement]:
    if isinstance(action, patch_specs.AddAction):
        return run_add_action(
            action=action,
            existing_body=existing_body,
        )
    elif isinstance(action, patch_specs.DeleteAction):
        return run_delete_action(
            action=action,
            existing_body=existing_body,
            parent=parent,
        )
    elif isinstance(action, patch_specs.ReplaceAction):
        return run_replace_action(
            action=action,
            existing_body=existing_body,
            parent=parent,
        )
    else:
        raise NotImplementedError(
            f"No transform implemented yet for patch {type(action)}"
        )


def run_actions_in_sequence(
    actions: Iterable[patch_specs.Action],
    existing_body: Sequence[libcst.BaseStatement],
    parent: str,
) -> Sequence[libcst.BaseStatement]:
    body = existing_body
    for action in actions:
        body = run_action(
            action=action,
            existing_body=body,
            parent=parent,
        )
    return body


class PatchTransform(libcst.codemod.ContextAwareTransformer):

    CONTEXT_KEY = "PatchTransform"

    actions_by_parent: dict[str, list[patch_specs.Action]]
    processed_parents: set[str]

    current_names: list[str]

    def __init__(
        self,
        actions_by_parent: dict[str, list[patch_specs.Action]],
    ) -> None:
        super().__init__(libcst.codemod.CodemodContext())
        self.actions_by_parent = actions_by_parent
        # State to track current scope name and find the parent
        self.current_names = []
        self.processed_parents = set()

    @staticmethod
    def from_patches(
        patches: Iterable[patch_specs.Patch],
    ) -> PatchTransform:
        actions_by_parent: dict[str, list[patch_specs.Action]] = {}
        for patch in patches:
            parent = patch.parent.to_string()
            if parent not in actions_by_parent:
                actions_by_parent[parent] = []
            actions_by_parent[parent].append(patch.action)
        return PatchTransform(actions_by_parent=actions_by_parent)

    def get_current_name(self) -> str:
        return ".".join(self.current_names)

    def pop_current_name(self) -> str:
        this_name = self.get_current_name()
        self.current_names.pop()
        return this_name

    def visit_ClassDef(
        self,
        node: libcst.ClassDef,
    ) -> None:
        self.current_names.append(node.name.value)

    def transform_parent_class(
        self,
        node: libcst.ClassDef,
        actions: Iterable[patch_specs.Action],
        parent: str,
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
                body=run_actions_in_sequence(
                    actions=actions,
                    existing_body=outer_body.body,
                    parent=parent,
                ),
            )
        else:
            inner_body_as_base_statements = [
                statement
                if isinstance(statement, libcst.BaseStatement)
                else libcst.SimpleStatementLine(body=[statement])
                for statement in outer_body.body
            ]
            new_outer_body = libcst.IndentedBlock(
                body=run_actions_in_sequence(
                    actions=actions,
                    existing_body=inner_body_as_base_statements,
                    parent=parent,
                ),
            )
        return node.with_changes(body=new_outer_body)

    def leave_ClassDef(
        self,
        original_node: libcst.ClassDef,
        updated_node: libcst.ClassDef,
    ) -> libcst.ClassDef:
        parent = self.pop_current_name()
        if parent not in self.actions_by_parent:
            return updated_node
        if parent in self.processed_parents:
            raise ValueError(f"Encountered two classes with name {parent}")
        self.processed_parents.add(parent)
        return self.transform_parent_class(
            updated_node,
            actions=self.actions_by_parent[parent],
            parent=parent,
        )

    def transform_global_scope(
        self,
        node: libcst.Module,
    ) -> libcst.Module:
        GLOBAL = ""
        if GLOBAL in self.actions_by_parent:
            self.processed_parents.add(GLOBAL)
            return node.with_changes(
                body=run_actions_in_sequence(
                    actions=self.actions_by_parent[GLOBAL],
                    existing_body=node.body,
                    parent="<global_scope>",
                )
            )
        else:
            return node

    def verify_all_parents_found(self) -> None:
        expected_parents = set(self.actions_by_parent.keys())
        missed_parents = expected_parents - self.processed_parents
        if len(missed_parents) > 0:
            raise ValueError(f"Did not find parents {missed_parents}")

    def leave_Module(
        self,
        original_node: libcst.Module,
        updated_node: libcst.Module,
    ) -> libcst.Module:
        node = self.transform_global_scope(updated_node)
        self.verify_all_parents_found()
        return node


def apply_patches_in_sequence(
    code: str,
    patches: Iterable[patch_specs.Patch],
) -> str:
    module = libcst.parse_module(code)
    transform = PatchTransform.from_patches(patches)
    module = transform.transform_module(module)
    return module.code
