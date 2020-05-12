# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree


from typing import IO, Any

import libcst as cst
from libcst.codemod import CodemodContext
from libcst.codemod.visitors._apply_type_annotations import ApplyTypeAnnotationsVisitor


def _parse(file: IO[Any]) -> cst.Module:  # pyre-fixme[2]
    contents = file.read()
    return cst.parse_module(contents)


def _annotate_source(stub: cst.Module, source: cst.Module) -> cst.Module:
    context = CodemodContext()
    ApplyTypeAnnotationsVisitor.add_stub_to_context(context, stub)
    return ApplyTypeAnnotationsVisitor(context).transform_module(source)


def apply_stub_annotations(stub_path: str, file_path: str) -> str:
    with open(stub_path) as stub_file, open(file_path) as source_file:
        stub = _parse(stub_file)
        source = _parse(source_file)
        modified_tree = _annotate_source(stub, source)
        return modified_tree.code
