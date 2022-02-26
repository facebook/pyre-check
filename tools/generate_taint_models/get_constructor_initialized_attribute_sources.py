# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


import logging
from typing import Callable, Iterable, List, Optional, Type, TypeVar

from ...api import query
from ...api.connection import PyreConnection
from .constructor_generator import gather_all_constructors_in_hierarchy
from .inspect_parser import extract_parameters, extract_qualified_name
from .model import AssignmentModel
from .model_generator import ModelGenerator


LOG: logging.Logger = logging.getLogger(__name__)
T = TypeVar("T")
BATCH_SIZE = 200


class ConstructorInitializedAttributeSourceGenerator(ModelGenerator[AssignmentModel]):
    """
    This Generator will taint the attributes initialized by the constructors of
    'classes_to_taint' and their descendants. Only descendants that have had
    their modules loaded at preprocessing time will be tainted. Models are
    generated on a best effort basis by assuming the name of the parameter will
    match the name of the attribute it is assigned to. This naive approach means
    this model generator will likely generate some invalid models.
    """

    def __init__(
        self,
        classes_to_taint: List[str],
        pyre_connection: PyreConnection,
        filter_classes_by: Optional[Callable[[Type[T]], bool]] = None,
        taint_annotation: str = "TaintSource[UserControlled]",
    ) -> None:
        self.classes_to_taint: List[str] = classes_to_taint
        self.pyre_connection = pyre_connection
        self.filter_classes_by = filter_classes_by
        self.taint_annotation: str = taint_annotation

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        return gather_all_constructors_in_hierarchy(
            self.classes_to_taint, self.filter_classes_by
        )

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[AssignmentModel]:
        constructors = {}
        for constructor in functions_to_model:
            qualified_name = extract_qualified_name(constructor)
            if not qualified_name:
                continue

            # Strip off __init__ and append the parameter name as an attribute
            # name.
            class_name = ".".join(qualified_name.split(".")[:-1])
            constructors[class_name] = constructor

        attributes_map = query.get_attributes(
            self.pyre_connection, constructors.keys(), BATCH_SIZE
        )

        for class_name, constructor in constructors.items():
            attributes = {attribute.name for attribute in attributes_map[class_name]}

            parameters = extract_parameters(constructor)
            for parameter in parameters:
                # Skip 'self', and attributes that are callables
                if parameter.name == "self" or (
                    "Callable[" in (parameter.annotation or "")
                    or "Coroutine[" in (parameter.annotation or "")
                ):
                    continue

                if parameter.name in attributes:
                    # If a parameter is a valid attribute, add a taint model.
                    target = f"{class_name}.{parameter.name}"
                    yield AssignmentModel(
                        target=target, annotation=self.taint_annotation
                    )

                if "_" + parameter.name in attributes:
                    # Same as above, but parameters might be prefixed with an
                    # underscore to indicate a private attribute.
                    target = f"{class_name}._{parameter.name}"
                    yield AssignmentModel(
                        target=target, annotation=self.taint_annotation
                    )
