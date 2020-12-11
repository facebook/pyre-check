# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import logging
from typing import Callable, Iterable

from .generator_specifications import (
    AnnotationSpecification,
    AllParametersAnnotation,
    DecoratorAnnotationSpecification,
)
from .get_annotated_free_functions_with_decorator import (
    AnnotatedFreeFunctionWithDecoratorGenerator,
)
from .model import FunctionDefinitionModel
from .model_generator import ModelGenerator


LOG: logging.Logger = logging.getLogger(__name__)


class FlaskModelGenerator(ModelGenerator[FunctionDefinitionModel]):
    def __init__(
        self,
        root: str,
    ) -> None:
        self.generator = AnnotatedFreeFunctionWithDecoratorGenerator(
            root=root,
            annotation_specifications=[
                DecoratorAnnotationSpecification(
                    decorator="@app.route()",
                    annotations=AnnotationSpecification(
                        parameter_annotation=AllParametersAnnotation(
                            arg="TaintSource[UserControlled, UserControlled_Parameter]"
                        ),
                        returns="TaintSink[ReturnedToUser]",
                    ),
                ),
                DecoratorAnnotationSpecification(
                    decorator="@app.errorhandler()",
                    annotations=AnnotationSpecification(
                        returns="TaintSink[ReturnedToUser]",
                    ),
                ),
            ],
        )

    def gather_functions_to_model(self) -> Iterable[Callable[..., object]]:
        return self.generator.gather_functions_to_model()

    def compute_models(
        self, functions_to_model: Iterable[Callable[..., object]]
    ) -> Iterable[FunctionDefinitionModel]:
        return self.generator.compute_models(functions_to_model)
