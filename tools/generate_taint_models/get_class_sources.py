# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict


from .constructor_generator import ConstructorGenerator
from .function_tainter import FunctionTainter
from .model_generator import Registry


class ClassSourceGenerator(FunctionTainter, ConstructorGenerator):
    """
    This Generator uses Configuration.classes_to_taint to taint the __init__
    functions of the classes passed as fully qualified strings. All recursive
    subclasses that have had their modules loaded at preprocessing time will
    also be tainted. The purpose of using this flag would be if it is not
    possible for the type system to assess full inheritance statically
    (ex: dynamic subclassing).
    """

    parameter_name_whitelist = {"self"}

    pass


Registry.register("get_class_sources", ClassSourceGenerator, include_by_default=True)
