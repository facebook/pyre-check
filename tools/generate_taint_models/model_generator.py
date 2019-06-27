# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

from abc import ABC, abstractmethod
from typing import Callable, Iterable


class ModelGenerator(ABC):
    WHITELISTED_VIEWS = [
        "django.views.generic.base.TemplateView",
        "django.views.static.serve",
        "django.views.generic.base.RedirectView",
    ]

    @abstractmethod
    def compute_models(self, visit_all_views: Callable[..., None]) -> Iterable[str]:
        pass
