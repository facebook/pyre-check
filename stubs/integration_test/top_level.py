# @nolint

import pyre
from django.http import HttpRequest


# Integration test illustrating that we check top-level functions.

request: HttpRequest = ...
eval(request.GET["bad"])
