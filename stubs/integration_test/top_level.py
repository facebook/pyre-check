# @nolint

from django.http import HttpRequest
import pyre

# Integration test illustrating that we check top-level functions.

request: HttpRequest = ...
eval(request.GET['bad'])
