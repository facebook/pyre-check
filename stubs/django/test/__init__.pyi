# pyre-unsafe

from unittest import TestCase as TestCase

from django.test.client import Client as Client
from django.test.utils import override_settings as override_settings

class SimpleTestCase(TestCase):
    client: testing.client.base.TestClient = ...
    @contextlib.contextmanager
    def settings(self, **kwargs) -> None: ...
