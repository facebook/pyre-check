# pyre-unsafe

from unittest import TestCase

from django.test.utils import override_settings

class SimpleTestCase(TestCase):
    client: testing.client.base.TestClient = ...
    @contextlib.contextmanager
    def settings(self, **kwargs) -> None: ...
