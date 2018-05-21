from django.test.utils import override_settings

class SimpleTestCase:
    client: testing.client.base.TestClient = ...
