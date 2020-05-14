from builtins import __test_sink, __test_source
from typing import Optional


class Client:
    def offer(self, message):
        __test_sink(message)


class ClientSingleton:
    def get_instance(self) -> Optional[Client]:
        return Client()


client: ClientSingleton = ClientSingleton()


def test():
    client.get_instance().offer(__test_source())
