class Node:
    def __init__(self, id) -> None:
        self.id = id

    def send(self, vc) -> None:
        ...

    @classmethod
    def get(cls, id) -> "Node":
        return cls(id)


def user_controlled_input():
    return "evil"


def permissive_context():
    return 0


def combine_tainted_user_and_dangerous_vc():
    id = user_controlled_input()
    vc = permissive_context()
    Node.get(id).send(vc)
