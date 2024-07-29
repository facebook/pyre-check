from builtins import _test_sink, _test_source

class Node:
    def __init__(self, val=None):
        self.val = val
        self.next = None

def linked_list_pattern(x):
    b = Node(x)
    c = Node()
    d = Node()

    d.next = c 
    c.next = b
    e = d.next.next.val 

    _test_sink(e) # false negative 

    # Pysa treats memory as flat and doesn't understand aliases.
    # It fails to recognize that `d.next` -> `c` and `c.next` -> `b`, 
    # so it misses that `e` (i.e., `d.next.next.val`) is derived from `x`.
