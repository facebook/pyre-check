a = input()
class Node:
    def __init__(self, val=None):
        self.val = val
        self.next = None

b = Node(a)
c = Node()
d = Node()
d.next = c
c.next = b
e = d.next.next.val
print(e)