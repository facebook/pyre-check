# @nolint

class Base:
    q: str = 'q'
    r: str = 'r'

    def __init__(self):
        pass

    def methodA(self):
        pass

    def methodB(self):
        pass


class A(Base):
    q: str = 'q'

    def __init__(self):
        super(Base, self).__init__()

    def methodA(self):
        pass


class B(Base):
    r: str = 'r'

    def __init__(self):
        super(Base, self).__init__()

    def methodB(self):
        pass


class C(B):
    q: str = 'q'

    def __init__(self):
        super(B, self).__init__()

    def methodA(self):
        pass


def testBase(o: Base):
    x = o.methodA()
    y = o.methodB()


def testA(o: A):
    x = o.methodA()
    y = o.methodB()


def testB(o: B):
    x = o.methodA()
    y = o.methodB()


def testC(o: C):
    x = o.methodA()
    y = o.methodB()
