# @nolint

class Base:
    q: str = 'q'
    r: str = 'r'

    def __init__(self):
        pass

    def methodA(self, arg):
        pass

    def methodB(self):
        pass

    @classmethod
    def classMethod(cls, arg):
        pass


class A(Base):
    q: str = 'q'

    def __init__(self):
        super(Base, self).__init__()

    def methodA(self, arg):
        __testSink(arg)


class B(Base):
    r: str = 'r'

    def __init__(self):
        super(Base, self).__init__()

    def methodB(self):
        return __testSource()

    @classmethod
    def classMethod(cls, arg):
        __testSink(arg)


class C(B):
    q: str = 'q'

    def __init__(self):
        super(B, self).__init__()

    def methodA(self, arg):
        pass

    @classmethod
    def classMethod(cls, arg):
        pass


class D(C):

    def __init__(self):
        super(C, self).__init__()

    def methodA(self, arg):
        __testSink(arg)

    def methodB(self):
        return __testSource();


def testBase(o: Base, cls: typing.Type[Base]):
    y = o.methodB()
    o.methodA(y)
    cls.classMethod(y)


def testStaticBase(o: Base):
    x = o.methodB()
    Base.classMethod(x)


def testMakeBase():
    o = Base()
    x = o.methodB()


def testA(o: A, cls: typing.Type[A]):
    y = o.methodB()
    o.methodA(y)
    cls.classMethod(y)


def testStaticA(o: A):
    x = o.methodB()
    A.classMethod(x)


def testMakeA():
    o = A()
    x = o.methodB()


def testB(o: B, cls: typing.Type[B]):
    y = o.methodB()
    o.methodA(y)
    cls.classMethod(y)


def testStaticB(o: B):
    x = o.methodB()
    B.classMethod(x)


def testMakeB():
    o = B()
    x = o.methodB()


def testC(o: C, cls: typing.Type[C]):
    y = o.methodB()
    o.methodA(y)
    cls.classMethod(y)


def testStaticC(o: C):
    x = o.methodB()
    C.classMethod(x)


def testMakeC():
    o = C()
    x = o.methodB()


def testD(o: D, cls: typing.Type[D]):
    y = o.methodB()
    o.methodA(y)
    cls.classMethod(y)


def testStaticD(o: D):
    x = o.methodB()
    D.classMethod(x)


def testMakeD():
    o = D()
    x = o.methodB()
