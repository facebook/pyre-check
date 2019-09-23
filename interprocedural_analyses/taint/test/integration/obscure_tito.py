# flake8: noqaes


class C:
    def update(self, parameter):
        ...

    def taint_parameter(self, tainted_parameter):
        ...


class D(C):
    def update(self, parameter):
        ...

    def taint_parameter(self, tainted_parameter):
        ...


def test_obscure_tito():
    c = C()
    c.update(__test_source())
    return c


def test_obscure_return():
    return c.update(__test_source())


def test_obscure_sink(parameter):
    c = C()
    c.taint_parameter(parameter)
