# @nolint


class Class:
    def __init__():
        self.tainted = ""
        self.untainted = ""

    @property
    def my_property(self) -> str:
        return self.tainted

    def uses_property(self):
        self.tainted = __test_source()
        return self.my_property

    def uses_property_but_no_tito_taint(self):
        self.untainted = __test_source()
        return self.my_property


class Derived(Class):
    def uses_property(self):
        self.tainted = __test_source()
        return self.my_property

    def uses_property_but_no_tito_taint(self):
        self.untainted = __test_source()
        return self.my_property


class OtherDerived(Class):
    @property
    def my_property(self) -> str:
        return __test_source()

    def uses_property_but_no_tito_taint(self):
        self.untainted = __test_source()
        return self.my_property
