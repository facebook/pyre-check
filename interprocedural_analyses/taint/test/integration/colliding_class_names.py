class C:
    def foo():
        return __test_source()


class C:
    def also_tainted_but_missing_from_analysis():
        return __test_source()
