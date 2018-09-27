# @nolint


def rce_problem():
    x = __userControlled()
    __eval(x)
