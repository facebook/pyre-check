# @nolint


def rce_problem():
    x = __user_controlled()
    eval(x)
