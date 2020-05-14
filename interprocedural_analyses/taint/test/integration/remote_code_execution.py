from builtins import __user_controlled


def rce_problem():
    x = __user_controlled()
    eval(x)
