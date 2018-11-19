# @nolint


def rce_problem():
    x = __userControlled()
    __eval(x)


def subprocess_problem():
    x = __userControlled()
    subprocess.check_call(x, shell=True)
