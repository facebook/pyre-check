# @nolint


def rce_problem():
    x = __userControlled()
    eval(x)


def subprocess_problem():
    x = __userControlled()
    subprocess.check_call(x, shell=True)
