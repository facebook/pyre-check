# @nolint


def rce_problem():
    x = __user_controlled()
    eval(x)


def subprocess_problem():
    x = __user_controlled()
    subprocess.check_call(x, shell=True)
