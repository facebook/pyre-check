def f1():
    return input()
a = f1()
def f2():
    return a
def f3():
    return f2()
b = f3()
def f4():
    return b
c = f4()
d = c
e = d
f = e
g = f
h = g
def f5():
    return h
def f6():
    return f5()
def f7():
    return f6()
def f8():
    return f7()
def f9():
    return f8()
def f10():
    return f9()
def f11():
    return f10()
def f12():
    return f11()
i = f12()
j = i
k = j
l = k
print(l)