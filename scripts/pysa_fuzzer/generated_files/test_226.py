import random
import math

a = input()
b = [a for _ in range(5)]
random.shuffle(b)
c = random.choice(b)
d_set = {c, c, c, c, c, c, c}
d = random.choice(list(d_set))
def e():
    return d
f = e()
def g():
    return f
def h():
    return g()
i = h()
j = i + '.'
k = [j for _ in range(8)]
random.shuffle(k)
l = random.choice(k)
m = ''
for _ in range(5):
        if _ == 5:
            break
        m += l
n = ''
for _ in range(2):
    for __ in range(4):
                n += m
o = [n for _ in range(7)]
random.shuffle(o)
p = random.choice(o)
q = p + '6'
r = q + '1'
s = r[0:]
t = s + '.'
u = ''
for _ in range(2):
    for __ in range(2):
                u += t
v = u + '.'
w = f'string {v}'
x = w + '.'
def y():
    return x
def z():
    return y()
def aa():
    return z()
ab = aa()
ac = [ab for _ in range(5)]
random.shuffle(ac)
ad = random.choice(ac)
print(ad)