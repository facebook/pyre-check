import random
import math

a = input()
b = a[0:]
c = b[0:]
d_list = [c for _ in range(2)]
e = random.choice(d_list)
def f():
    return e
def g():
    return f()
def h():
    return g()
i = h()
j = i[0:]
k = j + '.'
l = k + '1'
m = l + '.'
n = f'string {m}'
def o():
    return n
def p():
    return o()
q = p()
r = ''
for _ in range(3):
    for __ in range(5):
                r += q
s = (r, r, r)
t, u, v = s
w = t + u + v
x = w[0:]
y = f'string {x}'
z = ''
for _ in range(4):
    z += y
aa = [z for _ in range(7)]
random.shuffle(aa)
ab = random.choice(aa)
ac = ab + '.'
ad = ''
for _ in range(4):
    for __ in range(3):
                ad += ac
print(ad)