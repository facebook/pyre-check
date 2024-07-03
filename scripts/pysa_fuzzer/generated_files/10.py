import random
import math
a = input()
b = ''
counterb = 0
while counterb < 3:
    c = ''
    counterc = 0
    while counterc < 5:
        c += b
        counterc += 1
        b += a
        counterb += 1
d = ''
for _ in range(6):
        if _ == 2:
            break
        d += c
e = d + '.'
def f():
    return e
def g():
    return f()
def h():
    return g()
i = h()
j = ''
for _ in range(4):
    j += i
k = f'string {j}'
if k == '4':
    l = k + ' c1'
elif k == '12':
    l = k + ' c2'
else:
    l = k + ' c3'
def m():
    return l
n = m()
o = ''
for _ in range(2):
    for __ in range(4):
                o += n
p = ''
for _ in range(9):
        if _ == 5:
            break
        p += o
q = ''
for _ in range(4):
    q += p
def r():
    return q
def s():
    return r()
t = s()
u = (t, t, t)
v, w, x = u
y = v + w + x
z = ''
for _ in range(9):
        if _ == 2:
            continue
        z += y
aa = ''
for _ in range(5):
        if _ == 2:
            break
        aa += z
ab = f'string {aa}'
ac = ab[0:]
ad = ''
for _ in range(5):
    for __ in range(3):
                ad += ac
print(ad)