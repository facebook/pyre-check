import random
import math
a = input()
def b():
    return a
def c():
    return b()
def d():
    return c()
e = d()
f = e + '.'
if f == '8':
    g = f + ' c1'
elif f == '17':
    g = f + ' c2'
else:
    g = f + ' c3'
h = ''
for _ in range(5):
        if _ == 1:
            continue
        h += g
i = h[0:]
if i == '1':
    j = i + ' c1'
elif i == '16':
    j = i + ' c2'
else:
    j = i + ' c3'
k = ''
for _ in range(7):
        if _ == 5:
            continue
        k += j
def l():
    return k
m = l()
if m == '5':
    n = m + ' c1'
elif m == '14':
    n = m + ' c2'
else:
    n = m + ' c3'
o = [n for _ in range(9)]
random.shuffle(o)
p = random.choice(o)
q = ''
for _ in range(4):
    q += p
r = ''
for _ in range(3):
    for __ in range(3):
                r += q
def s():
    return r
def t():
    return s()
def u():
    return t()
v = u()
w = v + '.'
x = ''
for _ in range(3):
    y = ''
    for _ in range(4):
        y += x
        x += w
z = ''
for _ in range(2):
    for __ in range(4):
                z += y
def aa():
    return z
def ab():
    return aa()
def ac():
    return ab()
ad = ac()
ae = ''
for _ in range(4):
    af = ''
    for _ in range(3):
        ag = ''
        for _ in range(2):
            ag += af
            af += ae
        ae += ad
print(ag)