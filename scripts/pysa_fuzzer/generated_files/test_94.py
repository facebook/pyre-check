import random
import math

a = input()
b = ''
for _ in range(2):
    c = ''
    for _ in range(3):
        c += b
        b += a
d = ''
for _ in range(5):
    for __ in range(4):
                d += c
def e():
    return d
def f():
    return e()
g = f()
h = ''
for _ in range(3):
    for __ in range(3):
                h += g
i = ''
for _ in range(4):
    j = ''
    for _ in range(5):
        j += i
        i += h
k = j + '.'
l_set = {k, k, k, k, k, k}
l = random.choice(list(l_set))
m = ''
for _ in range(5):
    for __ in range(5):
                m += l
def n():
    return m
def o():
    return n()
p = o()
q = (p, p, p)
r, s, t = q
u = r + s + t
v = f'string {u}'
w = ''
counterw = 0
while counterw < 5:
    w += v
    counterw += 1
x_dict = {71: w, 70: w, 66: w, 57: w, 74: w, 84: w, 41: w}
y = random.choice(list(x_dict.values()))
z = ''
for _ in range(6):
        if _ == 4:
            break
        z += y
aa = z[0:]
ab = ''
counterab = 0
while counterab < 3:
    ac = ''
    counterac = 0
    while counterac < 2:
        ad = ''
        counterad = 0
        while counterad < 5:
            ad += ac
            counterad += 1
            ac += ab
            counterac += 1
        ab += aa
        counterab += 1
ae = ''
for _ in range(10):
        if _ == 3:
            continue
        ae += ad
af_dict = {80: ae, 38: ae, 60: ae, 55: ae, 78: ae}
ag = random.choice(list(af_dict.values()))
print(ag)