import random
import math

a = input()
b = a + '.'
c = f'string {b}'
d = ''
for _ in range(3):
    for __ in range(3):
                d += c
e = (d, d, d)
f, g, h = e
i = f + g + h
def j():
    return i
def k():
    return j()
def l():
    return k()
m = l()
n = ''
for _ in range(8):
        if _ == 4:
            continue
        n += m
o = ''
for _ in range(10):
        if _ == 5:
            break
        o += n
p = ''
for _ in range(7):
        if _ == 3:
            break
        p += o
q = ''
for _ in range(5):
    r = ''
    for _ in range(5):
        s = ''
        for _ in range(5):
            s += r
            r += q
        q += p
t_set = {s, s, s, s, s, s}
t = random.choice(list(t_set))
u_set = {t, t, t, t, t, t, t, t, t, t}
u = random.choice(list(u_set))
v = ''
for _ in range(9):
        if _ == 3:
            continue
        v += u
w = ''
for _ in range(8):
        if _ == 3:
            continue
        w += v
x = w + '.'
y = ''
for _ in range(8):
        if _ == 1:
            continue
        y += x
z = y + '.'
aa_set = {z, z, z, z, z, z, z}
aa = random.choice(list(aa_set))
ab = aa[0:]
print(ab)