import random
import math

a = input()
b_set = {a, a, a, a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c = ''
for _ in range(2):
    for __ in range(5):
                c += b
d_dict = {33: c, 69: c, 35: c, 47: c, 9: c, 74: c}
e = random.choice(list(d_dict.values()))
f_set = {e, e}
f = random.choice(list(f_set))
g = ''
for _ in range(5):
    h = ''
    for _ in range(5):
        h += g
        g += f
i = ''
for _ in range(8):
        if _ == 5:
            continue
        i += h
j = ''
for _ in range(2):
    for __ in range(2):
                j += i
k = j[0:]
l_set = {k, k, k, k, k, k}
l = random.choice(list(l_set))
m = f'string {l}'
n = (m, m, m)
o, p, q = n
r = o + p + q
s = ''
for _ in range(3):
    for __ in range(2):
                s += r
t = [s for _ in range(9)]
random.shuffle(t)
u = random.choice(t)
v = ''
for _ in range(4):
    v += u
w = v + '6'
x = w + '.'
y = ''
for _ in range(2):
    z = ''
    for _ in range(2):
        aa = ''
        for _ in range(2):
            aa += z
            z += y
        y += x
ab = f'string {aa}'
print(ab)