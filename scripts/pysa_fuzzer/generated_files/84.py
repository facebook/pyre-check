import random
import math
a = input()
b = a + '.'
c_set = {b, b, b, b}
c = random.choice(list(c_set))
d = ''
for _ in range(4):
    for __ in range(4):
                d += c
e = d + '.'
f = ''
for _ in range(4):
    g = ''
    for _ in range(3):
        h = ''
        for _ in range(5):
            h += g
            g += f
        f += e
i = ''
for _ in range(4):
    i += h
if i == '1':
    j = i + ' c1'
elif i == '11':
    j = i + ' c2'
else:
    j = i + ' c3'
k = [j for _ in range(7)]
random.shuffle(k)
l = random.choice(k)
m = f'string {l}'
n = m[0:]
o = n[0:]
p = ''
for _ in range(10):
        if _ == 5:
            continue
        p += o
if p == '1':
    q = p + ' c1'
elif p == '13':
    q = p + ' c2'
else:
    q = p + ' c3'
r_set = {q, q, q, q, q, q, q, q, q}
r = random.choice(list(r_set))
s_set = {r, r, r, r}
s = random.choice(list(s_set))
t = ''
for _ in range(5):
    for __ in range(2):
                t += s
u = ''
for _ in range(5):
    u += t
def v():
    return u
def w():
    return v()
def x():
    return w()
y = x()
print(y)