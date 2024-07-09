import random
import math

a = input()
b = ''
for _ in range(2):
    c = ''
    for _ in range(4):
        c += b
        b += a
d = f'string {c}'
e_set = {d, d, d, d, d, d, d}
e = random.choice(list(e_set))
f = ''
counterf = 0
while counterf < 3:
    g = ''
    counterg = 0
    while counterg < 4:
        g += f
        counterg += 1
        f += e
        counterf += 1
h_list = [g for _ in range(4)]
i = random.choice(h_list)
def j():
    return i
k = j()
l = ''
for _ in range(2):
    l += k
m_set = {l, l, l, l, l, l, l, l, l, l}
m = random.choice(list(m_set))
n = ''
for _ in range(2):
    for __ in range(4):
                n += m
o = f'string {n}'
p = ''
for _ in range(5):
        if _ == 3:
            continue
        p += o
q = ''
for _ in range(4):
    for __ in range(4):
                q += p
r = q + '.'
def s():
    return r
def t():
    return s()
def u():
    return t()
v = u()
w = f'string {v}'
x = ''
counterx = 0
while counterx < 2:
    y = ''
    countery = 0
    while countery < 2:
        y += x
        countery += 1
        x += w
        counterx += 1
z = ''
for _ in range(4):
    aa = ''
    for _ in range(2):
        ab = ''
        for _ in range(3):
            ab += aa
            aa += z
        z += y
ac = ''
counterac = 0
while counterac < 5:
    ad = ''
    counterad = 0
    while counterad < 3:
        ad += ac
        counterad += 1
        ac += ab
        counterac += 1
print(ad)