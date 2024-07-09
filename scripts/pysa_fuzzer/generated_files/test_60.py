import random
import math

a = input()
b = ''
for _ in range(3):
    c = ''
    for _ in range(3):
        d = ''
        for _ in range(4):
            d += c
            c += b
        b += a
e = (d, d, d)
f, g, h = e
i = f + g + h
j_set = {i, i, i, i, i, i, i}
j = random.choice(list(j_set))
k_set = {j, j, j, j, j, j, j, j, j, j}
k = random.choice(list(k_set))
l = ''
for _ in range(10):
        if _ == 4:
            continue
        l += k
def m():
    return l
def n():
    return m()
o = n()
p = ''
for _ in range(3):
    q = ''
    for _ in range(2):
        r = ''
        for _ in range(5):
            r += q
            q += p
        p += o
s = f'string {r}'
def t():
    return s
u = t()
v = ''
for _ in range(7):
        if _ == 5:
            continue
        v += u
w_dict = {48: v, 44: v, 51: v, 20: v}
x = random.choice(list(w_dict.values()))
y = f'string {x}'
z_set = {y, y, y, y, y, y}
z = random.choice(list(z_set))
aa = z[0:]
ab = [aa for _ in range(7)]
random.shuffle(ab)
ac = random.choice(ab)
ad = ac + '1'
ae = ad + '5'
af = ae + '8'
ag = af + '.'
ah = [ag for _ in range(9)]
random.shuffle(ah)
ai = random.choice(ah)
print(ai)