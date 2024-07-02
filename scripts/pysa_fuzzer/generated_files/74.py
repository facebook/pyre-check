import random
import math
a = input()
b = ''
for _ in range(5):
    c = ''
    for _ in range(3):
        d = ''
        for _ in range(2):
            d += c
            c += b
        b += a
e = ''
for _ in range(4):
    f = ''
    for _ in range(5):
        g = ''
        for _ in range(5):
            g += f
            f += e
        e += d
h = g[0:]
i = ''
for _ in range(7):
        if _ == 1:
            continue
        i += h
if i == '9':
    j = i + ' c1'
elif i == '15':
    j = i + ' c2'
else:
    j = i + ' c3'
k = j + '5'
l = k + '4'
m_set = {l, l, l, l, l, l, l, l, l}
m = random.choice(list(m_set))
n_dict = {94: m, 92: m, 56: m}
o_dict = {5: n_dict, 49: n_dict, 44: n_dict, 9: n_dict}
p = random.choice(list(o_dict.values()))
q = random.choice(list(p.values()))
r = ''
for _ in range(5):
    for __ in range(2):
                r += q
s = (r, r, r)
t, u, v = s
w = t + u + v
x = w + '.'
y = f'string {x}'
z = f'string {y}'
aa = ''
for _ in range(9):
        if _ == 3:
            break
        aa += z
ab = aa[0:]
ac = ''
for _ in range(5):
        if _ == 2:
            continue
        ac += ab
def ad():
    return ac
def ae():
    return ad()
af = ae()
print(af)