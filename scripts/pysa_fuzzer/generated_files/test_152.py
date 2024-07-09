import random
import math

a = input()
b = ''
for _ in range(2):
    c = ''
    for _ in range(5):
        d = ''
        for _ in range(2):
            d += c
            c += b
        b += a
e = ''
countere = 0
while countere < 2:
    e += d
    countere += 1
f = ''
for _ in range(4):
    g = ''
    for _ in range(5):
        h = ''
        for _ in range(5):
            h += g
            g += f
        f += e
def i():
    return h
def j():
    return i()
k = j()
l_set = {k, k, k, k, k, k, k}
l = random.choice(list(l_set))
m = ''
for _ in range(5):
    m += l
n = [m for _ in range(5)]
random.shuffle(n)
o = random.choice(n)
p = ''
for _ in range(3):
    q = ''
    for _ in range(5):
        q += p
        p += o
r = ''
for _ in range(5):
    r += q
s = [r for _ in range(10)]
random.shuffle(s)
t = random.choice(s)
u = ''
for _ in range(8):
        if _ == 1:
            break
        u += t
v = (u, u, u)
w, x, y = v
z = w + x + y
aa_dict = {81: z, 37: z, 45: z, 44: z, 72: z, 95: z, 98: z, 77: z, 88: z}
ab = random.choice(list(aa_dict.values()))
ac = ab[0:]
ad = [ac for _ in range(7)]
random.shuffle(ad)
ae = random.choice(ad)
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak = (aj, aj, aj)
al, am, an = ak
ao = al + am + an
ap = ao + '8'
aq = ap + '4'
ar = aq + '7'
print(ar)