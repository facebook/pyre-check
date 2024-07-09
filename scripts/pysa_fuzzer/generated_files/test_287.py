import random
import math

a = input()
b = ''
for _ in range(4):
    for __ in range(3):
                b += a
c = ''
for _ in range(2):
    for __ in range(5):
                c += b
d_dict = {9: c, 91: c, 41: c, 27: c, 6: c, 80: c, 21: c, 41: c, 22: c, 38: c}
e = random.choice(list(d_dict.values()))
f = ''
for _ in range(2):
    g = ''
    for _ in range(2):
        h = ''
        for _ in range(3):
            h += g
            g += f
        f += e
i = h + '.'
def j():
    return i
k = j()
l = (k, k, k)
m, n, o = l
p = m + n + o
q_set = {p, p, p, p, p, p, p, p, p}
q = random.choice(list(q_set))
r = ''
for _ in range(3):
    s = ''
    for _ in range(4):
        t = ''
        for _ in range(2):
            t += s
            s += r
        r += q
u = ''
for _ in range(5):
    v = ''
    for _ in range(3):
        v += u
        u += t
w = v + '7'
x = w + '3'
y = x + '1'
z = y + '3'
aa = z + '4'
def ab():
    return aa
def ac():
    return ab()
def ad():
    return ac()
ae = ad()
def af():
    return ae
def ag():
    return af()
def ah():
    return ag()
ai = ah()
aj = ''
counteraj = 0
while counteraj < 3:
    ak = ''
    counterak = 0
    while counterak < 2:
        ak += aj
        counterak += 1
        aj += ai
        counteraj += 1
al = ak + '6'
am = al + '5'
an_dict = {79: am, 22: am, 76: am, 61: am, 68: am, 60: am, 58: am, 92: am, 81: am, 14: am}
ao_dict = {75: an_dict, 65: an_dict, 20: an_dict, 19: an_dict, 4: an_dict, 21: an_dict, 95: an_dict, 55: an_dict, 8: an_dict, 2: an_dict}
ap = random.choice(list(ao_dict.values()))
aq = random.choice(list(ap.values()))
ar = ''
for _ in range(8):
        if _ == 1:
            continue
        ar += aq
print(ar)