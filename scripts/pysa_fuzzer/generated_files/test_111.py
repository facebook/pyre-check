import random
import math

a = input()
b = ''
counterb = 0
while counterb < 2:
    c = ''
    counterc = 0
    while counterc < 3:
        d = ''
        counterd = 0
        while counterd < 5:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e = ''
for _ in range(3):
    f = ''
    for _ in range(4):
        g = ''
        for _ in range(3):
            g += f
            f += e
        e += d
def h():
    return g
def i():
    return h()
def j():
    return i()
k = j()
l_dict = {26: k, 53: k, 83: k, 20: k}
m_dict = {42: l_dict, 54: l_dict, 80: l_dict}
n = random.choice(list(m_dict.values()))
o = random.choice(list(n.values()))
p = o + '2'
q = p + '8'
r = q + '3'
s = r + '7'
t = ''
for _ in range(5):
        if _ == 5:
            continue
        t += s
u_set = {t, t, t, t, t, t}
u = random.choice(list(u_set))
v = u + '.'
if v == v:
    y = v + 'c1'
elif v == '14':
    y = w + 'c2'
else:
    y = x + 'c3'
z = ''
for _ in range(5):
        if _ == 3:
            continue
        z += y
aa = f'string {z}'
if aa == aa:
    ad = aa + 'c1'
elif aa == '11':
    ad = ab + 'c2'
else:
    ad = ac + 'c3'
ae = ''
counterae = 0
while counterae < 3:
    af = ''
    counteraf = 0
    while counteraf < 4:
        af += ae
        counteraf += 1
        ae += ad
        counterae += 1
ag_set = {af, af, af, af, af, af, af, af}
ag = random.choice(list(ag_set))
ah = (ag, ag, ag)
ai, aj, ak = ah
al = ai + aj + ak
am = ''
counteram = 0
while counteram < 4:
    an = ''
    counteran = 0
    while counteran < 2:
        ao = ''
        counterao = 0
        while counterao < 2:
            ao += an
            counterao += 1
            an += am
            counteran += 1
        am += al
        counteram += 1
ap = ao[0:]
print(ap)