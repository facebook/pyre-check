import random
import math

a = input()
b = ''
for _ in range(4):
    for __ in range(2):
                b += a
c = ''
counterc = 0
while counterc < 3:
    d = ''
    counterd = 0
    while counterd < 3:
        e = ''
        countere = 0
        while countere < 5:
            e += d
            countere += 1
            d += c
            counterd += 1
        c += b
        counterc += 1
f = ''
for _ in range(6):
        if _ == 5:
            continue
        f += e
def g():
    return f
h = g()
i = (h, h, h)
j, k, l = i
m = j + k + l
n = m + '.'
o_list = [n for _ in range(2)]
p_list = [o_list for _ in range(5)]
q_list = [p_list for _ in range(7)]
r = random.choice(q_list)
s = random.choice(r)
t = random.choice(s)
u = ''
for _ in range(4):
    v = ''
    for _ in range(2):
        v += u
        u += t
w = f'string {v}'
x = (w, w, w)
y, z, aa = x
ab = y + z + aa
ac = f'string {ab}'
ad = ac[0:]
ae = ''
for _ in range(2):
    af = ''
    for _ in range(5):
        ag = ''
        for _ in range(2):
            ag += af
            af += ae
        ae += ad
ah = (ag, ag, ag)
ai, aj, ak = ah
al = ai + aj + ak
am_set = {al, al, al, al, al, al, al, al, al}
am = random.choice(list(am_set))
an_dict = {30: am, 67: am, 28: am}
ao = random.choice(list(an_dict.values()))
ap = ''
for _ in range(4):
    ap += ao
def aq():
    return ap
def ar():
    return aq()
def at():
    return ar()
au = at()
print(au)