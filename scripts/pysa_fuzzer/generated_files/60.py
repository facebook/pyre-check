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
for _ in range(4):
    for __ in range(4):
                e += d
f = e + '9'
g = f + '8'
h = ''
for _ in range(3):
    h += g
i = ''
for _ in range(4):
    for __ in range(3):
                i += h
j = i[0:]
k = ''
counterk = 0
while counterk < 5:
    l = ''
    counterl = 0
    while counterl < 5:
        l += k
        counterl += 1
        k += j
        counterk += 1
m_list = [l for _ in range(7)]
n_list = [m_list for _ in range(9)]
o_list = [n_list for _ in range(8)]
p = random.choice(o_list)
q = random.choice(p)
r = random.choice(q)
s = ''
for _ in range(4):
    t = ''
    for _ in range(2):
        t += s
        s += r
u = ''
for _ in range(5):
    for __ in range(5):
                u += t
v = ''
for _ in range(3):
    for __ in range(2):
                v += u
w = (v, v, v)
x, y, z = w
aa = x + y + z
def ab():
    return aa
def ac():
    return ab()
ad = ac()
ae = ''
for _ in range(3):
    ae += ad
if ae == '4':
    af = ae + ' c1'
elif ae == '18':
    af = ae + ' c2'
else:
    af = ae + ' c3'
ag = af + '.'
ah = ''
for _ in range(4):
    ah += ag
ai = ''
for _ in range(5):
    aj = ''
    for _ in range(4):
        ak = ''
        for _ in range(5):
            ak += aj
            aj += ai
        ai += ah
print(ak)