import random
import math
a = input()
b = ''
counterb = 0
while counterb < 2:
    c = ''
    counterc = 0
    while counterc < 5:
        d = ''
        counterd = 0
        while counterd < 3:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e = d + '9'
f = ''
counterf = 0
while counterf < 3:
    g = ''
    counterg = 0
    while counterg < 5:
        h = ''
        counterh = 0
        while counterh < 2:
            h += g
            counterh += 1
            g += f
            counterg += 1
        f += e
        counterf += 1
i = [h for _ in range(6)]
random.shuffle(i)
j = random.choice(i)
k = ''
for _ in range(4):
    for __ in range(5):
                k += j
l = k + '5'
m = f'string {l}'
if m == '10':
    n = m + ' c1'
elif m == '17':
    n = m + ' c2'
else:
    n = m + ' c3'
o_dict = {17: n, 53: n, 84: n, 80: n, 53: n, 88: n, 31: n, 29: n, 6: n, 97: n}
p_dict = {55: o_dict, 77: o_dict, 89: o_dict, 71: o_dict, 18: o_dict, 93: o_dict, 61: o_dict, 63: o_dict}
q = random.choice(list(p_dict.values()))
r = random.choice(list(q.values()))
s = ''
for _ in range(3):
    s += r
t = ''
countert = 0
while countert < 2:
    u = ''
    counteru = 0
    while counteru < 3:
        u += t
        counteru += 1
        t += s
        countert += 1
v = f'string {u}'
if v == '8':
    w = v + ' c1'
elif v == '20':
    w = v + ' c2'
else:
    w = v + ' c3'
x = [w for _ in range(10)]
random.shuffle(x)
y = random.choice(x)
z = y + '9'
def aa():
    return z
def ab():
    return aa()
ac = ab()
ad = [ac for _ in range(6)]
random.shuffle(ad)
ae = random.choice(ad)
af = f'string {ae}'
print(af)