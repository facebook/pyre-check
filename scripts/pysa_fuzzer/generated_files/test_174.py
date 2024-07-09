import random
import math

a = input()
b = f'string {a}'
c = b + '.'
d = ''
for _ in range(3):
    for __ in range(4):
                d += c
e = f'string {d}'
f = e + '2'
g = f + '1'
h = g + '3'
if h == h:
    k = h + 'c1'
elif h == '18':
    k = i + 'c2'
else:
    k = j + 'c3'
l = ''
for _ in range(2):
    for __ in range(2):
                l += k
m_dict = {89: l, 16: l, 61: l, 93: l, 92: l, 98: l}
n_dict = {31: m_dict, 34: m_dict, 88: m_dict, 2: m_dict, 70: m_dict, 41: m_dict}
o_dict = {57: n_dict, 89: n_dict, 33: n_dict, 89: n_dict, 74: n_dict, 3: n_dict, 45: n_dict, 89: n_dict, 1: n_dict, 12: n_dict}
p = random.choice(list(o_dict.values()))
q = random.choice(list(p.values()))
r = random.choice(list(q.values()))
s = ''
for _ in range(3):
    for __ in range(2):
                s += r
t = s + '7'
u = t + '5'
v = u + '5'
if v == v:
    y = v + 'c1'
elif v == '12':
    y = w + 'c2'
else:
    y = x + 'c3'
z_dict = {99: y, 16: y, 36: y}
aa_dict = {38: z_dict, 62: z_dict, 15: z_dict, 64: z_dict, 9: z_dict, 78: z_dict}
ab = random.choice(list(aa_dict.values()))
ac = random.choice(list(ab.values()))
ad = f'string {ac}'
ae = ''
for _ in range(2):
    af = ''
    for _ in range(3):
        af += ae
        ae += ad
ag = af[0:]
ah = ''
for _ in range(7):
        if _ == 2:
            break
        ah += ag
ai = ah[0:]
aj = ''
counteraj = 0
while counteraj < 5:
    ak = ''
    counterak = 0
    while counterak < 4:
        ak += aj
        counterak += 1
        aj += ai
        counteraj += 1
print(ak)