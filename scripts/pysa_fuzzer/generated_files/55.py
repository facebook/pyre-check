import random
import math
a = input()
b = ''
for _ in range(5):
    c = ''
    for _ in range(3):
        c += b
        b += a
d_dict = {66: c, 98: c, 77: c}
e_dict = {75: d_dict, 43: d_dict, 59: d_dict, 21: d_dict, 45: d_dict, 92: d_dict, 15: d_dict}
f_dict = {65: e_dict, 97: e_dict, 15: e_dict}
g = random.choice(list(f_dict.values()))
h = random.choice(list(g.values()))
i = random.choice(list(h.values()))
j = ''
for _ in range(4):
    for __ in range(2):
                j += i
k = ''
for _ in range(5):
        if _ == 2:
            continue
        k += j
l = ''
for _ in range(5):
        if _ == 4:
            continue
        l += k
m_list = [l for _ in range(3)]
n_list = [m_list for _ in range(5)]
o_list = [n_list for _ in range(8)]
p = random.choice(o_list)
q = random.choice(p)
r = random.choice(q)
s = ''
for _ in range(9):
        if _ == 3:
            continue
        s += r
t = (s, s, s)
u, v, w = t
x = u + v + w
y = x + '.'
z = ''
for _ in range(5):
    for __ in range(5):
                z += y
if z == '10':
    aa = z + ' c1'
elif z == '14':
    aa = z + ' c2'
else:
    aa = z + ' c3'
ab_dict = {87: aa, 62: aa, 40: aa, 23: aa, 99: aa}
ac = random.choice(list(ab_dict.values()))
ad_dict = {49: ac, 63: ac, 88: ac, 25: ac, 95: ac, 83: ac, 93: ac, 95: ac, 74: ac}
ae_dict = {32: ad_dict, 25: ad_dict, 3: ad_dict}
af_dict = {23: ae_dict, 60: ae_dict, 99: ae_dict, 60: ae_dict, 36: ae_dict, 50: ae_dict, 64: ae_dict, 39: ae_dict}
ag = random.choice(list(af_dict.values()))
ah = random.choice(list(ag.values()))
ai = random.choice(list(ah.values()))
aj_list = [ai for _ in range(5)]
ak = random.choice(aj_list)
al = ak + '3'
am = al + '7'
an = am + '7'
def ao():
    return an
ap = ao()
if ap == '1':
    aq = ap + ' c1'
elif ap == '17':
    aq = ap + ' c2'
else:
    aq = ap + ' c3'
ar = ''
for _ in range(3):
    for __ in range(5):
                ar += aq
print(ar)