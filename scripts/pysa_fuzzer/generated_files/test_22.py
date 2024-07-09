import random
import math

a = input()
b_list = [a for _ in range(10)]
c = random.choice(b_list)
d = c + '1'
e = d + '8'
f = e + '3'
g = (f, f, f)
h, i, j = g
k = h + i + j
l_dict = {29: k, 63: k, 81: k, 81: k, 47: k, 100: k, 85: k, 15: k}
m_dict = {95: l_dict, 23: l_dict}
n_dict = {5: m_dict, 33: m_dict, 12: m_dict, 54: m_dict}
o = random.choice(list(n_dict.values()))
p = random.choice(list(o.values()))
q = random.choice(list(p.values()))
r = [q for _ in range(8)]
random.shuffle(r)
s = random.choice(r)
t = (s, s, s)
u, v, w = t
x = u + v + w
y = x + '6'
z = y + '6'
aa = z + '1'
ab = ''
for _ in range(5):
        if _ == 5:
            continue
        ab += aa
ac_dict = {16: ab, 50: ab, 75: ab}
ad = random.choice(list(ac_dict.values()))
ae = ad[0:]
af_set = {ae, ae, ae, ae, ae, ae, ae}
af = random.choice(list(af_set))
ag = af + '3'
ah = ag + '3'
ai = ah + '5'
aj = ''
for _ in range(9):
        if _ == 4:
            break
        aj += ai
ak_dict = {80: aj, 48: aj, 25: aj}
al_dict = {67: ak_dict, 11: ak_dict, 3: ak_dict, 85: ak_dict, 37: ak_dict}
am = random.choice(list(al_dict.values()))
an = random.choice(list(am.values()))
ao = ''
for _ in range(9):
        if _ == 3:
            break
        ao += an
ap = ''
for _ in range(10):
        if _ == 1:
            break
        ap += ao
if ap == ap:
    at = ap + 'c1'
elif ap == '20':
    at = aq + 'c2'
else:
    at = ar + 'c3'
print(at)