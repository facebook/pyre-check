import random
import math

a = input()
def b():
    return a
c = b()
d_dict = {59: c, 99: c, 52: c, 34: c, 1: c, 79: c, 15: c, 57: c}
e_dict = {82: d_dict, 78: d_dict, 28: d_dict, 100: d_dict, 97: d_dict, 74: d_dict, 62: d_dict, 36: d_dict, 18: d_dict}
f_dict = {46: e_dict, 61: e_dict, 19: e_dict, 78: e_dict}
g = random.choice(list(f_dict.values()))
h = random.choice(list(g.values()))
i = random.choice(list(h.values()))
j = ''
for _ in range(6):
        if _ == 1:
            continue
        j += i
k_set = {j, j, j, j, j, j, j, j, j}
k = random.choice(list(k_set))
l_dict = {74: k, 18: k, 31: k, 77: k, 66: k, 66: k, 18: k, 30: k}
m_dict = {42: l_dict, 41: l_dict, 20: l_dict, 22: l_dict, 63: l_dict, 1: l_dict, 1: l_dict}
n_dict = {32: m_dict, 70: m_dict, 3: m_dict, 96: m_dict, 99: m_dict, 57: m_dict, 19: m_dict, 27: m_dict, 58: m_dict}
o = random.choice(list(n_dict.values()))
p = random.choice(list(o.values()))
q = random.choice(list(p.values()))
r_dict = {21: q, 40: q, 36: q, 19: q, 47: q, 45: q, 90: q, 97: q, 46: q, 19: q}
s = random.choice(list(r_dict.values()))
t = s + '.'
u = (t, t, t)
v, w, x = u
y = v + w + x
z_set = {y, y, y, y, y, y}
z = random.choice(list(z_set))
aa = ''
for _ in range(3):
    ab = ''
    for _ in range(2):
        ab += aa
        aa += z
ac = f'string {ab}'
ad = [ac for _ in range(7)]
random.shuffle(ad)
ae = random.choice(ad)
af = ''
for _ in range(2):
    for __ in range(2):
                af += ae
ag = [af for _ in range(8)]
random.shuffle(ag)
ah = random.choice(ag)
ai = ''
for _ in range(9):
        if _ == 1:
            break
        ai += ah
aj_dict = {87: ai, 36: ai}
ak_dict = {11: aj_dict, 47: aj_dict, 89: aj_dict, 32: aj_dict}
al = random.choice(list(ak_dict.values()))
am = random.choice(list(al.values()))
if am == am:
    ap = am + 'c1'
elif am == '11':
    ap = an + 'c2'
else:
    ap = ao + 'c3'
aq = ''
for _ in range(2):
    ar = ''
    for _ in range(3):
        ar += aq
        aq += ap
print(ar)