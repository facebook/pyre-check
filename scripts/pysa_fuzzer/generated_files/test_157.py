import random
import math

a = input()
b = a + '8'
c = b + '4'
d = c + '3'
e = d + '8'
f = (e, e, e)
g, h, i = f
j = g + h + i
k_dict = {93: j, 59: j, 12: j, 17: j, 89: j, 73: j, 32: j, 62: j}
l_dict = {21: k_dict, 30: k_dict, 94: k_dict, 33: k_dict, 22: k_dict, 95: k_dict, 56: k_dict, 45: k_dict, 8: k_dict}
m = random.choice(list(l_dict.values()))
n = random.choice(list(m.values()))
o = n[0:]
p_dict = {12: o, 55: o, 73: o, 95: o, 39: o, 83: o, 63: o, 27: o}
q_dict = {23: p_dict, 97: p_dict, 16: p_dict, 31: p_dict, 15: p_dict, 98: p_dict}
r = random.choice(list(q_dict.values()))
s = random.choice(list(r.values()))
t = ''
for _ in range(4):
    for __ in range(2):
                t += s
if t == t:
    w = t + 'c1'
elif t == '13':
    w = u + 'c2'
else:
    w = v + 'c3'
if w == w:
    z = w + 'c1'
elif w == '15':
    z = x + 'c2'
else:
    z = y + 'c3'
aa_set = {z, z, z, z, z, z, z}
aa = random.choice(list(aa_set))
ab_dict = {54: aa, 37: aa, 19: aa}
ac_dict = {82: ab_dict, 24: ab_dict, 55: ab_dict, 96: ab_dict, 2: ab_dict, 77: ab_dict, 57: ab_dict, 78: ab_dict, 17: ab_dict, 68: ab_dict}
ad = random.choice(list(ac_dict.values()))
ae = random.choice(list(ad.values()))
af = ''
for _ in range(5):
    af += ae
ag = f'string {af}'
ah = f'string {ag}'
ai_list = [ah for _ in range(7)]
aj = random.choice(ai_list)
ak = aj[0:]
al_dict = {81: ak, 43: ak, 68: ak, 99: ak, 40: ak, 98: ak, 56: ak, 88: ak}
am_dict = {88: al_dict, 25: al_dict, 62: al_dict, 75: al_dict, 7: al_dict, 41: al_dict, 53: al_dict, 39: al_dict}
an_dict = {89: am_dict, 28: am_dict, 96: am_dict, 26: am_dict, 84: am_dict, 81: am_dict}
ao = random.choice(list(an_dict.values()))
ap = random.choice(list(ao.values()))
aq = random.choice(list(ap.values()))
ar = f'string {aq}'
print(ar)