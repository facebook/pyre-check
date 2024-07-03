import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = ''
for _ in range(3):
    h = ''
    for _ in range(5):
        h += g
        g += f
i_dict = {71: h, 9: h, 39: h, 8: h}
j_dict = {18: i_dict, 96: i_dict, 5: i_dict, 1: i_dict, 52: i_dict, 16: i_dict, 86: i_dict, 34: i_dict, 73: i_dict, 59: i_dict}
k_dict = {42: j_dict, 42: j_dict, 27: j_dict, 51: j_dict, 46: j_dict, 60: j_dict}
l = random.choice(list(k_dict.values()))
m = random.choice(list(l.values()))
n = random.choice(list(m.values()))
o = n[0:]
p_dict = {91: o, 47: o, 42: o, 62: o, 78: o}
q_dict = {1: p_dict, 1: p_dict, 32: p_dict, 86: p_dict, 93: p_dict, 9: p_dict, 53: p_dict, 87: p_dict, 39: p_dict}
r_dict = {13: q_dict, 28: q_dict, 58: q_dict, 93: q_dict, 75: q_dict}
s = random.choice(list(r_dict.values()))
t = random.choice(list(s.values()))
u = random.choice(list(t.values()))
v = (u, u, u)
w, x, y = v
z = w + x + y
if z == '2':
    aa = z + ' c1'
elif z == '13':
    aa = z + ' c2'
else:
    aa = z + ' c3'
ab = aa[0:]
ac = ab[0:]
ad_dict = {4: ac, 33: ac, 80: ac, 86: ac, 58: ac, 15: ac}
ae_dict = {67: ad_dict, 99: ad_dict, 92: ad_dict, 19: ad_dict}
af_dict = {14: ae_dict, 62: ae_dict, 10: ae_dict, 14: ae_dict}
ag = random.choice(list(af_dict.values()))
ah = random.choice(list(ag.values()))
ai = random.choice(list(ah.values()))
aj = (ai, ai, ai)
ak, al, am = aj
an = ak + al + am
if an == '7':
    ao = an + ' c1'
elif an == '17':
    ao = an + ' c2'
else:
    ao = an + ' c3'
ap = ao[0:]
aq = ''
for _ in range(4):
    for __ in range(4):
                aq += ap
ar = ''
for _ in range(2):
    for __ in range(5):
                ar += aq
at_set = {ar, ar, ar, ar}
at = random.choice(list(at_set))
au_dict = {19: at, 21: at}
av_dict = {37: au_dict, 78: au_dict, 62: au_dict, 51: au_dict, 61: au_dict, 39: au_dict, 17: au_dict, 48: au_dict}
aw = random.choice(list(av_dict.values()))
ax = random.choice(list(aw.values()))
ay_dict = {16: ax, 15: ax, 31: ax, 79: ax, 43: ax}
az_dict = {4: ay_dict, 67: ay_dict, 55: ay_dict, 33: ay_dict}
ba_dict = {80: az_dict, 84: az_dict}
bb = random.choice(list(ba_dict.values()))
bc = random.choice(list(bb.values()))
bd = random.choice(list(bc.values()))
print(bd)