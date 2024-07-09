import random
import math

a = input()
b = ''
for _ in range(3):
    b += a
c_dict = {25: b, 58: b, 49: b, 95: b, 56: b, 95: b, 93: b, 77: b, 80: b}
d = random.choice(list(c_dict.values()))
e = [d for _ in range(6)]
random.shuffle(e)
f = random.choice(e)
g_list = [f for _ in range(10)]
h_list = [g_list for _ in range(5)]
i_list = [h_list for _ in range(4)]
j = random.choice(i_list)
k = random.choice(j)
l = random.choice(k)
m_set = {l, l, l, l, l, l, l}
m = random.choice(list(m_set))
n = ''
for _ in range(3):
    o = ''
    for _ in range(3):
        o += n
        n += m
p = (o, o, o)
q, r, s = p
t = q + r + s
u_set = {t, t}
u = random.choice(list(u_set))
v_set = {u, u}
v = random.choice(list(v_set))
w_set = {v, v, v, v, v, v, v, v, v}
w = random.choice(list(w_set))
x = (w, w, w)
y, z, aa = x
ab = y + z + aa
ac_dict = {71: ab, 40: ab}
ad_dict = {22: ac_dict, 14: ac_dict, 78: ac_dict, 65: ac_dict, 61: ac_dict, 85: ac_dict, 37: ac_dict, 28: ac_dict, 62: ac_dict, 8: ac_dict}
ae_dict = {90: ad_dict, 81: ad_dict, 59: ad_dict}
af = random.choice(list(ae_dict.values()))
ag = random.choice(list(af.values()))
ah = random.choice(list(ag.values()))
ai = ''
for _ in range(5):
    for __ in range(5):
                ai += ah
aj = ''
for _ in range(3):
    aj += ai
ak = ''
counterak = 0
while counterak < 2:
    al = ''
    counteral = 0
    while counteral < 4:
        al += ak
        counteral += 1
        ak += aj
        counterak += 1
am = ''
for _ in range(8):
        if _ == 2:
            continue
        am += al
an_dict = {5: am, 30: am, 76: am, 17: am, 84: am, 78: am, 46: am, 1: am, 63: am, 89: am}
ao_dict = {18: an_dict, 63: an_dict, 41: an_dict, 15: an_dict, 29: an_dict}
ap_dict = {70: ao_dict, 40: ao_dict, 77: ao_dict, 56: ao_dict, 23: ao_dict}
aq = random.choice(list(ap_dict.values()))
ar = random.choice(list(aq.values()))
at = random.choice(list(ar.values()))
if at == at:
    aw = at + 'c1'
elif at == '16':
    aw = au + 'c2'
else:
    aw = av + 'c3'
print(aw)