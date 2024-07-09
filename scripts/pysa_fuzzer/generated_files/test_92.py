import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '19':
    d = b + 'c2'
else:
    d = c + 'c3'
e_list = [d for _ in range(4)]
f_list = [e_list for _ in range(10)]
g = random.choice(f_list)
h = random.choice(g)
i_dict = {87: h, 84: h, 36: h, 31: h, 89: h, 54: h, 76: h, 37: h, 31: h, 11: h}
j_dict = {8: i_dict, 66: i_dict, 68: i_dict, 97: i_dict}
k = random.choice(list(j_dict.values()))
l = random.choice(list(k.values()))
m_dict = {92: l, 12: l, 26: l, 73: l, 24: l, 84: l}
n_dict = {47: m_dict, 27: m_dict, 83: m_dict, 5: m_dict, 8: m_dict, 96: m_dict}
o = random.choice(list(n_dict.values()))
p = random.choice(list(o.values()))
q = ''
for _ in range(5):
    q += p
if q == q:
    t = q + 'c1'
elif q == '12':
    t = r + 'c2'
else:
    t = s + 'c3'
u = ''
for _ in range(2):
    u += t
v_set = {u, u, u, u, u, u}
v = random.choice(list(v_set))
w = (v, v, v)
x, y, z = w
aa = x + y + z
if aa == aa:
    ad = aa + 'c1'
elif aa == '16':
    ad = ab + 'c2'
else:
    ad = ac + 'c3'
ae = f'string {ad}'
af = ae[0:]
ag_dict = {9: af, 12: af, 29: af, 46: af, 42: af, 74: af, 81: af, 2: af, 91: af}
ah_dict = {40: ag_dict, 22: ag_dict}
ai = random.choice(list(ah_dict.values()))
aj = random.choice(list(ai.values()))
ak_dict = {15: aj, 85: aj, 12: aj, 42: aj, 99: aj}
al_dict = {8: ak_dict, 72: ak_dict}
am = random.choice(list(al_dict.values()))
an = random.choice(list(am.values()))
ao = an + '9'
ap_list = [ao for _ in range(5)]
aq = random.choice(ap_list)
ar = ''
counterar = 0
while counterar < 5:
    at = ''
    counterat = 0
    while counterat < 5:
        at += ar
        counterat += 1
        ar += aq
        counterar += 1
au = (at, at, at)
av, aw, ax = au
ay = av + aw + ax
print(ay)