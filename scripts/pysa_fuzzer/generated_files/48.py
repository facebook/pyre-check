import random
import math
a = input()
b = ''
for _ in range(3):
    for __ in range(4):
                b += a
if b == '3':
    c = b + ' c1'
elif b == '12':
    c = b + ' c2'
else:
    c = b + ' c3'
d = c + '4'
e_list = [d for _ in range(10)]
f_list = [e_list for _ in range(3)]
g_list = [f_list for _ in range(5)]
h = random.choice(g_list)
i = random.choice(h)
j = random.choice(i)
if j == '3':
    k = j + ' c1'
elif j == '19':
    k = j + ' c2'
else:
    k = j + ' c3'
l = [k for _ in range(8)]
random.shuffle(l)
m = random.choice(l)
n_set = {m, m, m, m, m}
n = random.choice(list(n_set))
o = n[0:]
p_list = [o for _ in range(5)]
q = random.choice(p_list)
r_list = [q for _ in range(2)]
s = random.choice(r_list)
if s == '8':
    t = s + ' c1'
elif s == '19':
    t = s + ' c2'
else:
    t = s + ' c3'
u = f'string {t}'
v = (u, u, u)
w, x, y = v
z = w + x + y
aa_list = [z for _ in range(4)]
ab_list = [aa_list for _ in range(3)]
ac = random.choice(ab_list)
ad = random.choice(ac)
if ad == '6':
    ae = ad + ' c1'
elif ad == '14':
    ae = ad + ' c2'
else:
    ae = ad + ' c3'
af_dict = {35: ae, 22: ae, 67: ae, 95: ae, 38: ae, 6: ae, 18: ae, 30: ae, 84: ae}
ag_dict = {58: af_dict, 47: af_dict, 91: af_dict, 43: af_dict, 38: af_dict, 62: af_dict, 4: af_dict, 78: af_dict, 80: af_dict, 48: af_dict}
ah = random.choice(list(ag_dict.values()))
ai = random.choice(list(ah.values()))
aj_set = {ai, ai, ai, ai, ai, ai, ai}
aj = random.choice(list(aj_set))
ak_dict = {68: aj, 32: aj, 88: aj, 6: aj, 58: aj, 50: aj, 90: aj, 94: aj, 87: aj, 65: aj}
al_dict = {20: ak_dict, 90: ak_dict, 69: ak_dict, 72: ak_dict, 61: ak_dict, 30: ak_dict}
am = random.choice(list(al_dict.values()))
an = random.choice(list(am.values()))
print(an)