import random
import math

a = input()
b_list = [a for _ in range(7)]
c_list = [b_list for _ in range(4)]
d_list = [c_list for _ in range(10)]
e = random.choice(d_list)
f = random.choice(e)
g = random.choice(f)
if g == g:
    j = g + 'c1'
elif g == '12':
    j = h + 'c2'
else:
    j = i + 'c3'
k_dict = {71: j, 44: j, 66: j, 68: j, 93: j, 83: j, 35: j, 89: j, 35: j, 33: j}
l_dict = {76: k_dict, 71: k_dict, 83: k_dict, 69: k_dict, 40: k_dict, 29: k_dict, 2: k_dict, 33: k_dict, 44: k_dict, 88: k_dict}
m = random.choice(list(l_dict.values()))
n = random.choice(list(m.values()))
o = n[0:]
p = ''
for _ in range(5):
        if _ == 1:
            break
        p += o
q_dict = {41: p, 50: p, 7: p, 67: p, 92: p}
r_dict = {42: q_dict, 70: q_dict, 68: q_dict, 52: q_dict}
s_dict = {1: r_dict, 87: r_dict, 45: r_dict, 53: r_dict, 95: r_dict, 32: r_dict, 86: r_dict, 62: r_dict, 48: r_dict, 68: r_dict}
t = random.choice(list(s_dict.values()))
u = random.choice(list(t.values()))
v = random.choice(list(u.values()))
w_dict = {43: v, 98: v, 93: v, 78: v, 74: v}
x = random.choice(list(w_dict.values()))
y_set = {x, x, x, x, x, x}
y = random.choice(list(y_set))
z = y + '8'
aa = z + '6'
ab = aa + '8'
ac_dict = {59: ab, 66: ab, 37: ab, 69: ab, 24: ab, 70: ab}
ad = random.choice(list(ac_dict.values()))
ae = ''
for _ in range(8):
        if _ == 3:
            continue
        ae += ad
af = ae[0:]
ag_dict = {68: af, 52: af}
ah_dict = {51: ag_dict, 33: ag_dict, 77: ag_dict, 45: ag_dict, 90: ag_dict, 29: ag_dict, 64: ag_dict, 60: ag_dict, 43: ag_dict, 35: ag_dict}
ai = random.choice(list(ah_dict.values()))
aj = random.choice(list(ai.values()))
ak = (aj, aj, aj)
al, am, an = ak
ao = al + am + an
ap = ao[0:]
aq = (ap, ap, ap)
ar, at, au = aq
av = ar + at + au
aw = f'string {av}'
ax = aw[0:]
print(ax)