import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g_set = {f, f}
g = random.choice(list(g_set))
h_list = [g for _ in range(9)]
i = random.choice(h_list)
j_list = [i for _ in range(9)]
k_list = [j_list for _ in range(8)]
l_list = [k_list for _ in range(2)]
m = random.choice(l_list)
n = random.choice(m)
o = random.choice(n)
p = ''
for _ in range(5):
        if _ == 2:
            break
        p += o
q_dict = {90: p, 82: p, 32: p, 36: p}
r_dict = {68: q_dict, 46: q_dict, 44: q_dict, 87: q_dict, 46: q_dict, 14: q_dict, 20: q_dict, 54: q_dict}
s = random.choice(list(r_dict.values()))
t = random.choice(list(s.values()))
u = (t, t, t)
v, w, x = u
y = v + w + x
z_list = [y for _ in range(10)]
aa = random.choice(z_list)
ab = ''
for _ in range(8):
        if _ == 3:
            continue
        ab += aa
ac = ''
for _ in range(9):
        if _ == 5:
            break
        ac += ab
ad = ac[0:]
ae = ''
for _ in range(3):
    af = ''
    for _ in range(3):
        af += ae
        ae += ad
ag = [af for _ in range(7)]
random.shuffle(ag)
ah = random.choice(ag)
ai = ''
for _ in range(9):
        if _ == 2:
            break
        ai += ah
aj = ai + '.'
ak = aj + '.'
al = f'string {ak}'
if al == '10':
    am = al + ' c1'
elif al == '13':
    am = al + ' c2'
else:
    am = al + ' c3'
print(am)