import random
import math
a = input()
b = a + '.'
if b == '10':
    c = b + ' c1'
elif b == '16':
    c = b + ' c2'
else:
    c = b + ' c3'
d = [c for _ in range(7)]
random.shuffle(d)
e = random.choice(d)
f_dict = {19: e, 21: e, 51: e, 70: e, 49: e}
g_dict = {81: f_dict, 88: f_dict, 59: f_dict, 59: f_dict}
h_dict = {67: g_dict, 34: g_dict, 80: g_dict}
i = random.choice(list(h_dict.values()))
j = random.choice(list(i.values()))
k = random.choice(list(j.values()))
l = k + '.'
m_set = {l, l, l, l, l, l, l, l, l}
m = random.choice(list(m_set))
if m == '6':
    n = m + ' c1'
elif m == '20':
    n = m + ' c2'
else:
    n = m + ' c3'
o_set = {n, n, n, n}
o = random.choice(list(o_set))
p = f'string {o}'
q = (p, p, p)
r, s, t = q
u = r + s + t
v = ''
for _ in range(10):
        if _ == 2:
            continue
        v += u
w_dict = {86: v, 23: v, 39: v, 66: v}
x_dict = {16: w_dict, 4: w_dict, 1: w_dict}
y = random.choice(list(x_dict.values()))
z = random.choice(list(y.values()))
if z == '10':
    aa = z + ' c1'
elif z == '19':
    aa = z + ' c2'
else:
    aa = z + ' c3'
ab = aa + '.'
ac = ''
for _ in range(5):
    for __ in range(3):
                ac += ab
ad = ac[0:]
ae = [ad for _ in range(10)]
random.shuffle(ae)
af = random.choice(ae)
ag = (af, af, af)
ah, ai, aj = ag
ak = ah + ai + aj
print(ak)