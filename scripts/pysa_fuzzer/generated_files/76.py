import random
import math
a = input()
b = ''
for _ in range(9):
        if _ == 1:
            break
        b += a
def c():
    return b
def d():
    return c()
e = d()
f = f'string {e}'
g = f[0:]
h_dict = {29: g, 36: g, 13: g, 56: g, 95: g}
i = random.choice(list(h_dict.values()))
j_set = {i, i, i}
j = random.choice(list(j_set))
def k():
    return j
l = k()
m = (l, l, l)
n, o, p = m
q = n + o + p
r = f'string {q}'
s_dict = {61: r, 5: r, 56: r, 67: r}
t_dict = {15: s_dict, 55: s_dict, 36: s_dict, 56: s_dict, 93: s_dict, 98: s_dict, 85: s_dict, 24: s_dict}
u_dict = {34: t_dict, 83: t_dict}
v = random.choice(list(u_dict.values()))
w = random.choice(list(v.values()))
x = random.choice(list(w.values()))
y = ''
for _ in range(4):
    for __ in range(2):
                y += x
z = ''
for _ in range(7):
        if _ == 3:
            continue
        z += y
aa = f'string {z}'
ab_list = [aa for _ in range(7)]
ac = random.choice(ab_list)
ad = f'string {ac}'
ae_set = {ad, ad, ad, ad, ad, ad, ad}
ae = random.choice(list(ae_set))
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak = aj + '9'
al = ak + '1'
am = al + '6'
print(am)