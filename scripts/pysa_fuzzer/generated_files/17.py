import random
import math
a = input()
b = ''
for _ in range(5):
    c = ''
    for _ in range(5):
        c += b
        b += a
d_set = {c, c, c, c, c, c, c, c}
d = random.choice(list(d_set))
e = d[0:]
f_list = [e for _ in range(7)]
g = random.choice(f_list)
h = ''
for _ in range(5):
        if _ == 5:
            break
        h += g
def i():
    return h
def j():
    return i()
def k():
    return j()
l = k()
m = f'string {l}'
n = ''
for _ in range(3):
    for __ in range(4):
                n += m
o_dict = {93: n, 48: n, 94: n, 4: n}
p = random.choice(list(o_dict.values()))
q = [p for _ in range(8)]
random.shuffle(q)
r = random.choice(q)
s = f'string {r}'
t_dict = {92: s, 76: s, 78: s}
u_dict = {41: t_dict, 10: t_dict, 86: t_dict, 68: t_dict}
v = random.choice(list(u_dict.values()))
w = random.choice(list(v.values()))
x = [w for _ in range(9)]
random.shuffle(x)
y = random.choice(x)
z = ''
for _ in range(9):
        if _ == 1:
            continue
        z += y
if z == '7':
    aa = z + ' c1'
elif z == '18':
    aa = z + ' c2'
else:
    aa = z + ' c3'
ab_dict = {38: aa, 74: aa}
ac_dict = {20: ab_dict, 71: ab_dict, 6: ab_dict, 69: ab_dict, 80: ab_dict, 44: ab_dict, 1: ab_dict}
ad_dict = {56: ac_dict, 69: ac_dict, 29: ac_dict, 7: ac_dict, 33: ac_dict}
ae = random.choice(list(ad_dict.values()))
af = random.choice(list(ae.values()))
ag = random.choice(list(af.values()))
ah = (ag, ag, ag)
ai, aj, ak = ah
al = ai + aj + ak
am = ''
for _ in range(3):
    an = ''
    for _ in range(3):
        ao = ''
        for _ in range(2):
            ao += an
            an += am
        am += al
print(ao)