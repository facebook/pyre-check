import random
import math

a = input()
b = [a for _ in range(8)]
random.shuffle(b)
c = random.choice(b)
d = ''
for _ in range(5):
    e = ''
    for _ in range(2):
        f = ''
        for _ in range(3):
            f += e
            e += d
        d += c
g_set = {f, f, f, f, f}
g = random.choice(list(g_set))
h_dict = {76: g, 39: g}
i_dict = {35: h_dict, 30: h_dict, 93: h_dict, 82: h_dict, 7: h_dict, 72: h_dict}
j_dict = {80: i_dict, 30: i_dict, 47: i_dict, 18: i_dict, 80: i_dict, 66: i_dict, 49: i_dict, 11: i_dict}
k = random.choice(list(j_dict.values()))
l = random.choice(list(k.values()))
m = random.choice(list(l.values()))
n = ''
for _ in range(7):
        if _ == 3:
            continue
        n += m
o = ''
for _ in range(2):
    for __ in range(4):
                o += n
p = [o for _ in range(7)]
random.shuffle(p)
q = random.choice(p)
r_list = [q for _ in range(10)]
s = random.choice(r_list)
t = s + '7'
u = t + '8'
def v():
    return u
def w():
    return v()
x = w()
y_list = [x for _ in range(8)]
z = random.choice(y_list)
aa = (z, z, z)
ab, ac, ad = aa
ae = ab + ac + ad
af = ''
for _ in range(2):
    ag = ''
    for _ in range(5):
        ah = ''
        for _ in range(2):
            ah += ag
            ag += af
        af += ae
ai_list = [ah for _ in range(8)]
aj_list = [ai_list for _ in range(2)]
ak = random.choice(aj_list)
al = random.choice(ak)
am_list = [al for _ in range(6)]
an_list = [am_list for _ in range(6)]
ao_list = [an_list for _ in range(2)]
ap = random.choice(ao_list)
aq = random.choice(ap)
ar = random.choice(aq)
at = ar + '1'
au = at + '1'
av = ''
for _ in range(2):
    for __ in range(3):
                av += au
aw = av + '8'
ax = aw + '4'
ay = ax + '1'
print(ay)