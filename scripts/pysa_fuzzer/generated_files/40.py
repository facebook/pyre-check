import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = f + '.'
h = g[0:]
i_dict = {45: h, 82: h, 94: h, 31: h, 43: h, 13: h, 68: h}
j_dict = {93: i_dict, 90: i_dict}
k = random.choice(list(j_dict.values()))
l = random.choice(list(k.values()))
m_dict = {65: l, 49: l, 77: l, 77: l, 92: l}
n = random.choice(list(m_dict.values()))
o = n[0:]
p = f'string {o}'
q = p + '3'
r = q + '.'
s_list = [r for _ in range(8)]
t_list = [s_list for _ in range(6)]
u_list = [t_list for _ in range(5)]
v = random.choice(u_list)
w = random.choice(v)
x = random.choice(w)
y = ''
for _ in range(5):
    z = ''
    for _ in range(5):
        aa = ''
        for _ in range(3):
            aa += z
            z += y
        y += x
ab = aa[0:]
ac = f'string {ab}'
def ad():
    return ac
def ae():
    return ad()
af = ae()
ag = (af, af, af)
ah, ai, aj = ag
ak = ah + ai + aj
al = ''
for _ in range(5):
    al += ak
am_set = {al, al, al}
am = random.choice(list(am_set))
an = am + '1'
ao = an + '7'
ap = ao + '7'
print(ap)