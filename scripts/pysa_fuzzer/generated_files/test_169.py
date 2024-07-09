import random
import math

a = input()
b = ''
for _ in range(9):
        if _ == 1:
            continue
        b += a
c_list = [b for _ in range(8)]
d_list = [c_list for _ in range(9)]
e = random.choice(d_list)
f = random.choice(e)
g = ''
for _ in range(6):
        if _ == 2:
            continue
        g += f
h_dict = {40: g, 5: g, 14: g, 57: g, 11: g, 31: g}
i = random.choice(list(h_dict.values()))
j_list = [i for _ in range(8)]
k_list = [j_list for _ in range(9)]
l = random.choice(k_list)
m = random.choice(l)
n = (m, m, m)
o, p, q = n
r = o + p + q
s = r[0:]
t = ''
for _ in range(2):
    for __ in range(4):
                t += s
u_dict = {9: t, 97: t, 1: t, 97: t, 38: t, 65: t, 92: t}
v = random.choice(list(u_dict.values()))
w = v[0:]
x = ''
for _ in range(2):
    for __ in range(3):
                x += w
y = x[0:]
z = f'string {y}'
aa = ''
counteraa = 0
while counteraa < 3:
    aa += z
    counteraa += 1
ab_set = {aa, aa, aa, aa}
ab = random.choice(list(ab_set))
if ab == ab:
    ae = ab + 'c1'
elif ab == '16':
    ae = ac + 'c2'
else:
    ae = ad + 'c3'
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak = aj + '9'
al = ak + '6'
print(al)