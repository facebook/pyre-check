import random
import math
a = input()
b = f'string {a}'
c_list = [b for _ in range(6)]
c = random.choice(c_list)
d = f'string {c}'
e = ''
counter = 0
while counter < 4:
    e += d
    counter += 1
f = e + '7'
g = f'string {f}'
h_list = [g for _ in range(2)]
h = random.choice(h_list)
i_dict = {98: h, 84: h, 47: h, 24: h, 62: h, 66: h, 75: h}
i = random.choice(list(i_dict.values()))
j_set = {i, i, i, i, i, i, i, i, i, i}
j = random.choice(list(j_set))
k_set = {j, j, j, j}
k = random.choice(list(k_set))
l = f'string {k}'
m_list = [l for _ in range(8)]
m = random.choice(m_list)
n = ''
for _ in range(3):
    for __ in range(3):
                n += m
if n == '6':
    o = n + ' c1'
elif n == '14':
    o = n + ' c2'
else:
    o = n + ' c3'
p = o + '7'
q = f'string {p}'
r = ''
counter = 0
while counter < 2:
    r += q
    counter += 1
s_dict = {1: r, 68: r, 54: r}
s = random.choice(list(s_dict.values()))
t = (s, s, s)
u, v, w = t
x = u + v + w
y = ''
counter = 0
while counter < 2:
    y += x
    counter += 1
z = ''
for _ in range(5):
        if _ == 4:
            break
        z += y
aa = z + '4'
ab_list = [aa for _ in range(10)]
ab = random.choice(ab_list)
ac = f'string {ab}'
if ac == '5':
    ad = ac + ' c1'
elif ac == '14':
    ad = ac + ' c2'
else:
    ad = ac + ' c3'
ae = ad + '.'
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak = aj[0:]
al = ''
counter = 0
while counter < 4:
    al += ak
    counter += 1
am_dict = {10: al, 55: al, 83: al, 93: al, 87: al, 7: al, 1: al, 74: al, 3: al}
am = random.choice(list(am_dict.values()))
an = am + '.'
ao = an[0:]
ap = ao[0:]
print(ap)