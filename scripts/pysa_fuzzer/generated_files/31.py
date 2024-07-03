import random
import math
a = input()
b = ''
for _ in range(2):
    b += a
c = ''
for _ in range(5):
    c += b
d = ''
for _ in range(3):
    for __ in range(3):
                d += c
e = (d, d, d)
f, g, h = e
i = f + g + h
j = ''
for _ in range(3):
    j += i
k = (j, j, j)
l, m, n = k
o = l + m + n
p = o + '.'
q_set = {p, p}
q = random.choice(list(q_set))
r = q[0:]
s = r + '.'
t = ''
for _ in range(5):
    for __ in range(4):
                t += s
u = ''
for _ in range(4):
    for __ in range(5):
                u += t
v = (u, u, u)
w, x, y = v
z = w + x + y
aa = [z for _ in range(7)]
random.shuffle(aa)
ab = random.choice(aa)
ac_dict = {43: ab, 16: ab, 72: ab, 17: ab}
ad_dict = {9: ac_dict, 9: ac_dict, 23: ac_dict, 96: ac_dict, 32: ac_dict, 72: ac_dict, 49: ac_dict}
ae_dict = {77: ad_dict, 75: ad_dict, 68: ad_dict}
af = random.choice(list(ae_dict.values()))
ag = random.choice(list(af.values()))
ah = random.choice(list(ag.values()))
ai_list = [ah for _ in range(2)]
aj = random.choice(ai_list)
ak_list = [aj for _ in range(4)]
al_list = [ak_list for _ in range(5)]
am_list = [al_list for _ in range(4)]
an = random.choice(am_list)
ao = random.choice(an)
ap = random.choice(ao)
aq = f'string {ap}'
print(aq)