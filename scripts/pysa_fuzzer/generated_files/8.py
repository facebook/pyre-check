import random
import math
a = input()
b = a + '2'
c = b + '5'
d = ''
for _ in range(6):
        if _ == 1:
            continue
        d += c
e_set = {d, d, d, d, d, d, d}
e = random.choice(list(e_set))
f = ''
for _ in range(5):
    for __ in range(5):
                f += e
g = ''
for _ in range(10):
        if _ == 5:
            continue
        g += f
h_set = {g, g}
h = random.choice(list(h_set))
i = [h for _ in range(7)]
random.shuffle(i)
j = random.choice(i)
k = (j, j, j)
l, m, n = k
o = l + m + n
p_set = {o, o, o, o, o, o, o, o, o, o}
p = random.choice(list(p_set))
q_dict = {50: p, 95: p, 6: p, 35: p, 38: p, 67: p, 41: p, 54: p}
r_dict = {40: q_dict, 92: q_dict, 56: q_dict, 2: q_dict, 44: q_dict}
s_dict = {12: r_dict, 11: r_dict}
t = random.choice(list(s_dict.values()))
u = random.choice(list(t.values()))
v = random.choice(list(u.values()))
if v == '4':
    w = v + ' c1'
elif v == '15':
    w = v + ' c2'
else:
    w = v + ' c3'
x = ''
for _ in range(6):
        if _ == 2:
            continue
        x += w
if x == '8':
    y = x + ' c1'
elif x == '17':
    y = x + ' c2'
else:
    y = x + ' c3'
z = y + '2'
aa = z + '1'
ab = [aa for _ in range(8)]
random.shuffle(ab)
ac = random.choice(ab)
ad_set = {ac, ac, ac, ac, ac, ac, ac, ac, ac, ac}
ad = random.choice(list(ad_set))
ae = f'string {ad}'
af = ''
for _ in range(3):
    for __ in range(4):
                af += ae
print(af)