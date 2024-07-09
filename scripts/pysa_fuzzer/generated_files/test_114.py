import random
import math

a = input()
b = a + '.'
c = f'string {b}'
d_dict = {3: c, 92: c, 7: c, 53: c, 47: c, 14: c, 74: c, 15: c, 36: c}
e = random.choice(list(d_dict.values()))
f = f'string {e}'
g = ''
counterg = 0
while counterg < 5:
    g += f
    counterg += 1
h = g[0:]
i = h + '.'
j = ''
counterj = 0
while counterj < 5:
    j += i
    counterj += 1
k = j[0:]
l = ''
for _ in range(2):
    m = ''
    for _ in range(2):
        n = ''
        for _ in range(2):
            n += m
            m += l
        l += k
o = n + '7'
p = [o for _ in range(9)]
random.shuffle(p)
q = random.choice(p)
r_dict = {97: q, 20: q, 87: q, 10: q, 49: q, 29: q, 99: q}
s_dict = {2: r_dict, 84: r_dict, 3: r_dict, 2: r_dict}
t_dict = {45: s_dict, 93: s_dict, 50: s_dict, 13: s_dict, 6: s_dict, 41: s_dict, 95: s_dict}
u = random.choice(list(t_dict.values()))
v = random.choice(list(u.values()))
w = random.choice(list(v.values()))
x = ''
for _ in range(10):
        if _ == 1:
            continue
        x += w
y = x[0:]
z_set = {y, y, y, y, y, y, y}
z = random.choice(list(z_set))
if z == z:
    ac = z + 'c1'
elif z == '13':
    ac = aa + 'c2'
else:
    ac = ab + 'c3'
ad_set = {ac, ac, ac}
ad = random.choice(list(ad_set))
print(ad)