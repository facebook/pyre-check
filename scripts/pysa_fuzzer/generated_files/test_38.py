import random
import math

a = input()
b = f'string {a}'
c_set = {b, b, b, b, b}
c = random.choice(list(c_set))
d = c + '.'
e = (d, d, d)
f, g, h = e
i = f + g + h
j_set = {i, i, i, i, i, i, i, i}
j = random.choice(list(j_set))
k_set = {j, j, j, j}
k = random.choice(list(k_set))
l = [k for _ in range(7)]
random.shuffle(l)
m = random.choice(l)
n_dict = {35: m, 57: m}
o_dict = {52: n_dict, 47: n_dict, 83: n_dict, 68: n_dict, 77: n_dict, 78: n_dict}
p = random.choice(list(o_dict.values()))
q = random.choice(list(p.values()))
r = ''
for _ in range(3):
    for __ in range(3):
                r += q
s_set = {r, r, r, r, r, r, r}
s = random.choice(list(s_set))
t = ''
countert = 0
while countert < 2:
    t += s
    countert += 1
u = t[0:]
v = ''
for _ in range(3):
    for __ in range(5):
                v += u
if v == v:
    y = v + 'c1'
elif v == '14':
    y = w + 'c2'
else:
    y = x + 'c3'
z = ''
for _ in range(3):
    aa = ''
    for _ in range(5):
        aa += z
        z += y
ab = [aa for _ in range(7)]
random.shuffle(ab)
ac = random.choice(ab)
ad_set = {ac, ac, ac, ac, ac}
ad = random.choice(list(ad_set))
ae = ''
for _ in range(9):
        if _ == 1:
            break
        ae += ad
print(ae)