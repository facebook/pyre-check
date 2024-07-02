import random
import math
a = input()
b_dict = {38: a, 22: a}
c_dict = {95: b_dict, 90: b_dict, 28: b_dict, 58: b_dict, 90: b_dict, 81: b_dict, 46: b_dict, 4: b_dict, 64: b_dict}
d = random.choice(list(c_dict.values()))
e = random.choice(list(d.values()))
f = e + '.'
g = [f for _ in range(8)]
random.shuffle(g)
h = random.choice(g)
i = f'string {h}'
j = ''
for _ in range(2):
    for __ in range(3):
                j += i
k_set = {j, j, j}
k = random.choice(list(k_set))
l = ''
counterl = 0
while counterl < 3:
    l += k
    counterl += 1
m_set = {l, l, l, l, l, l}
m = random.choice(list(m_set))
n = ''
for _ in range(8):
        if _ == 1:
            continue
        n += m
o_set = {n, n, n, n}
o = random.choice(list(o_set))
p = (o, o, o)
q, r, s = p
t = q + r + s
u = t[0:]
v = ''
for _ in range(3):
    for __ in range(4):
                v += u
w = ''
for _ in range(4):
    x = ''
    for _ in range(2):
        x += w
        w += v
y = f'string {x}'
z = [y for _ in range(8)]
random.shuffle(z)
aa = random.choice(z)
ab_set = {aa, aa, aa, aa, aa, aa}
ab = random.choice(list(ab_set))
ac = ''
for _ in range(2):
    ac += ab
print(ac)