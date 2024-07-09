import random
import math

a = input()
b_set = {a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c_list = [b for _ in range(6)]
d = random.choice(c_list)
e = ''
for _ in range(5):
    for __ in range(2):
                e += d
f = e + '.'
g = f + '.'
h = ''
for _ in range(7):
        if _ == 2:
            continue
        h += g
i = h + '.'
j = (i, i, i)
k, l, m = j
n = k + l + m
o = ''
for _ in range(3):
    for __ in range(3):
                o += n
p = [o for _ in range(10)]
random.shuffle(p)
q = random.choice(p)
r_dict = {45: q, 82: q, 12: q, 75: q, 32: q, 50: q, 82: q}
s = random.choice(list(r_dict.values()))
t = (s, s, s)
u, v, w = t
x = u + v + w
if x == x:
    aa = x + 'c1'
elif x == '17':
    aa = y + 'c2'
else:
    aa = z + 'c3'
ab_list = [aa for _ in range(2)]
ac_list = [ab_list for _ in range(8)]
ad = random.choice(ac_list)
ae = random.choice(ad)
af = ae + '2'
ag = af + '5'
ah_set = {ag, ag}
ah = random.choice(list(ah_set))
ai = ''
for _ in range(4):
    for __ in range(2):
                ai += ah
aj = ''
for _ in range(2):
    for __ in range(2):
                aj += ai
print(aj)