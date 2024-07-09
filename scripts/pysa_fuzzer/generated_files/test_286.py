import random
import math

a = input()
b_set = {a, a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c = ''
for _ in range(4):
    d = ''
    for _ in range(2):
        d += c
        c += b
e = ''
for _ in range(4):
    for __ in range(4):
                e += d
f = e + '.'
g = [f for _ in range(8)]
random.shuffle(g)
h = random.choice(g)
i = ''
for _ in range(5):
    for __ in range(5):
                i += h
j_set = {i, i, i, i}
j = random.choice(list(j_set))
k = ''
for _ in range(5):
    l = ''
    for _ in range(5):
        m = ''
        for _ in range(3):
            m += l
            l += k
        k += j
n = ''
for _ in range(4):
    n += m
o = n + '.'
p = (o, o, o)
q, r, s = p
t = q + r + s
u = f'string {t}'
v = (u, u, u)
w, x, y = v
z = w + x + y
aa_dict = {40: z, 24: z, 23: z, 43: z, 93: z, 56: z, 9: z, 67: z, 50: z}
ab = random.choice(list(aa_dict.values()))
ac = ''
for _ in range(2):
    for __ in range(2):
                ac += ab
ad_dict = {59: ac, 58: ac, 46: ac, 47: ac, 91: ac, 22: ac}
ae = random.choice(list(ad_dict.values()))
af = ''
counteraf = 0
while counteraf < 3:
    ag = ''
    counterag = 0
    while counterag < 5:
        ag += af
        counterag += 1
        af += ae
        counteraf += 1
ah = (ag, ag, ag)
ai, aj, ak = ah
al = ai + aj + ak
print(al)