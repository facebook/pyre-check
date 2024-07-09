import random
import math

a = input()
b = f'string {a}'
c = (b, b, b)
d, e, f = c
g = d + e + f
h_dict = {50: g, 73: g, 40: g, 26: g, 90: g, 51: g}
i = random.choice(list(h_dict.values()))
j = i + '4'
k = j + '9'
l = k + '7'
m_set = {l, l, l, l, l, l, l, l, l}
m = random.choice(list(m_set))
n = ''
for _ in range(4):
    o = ''
    for _ in range(3):
        o += n
        n += m
p = ''
for _ in range(3):
    q = ''
    for _ in range(3):
        r = ''
        for _ in range(5):
            r += q
            q += p
        p += o
s_set = {r, r, r}
s = random.choice(list(s_set))
t = (s, s, s)
u, v, w = t
x = u + v + w
y = ''
for _ in range(2):
    for __ in range(2):
                y += x
z_dict = {3: y, 10: y}
aa_dict = {13: z_dict, 49: z_dict, 100: z_dict, 24: z_dict, 36: z_dict, 41: z_dict, 98: z_dict, 79: z_dict, 92: z_dict}
ab_dict = {77: aa_dict, 87: aa_dict, 57: aa_dict, 86: aa_dict, 4: aa_dict, 10: aa_dict, 49: aa_dict, 42: aa_dict}
ac = random.choice(list(ab_dict.values()))
ad = random.choice(list(ac.values()))
ae = random.choice(list(ad.values()))
af = ''
counteraf = 0
while counteraf < 3:
    ag = ''
    counterag = 0
    while counterag < 4:
        ag += af
        counterag += 1
        af += ae
        counteraf += 1
ah = ag + '1'
ai = (ah, ah, ah)
aj, ak, al = ai
am = aj + ak + al
an = am + '.'
ao = an[0:]
ap = ''
for _ in range(10):
        if _ == 4:
            break
        ap += ao
aq = ap + '.'
print(aq)