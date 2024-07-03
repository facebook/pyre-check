import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = ''
for _ in range(5):
    for __ in range(4):
                g += f
h = ''
for _ in range(5):
        if _ == 1:
            break
        h += g
i = h + '.'
j = ''
for _ in range(3):
    k = ''
    for _ in range(3):
        l = ''
        for _ in range(4):
            l += k
            k += j
        j += i
m_dict = {7: l, 14: l, 67: l, 49: l}
n_dict = {63: m_dict, 27: m_dict, 52: m_dict, 8: m_dict}
o_dict = {70: n_dict, 88: n_dict, 95: n_dict, 95: n_dict, 42: n_dict}
p = random.choice(list(o_dict.values()))
q = random.choice(list(p.values()))
r = random.choice(list(q.values()))
s = r + '4'
t = s + '8'
u = t + '4'
v = ''
for _ in range(4):
    v += u
w = v + '.'
x = ''
counterx = 0
while counterx < 4:
    y = ''
    countery = 0
    while countery < 2:
        z = ''
        counterz = 0
        while counterz < 5:
            z += y
            counterz += 1
            y += x
            countery += 1
        x += w
        counterx += 1
aa_set = {z, z, z, z, z, z, z}
aa = random.choice(list(aa_set))
ab = ''
for _ in range(10):
        if _ == 3:
            break
        ab += aa
ac_dict = {76: ab, 3: ab, 15: ab, 70: ab, 7: ab, 12: ab, 82: ab, 68: ab, 20: ab}
ad_dict = {76: ac_dict, 28: ac_dict, 46: ac_dict, 6: ac_dict, 10: ac_dict, 67: ac_dict, 88: ac_dict, 9: ac_dict}
ae_dict = {51: ad_dict, 45: ad_dict, 81: ad_dict}
af = random.choice(list(ae_dict.values()))
ag = random.choice(list(af.values()))
ah = random.choice(list(ag.values()))
ai = (ah, ah, ah)
aj, ak, al = ai
am = aj + ak + al
an = am[0:]
ao = an[0:]
ap = ''
for _ in range(3):
    for __ in range(4):
                ap += ao
aq = (ap, ap, ap)
ar, at, au = aq
av = ar + at + au
print(av)