import random
import math

a = input()
b = ''
for _ in range(3):
    c = ''
    for _ in range(5):
        d = ''
        for _ in range(4):
            d += c
            c += b
        b += a
e = ''
for _ in range(2):
    f = ''
    for _ in range(3):
        f += e
        e += d
g = ''
for _ in range(4):
    h = ''
    for _ in range(4):
        h += g
        g += f
i = h + '8'
j = i + '9'
k = j + '.'
l = k[0:]
m_set = {l, l, l, l, l, l, l}
m = random.choice(list(m_set))
n_dict = {74: m, 78: m}
o_dict = {60: n_dict, 1: n_dict, 89: n_dict, 11: n_dict, 97: n_dict}
p = random.choice(list(o_dict.values()))
q = random.choice(list(p.values()))
r = f'string {q}'
s = r[0:]
t = (s, s, s)
u, v, w = t
x = u + v + w
y = (x, x, x)
z, aa, ab = y
ac = z + aa + ab
ad = ''
for _ in range(5):
        if _ == 3:
            break
        ad += ac
ae = ''
counterae = 0
while counterae < 5:
    ae += ad
    counterae += 1
af = ''
for _ in range(3):
    af += ae
if af == af:
    ai = af + 'c1'
elif af == '12':
    ai = ag + 'c2'
else:
    ai = ah + 'c3'
aj = [ai for _ in range(7)]
random.shuffle(aj)
ak = random.choice(aj)
al = [ak for _ in range(6)]
random.shuffle(al)
am = random.choice(al)
print(am)