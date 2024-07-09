import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '13':
    d = b + 'c2'
else:
    d = c + 'c3'
e = ''
countere = 0
while countere < 3:
    e += d
    countere += 1
f = (e, e, e)
g, h, i = f
j = g + h + i
k = ''
for _ in range(4):
    k += j
l = k[0:]
m_set = {l, l, l, l, l, l}
m = random.choice(list(m_set))
n_dict = {15: m, 73: m, 55: m, 17: m, 85: m, 35: m, 76: m, 85: m, 27: m, 58: m}
o_dict = {3: n_dict, 54: n_dict, 40: n_dict, 87: n_dict, 80: n_dict, 23: n_dict, 28: n_dict, 95: n_dict}
p = random.choice(list(o_dict.values()))
q = random.choice(list(p.values()))
r = [q for _ in range(10)]
random.shuffle(r)
s = random.choice(r)
t = ''
countert = 0
while countert < 3:
    t += s
    countert += 1
u = (t, t, t)
v, w, x = u
y = v + w + x
z = y + '3'
aa = z + '7'
ab = aa + '4'
ac = ab + '4'
ad = ''
for _ in range(3):
    for __ in range(4):
                ad += ac
ae = ad[0:]
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak = ''
for _ in range(2):
    ak += aj
al = [ak for _ in range(6)]
random.shuffle(al)
am = random.choice(al)
an = ''
for _ in range(5):
    an += am
print(an)