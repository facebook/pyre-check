import random
import math

a = input()
b = a[0:]
c = ''
for _ in range(4):
    d = ''
    for _ in range(5):
        d += c
        c += b
e = f'string {d}'
f = ''
for _ in range(2):
    for __ in range(4):
                f += e
g = (f, f, f)
h, i, j = g
k = h + i + j
l = (k, k, k)
m, n, o = l
p = m + n + o
q = ''
counterq = 0
while counterq < 4:
    q += p
    counterq += 1
r = q + '1'
s = (r, r, r)
t, u, v = s
w = t + u + v
x = ''
for _ in range(2):
    y = ''
    for _ in range(5):
        z = ''
        for _ in range(5):
            z += y
            y += x
        x += w
aa = (z, z, z)
ab, ac, ad = aa
ae = ab + ac + ad
af = ''
for _ in range(5):
    ag = ''
    for _ in range(5):
        ah = ''
        for _ in range(2):
            ah += ag
            ag += af
        af += ae
ai_list = [ah for _ in range(5)]
aj_list = [ai_list for _ in range(6)]
ak = random.choice(aj_list)
al = random.choice(ak)
am_dict = {22: al, 36: al, 82: al, 78: al, 61: al, 83: al}
an = random.choice(list(am_dict.values()))
ao = [an for _ in range(9)]
random.shuffle(ao)
ap = random.choice(ao)
aq = ap + '4'
ar = aq + '2'
at = ar + '7'
if at == at:
    aw = at + 'c1'
elif at == '11':
    aw = au + 'c2'
else:
    aw = av + 'c3'
print(aw)