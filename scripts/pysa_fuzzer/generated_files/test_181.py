import random
import math

a = input()
b = a[0:]
c = ''
counterc = 0
while counterc < 2:
    d = ''
    counterd = 0
    while counterd < 3:
        d += c
        counterd += 1
        c += b
        counterc += 1
e = (d, d, d)
f, g, h = e
i = f + g + h
j = i[0:]
k = [j for _ in range(7)]
random.shuffle(k)
l = random.choice(k)
m_dict = {70: l, 26: l, 80: l, 3: l, 57: l, 10: l, 48: l, 35: l}
n_dict = {92: m_dict, 81: m_dict, 65: m_dict, 69: m_dict, 87: m_dict, 52: m_dict, 12: m_dict, 60: m_dict}
o = random.choice(list(n_dict.values()))
p = random.choice(list(o.values()))
q = ''
counterq = 0
while counterq < 2:
    r = ''
    counterr = 0
    while counterr < 5:
        r += q
        counterr += 1
        q += p
        counterq += 1
s_set = {r, r, r}
s = random.choice(list(s_set))
t = s + '.'
u = ''
for _ in range(6):
        if _ == 5:
            break
        u += t
v = u[0:]
w = (v, v, v)
x, y, z = w
aa = x + y + z
ab = (aa, aa, aa)
ac, ad, ae = ab
af = ac + ad + ae
ag_list = [af for _ in range(9)]
ah = random.choice(ag_list)
ai = ''
for _ in range(3):
    for __ in range(5):
                ai += ah
aj = (ai, ai, ai)
ak, al, am = aj
an = ak + al + am
ao = [an for _ in range(8)]
random.shuffle(ao)
ap = random.choice(ao)
aq = f'string {ap}'
print(aq)