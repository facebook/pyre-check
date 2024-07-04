import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = ''
for _ in range(3):
    for __ in range(4):
                g += f
h = f'string {g}'
i = [h for _ in range(8)]
random.shuffle(i)
j = random.choice(i)
k = ''
for _ in range(2):
    k += j
if k == '3':
    l = k + ' c1'
elif k == '20':
    l = k + ' c2'
else:
    l = k + ' c3'
m_dict = {2: l, 72: l, 24: l, 55: l, 14: l}
n = random.choice(list(m_dict.values()))
o_set = {n, n, n, n, n, n, n, n, n}
o = random.choice(list(o_set))
p = (o, o, o)
q, r, s = p
t = q + r + s
u_set = {t, t, t, t, t, t, t, t, t}
u = random.choice(list(u_set))
v = u + '.'
w = (v, v, v)
x, y, z = w
aa = x + y + z
ab = f'string {aa}'
ac = f'string {ab}'
if ac == '7':
    ad = ac + ' c1'
elif ac == '17':
    ad = ac + ' c2'
else:
    ad = ac + ' c3'
ae = ''
counterae = 0
while counterae < 3:
    ae += ad
    counterae += 1
af_list = [ae for _ in range(6)]
ag_list = [af_list for _ in range(8)]
ah = random.choice(ag_list)
ai = random.choice(ah)
aj_dict = {48: ai, 84: ai, 78: ai, 44: ai, 4: ai, 88: ai, 56: ai, 50: ai, 55: ai}
ak_dict = {79: aj_dict, 7: aj_dict, 54: aj_dict, 64: aj_dict, 6: aj_dict, 41: aj_dict, 48: aj_dict, 93: aj_dict, 56: aj_dict, 84: aj_dict}
al_dict = {17: ak_dict, 17: ak_dict, 27: ak_dict, 44: ak_dict, 49: ak_dict, 54: ak_dict, 56: ak_dict}
am = random.choice(list(al_dict.values()))
an = random.choice(list(am.values()))
ao = random.choice(list(an.values()))
print(ao)