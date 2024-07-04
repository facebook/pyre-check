import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = ''
for _ in range(6):
        if _ == 4:
            continue
        g += f
h = (g, g, g)
i, j, k = h
l = i + j + k
m = ''
for _ in range(10):
        if _ == 5:
            break
        m += l
n = ''
for _ in range(3):
    n += m
o = ''
for _ in range(3):
    p = ''
    for _ in range(5):
        p += o
        o += n
q_dict = {11: p, 42: p, 15: p, 32: p, 37: p, 82: p, 1: p}
r_dict = {55: q_dict, 39: q_dict, 63: q_dict, 2: q_dict, 95: q_dict}
s = random.choice(list(r_dict.values()))
t = random.choice(list(s.values()))
u = ''
for _ in range(8):
        if _ == 4:
            break
        u += t
v = ''
for _ in range(6):
        if _ == 1:
            continue
        v += u
w = v[0:]
x = (w, w, w)
y, z, aa = x
ab = y + z + aa
ac = ab + '.'
ad_list = [ac for _ in range(5)]
ae = random.choice(ad_list)
af = ae[0:]
ag = f'string {af}'
ah_set = {ag, ag, ag, ag, ag, ag, ag, ag}
ah = random.choice(list(ah_set))
if ah == '5':
    ai = ah + ' c1'
elif ah == '19':
    ai = ah + ' c2'
else:
    ai = ah + ' c3'
aj_dict = {63: ai, 30: ai, 38: ai, 77: ai, 58: ai, 75: ai, 18: ai, 80: ai, 79: ai}
ak_dict = {89: aj_dict, 52: aj_dict, 42: aj_dict, 53: aj_dict, 56: aj_dict, 22: aj_dict, 87: aj_dict, 41: aj_dict, 37: aj_dict}
al = random.choice(list(ak_dict.values()))
am = random.choice(list(al.values()))
print(am)