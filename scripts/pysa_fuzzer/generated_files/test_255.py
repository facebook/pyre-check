import random
import math

a = input()
b = ''
for _ in range(3):
    b += a
c = ''
for _ in range(4):
    d = ''
    for _ in range(4):
        e = ''
        for _ in range(2):
            e += d
            d += c
        c += b
f_dict = {9: e, 99: e, 49: e, 78: e, 82: e, 69: e, 6: e, 66: e, 95: e, 93: e}
g_dict = {30: f_dict, 73: f_dict, 19: f_dict, 52: f_dict, 39: f_dict, 21: f_dict, 5: f_dict}
h = random.choice(list(g_dict.values()))
i = random.choice(list(h.values()))
j = i + '.'
k = ''
for _ in range(2):
    for __ in range(3):
                k += j
l = k[0:]
m_list = [l for _ in range(9)]
n_list = [m_list for _ in range(9)]
o_list = [n_list for _ in range(2)]
p = random.choice(o_list)
q = random.choice(p)
r = random.choice(q)
s = ''
for _ in range(5):
    for __ in range(5):
                s += r
if s == s:
    v = s + 'c1'
elif s == '19':
    v = t + 'c2'
else:
    v = u + 'c3'
w = ''
for _ in range(3):
    w += v
x = w[0:]
y = ''
for _ in range(9):
        if _ == 4:
            continue
        y += x
z = y + '5'
aa = z + '4'
ab = [aa for _ in range(7)]
random.shuffle(ab)
ac = random.choice(ab)
ad_dict = {40: ac, 21: ac, 40: ac, 58: ac, 37: ac, 25: ac, 75: ac, 79: ac}
ae_dict = {13: ad_dict, 38: ad_dict, 88: ad_dict, 52: ad_dict}
af_dict = {15: ae_dict, 56: ae_dict, 48: ae_dict, 99: ae_dict, 94: ae_dict, 30: ae_dict, 56: ae_dict, 38: ae_dict}
ag = random.choice(list(af_dict.values()))
ah = random.choice(list(ag.values()))
ai = random.choice(list(ah.values()))
aj = ''
for _ in range(3):
    for __ in range(3):
                aj += ai
ak = aj + '9'
al = ak + '8'
am = al + '3'
def an():
    return am
ao = an()
print(ao)