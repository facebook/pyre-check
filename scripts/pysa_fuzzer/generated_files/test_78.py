import random
import math

a = input()
b = f'string {a}'
c = f'string {b}'
d = c + '.'
e = ''
countere = 0
while countere < 4:
    e += d
    countere += 1
f = e + '.'
g = f[0:]
h_dict = {55: g, 73: g, 55: g, 20: g, 99: g, 43: g}
i = random.choice(list(h_dict.values()))
j = [i for _ in range(5)]
random.shuffle(j)
k = random.choice(j)
l_dict = {73: k, 67: k, 9: k, 19: k, 45: k, 56: k}
m_dict = {68: l_dict, 62: l_dict, 57: l_dict, 68: l_dict, 92: l_dict, 13: l_dict, 31: l_dict, 28: l_dict, 68: l_dict, 20: l_dict}
n_dict = {27: m_dict, 16: m_dict, 99: m_dict, 36: m_dict, 28: m_dict, 16: m_dict, 63: m_dict, 32: m_dict}
o = random.choice(list(n_dict.values()))
p = random.choice(list(o.values()))
q = random.choice(list(p.values()))
def r():
    return q
def s():
    return r()
def t():
    return s()
u = t()
v = ''
counterv = 0
while counterv < 4:
    w = ''
    counterw = 0
    while counterw < 2:
        w += v
        counterw += 1
        v += u
        counterv += 1
x = [w for _ in range(8)]
random.shuffle(x)
y = random.choice(x)
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
ae = ''
for _ in range(2):
    af = ''
    for _ in range(5):
        ag = ''
        for _ in range(5):
            ag += af
            af += ae
        ae += ad
ah_dict = {71: ag, 49: ag, 57: ag, 21: ag, 99: ag, 11: ag, 1: ag}
ai = random.choice(list(ah_dict.values()))
aj_dict = {63: ai, 50: ai, 20: ai, 23: ai, 18: ai}
ak_dict = {55: aj_dict, 83: aj_dict, 66: aj_dict, 81: aj_dict}
al_dict = {21: ak_dict, 93: ak_dict, 91: ak_dict, 73: ak_dict}
am = random.choice(list(al_dict.values()))
an = random.choice(list(am.values()))
ao = random.choice(list(an.values()))
ap_list = [ao for _ in range(5)]
aq_list = [ap_list for _ in range(5)]
ar_list = [aq_list for _ in range(9)]
at = random.choice(ar_list)
au = random.choice(at)
av = random.choice(au)
aw = f'string {av}'
print(aw)