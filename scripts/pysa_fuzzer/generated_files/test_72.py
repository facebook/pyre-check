import random
import math

a = input()
b = a + '8'
c = b + '8'
d = c + '6'
e = ''
for _ in range(5):
        if _ == 2:
            continue
        e += d
f = ''
for _ in range(10):
        if _ == 4:
            continue
        f += e
g_dict = {20: f, 11: f, 80: f, 62: f, 51: f}
h_dict = {33: g_dict, 50: g_dict, 58: g_dict, 57: g_dict, 40: g_dict, 30: g_dict}
i = random.choice(list(h_dict.values()))
j = random.choice(list(i.values()))
k_list = [j for _ in range(5)]
l_list = [k_list for _ in range(9)]
m_list = [l_list for _ in range(6)]
n = random.choice(m_list)
o = random.choice(n)
p = random.choice(o)
q = p + '.'
r = q + '.'
s = r + '.'
t = s + '.'
def u():
    return t
v = u()
if v == v:
    y = v + 'c1'
elif v == '16':
    y = w + 'c2'
else:
    y = x + 'c3'
z = [y for _ in range(5)]
random.shuffle(z)
aa = random.choice(z)
ab_dict = {92: aa, 15: aa}
ac_dict = {86: ab_dict, 29: ab_dict}
ad = random.choice(list(ac_dict.values()))
ae = random.choice(list(ad.values()))
af = ''
for _ in range(5):
        if _ == 2:
            continue
        af += ae
ag_list = [af for _ in range(7)]
ah_list = [ag_list for _ in range(4)]
ai = random.choice(ah_list)
aj = random.choice(ai)
ak = f'string {aj}'
al = (ak, ak, ak)
am, an, ao = al
ap = am + an + ao
aq = f'string {ap}'
print(aq)