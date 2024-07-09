import random
import math

a = input()
b_set = {a, a, a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c = ''
for _ in range(9):
        if _ == 3:
            continue
        c += b
d_dict = {45: c, 93: c, 34: c, 35: c, 61: c, 3: c, 94: c, 36: c, 48: c, 7: c}
e = random.choice(list(d_dict.values()))
f_dict = {47: e, 23: e, 49: e}
g_dict = {13: f_dict, 23: f_dict, 86: f_dict, 81: f_dict, 12: f_dict, 8: f_dict, 90: f_dict, 19: f_dict}
h = random.choice(list(g_dict.values()))
i = random.choice(list(h.values()))
j = ''
for _ in range(5):
        if _ == 5:
            continue
        j += i
k = j + '.'
l = ''
counterl = 0
while counterl < 4:
    m = ''
    counterm = 0
    while counterm < 3:
        m += l
        counterm += 1
        l += k
        counterl += 1
n = (m, m, m)
o, p, q = n
r = o + p + q
s = f'string {r}'
def t():
    return s
def u():
    return t()
def v():
    return u()
w = v()
x_set = {w, w, w, w}
x = random.choice(list(x_set))
y = f'string {x}'
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
ae = ''
for _ in range(5):
        if _ == 3:
            continue
        ae += ad
af = ''
for _ in range(2):
    ag = ''
    for _ in range(3):
        ag += af
        af += ae
ah_set = {ag, ag, ag, ag, ag, ag}
ah = random.choice(list(ah_set))
ai = (ah, ah, ah)
aj, ak, al = ai
am = aj + ak + al
an_dict = {7: am, 3: am, 11: am}
ao_dict = {73: an_dict, 81: an_dict, 37: an_dict, 51: an_dict, 88: an_dict, 16: an_dict, 50: an_dict, 15: an_dict, 87: an_dict, 89: an_dict}
ap = random.choice(list(ao_dict.values()))
aq = random.choice(list(ap.values()))
print(aq)