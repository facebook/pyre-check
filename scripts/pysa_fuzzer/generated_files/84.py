import random
import math
a = input()
b_dict = {24: a, 1: a}
c_dict = {77: b_dict, 70: b_dict, 13: b_dict, 25: b_dict, 27: b_dict, 30: b_dict, 55: b_dict, 92: b_dict, 60: b_dict}
d_dict = {99: c_dict, 29: c_dict, 92: c_dict, 79: c_dict, 28: c_dict, 90: c_dict, 54: c_dict, 82: c_dict}
e = random.choice(list(d_dict.values()))
f = random.choice(list(e.values()))
g = random.choice(list(f.values()))
h_list = [g for _ in range(8)]
i = random.choice(h_list)
j_list = [i for _ in range(10)]
k_list = [j_list for _ in range(7)]
l = random.choice(k_list)
m = random.choice(l)
if m == '10':
    n = m + ' c1'
elif m == '20':
    n = m + ' c2'
else:
    n = m + ' c3'
o_set = {n, n, n, n, n, n}
o = random.choice(list(o_set))
if o == '10':
    p = o + ' c1'
elif o == '16':
    p = o + ' c2'
else:
    p = o + ' c3'
q = ''
for _ in range(10):
        if _ == 3:
            break
        q += p
r = (q, q, q)
s, t, u = r
v = s + t + u
w = v + '6'
x = w + '6'
y = x + '1'
def z():
    return y
def aa():
    return z()
def ab():
    return aa()
ac = ab()
ad = ''
counterad = 0
while counterad < 4:
    ad += ac
    counterad += 1
ae = (ad, ad, ad)
af, ag, ah = ae
ai = af + ag + ah
aj = ai + '.'
if aj == '7':
    ak = aj + ' c1'
elif aj == '18':
    ak = aj + ' c2'
else:
    ak = aj + ' c3'
al = (ak, ak, ak)
am, an, ao = al
ap = am + an + ao
aq = ap[0:]
ar_dict = {46: aq, 19: aq, 10: aq, 76: aq, 85: aq, 14: aq}
at_dict = {20: ar_dict, 79: ar_dict, 41: ar_dict}
au_dict = {66: at_dict, 93: at_dict, 94: at_dict, 25: at_dict, 8: at_dict, 29: at_dict}
av = random.choice(list(au_dict.values()))
aw = random.choice(list(av.values()))
ax = random.choice(list(aw.values()))
ay = ''
for _ in range(5):
        if _ == 2:
            continue
        ay += ax
print(ay)