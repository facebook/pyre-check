import random
import math

a = input()
b = ''
for _ in range(4):
    b += a
c = ''
for _ in range(5):
    d = ''
    for _ in range(2):
        e = ''
        for _ in range(2):
            e += d
            d += c
        c += b
f = ''
for _ in range(7):
        if _ == 1:
            break
        f += e
if f == f:
    i = f + 'c1'
elif f == '20':
    i = g + 'c2'
else:
    i = h + 'c3'
j_dict = {88: i, 94: i, 25: i, 5: i, 75: i, 31: i, 44: i, 43: i, 7: i}
k_dict = {67: j_dict, 87: j_dict, 55: j_dict, 5: j_dict}
l_dict = {60: k_dict, 65: k_dict, 39: k_dict, 35: k_dict}
m = random.choice(list(l_dict.values()))
n = random.choice(list(m.values()))
o = random.choice(list(n.values()))
p = ''
for _ in range(4):
    for __ in range(2):
                p += o
q = ''
for _ in range(7):
        if _ == 2:
            continue
        q += p
r = q + '9'
s = r + '9'
t = s + '9'
u = [t for _ in range(7)]
random.shuffle(u)
v = random.choice(u)
w = v[0:]
x_dict = {38: w, 48: w, 1: w, 72: w, 28: w, 32: w, 25: w, 82: w, 4: w}
y_dict = {9: x_dict, 57: x_dict, 55: x_dict, 1: x_dict, 56: x_dict, 66: x_dict, 100: x_dict, 5: x_dict, 29: x_dict, 36: x_dict}
z = random.choice(list(y_dict.values()))
aa = random.choice(list(z.values()))
ab = f'string {aa}'
if ab == ab:
    ae = ab + 'c1'
elif ab == '18':
    ae = ac + 'c2'
else:
    ae = ad + 'c3'
af = ''
for _ in range(2):
    ag = ''
    for _ in range(2):
        ah = ''
        for _ in range(3):
            ah += ag
            ag += af
        af += ae
ai = (ah, ah, ah)
aj, ak, al = ai
am = aj + ak + al
if am == am:
    ap = am + 'c1'
elif am == '15':
    ap = an + 'c2'
else:
    ap = ao + 'c3'
aq = ''
counteraq = 0
while counteraq < 5:
    ar = ''
    counterar = 0
    while counterar < 2:
        ar += aq
        counterar += 1
        aq += ap
        counteraq += 1
if ar == ar:
    av = ar + 'c1'
elif ar == '13':
    av = at + 'c2'
else:
    av = au + 'c3'
print(av)