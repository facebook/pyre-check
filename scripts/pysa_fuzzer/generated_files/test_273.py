import random
import math

a = input()
b = ''
for _ in range(5):
    c = ''
    for _ in range(4):
        d = ''
        for _ in range(4):
            d += c
            c += b
        b += a
e = d[0:]
f_list = [e for _ in range(3)]
g_list = [f_list for _ in range(6)]
h_list = [g_list for _ in range(8)]
i = random.choice(h_list)
j = random.choice(i)
k = random.choice(j)
l = ''
for _ in range(4):
    m = ''
    for _ in range(5):
        m += l
        l += k
n_list = [m for _ in range(6)]
o = random.choice(n_list)
p_dict = {80: o, 34: o, 87: o, 77: o, 1: o}
q_dict = {54: p_dict, 26: p_dict, 92: p_dict, 62: p_dict}
r = random.choice(list(q_dict.values()))
s = random.choice(list(r.values()))
if s == s:
    v = s + 'c1'
elif s == '18':
    v = t + 'c2'
else:
    v = u + 'c3'
w = v + '.'
x = ''
for _ in range(8):
        if _ == 2:
            break
        x += w
y = ''
countery = 0
while countery < 4:
    z = ''
    counterz = 0
    while counterz < 4:
        aa = ''
        counteraa = 0
        while counteraa < 3:
            aa += z
            counteraa += 1
            z += y
            counterz += 1
        y += x
        countery += 1
ab = aa[0:]
ac = ab + '.'
ad_set = {ac, ac, ac, ac, ac, ac}
ad = random.choice(list(ad_set))
ae_set = {ad, ad, ad, ad, ad, ad}
ae = random.choice(list(ae_set))
af = ae + '4'
ag = af + '6'
ah = ag + '3'
ai = ''
for _ in range(2):
    aj = ''
    for _ in range(5):
        ak = ''
        for _ in range(3):
            ak += aj
            aj += ai
        ai += ah
al = ak + '.'
am_set = {al, al, al}
am = random.choice(list(am_set))
print(am)