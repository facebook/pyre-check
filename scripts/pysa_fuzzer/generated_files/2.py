import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = ''
counterg = 0
while counterg < 5:
    h = ''
    counterh = 0
    while counterh < 5:
        h += g
        counterh += 1
        g += f
        counterg += 1
i_set = {h, h, h, h, h, h}
i = random.choice(list(i_set))
j = ''
for _ in range(5):
        if _ == 3:
            break
        j += i
k = f'string {j}'
l_dict = {7: k, 62: k, 94: k, 9: k}
m = random.choice(list(l_dict.values()))
n = ''
for _ in range(5):
    for __ in range(4):
                n += m
o = n[0:]
p = o[0:]
q = ''
for _ in range(6):
        if _ == 3:
            continue
        q += p
if q == '1':
    r = q + ' c1'
elif q == '16':
    r = q + ' c2'
else:
    r = q + ' c3'
s = ''
counters = 0
while counters < 3:
    t = ''
    countert = 0
    while countert < 2:
        u = ''
        counteru = 0
        while counteru < 3:
            u += t
            counteru += 1
            t += s
            countert += 1
        s += r
        counters += 1
v_dict = {4: u, 89: u, 61: u, 46: u, 75: u, 19: u, 45: u, 91: u, 80: u}
w = random.choice(list(v_dict.values()))
x = w[0:]
y = (x, x, x)
z, aa, ab = y
ac = z + aa + ab
if ac == '10':
    ad = ac + ' c1'
elif ac == '12':
    ad = ac + ' c2'
else:
    ad = ac + ' c3'
ae_dict = {2: ad, 75: ad, 98: ad}
af_dict = {35: ae_dict, 78: ae_dict, 80: ae_dict, 18: ae_dict, 50: ae_dict, 35: ae_dict}
ag = random.choice(list(af_dict.values()))
ah = random.choice(list(ag.values()))
ai = ah[0:]
print(ai)