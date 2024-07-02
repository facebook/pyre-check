import random
import math
a = input()
b = ''
for _ in range(2):
    c = ''
    for _ in range(4):
        d = ''
        for _ in range(5):
            d += c
            c += b
        b += a
e = ''
for _ in range(2):
    for __ in range(3):
                e += d
f_set = {e, e, e, e, e, e}
f = random.choice(list(f_set))
g_list = [f for _ in range(9)]
h_list = [g_list for _ in range(7)]
i = random.choice(h_list)
j = random.choice(i)
k_list = [j for _ in range(8)]
l_list = [k_list for _ in range(9)]
m = random.choice(l_list)
n = random.choice(m)
o = ''
for _ in range(5):
        if _ == 1:
            break
        o += n
if o == '10':
    p = o + ' c1'
elif o == '12':
    p = o + ' c2'
else:
    p = o + ' c3'
q = [p for _ in range(6)]
random.shuffle(q)
r = random.choice(q)
s = (r, r, r)
t, u, v = s
w = t + u + v
x = ''
for _ in range(10):
        if _ == 2:
            continue
        x += w
y = f'string {x}'
z = ''
for _ in range(2):
    for __ in range(5):
                z += y
aa = z + '4'
ab = aa + '3'
if ab == '1':
    ac = ab + ' c1'
elif ab == '14':
    ac = ab + ' c2'
else:
    ac = ab + ' c3'
ad = ac + '9'
ae = ''
for _ in range(2):
    for __ in range(5):
                ae += ad
af = ''
for _ in range(4):
    ag = ''
    for _ in range(3):
        ag += af
        af += ae
ah_list = [ag for _ in range(8)]
ai_list = [ah_list for _ in range(3)]
aj = random.choice(ai_list)
ak = random.choice(aj)
print(ak)