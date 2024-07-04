import random
import math
a = input()
b = a[0:]
c = ''
for _ in range(9):
        if _ == 3:
            break
        c += b
d = ''
for _ in range(10):
        if _ == 1:
            break
        d += c
e = d[0:]
if e == '4':
    f = e + ' c1'
elif e == '14':
    f = e + ' c2'
else:
    f = e + ' c3'
g = ''
counterg = 0
while counterg < 3:
    g += f
    counterg += 1
h = [g for _ in range(8)]
random.shuffle(h)
i = random.choice(h)
j_dict = {37: i, 78: i, 72: i, 10: i, 53: i, 50: i, 6: i, 64: i, 1: i}
k_dict = {83: j_dict, 43: j_dict, 78: j_dict}
l = random.choice(list(k_dict.values()))
m = random.choice(list(l.values()))
n = (m, m, m)
o, p, q = n
r = o + p + q
s = ''
for _ in range(4):
    for __ in range(4):
                s += r
if s == '10':
    t = s + ' c1'
elif s == '18':
    t = s + ' c2'
else:
    t = s + ' c3'
if t == '5':
    u = t + ' c1'
elif t == '17':
    u = t + ' c2'
else:
    u = t + ' c3'
v = u + '.'
w = v[0:]
x = ''
counterx = 0
while counterx < 2:
    x += w
    counterx += 1
y = f'string {x}'
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
ae = ''
for _ in range(2):
    for __ in range(4):
                ae += ad
print(ae)