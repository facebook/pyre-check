import random
import math
a = input()
b_dict = {70: a, 55: a, 36: a, 15: a}
c = random.choice(list(b_dict.values()))
def d():
    return c
e = d()
f = ''
counterf = 0
while counterf < 5:
    g = ''
    counterg = 0
    while counterg < 5:
        h = ''
        counterh = 0
        while counterh < 4:
            h += g
            counterh += 1
            g += f
            counterg += 1
        f += e
        counterf += 1
if h == '6':
    i = h + ' c1'
elif h == '19':
    i = h + ' c2'
else:
    i = h + ' c3'
j = i + '.'
k = j[0:]
if k == '5':
    l = k + ' c1'
elif k == '13':
    l = k + ' c2'
else:
    l = k + ' c3'
m = ''
for _ in range(10):
        if _ == 1:
            continue
        m += l
n = f'string {m}'
o = f'string {n}'
p = ''
for _ in range(3):
    q = ''
    for _ in range(3):
        r = ''
        for _ in range(2):
            r += q
            q += p
        p += o
s = ''
for _ in range(3):
    for __ in range(2):
                s += r
t = s[0:]
u = ''
for _ in range(2):
    v = ''
    for _ in range(4):
        w = ''
        for _ in range(2):
            w += v
            v += u
        u += t
x = ''
counterx = 0
while counterx < 3:
    x += w
    counterx += 1
y = ''
countery = 0
while countery < 5:
    y += x
    countery += 1
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
ae = ad[0:]
print(ae)