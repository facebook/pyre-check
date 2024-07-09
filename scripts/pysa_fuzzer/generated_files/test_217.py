import random
import math

a = input()
b = ''
for _ in range(4):
    c = ''
    for _ in range(4):
        c += b
        b += a
d = c + '1'
e = d + '3'
f = (e, e, e)
g, h, i = f
j = g + h + i
k = ''
for _ in range(5):
        if _ == 1:
            break
        k += j
l = k[0:]
m = ''
for _ in range(2):
    for __ in range(5):
                m += l
n = m[0:]
o_set = {n, n, n, n}
o = random.choice(list(o_set))
p = ''
for _ in range(10):
        if _ == 5:
            break
        p += o
q = p + '8'
r = q + '7'
s = r + '4'
t = (s, s, s)
u, v, w = t
x = u + v + w
y_set = {x, x, x, x, x}
y = random.choice(list(y_set))
z = ''
for _ in range(10):
        if _ == 1:
            break
        z += y
aa = ''
for _ in range(10):
        if _ == 5:
            continue
        aa += z
ab = f'string {aa}'
ac = ab + '5'
ad = ac + '1'
ae = ''
counterae = 0
while counterae < 4:
    af = ''
    counteraf = 0
    while counteraf < 4:
        af += ae
        counteraf += 1
        ae += ad
        counterae += 1
ag = af + '1'
ah = ag + '6'
ai = ah + '9'
print(ai)