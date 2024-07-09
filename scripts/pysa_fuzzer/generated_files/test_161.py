import random
import math

a = input()
b = ''
for _ in range(10):
        if _ == 3:
            continue
        b += a
c_set = {b, b, b, b, b, b, b}
c = random.choice(list(c_set))
d = ''
for _ in range(5):
    e = ''
    for _ in range(5):
        e += d
        d += c
f = e + '.'
g = ''
for _ in range(2):
    h = ''
    for _ in range(5):
        h += g
        g += f
i = h + '8'
j = i + '6'
k_dict = {21: j, 56: j, 57: j, 88: j, 2: j}
l_dict = {30: k_dict, 86: k_dict, 6: k_dict, 88: k_dict, 16: k_dict, 88: k_dict, 8: k_dict, 88: k_dict}
m = random.choice(list(l_dict.values()))
n = random.choice(list(m.values()))
o = ''
for _ in range(9):
        if _ == 2:
            break
        o += n
p = ''
for _ in range(4):
    for __ in range(5):
                p += o
q = ''
counterq = 0
while counterq < 5:
    q += p
    counterq += 1
if q == q:
    t = q + 'c1'
elif q == '12':
    t = r + 'c2'
else:
    t = s + 'c3'
if t == t:
    w = t + 'c1'
elif t == '19':
    w = u + 'c2'
else:
    w = v + 'c3'
x = ''
for _ in range(6):
        if _ == 5:
            continue
        x += w
y = ''
countery = 0
while countery < 2:
    z = ''
    counterz = 0
    while counterz < 3:
        z += y
        counterz += 1
        y += x
        countery += 1
aa = ''
for _ in range(3):
    for __ in range(5):
                aa += z
def ab():
    return aa
def ac():
    return ab()
ad = ac()
ae = f'string {ad}'
af = ae + '8'
ag = af + '5'
ah = ag + '5'
print(ah)