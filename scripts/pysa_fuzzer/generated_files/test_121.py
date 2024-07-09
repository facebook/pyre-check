import random
import math

a = input()
def b():
    return a
c = b()
d = ''
for _ in range(3):
    for __ in range(4):
                d += c
e = d + '.'
f = ''
counterf = 0
while counterf < 3:
    g = ''
    counterg = 0
    while counterg < 4:
        g += f
        counterg += 1
        f += e
        counterf += 1
h = ''
for _ in range(6):
        if _ == 4:
            break
        h += g
i = h + '3'
j = i + '2'
k_dict = {13: j, 10: j, 66: j, 19: j, 65: j, 63: j}
l_dict = {48: k_dict, 89: k_dict, 3: k_dict, 31: k_dict}
m = random.choice(list(l_dict.values()))
n = random.choice(list(m.values()))
o = [n for _ in range(10)]
random.shuffle(o)
p = random.choice(o)
q = p + '.'
r = q + '2'
s = ''
counters = 0
while counters < 5:
    t = ''
    countert = 0
    while countert < 3:
        t += s
        countert += 1
        s += r
        counters += 1
u = ''
for _ in range(3):
    v = ''
    for _ in range(5):
        w = ''
        for _ in range(3):
            w += v
            v += u
        u += t
x = ''
for _ in range(2):
    for __ in range(2):
                x += w
y = f'string {x}'
z = ''
counterz = 0
while counterz < 2:
    z += y
    counterz += 1
aa = ''
for _ in range(2):
    ab = ''
    for _ in range(2):
        ab += aa
        aa += z
ac = f'string {ab}'
ad = ''
for _ in range(2):
    ad += ac
print(ad)