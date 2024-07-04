import random
import math
a = input()
b = ''
for _ in range(6):
        if _ == 1:
            continue
        b += a
c = (b, b, b)
d, e, f = c
g = d + e + f
h = f'string {g}'
i = h[0:]
j = i + '4'
k_dict = {38: j, 63: j, 66: j, 95: j, 25: j, 52: j, 35: j}
l = random.choice(list(k_dict.values()))
m = ''
for _ in range(3):
    n = ''
    for _ in range(4):
        o = ''
        for _ in range(2):
            o += n
            n += m
        m += l
p = ''
counterp = 0
while counterp < 3:
    q = ''
    counterq = 0
    while counterq < 4:
        q += p
        counterq += 1
        p += o
        counterp += 1
r_set = {q, q, q, q, q, q, q}
r = random.choice(list(r_set))
s = ''
for _ in range(5):
    for __ in range(3):
                s += r
t = s + '.'
u_list = [t for _ in range(2)]
v = random.choice(u_list)
w = ''
counterw = 0
while counterw < 4:
    x = ''
    counterx = 0
    while counterx < 4:
        y = ''
        countery = 0
        while countery < 2:
            y += x
            countery += 1
            x += w
            counterx += 1
        w += v
        counterw += 1
z = y + '.'
if z == '7':
    aa = z + ' c1'
elif z == '11':
    aa = z + ' c2'
else:
    aa = z + ' c3'
ab_dict = {76: aa, 27: aa}
ac = random.choice(list(ab_dict.values()))
ad = ac + '.'
ae = ad + '9'
af = ae + '2'
ag = af + '5'
print(ag)