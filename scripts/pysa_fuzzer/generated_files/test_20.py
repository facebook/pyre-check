import random
import math

a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = ''
for _ in range(4):
    for __ in range(2):
                g += f
h_set = {g, g, g, g, g, g, g, g}
h = random.choice(list(h_set))
i = f'string {h}'
j = i + '9'
k = j + '4'
l = k + '5'
m = f'string {l}'
n = ''
for _ in range(3):
    o = ''
    for _ in range(5):
        o += n
        n += m
p = ''
for _ in range(3):
    p += o
q = ''
counterq = 0
while counterq < 5:
    q += p
    counterq += 1
r = ''
for _ in range(3):
    for __ in range(2):
                r += q
s = r + '2'
t = s + '9'
u = ''
for _ in range(2):
    u += t
v = ''
counterv = 0
while counterv < 4:
    w = ''
    counterw = 0
    while counterw < 4:
        x = ''
        counterx = 0
        while counterx < 2:
            x += w
            counterx += 1
            w += v
            counterw += 1
        v += u
        counterv += 1
y = x + '.'
z = ''
for _ in range(2):
    for __ in range(2):
                z += y
aa = ''
for _ in range(5):
    for __ in range(4):
                aa += z
ab_set = {aa, aa, aa}
ab = random.choice(list(ab_set))
ac = ab + '2'
ad = ac + '1'
print(ad)