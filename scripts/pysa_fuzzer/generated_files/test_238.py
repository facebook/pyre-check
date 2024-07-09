import random
import math

a = input()
b = ''
counterb = 0
while counterb < 4:
    b += a
    counterb += 1
c = b[0:]
d = ''
for _ in range(2):
    d += c
e = d + '.'
f = ''
for _ in range(3):
    for __ in range(2):
                f += e
g = ''
for _ in range(5):
    h = ''
    for _ in range(2):
        i = ''
        for _ in range(2):
            i += h
            h += g
        g += f
j = i[0:]
k_set = {j, j, j, j}
k = random.choice(list(k_set))
if k == k:
    n = k + 'c1'
elif k == '17':
    n = l + 'c2'
else:
    n = m + 'c3'
o = n[0:]
def p():
    return o
def q():
    return p()
r = q()
s = r[0:]
t = ''
for _ in range(2):
    for __ in range(5):
                t += s
u = ''
for _ in range(5):
        if _ == 2:
            continue
        u += t
v = [u for _ in range(10)]
random.shuffle(v)
w = random.choice(v)
x = ''
for _ in range(2):
    for __ in range(4):
                x += w
if x == x:
    aa = x + 'c1'
elif x == '11':
    aa = y + 'c2'
else:
    aa = z + 'c3'
ab = aa + '.'
print(ab)