import random
import math
a = input()
b = ''
for _ in range(4):
    b += a
c = ''
counterc = 0
while counterc < 4:
    d = ''
    counterd = 0
    while counterd < 5:
        d += c
        counterd += 1
        c += b
        counterc += 1
e = (d, d, d)
f, g, h = e
i = f + g + h
j = ''
counterj = 0
while counterj < 5:
    j += i
    counterj += 1
k = (j, j, j)
l, m, n = k
o = l + m + n
p = ''
for _ in range(4):
    for __ in range(5):
                p += o
q = ''
for _ in range(8):
        if _ == 3:
            continue
        q += p
r_set = {q, q, q, q, q, q, q, q, q}
r = random.choice(list(r_set))
s = r + '.'
t_set = {s, s, s, s, s}
t = random.choice(list(t_set))
u = t + '9'
v = u + '6'
w = v + '1'
x = f'string {w}'
y_dict = {9: x, 66: x, 14: x, 29: x, 43: x, 86: x, 47: x, 50: x, 99: x, 87: x}
z = random.choice(list(y_dict.values()))
aa = ''
for _ in range(5):
    for __ in range(3):
                aa += z
ab = aa[0:]
if ab == '6':
    ac = ab + ' c1'
elif ab == '14':
    ac = ab + ' c2'
else:
    ac = ab + ' c3'
ad = ac[0:]
if ad == '6':
    ae = ad + ' c1'
elif ad == '11':
    ae = ad + ' c2'
else:
    ae = ad + ' c3'
print(ae)