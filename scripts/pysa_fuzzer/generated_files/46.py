import random
import math
a = input()
b_set = {a, a, a, a, a, a, a, a, a, a}
b = random.choice(list(b_set))
c = b[0:]
d = ''
for _ in range(2):
    for __ in range(2):
                d += c
if d == '2':
    e = d + ' c1'
elif d == '17':
    e = d + ' c2'
else:
    e = d + ' c3'
f = f'string {e}'
g = f[0:]
h = ''
for _ in range(5):
        if _ == 2:
            break
        h += g
i = h + '.'
j = f'string {i}'
k = f'string {j}'
l = (k, k, k)
m, n, o = l
p = m + n + o
q = ''
for _ in range(2):
    for __ in range(4):
                q += p
r = f'string {q}'
s = (r, r, r)
t, u, v = s
w = t + u + v
x = w + '.'
if x == '1':
    y = x + ' c1'
elif x == '11':
    y = x + ' c2'
else:
    y = x + ' c3'
z_set = {y, y, y, y, y, y}
z = random.choice(list(z_set))
aa = z + '8'
print(aa)