import random
import math

a = input()
b = a + '.'
c_set = {b, b, b, b, b, b, b, b, b, b}
c = random.choice(list(c_set))
d = ''
for _ in range(6):
        if _ == 1:
            continue
        d += c
if d == d:
    g = d + 'c1'
elif d == '20':
    g = e + 'c2'
else:
    g = f + 'c3'
h = ''
for _ in range(8):
        if _ == 4:
            break
        h += g
i = h[0:]
j = ''
for _ in range(5):
    j += i
k = ''
for _ in range(5):
    for __ in range(2):
                k += j
if k == k:
    n = k + 'c1'
elif k == '12':
    n = l + 'c2'
else:
    n = m + 'c3'
o = ''
for _ in range(9):
        if _ == 2:
            break
        o += n
p = f'string {o}'
q = [p for _ in range(10)]
random.shuffle(q)
r = random.choice(q)
s = f'string {r}'
t_dict = {85: s, 80: s}
u = random.choice(list(t_dict.values()))
v = u + '6'
w = ''
for _ in range(5):
    x = ''
    for _ in range(4):
        x += w
        w += v
y = ''
for _ in range(2):
    y += x
z = y + '4'
aa = z + '2'
ab = aa + '5'
print(ab)