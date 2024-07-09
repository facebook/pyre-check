import random
import math

a = input()
b = [a for _ in range(6)]
random.shuffle(b)
c = random.choice(b)
d = c + '.'
e = d[0:]
f = ''
for _ in range(5):
    for __ in range(2):
                f += e
g = ''
counterg = 0
while counterg < 5:
    g += f
    counterg += 1
h = g[0:]
i = ''
for _ in range(3):
    for __ in range(2):
                i += h
j = i + '.'
k = f'string {j}'
l = ''
for _ in range(5):
    for __ in range(4):
                l += k
m = ''
for _ in range(10):
        if _ == 2:
            continue
        m += l
n = m + '2'
o = n + '6'
p = o + '9'
q = ''
for _ in range(5):
    for __ in range(3):
                q += p
r = q[0:]
s = r + '.'
t = s + '.'
if t == t:
    w = t + 'c1'
elif t == '11':
    w = u + 'c2'
else:
    w = v + 'c3'
x = w[0:]
print(x)