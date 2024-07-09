import random
import math

a = input()
b = ''
for _ in range(4):
    c = ''
    for _ in range(4):
        c += b
        b += a
d = ''
for _ in range(4):
    for __ in range(2):
                d += c
if d == d:
    g = d + 'c1'
elif d == '11':
    g = e + 'c2'
else:
    g = f + 'c3'
h = g + '.'
i = f'string {h}'
j = f'string {i}'
k = j + '.'
l = f'string {k}'
m = ''
for _ in range(5):
        if _ == 5:
            break
        m += l
n = [m for _ in range(5)]
random.shuffle(n)
o = random.choice(n)
p = ''
for _ in range(8):
        if _ == 3:
            break
        p += o
if p == p:
    s = p + 'c1'
elif p == '18':
    s = q + 'c2'
else:
    s = r + 'c3'
t = [s for _ in range(9)]
random.shuffle(t)
u = random.choice(t)
v = (u, u, u)
w, x, y = v
z = w + x + y
aa = z + '3'
ab = ''
counterab = 0
while counterab < 3:
    ac = ''
    counterac = 0
    while counterac < 5:
        ac += ab
        counterac += 1
        ab += aa
        counterab += 1
ad = ''
counterad = 0
while counterad < 4:
    ad += ac
    counterad += 1
def ae():
    return ad
af = ae()
print(af)