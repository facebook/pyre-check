import random
import math

a = input()
b = a + '2'
c = b + '6'
d = c + '7'
e = d + '7'
f = ''
for _ in range(4):
    g = ''
    for _ in range(3):
        h = ''
        for _ in range(3):
            h += g
            g += f
        f += e
i = ''
for _ in range(6):
        if _ == 2:
            continue
        i += h
j = i[0:]
if j == j:
    m = j + 'c1'
elif j == '12':
    m = k + 'c2'
else:
    m = l + 'c3'
n = m + '.'
if n == n:
    q = n + 'c1'
elif n == '14':
    q = o + 'c2'
else:
    q = p + 'c3'
if q == q:
    t = q + 'c1'
elif q == '16':
    t = r + 'c2'
else:
    t = s + 'c3'
def u():
    return t
v = u()
def w():
    return v
x = w()
y = ''
for _ in range(6):
        if _ == 4:
            continue
        y += x
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
ae = [ad for _ in range(9)]
random.shuffle(ae)
af = random.choice(ae)
ag = f'string {af}'
ah = f'string {ag}'
ai = [ah for _ in range(7)]
random.shuffle(ai)
aj = random.choice(ai)
ak = aj + '.'
print(ak)