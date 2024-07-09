import random
import math

a = input()
if a == a:
    d = a + 'c1'
elif a == '17':
    d = b + 'c2'
else:
    d = c + 'c3'
e = ''
for _ in range(2):
    for __ in range(2):
                e += d
f = ''
for _ in range(5):
    for __ in range(3):
                f += e
g = ''
for _ in range(4):
    for __ in range(4):
                g += f
h = f'string {g}'
if h == h:
    k = h + 'c1'
elif h == '15':
    k = i + 'c2'
else:
    k = j + 'c3'
l = [k for _ in range(10)]
random.shuffle(l)
m = random.choice(l)
n = ''
for _ in range(5):
    o = ''
    for _ in range(2):
        p = ''
        for _ in range(3):
            p += o
            o += n
        n += m
q = ''
for _ in range(3):
    r = ''
    for _ in range(4):
        r += q
        q += p
s = r + '.'
t = s[0:]
u = [t for _ in range(7)]
random.shuffle(u)
v = random.choice(u)
w = v + '.'
x = [w for _ in range(5)]
random.shuffle(x)
y = random.choice(x)
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
ae = [ad for _ in range(9)]
random.shuffle(ae)
af = random.choice(ae)
ag = ''
counterag = 0
while counterag < 5:
    ah = ''
    counterah = 0
    while counterah < 3:
        ai = ''
        counterai = 0
        while counterai < 5:
            ai += ah
            counterai += 1
            ah += ag
            counterah += 1
        ag += af
        counterag += 1
aj = ai + '3'
ak = aj + '9'
print(ak)