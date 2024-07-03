import random
import math
a = input()
def b():
    return a
def c():
    return b()
def d():
    return c()
e = d()
f = f'string {e}'
g = f + '7'
if g == '5':
    h = g + ' c1'
elif g == '20':
    h = g + ' c2'
else:
    h = g + ' c3'
if h == '8':
    i = h + ' c1'
elif h == '14':
    i = h + ' c2'
else:
    i = h + ' c3'
j = ''
for _ in range(4):
    j += i
k = j[0:]
def l():
    return k
def m():
    return l()
def n():
    return m()
o = n()
p = [o for _ in range(8)]
random.shuffle(p)
q = random.choice(p)
r = ''
for _ in range(8):
        if _ == 4:
            continue
        r += q
s = f'string {r}'
t = s + '4'
u = t + '1'
v = u + '9'
w = ''
for _ in range(3):
    for __ in range(2):
                w += v
x = ''
counterx = 0
while counterx < 3:
    y = ''
    countery = 0
    while countery < 3:
        z = ''
        counterz = 0
        while counterz < 4:
            z += y
            counterz += 1
            y += x
            countery += 1
        x += w
        counterx += 1
aa = (z, z, z)
ab, ac, ad = aa
ae = ab + ac + ad
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
ak = aj + '.'
al = ak[0:]
print(al)