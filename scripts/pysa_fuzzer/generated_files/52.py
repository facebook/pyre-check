import random
import math
a = input()
b = [a for _ in range(6)]
random.shuffle(b)
c = random.choice(b)
d = c[0:]
e = d[0:]
if e == '8':
    f = e + ' c1'
elif e == '14':
    f = e + ' c2'
else:
    f = e + ' c3'
g = f[0:]
h = g + '.'
i = (h, h, h)
j, k, l = i
m = j + k + l
n = ''
for _ in range(5):
    for __ in range(5):
                n += m
o = ''
countero = 0
while countero < 5:
    o += n
    countero += 1
if o == '10':
    p = o + ' c1'
elif o == '13':
    p = o + ' c2'
else:
    p = o + ' c3'
def q():
    return p
def r():
    return q()
def s():
    return r()
t = s()
u = t + '3'
v = u + '9'
w = v + '2'
x = ''
counterx = 0
while counterx < 3:
    y = ''
    countery = 0
    while countery < 2:
        z = ''
        counterz = 0
        while counterz < 5:
            z += y
            counterz += 1
            y += x
            countery += 1
        x += w
        counterx += 1
aa_set = {z, z, z, z, z, z, z, z, z, z}
aa = random.choice(list(aa_set))
ab = f'string {aa}'
if ab == '2':
    ac = ab + ' c1'
elif ab == '14':
    ac = ab + ' c2'
else:
    ac = ab + ' c3'
ad = ''
for _ in range(9):
        if _ == 2:
            continue
        ad += ac
ae = (ad, ad, ad)
af, ag, ah = ae
ai = af + ag + ah
print(ai)