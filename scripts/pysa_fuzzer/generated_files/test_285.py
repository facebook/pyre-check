import random
import math

a = input()
b = ''
for _ in range(4):
    for __ in range(4):
                b += a
c = ''
counterc = 0
while counterc < 2:
    c += b
    counterc += 1
d = c[0:]
e = ''
countere = 0
while countere < 2:
    f = ''
    counterf = 0
    while counterf < 5:
        g = ''
        counterg = 0
        while counterg < 4:
            g += f
            counterg += 1
            f += e
            counterf += 1
        e += d
        countere += 1
h = ''
counterh = 0
while counterh < 5:
    i = ''
    counteri = 0
    while counteri < 3:
        i += h
        counteri += 1
        h += g
        counterh += 1
j = [i for _ in range(6)]
random.shuffle(j)
k = random.choice(j)
l_set = {k, k, k, k, k, k, k}
l = random.choice(list(l_set))
def m():
    return l
def n():
    return m()
def o():
    return n()
p = o()
q = p + '.'
r_list = [q for _ in range(10)]
s = random.choice(r_list)
t = ''
countert = 0
while countert < 4:
    u = ''
    counteru = 0
    while counteru < 5:
        v = ''
        counterv = 0
        while counterv < 4:
            v += u
            counterv += 1
            u += t
            counteru += 1
        t += s
        countert += 1
w = v + '.'
if w == w:
    z = w + 'c1'
elif w == '12':
    z = x + 'c2'
else:
    z = y + 'c3'
aa = ''
for _ in range(7):
        if _ == 3:
            continue
        aa += z
ab = f'string {aa}'
def ac():
    return ab
ad = ac()
def ae():
    return ad
af = ae()
ag = af + '.'
print(ag)