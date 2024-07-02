import random
import math
a = input()
b = ''
counterb = 0
while counterb < 3:
    c = ''
    counterc = 0
    while counterc < 3:
        d = ''
        counterd = 0
        while counterd < 5:
            d += c
            counterd += 1
            c += b
            counterc += 1
        b += a
        counterb += 1
e = d + '.'
f = ''
counterf = 0
while counterf < 4:
    g = ''
    counterg = 0
    while counterg < 5:
        h = ''
        counterh = 0
        while counterh < 3:
            h += g
            counterh += 1
            g += f
            counterg += 1
        f += e
        counterf += 1
i = [h for _ in range(10)]
random.shuffle(i)
j = random.choice(i)
def k():
    return j
def l():
    return k()
def m():
    return l()
n = m()
o_set = {n, n, n, n, n, n}
o = random.choice(list(o_set))
p = (o, o, o)
q, r, s = p
t = q + r + s
u = ''
for _ in range(3):
    v = ''
    for _ in range(5):
        v += u
        u += t
w = ''
for _ in range(3):
    for __ in range(4):
                w += v
x = w[0:]
y = x + '6'
if y == '5':
    z = y + ' c1'
elif y == '19':
    z = y + ' c2'
else:
    z = y + ' c3'
aa = ''
counteraa = 0
while counteraa < 4:
    aa += z
    counteraa += 1
ab = ''
for _ in range(4):
    for __ in range(4):
                ab += aa
ac = ''
for _ in range(6):
        if _ == 3:
            break
        ac += ab
ad = ''
for _ in range(6):
        if _ == 3:
            break
        ad += ac
ae = (ad, ad, ad)
af, ag, ah = ae
ai = af + ag + ah
aj = ''
for _ in range(5):
    ak = ''
    for _ in range(3):
        ak += aj
        aj += ai
print(ak)