import random
import math
a = input()
if a == '7':
    b = a + ' c1'
elif a == '11':
    b = a + ' c2'
else:
    b = a + ' c3'
c = ''
counterc = 0
while counterc < 2:
    d = ''
    counterd = 0
    while counterd < 3:
        e = ''
        countere = 0
        while countere < 4:
            e += d
            countere += 1
            d += c
            counterd += 1
        c += b
        counterc += 1
f = e + '.'
g = ''
counterg = 0
while counterg < 2:
    h = ''
    counterh = 0
    while counterh < 5:
        i = ''
        counteri = 0
        while counteri < 4:
            i += h
            counteri += 1
            h += g
            counterh += 1
        g += f
        counterg += 1
j = i + '1'
k = j + '7'
l = ''
for _ in range(5):
    m = ''
    for _ in range(4):
        n = ''
        for _ in range(4):
            n += m
            m += l
        l += k
o = ''
for _ in range(4):
    for __ in range(2):
                o += n
p = ''
for _ in range(5):
    q = ''
    for _ in range(2):
        q += p
        p += o
r = (q, q, q)
s, t, u = r
v = s + t + u
w = ''
for _ in range(9):
        if _ == 1:
            continue
        w += v
def x():
    return w
def y():
    return x()
def z():
    return y()
aa = z()
ab = aa + '.'
if ab == '3':
    ac = ab + ' c1'
elif ab == '17':
    ac = ab + ' c2'
else:
    ac = ab + ' c3'
ad_list = [ac for _ in range(9)]
ae = random.choice(ad_list)
if ae == '8':
    af = ae + ' c1'
elif ae == '12':
    af = ae + ' c2'
else:
    af = ae + ' c3'
ag = (af, af, af)
ah, ai, aj = ag
ak = ah + ai + aj
al = f'string {ak}'
am = ''
for _ in range(5):
    for __ in range(5):
                am += al
print(am)