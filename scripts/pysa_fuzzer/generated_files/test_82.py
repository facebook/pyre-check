import random
import math

a = input()
b_set = {a, a, a, a, a, a, a, a, a, a}
b = random.choice(list(b_set))
def c():
    return b
def d():
    return c()
e = d()
f = e + '.'
g = ''
counterg = 0
while counterg < 4:
    h = ''
    counterh = 0
    while counterh < 4:
        i = ''
        counteri = 0
        while counteri < 4:
            i += h
            counteri += 1
            h += g
            counterh += 1
        g += f
        counterg += 1
j = (i, i, i)
k, l, m = j
n = k + l + m
o_dict = {98: n, 27: n}
p = random.choice(list(o_dict.values()))
q = p[0:]
r = q + '.'
s = ''
for _ in range(4):
    s += r
t = [s for _ in range(7)]
random.shuffle(t)
u = random.choice(t)
v = ''
for _ in range(2):
    w = ''
    for _ in range(3):
        x = ''
        for _ in range(4):
            x += w
            w += v
        v += u
y = ''
countery = 0
while countery < 3:
    z = ''
    counterz = 0
    while counterz < 3:
        aa = ''
        counteraa = 0
        while counteraa < 2:
            aa += z
            counteraa += 1
            z += y
            counterz += 1
        y += x
        countery += 1
ab = aa[0:]
ac = ab + '9'
ad = ac + '8'
ae = ad[0:]
af = f'string {ae}'
ag = [af for _ in range(9)]
random.shuffle(ag)
ah = random.choice(ag)
ai = ah[0:]
print(ai)