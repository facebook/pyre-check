import random
import math

a = input()
b = a + '3'
c = b + '6'
d = c + '1'
e = ''
for _ in range(5):
    for __ in range(5):
                e += d
f = ''
for _ in range(2):
    g = ''
    for _ in range(4):
        h = ''
        for _ in range(4):
            h += g
            g += f
        f += e
i = (h, h, h)
j, k, l = i
m = j + k + l
n = m[0:]
o = [n for _ in range(10)]
random.shuffle(o)
p = random.choice(o)
q_set = {p, p, p}
q = random.choice(list(q_set))
r = q + '7'
def s():
    return r
def t():
    return s()
u = t()
v_set = {u, u}
v = random.choice(list(v_set))
w = ''
counterw = 0
while counterw < 3:
    x = ''
    counterx = 0
    while counterx < 4:
        y = ''
        countery = 0
        while countery < 3:
            y += x
            countery += 1
            x += w
            counterx += 1
        w += v
        counterw += 1
def z():
    return y
def aa():
    return z()
ab = aa()
ac = f'string {ab}'
ad = f'string {ac}'
ae = ''
for _ in range(8):
        if _ == 3:
            break
        ae += ad
if ae == ae:
    ah = ae + 'c1'
elif ae == '20':
    ah = af + 'c2'
else:
    ah = ag + 'c3'
ai = ah + '.'
aj = [ai for _ in range(10)]
random.shuffle(aj)
ak = random.choice(aj)
print(ak)