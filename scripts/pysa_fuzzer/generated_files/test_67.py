import random
import math

a = input()
b = ''
for _ in range(9):
        if _ == 5:
            break
        b += a
c_set = {b, b}
c = random.choice(list(c_set))
d = ''
for _ in range(9):
        if _ == 1:
            continue
        d += c
e = ''
countere = 0
while countere < 4:
    f = ''
    counterf = 0
    while counterf < 3:
        f += e
        counterf += 1
        e += d
        countere += 1
g = ''
counterg = 0
while counterg < 2:
    g += f
    counterg += 1
h = ''
for _ in range(3):
    for __ in range(3):
                h += g
i = ''
for _ in range(5):
        if _ == 5:
            break
        i += h
j = ''
for _ in range(4):
    k = ''
    for _ in range(3):
        k += j
        j += i
l = [k for _ in range(5)]
random.shuffle(l)
m = random.choice(l)
n = ''
for _ in range(3):
    for __ in range(3):
                n += m
def o():
    return n
def p():
    return o()
q = p()
r = ''
counterr = 0
while counterr < 3:
    s = ''
    counters = 0
    while counters < 2:
        t = ''
        countert = 0
        while countert < 5:
            t += s
            countert += 1
            s += r
            counters += 1
        r += q
        counterr += 1
u = [t for _ in range(8)]
random.shuffle(u)
v = random.choice(u)
w = f'string {v}'
x = ''
for _ in range(2):
    y = ''
    for _ in range(4):
        y += x
        x += w
z = ''
for _ in range(7):
        if _ == 5:
            break
        z += y
aa = z + '.'
ab_set = {aa, aa, aa, aa, aa, aa}
ab = random.choice(list(ab_set))
print(ab)