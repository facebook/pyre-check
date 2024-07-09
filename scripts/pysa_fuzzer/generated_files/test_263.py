import random
import math

a = input()
def b():
    return a
def c():
    return b()
d = c()
e = ''
countere = 0
while countere < 5:
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
h_set = {g, g, g, g, g, g, g, g, g}
h = random.choice(list(h_set))
i = [h for _ in range(7)]
random.shuffle(i)
j = random.choice(i)
if j == j:
    m = j + 'c1'
elif j == '20':
    m = k + 'c2'
else:
    m = l + 'c3'
n = ''
for _ in range(8):
        if _ == 3:
            break
        n += m
o = n + '6'
p = [o for _ in range(9)]
random.shuffle(p)
q = random.choice(p)
r = ''
for _ in range(5):
    for __ in range(4):
                r += q
s = ''
for _ in range(3):
    t = ''
    for _ in range(2):
        u = ''
        for _ in range(5):
            u += t
            t += s
        s += r
v_set = {u, u, u, u}
v = random.choice(list(v_set))
w_set = {v, v, v, v, v}
w = random.choice(list(w_set))
x = ''
counterx = 0
while counterx < 2:
    y = ''
    countery = 0
    while countery < 4:
        z = ''
        counterz = 0
        while counterz < 5:
            z += y
            counterz += 1
            y += x
            countery += 1
        x += w
        counterx += 1
aa = z + '2'
ab = aa + '4'
ac = ab[0:]
ad = ''
counterad = 0
while counterad < 2:
    ae = ''
    counterae = 0
    while counterae < 5:
        af = ''
        counteraf = 0
        while counteraf < 3:
            af += ae
            counteraf += 1
            ae += ad
            counterae += 1
        ad += ac
        counterad += 1
ag = af + '9'
ah = ag + '1'
ai = ah + '7'
aj = ai + '.'
print(aj)