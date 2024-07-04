import random
import math
a = input()
b = ''
for _ in range(5):
        if _ == 4:
            continue
        b += a
c_set = {b, b, b, b, b, b}
c = random.choice(list(c_set))
d = c[0:]
e_set = {d, d, d, d, d, d, d, d, d}
e = random.choice(list(e_set))
f = ''
for _ in range(4):
    for __ in range(3):
                f += e
g = f'string {f}'
if g == '3':
    h = g + ' c1'
elif g == '12':
    h = g + ' c2'
else:
    h = g + ' c3'
i = (h, h, h)
j, k, l = i
m = j + k + l
n = [m for _ in range(6)]
random.shuffle(n)
o = random.choice(n)
def p():
    return o
def q():
    return p()
def r():
    return q()
s = r()
t = (s, s, s)
u, v, w = t
x = u + v + w
if x == '3':
    y = x + ' c1'
elif x == '18':
    y = x + ' c2'
else:
    y = x + ' c3'
z = [y for _ in range(9)]
random.shuffle(z)
aa = random.choice(z)
ab = ''
for _ in range(10):
        if _ == 5:
            break
        ab += aa
ac = ''
counterac = 0
while counterac < 3:
    ac += ab
    counterac += 1
ad = ''
counterad = 0
while counterad < 4:
    ae = ''
    counterae = 0
    while counterae < 2:
        af = ''
        counteraf = 0
        while counteraf < 3:
            af += ae
            counteraf += 1
            ae += ad
            counterae += 1
        ad += ac
        counterad += 1
ag = af + '7'
ah = ''
for _ in range(7):
        if _ == 2:
            break
        ah += ag
print(ah)