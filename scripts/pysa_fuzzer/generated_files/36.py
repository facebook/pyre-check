import random
import math
a = input()
def b():
    return a
def c():
    return b()
d = c()
e = ''
for _ in range(3):
    for __ in range(4):
                e += d
if e == '4':
    f = e + ' c1'
elif e == '11':
    f = e + ' c2'
else:
    f = e + ' c3'
g = ''
for _ in range(8):
        if _ == 2:
            break
        g += f
if g == '2':
    h = g + ' c1'
elif g == '18':
    h = g + ' c2'
else:
    h = g + ' c3'
def i():
    return h
j = i()
k = (j, j, j)
l, m, n = k
o = l + m + n
p = (o, o, o)
q, r, s = p
t = q + r + s
u = ''
for _ in range(4):
    for __ in range(2):
                u += t
if u == '3':
    v = u + ' c1'
elif u == '16':
    v = u + ' c2'
else:
    v = u + ' c3'
w = ''
counterw = 0
while counterw < 5:
    x = ''
    counterx = 0
    while counterx < 3:
        x += w
        counterx += 1
        w += v
        counterw += 1
y = x[0:]
z = ''
counterz = 0
while counterz < 3:
    z += y
    counterz += 1
aa = ''
counteraa = 0
while counteraa < 3:
    ab = ''
    counterab = 0
    while counterab < 3:
        ac = ''
        counterac = 0
        while counterac < 3:
            ac += ab
            counterac += 1
            ab += aa
            counterab += 1
        aa += z
        counteraa += 1
ad = f'string {ac}'
ae = ''
for _ in range(5):
    ae += ad
af_set = {ae, ae}
af = random.choice(list(af_set))
ag_list = [af for _ in range(9)]
ah = random.choice(ag_list)
print(ah)