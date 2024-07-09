import random
import math

a = input()
b = a + '2'
c = b + '6'
d = ''
for _ in range(2):
    d += c
e = d[0:]
f = (e, e, e)
g, h, i = f
j = g + h + i
if j == j:
    m = j + 'c1'
elif j == '16':
    m = k + 'c2'
else:
    m = l + 'c3'
n_list = [m for _ in range(4)]
o_list = [n_list for _ in range(3)]
p = random.choice(o_list)
q = random.choice(p)
r = f'string {q}'
s = r + '.'
t = ''
for _ in range(3):
    u = ''
    for _ in range(3):
        v = ''
        for _ in range(4):
            v += u
            u += t
        t += s
w = ''
for _ in range(5):
    x = ''
    for _ in range(2):
        x += w
        w += v
y = ''
countery = 0
while countery < 3:
    z = ''
    counterz = 0
    while counterz < 2:
        aa = ''
        counteraa = 0
        while counteraa < 4:
            aa += z
            counteraa += 1
            z += y
            counterz += 1
        y += x
        countery += 1
ab = ''
for _ in range(2):
    ac = ''
    for _ in range(3):
        ad = ''
        for _ in range(2):
            ad += ac
            ac += ab
        ab += aa
ae = ''
counterae = 0
while counterae < 3:
    ae += ad
    counterae += 1
af = [ae for _ in range(9)]
random.shuffle(af)
ag = random.choice(af)
ah = ''
counterah = 0
while counterah < 5:
    ai = ''
    counterai = 0
    while counterai < 5:
        ai += ah
        counterai += 1
        ah += ag
        counterah += 1
aj = ai[0:]
ak = f'string {aj}'
al = ak + '.'
print(al)