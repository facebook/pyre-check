import random
import math

a = input()
b = f'string {a}'
c_list = [b for _ in range(4)]
d_list = [c_list for _ in range(7)]
e = random.choice(d_list)
f = random.choice(e)
g = ''
for _ in range(5):
    h = ''
    for _ in range(5):
        h += g
        g += f
i = (h, h, h)
j, k, l = i
m = j + k + l
n = f'string {m}'
o = ''
for _ in range(5):
    for __ in range(2):
                o += n
p = ''
counterp = 0
while counterp < 5:
    q = ''
    counterq = 0
    while counterq < 2:
        r = ''
        counterr = 0
        while counterr < 4:
            r += q
            counterr += 1
            q += p
            counterq += 1
        p += o
        counterp += 1
s = ''
counters = 0
while counters < 4:
    t = ''
    countert = 0
    while countert < 4:
        u = ''
        counteru = 0
        while counteru < 4:
            u += t
            counteru += 1
            t += s
            countert += 1
        s += r
        counters += 1
v = ''
counterv = 0
while counterv < 3:
    v += u
    counterv += 1
w = v + '6'
x = w + '3'
y = x + '7'
def z():
    return y
def aa():
    return z()
def ab():
    return aa()
ac = ab()
ad = (ac, ac, ac)
ae, af, ag = ad
ah = ae + af + ag
ai = ''
counterai = 0
while counterai < 4:
    aj = ''
    counteraj = 0
    while counteraj < 5:
        ak = ''
        counterak = 0
        while counterak < 4:
            ak += aj
            counterak += 1
            aj += ai
            counteraj += 1
        ai += ah
        counterai += 1
if ak == ak:
    an = ak + 'c1'
elif ak == '19':
    an = al + 'c2'
else:
    an = am + 'c3'
ao = ''
for _ in range(5):
        if _ == 5:
            continue
        ao += an
ap = ao[0:]
aq = [ap for _ in range(9)]
random.shuffle(aq)
ar = random.choice(aq)
at = ''
for _ in range(4):
    for __ in range(5):
                at += ar
print(at)