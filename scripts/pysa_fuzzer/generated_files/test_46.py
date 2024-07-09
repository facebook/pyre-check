import random
import math

a = input()
b = [a for _ in range(5)]
random.shuffle(b)
c = random.choice(b)
d = c[0:]
e_set = {d, d, d, d, d, d, d, d, d, d}
e = random.choice(list(e_set))
f = (e, e, e)
g, h, i = f
j = g + h + i
k_set = {j, j, j, j, j, j, j, j, j}
k = random.choice(list(k_set))
l = ''
counterl = 0
while counterl < 5:
    m = ''
    counterm = 0
    while counterm < 5:
        n = ''
        countern = 0
        while countern < 4:
            n += m
            countern += 1
            m += l
            counterm += 1
        l += k
        counterl += 1
o = ''
for _ in range(3):
    p = ''
    for _ in range(5):
        q = ''
        for _ in range(2):
            q += p
            p += o
        o += n
r = ''
for _ in range(2):
    r += q
s = ''
for _ in range(2):
    t = ''
    for _ in range(5):
        u = ''
        for _ in range(2):
            u += t
            t += s
        s += r
v = ''
for _ in range(2):
    w = ''
    for _ in range(5):
        x = ''
        for _ in range(4):
            x += w
            w += v
        v += u
y = ''
for _ in range(9):
        if _ == 1:
            continue
        y += x
def z():
    return y
def aa():
    return z()
def ab():
    return aa()
ac = ab()
ad = ''
for _ in range(8):
        if _ == 3:
            break
        ad += ac
ae = [ad for _ in range(5)]
random.shuffle(ae)
af = random.choice(ae)
ag = af[0:]
ah = ''
counterah = 0
while counterah < 5:
    ai = ''
    counterai = 0
    while counterai < 4:
        ai += ah
        counterai += 1
        ah += ag
        counterah += 1
aj_list = [ai for _ in range(6)]
ak = random.choice(aj_list)
al_list = [ak for _ in range(6)]
am_list = [al_list for _ in range(8)]
an = random.choice(am_list)
ao = random.choice(an)
print(ao)