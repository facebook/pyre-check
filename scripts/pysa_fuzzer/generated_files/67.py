import random
import math
a = input()
b = (a, a, a)
c, d, e = b
f = c + d + e
g = [f for _ in range(6)]
random.shuffle(g)
h = random.choice(g)
i = (h, h, h)
j, k, l = i
m = j + k + l
n = ''
for _ in range(5):
    o = ''
    for _ in range(4):
        p = ''
        for _ in range(2):
            p += o
            o += n
        n += m
q = (p, p, p)
r, s, t = q
u = r + s + t
v = (u, u, u)
w, x, y = v
z = w + x + y
aa = z + '.'
ab = [aa for _ in range(8)]
random.shuffle(ab)
ac = random.choice(ab)
ad = ''
counterad = 0
while counterad < 2:
    ae = ''
    counterae = 0
    while counterae < 3:
        ae += ad
        counterae += 1
        ad += ac
        counterad += 1
af = ''
for _ in range(7):
        if _ == 3:
            continue
        af += ae
ag = (af, af, af)
ah, ai, aj = ag
ak = ah + ai + aj
al = ak + '8'
am = al + '2'
an = am + '5'
ao_list = [an for _ in range(4)]
ap_list = [ao_list for _ in range(3)]
aq = random.choice(ap_list)
ar = random.choice(aq)
at = ''
for _ in range(8):
        if _ == 1:
            continue
        at += ar
au = ''
for _ in range(6):
        if _ == 3:
            break
        au += at
av = [au for _ in range(10)]
random.shuffle(av)
aw = random.choice(av)
def ax():
    return aw
ay = ax()
az = ''
for _ in range(4):
    ba = ''
    for _ in range(5):
        ba += az
        az += ay
print(ba)