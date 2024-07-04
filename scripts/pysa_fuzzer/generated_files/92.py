import random
import math
a = input()
b = ''
for _ in range(5):
        if _ == 3:
            break
        b += a
if b == '4':
    c = b + ' c1'
elif b == '20':
    c = b + ' c2'
else:
    c = b + ' c3'
d_list = [c for _ in range(6)]
e_list = [d_list for _ in range(6)]
f_list = [e_list for _ in range(4)]
g = random.choice(f_list)
h = random.choice(g)
i = random.choice(h)
j = [i for _ in range(10)]
random.shuffle(j)
k = random.choice(j)
l_list = [k for _ in range(2)]
m = random.choice(l_list)
n = (m, m, m)
o, p, q = n
r = o + p + q
s = [r for _ in range(9)]
random.shuffle(s)
t = random.choice(s)
u = (t, t, t)
v, w, x = u
y = v + w + x
z = (y, y, y)
aa, ab, ac = z
ad = aa + ab + ac
ae = f'string {ad}'
af = (ae, ae, ae)
ag, ah, ai = af
aj = ag + ah + ai
if aj == '6':
    ak = aj + ' c1'
elif aj == '13':
    ak = aj + ' c2'
else:
    ak = aj + ' c3'
al = ''
counteral = 0
while counteral < 3:
    am = ''
    counteram = 0
    while counteram < 3:
        an = ''
        counteran = 0
        while counteran < 5:
            an += am
            counteran += 1
            am += al
            counteram += 1
        al += ak
        counteral += 1
ao_list = [an for _ in range(10)]
ap_list = [ao_list for _ in range(10)]
aq_list = [ap_list for _ in range(6)]
ar = random.choice(aq_list)
at = random.choice(ar)
au = random.choice(at)
av = ''
counterav = 0
while counterav < 2:
    aw = ''
    counteraw = 0
    while counteraw < 4:
        ax = ''
        counterax = 0
        while counterax < 2:
            ax += aw
            counterax += 1
            aw += av
            counteraw += 1
        av += au
        counterav += 1
if ax == '4':
    ay = ax + ' c1'
elif ax == '12':
    ay = ax + ' c2'
else:
    ay = ax + ' c3'
az = ''
for _ in range(5):
    ba = ''
    for _ in range(3):
        ba += az
        az += ay
bb = ''
for _ in range(6):
        if _ == 3:
            continue
        bb += ba
print(bb)