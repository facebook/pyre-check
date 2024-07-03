import random
import math
a = input()
if a == '8':
    b = a + ' c1'
elif a == '14':
    b = a + ' c2'
else:
    b = a + ' c3'
c = b[0:]
d = ''
counterd = 0
while counterd < 5:
    e = ''
    countere = 0
    while countere < 4:
        f = ''
        counterf = 0
        while counterf < 5:
            f += e
            counterf += 1
            e += d
            countere += 1
        d += c
        counterd += 1
g = (f, f, f)
h, i, j = g
k = h + i + j
l = k[0:]
m = l[0:]
n = [m for _ in range(10)]
random.shuffle(n)
o = random.choice(n)
p_set = {o, o, o, o, o}
p = random.choice(list(p_set))
q_set = {p, p, p, p}
q = random.choice(list(q_set))
r_set = {q, q, q, q, q, q, q, q, q}
r = random.choice(list(r_set))
s = ''
for _ in range(9):
        if _ == 3:
            continue
        s += r
t_list = [s for _ in range(2)]
u_list = [t_list for _ in range(3)]
v = random.choice(u_list)
w = random.choice(v)
x = (w, w, w)
y, z, aa = x
ab = y + z + aa
if ab == '1':
    ac = ab + ' c1'
elif ab == '12':
    ac = ab + ' c2'
else:
    ac = ab + ' c3'
ad_list = [ac for _ in range(8)]
ae = random.choice(ad_list)
af = [ae for _ in range(8)]
random.shuffle(af)
ag = random.choice(af)
ah = ''
counterah = 0
while counterah < 2:
    ai = ''
    counterai = 0
    while counterai < 5:
        aj = ''
        counteraj = 0
        while counteraj < 2:
            aj += ai
            counteraj += 1
            ai += ah
            counterai += 1
        ah += ag
        counterah += 1
ak = (aj, aj, aj)
al, am, an = ak
ao = al + am + an
print(ao)